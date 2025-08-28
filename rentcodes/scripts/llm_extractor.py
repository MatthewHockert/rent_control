# scripts/llm_extractor.py
import os, json, re, csv, time, argparse, hashlib, threading, math
from pathlib import Path
from typing import Dict, Any, List, Tuple, Optional
from concurrent.futures import ThreadPoolExecutor, as_completed
from datetime import datetime
from openai import OpenAI
from tqdm import tqdm

# ----------------------------
# Config (env-tunable)
# ----------------------------
MODEL = os.getenv("OPENAI_LLM_MODEL", "gpt-4o-mini")
MAX_SECTIONS_PER_DOC = 30
MAX_SECTION_CHARS = 8000          # hard cap (safety)
SNIPPET_WINDOW = 320              # window around keyword hits
MAX_SNIPPET_CHARS = 2200          # target snippet size per section
TEMPERATURE = 0

# Cost model (USD per 1K tokens) — set exact prices for your model
INPUT_COST_PER_1K  = float(os.getenv("INPUT_COST_PER_1K",  "0.0005"))
OUTPUT_COST_PER_1K = float(os.getenv("OUTPUT_COST_PER_1K", "0.0015"))

# Output columns (match NLP CSV + decontrol_type)
COLUMNS = [
    "municipality","ordinance_date","history_id","doc_type",
    "max_increase_pct","cpi_linked","increase_frequency_per_year",
    "vacancy_decontrol","vacancy_increase_max_pct",
    "hardship_allowed","capital_improvement_surcharge_allowed",
    "rent_board_present","owner_occupied_exempt_units",
    "small_building_exempt_threshold","min_units_covered",
    "sf_exempt","mf_only","year_built_cutoff","new_construction_exempt",
    "emergency_only","decontrol_type"
]

BOOL_FIELDS = {
    "cpi_linked","vacancy_decontrol","hardship_allowed",
    "capital_improvement_surcharge_allowed","rent_board_present",
    "sf_exempt","mf_only","new_construction_exempt","emergency_only"
}
PCT_FIELDS = {"max_increase_pct","vacancy_increase_max_pct"}
NUM_FIELDS = {
    "increase_frequency_per_year","owner_occupied_exempt_units",
    "small_building_exempt_threshold","min_units_covered","year_built_cutoff"
}
ENUM_FIELDS = {"decontrol_type"}  # temporary | continuous | none | null

# ----------------------------
# OpenAI
# ----------------------------
client = OpenAI(api_key=os.getenv("OPENAI_API_KEY"))

# ----------------------------
# Optional token counting
# ----------------------------
try:
    import tiktoken
    _TIKTOKEN = True
except Exception:
    _TIKTOKEN = False

def _count_tokens(text: str, model: str = MODEL) -> int:
    if not text:
        return 0
    if _TIKTOKEN:
        try:
            enc = tiktoken.encoding_for_model(model)
        except Exception:
            enc = tiktoken.get_encoding("cl100k_base")
        return len(enc.encode(text))
    # simple heuristic fallback (~4 chars/token)
    return max(1, math.ceil(len(text) / 4))

# ----------------------------
# Thread-safe cost tracker
# ----------------------------
class CostTracker:
    def __init__(self, model: str, input_cost_per_1k: float, output_cost_per_1k: float):
        self.model = model
        self.input_cost_per_1k = input_cost_per_1k
        self.output_cost_per_1k = output_cost_per_1k
        self.prompt_tokens = 0
        self.completion_tokens = 0
        self.docs_done = 0
        self.started_at = time.time()
        self._lock = threading.Lock()

    def add_usage(self, prompt_tokens: int, completion_tokens: int, docs_inc: int = 0):
        with self._lock:
            self.prompt_tokens += int(prompt_tokens or 0)
            self.completion_tokens += int(completion_tokens or 0)
            self.docs_done += int(docs_inc or 0)

    def snapshot(self) -> Dict[str, Any]:
        with self._lock:
            p = self.prompt_tokens
            c = self.completion_tokens
            d = self.docs_done
        in_usd  = (p / 1000.0) * self.input_cost_per_1k
        out_usd = (c / 1000.0) * self.output_cost_per_1k
        total   = in_usd + out_usd
        elapsed = (time.time() - self.started_at) / 60.0
        return {
            "model": self.model,
            "processed_docs": d,
            "prompt_tokens": p,
            "completion_tokens": c,
            "est_input_usd": round(in_usd, 4),
            "est_output_usd": round(out_usd, 4),
            "est_total_usd": round(total, 4),
            "elapsed_min": round(elapsed, 2),
            "input_cost_per_1k": self.input_cost_per_1k,
            "output_cost_per_1k": self.output_cost_per_1k,
            "timestamp": datetime.utcnow().isoformat() + "Z",
        }

# ----------------------------
# Simple global rate limiter
# ----------------------------
class RateLimiter:
    def __init__(self, rps: float):
        self.lock = threading.Lock()
        self.min_interval = 1.0 / max(rps, 0.001)
        self.last = 0.0
    def wait(self):
        with self.lock:
            now = time.time()
            delay = self.min_interval - (now - self.last)
            if delay > 0:
                time.sleep(delay)
            self.last = time.time()

# ----------------------------
# Prompts
# ----------------------------
SYSTEM = """You are an ordinance extraction engine. Be literal and conservative.
Only return values explicitly grounded in the provided text. If a field is not
clearly present, return null (for numbers) or 0 (for booleans). Do not guess.
"""

SECTION_PROMPT = """You will receive one ordinance section (title + body).
Extract ONLY the facts that are explicitly stated in THIS section. If a field is
not supported by this section, leave it null/0.

Return STRICT JSON with this schema (no prose):

{{
  "max_increase_pct": number|null,
  "cpi_linked": 0|1,
  "increase_frequency_per_year": number|null,
  "vacancy_decontrol": 0|1,
  "vacancy_increase_max_pct": number|null,
  "hardship_allowed": 0|1,
  "capital_improvement_surcharge_allowed": 0|1,
  "rent_board_present": 0|1,
  "owner_occupied_exempt_units": number|null,
  "small_building_exempt_threshold": number|null,
  "min_units_covered": number|null,
  "sf_exempt": 0|1,
  "mf_only": 0|1,
  "year_built_cutoff": number|null,
  "new_construction_exempt": 0|1,
  "emergency_only": 0|1,
  "decontrol_type": "temporary"|"continuous"|"none"|null,
  "evidence": [
    {{ "bucket": "<field name>", "quote": "<short quote>", "start_offset": int, "end_offset": int }}
  ]
}}

Extraction rules:
- “Lesser of X% or CPI”: set max_increase_pct = X and cpi_linked = 1.
- If CPI/Consumer Price Index is mentioned as the formula, set cpi_linked = 1.
- “Once re-rented it is subject to this chapter/rent control” -> decontrol_type = "temporary".
- “Exempt/decontrolled upon vacancy (no snap back)” -> decontrol_type = "continuous".
- If section is clearly emergency-only, set emergency_only = 1.
- “Rent leveling/control/review board” -> rent_board_present = 1.
- If a percent appears in a prohibited context (e.g., “less than zero (0%) not permitted”) do NOT use 0 as a max cap.

Now the section:
TITLE: {title}
TEXT:
{body}
"""

# ----------------------------
# Helpers
# ----------------------------
KW_PATTERNS = [
    r"\b(?:consumer\s+price\s+index|CPI|cost[-\s]?of[-\s]?living)\b",
    r"\b(?:rent(?:al)?\s+increase|percentage\s+increase|shall\s+not\s+exceed|may\s+not\s+exceed|not\s+to\s+exceed)\b",
    r"\bvacanc(?:y|ies)|vacated|turnover|decontrol(?:led)?\b",
    r"\bhardship\b",
    r"\b(?:capital|major)\s+improvement(?:s)?\b",
    r"\brent\s*(?:control|leveling|stabiliz(?:ation|ing)?|review)\s*board\b",
    r"\bemergency\b",
]
KW_RX = [re.compile(p, re.I) for p in KW_PATTERNS]

def _row_key(row: Dict[str, Any]) -> str:
    return f"{row.get('municipality','')}|{row.get('ordinance_date','')}|{row.get('history_id','')}"

def _csv_existing_keys(csv_path: Path) -> set:
    keys = set()
    if not csv_path.exists():
        return keys
    with open(csv_path, "r", encoding="utf-8", newline="") as f:
        r = csv.DictReader(f)
        for rec in r:
            keys.add(_row_key(rec))
    return keys

def _row_from_audit(audit_obj: Dict[str, Any]) -> Dict[str, Any]:
    """
    Prefer the 'meta' block (already reconciled). If it's missing or incomplete,
    re-reconcile from llm_sections.
    """
    meta = audit_obj.get("meta", {}) or {}
    # If meta contains all columns, just return it aligned to COLUMNS
    if meta and all(k in meta for k in ["municipality","ordinance_date","history_id","doc_type"]):
        row = {k: meta.get(k) for k in COLUMNS if k in meta}
        # ensure any missing columns (older audits) become None/0
        for k in COLUMNS:
            if k not in row:
                row[k] = None
        return row

    # Fallback: re-reconcile from llm_sections
    llm_secs = audit_obj.get("llm_sections", []) or []
    doc_info = {
        "municipality": meta.get("municipality",""),
        "ordinance_date": meta.get("ordinance_date",""),
        "history_id": meta.get("history_id",""),
        "doc_type": meta.get("doc_type","other"),
    }
    reconciled = _reconcile(doc_info, llm_secs)
    row = {
        "municipality": doc_info["municipality"],
        "ordinance_date": doc_info["ordinance_date"],
        "history_id": doc_info["history_id"],
        "doc_type": doc_info["doc_type"],
        **{k: reconciled.get(k) for k in COLUMNS if k not in {"municipality","ordinance_date","history_id","doc_type"}}
    }
    # fill any missing keys defensively
    for k in COLUMNS:
        if k not in row:
            row[k] = None
    return row

def backfill_from_audits(llm_segments_dir: Path, out_csv_path: Path):
    """
    Scan *_segments_llm.json and ensure there is exactly one row in the aggregated CSV per audit.
    This will append any missing rows; existing rows are left unchanged.
    """
    audits = sorted(llm_segments_dir.glob("*_segments_llm.json"), key=lambda p: p.name)
    if not audits:
        print(f"No audits found in {llm_segments_dir}, nothing to backfill.")
        return

    existing = _csv_existing_keys(out_csv_path)
    write_header = not out_csv_path.exists()
    added = 0

    out_csv_path.parent.mkdir(parents=True, exist_ok=True)
    with open(out_csv_path, "a", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=COLUMNS)
        if write_header:
            w.writeheader()

        for ap in audits:
            try:
                audit_obj = json.loads(ap.read_text(encoding="utf-8"))
            except Exception:
                continue
            row = _row_from_audit(audit_obj)
            key = _row_key(row)
            if key in existing:
                continue
            # write and remember key
            w.writerow({k: row.get(k) for k in COLUMNS})
            existing.add(key)
            added += 1

    print(f"Backfill complete: added {added} missing rows to {out_csv_path}.")


def _strip_md_fences(s: str) -> str:
    s = s.strip()
    if s.startswith("```"):
        s = re.sub(r"^```(?:json)?", "", s).strip()
        s = re.sub(r"```$", "", s).strip()
    return s

def _safe_json(s: str) -> Dict[str, Any]:
    try:
        return json.loads(s)
    except Exception:
        return {}

def _is_candidate(sec: Dict[str, Any]) -> bool:
    if sec.get("matched_buckets"):
        return True
    t = (sec.get("title") or "") + " " + (sec.get("text") or "")
    t = t.lower()
    for rx in KW_RX:
        if rx.search(t):
            return True
    return False

def _coerce_bool(x): return 1 if x in (1,"1",True) else 0
def _coerce_num(x):
    if x is None: return None
    try: return float(x)
    except: return None
def _pick_smallest_positive(nums: List[float]):
    pos = [n for n in nums if n is not None and n > 0]
    return min(pos) if pos else None

def _reconcile(doc_level: Dict[str, Any], section_results: List[Dict[str, Any]]) -> Dict[str, Any]:
    out = {k: None for k in PCT_FIELDS|NUM_FIELDS}
    out.update({k: 0 for k in BOOL_FIELDS})
    out["decontrol_type"] = "none"

    agg_pct = {k: [] for k in PCT_FIELDS}
    agg_num = {k: [] for k in NUM_FIELDS}
    saw_emerg = saw_gen = False
    decontrol_votes = []

    for sec in section_results:
        for k in PCT_FIELDS:
            v = _coerce_num(sec.get(k))
            if v is not None: agg_pct[k].append(v)
        for k in NUM_FIELDS:
            v = _coerce_num(sec.get(k))
            if v is not None: agg_num[k].append(v)
        for k in BOOL_FIELDS:
            if _coerce_bool(sec.get(k)) == 1: out[k] = 1

        if _coerce_bool(sec.get("emergency_only")):
            saw_emerg = True
        if (_coerce_num(sec.get("max_increase_pct")) or _coerce_bool(sec.get("cpi_linked"))) and not _coerce_bool(sec.get("emergency_only")):
            saw_gen = True

        d = sec.get("decontrol_type")
        if d in ("temporary","continuous"): decontrol_votes.append(d)

    for k in PCT_FIELDS: out[k] = _pick_smallest_positive(agg_pct[k])
    for k in NUM_FIELDS: out[k] = agg_num[k][0] if agg_num[k] else None
    out["emergency_only"] = 1 if (saw_emerg and not saw_gen) else 0

    if "temporary" in decontrol_votes:
        out["decontrol_type"] = "temporary"
    elif "continuous" in decontrol_votes:
        out["decontrol_type"] = "continuous"

    out.update({k: doc_level.get(k) for k in ["doc_type"] if k in doc_level})
    return out

def _snippet(body: str) -> Tuple[str, bool]:
    """
    Return a reduced snippet around keyword hits; if nothing hits, return a truncated body.
    Boolean indicates whether this was a snippet (True) or full/truncated (False).
    """
    body = body or ""
    if len(body) <= MAX_SNIPPET_CHARS:
        return body, False

    hits = []
    for rx in KW_RX:
        for m in rx.finditer(body):
            hits.append((max(0, m.start() - SNIPPET_WINDOW), min(len(body), m.end() + SNIPPET_WINDOW)))
    if not hits:
        return body[:MAX_SECTION_CHARS], False

    # merge overlapping windows
    hits.sort()
    merged = []
    cur_s, cur_e = hits[0]
    for s, e in hits[1:]:
        if s <= cur_e + 20:
            cur_e = max(cur_e, e)
        else:
            merged.append((cur_s, cur_e))
            cur_s, cur_e = s, e
    merged.append((cur_s, cur_e))

    # build snippet up to MAX_SNIPPET_CHARS
    chunks = []
    total = 0
    for s, e in merged:
        seg = body[s:e]
        if total + len(seg) > MAX_SNIPPET_CHARS:
            seg = seg[:MAX_SNIPPET_CHARS - total]
        chunks.append(seg)
        total += len(seg)
        if total >= MAX_SNIPPET_CHARS:
            break
    snippet = "\n...\n".join(chunks)
    return snippet, True

def _prompt_hash(title: str, body: str) -> str:
    h = hashlib.sha256()
    # include model and a prompt version tag so cache invalidates if you change schema
    h.update(("v3|" + MODEL + "|" + title + "|" + body).encode("utf-8", errors="ignore"))
    return h.hexdigest()

def _cache_get(cache_dir: Optional[Path], key: str) -> Optional[Dict[str, Any]]:
    if not cache_dir: return None
    p = cache_dir / f"{key}.json"
    if p.exists():
        try:
            return json.loads(p.read_text(encoding="utf-8"))
        except Exception:
            return None
    return None

def _cache_put(cache_dir: Optional[Path], key: str, data: Dict[str, Any], usage: Optional[Dict[str, int]] = None):
    if not cache_dir: return
    cache_dir.mkdir(parents=True, exist_ok=True)
    p = cache_dir / f"{key}.json"
    try:
        obj = {"data": data}
        if usage:
            obj["usage"] = {
                "prompt_tokens": int(usage.get("prompt_tokens", 0) or 0),
                "completion_tokens": int(usage.get("completion_tokens", 0) or 0),
            }
        p.write_text(json.dumps(obj, ensure_ascii=False, indent=2), encoding="utf-8")
    except Exception:
        pass

def build_prompt_section(title: str, body: str, template: str) -> str:
    # literal replacement (avoid .format {} collisions inside JSON schema)
    return template.replace("{title}", title or "").replace("{body}", body or "")

# ----------------------------
# LLM call (cache-aware, usage-aware)
# ----------------------------
def _call_llm(title: str, body: str, limiter: RateLimiter, cache_dir: Optional[Path]) -> Tuple[Dict[str, Any], int, int]:
    """
    Returns: (data_dict, prompt_tokens, completion_tokens)
    - data_dict: parsed JSON (or {})
    - tokens: API usage if available; else estimated on cache hits
    """
    body = (body or "")[:MAX_SECTION_CHARS]
    prompt = build_prompt_section(title, body, SECTION_PROMPT)
    key = _prompt_hash(title, body)

    # cache read
    cached = _cache_get(cache_dir, key)
    if cached is not None:
        data = cached.get("data", {})
        u = cached.get("usage")
        if u:
            return data or {}, int(u.get("prompt_tokens", 0)), int(u.get("completion_tokens", 0))
        # else estimate (older cache without usage)
        return data or {}, _count_tokens(prompt, MODEL), 0

    # rate-limit
    limiter.wait()

    # API call
    resp = client.chat.completions.create(
        model=MODEL,
        temperature=TEMPERATURE,
        messages=[
            {"role":"system","content": SYSTEM},
            {"role":"user",  "content": prompt}
        ]
    )
    raw = resp.choices[0].message.content or ""
    raw = _strip_md_fences(raw)
    data = _safe_json(raw)

    # usage accounting
    try:
        usage = resp.usage or {}
        pt = int(usage.get("prompt_tokens", 0) or 0)
        ct = int(usage.get("completion_tokens", 0) or 0)
    except Exception:
        pt = _count_tokens(prompt, MODEL)
        ct = _count_tokens(raw, MODEL)

    # cache write with usage
    _cache_put(cache_dir, key, data, usage={"prompt_tokens": pt, "completion_tokens": ct})
    return data or {}, pt, ct

def _normalize_section_result(res: Dict[str, Any]) -> Dict[str, Any]:
    out = {}
    for k in PCT_FIELDS: out[k] = _coerce_num(res.get(k))
    for k in NUM_FIELDS: out[k] = _coerce_num(res.get(k))
    for k in BOOL_FIELDS: out[k] = _coerce_bool(res.get(k))
    d = res.get("decontrol_type")
    out["decontrol_type"] = d if d in ("temporary","continuous","none") else None
    out["emergency_only"] = _coerce_bool(res.get("emergency_only"))
    out["evidence"] = res.get("evidence", [])
    return out

def _stem(meta: Dict[str, Any]) -> str:
    return f"{meta.get('municipality','')}_{meta.get('ordinance_date','')}_{meta.get('history_id','')}"

def _already_done(stem: str, out_seg_dir: Path) -> bool:
    return Path(out_seg_dir, f"{stem}_segments_llm.json").exists()

def _all_fields_filled(partial: Dict[str, Any]) -> bool:
    # all booleans decided and each numeric/pct has some value
    for b in BOOL_FIELDS:
        if partial.get(b) not in (0,1):
            return False
    for k in PCT_FIELDS|NUM_FIELDS:
        if partial.get(k) is None:
            return False
    return True

# ----------------------------
# Per-document processing
# ----------------------------
def process_one_doc(seg_path: Path, out_seg_dir: Path, cache_dir: Optional[Path],
                    limiter: RateLimiter, tracker: CostTracker,
                    resume: bool, early_stop: bool) -> Optional[Dict[str, Any]]:
    try:
        seg = json.loads(seg_path.read_text(encoding="utf-8"))
    except Exception:
        return None

    meta = seg.get("meta", {})
    sections = seg.get("sections", [])
    stem = _stem(meta)
    audit_path = out_seg_dir / f"{stem}_segments_llm.json"

    # resume: if audit exists, reuse its per-section results
    per_section_cached = None
    if resume and audit_path.exists():
        try:
            cached = json.loads(audit_path.read_text(encoding="utf-8"))
            per_section_cached = cached.get("llm_sections")
        except Exception:
            per_section_cached = None

    # choose candidate sections deterministically
    cand = [s for s in sections if _is_candidate(s)]
    cand = cand[:MAX_SECTIONS_PER_DOC]

    per_section: List[Dict[str, Any]] = []
    doc_prompt_tokens = 0
    doc_completion_tokens = 0

    if per_section_cached is not None:
        per_section = per_section_cached
    else:
        # live extraction, with snippet + cache and optional early-stop
        running = {
            **{k: None for k in PCT_FIELDS|NUM_FIELDS},
            **{k: 0 for k in BOOL_FIELDS},
            "decontrol_type": None
        }
        for s in cand:
            title = s.get("title") or s.get("title_normalized") or ""
            full_body = s.get("text") or ""
            body, used_snippet = _snippet(full_body)

            res, pt, ct = _call_llm(title, body[:MAX_SECTION_CHARS], limiter, cache_dir)
            doc_prompt_tokens += pt
            doc_completion_tokens += ct

            out = _normalize_section_result(res)
            out["_title"] = title
            out["_snippet"] = used_snippet
            per_section.append(out)

            # update running fill state (for early_stop)
            for k in PCT_FIELDS|NUM_FIELDS:
                if running[k] is None and out.get(k) is not None:
                    running[k] = out.get(k)
            for b in BOOL_FIELDS:
                if running[b] == 0 and out.get(b) == 1:
                    running[b] = 1
            if running.get("decontrol_type") is None and out.get("decontrol_type") in ("temporary","continuous","none"):
                running["decontrol_type"] = out.get("decontrol_type")

            if early_stop and _all_fields_filled(running):
                break

    # reconcile to doc-level
    doc_info = {
        "municipality": meta.get("municipality",""),
        "ordinance_date": meta.get("ordinance_date",""),
        "history_id": meta.get("history_id",""),
        "doc_type": meta.get("doc_type","other"),
    }
    reconciled = _reconcile(doc_info, per_section)

    # build final row
    row = {
        "municipality": doc_info["municipality"],
        "ordinance_date": doc_info["ordinance_date"],
        "history_id": doc_info["history_id"],
        "doc_type": doc_info["doc_type"],
        **{k: reconciled.get(k) for k in COLUMNS if k not in {"municipality","ordinance_date","history_id","doc_type"}}
    }

    # write audit
    llm_seg_obj = {
        "meta": row,
        "llm_sections": per_section
    }
    out_seg_dir.mkdir(parents=True, exist_ok=True)
    audit_path.write_text(json.dumps(llm_seg_obj, indent=2), encoding="utf-8")

    # update cost tracker
    tracker.add_usage(doc_prompt_tokens, doc_completion_tokens, docs_inc=1)

    return row

# ----------------------------
# CLI / Orchestration
# ----------------------------
def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--segments", required=True, help="Path to segments/*.json OR a directory")
    ap.add_argument("--out_csv", default="output/llm_aggregated.csv")
    ap.add_argument("--out_segments", default="output/llm_segments")
    ap.add_argument("--cache_dir", default="output/llm_cache", help="Persistent per-section cache")
    ap.add_argument("--limit", type=int, default=0, help="Process only the first N files (after sorting)")
    ap.add_argument("--resume", action="store_true", help="Reuse per-city audits if present")
    ap.add_argument("--workers", type=int, default=4, help="Parallel docs processed at once")
    ap.add_argument("--rps", type=float, default=2.0, help="Global requests per second limit")
    ap.add_argument("--early_stop", action="store_true", help="Stop scanning sections once fields are filled")
    ap.add_argument("--backfill", action="store_true",
                help="Rebuild aggregated CSV from existing *_segments_llm.json audits only (no API calls).")
    args = ap.parse_args()

    p = Path(args.segments)
    if p.is_dir():
        seg_paths = sorted(p.glob("*.json"), key=lambda x: x.name)
    else:
        seg_paths = [p]

    if args.limit and args.limit > 0:
        seg_paths = seg_paths[:args.limit]
    
    if args.backfill:
        out_seg_dir = Path(args.out_segments)
        out_csv_path = Path(args.out_csv)
        backfill_from_audits(out_seg_dir, out_csv_path)
        return

    out_seg_dir = Path(args.out_segments)
    out_csv_path = Path(args.out_csv)
    out_csv_path.parent.mkdir(parents=True, exist_ok=True)

    cache_dir = Path(args.cache_dir) if args.cache_dir else None
    limiter = RateLimiter(args.rps)

    # tracker for live cost
    tracker = CostTracker(MODEL, INPUT_COST_PER_1K, OUTPUT_COST_PER_1K)

    # resume: skip docs with existing audit
    if args.resume:
        pending = []
        for sp in seg_paths:
            try:
                seg = json.loads(Path(sp).read_text(encoding="utf-8"))
                m = seg.get("meta", {})
                stem = f"{m.get('municipality','')}_{m.get('ordinance_date','')}_{m.get('history_id','')}"
                done = Path(out_seg_dir, f"{stem}_segments_llm.json")
                if not done.exists():
                    pending.append(sp)
            except Exception:
                pending.append(sp)
        seg_paths = pending

    # prepare aggregated CSV
    write_header = not out_csv_path.exists()
    csv_f = open(out_csv_path, "a", newline="", encoding="utf-8")
    writer = csv.DictWriter(csv_f, fieldnames=COLUMNS)
    if write_header:
        writer.writeheader()

    progress = tqdm(total=len(seg_paths), desc="LLM extracting", unit="doc")
    rows_buffer: List[Dict[str, Any]] = []
    report_every = 25
    t0 = time.time()

    try:
        with ThreadPoolExecutor(max_workers=max(1, args.workers)) as ex:
            futures = [ex.submit(process_one_doc, sp, out_seg_dir, cache_dir, limiter, tracker, args.resume, args.early_stop)
                       for sp in seg_paths]

            for i, fut in enumerate(as_completed(futures), start=1):
                try:
                    row = fut.result()
                    if row:
                        rows_buffer.append(row)
                except Exception:
                    # swallow and continue; could also log filename
                    pass

                progress.update(1)

                # flush rows in small batches
                if len(rows_buffer) >= 50:
                    for r in rows_buffer:
                        writer.writerow({k: r.get(k) for k in COLUMNS})
                    csv_f.flush()
                    rows_buffer.clear()

                # periodic cost report in progress bar
                if i % report_every == 0:
                    snap = tracker.snapshot()
                    progress.set_postfix({
                        "docs": snap["processed_docs"],
                        "in_tok": snap["prompt_tokens"],
                        "out_tok": snap["completion_tokens"],
                        "cost": f"${snap['est_total_usd']}"
                    })

        # final flush
        if rows_buffer:
            for r in rows_buffer:
                writer.writerow({k: r.get(k) for k in COLUMNS})
            csv_f.flush()

    finally:
        csv_f.close()

        # final cost report
        snap = tracker.snapshot()
        cost_report_path = Path(out_csv_path.parent, "llm_cost_report.json")
        cost_report_path.write_text(json.dumps(snap, indent=2), encoding="utf-8")

        try:
            backfill_from_audits(Path(args.out_segments), out_csv_path)
        except Exception as e:
            print(f"Backfill step skipped due to error: {e}")

        progress.close()
        print(f"\nWrote aggregated CSV: {out_csv_path}")
        print(f"Wrote cost report: {cost_report_path}")
        print(f"Processed {snap['processed_docs']} docs in {snap['elapsed_min']} min.")
        print(f"Estimated total cost: ${snap['est_total_usd']:.4f}")

if __name__ == "__main__":
    main()