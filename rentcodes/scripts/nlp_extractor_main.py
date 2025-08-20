import difflib
import unicodedata
import re, os, json, csv, glob
import time
import argparse
from pathlib import Path

# Fallbacks if buckets fail
HEADING_RX = re.compile(r"^\s*(?:§|Sec\.?|Section)\s*([0-9A-Za-z\-\.:]+)\s*(.*)", re.IGNORECASE)
EMERGENCY_RX = re.compile(r"\b(housing\s+state\s+of\s+emergency|state\s+of\s+emergency|housing\s+emergenc(?:y|ies)|during\s+.*emergenc|while\s+.*emergenc)\b", re.I)
APPEAL_RX = re.compile(r"\bappeals?\b", re.I)
INCREASE_IN_TITLE_RX = re.compile(r"(increase|percentage|cpi|consumer\s+price\s+index|cost[-\s]?of[-\s]?living)", re.I)
INCREASE_BODY_RX = re.compile(r"\b(?:percentage\s+increase|rent(?:al)?\s+increase|shall\s+not\s+exceed|may\s+not\s+exceed|not\s+to\s+exceed)\b", re.I)
CPI_BODY_RX = re.compile(r"\b(?:consumer\s+price\s+index|CPI|cost[-\s]?of[-\s]?living)\b", re.I)
HARDSHIP_BODY_RX = re.compile(r"\bhardship\b.*?(increase|surcharge|application|rent|petition)", re.I|re.S)
CAPIMPR_BODY_RX = re.compile(r"\b(?:capital|major)\s+improvement(?:s)?\b", re.I)
RENT_BOARD_RX = re.compile(r"\brent\s*(?:control|leveling|stabiliz(?:ation|ing)?|review)\s*board\b", re.I)

TEMP_DECONTROL_PATTERNS = [
    r"\btemporary\s+(?:rent\s+)?decontrol\b",
    r"\bapply\s+for\s+temporary\s+(?:rent\s+)?decontrol\b",
    r"\b(?:unit|apartment|dwelling)\s+(?:shall|will)\s+be\s+(?:temporar\w+\s+)?decontrol\w+",
    r"\b(?:upon|after)\s+(?:first|initial|re-?rent(?:al|ing)?)\b.*?\b(?:subject|return|revert)\s+to\s+(?:rent\s+control|this\s+chapter)\b",
    r"\bdecontrol\w+.*?\bthereafter\b.*?\b(?:subject|governed)\s+by\s+(?:rent\s+control|this\s+chapter)\b",
    r"\bupon\s+re-?rent(?:ing)?\b.*?\b(?:subject|governed)\s+by\s+(?:rent\s+control|this\s+chapter)\b",
    r"\bonce\s+(?:rented|leased)\s+again\b.*?\b(?:subject|governed)\s+by\s+(?:rent\s+control|this\s+chapter)\b",
    r"\bshall\s+revert\s+to\s+(?:rent\s+control|this\s+chapter)\b",
    r"\bshall\s+be\s+recontrolled\b",
]
TEMP_DECONTROL_RX = [re.compile(p, re.I|re.S) for p in TEMP_DECONTROL_PATTERNS]

CONT_DECONTROL_PATTERNS = [
    r"\b(?:permanent|permanently)\s+(?:decontrolled|exempt)\s+from\s+(?:rent\s+control|this\s+chapter)\b",
    r"\b(?:shall|will)\s+(?:no\s+longer\s+be|become)\s+(?:exempt|decontrolled)\s+from\s+(?:rent\s+control|this\s+chapter)\b",
    r"\bexempt\s+from\s+(?:rent\s+control|this\s+chapter)\s+(?:upon|after)\s+vacancy\b",
    r"\bvacancy\s+decontrol\b(?!.*\bsubject\s+to\s+(?:rent\s+control|this\s+chapter)\b)",
]
CONT_DECONTROL_RX = [re.compile(p, re.I|re.S) for p in CONT_DECONTROL_PATTERNS]

BUCKETS = [
    "max_increase_pct",
    "cpi_linked",
    "increase_frequency_per_year",
    "vacancy_decontrol",
    "vacancy_increase_max_pct",
    "hardship_allowed",
    "capital_improvement_surcharge_allowed",
    "rent_board_present",
    "owner_occupied_exempt_units",
    "small_building_exempt_threshold",
    "min_units_covered",
    "sf_exempt",
    "mf_only",
    "year_built_cutoff",
    "new_construction_exempt",
    "emergency_only"
]

FNAME_RX = re.compile(r"^(?P<muni>.+)_(?P<date>\d{4}-\d{2}-\d{2})_(?P<hid>[A-Za-z0-9]+)_print\.txt$")

STOPWORDS = {"a","an","the","and","or","of","for","to","from","in","on","with","without","during","under","over","at","by","per","as","is","are","be","being","been","shall","may"}
INCREASE_TOKENS = {"increase","increases","rental","rent","adjustment","adjustments"}
CPI_TOKENS = {"cpi","consumer","price","index","col","cola","cost","living","cost-of-living"}
VACANCY_TOKENS = {"vacancy","vacancies","vacated","evicted","turnover"}

def _classify_decontrol_in_text(txt):
    temp_hits = []
    cont_hits = []
    for rx in TEMP_DECONTROL_RX:
        m = rx.search(txt)
        if m:
            temp_hits.append(rx.pattern)
    for rx in CONT_DECONTROL_RX:
        m = rx.search(txt)
        if m:
            cont_hits.append(rx.pattern)
    # Precedence: temporary > continuous (conservative snap-back assumption)
    if temp_hits:
        return "temporary", temp_hits, cont_hits
    if cont_hits:
        return "continuous", temp_hits, cont_hits
    return "none", temp_hits, cont_hits


def precompile_bucket_regex(br):
    out = {}
    for k, pats in br.items():
        out[k] = [re.compile(p, re.IGNORECASE|re.DOTALL) for p in pats]
    return out

def tokenize_norm(s):
    s = normalize_heading(s)
    toks = re.findall(r"[a-z0-9]+", s)
    return [t for t in toks if t not in STOPWORDS]

def jaccard(a, b):
    sa, sb = set(a), set(b)
    if not sa or not sb:
        return 0.0
    return len(sa & sb) / len(sa | sb)

def hybrid_sim(a, b):
    ta, tb = tokenize_norm(a), tokenize_norm(b)
    j = jaccard(ta, tb)
    r = difflib.SequenceMatcher(None, " ".join(ta), " ".join(tb)).ratio()
    return 0.6 * j + 0.4 * r

def tokens_set_from_norm(norm_s):
    return set(re.findall(r"[a-z0-9]+", norm_s)) - STOPWORDS

def norm_text(t):
    t = t.replace("\r\n","\n").replace("\r","\n")
    t = re.sub(r"\n{3,}", "\n\n", t)
    t = re.sub(r"[ \t]+\n", "\n", t)
    return t.strip()

def read_file(p):
    return norm_text(Path(p).read_text(encoding="utf-8", errors="ignore"))

def infer_meta(path):
    b = Path(path).name
    m = FNAME_RX.match(b)
    if not m:
        return {"municipality":"", "ordinance_date":"", "history_id":""}
    return {"municipality":m.group("muni"), "ordinance_date":m.group("date"), "history_id":m.group("hid")}

def segment_sections(txt):
    lines = txt.split("\n")
    out, buf, cur = [], [], {"code":"","title":""}
    for ln in lines:
        m = HEADING_RX.match(ln.strip())
        if m:
            if buf:
                out.append({"code":cur["code"], "title":cur["title"], "text":"\n".join(buf).strip()})
                buf = []
            cur = {"code":m.group(1).strip(), "title":m.group(2).strip()}
        else:
            buf.append(ln)
    if buf:
        out.append({"code":cur["code"], "title":cur["title"], "text":"\n".join(buf).strip()})
    return out

def normalize_heading(s):
    s = (s or "").lower().replace("\xa0"," ")
    s = "".join(ch for ch in s if not unicodedata.category(ch).startswith("P"))
    s = re.sub(r"\s+"," ", s).strip()
    return s

WORDS = {"one":1,"two":2,"three":3,"four":4,"five":5,"six":6,"seven":7,"eight":8,"nine":9,"ten":10}
def pick_numeric_or_word(groups):
    txt = " ".join(g for g in groups if g) if groups else ""
    m_year = re.search(r"\b(19\d{2}|20\d{2})\b", txt)
    if m_year:
        return float(m_year.group(1))
    m_num = re.search(r"\b\d+(?:\.\d+)?\b", txt)
    if m_num:
        return float(m_num.group(0))
    for w,v in WORDS.items():
        if re.search(rf"\b{w}\b", txt, re.I):
            return float(v)
    return None

def normalize(s):
    return re.sub(r"\s+"," ", (s or "").lower()).strip()

def load_json(p):
    with open(p, "r", encoding="utf-8") as f:
        return json.load(f)

def heading_triggers(bucket_map):
    trig = {}
    phrases = []
    for b, titles in bucket_map.items():
        for t in titles:
            k = normalize_heading(t)
            if not k:
                continue
            trig.setdefault(k, set()).add(b)
            phrases.append(k)
    return trig, phrases

def build_phrase_index(phrases):
    idx = {}
    for p in phrases:
        for tok in re.findall(r"[a-z0-9]+", p):
            if tok in STOPWORDS:
                continue
            idx.setdefault(tok, set()).add(p)
    return idx

def match_buckets_for_heading_norm(norm_title, triggers, phrases, phrase_index, sim_threshold=0.62, topk=3):
    hits = set()
    for k, buckets in triggers.items():
        if k in norm_title:
            hits |= buckets
    if not hits:
        toks = tokens_set_from_norm(norm_title)
        cand = set()
        for t in toks:
            cand |= phrase_index.get(t, set())
        if not cand:
            cand = set(phrases)
        scored = []
        for p in cand:
            s = hybrid_sim(norm_title, p)
            if s >= sim_threshold:
                scored.append((s, p))
        scored.sort(reverse=True)
        for _, p in scored[:topk]:
            hits |= triggers.get(p, set())
    toks = tokens_set_from_norm(norm_title)
    if INCREASE_TOKENS & toks:
        hits.add("max_increase_pct")
    if CPI_TOKENS & toks:
        hits.add("cpi_linked")
    if VACANCY_TOKENS & toks:
        hits.add("vacancy_decontrol")
        hits.add("vacancy_increase_max_pct")
    return list(hits)

def pick_numeric(groups):
    for g in groups:
        if g is None:
            continue
        if isinstance(g, (int,float)):
            return float(g)
        if isinstance(g, str):
            m = re.search(r"\d+(?:\.\d+)?", g.replace(",", ""))
            if m:
                return float(m.group(0))
    return None

def eval_bucket_regex_with_hits(bucket, patterns, text):
    vals = []
    hits = []

    pct_buckets = {"max_increase_pct", "vacancy_increase_max_pct"}
    num_buckets = {"increase_frequency_per_year",
                   "owner_occupied_exempt_units",
                   "small_building_exempt_threshold",
                   "min_units_covered",
                   "year_built_cutoff"}
    bool_buckets = {"cpi_linked", "vacancy_decontrol", "hardship_allowed",
                    "capital_improvement_surcharge_allowed", "rent_board_present",
                    "sf_exempt", "mf_only", "new_construction_exempt"}

    PCT_RX = re.compile(r"(\d{1,2}(?:\.\d{1,2})?)\s*%")
    HALF_RX = re.compile(r"(\d{1,2})\s*[½1/2]\s*%|(?:(\d{1,2})\s*(?:\.|,)\s*5)\s*%")

    def extract_percents(s):
        out = []
        for m in PCT_RX.finditer(s):
            try:
                v = float(m.group(1))
                out.append(v)
            except:
                pass
        # handle "7½%" or "7.5%" variants
        for m in HALF_RX.finditer(s):
            a = m.group(1) or m.group(2)
            if a:
                out.append(float(a) + 0.5)
        # keep only > 0
        return [v for v in out if v > 0]

    for pat in patterns:
        for m in pat.finditer(text):
            v = None
            if bucket in pct_buckets:
                # Prefer reading directly from the matched snippet; it handles “whichever is lesser”
                snippet = m.group(0)
                percents = extract_percents(snippet)
                # If pattern already captured groups, consider those too
                gnum = pick_numeric(m.groups() or [])
                if gnum and gnum > 0:
                    percents.append(gnum)
                if percents:
                    # choose the smallest positive percent (conservative)
                    v = min(percents)

            elif bucket in num_buckets:
                v = pick_numeric_or_word(m.groups() or [])

            elif bucket in bool_buckets:
                v = 1.0

            hits.append({"pattern": getattr(pat, "pattern", str(pat)),
                         "match": m.group(0)})

            if v is not None:
                vals.append(v)

    # CPI fallback: clear CPI language in body → set 1
    if bucket == "cpi_linked" and not hits and CPI_BODY_RX.search(text):
        m = CPI_BODY_RX.search(text)
        return 1, [{"pattern": "[CPI_BODY_RX]", "match": m.group(0)}]

    if bucket in pct_buckets:
        value = min(vals) if vals else None
    elif bucket in num_buckets:
        value = vals[0] if vals else None
    elif bucket in bool_buckets:
        value = 1 if vals else 0
    else:
        value = None

    return value, hits

def merge_values(a, b, bucket):
    if b is None:
        return a
    if a is None:
        return b
    if bucket in {"max_increase_pct","vacancy_increase_max_pct"}:
        return min(a, b)
    return a

def classify_doc_type(text):
    t = text.lower()
    if re.search(r"\b(rent\s+control|rent\s+stabilization|rent\s+leveling|landlord[-\s]?tenant\s+affairs)\b", t, re.I):
        return "rent_control"
    if re.search(r"\b(uhac|coah|affordable\s+housing|housing\s+element\s+and\s+fair\s+share|restricted\s+unit|income[-\s]?restricted)\b", t, re.I):
        return "affordable_housing_program"
    return "other"

def process_file(path, triggers, phrases, phrase_index, bucket_regex, seg_outdir=None):
    meta = infer_meta(path)
    txt = read_file(path)
    secs = segment_sections(txt)
    doc_type = classify_doc_type(txt)

    result = {b: None for b in BUCKETS}
    audit_sections = []
    saw_general_increase = False
    saw_emergency_increase = False

    for sec in secs:
        norm_title = normalize_heading(sec["title"])
        buckets = match_buckets_for_heading_norm(norm_title, triggers, phrases, phrase_index)

        is_emerg = bool(EMERGENCY_RX.search(sec["title"]) or EMERGENCY_RX.search(sec["text"][:600]))
        is_appeal = bool(APPEAL_RX.search(sec["title"]))

        # NEW: general body-text fallbacks (apply in any section)
        if CPI_BODY_RX.search(sec["text"]) and "cpi_linked" not in buckets:
            buckets.append("cpi_linked")
        if INCREASE_BODY_RX.search(sec["text"]) and "max_increase_pct" not in buckets:
            buckets.append("max_increase_pct")

        # Keep your emergency-specific boost (optional but harmless now)
        if is_emerg:
            if CPI_BODY_RX.search(sec["text"]) and "cpi_linked" not in buckets:
                buckets.append("cpi_linked")
            if INCREASE_BODY_RX.search(sec["text"]) and "max_increase_pct" not in buckets:
                buckets.append("max_increase_pct")

        # NEW: Rent Board detection (boolean)
        if RENT_BOARD_RX.search(sec["title"]) or RENT_BOARD_RX.search(sec["text"]):
            # set result immediately (and audit it)
            if result.get("rent_board_present") not in (1, True):
                result["rent_board_present"] = 1

        # De-dup while preserving order
        buckets = list(dict.fromkeys(buckets))

        sec_audit = {
            "code": sec["code"],
            "title": sec["title"],
            "title_normalized": norm_title,
            "matched_buckets": buckets,
            "emergency_section": int(is_emerg),
            "appeal_section": int(is_appeal),
            "text": sec["text"],
            "extractions": []
        }

        if buckets:
            for b in buckets:
                # Skip appeal sections for base increase/CPI unless title itself signals increase
                if is_appeal and b in {"max_increase_pct", "cpi_linked", "increase_frequency_per_year"} \
                   and not INCREASE_IN_TITLE_RX.search(sec["title"]):
                    continue

                pats = bucket_regex.get(b, [])
                if not pats:
                    continue

                v, hits = eval_bucket_regex_with_hits(b, pats, sec["text"])
                if hits or v is not None:
                    sec_audit["extractions"].append({"bucket": b, "value": v, "hits": hits})

                # Track emergency-vs-general increase signal
                if b in {"max_increase_pct", "cpi_linked"}:
                    if is_emerg and (hits or v not in (None, 0) or INCREASE_BODY_RX.search(sec["text"]) or CPI_BODY_RX.search(sec["text"])):
                        saw_emergency_increase = True
                    if (not is_emerg) and (hits or v not in (None, 0)):
                        saw_general_increase = True

                result[b] = merge_values(result[b], v, b)

        audit_sections.append(sec_audit)

    # Flag emergency-only if we saw increase/CPI only in emergency sections
    result["emergency_only"] = 1 if (saw_emergency_increase and not saw_general_increase) else 0

    # ---- Decontrol classification (temporary | continuous | none) ----
    vacancy_texts = []
    for sec_a in audit_sections:
        if "vacancy_decontrol" in sec_a.get("matched_buckets", []):
            vacancy_texts.append(sec_a["text"])

    decontrol_type = "none"
    decontrol_temp_hits = []
    decontrol_cont_hits = []

    if vacancy_texts:
        # Try each vacancy section individually, then combined
        decided = False
        for vt in vacancy_texts:
            dtype, th, ch = _classify_decontrol_in_text(vt)
            if dtype != "none":
                decontrol_type, decontrol_temp_hits, decontrol_cont_hits = dtype, th, ch
                decided = True
                break
        if not decided:
            dtype, th, ch = _classify_decontrol_in_text("\n\n".join(vacancy_texts))
            decontrol_type, decontrol_temp_hits, decontrol_cont_hits = dtype, th, ch
    else:
        # Whole-document fallback
        dtype, th, ch = _classify_decontrol_in_text(txt)
        decontrol_type, decontrol_temp_hits, decontrol_cont_hits = dtype, th, ch

    # If we classified any decontrol at all, ensure vacancy_decontrol gets turned on
    if decontrol_type in ("temporary", "continuous") and result.get("vacancy_decontrol") in (None, 0):
        result["vacancy_decontrol"] = 1

    # Optional: append a classifier summary to audit trail
    audit_sections.append({
        "code": "",
        "title": "[decontrol classifier summary]",
        "title_normalized": "",
        "matched_buckets": [],
        "emergency_section": 0,
        "appeal_section": 0,
        "text": "",
        "extractions": [{
            "bucket": "decontrol_type",
            "value": decontrol_type,
            "hits": [{"pattern": p, "type": "temporary"} for p in decontrol_temp_hits] +
                    [{"pattern": p, "type": "continuous"} for p in decontrol_cont_hits]
        }]
    })

    # ---- Build row ----
    row = {
        "municipality": meta["municipality"],
        "ordinance_date": meta["ordinance_date"],
        "history_id": meta["history_id"],
        "doc_type": doc_type
    }
    for b in BUCKETS:
        row[b] = int(result[b]) if isinstance(result[b], bool) else (
            int(result[b]) if (isinstance(result[b], float) and b in {
                "cpi_linked","vacancy_decontrol","hardship_allowed",
                "capital_improvement_surcharge_allowed","rent_board_present",
                "sf_exempt","mf_only","new_construction_exempt","emergency_only"
            }) else result[b]
        )

    # Add derived decontrol_type to row (remember to add to CSV fields list)
    row["decontrol_type"] = decontrol_type

    # Write audit JSON (segments)
    if seg_outdir:
        Path(seg_outdir).mkdir(parents=True, exist_ok=True)
        seg_fname = f"{meta['municipality']}_{meta['ordinance_date']}_{meta['history_id']}_segments.json".strip("_")
        with open(Path(seg_outdir, seg_fname), "w", encoding="utf-8") as f:
            json.dump({"meta": row, "sections": audit_sections}, f, ensure_ascii=False, indent=2)

    return row


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--input", required=True)
    ap.add_argument("--outdir", default="nlp_outputs")
    ap.add_argument("--bucket_map", required=True)
    ap.add_argument("--bucket_regex", required=True)
    args = ap.parse_args()

    Path(args.outdir).mkdir(parents=True, exist_ok=True)
    bucket_map = load_json(args.bucket_map)
    bucket_regex_json = load_json(args.bucket_regex)
    bucket_regex = precompile_bucket_regex(bucket_regex_json)
    triggers, phrases = heading_triggers(bucket_map)
    phrase_index = build_phrase_index(phrases)

    if os.path.isdir(args.input):
        files = glob.glob(os.path.join(args.input, "**", "*_print.txt"), recursive=True)
    else:
        files = [args.input]

    out_path = Path(args.outdir, "_features_wide4.csv")
    fields = ["municipality","ordinance_date","history_id","doc_type"] + BUCKETS + ["decontrol_type"]
    seg_outdir = Path("segments")
    seg_outdir.mkdir(parents=True, exist_ok=True)

    with open(out_path, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=fields)
        w.writeheader()
        for p in files:
            row = process_file(p, triggers, phrases, phrase_index, bucket_regex, seg_outdir=seg_outdir)
            w.writerow(row)

    print(out_path)

if __name__ == "__main__":
    start_time = time.time()
    main()
    end_time = time.time()
    runtime_minutes = (end_time - start_time) / 60
    print(f"Runtime: {runtime_minutes:.2f} minutes")