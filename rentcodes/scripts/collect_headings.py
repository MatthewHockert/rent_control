#!/usr/bin/env python3
import re, json, argparse, hashlib, difflib, csv
from pathlib import Path
from collections import defaultdict

# ===== Config: valid code token patterns (STRICT) =====
CODE_PATTERNS = [
    r"\d{1,4}[:\-]\d{1,4}(?:\.\d+)*\.?",         # 19:2-3.1.  or 297-14. or 145-4
    r"\d{1,4}(?:-\d{1,4})+(?:\.\d+)*\.?",        # 145-4-2  or 297-14.2
    r"\d{1,4}(?:\.\d{1,4}){1,3}\.?",             # 2.3  or 2.3.4
]
CODE_COMPILED = [re.compile(rf"^\s*(?:§|Sec\.?|Section)?\s*({p})\s+(.*)\s*$", re.IGNORECASE) for p in CODE_PATTERNS]

EXCLUDE_TITLE_PREFIXES = re.compile(r"^(by|ord\.?|editor|editor’s|editor's)\b", re.IGNORECASE)
DATE_LINE = re.compile(r"^\s*\d{1,2}[-/]\d{1,2}[-/]\d{2,4}\s*$")
SINGLE_LETTER_CODE = re.compile(r"^[A-Z]\.?$")

FNAME_PAT = re.compile(r"^(?P<muni>.+)_(?P<date>\d{4}-\d{2}-\d{2})_(?P<hid>[A-Za-z0-9]+)_print\.txt$")

def norm_text(t):
    t = t.replace("\r\n","\n").replace("\r","\n")
    t = re.sub(r"\n{3,}", "\n\n", t)
    t = re.sub(r"[ \t]+\n", "\n", t)
    return t

def norm_title(s):
    s = (s or "").strip().lower()
    s = re.sub(r"\s+", " ", s)
    s = re.sub(r"[^\w\s\.:/-]", "", s)
    return s.strip(".: ").strip()

def md5_hex(s):
    return hashlib.md5(s.encode("utf-8")).hexdigest()

def infer_meta(name):
    m = FNAME_PAT.match(name)
    if not m:
        return {"municipality":"", "ordinance_date":"", "history_id":""}
    return {"municipality":m.group("muni"), "ordinance_date":m.group("date"), "history_id":m.group("hid")}

def match_heading(line):
    if DATE_LINE.match(line):
        return None
    for rx in CODE_COMPILED:
        m = rx.match(line)
        if m:
            code, title = m.group(1).strip(), (m.group(2) or "").strip()
            if not title or len(title) < 3:
                return None
            if SINGLE_LETTER_CODE.match(code):
                return None
            if EXCLUDE_TITLE_PREFIXES.match(title):
                return None
            return code, title
    return None

def find_sections(text):
    lines = text.split("\n")
    offsets, pos = [], 0
    for ln in lines:
        offsets.append(pos)
        pos += len(ln) + 1

    raw, cur = [], None
    for i, ln in enumerate(lines):
        m = match_heading(ln.strip())
        if m:
            code, title = m
            if cur:
                cur["end_line"] = i
                raw.append(cur)
            cur = {"start_line": i, "code": code, "title": title}
    if cur:
        cur["end_line"] = len(lines)
        raw.append(cur)

    out = []
    for s in raw:
        start = offsets[s["start_line"]]
        end = offsets[s["end_line"]] if s["end_line"] < len(offsets) else len(text)
        out.append({"code": s["code"], "title": s["title"], "start": start, "end": end})
    return out

def resolve(base, p):
    p1 = Path(p)
    if p1.exists():
        return p1
    root = Path(base).resolve().parents[1]
    return (root / p).resolve()

def main():
    # ===== Args & folders =====
    ap = argparse.ArgumentParser()
    ap.add_argument("--input_dir", default="input/muni_codes")
    ap.add_argument("--segments_dir", default="segments")
    ap.add_argument("--headings_dir", default="headings")
    ap.add_argument("--neighbors_k", type=int, default=5)
    args = ap.parse_args()

    script_dir = Path(__file__).resolve().parent
    input_dir = resolve(script_dir, args.input_dir)
    segments_dir = resolve(script_dir, args.segments_dir)
    headings_dir = resolve(script_dir, args.headings_dir)
    segments_dir.mkdir(parents=True, exist_ok=True)
    headings_dir.mkdir(parents=True, exist_ok=True)

    print(f"[1] Scanning input dir: {input_dir}")
    files = sorted(input_dir.rglob("*_print.txt"))
    print(f"    Found {len(files)} muni files")

    corpus_counts = defaultdict(int)
    corpus_examples = defaultdict(list)
    perfile = []

    # ===== Per-file processing =====
    for fp in files:
        print(f"\n[2] Processing: {fp.relative_to(input_dir)}")
        txt = fp.read_text(encoding="utf-8", errors="ignore")
        txt = norm_text(txt)
        meta = infer_meta(fp.name)

        secs = find_sections(txt)
        print(f"    Matched sections: {len(secs)}")

        recs, seen = [], set()
        for s in secs:
            body = txt[s["start"]:s["end"]]
            title_norm = norm_title(s["title"])
            recs.append({
                "code": s["code"],
                "title": s["title"],
                "title_normalized": title_norm,
                "start": s["start"],
                "end": s["end"],
                "hash": md5_hex(body),
                "text": body
            })
            if title_norm:
                corpus_counts[title_norm] += 1
                if len(corpus_examples[title_norm]) < 3:
                    corpus_examples[title_norm].append({"code": s["code"], "title": s["title"]})
                seen.add(title_norm)

        if secs:
            sample = [f"- {s['code']} {s['title']}" for s in secs[:5]]
            print("    Sample headings:")
            for line in sample:
                print("      " + line)

        # out_base = f"{meta['municipality']}_{meta['ordinance_date']}_{meta['history_id']}".strip("_")
        # seg_obj = {"file": str(fp), "meta": meta, "sections": recs}
        # (segments_dir / f"{out_base}_sections.json").write_text(json.dumps(seg_obj, ensure_ascii=False, indent=2), encoding="utf-8")
        # perfile.append({"file": str(fp), "unique_headings": len(seen), "total_sections": len(recs)})

    # ===== Corpus for categorization =====
    print("\n[3] Writing corpus CSV for heading categorization")
    all_titles = sorted(corpus_counts.keys(), key=lambda t: corpus_counts[t], reverse=True)
    with (headings_dir / "_corpus_headings.csv").open("w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=[
            "title_normalized","count",
            "example_1_code","example_1_title",
            "example_2_code","example_2_title",
            "example_3_code","example_3_title"
        ])
        w.writeheader()
        for t in all_titles:
            ex = corpus_examples[t]
            w.writerow({
                "title_normalized": t,
                "count": corpus_counts[t],
                "example_1_code": ex[0]["code"] if len(ex)>0 else "",
                "example_1_title": ex[0]["title"] if len(ex)>0 else "",
                "example_2_code": ex[1]["code"] if len(ex)>1 else "",
                "example_2_title": ex[1]["title"] if len(ex)>1 else "",
                "example_3_code": ex[2]["code"] if len(ex)>2 else "",
                "example_3_title": ex[2]["title"] if len(ex)>2 else ""
            })
    print(f"    -> {headings_dir / '_corpus_headings.csv'}")

    # ===== Light fuzzy neighbors =====
    print("[4] Writing neighbor similarity CSV")
    with (headings_dir / "_neighbors.csv").open("w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=["title_normalized","neighbor_title","similarity"])
        w.writeheader()
        for t in all_titles:
            pool = [u for u in all_titles if u != t]
            cands = difflib.get_close_matches(t, pool, n=10, cutoff=0.7)
            scored = [(u, difflib.SequenceMatcher(None, t, u).ratio()) for u in cands]
            scored.sort(key=lambda x: x[1], reverse=True)
            for u, s in scored[:args.neighbors_k]:
                w.writerow({"title_normalized": t, "neighbor_title": u, "similarity": f"{s:.4f}"})
    print(f"    -> {headings_dir / '_neighbors.csv'}")

    # ===== Per-file summary =====
    # print("[5] Writing per-file summary CSV")
    # with (headings_dir / "_perfile_summary.csv").open("w", newline="", encoding="utf-8") as f:
    #     w = csv.DictWriter(f, fieldnames=["file","unique_headings","total_sections"])
    #     w.writeheader()
    #     for r in perfile:
    #         w.writerow(r)
    # print(f"    -> {headings_dir / '_perfile_summary.csv'}")

    print("\n[Done] Headings extracted. Open the corpus CSV to start mapping to your schema.")

if __name__ == "__main__":
    main()