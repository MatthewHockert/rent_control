#!/usr/bin/env python3
import re, argparse, csv
from pathlib import Path
from collections import defaultdict

CODE_PATTERNS = [
    r"\d{1,4}[:\-]\d{1,4}(?:\.\d+)*\.?",
    r"[A-Za-z0-9]{1,6}(?:-[A-Za-z0-9]{1,6})+(?:\.[A-Za-z0-9]+)*\.?",
    r"\d{1,4}(?:\.\d{1,4}){1,3}\.?",
    r"\d{2,}[A-Za-z]{1,3}",
]
LEADER = r"^\s*(?:§|Sec\.?|Section)?\s*"
CODE_COMPILED = [re.compile(rf"{LEADER}({p})\s+(.*)\s*$", re.IGNORECASE) for p in CODE_PATTERNS]

EXCLUDE_TITLE_PREFIXES = re.compile(r"^(by|ord\.?|editor|editor’s|editor's)\b", re.IGNORECASE)
DATE_LINE = re.compile(r"^\s*\d{1,2}[-/]\d{1,2}[-/]\d{2,4}\s*$")
SINGLE_LETTER_CODE = re.compile(r"^[A-Z]\.?$")

def norm_text(t):
    t = t.replace("\r\n","\n").replace("\r","\n").replace("\xa0"," ")
    t = re.sub(r"\n{3,}", "\n\n", t)
    t = re.sub(r"[ \t]+\n", "\n", t)
    return t

def norm_title(s):
    s = (s or "").strip().lower()
    s = re.sub(r"\s+", " ", s)
    s = re.sub(r"[^\w\s\.:/-]", "", s)
    return s.strip(".: ").strip()

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

def extract_titles(txt):
    titles = []
    for ln in txt.split("\n"):
        m = match_heading(ln.strip())
        if m:
            _, title = m
            tnorm = norm_title(title)
            if tnorm:
                titles.append(tnorm)
    return titles

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--input_dir", default="input/municodes")
    ap.add_argument("--headings_dir", default="headings")
    args = ap.parse_args()

    input_dir = Path(args.input_dir).resolve()
    if not input_dir.exists():
        alt = Path("input/muni_codes").resolve()
        if alt.exists():
            input_dir = alt
        else:
            raise SystemExit(f"Input directory not found: {args.input_dir}")

    headings_dir = Path(args.headings_dir).resolve()
    headings_dir.mkdir(parents=True, exist_ok=True)

    files = sorted(input_dir.rglob("*_print.txt"))
    counts = defaultdict(int)

    for fp in files:
        txt = fp.read_text(encoding="utf-8", errors="ignore")
        txt = norm_text(txt)
        for t in extract_titles(txt):
            counts[t] += 1

    out_csv = headings_dir / "_corpus_headings.csv"
    with out_csv.open("w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=["title_normalized","count"])
        w.writeheader()
        for t in sorted(counts.keys()):
            w.writerow({"title_normalized": t, "count": counts[t]})

    print(f"Done -> {out_csv}")

if __name__ == "__main__":
    main()