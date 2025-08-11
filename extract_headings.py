

import os, re, csv, glob, hashlib
from pathlib import Path

fname_pat = re.compile(r"^(?P<muni>.+)_(?P<date>\d{4}-\d{2}-\d{2})_(?P<hid>[A-Za-z0-9]+)_print\.txt$")
heading_pat = re.compile(r"^\s*(§|Sec\.|Section)\s*([0-9A-Za-z\-\.:]+)\s*(.*)", re.IGNORECASE)
clean_ws = re.compile(r"[ \t]+\n")

def read_text(p):
    t = Path(p).read_text(encoding="utf-8", errors="ignore")
    t = t.replace("\r\n","\n").replace("\r","\n")
    t = re.sub(r"\n{3,}", "\n\n", t)
    t = clean_ws.sub("\n", t)
    return t.strip()

def infer_meta(path):
    b = Path(path).name
    m = fname_pat.match(b)
    if not m:
        return {"municipality":"", "ordinance_date":"", "history_id":""}
    return {"municipality":m.group("muni"), "ordinance_date":m.group("date"), "history_id":m.group("hid")}

def segment_sections(text):
    lines = text.split("\n")
    sections = []
    buf = []
    cur = {"code":"","title":""}
    for ln in lines:
        if heading_pat.match(ln.strip()):
            if buf:
                sections.append({"code":cur["code"], "title":cur["title"], "text":"\n".join(buf).strip()})
                buf = []
            m = heading_pat.match(ln.strip())
            cur = {"code":m.group(2).strip(), "title":m.group(3).strip()}
        else:
            buf.append(ln)
    if buf:
        sections.append({"code":cur["code"], "title":cur["title"], "text":"\n".join(buf).strip()})
    return sections

def slugify(s):
    s = s.lower()
    s = re.sub(r"[^a-z0-9]+","_",s).strip("_")
    s = re.sub(r"_+","_",s)
    return s[:80] if s else "unknown"

def load_heading_map(csv_path):
    if not csv_path or not Path(csv_path).exists():
        return {}
    mp = {}
    with open(csv_path, "r", encoding="utf-8") as f:
        r = csv.DictReader(f)
        for row in r:
            if row.get("pattern") and row.get("canonical"):
                mp[row["pattern"]] = row["canonical"]
    return mp

def canonical_heading(title, mapping):
    t = (title or "").strip().lower()
    for k,v in mapping.items():
        if re.search(k, t, re.IGNORECASE):
            return v, True
    return slugify(title or ""), False

def load_term_dict(path):
    if not path or not Path(path).exists():
        return []
    rows = []
    with open(path, "r", encoding="utf-8") as f:
        r = csv.DictReader(f)
        for row in r:
            if row.get("term") and row.get("feature"):
                rows.append({"term":row["term"], "feature":row["feature"]})
    return rows

def detect_terms(text, term_rows):
    feats = {}
    for tr in term_rows:
        pat = tr["term"]
        f = tr["feature"]
        feats.setdefault(f, 0)
        if re.search(pat, text, re.IGNORECASE|re.DOTALL):
            feats[f] = 1
    return feats

# -------------------------
# Robust % cap extraction
# -------------------------

# Positive rent-increase context near the % number
_POSITIVE_INCREASE_CTX = re.compile(
    r"""(?ix)
    \b(
      annual|allowable|authorized|maximum|max|cap|shall\W*not\W*exceed|
      rent\W*increase|increase\W*in\W*rent|percentage\W*increase|
      limit(?:ed)?\W*to|no\W*greater\W*than
    )\b
    """
)

# Income/eligibility/affordability context to EXCLUDE
_NEG_INCOME_CTX = re.compile(
    r"""(?ix)
    \b(
      income|household|median|ami|area\W*median|hud|voucher|eligib|afford|
      coah|low\W*income|moderate\W*income|section\W*8|tax\W*credit|
      inclusionary|regional\W*median
    )\b
    """
)

# Tighter phrases like "shall not exceed 4%" etc.
_CAP_PHRASE = re.compile(
    r"""(?ix)
    (?:shall\W*not\W*exceed|no\W*greater\W*than|up\W*to|limited\W*to|
       may\W*be\W*increased\W*by|maximum\W*increase\W*of|cap\W*of)
    \s*(?P<num>\d{1,2}(?:\.\d+)?)
    \s*%
    """
)

# Generic percent fallback (used with positive/negative context checks)
_ANY_PCT = re.compile(r"(?i)(?P<num>\d{1,2}(?:\.\d+)?)\s*%")

def _near(text: str, span, window: int = 140):
    start = max(0, span[0] - window)
    end = min(len(text), span[1] + window)
    return text[start:end]

def _is_income_context(text: str, span) -> bool:
    return bool(_NEG_INCOME_CTX.search(_near(text, span)))

def _has_pos_increase_context(text: str, span) -> bool:
    return bool(_POSITIVE_INCREASE_CTX.search(_near(text, span)))

def _extract_max_increase_pct_robust(txt: str):
    if not txt:
        return None

    # Pass 1: tight cap phrases AND positive increase context AND not income context
    best = None
    for m in _CAP_PHRASE.finditer(txt):
        if _is_income_context(txt, m.span()):
            continue
        if not _has_pos_increase_context(txt, m.span()):
            continue
        try:
            val = float(m.group("num"))
        except Exception:
            continue
        if 0 < val <= 50:
            best = val if best is None else min(best, val)
    if best is not None:
        return best

    # Pass 2: any percent, but must be in positive increase context and not income context
    for m in _ANY_PCT.finditer(txt):
        if _is_income_context(txt, m.span()):
            continue
        if not _has_pos_increase_context(txt, m.span()):
            continue
        try:
            val = float(m.group("num"))
        except Exception:
            continue
        if 0 < val <= 50:
            best = val if best is None else min(best, val)

    return best

def extract_numerics(text):
    def max_increase_pct(txt):
        return _extract_max_increase_pct_robust(txt)

    def cpi_linked(txt):
        return int(bool(re.search(r"\bCPI\b|\bConsumer\s+Price\s+Index\b", txt, re.IGNORECASE)))

    def increase_freq(txt):
        m = re.search(r"increase(?:s)?\s+(?:no\s+more\s+than|not\s+more\s+than)\s+once\s+in\s+a\s+twelve[- ]month", txt, re.IGNORECASE)
        if m:
            return 1
        m2 = re.findall(r"no\s+more\s+than\s+(\d+)\s+increase", txt, re.IGNORECASE)
        if m2:
            try:
                return int(max(m2, key=lambda x:int(x)))
            except:
                return None
        return None

    def vacancy_decontrol(txt):
        if re.search(r"\bupon\s+vacat\w+.*(increase|reset).*(fair\s+market|market\s+value|market\s+rent)", txt, re.IGNORECASE|re.DOTALL):
            return 1
        if re.search(r"\bno\s+vacancy\s+(increase|decontrol)\b|\bprohibit\w+\s+vacancy", txt, re.IGNORECASE):
            return 0
        return None

    def vacancy_rule(txt):
        if re.search(r"\bfair\s+market\s+value\b", txt, re.IGNORECASE):
            return "fmv"
        if re.search(r"vacanc\w+.*?\b([0-9]+(?:\.[0-9]+)?)\s*%", txt, re.IGNORECASE):
            return "fixed_pct"
        if re.search(r"vacanc\w+.*?(CPI|formula|index)", txt, re.IGNORECASE):
            return "formula"
        return "none"

    def vacancy_pct(txt):
        m = re.search(r"vacanc\w+.*?\b([0-9]+(?:\.[0-9]+)?)\s*%", txt, re.IGNORECASE|re.DOTALL)
        if m:
            try:
                return float(m.group(1))
            except:
                return None
        return None

    def hardship_allowed(txt):
        return int(bool(re.search(r"\bhardship\b.*(increase|application|appeal)", txt, re.IGNORECASE)))

    def capital_impr_allowed(txt):
        return int(bool(re.search(r"\b(capital|major\s+new)\s+improvement", txt, re.IGNORECASE)))

    def tax_surcharge_allowed(txt):
        return int(bool(re.search(r"\btax\s+surcharge|\bproperty\s+tax\s+increase\s+pass-?through", txt, re.IGNORECASE)))

    def utility_pass_through_allowed(txt):
        return int(bool(re.search(r"\butility\s+(?:pass[- ]through|surcharge)|\bsubmeter", txt, re.IGNORECASE)))

    def rent_board_present(txt):
        return int(bool(re.search(r"Rent\s+(Leveling|Control)\s+Board", txt, re.IGNORECASE)))

    def registration_required(txt):
        return int(bool(re.search(r"\bregister\b|\bregistration\b", txt, re.IGNORECASE)))

    def noncompliance_blocks_increase(txt):
        return int(bool(re.search(r"no\s+annual\s+increase.*?(?:substantial\s+compliance|registration)", txt, re.IGNORECASE)))

    return {
        "max_increase_pct": max_increase_pct(text),
        "cpi_linked": cpi_linked(text),
        # REMOVED: "cpi_formula_window_months"
        "increase_frequency_per_year": increase_freq(text),
        # REMOVED: banking flags
        "vacancy_decontrol": vacancy_decontrol(text),
        "vacancy_increase_rule": vacancy_rule(text),
        "vacancy_increase_max_pct": vacancy_pct(text),
        "hardship_allowed": hardship_allowed(text),
        "capital_improvement_surcharge_allowed": capital_impr_allowed(text),
        "tax_surcharge_allowed": tax_surcharge_allowed(text),
        "utility_pass_through_allowed": utility_pass_through_allowed(text),
        "rent_board_present": rent_board_present(text),
        "registration_required": registration_required(text),
        "noncompliance_blocks_increase": noncompliance_blocks_increase(text),
    }

def extract_coverage(text):
    wordnum = {"one":1,"two":2,"three":3,"four":4,"five":5,"six":6,"seven":7,"eight":8,"nine":9,"ten":10}
    def to_int_token(tok):
        tok = tok.lower()
        if tok.isdigit():
            return int(tok)
        return wordnum.get(tok, None)
    def owner_occupied_exempt_units(txt):
        m = re.search(r"owner-?occupied\s+(?:one|two|three|four|\d+)[-\s]family", txt, re.IGNORECASE)
        if m:
            tok = re.search(r"(one|two|three|four|\d+)", m.group(0), re.IGNORECASE).group(1)
            return to_int_token(tok)
        m2 = re.search(r"owner-?occupied.*?(\d+)\s*(unit|family)", txt, re.IGNORECASE)
        if m2:
            try:
                return int(m2.group(1))
            except:
                return None
        return None
    def small_building_exempt_threshold(txt):
        m = re.search(r"(?:\b(\d+)|\b(one|two|three|four|five|six|seven|eight|nine|ten))\s*(units?|famil(?:y|ies)).{0,40}\b(exempt|excluded|not\s+covered)", txt, re.IGNORECASE)
        if m:
            tok = m.group(1) or m.group(2)
            v = to_int_token(tok)
            if v is not None:
                return v
        m2 = re.search(r"buildings?\s+with\s+(?:\b(\d+)|\b(one|two|three|four|five|six|seven|eight|nine|ten))\s+or\s+fewer\s+units?", txt, re.IGNORECASE)
        if m2:
            tok = m2.group(1) or m2.group(2)
            v = to_int_token(tok)
            if v is not None:
                return v
        return None
    def min_units_covered(txt):
        m = re.search(r"(one|two|three|four|five|six|seven|eight|nine|ten|\d+)\s+or\s+more\s+(residential\s+)?(rental\s+)?units?", txt, re.IGNORECASE)
        if m:
            tok = m.group(1)
            return to_int_token(tok) if not tok.isdigit() else int(tok)
        m2 = re.search(r"containing\s+(one|two|three|four|five|six|seven|eight|nine|ten|\d+)\s+or\s+more\s+units?", txt, re.IGNORECASE)
        if m2:
            tok = m2.group(1)
            return to_int_token(tok) if not tok.isdigit() else int(tok)
        return None
    def sf_exempt(txt):
        if re.search(r"\bsingle[-\s]?family\b.*\b(exempt|excluded|not\s+covered)", txt, re.IGNORECASE):
            return 1
        if re.search(r"owner-?occupied\s+one[-\s]?family.*\b(exempt|excluded|not\s+covered)", txt, re.IGNORECASE):
            return 1
        return 0
    def mf_only(txt):
        if re.search(r"\bappl(?:y|ies)\s+to\s+multiple\s+dwellings?\b", txt, re.IGNORECASE):
            return 1
        return 0
    def year_built_cutoff(txt):
        m = re.search(r"(constructed|built)\s+(before|prior to)\s+(January\s+\d{1,2},\s+)?(\d{4})", txt, re.IGNORECASE)
        if m:
            try:
                return int(m.group(4))
            except:
                return None
        m2 = re.search(r"certificate\s+of\s+occupancy.*(before|prior to)\s+(January\s+\d{1,2},\s+)?(\d{4})", txt, re.IGNORECASE)
        if m2:
            try:
                return int(m2.group(3))
            except:
                return None
        return None
    def new_construction_exempt(txt):
        if re.search(r"\bnew(ly)?\s+construct\w+.*\b(exempt|excluded|not\s+covered)", txt, re.IGNORECASE):
            return 1
        return 0
    return {
        "owner_occupied_exempt_units": owner_occupied_exempt_units(text),
        "small_building_exempt_threshold": small_building_exempt_threshold(text),
        "min_units_covered": min_units_covered(text),
        "sf_exempt": sf_exempt(text),
        "mf_only": mf_only(text),
        "year_built_cutoff": year_built_cutoff(text),
        "new_construction_exempt": new_construction_exempt(text),
    }

def harvest_headings(input_glob, outdir):
    all_titles = {}
    for p in glob.glob(input_glob, recursive=True):
        if not p.endswith("_print.txt"):
            continue
        text = read_text(p)
        secs = segment_sections(text)
        for s in secs:
            t = (s["title"] or "").strip()
            key = slugify(t)
            all_titles.setdefault(key, {"title":t, "count":0})
            all_titles[key]["count"] += 1
    out = Path(outdir, "_headings_catalog.csv")
    with open(out, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=["heading_key","example_title","count"])
        w.writeheader()
        for k,v in sorted(all_titles.items(), key=lambda x: (-x[1]["count"], x[0])):
            w.writerow({"heading_key":k, "example_title":v["title"], "count":v["count"]})
    return out

def process_file(path, outdir, heading_map, term_rows, discoveries_writer, sections_writer, features_writer, features_fields):
    meta = infer_meta(path)
    text = read_text(path)
    secs = segment_sections(text)
    discovered = []
    for s in secs:
        k, matched = canonical_heading(s["title"], heading_map)
        s["heading_key"] = k or "unknown"
        s["chars"] = len(s["text"])
        s["hash"] = hashlib.md5(s["text"].encode("utf-8")).hexdigest()
        sections_writer.writerow({
            "municipality":meta["municipality"],
            "ordinance_date":meta["ordinance_date"],
            "history_id":meta["history_id"],
            "file":path,
            "section_code":s["code"],
            "section_title":s["title"],
            "heading_key":s["heading_key"],
            "section_chars":s["chars"],
            "section_hash":s["hash"]
        })
        if not matched:
            discovered.append((s["heading_key"], s["title"] or ""))
    for dk, dt in {k:v for k,v in discovered}.items():
        discoveries_writer.writerow({
            "municipality":meta["municipality"],
            "ordinance_date":meta["ordinance_date"],
            "history_id":meta["history_id"],
            "file":path,
            "heading_key":dk,
            "example_title":dt
        })
    join_text = "\n\n".join([s["text"] for s in secs]) if secs else text
    base_vars = extract_numerics(join_text)
    cov_vars = extract_coverage(join_text)
    flags = {}
    for s in secs:
        hk = s["heading_key"] or "unknown"
        flags[f"has_section__{hk}"] = 1
    term_flags = detect_terms(join_text, term_rows) if term_rows else {}
    row = {**meta, **base_vars, **cov_vars, **flags, **term_flags}
    out_row = {k: row.get(k, "") for k in features_fields}
    features_writer.writerow(out_row)

def main():
    import argparse
    ap = argparse.ArgumentParser()
    ap.add_argument("--input", required=True)
    ap.add_argument("--outdir", default="nlp_outputs")
    ap.add_argument("--heading_map_csv", default="")
    ap.add_argument("--term_dict_csv", default="")
    ap.add_argument("--harvest_only", action="store_true")
    args = ap.parse_args()
    Path(args.outdir).mkdir(parents=True, exist_ok=True)
    files = []
    if os.path.isdir(args.input):
        files = glob.glob(os.path.join(args.input, "**", "*_print.txt"), recursive=True)
    else:
        files = [args.input]
    if args.harvest_only:
        harvest_headings(os.path.join(args.input, "**", "*_print.txt") if os.path.isdir(args.input) else args.input, args.outdir)
        return
    heading_map = load_heading_map(args.heading_map_csv)
    term_rows = load_term_dict(args.term_dict_csv)
    term_feature_names = sorted({r["feature"] for r in term_rows}) if term_rows else []
    flag_cols = set()
    for p in files:
        text = read_text(p)
        secs = segment_sections(text)
        for s in secs:
            hk, _ = canonical_heading(s["title"], heading_map)
            hk = hk or "unknown"
            flag_cols.add(f"has_section__{hk}")

    # FINAL slim features (removed: cpi_formula_window_months, banking_allowed, banking_limit_pct)
    base_fields = [
        "municipality","ordinance_date","history_id",
        "max_increase_pct","cpi_linked","increase_frequency_per_year",
        "vacancy_decontrol","vacancy_increase_rule","vacancy_increase_max_pct",
        "hardship_allowed","capital_improvement_surcharge_allowed","tax_surcharge_allowed","utility_pass_through_allowed",
        "rent_board_present","registration_required","noncompliance_blocks_increase",
        "owner_occupied_exempt_units","small_building_exempt_threshold","min_units_covered","sf_exempt","mf_only","year_built_cutoff","new_construction_exempt"
    ]

    features_fields = base_fields + sorted(flag_cols) + term_feature_names

    sections_path = Path(args.outdir, "_sections_long.csv")
    discoveries_path = Path(args.outdir, "discoveries.csv")
    features_path = Path(args.outdir, "_features_wide.csv")
    sections_fields = ["municipality","ordinance_date","history_id","file","section_code","section_title","heading_key","section_chars","section_hash"]
    discoveries_fields = ["municipality","ordinance_date","history_id","file","heading_key","example_title"]
    sections_new = not sections_path.exists()
    disc_new = not discoveries_path.exists()
    features_new = not features_path.exists()
    sections_f = open(sections_path, "a", newline="", encoding="utf-8")
    sections_w = csv.DictWriter(sections_f, fieldnames=sections_fields)
    if sections_new:
        sections_w.writeheader()
    disc_f = open(discoveries_path, "a", newline="", encoding="utf-8")
    disc_w = csv.DictWriter(disc_f, fieldnames=discoveries_fields)
    if disc_new:
        disc_w.writeheader()
    features_f = open(features_path, "a", newline="", encoding="utf-8")
    features_w = csv.DictWriter(features_f, fieldnames=features_fields)
    if features_new:
        features_w.writeheader()
    for p in files:
        process_file(p, args.outdir, heading_map, term_rows, disc_w, sections_w, features_w, features_fields)
    sections_f.close()
    disc_f.close()
    features_f.close()
    harvest_headings(os.path.join(args.input, "**", "*_print.txt") if os.path.isdir(args.input) else args.input, args.outdir)

if __name__ == "__main__":
    main()