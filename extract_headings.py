import os, re, csv, glob, hashlib
from pathlib import Path

fname_pat = re.compile(r"^(?P<muni>.+)_(?P<date>\d{4}-\d{2}-\d{2})_(?P<hid>[A-Za-z0-9]+)_print\.txt$")
heading_pat = re.compile(r"^\s*(§|Sec\.|Section)\s*([0-9A-Za-z\-\.:]+)\s*(.*)", re.IGNORECASE)
clean_ws = re.compile(r"[ \t]+\n")

SECTION_BUCKETS = {
    "increase": re.compile(r"(allowable|authorized|max(imum)?|cap|limit|rent\s*increase|annual\s*increase|permitted\s*increase|cost\s*of\s*living|CPI|price\s*index|percentage\s*increase)", re.I),
    "vacancy": re.compile(r"(vacanc(y|ies)|turnover|apartment\s+vacated|fair\s*market\s*(value|rent)|decontrol|reset)", re.I),
    "exemption": re.compile(r"(exempt|excluded|not\s*covered|owner[-\s]?occupied|single[-\s]?family|multiple\s*dwellings?|new\s+construction|constructed|built|certificate\s+of\s+occupancy)", re.I),
    "admin": re.compile(r"(board|registration|register|substantial\s+compliance|enforcement|notice|hearing|application|hardship|capital\s+improvement|surcharge|utility|pass[-\s]?through|tax\s+surcharge)", re.I),
}

_POSITIVE_INCREASE_CTX = re.compile(r"\b(annual|allowable|authorized|maximum|max|cap|shall\W*not\W*exceed|rent\W*increase|increase\W*in\W*rent|percentage\W*increase|limit(?:ed)?\W*to|no\W*greater\W*than)\b", re.I)
_NEG_INCOME_CTX = re.compile(r"\b(income|household|median|ami|area\W*median|hud|voucher|eligib|afford|coah|low\W*income|moderate\W*income|section\W*8|tax\W*credit|regional\W*median)\b", re.I)
_CAP_PHRASE = re.compile(r"(?:shall\W*not\W*exceed|no\W*greater\W*than|up\W*to|limited\W*to|may\W*be\W*increased\W*by|maximum\W*increase\W*of|cap\W*of)\s*(?P<num>\d{1,2}(?:\.\d+)?)\s*%", re.I)
_ANY_PCT = re.compile(r"(?P<num>\d{1,2}(?:\.\d+)?)\s*%", re.I)
_LESSER_OF = re.compile(r"(lesser|lower)\s+of\s+(?P<a>\d{1,2}(?:\.\d+)?)\s*%\s*(?:and|or)\s*(?P<b>cpi|consumer\s+price\s+index)", re.I)
_GREATER_OF = re.compile(r"(greater|higher)\s+of\s+(?P<a>\d{1,2}(?:\.\d+)?)\s*%\s*(?:and|or)\s*(?P<b>cpi|consumer\s+price\s+index)", re.I)
_CPI_MULT = re.compile(r"(?P<pct>\d{1,3})(?:\s*%|\s*percent)?\s+of\s+(?:the\s+)?(?:CPI|Consumer\s+Price\s+Index)", re.I)
_ONCE_PER_YEAR = re.compile(r"(once|one)\s+per\s+(calendar\s+)?year|once\s+in\s+(a\s+)?twelve[-\s]?month", re.I)
_FREQ_NUM = re.compile(r"no\s+more\s+than\s+(\d+)\s+increase", re.I)
_VACANCY_FMV = re.compile(r"vacanc\w+.*fair\s+market\s+(value|rent)", re.I|re.S)
_VACANCY_PCT = re.compile(r"vacanc\w+.*?(\d{1,2}(?:\.\d+)?)\s*%", re.I|re.S)
_VACANCY_FORMULA = re.compile(r"vacanc\w+.*?(CPI|formula|index)", re.I|re.S)
_VACANCY_PROHIBIT = re.compile(r"\bno\s+vacancy\s+(increase|decontrol)\b|\bprohibit\w+\s+vacancy", re.I)
_VACANCY_COMPARABLE = re.compile(r"(comparable|same\s+or\s+fewer\s+rooms|highest\s+rent\s+for\s+same\s+or\s+fewer)", re.I)
_CPI_PRESENT = re.compile(r"\b(CPI|Consumer\s+Price\s+Index|price\s*index)\b", re.I)
_HARDSHIP = re.compile(r"\bhardship\b.*(increase|application|appeal)", re.I|re.S)
_CAP_IMPR = re.compile(r"\b(capital|major\s+new)\s+improvement", re.I)
_TAX_SUR = re.compile(r"\btax\s+surcharge|\bproperty\s+tax\s+increase\s+pass-?through", re.I)
_UTIL_PASS = re.compile(r"\butility\s+(?:pass[-\s]?through|surcharge)|\bsubmeter", re.I)
_RENT_BOARD = re.compile(r"(rent\s+(leveling|control)\s+board|landlord[-\s]?tenant\s+affairs\s+board|rent\s+advisory\s+board)", re.I)
_REGISTER = re.compile(r"\bregister\b|\bregistration\b", re.I)
_BLOCKS_INCR = re.compile(r"(no\s+annual\s+increase.*?(substantial\s+compliance|registration)|outstanding\s+notice\s+of\s+violation)", re.I|re.S)
_INCOME_LINK = re.compile(r"\b(AMI|area\s+median\s+income|percent\s+of\s+income|income[-\s]?based|low[-\s]?income|moderate[-\s]?income)\b", re.I)

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

def section_bucket_from_title(title, heading_key):
    t = (title or "").strip()
    for b, pat in SECTION_BUCKETS.items():
        if pat.search(t) or pat.search(heading_key or ""):
            return b
    return None

def _near(text, span, window=160):
    start = max(0, span[0] - window)
    end = min(len(text), span[1] + window)
    return text[start:end]

def _is_income_context(text, span):
    return bool(_NEG_INCOME_CTX.search(_near(text, span)))

def _has_pos_increase_context(text, span):
    return bool(_POSITIVE_INCREASE_CTX.search(_near(text, span)))

def _extract_max_increase_pct_robust(txt):
    if not txt:
        return None
    best = None
    for m in _CAP_PHRASE.finditer(txt):
        if _is_income_context(txt, m.span()):
            continue
        if not _has_pos_increase_context(txt, m.span()):
            continue
        try:
            val = float(m.group("num"))
        except:
            continue
        if 0 < val <= 50:
            best = val if best is None else min(best, val)
    if best is not None:
        return best
    for m in _ANY_PCT.finditer(txt):
        if _is_income_context(txt, m.span()):
            continue
        if not _has_pos_increase_context(txt, m.span()):
            continue
        try:
            val = float(m.group("num"))
        except:
            continue
        if 0 < val <= 50:
            best = val if best is None else min(best, val)
    return best

def classify_doc_type(text):
    t = text.lower()
    if re.search(r"\b(uhac|coah|affordable\s+housing|housing\s+element\s+and\s+fair\s+share|restricted\s+unit|income[-\s]?restricted)\b", t, re.I):
        return "affordable_housing_program"
    if re.search(r"\b(rent\s+control|rent\s+stabilization|rent\s+leveling|landlord[-\s]?tenant\s+affairs)\b", t, re.I):
        return "rent_control"
    return "other"

def extract_numerics_scoped(scoped_texts, whole_text):
    inc = scoped_texts.get("increase") or ""
    corpus = inc if inc.strip() else whole_text
    v_best = _extract_max_increase_pct_robust(corpus)
    lesser = _LESSER_OF.search(corpus)
    greater = _GREATER_OF.search(corpus)
    cpi = 1 if _CPI_PRESENT.search(corpus) else 0
    mult = None
    m_cpi_mult = _CPI_MULT.search(corpus)
    if m_cpi_mult:
        try:
            pct = float(m_cpi_mult.group("pct"))
            if 0 < pct <= 200:
                mult = pct / 100.0
                cpi = 1
        except:
            pass
    if lesser:
        try:
            cap = float(lesser.group("a"))
            v_best = cap if v_best is None else min(v_best, cap)
            cpi = 1
        except:
            pass
    if greater and v_best is None:
        try:
            v_best = float(greater.group("a"))
            cpi = 1
        except:
            pass
    freq = None
    if _ONCE_PER_YEAR.search(corpus):
        freq = 1
    else:
        m = _FREQ_NUM.findall(corpus)
        if m:
            try:
                freq = int(max(m, key=lambda x:int(x)))
            except:
                freq = None
    vac = scoped_texts.get("vacancy") or ""
    vac_decontrol = None
    if _VACANCY_FMV.search(vac) or _VACANCY_PCT.search(vac) or _VACANCY_FORMULA.search(vac):
        vac_decontrol = 1
    if _VACANCY_PROHIBIT.search(corpus) or _VACANCY_PROHIBIT.search(vac):
        vac_decontrol = 0
    vac_rule = "none"
    if _VACANCY_FMV.search(vac):
        vac_rule = "fmv"
    elif _VACANCY_PCT.search(vac):
        vac_rule = "fixed_pct"
    elif _VACANCY_FORMULA.search(vac):
        vac_rule = "formula"
    elif _VACANCY_COMPARABLE.search(vac):
        vac_rule = "comparable_unit"
        vac_decontrol = 0 if vac_decontrol is None else vac_decontrol
    vac_pct = None
    mvp = _VACANCY_PCT.search(vac)
    if mvp:
        try:
            vac_pct = float(mvp.group(1))
        except:
            vac_pct = None
    admin = scoped_texts.get("admin") or whole_text
    hardship = 1 if _HARDSHIP.search(admin) else 0
    cap_impr = 1 if _CAP_IMPR.search(admin) else 0
    tax_sur = 1 if _TAX_SUR.search(admin) else 0
    util_pass = 1 if _UTIL_PASS.search(admin) else 0
    rent_board = 1 if _RENT_BOARD.search(admin) else 0
    registration = 1 if _REGISTER.search(admin) else 0
    blocks_inc = 1 if _BLOCKS_INCR.search(admin) else 0
    return {
        "max_increase_pct": v_best,
        "cpi_linked": cpi,
        "increase_frequency_per_year": freq,
        "vacancy_decontrol": vac_decontrol,
        "vacancy_increase_rule": vac_rule,
        "vacancy_increase_max_pct": vac_pct,
        "hardship_allowed": hardship,
        "capital_improvement_surcharge_allowed": cap_impr,
        "tax_surcharge_allowed": tax_sur,
        "utility_pass_through_allowed": util_pass,
        "rent_board_present": rent_board,
        "registration_required": registration,
        "noncompliance_blocks_increase": blocks_inc,
        "cpi_multiplier": mult,
    }

def extract_coverage_scoped(scoped_texts, whole_text):
    txt = scoped_texts.get("exemption") or whole_text
    wordnum = {"one":1,"two":2,"three":3,"four":4,"five":5,"six":6,"seven":7,"eight":8,"nine":9,"ten":10}
    def to_int_token(tok):
        tok = tok.lower()
        if tok.isdigit():
            return int(tok)
        return wordnum.get(tok, None)
    m = re.search(r"owner-?occupied\s+(?:one|two|three|four|\d+)[-\s]family", txt, re.I)
    def owner_occupied_exempt_units(x):
        if m:
            tok = re.search(r"(one|two|three|four|\d+)", m.group(0), re.I).group(1)
            return to_int_token(tok)
        m2 = re.search(r"owner-?occupied.*?(\d+)\s*(unit|family)", x, re.I)
        if m2:
            try:
                return int(m2.group(1))
            except:
                return None
        return None
    def small_building_exempt_threshold(x):
        m1 = re.search(r"(?:\b(\d+)|\b(one|two|three|four|five|six|seven|eight|nine|ten))\s*(units?|famil(?:y|ies)).{0,40}\b(exempt|excluded|not\s+covered)", x, re.I)
        if m1:
            tok = m1.group(1) or m1.group(2)
            v = to_int_token(tok)
            if v is not None:
                return v
        m2 = re.search(r"buildings?\s+with\s+(?:\b(\d+)|\b(one|two|three|four|five|six|seven|eight|nine|ten))\s+or\s+fewer\s+units?", x, re.I)
        if m2:
            tok = m2.group(1) or m2.group(2)
            v = to_int_token(tok)
            if v is not None:
                return v
        return None
    def min_units_covered(x):
        m1 = re.search(r"(one|two|three|four|five|six|seven|eight|nine|ten|\d+)\s+or\s+more\s+(residential\s+)?(rental\s+)?units?", x, re.I)
        if m1:
            tok = m1.group(1)
            return to_int_token(tok) if not tok.isdigit() else int(tok)
        m2 = re.search(r"containing\s+(one|two|three|four|five|six|seven|eight|nine|ten|\d+)\s+or\s+more\s+units?", x, re.I)
        if m2:
            tok = m2.group(1)
            return to_int_token(tok) if not tok.isdigit() else int(tok)
        return None
    def sf_exempt(x):
        if re.search(r"\bsingle[-\s]?family\b.*\b(exempt|excluded|not\s+covered)", x, re.I):
            return 1
        if re.search(r"owner-?occupied\s+one[-\s]?family.*\b(exempt|excluded|not\s+covered)", x, re.I):
            return 1
        return 0
    def mf_only(x):
        if re.search(r"\bappl(?:y|ies)\s+to\s+multiple\s+dwellings?\b", x, re.I):
            return 1
        return 0
    def year_built_cutoff(x):
        m1 = re.search(r"(constructed|built)\s+(before|prior to)\s+(January\s+\d{1,2},\s+)?(\d{4})", x, re.I)
        if m1:
            try:
                return int(m1.group(4))
            except:
                return None
        m2 = re.search(r"certificate\s+of\s+occupancy.*(before|prior to)\s+(January\s+\d{1,2},\s+)?(\d{4})", x, re.I)
        if m2:
            try:
                return int(m2.group(3))
            except:
                return None
        return None
    def new_construction_exempt(x):
        if re.search(r"\bnew(ly)?\s+construct\w+.*\b(exempt|excluded|not\s+covered)", x, re.I):
            return 1
        return 0
    return {
        "owner_occupied_exempt_units": owner_occupied_exempt_units(txt),
        "small_building_exempt_threshold": small_building_exempt_threshold(txt),
        "min_units_covered": min_units_covered(txt),
        "sf_exempt": sf_exempt(txt),
        "mf_only": mf_only(txt),
        "year_built_cutoff": year_built_cutoff(txt),
        "new_construction_exempt": new_construction_exempt(txt),
    }

def detect_income_linked_cap(text):
    return 1 if _INCOME_LINK.search(text) else 0

def process_file(path, outdir, heading_map, term_rows, discoveries_writer, sections_writer, features_writer, features_fields):
    meta = infer_meta(path)
    text = read_text(path)
    secs = segment_sections(text)
    doc_type = classify_doc_type(text)
    discovered = []
    texts_by_bucket = {"increase": [], "vacancy": [], "exemption": [], "admin": []}
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
        b = section_bucket_from_title(s["title"], s["heading_key"])
        if b:
            texts_by_bucket[b].append(s["text"])
    for dk, dt in {k:v for k,v in discovered}.items():
        discoveries_writer.writerow({
            "municipality":meta["municipality"],
            "ordinance_date":meta["ordinance_date"],
            "history_id":meta["history_id"],
            "file":path,
            "heading_key":dk,
            "example_title":dt
        })
    joined = {k: "\n\n".join(v) for k,v in texts_by_bucket.items()}
    whole_text = "\n\n".join([s["text"] for s in secs]) if secs else text
    if doc_type == "affordable_housing_program":
        base_vars = {k: None for k in ["max_increase_pct","cpi_linked","increase_frequency_per_year","vacancy_decontrol","vacancy_increase_rule","vacancy_increase_max_pct","hardship_allowed","capital_improvement_surcharge_allowed","tax_surcharge_allowed","utility_pass_through_allowed","rent_board_present","registration_required","noncompliance_blocks_increase","cpi_multiplier"]}
        cov_vars = {k: None for k in ["owner_occupied_exempt_units","small_building_exempt_threshold","min_units_covered","sf_exempt","mf_only","year_built_cutoff","new_construction_exempt"]}
    else:
        base_vars = extract_numerics_scoped(joined, whole_text)
        cov_vars = extract_coverage_scoped(joined, whole_text)
    term_flags = detect_terms(whole_text, term_rows) if term_rows else {}
    income_linked_cap = detect_income_linked_cap(whole_text) if doc_type != "rent_control" else 0
    row = {**meta, **base_vars, **cov_vars, **term_flags, "doc_type":doc_type, "income_linked_cap":income_linked_cap}
    out_row = {k: row.get(k, "") for k in features_fields}
    features_writer.writerow(out_row)

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

def open_csv_writer(path_obj, fieldnames):
    f = open(path_obj, "w", newline="", encoding="utf-8")
    w = csv.DictWriter(f, fieldnames=fieldnames, extrasaction="ignore")
    w.writeheader()
    return f, w

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
    if args.harvest_only:
        harvest_headings(os.path.join(args.input, "**", "*_print.txt") if os.path.isdir(args.input) else args.input, args.outdir)
        return
    heading_map = load_heading_map(args.heading_map_csv)
    term_rows = load_term_dict(args.term_dict_csv)
    term_feature_names = sorted({r["feature"] for r in term_rows}) if term_rows else []
    files = []
    if os.path.isdir(args.input):
        files = glob.glob(os.path.join(args.input, "**", "*_print.txt"), recursive=True)
    else:
        files = [args.input]
    base_fields = [
        "municipality","ordinance_date","history_id","doc_type",
        "max_increase_pct","cpi_linked","cpi_multiplier","increase_frequency_per_year",
        "vacancy_decontrol","vacancy_increase_max_pct",
        "hardship_allowed","capital_improvement_surcharge_allowed",
        "rent_board_present",
        "owner_occupied_exempt_units","small_building_exempt_threshold","min_units_covered","sf_exempt","mf_only","year_built_cutoff","new_construction_exempt",
        "income_linked_cap"
    ]
    features_fields = base_fields + [t for t in term_feature_names if t not in base_fields]
    sections_path = Path(args.outdir, "_sections_long.csv")
    discoveries_path = Path(args.outdir, "discoveries.csv")
    features_path = Path(args.outdir, "_features_wide3.csv")
    sections_fields = ["municipality","ordinance_date","history_id","file","section_code","section_title","heading_key","section_chars","section_hash"]
    discoveries_fields = ["municipality","ordinance_date","history_id","file","heading_key","example_title"]
    sections_f, sections_w = open_csv_writer(sections_path, sections_fields)
    disc_f, disc_w = open_csv_writer(discoveries_path, discoveries_fields)
    features_f, features_w = open_csv_writer(features_path, features_fields)
    for p in files:
        process_file(p, args.outdir, heading_map, term_rows, disc_w, sections_w, features_w, features_fields)
    sections_f.close()
    disc_f.close()
    features_f.close()
    harvest_headings(os.path.join(args.input, "**", "*_print.txt") if os.path.isdir(args.input) else args.input, args.outdir)

if __name__ == "__main__":
    main()