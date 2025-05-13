import pandas as pd
import requests
from bs4 import BeautifulSoup
import time
import re
from datetime import datetime


def pick_hyphen_year(a_str, b_str):
    """
    Given two digit‑strings a and b from “a‑b”, try:
       19a, 20a, 19b, 20b
    and pick the one that falls between 1960 and today.
    If both centuries for a are valid, pick the a‑year.
    If a yields nothing, but b does, pick the b‑year.
    """
    current_year = datetime.now().year
    candidates = []
    for part, side in ((a_str, "a"), (b_str, "b")):
        for century in (1900, 2000):
            y = century + int(part)
            if 1960 <= y <= current_year:
                candidates.append((side, y))
    if not candidates:
        return None
    # Prefer any 'a' entries over 'b'
    a_years = [y for side, y in candidates if side=="a"]
    if a_years:
        return min(a_years)  # or max(a_years), but usually there's only one
    # Otherwise fall back to b
    b_years = [y for side, y in candidates if side=="b"]
    return min(b_years)

def extract_dates(text):
    # 1) Full dates anywhere
    date_pattern = r'\b(?:\d{1,2}[/-]){2}\d{2,4}\b'
    full_dates = []
    for m in re.finditer(date_pattern, text):
        s = m.group(0)
        for fmt in ("%m-%d-%Y","%m-%d-%y","%Y-%m-%d","%d-%m-%Y"):
            try:
                d = datetime.strptime(s, fmt)
                if d.year <= datetime.now().year:
                    full_dates.append(d.strftime("%Y-%m-%d"))
                break
            except:
                pass

    year_pattern = r'\b(?:19|20)\d{2}\b'
    contexts = []
    contexts += re.findall(r'\[HISTORY:.*?\]',   text, flags=re.IGNORECASE|re.DOTALL)
    contexts += re.findall(r"Editor's Note:.*", text, flags=re.IGNORECASE)
    contexts += re.findall(r'\([^)]*?(?:19|20)\d{2}[^)]*?\)', text, flags=re.DOTALL)
    contexts += re.findall(r'\[.*?Code.*?\]', text, flags=re.IGNORECASE|re.DOTALL)

    bare_years = set()
    for block in contexts:
        bare_years.update(re.findall(year_pattern, block))

    # 3) Ordinance‑number years
    ord_years = set()
    # match either hyphen or colon between the two parts
    pat = re.compile(r'Ord\.\s*No\.\s*(\d{1,4})\s*[-–:]\s*(\d{1,4})', re.IGNORECASE)
    for left, right in pat.findall(text):
        y = None
        # if one side is 4‑digits, use that
        if len(left) == 4:
            y = int(left)
        elif len(right) == 4:
            y = int(right)
        else:
            # both are 1–4 digits but not 4: if both are 2‑digit, default to left
            if len(left) == 2 and len(right) == 2:
                y = pick_hyphen_year(left, right) or (1900 + int(left))
            else:
                # if one is 3‑ or 1‑digit, you can add more heuristics here;
                # for now, treat as two‑digit → 1900s
                y = 1900 + int(left)

        # keep only plausible years
        if y and 1960 <= y <= datetime.now().year:
            ord_years.add(str(y))
   
    # 4) Build placeholder dates for those years if we don't already have them
    years_with_full = {d[:4] for d in full_dates}
    placeholder_dates = []
    for y in bare_years | ord_years:
        if y not in years_with_full and 1960 <= int(y) <= 2025:
            placeholder_dates.append(f"{y}-01-01")

    # 5) Combine
    return sorted(set(full_dates + placeholder_dates), reverse=True)

def find_sections(text):
    heading_re = re.compile(
        r'(?:^|\n)'                              # start of line
      + r'('
          r'§\s*\d+(?:[-.]\d+)*\s+[A-Z].*?'      # existing §‑heading
      +   r'|'
      +   r'Article\s+[IVXLC]+\s+[A-Za-z ].*?'   # **new** Article II, III, etc.
      + r')\s*(?=\[|§|\n|$)'                    # stop before “[”, “§”, newline or EOS
    , re.MULTILINE)
    matches = list(heading_re.finditer(text))
    sections = []
    for i, m in enumerate(matches):
        heading = m.group(1).strip()
        start   = m.end()
        end     = matches[i+1].start() if i+1 < len(matches) else len(text)
        sections.append((heading, text[start:end]))
    return sections


#Hardship*rent*increase*or*capital*improvement*surcharge|
SECTION_MAP = [
    (re.compile(r'^\d+-4(?:\.\d+)*\b'),   "RentControl"),
    (re.compile(r'^\d+-1\.2\b'),          "Exemptions"),
    (re.compile(r'^\d+-2\.2\b'),          "VacancyIncrease"),
    (re.compile(r'^\d+-2(?:\.1)?\b'),     "RentIncreaseLimit"),  # 2 and 2.1
]

cats = {
    'RentControl':       r'rent control|rent stabili|rent leveling|Powers\s*of\s*Board',
    'RentIncreaseLimit': r'\b(?:Rental\s*Increase|Vacancy\s*Increase|Maximum\s*Rent\s*Increase|Annual\s*Increase|increase\s*allowed|allowable\s*percentage\s*increase|rent\s*increase)\b',
    'Exemptions':        r'\b(?:Exemptions|Exceptions?|Exclusions?|Waivers?|Vacancy\s*Decontrol)\b',
    'UnitsStructure':    r'\b(?:Units\s*in\s*Structure|Covered\s*Units|Dwelling\s*Units|Building\s*Units)\b',
    'VacancyIncrease':   r'\b(?:Termination\s+of\s+occupancy|Vacancy\s+increase|Vacancy\s*Decontrol)\b'
}

HISTORY_BLOCKS = [
    r'\[HISTORY:.*?\]',
    r'\[Prior ordinance history:.*?\]',
    r"Editor's Note:.*?(?=\[\d+\]|§|\Z)",
]

def determine_key_from_heading(sec_no, heading):
    # 1) numeric map (unchanged)…
    if sec_no:
        for pat, cat in SECTION_MAP:
            if pat.match(sec_no):
                return cat

    # 2) Article‑level (new)
    if heading.startswith("Article") and re.search(r'Rent\s+Stabilization|Rent\s+Review|Rent\s+Control', heading, re.IGNORECASE):
        return 'RentControl'

    # 3) keyword fallback (unchanged)…
    for col, pat in cats.items():
        if re.search(pat, heading, re.IGNORECASE):
            return col

    return None

def collect_amendments(text: str) -> dict[str, list[int]]:
    # prepare output buckets
    out = {col: [] for col in cats}

    # 1) grab ANY top‑level history blocks into RentControl
    for pat in HISTORY_BLOCKS:
        for blk in re.findall(pat, text, flags=re.IGNORECASE|re.DOTALL):
            out['RentControl'].append(blk)

    # 2) split into §‑sections
    sections = find_sections(text)

    # 3) walk each section, assign it a key (with numeric→keyword→inherit logic)
    current_parent = None
    for heading, body in sections:
        # pull out the “13-2” part
        m = re.match(r'§\s*([\d.-]+)', heading)
        sec_no = m.group(1) if m else None

        # if this is a top‑level §X (no dot) and its heading matches RentControl,
        # remember it as the current “parent” for inheritance
        if sec_no and '.' not in sec_no:
            if re.search(cats['RentControl'], heading, re.IGNORECASE):
                current_parent = 'RentControl'
            else:
                current_parent = None

        # determine this section’s bucket
        key = determine_key_from_heading(sec_no, heading)

        # if still nothing, inherit from parent
        if not key and current_parent:
            key = current_parent

        if not key:
            continue

        # 4) pull out every [… ] in this section’s body
        for blk in re.findall(r'\[[^\]]+\]', body):
            out[key].append(blk)

    # 5) finally run every collected block through extract_dates()
    parsed = {}
    for col, blks in out.items():
        dates = []
        for blk in blks:
            dates += extract_dates(blk)
        parsed[col] = sorted(set(dates), reverse=True)

    return parsed


print("Loading Excel file…")
df = pd.read_excel('NJ_Rent_Control_Survey_scrape.xlsx', engine='openpyxl')
df.columns = df.columns.str.strip()
assert 'Rent Control Ordinance' in df.columns
#elmwood_df = df[df['Municipality'] == 'Elmwood Park Borough']
for i, row in df.iterrows():
    muni = row['Municipality']
    url  = row['Rent Control Ordinance']
    print(f"[{i+1}/{len(df)}] {muni} → {url}")

    if not isinstance(url, str) or not url.startswith("http"):
        continue

    resp = requests.get(url, headers={'User-Agent':'Mozilla/5.0'})
    text = BeautifulSoup(resp.text, 'html.parser').get_text("\n", strip=True)

    by_cat = collect_amendments(text)
    for col, dates in by_cat.items():
        df.at[i, col] = ";".join(dates)

    time.sleep(1)

na_count = df['RentControl'].isna().sum()
empty_str_count = (df['RentControl'].fillna('').str.strip() == '').sum()
print(f"Missing RentControl entries (NaN): {na_count}")
print(f"Missing RentControl entries (empty string): {empty_str_count}")

df.to_excel('rent_control_raw_dates_by_city.xlsx', index=False)
print("\nDone! Wrote rent_control_raw_dates_by_city.xlsx")

