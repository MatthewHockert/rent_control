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
    (re.compile(r'^\d+-2(?:\.1)?\b'),     "RentIncreaseLimit"),  
]

cats = {
    'RentControl':         r'rent control|rent stabili|rent leveling|Powers\s*of\s*Board',
    'RentIncreaseLimit':   r'\b(?:Rental\s*Increase|Vacancy\s*Increase|Maximum\s*Rent\s*Increase|Annual\s*Increase|increase\s*allowed|allowable\s*percentage\s*increase|rent\s*increase)\b',
    'Exemptions':          r'\b(?:Exemptions|Exceptions?|Exclusions?|Waivers?|Vacancy\s*Decontrol)\b',
    'UnitsStructure':      r'\b(?:Units\s*in\s*Structure|Covered\s*Units|Dwelling\s*Units|Building\s*Units)\b',
    'VacancyIncrease':     r'\b(?:Termination\s+of\s+occupancy|Vacancy\s+increase|Vacancy\s*Decontrol)\b',
    'VacancyControl':      r'\b(?:vacancy\s+control|vacancy\s+decontrol|vacancy\s+adjustment|vacancy\s+rent|new\s+tenancy\s+rent)\b',
    'HardshipIncreases':   r'\b(?:hardship|fair\s+return|insufficient\s+return|net\s+operating\s+income|NOI)\b',
    'CapitalImprovements': r'\b(?:capital\s+improvement|major\s+repairs|building\s+upgrade|rehabilitation\s+costs?)\b',
    'AdministrativeProcedures': r'\b(?:hearing\s+officer|Rent\s+Control\s+Board|enforcement\s+officer|board\s+of\s+adjustment|appeals?)\b',
    'EvictionControls':    r'\b(?:just\s+cause\s+eviction|eviction\s+controls?|cause\s+for\s+termination|grounds\s+for\s+eviction)\b',
    'RegistrationRequirements': r'\b(?:unit\s+registration|annual\s+registration|filing\s+requirement|landlord\s+registration)\b',
    'FeeSchedules':        r'\b(?:registration\s+fee|administrative\s+fee|filing\s+fee|per\s+unit\s+fee)\b',
    'ExpirationOrSunset':  r'\b(?:sunset\s+provision|expiration\s+date|temporary\s+ordinance|review\s+period)\b',
    'Definitions':         r'\bdefinitions\b'
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

def collect_amendments(text):
    out = {col: [] for col in cats}
    out["UnmatchedSections"] = []

    history_and_ords = re.findall(
        r"\[[^\]]*(?:HISTORY:|Editor's Note:|Prior ordinance history|Ord\.\s*No\.|Ordinance Nos?\.)[^\]]*\]",
        text, flags=re.IGNORECASE | re.DOTALL
    )
    for blk in set(history_and_ords):
        if 'Ordinance Nos' in blk:
            for left, right in re.findall(r'(\d{1,4})\s*[-–:]\s*(\d{1,4})', blk):
                year = pick_hyphen_year(left, right)
                if year:
                    out['RentControl'].append(f"{year}-01-01")
        else:
            out['RentControl'].extend(extract_dates(blk))

    sections = find_sections(text)
    for heading, body in sections:
        key = None
        sec_no = None  # ← ensure it's defined
        m = re.match(r'§\s*([\d.-]+)', heading)
        if m:
            sec_no = m.group(1)
            for pat, cat in SECTION_MAP:
                if pat.match(sec_no):
                    key = cat
                    break

        if key is None:
            for col, pat in cats.items():
                if re.search(pat, heading, flags=re.IGNORECASE):
                    key = col
                    break

        if not key:
            out["UnmatchedSections"].append(f"{sec_no or '?'}: {heading.strip()}")
            continue

        for blk in re.findall(r'\[[^\]]+\]', body):
            out[key].append(blk)

    parsed = {}
    for col, blks in out.items():
        if col == "UnmatchedSections":
            parsed[col] = sorted(set(blks))
            continue

        existing_dates = [d for d in blks if re.match(r"\d{4}-\d{2}-\d{2}", d)]
        more_dates = []
        for blk in blks:
            if not re.match(r"\d{4}-\d{2}-\d{2}", blk):
                more_dates += extract_dates(blk)
        all_dates = existing_dates + more_dates
        parsed[col] = sorted(set(all_dates), reverse=True)

    return parsed

    
print("Loading Excel file…")
df = pd.read_excel('NJ_Rent_Control_Survey_scrape.xlsx', engine='openpyxl')
for col in cats:
    if col not in df.columns:
        df[col] = ""
if "UnmatchedSections" not in df.columns:
    df["UnmatchedSections"] = ""
    
df.columns = df.columns.str.strip()
assert 'Rent Control Ordinance' in df.columns
#elmwood_df = df[df['Municipality'] == 'Elmwood Park Borough']
#woodland_df = df[df['Municipality'] == 'Woodland Park Borough']

for i, row in df.iterrows():
    muni = row['Municipality']
    url  = row['Rent Control Ordinance']
    print(f"[{i+1}/{len(df)}] {muni} → {url}")

    if not isinstance(url, str) or not url.startswith("http"):
        continue

    resp = requests.get(url, headers={'User-Agent':'Mozilla/5.0'})
    text = BeautifulSoup(resp.text, 'html.parser').get_text("\n", strip=True)

    by_cat = collect_amendments(text)
    #print(by_cat)
    for col, dates in by_cat.items():
        df.at[i, col] = ";".join(dates)

    time.sleep(1)

na_count = df['RentControl'].isna().sum()
empty_str_count = (df['RentControl'].fillna('').str.strip() == '').sum()
print(f"Missing RentControl entries (NaN): {na_count}")
print(f"Missing RentControl entries (empty string): {empty_str_count}")

unmatched_cols = [
    'Municipality', 'County', 'Rent Control Office/Board', 'Rent Control Ordinance', 'UnmatchedSections'
]
matched_cols = [
    'Municipality', 'County', 'Rent Control Office/Board', 'Rent Control Ordinance'
] + list(cats.keys())

# Split out unmatched rows
only_unmatched = df[
    (df["UnmatchedSections"].str.strip() != "") &
    (df[list(cats.keys())].apply(lambda row: all(cell.strip() == "" for cell in row.fillna("")), axis=1))
]

# Write unmatched sections to separate file
only_unmatched[unmatched_cols].to_excel("unmatched_sections.xlsx", index=False)
print(f"Wrote {len(only_unmatched)} rows with only unmatched sections to unmatched_sections.xlsx")

# Drop unmatched sections column from main file and write
df[matched_cols].to_excel('rent_control_raw_dates_by_city.xlsx', index=False)
print("Done! Wrote rent_control_raw_dates_by_city.xlsx")

