import pandas as pd
import requests
from bs4 import BeautifulSoup
import re
import time
from datetime import datetime

def extract_dates(text):
    date_pattern = r'\b\d{1,4}-\d{1,2}-\d{1,4}\b'
    year_pattern = r'\b(?:19|20)\d{2}\b'

    # Find all date-like matches
    matches = [m for m in re.finditer(date_pattern, text)]
    year_matches = re.findall(year_pattern, text)

    dates = []

    for match in matches:
        date_str = match.group(0)
        start_idx = match.start()
        context_before = text[max(0, start_idx - 10):start_idx].lower()

        if "ord. no." in context_before:
            continue  # Skip likely ordinance number

        for fmt in ("%m-%d-%Y", "%m-%d-%y", "%Y-%m-%d", "%d-%m-%Y"):
            try:
                parsed_date = datetime.strptime(date_str, fmt)
                dates.append(parsed_date)
                break
            except ValueError:
                continue
        else:
            print(f"Skipping unrecognized date format: {date_str}")

    for y in set(year_matches):
        try:
            year_date = datetime(int(y), 1, 1)
            if year_date not in dates:
                dates.append(year_date)
        except:
            continue

    return sorted(dates, reverse=True)

print("Loading Excel file...")
df = pd.read_excel(
    '/Users/matthewhockert/Desktop/Personal Info/rent_control/NJ_Rent_Control_Survey_scrape.xlsx',
    engine='openpyxl'
)

df.columns = df.columns.str.strip()

print("Available columns:", df.columns.tolist())

if 'Rent Control Ordinance' not in df.columns:
    raise ValueError("Column 'Rent Control Ordinance' not found. Check the header row or column name.")

all_dates = []
all_repealed_dates = []
total = len(df)
print(f"Processing {total} municipalities...")

for i, url in enumerate(df['Rent Control Ordinance']):
    if not isinstance(url, str) or not url.startswith('http'):
        print(f"\n[{i+1}/{total}] Skipping invalid or non-URL entry: {url}")
        all_dates.append([])
        all_repealed_dates.append(None)
        continue

    municipality = df.loc[i, 'Municipality']
    print(f"\n[{i+1}/{total}] Processing municipality: {municipality}")
    print(f"Processing URL: {url}")
    dates = []
    repealed_date = None

    try:
        headers = {
            'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36'
        }
        response = requests.get(url, timeout=10, headers=headers)
        if response.status_code == 200:
            soup = BeautifulSoup(response.text, 'html.parser')
            text = soup.get_text()

            is_municode = url.startswith("https://library.municode.com/")
            if is_municode:
                print("Detected Municode page.")
                paren_blocks = re.findall(r'\(.*?\)', text, re.DOTALL)
                municode_dates = []
                for block in paren_blocks:
                    if re.search(r'\d{1,2}-\d{1,2}-\d{2,4}', block):
                        municode_dates.extend(extract_dates(block))
                if municode_dates:
                    dates = sorted(municode_dates, reverse=True)
                    print(f"Found {len(dates)} Municode ordinance dates: {[d.strftime('%Y-%m-%d') for d in dates]}")

            if not dates:
                for block in re.findall(r'\[.*?\]', text, re.DOTALL):
                    if 'repealed' in block.lower():
                        match = re.search(r'\d{1,2}-\d{1,2}-\d{2,4}', block)
                        if match:
                            try:
                                repealed_date = datetime.strptime(match.group(0), "%m-%d-%Y")
                            except ValueError:
                                repealed_date = datetime.strptime(match.group(0), "%m-%d-%y")
                            print(f"Found repealed date: {repealed_date.strftime('%Y-%m-%d')}")
                            break

                bracket_block = None
                history_match = re.search(r'\[HISTORY:.*?\]', text, re.DOTALL | re.IGNORECASE)
                if history_match:
                    bracket_block = history_match.group(0)
                    print("Found [HISTORY: ...] block.")
                else:
                    all_brackets = re.findall(r'\[.*?\]', text, re.DOTALL)
                    for block in all_brackets:
                        if re.search(r'\d{1,2}-\d{1,2}-\d{2,4}', block):
                            bracket_block = block
                            print("Found alternative [ ... ] block with date.")
                            break

                if bracket_block:
                    dates = extract_dates(bracket_block)
                    print(f"Found {len(dates)} ordinance dates: {[d.strftime('%Y-%m-%d') for d in dates]}")
                else:
                    print("No date-containing bracket block found. Checking fallback phrases")
                    fallback_phrases = [
                        "Rent Leveling Board", "Division of Landlord Tenant Affairs", "Bayonne Rent Control Board",
                        "Rent Review Board", "Bureau of Rent Control", "Rent Control Board",
                        "Division of Rent Leveling", "Township of East Windsor", "Fair Rental Housing Board",
                        "Administrative Hearing Officer", "Rent Leveling Office", "Township of Gloucester",
                        "Rental Stabilization Board", "Rent Leveling Commission", "Rent Leveling & Stabilization Office",
                        "Mobile Home Rent Control Board", "Office of Landlord Tenant Relations", "Rent Stabilization Board",
                        "Rent Advisory Board", "Rent Stabilization Board", "Rent Control Office",
                        "Mobile Home Park Rent Leveling Board", "Rent Leveling Department", "Office of Rent Control",
                        "Rent Leveling and Control Board", "Office of Rent Leveling", "Rent Stabilization Commission",
                        "Randolph Township", "Rent Leveling Commission/Tenant Advocate", "Rent Board",
                        "Administrative Monitoring Officer", "Mobile Home Rent Stabilization & Control Board",
                        "Multiple Dwellings Regulation Board"
                    ]
                    bracket_matches = list(re.finditer(r'\[.*?\]', text, re.DOTALL))
                    for match in bracket_matches:
                        start_idx = match.start()
                        context_window = text[max(0, start_idx - 200):start_idx].lower()
                        for phrase in fallback_phrases:
                            if phrase.lower() in context_window:
                                fallback_block = match.group(0)
                                dates = extract_dates(fallback_block)
                                if dates:
                                    print(f"Found {len(dates)} fallback dates near '{phrase}': {[d.strftime('%Y-%m-%d') for d in dates]}")
                                    break
                        if dates:
                            break
        else:
            print(f"Failed to fetch page (status {response.status_code})")
    except Exception as e:
        print(f"Error fetching or parsing URL: {e}")

    all_dates.append(dates)
    all_repealed_dates.append(repealed_date)
    time.sleep(1.5)

max_dates = max((len(d) for d in all_dates), default=0)
print(f"\nMaximum number of ordinance dates found: {max_dates}")
print("Adding columns to DataFrame...")

for j in range(max_dates):
    col_name = f'ordinance_date_{j+1}'
    df[col_name] = [d[j].strftime('%Y-%m-%d') if len(d) > j else None for d in all_dates]

df['repealed_date'] = [d.strftime('%Y-%m-%d') if d else None for d in all_repealed_dates]

output_path = '/Users/matthewhockert/Desktop/Personal Info/rent_control/NJ_Rent_Control_Survey_with_dates.xlsx'
df.to_excel(output_path, index=False)
print(f"Done! Results written to: {output_path}")