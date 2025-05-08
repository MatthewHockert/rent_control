import pandas as pd
import requests
from bs4 import BeautifulSoup
import re
import time
from datetime import datetime

def extract_dates(text):
    date_pattern = r'\b\d{1,2}-\d{1,2}-\d{2,4}\b'
    matches = re.findall(date_pattern, text)
    dates = []
    for date_str in matches:
        try:
            parsed_date = datetime.strptime(date_str, "%m-%d-%Y")
        except ValueError:
            try:
                parsed_date = datetime.strptime(date_str, "%m-%d-%y")
            except ValueError:
                print(f"Skipping unrecognized date format: {date_str}")
                continue
        dates.append(parsed_date)
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

    print(f"\n[{i+1}/{total}] Processing URL: {url}")
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

            dates = []
            if bracket_block:
                dates = extract_dates(bracket_block)
                print(f"Found {len(dates)} ordinance dates: {[d.strftime('%Y-%m-%d') for d in dates]}")
            else:
                print("No date-containing bracket block found.")
                # Try fallback 1: look for Rent Control Office/Board keyword
                fallback_phrase = df.loc[i, 'Rent Control Office/Board']
                if isinstance(fallback_phrase, str) and fallback_phrase.strip().lower() not in ['nan', 'none', '']:
                    fallback_phrase = fallback_phrase.strip()
                    idx = text.lower().find(fallback_phrase.lower())
                    if idx != -1:
                        bracket_match = re.search(r'\[.*?\]', text[idx:], re.DOTALL)
                        if bracket_match:
                            fallback_block = bracket_match.group(0)
                            dates = extract_dates(fallback_block)
                            print(f"Found {len(dates)} fallback dates after '{fallback_phrase}': {[d.strftime('%Y-%m-%d') for d in dates]}")
                # Try fallback 2: look for "rent control" and next bracket
                if not dates:
                    idx = text.lower().find("rent control")
                    if idx != -1:
                        bracket_match = re.search(r'\[.*?\]', text[idx:], re.DOTALL)
                        if bracket_match:
                            fallback_block = bracket_match.group(0)
                            dates = extract_dates(fallback_block)
                            print(f"Found {len(dates)} fallback dates after 'Rent Control': {[d.strftime('%Y-%m-%d') for d in dates]}")
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