import os
import csv
import requests
import browser_cookie3
import time
import random
import re

parsed_folder = "parsed_versions"
output_root = "muni_codes"

cookies = browser_cookie3.chrome()
chrome_user_agent = (
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 13_5_2) "
    "AppleWebKit/537.36 (KHTML, like Gecko) "
    "Chrome/125.0.6422.113 Safari/537.36"
)

session = requests.Session()
session.cookies = cookies
session.headers.update({"User-Agent": chrome_user_agent})

def fetch_with_retries(url, retries=5):
    for attempt in range(retries):
        r = session.get(url)
        if r.status_code == 200:
            return r
        elif r.status_code == 429:
            wait = 2 ** attempt + random.uniform(0, 1)
            print(f"429 Too Many Requests. Backing off {wait:.1f} seconds…")
            time.sleep(wait)
        else:
            print(f"Request failed with {r.status_code}")
            return r
    return None

for filename in os.listdir(parsed_folder):
    if not filename.endswith(".csv"):
        continue

    base_page_id_match = re.search(r"_(\d+)\.csv$", filename)
    if not base_page_id_match:
        print(f"❌ Skipping file with no numeric ID: {filename}")
        continue

    base_page_id = base_page_id_match.group(1)
    csv_path = os.path.join(parsed_folder, filename)

    with open(csv_path, newline="") as f:
        rows = list(csv.DictReader(f))

    if not rows:
        print(f"No entries in {filename}. Skipping.")
        continue

    city = rows[0]["city"].replace(" ", "_").lower()
    output_folder = os.path.join(output_root, city)
    os.makedirs(output_folder, exist_ok=True)

    for idx, row in enumerate(rows, start=1):
        version_id = row["version_id"]
        date = row["date"].replace("/", "-")
        base_filename = f"{city}_{date}_{version_id}_print.html"
        output_path = os.path.join(output_folder, base_filename)

        print_url = f"https://ecode360.com/print/{version_id}?guid={base_page_id}"
        print(f"[{filename} {idx}/{len(rows)}] Downloading print version from {print_url}…")

        r = fetch_with_retries(print_url)
        if r and r.status_code == 200:
            with open(output_path, "w", encoding="utf-8") as f:
                f.write(r.text)
            print(f"✔ Saved to {output_path}")
        else:
            print("✖ Print view failed.")

        time.sleep(random.uniform(8, 12))