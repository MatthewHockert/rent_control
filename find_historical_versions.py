import pdfplumber
import re
import os
import csv

input_dir = "/Users/matthewhockert/Desktop/Personal Info/rent_control/html_sources"
output_dir = "parsed_versions"
os.makedirs(output_dir, exist_ok=True)

for filename in os.listdir(input_dir):
    if not filename.endswith(".pdf"):
        continue

    pdf_path = os.path.join(input_dir, filename)
    output_path = os.path.join(output_dir, f"{os.path.splitext(filename)[0]}.csv")

    version_date_pairs = []
    archive_id = None
    city = "UNKNOWN"
    state = ""

    with pdfplumber.open(pdf_path) as pdf:
        all_text = []
        for page in pdf.pages:
            text = page.extract_text()
            if text:
                all_text.extend(text.split("\n"))

        for line in all_text:
            if 'property="og:site_name"' in line:
                city_match = re.search(r'content="([^"]+)"', line)
                if city_match:
                    raw = city_match.group(1).replace(" Code", "").strip()
                    if "," in raw:
                        city, state = [s.strip() for s in raw.split(",", 1)]
                    else:
                        city = raw
                        state = ""

        # Add CURRENT version first
        current_id = None
        current_date = None
        for line in all_text:
            match = re.search(r'href="/([A-Z]{2}\d{4})".*title="\(CURRENT\)(\d{4}-\d{2}-\d{2})"', line)
            if match:
                current_id = match.group(1)
                current_date = match.group(2)
                print(f"Found current version: {current_id} on {current_date}")
                version_date_pairs.append((city, state, current_id, current_date))
                break

        # Continue collecting ARCHIVE entries
        for line in all_text:
            id_match = re.search(r'value="(\d+)"', line)
            date_match = re.search(r'\(ARCHIVE\)\s*(\d{4}-\d{2}-\d{2})', line)

            if id_match:
                archive_id = id_match.group(1)
            elif date_match and archive_id:
                archive_date = date_match.group(1)
                version_date_pairs.append((city, state, archive_id, archive_date))
                archive_id = None

    with open(output_path, "w", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(["city", "state", "version_id", "date"])
        writer.writerows(version_date_pairs)

    print(f"Saved {len(version_date_pairs)} versions to {output_path}")