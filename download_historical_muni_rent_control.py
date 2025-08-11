import os
import time
import requests
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.by import By

municipality_name = "Highland Park Borough"
base_page_id = "10613114"
base_url = f"https://ecode360.com/{base_page_id}"
download_template = "https://ecode360.com/output/word_html/{}"
archive_template = "https://ecode360.com/{}/" + base_page_id

output_folder = municipality_name.replace(" ", "_")
os.makedirs(output_folder, exist_ok=True)

options = Options()
options.add_argument("--headless")
options.add_argument("--disable-gpu")
driver = webdriver.Chrome(options=options)
driver.get(base_url)
time.sleep(5)

try:
    version_select = driver.find_element(By.ID, "version-selector")
    version_options = version_select.find_elements(By.TAG_NAME, "option")
    archive_versions = [
        (opt.get_attribute("value"), opt.text.strip())
        for opt in version_options
        if "ARCHIVE" in opt.text
    ]
except Exception as e:
    print(f"Error extracting version list: {e}")
    driver.quit()
    exit()

driver.quit()

if not archive_versions:
    print(f"No historical versions found for {municipality_name}")
    exit()

print(f"Found {len(archive_versions)} historical versions for {municipality_name}")

for i, (version_id, version_label) in enumerate(archive_versions, 1):
    version_label = version_label.replace(" ", "_").replace("*", "").replace("/", "-").replace("(", "").replace(")", "")
    doc_url = download_template.format(version_id)
    filename = os.path.join(output_folder, f"{version_label}_{version_id}.doc")

    try:
        print(f"[{i}/{len(archive_versions)}] Downloading version {version_label} ...")
        response = requests.get(doc_url)
        if response.status_code == 200:
            with open(filename, "wb") as f:
                f.write(response.content)
            print(f"Saved to {filename}")
        else:
            print(f"Failed to download version {version_id} (status {response.status_code})")
    except Exception as e:
        print(f"Error downloading version {version_id}: {e}")