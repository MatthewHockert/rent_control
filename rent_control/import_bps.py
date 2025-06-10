import os
import requests
from pathlib import Path

DATA_ROOT = Path("downloads/bps/Northeast_Region")
DATA_ROOT.mkdir(parents=True, exist_ok=True)

def download_file(url, output_dir):
    filename = url.split("/")[-1]
    response = requests.get(url)
    if response.status_code == 200:
        with open(output_dir / filename, "wb") as f:
            f.write(response.content)
        print(f"Downloaded: {filename}")
    else:
        print(f"Failed to download: {url}")

region_code = "ne"
region_name = "Northeast%20Region"
start_year = 1980
end_year = 2024  
for year in range(start_year, end_year + 1):
    filename = f"{region_code}{year}a.txt"
    url = f"https://www2.census.gov/econ/bps/Place/{region_name}/{filename}"
    download_file(url, DATA_ROOT)