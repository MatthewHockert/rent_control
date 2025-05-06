import requests
from bs4 import BeautifulSoup
import os
import time
from requests.exceptions import SSLError, RequestException

def safe_get(url, retries=3, timeout=10):
    for i in range(retries):
        try:
            return requests.get(url, timeout=timeout)
        except SSLError as e:
            print(f"SSL error on {url} (attempt {i+1}): {e}")
            time.sleep(1)
        except RequestException as e:
            print(f"Request failed on {url} (attempt {i+1}): {e}")
            time.sleep(1)
    return None

def download_files_from_directory(base_url, output_dir="downloads", failed_downloads=None):
    response = safe_get(base_url)
    if response is None or response.status_code != 200:
        print(f"Failed to access {base_url}")
        if failed_downloads is not None:
            failed_downloads.append((base_url, "directory access failure"))
        return

    os.makedirs(output_dir, exist_ok=True)
    soup = BeautifulSoup(response.content, "html.parser")
    links = soup.find_all("a")

    file_links = [
        link.get("href") for link in links
        if link.get("href")
        and link.get("href").endswith("y.txt")
    ]

    for file_link in file_links:
        file_url = f"{base_url}{file_link}"
        print(f"Downloading {file_url}")
        file_resp = safe_get(file_url)
        if file_resp and file_resp.status_code == 200:
            with open(os.path.join(output_dir, file_link), "wb") as f:
                f.write(file_resp.content)
            print(f"Saved {file_link}")
        else:
            print(f"Failed to download {file_url}")
            if failed_downloads is not None:
                failed_downloads.append((file_url, "file download failure"))
#,"South%20Region", "West%20Region"
if __name__ == "__main__":
    regions = ["Northeast%20Region", "Midwest%20Region"]
    failed = []
    for region in regions:
        base_url = f"https://www2.census.gov/econ/bps/Place/{region}/"
        region_output = os.path.join("downloads", region.replace("%20", "_"))
        download_files_from_directory(base_url, output_dir=region_output, failed_downloads=failed)

    if failed:
        print("\nFailed Downloads:")
        for url, reason in failed:
            print(f"{url} -> {reason}")
    else:
        print("\nAll downloads completed successfully.")