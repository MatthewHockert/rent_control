import os
import subprocess
import glob

root_folder = "muni_codes"

for city_folder in os.listdir(root_folder):
    city_path = os.path.join(root_folder, city_folder)
    if not os.path.isdir(city_path):
        continue

    html_files = glob.glob(os.path.join(city_path, "*.html"))

    for html_file in html_files:
        base_name = os.path.splitext(os.path.basename(html_file))[0]
        pdf_file = os.path.join(city_path, f"{base_name}.pdf")

        if os.path.exists(pdf_file):
            continue

        subprocess.run([
            "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
            "--headless",
            "--disable-gpu",
            f"--print-to-pdf={pdf_file}",
            f"file://{os.path.abspath(html_file)}"
        ])

        print(f"✔ Converted {city_folder}/{base_name}.html → {base_name}.pdf")