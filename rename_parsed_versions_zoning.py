import os
import re
import shutil
import pandas as pd
from fuzzywuzzy import process

excel_path = 'nj_zoning_scrape.xlsx'
source_folder = 'parsed_versions'
destination_folder = 'parsed_versions_zoning'
os.makedirs(destination_folder, exist_ok=True)

mapping_df = pd.read_excel(excel_path, dtype=str)
mapping_df = mapping_df.dropna(subset=['Municipality', 'Base_id'])
mapping_df['Municipality_clean'] = mapping_df['Municipality'].str.strip()

municipality_list = mapping_df['Municipality_clean'].tolist()

for filename in os.listdir(source_folder):
    if not filename.endswith('.csv'):
        continue

    base_name = filename.replace('.csv', '')
    possible_name = re.sub(r'_\d+$', '', base_name)
    pretty_name = possible_name.replace('_', ' ').strip()

    match_name, score = process.extractOne(pretty_name, municipality_list)
    if score < 90:
        print(f'Skipping {filename}: best match "{match_name}" only scored {score}')
        continue

    matched_row = mapping_df[mapping_df['Municipality_clean'] == match_name].iloc[0]
    base_id = matched_row['Base_id']
    new_filename = f"{match_name.replace(' ', '_')}_{base_id}.csv"

    src_path = os.path.join(source_folder, filename)
    dst_path = os.path.join(destination_folder, new_filename)
    shutil.copy(src_path, dst_path)
    print(f'Renamed {filename} → {new_filename}')