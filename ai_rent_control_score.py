from openai import OpenAI
import pandas as pd
import time
import os
import re
from pca_weighted_index import compute_pca_weighted_index
from tqdm import tqdm
import json

client = OpenAI(api_key=os.getenv("OPENAI_API_KEY"))

input_path = "openai_nj_rent_control_survey.xlsx"
df = pd.read_excel(input_path)
df.columns = df.columns.str.strip().str.replace(r'\s+', ' ', regex=True)

def score_units_covered(text):
    t = str(text).lower().strip()
    if "1+" in t:
        return 1.0
    elif "2+" in t or "2 family" in t:
        return 0.9
    elif "3+" in t or "4+" in t:
        return 0.8
    elif "5+" in t or "6+" in t or "10+" in t:
        return 0.6
    elif "20+" in t or "21+" in t:
        return 0.4
    elif "mobile home" in t:
        if "senior" in t:
            return 0.3
        return 0.5
    else:
        return 0.2

def extract_numeric_cap(text):
    if pd.isnull(text) or str(text).strip().lower() in ["unknown", ""]:
        return None
    text = str(text).lower()
    percent_matches = re.findall(r"(\d+\.?\d*)\s*%", text)
    decimal_matches = re.findall(r"\b0?\.\d+\b", text)
    scores = [float(p) for p in percent_matches] + [float(d) * 100 for d in decimal_matches if float(d) < 1]
    return min(scores) if scores else None

df = df[["Municipality", "Units-in-Structure Ordinance Applies to", "Rent Increase Limit", "Exceptions"]]
df["units_covered"] = df["Units-in-Structure Ordinance Applies to"].apply(score_units_covered)
df["cap_numeric"] = df["Rent Increase Limit"].apply(extract_numeric_cap)
cap_values = pd.to_numeric(df["cap_numeric"], errors="coerce")
cap_max = cap_values.max()

def inverse_score(cap_pct):
    if cap_pct is None:
        return None
    try:
        cap = float(cap_pct)
        cap = min(cap, cap_max)
        return round(1 - (cap / cap_max), 3)
    except:
        return None

df["max_rent_increase"] = df["cap_numeric"].apply(inverse_score)

with open("prompt_template.txt", "r") as f:
    prompt_template = f.read()

def build_prompt(row):
    return prompt_template.format(
        Municipality=row['Municipality'],
        Rent_Increase_Limit=row['Rent Increase Limit'],
        Exceptions=row['Exceptions'],
    )

for idx, row in tqdm(df.iterrows(), total=len(df)):
    prompt = build_prompt(row)
    try:
        response = client.chat.completions.create(
            model="gpt-3.5-turbo",
            messages=[
                {"role": "system", "content": "You are a policy analyst."},
                {"role": "user", "content": prompt}
            ],
            temperature=0
        )
        answer = response.choices[0].message.content.strip()
        if answer.startswith("```"):
            answer = re.sub(r"^```json|^```|```$", "", answer.strip(), flags=re.MULTILINE).strip()
        parsed = json.loads(answer)
        for k, v in parsed.items():
            colname = f"chat_{k}" if k == "max_rent_increase" else k
            df.loc[idx, colname] = v
    except json.JSONDecodeError:
        print(f"Failed to parse JSON at row {idx}: {answer}")
        continue

score_cols = ["max_rent_increase", "sf_exempt", "cpi_tied", "new_construction_exempt", "hardship_appeals", "units_covered"]
for col in score_cols:
    df[col] = pd.to_numeric(df[col], errors="coerce")

pca_cols = ["sf_exempt", "cpi_tied", "new_construction_exempt", "hardship_appeals", "units_covered"]
df, pca_weights = compute_pca_weighted_index(df, pca_cols)
df["rent_control_unweighted"] = df[pca_cols].mean(axis=1)

with open("pca_weights.json", "w") as f:
    json.dump(pca_weights, f, indent=2)

output_path = "rent_control_scored_output.xlsx"
df.to_excel(output_path, index=False)
print(f"Saved results to {output_path}")
