from openai import OpenAI
import pandas as pd
import time
import os
from tqdm import tqdm

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

df = df[["Municipality", "Units-in-Structure Ordinance Applies to", "Rent Increase Limit", "Exceptions"]]
df["units_covered"] = df["Units-in-Structure Ordinance Applies to"].apply(score_units_covered)

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
        answer = response.choices[0].message.content
        parsed = eval(answer)
        for k, v in parsed.items():
            df.loc[idx, k] = v
        time.sleep(1.2)
    except Exception as e:
        print(f"Error at row {idx}: {e}")
        continue

weights = {
    "max_rent_increase": 0.25,
    "sf_exempt": 0.15,
    "cpi_tied": 0.15,
    "new_construction_exempt": 0.15,
    "hardship_appeals": 0.1,
    "units_covered": 0.2
}

score_cols = list(weights.keys())
for col in score_cols:
    if col not in df.columns:
        df[col] = None

df["rent_control_unweighted"] = df[score_cols].mean(axis=1)
df["rent_control_weighted"] = df.apply(
    lambda row: sum(row[k] * w for k, w in weights.items() if pd.notnull(row[k])),
    axis=1
)

output_path = "rent_control_scored_output.xlsx"
df.to_excel(output_path, index=False)
print(f"Saved results to {output_path}")