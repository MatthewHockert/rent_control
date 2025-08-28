# scripts/reconcile_nlp_llm.py
import csv, json, argparse
from pathlib import Path

COLUMNS = [
    "municipality","ordinance_date","history_id","doc_type",
    "max_increase_pct","cpi_linked","increase_frequency_per_year",
    "vacancy_decontrol","vacancy_increase_max_pct",
    "hardship_allowed","capital_improvement_surcharge_allowed",
    "rent_board_present","owner_occupied_exempt_units",
    "small_building_exempt_threshold","min_units_covered",
    "sf_exempt","mf_only","year_built_cutoff","new_construction_exempt",
    "emergency_only","decontrol_type"
]

BOOLS = {
    "cpi_linked","vacancy_decontrol","hardship_allowed",
    "capital_improvement_surcharge_allowed","rent_board_present",
    "sf_exempt","mf_only","new_construction_exempt","emergency_only"
}
PCTS  = {"max_increase_pct","vacancy_increase_max_pct"}
NUMS  = {"increase_frequency_per_year","owner_occupied_exempt_units",
         "small_building_exempt_threshold","min_units_covered","year_built_cutoff"}
ENUMS = {"decontrol_type"}  # "temporary"|"continuous"|"none"|None

def _coerce_bool(v):
    if v in (None, "", "null", "None"): return None
    s = str(v).strip().lower()
    if s in {"1","true","yes","y"}:  return 1
    if s in {"0","false","no","n"}:  return 0
    try:
        return 1 if int(float(s)) != 0 else 0
    except Exception:
        return None

def _coerce_num(v):
    if v in (None, "", "null", "None"): return None
    try:
        return float(v)
    except Exception:
        return None

def _coerce_enum(v):
    if v in (None, "", "null", "None"): return None
    return str(v).strip().lower()

def _normalize_row(row: dict) -> dict:
    out = dict(row)
    # normalize types by field groups
    for f in PCTS | NUMS:
        out[f] = _coerce_num(row.get(f))
    for f in BOOLS:
        out[f] = _coerce_bool(row.get(f))
    for f in ENUMS:
        out[f] = _coerce_enum(row.get(f))
    return out

def _equal(a, b, field, tol: float) -> bool:
    if a is None and b is None:
        return True
    if a is None or b is None:
        return False
    if field in NUMS or field in PCTS:
        try:
            return abs(float(a) - float(b)) <= tol
        except Exception:
            return False
    if field in BOOLS:
        return int(a) == int(b)
    if field in ENUMS:
        return str(a).strip().lower() == str(b).strip().lower()
    return str(a) == str(b)

def _key(row: dict):
    return (row.get("municipality",""), row.get("ordinance_date",""), row.get("history_id",""))

def _read_csv(p: Path) -> dict:
    if not p.exists():
        return {}
    out = {}
    with open(p, newline="", encoding="utf-8") as f:
        rd = csv.DictReader(f)
        for r in rd:
            out[_key(r)] = _normalize_row(r)
    return out

def reconcile(nlp_csv, llm_csv, out_csv, flags_dir, flags_index_path, prefer="nlp", tol=0.01):
    nlp = _read_csv(Path(nlp_csv))
    llm = _read_csv(Path(llm_csv))

    all_keys = sorted(set(nlp.keys()) | set(llm.keys()))

    Path(out_csv).parent.mkdir(parents=True, exist_ok=True)
    Path(flags_dir).mkdir(parents=True, exist_ok=True)

    write_header = not Path(out_csv).exists()
    combined_flags = []

    with open(out_csv, "a", newline="", encoding="utf-8") as f_out:
        w = csv.DictWriter(f_out, fieldnames=COLUMNS + ["_has_flags"])
        if write_header:
            w.writeheader()

        for key in all_keys:
            n = nlp.get(key, {})
            l = llm.get(key, {})

            # core identity fields
            muni, date, hid = key
            doc_type = n.get("doc_type") or l.get("doc_type") or "other"

            final = {
                "municipality": muni,
                "ordinance_date": date,
                "history_id": hid,
                "doc_type": doc_type
            }

            flags = []
            for field in COLUMNS:
                if field in final:  # already set identity fields
                    continue
                a = n.get(field)
                b = l.get(field)

                # present wins over missing
                if a is None and b is not None:
                    chosen = b
                elif b is None and a is not None:
                    chosen = a
                elif _equal(a, b, field, tol):
                    chosen = a  # agree (or both None)
                else:
                    # disagreement → choose by policy, but record both
                    chosen = a if prefer == "nlp" else b
                    flags.append({
                        "field": field,
                        "nlp_value": a,
                        "llm_value": b,
                        "chosen": "nlp" if prefer == "nlp" else "llm"
                    })

                final[field] = chosen

            final["_has_flags"] = 1 if flags else 0
            w.writerow(final)

            if flags:
                flag_obj = {
                    "key": {"municipality": muni, "ordinance_date": date, "history_id": hid},
                    "doc_type": doc_type,
                    "flags": flags
                }
                # write per-doc
                flag_file = Path(flags_dir, f"{muni}_{date}_{hid}_flags.json")
                flag_file.write_text(json.dumps(flag_obj, indent=2), encoding="utf-8")
                combined_flags.append(flag_obj)

    # index of all flags
    if flags_index_path:
        Path(flags_index_path).parent.mkdir(parents=True, exist_ok=True)
        Path(flags_index_path).write_text(json.dumps(combined_flags, indent=2), encoding="utf-8")

    print(f"Wrote {out_csv}")
    print(f"Wrote flags to {flags_dir}")
    if flags_index_path:
        print(f"Wrote flags index: {flags_index_path}")

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--nlp_csv", required=True, help="Path to NLP aggregated CSV")
    ap.add_argument("--llm_csv", required=True, help="Path to LLM aggregated CSV")
    ap.add_argument("--out_csv", default="output/final/final_merged.csv")
    ap.add_argument("--flags_dir", default="output/final/flags")
    ap.add_argument("--flags_index", default="output/final/flags_index.json")
    ap.add_argument("--prefer", choices=["nlp","llm"], default="nlp", help="Winner when values disagree.")
    ap.add_argument("--tol", type=float, default=0.01, help="Numeric equality tolerance.")
    args = ap.parse_args()

    reconcile(
        nlp_csv=args.nlp_csv,
        llm_csv=args.llm_csv,
        out_csv=args.out_csv,
        flags_dir=args.flags_dir,
        flags_index_path=args.flags_index,
        prefer=args.prefer,
        tol=args.tol
    )

if __name__ == "__main__":
    main()