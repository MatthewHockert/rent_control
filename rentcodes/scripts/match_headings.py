#!/usr/bin/env python3
import argparse, csv, difflib
from pathlib import Path

def load_titles(csv_path):
    rows = []
    with open(csv_path, newline="", encoding="utf-8") as f:
        r = csv.DictReader(f)
        for row in r:
            t = (row.get("title_normalized") or "").strip()
            c = int(row.get("count") or 0)
            if t:
                rows.append((t, c))
    return rows

def similarity(a, b):
    return difflib.SequenceMatcher(None, a, b).ratio()

def neighbors(all_titles, k=8, cutoff=0.7):
    titles = [t for t,_ in all_titles]
    out = []
    for t,_ in all_titles:
        pool = [u for u in titles if u != t]
        cands = difflib.get_close_matches(t, pool, n=20, cutoff=cutoff)
        scored = [(u, similarity(t,u)) for u in cands]
        scored.sort(key=lambda x: x[1], reverse=True)
        for u,s in scored[:k]:
            out.append((t, u, f"{s:.4f}"))
    return out

def cluster(all_titles, cutoff=0.78):
    titles = [t for t,_ in all_titles]
    idx = {t:i for i,t in enumerate(titles)}
    parent = list(range(len(titles)))
    def find(x):
        while parent[x]!=x:
            parent[x]=parent[parent[x]]
            x=parent[x]
        return x
    def union(a,b):
        ra, rb = find(a), find(b)
        if ra!=rb:
            parent[rb]=ra
    for i,ti in enumerate(titles):
        for j in range(i+1, len(titles)):
            tj = titles[j]
            if similarity(ti,tj) >= cutoff:
                union(i,j)
    groups = {}
    for i,t in enumerate(titles):
        r = find(i)
        groups.setdefault(r, []).append(t)
    counts = dict(all_titles)
    result = []
    for gid, members in groups.items():
        members_sorted = sorted(members, key=lambda x: (-counts.get(x,0), len(x), x))
        canonical = members_sorted[0]
        for m in members_sorted:
            score = similarity(canonical, m)
            result.append((gid, canonical, m, counts.get(m,0), f"{score:.4f}"))
    return result

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--headings_csv", default="headings/_corpus_headings.csv")
    ap.add_argument("--out_dir", default="headings")
    ap.add_argument("--neighbors_k", type=int, default=8)
    ap.add_argument("--neighbors_cutoff", type=float, default=0.70)
    ap.add_argument("--cluster_cutoff", type=float, default=0.78)
    args = ap.parse_args()

    out_dir = Path(args.out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)

    rows = load_titles(args.headings_csv)
    rows.sort(key=lambda x: (-x[1], x[0]))

    neigh = neighbors(rows, k=args.neighbors_k, cutoff=args.neighbors_cutoff)
    with (out_dir / "_neighbors.csv").open("w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["title_normalized","neighbor_title","similarity"])
        w.writerows(neigh)

    groups = cluster(rows, cutoff=args.cluster_cutoff)
    with (out_dir / "_groups.csv").open("w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["group_id","canonical_title","member_title","count","similarity_to_canonical"])
        w.writerows(groups)

    print(f"-> {out_dir / '_neighbors.csv'}")
    print(f"-> {out_dir / '_groups.csv'}")

if __name__ == "__main__":
    main()