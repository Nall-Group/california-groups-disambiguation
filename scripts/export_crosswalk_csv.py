import csv
import json
from datetime import date
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent
IN = REPO / "2_webapp" / "org_clusters_crosswalk.json"
OUT = REPO / f"crosswalk_flat_{date.today().isoformat()}.csv"


def walk(children, parent_name, out):
    for child in children:
        name = child.get("name", "")
        rel = child.get("relationship", "").replace("_", " ")
        if name:
            out.append(f"{name} ({rel} of {parent_name})")
            if child.get("children"):
                walk(child["children"], name, out)


def main():
    with IN.open() as f:
        data = json.load(f)

    with OUT.open("w", newline="") as f:
        w = csv.writer(f)
        for cluster in data["clusters"]:
            canonical = cluster.get("canonical", "")
            if not canonical:
                continue
            row = [canonical]
            walk(cluster.get("children") or [], canonical, row)
            w.writerow(row)

    print(f"Wrote {OUT}")


if __name__ == "__main__":
    main()
