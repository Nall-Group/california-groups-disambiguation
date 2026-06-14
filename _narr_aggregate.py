#!/usr/bin/env python3
"""Aggregate per-batch extraction outputs into narrative_org_mapping.tsv.

Each batch output line is:  <fragment>\\t<extracted_org>\\t<status>
The fragment is echoed verbatim by the agent, so we KEY the extraction by fragment
content (robust to any line shift/reorder) and verify every input fragment is covered.
Any batch with uncovered fragments is reported; those must be re-run before trusting.
"""
import csv
import os
import sys

DIR = "/Users/ruthgracewong/california-groups-disambiguation/_narr_batches"
MASTER = "/Users/ruthgracewong/california-groups-disambiguation/gaps_master_narrative.txt"
OUT = "/Users/ruthgracewong/california-groups-disambiguation/narrative_org_mapping.tsv"


def read_lines(path):
    with open(path, encoding="utf-8") as f:
        return [ln.rstrip("\n") for ln in f]


def main():
    batch_files = sorted(fn for fn in os.listdir(DIR)
                         if fn.startswith("batch_") and fn.endswith(".txt"))
    frag_to_result = {}
    parsed = none_parsed = 0
    problem_batches = []
    missing = []

    for bf in batch_files:
        idx = bf[len("batch_"):-len(".txt")]
        in_path = os.path.join(DIR, bf)
        out_path = os.path.join(DIR, f"batch_{idx}.tsv")
        frags = [ln for ln in read_lines(in_path) if ln.strip() != ""]
        frag_set = set(frags)
        if not os.path.exists(out_path):
            missing.append(idx)
            continue
        # parse echoed output, key by fragment (exact), with a case/space-insensitive fallback
        local = {}
        ci = {}  # strip().lower() -> (org, status); only used when exact match fails
        for oline in read_lines(out_path):
            if oline.strip() == "":
                continue
            parts = oline.split("\t")
            frag = parts[0]
            org = parts[1].strip() if len(parts) > 1 else ""
            status = parts[2].strip().lower() if len(parts) > 2 else ""
            if frag in frag_set:
                local[frag] = (org, status)
            else:
                ci.setdefault(frag.strip().lower(), (org, status))
        # recover fragments the agent echoed with only case/whitespace differences
        for f in frags:
            if f not in local:
                k = f.strip().lower()
                if k in ci:
                    local[f] = ci[k]
        uncovered = [f for f in frags if f not in local]
        if uncovered:
            problem_batches.append((idx, len(frags), len(local), len(uncovered)))
        for f in frags:
            if f in local:
                org, status = local[f]
                if status != "parsed" or org == "":
                    frag_to_result[f] = ("", "none_parsed")
                else:
                    frag_to_result[f] = (org, "parsed")

    master = [ln for ln in read_lines(MASTER) if ln.strip() != ""]
    covered = sum(1 for m in master if m in frag_to_result)
    for f, (org, status) in frag_to_result.items():
        if status == "parsed":
            parsed += 1
        else:
            none_parsed += 1

    print(f"batches: {len(batch_files)}")
    print(f"missing output files: {missing or 'none'}")
    print(f"problem batches (idx, in, matched, uncovered): {problem_batches or 'none'}")
    print(f"mapping rows: {len(frag_to_result)}  parsed={parsed}  none_parsed={none_parsed}")
    print(f"master fragments: {len(master)}  covered: {covered}")
    print(f"uncovered master fragments: {len(master) - covered}")

    if problem_batches or missing:
        bad = [b[0] for b in problem_batches] + missing
        print(f"\n!! NOT writing mapping. Re-run batches: {','.join(sorted(set(bad)))}", file=sys.stderr)
        return 1

    with open(OUT, "w", encoding="utf-8", newline="") as f:
        w = csv.writer(f, delimiter="\t")
        w.writerow(["fragment", "extracted_org", "status"])
        for frag in master:
            org, status = frag_to_result[frag]
            w.writerow([frag, org, status])
    print(f"\nwrote {OUT} ({len(master)} rows)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
