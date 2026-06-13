#!/usr/bin/env python3
"""FINAL gaps mop-up worklist — static snapshot of the 3,043 rows still pending
after the audit completed (waves 1-140, all 67,609 orgs triaged).

Unlike gaps_band.py (which re-slices the ever-growing audit masters), this list is
FIXED: each pending row was assigned one category and one small chunk label in
gaps_final_worklist.csv. Chunks are disjoint, so several RAs can work different
labels concurrently with no collisions.

Usage:
  python3 gaps_final.py list                 # all labels + still-pending counts
  python3 gaps_final.py <label>              # this label's STILL-PENDING rows
  python3 gaps_final.py <label> --all        # this label's rows incl. already-handled

"Pending" = org still present in crosswalk_gaps_all_stances.csv (source of truth).
A row is done once you've placed it in the crosswalk (or routed it to the right
invalidity CSV) AND removed it from crosswalk_gaps_all_stances.csv. When `list`
shows 0 pending for a label, that label's task is Done.
"""
import sys, csv, unicodedata, collections

WORKLIST = "gaps_final_worklist.csv"
GAPS = "crosswalk_gaps_all_stances.csv"

def norm(s):
    return unicodedata.normalize("NFC", s.strip())

def load_worklist():
    rows = collections.OrderedDict()  # label -> list of org_name
    with open(WORKLIST, encoding="utf-8") as f:
        r = csv.DictReader(f)
        for row in r:
            rows.setdefault(row["label"], []).append((row["category"], row["org_name"]))
    return rows

def pending_orgs():
    s = set()
    with open(GAPS, encoding="utf-8") as f:
        r = csv.reader(f); next(r, None)
        for row in r:
            if row:
                s.add(norm(row[0]))
    return s

def main():
    a = sys.argv[1:]
    if not a or a[0] in ("-h", "--help"):
        print(__doc__); return
    wl = load_worklist()
    if a[0] == "list":
        pend = pending_orgs()
        tot_p = tot_n = 0
        for label, items in wl.items():
            p = sum(1 for _, o in items if norm(o) in pend)
            tot_p += p; tot_n += len(items)
            cat = items[0][0]
            print(f"  {label:<16} {cat:<14} {p:>4} pending / {len(items):>4} total")
        print(f"  {'TOTAL':<16} {'':<14} {tot_p:>4} pending / {tot_n:>4} total")
        return
    label = a[0]
    if label not in wl:
        print(f"unknown label {label!r}; run `python3 gaps_final.py list`"); sys.exit(1)
    items = wl[label]
    if "--all" in a:
        for _, o in items: print(o)
        return
    pend = pending_orgs()
    keep = [o for _, o in items if norm(o) in pend]
    sys.stderr.write(f"# {label} ({items[0][0]}): {len(keep)} pending of {len(items)}\n")
    for o in keep: print(o)

if __name__ == "__main__":
    main()
