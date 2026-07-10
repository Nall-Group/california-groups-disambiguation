#!/usr/bin/env python3
"""Emit the next K unprocessed batches (15 items each) from the count-sorted worklist.

Worklist = org_names_for_cleaning/org_names_not_in_crosswalk.csv (already sorted by
count desc). Skips names already in the processed ledger. Writes each batch to
$SCAN/batches/batch_NNNN.csv and prints a JSON array:
  [{"batch": <int>, "items": [[name, count], ...]}, ...]
Batch numbers come from $SCAN/next_batch_num.txt (starts at 2; batch 1 was the pilot).
Does NOT mark anything processed — that happens in apply_results.py once diagnosed.
"""
import csv, json, os, sys
from pathlib import Path

PROJECT = Path("/Users/ruthgracewong/california-groups-disambiguation")
WORKLIST = PROJECT / "org_names_for_cleaning" / "org_names_not_in_crosswalk.csv"
SCAN = Path(os.environ["TMPDIR"]) / "leginfo_scan"
BATCHES = SCAN / "batches"; BATCHES.mkdir(exist_ok=True)
STATE = PROJECT / "leginfo_scan_state"          # durable (committed) ledgers
PROCESSED = STATE / "processed.txt"
COUNTER = STATE / "next_batch_num.txt"

K = int(sys.argv[1]) if len(sys.argv) > 1 else 40
SIZE = 15

processed = set()
if PROCESSED.exists():
    processed = {l.rstrip("\n") for l in PROCESSED.read_text().splitlines()}

rows = []
with open(WORKLIST, newline="") as f:
    r = csv.reader(f); next(r)
    for row in r:
        if not row: continue
        if row[0] in processed: continue
        rows.append((row[0], int(row[1])))

rows = rows[:K * SIZE]
start = int(COUNTER.read_text()) if COUNTER.exists() else 2

out = []
bn = start
for i in range(0, len(rows), SIZE):
    chunk = rows[i:i+SIZE]
    bf = BATCHES / f"batch_{bn:04d}.csv"
    with open(bf, "w", newline="") as f:
        w = csv.writer(f)
        for name, cnt in chunk: w.writerow([name, cnt])
    out.append({"batch": bn, "items": chunk})
    bn += 1

COUNTER.write_text(str(bn))
print(json.dumps(out))
sys.stderr.write(f"emitted {len(out)} batches ({len(rows)} items), batches {start}..{bn-1}\n")
