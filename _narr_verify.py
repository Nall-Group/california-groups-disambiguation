#!/usr/bin/env python3
"""Post-apply verification: schema integrity + content sanity on the written leginfo file."""
import csv
import sys

csv.field_size_limit(sys.maxsize)
PATH = "/Users/ruthgracewong/leginfo/extract_all_leginfo_metadata/leginfo_metadata.csv"
EXPECTED_HEADER = ["folder_name", "file_name", "file_format", "committee_name", "bill_number",
                   "subject", "summary", "support", "opposition", "opposition_unless_amended",
                   "support_with_amendments", "sponsor", "passed_legislature", "narrative_text"]

rows = 0
bad_cols = 0
with_narr = 0
none_parsed_cells = 0
example = None
with open(PATH, encoding="utf-8", errors="replace", newline="") as f:
    reader = csv.reader(f)
    header = next(reader)
    if header != EXPECTED_HEADER:
        print("HEADER MISMATCH:", header)
        sys.exit(1)
    ncol = len(header)
    for row in reader:
        rows += 1
        if len(row) != ncol:
            bad_cols += 1
            if bad_cols <= 3:
                print(f"  bad row {rows}: {len(row)} cols")
        narr = row[-1] if len(row) == ncol else ""
        if narr:
            with_narr += 1
            if example is None and " || " in narr:
                example = (rows, narr, row[7], row[8])
        # count None parsed tokens in stance cols
        for c in (7, 8, 9, 10):
            if c < len(row) and "None parsed" in row[c]:
                none_parsed_cells += 1
                break

print(f"header: OK ({ncol} columns, narrative_text present)")
print(f"data rows: {rows}")
print(f"rows with bad column count: {bad_cols}")
print(f"rows with non-empty narrative_text: {with_narr}")
print(f"rows with >=1 'None parsed' in a stance col: {none_parsed_cells}")
if example:
    r, narr, sup, opp = example
    print(f"\nexample multi-fragment row {r}:")
    print(f"  narrative_text: {narr[:300]}")
    print(f"  support:    {sup[:200]}")
    print(f"  opposition: {opp[:200]}")
