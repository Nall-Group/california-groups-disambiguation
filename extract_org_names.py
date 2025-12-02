#!/usr/bin/env python3
"""
Extract unique organization names from leginfo_metadata.csv and check against crosswalk.
Processes the large file line by line to avoid memory issues.
"""

import csv
import sys
from collections import Counter

# Increase CSV field size limit for large fields
csv.field_size_limit(sys.maxsize)

# Paths
LEGINFO_PATH = "/Users/ruthgracewong/leginfo/extract_all_leginfo_metadata/leginfo_metadata.csv"
CROSSWALK_PATH = "/Users/ruthgracewong/california-groups-disambiguation/crosswalk.standardizenames.manualedits_clean.csv"
OUTPUT_PATH = "/Users/ruthgracewong/california-groups-disambiguation/org_names_summary.csv"

# Columns to extract org names from
ORG_COLUMNS = ["support", "opposition", "opposition_unless_amended", "support_with_amendments"]

def load_crosswalk_names():
    """Load all original names from crosswalk into a set for fast lookup."""
    names = set()
    with open(CROSSWALK_PATH, 'r', encoding='utf-8') as f:
        reader = csv.DictReader(f)
        for row in reader:
            name = row['originalname'].strip().upper()
            if name:
                names.add(name)
    print(f"Loaded {len(names)} names from crosswalk")
    return names

def extract_org_names_from_cell(cell_value):
    """Extract individual org names from a comma-separated cell."""
    if not cell_value or cell_value.strip() == "":
        return []

    # Split by semicolon first (common separator), then by comma if no semicolons
    # Looking at the sample data, it seems to use semicolons
    orgs = []
    for part in cell_value.split(";"):
        name = part.strip()
        if name:
            orgs.append(name)
    return orgs

def process_leginfo_file():
    """Process leginfo file line by line and count org occurrences."""
    org_counts = Counter()
    rows_processed = 0

    with open(LEGINFO_PATH, 'r', encoding='utf-8', errors='replace') as f:
        reader = csv.DictReader(f)

        for row in reader:
            rows_processed += 1
            if rows_processed % 50000 == 0:
                print(f"Processed {rows_processed} rows...")

            for col in ORG_COLUMNS:
                if col in row:
                    orgs = extract_org_names_from_cell(row[col])
                    for org in orgs:
                        org_counts[org] += 1

    print(f"Finished processing {rows_processed} rows")
    print(f"Found {len(org_counts)} unique organization names")
    return org_counts

def main():
    # Load crosswalk names
    crosswalk_names = load_crosswalk_names()

    # Process leginfo file
    org_counts = process_leginfo_file()

    # Write output CSV
    with open(OUTPUT_PATH, 'w', encoding='utf-8', newline='') as f:
        writer = csv.writer(f)
        writer.writerow(["org_name", "count", "in_crosswalk"])

        for org_name, count in sorted(org_counts.items(), key=lambda x: (-x[1], x[0])):
            # Check if in crosswalk (case-insensitive)
            in_crosswalk = "yes" if org_name.strip().upper() in crosswalk_names else "no"
            writer.writerow([org_name, count, in_crosswalk])

    print(f"Output written to {OUTPUT_PATH}")

    # Print some stats
    in_crosswalk_count = sum(1 for org in org_counts if org.strip().upper() in crosswalk_names)
    print(f"\nStats:")
    print(f"  Total unique orgs: {len(org_counts)}")
    print(f"  In crosswalk: {in_crosswalk_count}")
    print(f"  Not in crosswalk: {len(org_counts) - in_crosswalk_count}")

if __name__ == "__main__":
    main()
