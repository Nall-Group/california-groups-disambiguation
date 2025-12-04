#!/usr/bin/env python3
"""
Extract unique organization names from leginfo_metadata.csv and check against crosswalk.
Processes the large file line by line to avoid memory issues.
"""

import csv
import sys
from collections import Counter

from org_matching_utils import CrosswalkMatcher

# Increase CSV field size limit for large fields
csv.field_size_limit(sys.maxsize)

# Paths
LEGINFO_PATH = "/Users/ruthgracewong/leginfo/extract_all_leginfo_metadata/leginfo_metadata.csv"
OUTPUT_PATH = "/Users/ruthgracewong/california-groups-disambiguation/org_names_summary.csv"

# Columns to extract org names from
ORG_COLUMNS = ["support", "opposition", "opposition_unless_amended", "support_with_amendments"]


def extract_org_names_from_cell(cell_value, matcher):
    """Extract individual org names from a semicolon-separated cell and clean them."""
    if not cell_value or cell_value.strip() == "":
        return []

    orgs = []
    for part in cell_value.split(";"):
        name = part.strip()
        if name:
            # Apply cleaning patterns to remove metadata annotations
            cleaned = matcher.clean(name)
            if cleaned:
                orgs.append(cleaned)
    return orgs


def process_leginfo_file(matcher):
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
                    orgs = extract_org_names_from_cell(row[col], matcher)
                    for org in orgs:
                        org_counts[org] += 1

    print(f"Finished processing {rows_processed} rows")
    print(f"Found {len(org_counts)} unique organization names")
    return org_counts


def main():
    # Initialize the matcher (loads crosswalk and cleaning patterns)
    print("Loading crosswalk matcher...")
    matcher = CrosswalkMatcher()
    print(f"Loaded {matcher.exact_name_count} exact names")
    print(f"Loaded {matcher.normalized_name_count} normalized names for fuzzy matching")

    # Process leginfo file
    org_counts = process_leginfo_file(matcher)

    # Track match statistics
    exact_match_count = 0
    normalized_match_count = 0
    no_match_count = 0

    # Write output CSV
    with open(OUTPUT_PATH, 'w', encoding='utf-8', newline='') as f:
        writer = csv.writer(f)
        writer.writerow(["org_name", "count", "in_crosswalk", "match_type", "canonical_name"])

        for org_name, count in sorted(org_counts.items(), key=lambda x: (-x[1], x[0])):
            # Check if in crosswalk (cleaning already applied during extraction)
            result = matcher.match(org_name)

            if result.is_match:
                in_crosswalk = "yes"
                if result.match_type == 'exact':
                    exact_match_count += 1
                else:
                    normalized_match_count += 1
                match_type = result.match_type
                canonical = result.canonical
            else:
                in_crosswalk = "no"
                match_type = ""
                canonical = ""
                no_match_count += 1

            writer.writerow([org_name, count, in_crosswalk, match_type, canonical])

    print(f"Output written to {OUTPUT_PATH}")

    # Print stats
    total_unique = len(org_counts)
    print(f"\nStats:")
    print(f"  Total unique orgs: {total_unique}")
    print(f"  Exact matches: {exact_match_count}")
    print(f"  Normalized matches (punctuation/spacing): {normalized_match_count}")
    print(f"  Total in crosswalk: {exact_match_count + normalized_match_count}")
    print(f"  Not in crosswalk: {no_match_count}")

    # Generate stats JSON
    from generate_stats import generate_stats
    print("\nGenerating stats.json...")
    generate_stats()


if __name__ == "__main__":
    main()
