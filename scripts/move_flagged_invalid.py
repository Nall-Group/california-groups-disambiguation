#!/usr/bin/env python3
"""
Move flagged invalid entries from org_names_not_in_crosswalk.csv to the
appropriate invalidity CSV files based on categories in flagged_invalid.json.

This should run BEFORE adding entries to the crosswalk so invalid entries
don't get added.
"""

import csv
import json
import sys
from collections import defaultdict
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parent.parent
SUBSETS_DIR = PROJECT_ROOT / "org_name_subsets_for_cleaning"
OUTPUT_DIR = PROJECT_ROOT / "grouping_output"
FLAGGED_PATH = OUTPUT_DIR / "flagged_invalid.json"
NOT_IN_CROSSWALK = SUBSETS_DIR / "org_names_not_in_crosswalk.csv"

# Map category keys to destination CSV filenames
CATEGORY_TO_CSV = {
    "numbers_dates": "org_names_that_are_dates_or_phone_numbers.csv",
    "partial": "org_names_partial.csv",
    "starts_with_parens": "org_names_that_start_with_parens.csv",
    "not_capitalized": "org_names_not_capitalized.csv",
    "conjoined": "org_names_conjoined.csv",
    "narrative_text": "org_names_embedded_in_narrative_text.csv",
    "individual": "org_names_that_are_actually_individuals.csv",
    "invalid": "org_names_invalid.csv",
}


def read_csv(filepath):
    """Read CSV and return list of (org_name, count) tuples."""
    rows = []
    with open(filepath, "r", encoding="utf-8", newline="") as f:
        reader = csv.reader(f)
        next(reader)  # skip header
        for row in reader:
            if not row:
                continue
            rows.append((row[0], int(row[1])))
    return rows


def write_csv(filepath, rows):
    """Write (org_name, count) rows to CSV with header."""
    with open(filepath, "w", encoding="utf-8", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(["org_name", "count"])
        for name, count in rows:
            writer.writerow([name, count])


def main():
    # Load flagged entries
    print(f"Loading {FLAGGED_PATH.name}...")
    with open(FLAGGED_PATH, "r", encoding="utf-8") as f:
        flagged = json.load(f)

    total_flagged = sum(len(items) for items in flagged.values())
    print(f"  {total_flagged:,} flagged entries across {len(flagged)} categories")

    # Build set of org_names to remove from not_in_crosswalk
    names_to_remove = set()
    for category, items in flagged.items():
        for item in items:
            names_to_remove.add(item["org_name"])

    # Read not_in_crosswalk
    print(f"\nReading {NOT_IN_CROSSWALK.name}...")
    source_rows = read_csv(NOT_IN_CROSSWALK)
    print(f"  {len(source_rows):,} entries")

    # Split: keep vs remove
    remaining = []
    removed = {}  # org_name -> count (for verification)
    for org_name, count in source_rows:
        if org_name in names_to_remove:
            removed[org_name] = count
        else:
            remaining.append((org_name, count))

    print(f"  Will remove: {len(removed):,}")
    print(f"  Will keep: {len(remaining):,}")

    if len(removed) != total_flagged:
        # Some flagged entries might not be in the CSV (already removed)
        missing = names_to_remove - set(removed.keys())
        print(f"  Note: {len(missing)} flagged entries not found in CSV (may have been already removed)")

    # Read destination CSVs, append flagged entries, write back
    print("\nMoving entries to destination CSVs...")
    total_appended = 0
    for category, csv_filename in CATEGORY_TO_CSV.items():
        if category not in flagged or not flagged[category]:
            continue

        dest_path = SUBSETS_DIR / csv_filename
        dest_rows = read_csv(dest_path)
        before_count = len(dest_rows)

        # Append flagged entries for this category
        appended = 0
        for item in flagged[category]:
            org_name = item["org_name"]
            if org_name in removed:
                dest_rows.append((org_name, removed[org_name]))
                appended += 1

        # Sort by count descending
        dest_rows.sort(key=lambda x: x[1], reverse=True)
        write_csv(dest_path, dest_rows)

        print(f"  {csv_filename}: {before_count:,} + {appended} = {len(dest_rows):,}")
        total_appended += appended

    # Write remaining back to not_in_crosswalk
    print(f"\nWriting remaining to {NOT_IN_CROSSWALK.name}...")
    write_csv(NOT_IN_CROSSWALK, remaining)
    print(f"  {len(source_rows):,} -> {len(remaining):,} ({len(removed):,} removed)")

    # Verify
    print(f"\n--- Verification ---")
    print(f"  Removed from source: {len(removed):,}")
    print(f"  Appended to destinations: {total_appended:,}")
    if len(removed) == total_appended:
        print(f"  Counts match!")
    else:
        print(f"  WARNING: mismatch (some entries may not have been in source CSV)")


if __name__ == "__main__":
    main()
