#!/usr/bin/env python3
"""
Extract unique organization names from leginfo_metadata.csv, check against crosswalk,
and route unmatched orgs to org_names_not_in_crosswalk.csv.
Processes the large file line by line to avoid memory issues.

Step 1 of LEGINFO_IMPORT.md — pure deterministic matching:
- For every org name in the five stance columns, clean it and check membership in the
  crosswalk (a plain name-set — no canonical resolution; canonicals are a step-4 concern).
- Count = the number of BILL ANALYSES (rows) each org appears in. Within a single row an
  org is counted once even if it shows up in more than one stance column; the same bill
  counts multiple times if it has multiple analysis rows (that is intended).
- Narrative-prose cells won't cleanly match, so they fall through to the unmatched pile
  and are handled by the resolution scan (step 2).
"""

import csv
import json
import os
import sys
from collections import Counter
from pathlib import Path

from org_matching_utils import (
    normalize_for_matching,
    clean_org_name,
    load_cleaning_patterns,
    strip_embedded_tables,
)

# Increase CSV field size limit for large fields
csv.field_size_limit(sys.maxsize)

# Paths
PROJECT_ROOT = Path(__file__).resolve().parent
LEGINFO_PATH = "/Users/ruthgracewong/leginfo/extract_all_leginfo_metadata/leginfo_metadata.csv"
CROSSWALK_PATH = PROJECT_ROOT / "2_webapp" / "org_clusters_crosswalk.json"
OUTPUT_PATH = PROJECT_ROOT / "org_names_import_summary.csv"
SUBSETS_DIR = PROJECT_ROOT / "org_names_for_cleaning"
NOT_IN_CROSSWALK_PATH = SUBSETS_DIR / "org_names_not_in_crosswalk.csv"

# The PERMANENT routing buckets. org_names_not_in_crosswalk.csv is deliberately NOT
# here: it is the transient step-2 worklist, not a routing destination. Treating it as
# one would mark last run's still-undiagnosed items "already_routed" and drop them from
# the worklist this run rewrites — silently losing them.
ALL_CSV_FILES = [
    "org_names_invalid.csv",
    "org_names_partial.csv",
    "org_names_that_are_actually_individuals.csv",
]
# leginfo_added_to_crosswalk.csv (a tracking log of orgs added to the crosswalk,
# `org_name,bills_supported`) was RETIRED 2026-07-17: nothing consumed it (step 4
# attributes counts from the source + mapping files, not this file), it drifted with
# duplicate/dirty rows, and the per-org bill counts are re-derivable from the source.

# Mapping files — NOT in ALL_CSV_FILES. Each has a `<string>,mapped_org(s)` schema
# (no count column) and must NEVER go through update_csv_counts() (which would
# overwrite the mapped-orgs column with a count). Each is read only for its col-0
# source strings, so a cell we've already diagnosed is recognized as already-routed
# and skips re-diagnosis; the mapped-orgs column is consumed by the step-4 cell
# rewrite (LEGINFO_IMPORT.md), which splits the count onto the component orgs.
#   - narrative: prose that describes org(s)          -> narrative_text,mapped_org
#   - conjoined: 2+ orgs mashed into one string       -> conjoined_text,mapped_orgs
# (The old flat `org_names_conjoined.csv` was retired 2026-07-16, migrated into the
# conjoined mapping so the component orgs are recorded and counts attribute properly.)
NARRATIVE_MAP_CSV = "narrative_text_mapping_to_orgs.csv"
CONJOINED_MAP_CSV = "conjoined_text_mapping_to_orgs.csv"
MAPPING_CSVS = [NARRATIVE_MAP_CSV, CONJOINED_MAP_CSV]

# Columns to extract org names from
ORG_COLUMNS = ["support", "opposition", "opposition_unless_amended", "support_with_amendments", "sponsor"]


def load_crosswalk_names(path):
    """Load the crosswalk into a plain name-set for membership checks.

    Returns two sets: exact names (uppercased) and normalized names. No canonical
    mapping is built — step 1 only needs to know whether a name is present.
    Walks canonicals and all nested children recursively.
    """
    with open(path, "r", encoding="utf-8") as f:
        data = json.load(f)

    exact_names = set()
    normalized_names = set()

    def _add(name):
        exact_names.add(name.strip().upper())
        norm = normalize_for_matching(name)
        if norm:
            normalized_names.add(norm)

    def _walk_children(children):
        for child in children:
            _add(child["name"])
            if "children" in child:
                _walk_children(child["children"])

    for cluster in data["clusters"]:
        _add(cluster["canonical"])
        _walk_children(cluster.get("children", []))

    return exact_names, normalized_names


def is_in_crosswalk(name, exact_set, normalized_set):
    """Check if a name matches the crosswalk (exact or normalized)."""
    if name.strip().upper() in exact_set:
        return True
    if normalize_for_matching(name) in normalized_set:
        return True
    return False


def extract_orgs_from_row(row, cleaning_patterns):
    """Return the set of cleaned org names appearing in a single row.

    Splits each of the five stance columns on ';', cleans each part, and collects
    the results into a set so an org is counted once per row (bill analysis) even if
    it appears in more than one stance column. Narrative-prose cells are split and
    cleaned like everything else; they simply won't match the crosswalk and fall
    through to the unmatched pile (handled in step 2).
    """
    orgs = set()
    for col in ORG_COLUMNS:
        cell_value = strip_embedded_tables(row.get(col))
        if not cell_value or not cell_value.strip():
            continue
        for part in cell_value.split(";"):
            name = part.strip()
            if not name:
                continue
            cleaned = clean_org_name(name, cleaning_patterns)
            if cleaned:
                orgs.add(cleaned)
    return orgs


def process_leginfo_file(cleaning_patterns):
    """Process leginfo file line by line, counting bill-analysis rows per org."""
    org_counts = Counter()  # org name -> number of bill-analysis rows it appears in
    rows_processed = 0

    with open(LEGINFO_PATH, 'r', encoding='utf-8', errors='replace') as f:
        reader = csv.DictReader(f)

        for row in reader:
            rows_processed += 1
            if rows_processed % 50000 == 0:
                print(f"Processed {rows_processed} rows...")

            for org in extract_orgs_from_row(row, cleaning_patterns):
                org_counts[org] += 1

    print(f"Finished processing {rows_processed} rows")
    print(f"Found {len(org_counts)} unique organization names")
    return org_counts


def load_already_routed_orgs():
    """Load normalized names from all existing CSV files, tracking which file each is in."""
    already_routed = {}  # norm -> csv filename
    for csv_file in ALL_CSV_FILES:
        filepath = SUBSETS_DIR / csv_file
        if not filepath.exists():
            continue
        with open(filepath, "r", encoding="utf-8", newline="") as f:
            reader = csv.reader(f)
            next(reader, None)  # skip header
            for row in reader:
                if not row:
                    continue
                norm = normalize_for_matching(row[0])
                if norm:
                    already_routed[norm] = csv_file

    # Mapping files (different schema): mark each already-diagnosed source string
    # (col 0) as already-routed so it never re-enters the worklist. Covers the
    # narrative-prose map and the conjoined map.
    for map_csv in MAPPING_CSVS:
        map_path = SUBSETS_DIR / map_csv
        if not map_path.exists():
            continue
        with open(map_path, "r", encoding="utf-8", newline="") as f:
            reader = csv.reader(f)
            next(reader, None)  # skip header
            for row in reader:
                if not row:
                    continue
                norm = normalize_for_matching(row[0])
                if norm:
                    already_routed[norm] = map_csv
    return already_routed


def update_csv_counts(org_counts):
    """Update counts in all routing CSVs with new counts from leginfo.

    For each org in each CSV, if we saw it in leginfo, replace its count
    with the leginfo count.
    """
    updated_total = 0
    for csv_file in ALL_CSV_FILES:
        filepath = SUBSETS_DIR / csv_file
        if not filepath.exists():
            continue
        rows = []
        updated_in_file = 0
        with open(filepath, "r", encoding="utf-8", newline="") as f:
            reader = csv.reader(f)
            header = next(reader, None)
            if not header:
                continue
            for row in reader:
                if not row:
                    continue
                org_name = row[0]
                cleaned = org_name  # already stored cleaned in CSVs
                if cleaned in org_counts:
                    row[1] = str(org_counts[cleaned])
                    updated_in_file += 1
                rows.append(row)
        if updated_in_file > 0:
            with open(filepath, "w", encoding="utf-8", newline="") as f:
                writer = csv.writer(f)
                writer.writerow(header)
                for row in rows:
                    writer.writerow(row)
            updated_total += updated_in_file
            print(f"  Updated {updated_in_file} counts in {csv_file}")
    return updated_total


def write_worklist(new_unmatched):
    """Rewrite the transient step-2 worklist with exactly this run's unmatched orgs.

    Deduplicated (first count wins) and written via a temp file + atomic replace, so
    an interrupted or concurrent run can never leave a half-written or doubled-up
    worklist behind. An empty pile still writes a header-only file, which is how a
    fully-drained worklist is represented.
    """
    seen = set()
    rows = []
    for org_name, count in new_unmatched:
        if org_name in seen:
            continue
        seen.add(org_name)
        rows.append((org_name, count))

    tmp_path = NOT_IN_CROSSWALK_PATH.with_suffix(".csv.tmp")
    with open(tmp_path, "w", encoding="utf-8", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(["org_name", "count"])
        for org_name, count in rows:
            writer.writerow([org_name, count])
    os.replace(tmp_path, NOT_IN_CROSSWALK_PATH)

    dropped = len(new_unmatched) - len(rows)
    dup_note = f" ({dropped} duplicate name(s) collapsed)" if dropped else ""
    print(f"\nWrote {len(rows)} unmatched orgs to {NOT_IN_CROSSWALK_PATH}{dup_note}")


def main():
    # Load cleaning patterns (cheap — just cleaning_patterns.txt, no crosswalk)
    print("Loading cleaning patterns...")
    cleaning_patterns = load_cleaning_patterns()

    # Load crosswalk into a plain name-set for membership checks
    print("Loading crosswalk names...")
    exact_set, normalized_set = load_crosswalk_names(CROSSWALK_PATH)
    print(f"Loaded {len(exact_set)} exact names")
    print(f"Loaded {len(normalized_set)} normalized names")

    # Load already-routed org names from all existing CSVs (norm -> csv filename)
    already_routed = load_already_routed_orgs()
    print(f"Loaded {len(already_routed)} already-routed org names from CSVs")

    # Process leginfo file
    org_counts = process_leginfo_file(cleaning_patterns)

    # Update counts in existing CSVs with leginfo counts
    print("\nUpdating counts in existing CSVs...")
    updated_count = update_csv_counts(org_counts)
    print(f"Updated {updated_count} total counts across CSVs")

    # Track match statistics
    in_crosswalk_count = 0
    already_routed_count = 0
    unmatched_count = 0
    new_unmatched = []

    # Write output CSV
    with open(OUTPUT_PATH, 'w', encoding='utf-8', newline='') as f:
        writer = csv.writer(f)
        writer.writerow(["org_name", "count", "status", "routed_to"])

        for org_name, count in sorted(org_counts.items(), key=lambda x: (-x[1], x[0])):
            if is_in_crosswalk(org_name, exact_set, normalized_set):
                status = "in_crosswalk"
                in_crosswalk_count += 1
                routed_to = ""
            else:
                norm = normalize_for_matching(org_name)
                if norm and norm in already_routed:
                    status = "already_routed"
                    already_routed_count += 1
                    routed_to = already_routed[norm]
                else:
                    status = "unmatched"
                    unmatched_count += 1
                    new_unmatched.append((org_name, count))
                    routed_to = ""

            writer.writerow([org_name, count, status, routed_to])

    print(f"Output written to {OUTPUT_PATH}")

    # Route unmatched orgs to org_names_not_in_crosswalk.csv. This is a TRANSIENT
    # handoff to the resolution scan (the file was retired as a persistent artifact
    # 2026-07-17), so it is REWRITTEN from scratch every run rather than appended to:
    # the unmatched pile is a pure function of (leginfo source, crosswalk, routing
    # CSVs), so rewriting makes re-runs idempotent. Appending used to require
    # truncating the file by hand first, and a forgotten (or concurrent) run left a
    # previous pile stacked underneath — stale rows, or every row duplicated.
    write_worklist(new_unmatched)

    # Print stats
    total_unique = len(org_counts)
    print(f"\nStats:")
    print(f"  Total unique orgs: {total_unique}")
    print(f"  In crosswalk: {in_crosswalk_count}")
    print(f"  Already routed to CSV: {already_routed_count}")
    print(f"  Unmatched (new): {unmatched_count}")

    # Generate stats JSON
    from generate_stats import generate_stats
    print("\nGenerating stats.json...")
    generate_stats()


if __name__ == "__main__":
    main()
