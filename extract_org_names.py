#!/usr/bin/env python3
"""
Extract unique organization names from leginfo_metadata.csv and check against crosswalk.
Processes the large file line by line to avoid memory issues.
"""

import csv
import sys
from collections import Counter

from org_matching_utils import CrosswalkMatcher, normalize_for_matching

# Increase CSV field size limit for large fields
csv.field_size_limit(sys.maxsize)

# Paths
LEGINFO_PATH = "/Users/ruthgracewong/leginfo/extract_all_leginfo_metadata/leginfo_metadata.csv"
OUTPUT_PATH = "/Users/ruthgracewong/california-groups-disambiguation/org_names_summary.csv"
MAPPING_PATH = "/Users/ruthgracewong/california-groups-disambiguation/narrative_org_mapping.tsv"


def load_narrative_fragments():
    """Load the known narrative-fragment strings (raw + normalized) from the mapping.

    Stance-column items matching one of these are prose we already parsed into the
    narrative_orgs column (step 1). We SKIP them here so the raw prose does not
    re-enter the org universe as junk; the parsed org flows in via narrative_orgs.
    Matching is raw-exact + normalized only (never clean()), mirroring
    apply_narrative_to_leginfo.py so we don't collapse legit orgs onto fragments.
    """
    frag_raw, frag_norm = set(), set()
    with open(MAPPING_PATH, encoding="utf-8") as f:
        reader = csv.reader(f, delimiter="\t")
        next(reader, None)  # header
        for row in reader:
            if not row:
                continue
            frag = row[0]
            frag_raw.add(frag)
            nz = normalize_for_matching(frag)
            if nz:
                frag_norm.add(nz)
    print(f"Loaded {len(frag_raw)} narrative fragments to skip in stance columns")
    return frag_raw, frag_norm

# Columns to extract org names from
ORG_COLUMNS = ["support", "opposition", "opposition_unless_amended", "support_with_amendments", "sponsor"]

# The narrative_orgs column (written by apply_narrative_to_leginfo.py) is NOT
# ';'-separated: it is ' || '-separated, each entry prefixed with "<source_col>: ",
# and may hold the sentinel "None parsed".
NARR_COL = "narrative_orgs"
NONE_TOKEN = "None parsed"


def extract_org_names_from_narrative(cell_value, matcher):
    """Extract parsed org names from a narrative_orgs cell.

    Entries look like "support: California Hospital Association || sponsor: None parsed".
    Strip the "<col>: " prefix, drop None-parsed sentinels, and clean the rest.
    """
    if not cell_value or cell_value.strip() == "":
        return []

    orgs = []
    for entry in cell_value.split(" || "):
        entry = entry.strip()
        if not entry:
            continue
        # Drop the "<source_col>: " prefix (split on the first ": ").
        _, sep, org = entry.partition(": ")
        org = (org if sep else entry).strip()
        if not org or org == NONE_TOKEN:
            continue
        cleaned = matcher.clean(org)
        if cleaned:
            orgs.append(cleaned)
    return orgs


def extract_org_names_from_cell(cell_value, matcher, frag_raw, frag_norm):
    """Extract individual org names from a semicolon-separated cell and clean them.

    Skip any item that is a known narrative fragment (raw-exact or normalized): it was
    already parsed into the narrative_orgs column, so the clean org flows in from there
    and the raw prose must not re-enter the org universe as junk.
    """
    if not cell_value or cell_value.strip() == "":
        return []

    orgs = []
    for part in cell_value.split(";"):
        name = part.strip()
        if not name:
            continue
        if name in frag_raw or normalize_for_matching(name) in frag_norm:
            continue  # known narrative prose — handled via narrative_orgs
        # Apply cleaning patterns to remove metadata annotations
        cleaned = matcher.clean(name)
        if cleaned:
            orgs.append(cleaned)
    return orgs


def process_leginfo_file(matcher, frag_raw, frag_norm):
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
                    orgs = extract_org_names_from_cell(row[col], matcher, frag_raw, frag_norm)
                    for org in orgs:
                        org_counts[org] += 1

            if NARR_COL in row:
                for org in extract_org_names_from_narrative(row[NARR_COL], matcher):
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

    # Load narrative fragments to skip in stance columns (parsed orgs arrive via narrative_orgs)
    frag_raw, frag_norm = load_narrative_fragments()

    # Process leginfo file
    org_counts = process_leginfo_file(matcher, frag_raw, frag_norm)

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
