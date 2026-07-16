#!/usr/bin/env python3
"""
Regenerate org_names_for_cleaning CSVs by re-checking all names
against the current crosswalk.

Names that are now in the crosswalk (regardless of original category) move
to org_names_in_crosswalk.csv. Names in org_names_in_crosswalk.csv that are
no longer in the crosswalk move to org_names_not_in_crosswalk.csv.

Deduplication: names that normalize to the same string (via
normalize_for_matching) are merged — counts are summed and the most common
spelling is kept as the representative.

Output CSVs all have the format: org_name,count (sorted by count descending).
"""

import csv
import json
import sys
from collections import defaultdict
from pathlib import Path

# Add project root so we can import org_matching_utils
PROJECT_ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(PROJECT_ROOT))

from org_matching_utils import normalize_for_matching, clean_org_name, load_cleaning_patterns

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------
CROSSWALK_PATH = PROJECT_ROOT / "2_webapp" / "org_clusters_crosswalk.json"
SUBSETS_DIR = PROJECT_ROOT / "org_names_for_cleaning"

# All CSV files, keyed by a short label
# Standard `org_name,count` routing buckets that this script reads, re-checks
# against the crosswalk, and redistributes. Do NOT add
# `narrative_text_mapping_to_orgs.csv` here: it has a different schema
# (`narrative_text,mapped_org`, no count) and its rows are prose, not orgs —
# redistributing them would re-check prose against the crosswalk and could merge
# it back in. It is intentionally left untouched by this script.
CSV_FILES = {
    "in_crosswalk":   "org_names_in_crosswalk.csv",
    "not_in_crosswalk": "org_names_not_in_crosswalk.csv",
    "invalid":        "org_names_invalid.csv",
    "partial":        "org_names_partial.csv",
    "individuals":    "org_names_that_are_actually_individuals.csv",
    "conjoined":      "org_names_conjoined.csv",
}

# Priority order for cross-file deduplication: when a normalized name appears
# in multiple buckets, the highest-priority (lowest index) bucket keeps it.
BUCKET_PRIORITY = [
    "in_crosswalk",
    "not_in_crosswalk",
    "individuals",
    "conjoined",
    "partial",
    "invalid",
]


# ---------------------------------------------------------------------------
# 1. Load crosswalk — full recursive walk
# ---------------------------------------------------------------------------
def load_crosswalk_names(path: Path):
    """Return two sets: exact names (uppercased) and normalized names."""
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


# ---------------------------------------------------------------------------
# 2. Read all CSVs
# ---------------------------------------------------------------------------
def read_csv(filepath):
    """Read a CSV and return list of (org_name, count) tuples.

    Handles both 2-column (org_name, count) and 3-column (org_name, count,
    in_crosswalk) formats.
    """
    rows = []
    with open(filepath, "r", encoding="utf-8", newline="") as f:
        reader = csv.reader(f)
        header = next(reader)
        for row in reader:
            if not row:
                continue
            org_name = row[0]
            count = int(row[1])
            rows.append((org_name, count))
    return rows


# ---------------------------------------------------------------------------
# 3. Deduplicate within a list using normalize_for_matching
# ---------------------------------------------------------------------------
def deduplicate(rows):
    """Merge rows that normalize to the same string.

    For each group of duplicates, sum counts and keep the spelling with the
    highest individual count as the representative name.

    Returns a list of (org_name, total_count) sorted by count descending.
    """
    # normalized -> list of (org_name, count)
    groups = defaultdict(list)
    for name, count in rows:
        norm = normalize_for_matching(name)
        groups[norm].append((name, count))

    deduped = []
    for norm, members in groups.items():
        total = sum(c for _, c in members)
        # Pick the spelling with the highest count
        best_name = max(members, key=lambda x: x[1])[0]
        deduped.append((best_name, total))

    deduped.sort(key=lambda x: x[1], reverse=True)
    return deduped


def deduplicate_across_buckets(buckets):
    """Remove names that appear in multiple buckets.

    When a normalized name exists in more than one bucket, the
    highest-priority bucket (per BUCKET_PRIORITY) keeps the entry.
    The spelling from the winning bucket is kept and counts from all
    buckets are summed.

    Mutates *buckets* in place and returns a dict of
    {label: number_of_entries_removed}.
    """
    priority = {label: i for i, label in enumerate(BUCKET_PRIORITY)}

    # Build global map: normalized -> [(bucket_label, index, org_name, count)]
    norm_map = defaultdict(list)
    for label, rows in buckets.items():
        for idx, (name, count) in enumerate(rows):
            norm = normalize_for_matching(name)
            norm_map[norm].append((label, idx, name, count))

    # Find entries to remove from losing buckets
    # removals[label] = set of indices to drop
    removals = defaultdict(set)

    for norm, entries in norm_map.items():
        # Only care about names in multiple buckets
        unique_buckets = {label for label, _, _, _ in entries}
        if len(unique_buckets) <= 1:
            continue

        # Winner = bucket with lowest priority index
        winner_label = min(unique_buckets, key=lambda b: priority[b])

        # Sum counts from all buckets
        total_count = sum(count for _, _, _, count in entries)

        # Find the winner's entry (first one in winner bucket)
        winner_entries = [(idx, name, count) for label, idx, name, count in entries
                         if label == winner_label]
        winner_idx = winner_entries[0][0]
        winner_name = winner_entries[0][1]

        # Update winner entry with summed count
        buckets[winner_label][winner_idx] = (winner_name, total_count)

        # Mark all other entries for removal (including extra winner-bucket dupes)
        for label, idx, name, count in entries:
            if label == winner_label and idx == winner_idx:
                continue
            removals[label].add(idx)

    # Apply removals (rebuild each affected list, skipping removed indices)
    removed_counts = {}
    for label in buckets:
        if label in removals and removals[label]:
            to_remove = removals[label]
            buckets[label] = [row for i, row in enumerate(buckets[label])
                              if i not in to_remove]
            removed_counts[label] = len(to_remove)
        else:
            removed_counts[label] = 0

    # Re-sort affected buckets by count descending
    for label in buckets:
        if removed_counts.get(label, 0) > 0:
            buckets[label].sort(key=lambda x: x[1], reverse=True)

    return removed_counts


# ---------------------------------------------------------------------------
# 4. Write CSV
# ---------------------------------------------------------------------------
def write_csv(filepath, rows):
    """Write (org_name, count) rows to a CSV with header org_name,count."""
    with open(filepath, "w", encoding="utf-8", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(["org_name", "count"])
        for name, count in rows:
            writer.writerow([name, count])


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
def main():
    print("Loading crosswalk (recursive)...")
    exact_set, normalized_set = load_crosswalk_names(CROSSWALK_PATH)
    print(f"  {len(exact_set):,} exact names, {len(normalized_set):,} normalized names\n")

    # Read all CSVs
    before_counts = {}
    category_rows = {}
    for label, filename in CSV_FILES.items():
        filepath = SUBSETS_DIR / filename
        rows = read_csv(filepath)
        category_rows[label] = rows
        before_counts[label] = len(rows)
        print(f"  Read {len(rows):>7,} names from {filename}")

    total_before = sum(before_counts.values())
    print(f"\n  Total names across all files: {total_before:,}\n")

    # ----- Clean metadata suffixes like (Co-Source), (Co-Sponsor), etc. -----
    print("Cleaning metadata suffixes from org names...")
    cleaning_patterns = load_cleaning_patterns()
    names_cleaned = 0
    for label, rows in category_rows.items():
        cleaned_rows = []
        for name, count in rows:
            cleaned = clean_org_name(name, cleaning_patterns)
            if cleaned != name:
                names_cleaned += 1
            cleaned_rows.append((cleaned, count))
        category_rows[label] = cleaned_rows
    if names_cleaned:
        print(f"  Cleaned {names_cleaned} name(s)\n")
    else:
        print("  No names needed cleaning\n")

    # ----- Redistribute -----
    # Buckets after redistribution (lists of (name, count))
    new_buckets = {label: [] for label in CSV_FILES}

    moved_to_crosswalk = 0
    moved_from_crosswalk = 0

    for label, rows in category_rows.items():
        for name, count in rows:
            in_cw = is_in_crosswalk(name, exact_set, normalized_set)

            if label == "in_crosswalk":
                if in_cw:
                    new_buckets["in_crosswalk"].append((name, count))
                else:
                    new_buckets["not_in_crosswalk"].append((name, count))
                    moved_from_crosswalk += 1
            else:
                if in_cw:
                    new_buckets["in_crosswalk"].append((name, count))
                    moved_to_crosswalk += 1
                else:
                    new_buckets[label].append((name, count))

    # ----- Deduplicate each bucket -----
    print("Deduplicating...")
    dedup_removed = {}
    for label in CSV_FILES:
        before_dedup = len(new_buckets[label])
        new_buckets[label] = deduplicate(new_buckets[label])
        after_dedup = len(new_buckets[label])
        dedup_removed[label] = before_dedup - after_dedup
        if dedup_removed[label]:
            print(f"  {CSV_FILES[label]}: merged {dedup_removed[label]} duplicate(s)")

    total_dedup = sum(dedup_removed.values())
    if total_dedup == 0:
        print("  No within-file duplicates found.")
    print()

    # ----- Deduplicate across buckets -----
    print("Cross-file deduplication...")
    cross_removed = deduplicate_across_buckets(new_buckets)
    total_cross = sum(cross_removed.values())
    if total_cross > 0:
        for label in CSV_FILES:
            if cross_removed[label]:
                print(f"  {CSV_FILES[label]}: removed {cross_removed[label]} cross-file duplicate(s)")
    else:
        print("  No cross-file duplicates found.")
    print()

    # ----- Write all CSVs -----
    print("Writing CSVs...")
    after_counts = {}
    for label, filename in CSV_FILES.items():
        filepath = SUBSETS_DIR / filename
        write_csv(filepath, new_buckets[label])
        after_counts[label] = len(new_buckets[label])
        print(f"  Wrote {len(new_buckets[label]):>7,} names to {filename}")

    total_after = sum(after_counts.values())

    # ----- Summary -----
    print("\n" + "=" * 60)
    print("SUMMARY")
    print("=" * 60)
    print(f"\n{'File':<50} {'Before':>7} {'After':>7} {'Diff':>7}")
    print("-" * 71)
    for label, filename in CSV_FILES.items():
        diff = after_counts[label] - before_counts[label]
        sign = "+" if diff > 0 else ""
        print(f"  {filename:<48} {before_counts[label]:>7,} {after_counts[label]:>7,} {sign}{diff:>6,}")
    print("-" * 71)
    diff_total = total_after - total_before
    sign = "+" if diff_total > 0 else ""
    print(f"  {'TOTAL':<48} {total_before:>7,} {total_after:>7,} {sign}{diff_total:>6,}")

    print(f"\nMoved to in_crosswalk (from other categories):  {moved_to_crosswalk:,}")
    print(f"Moved from in_crosswalk (to not_in_crosswalk):  {moved_from_crosswalk:,}")
    print(f"Within-file duplicates merged:                  {total_dedup:,}")
    print(f"Cross-file duplicates removed:                  {total_cross:,}")

    total_removed = total_dedup + total_cross
    if total_removed == 0 and total_after != total_before:
        print(f"\nWARNING: total names changed from {total_before:,} to {total_after:,} without dedup!")
    elif total_removed > 0:
        expected = total_before - total_removed
        if total_after != expected:
            print(f"\nWARNING: expected {expected:,} after dedup, got {total_after:,}")
        else:
            print(f"\nVerified: {total_before:,} - {total_dedup:,} within-file - {total_cross:,} cross-file = {total_after:,} (correct)")
    else:
        print(f"\nVerified: {total_after:,} names total (unchanged)")


if __name__ == "__main__":
    main()
