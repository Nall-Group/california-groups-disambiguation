#!/usr/bin/env python3
"""
Merge organization names that differ only in spacing/punctuation.

Uses normalize_for_matching() from org_matching_utils.py to identify duplicates.
Merges by summing counts and selecting the canonical name based on:
1. Crosswalk canonical if available
2. Otherwise, least punctuation then least capitals
"""

import csv
import re
from collections import defaultdict
from pathlib import Path

from org_matching_utils import normalize_for_matching, CrosswalkMatcher


def count_punctuation(name: str) -> int:
    """Count punctuation characters in a name."""
    return len(re.findall(r'[^\w\s]', name))


def count_capitals(name: str) -> int:
    """Count uppercase letters in a name."""
    return sum(1 for c in name if c.isupper())


def load_and_group_by_normalized(csv_path: str) -> dict[str, list[tuple[str, int]]]:
    """
    Load CSV and group entries by their normalized form.

    Returns:
        Dict mapping normalized_name -> [(original_name, count), ...]
    """
    normalized_groups = defaultdict(list)

    with open(csv_path, 'r', encoding='utf-8') as f:
        reader = csv.DictReader(f)
        for row in reader:
            name = row['org_name']
            count = int(row['count'])
            normalized = normalize_for_matching(name)

            # Skip empty or names that normalize to empty
            if not name.strip() or not normalized:
                continue

            normalized_groups[normalized].append((name, count))

    return normalized_groups


def select_canonical_name(
    variants: list[tuple[str, int]],
    crosswalk_canonicals: dict[str, str]
) -> str:
    """
    Select the canonical name from a group of variants.

    Priority:
    1. If any variant matches a crosswalk canonical, use that canonical
    2. Otherwise, pick variant with least punctuation
    3. Tie-breaker: least capital letters

    Args:
        variants: List of (name, count) tuples
        crosswalk_canonicals: Dict mapping normalized_name -> canonical_name

    Returns:
        The selected canonical name
    """
    # Check if any variant matches a crosswalk canonical
    for name, _ in variants:
        normalized = normalize_for_matching(name)
        if normalized in crosswalk_canonicals:
            return crosswalk_canonicals[normalized]

    # Sort by: punctuation count (asc), then capital count (asc)
    sorted_variants = sorted(
        variants,
        key=lambda x: (count_punctuation(x[0]), count_capitals(x[0]))
    )

    return sorted_variants[0][0]


def merge_duplicates(
    csv_path: str,
    crosswalk_matcher: CrosswalkMatcher
) -> tuple[list[dict], int, int]:
    """
    Merge duplicate entries in a CSV file.

    Returns:
        Tuple of (merged_entries, original_count, merge_count)
    """
    # Get crosswalk canonical lookup
    crosswalk_canonicals = crosswalk_matcher._normalized_to_canonical

    # Load and group
    normalized_groups = load_and_group_by_normalized(csv_path)

    merged_entries = []
    merge_count = 0
    original_count = 0

    for normalized, variants in normalized_groups.items():
        original_count += len(variants)

        if len(variants) == 1:
            # No duplicate, keep as-is
            name, count = variants[0]
            merged_entries.append({'org_name': name, 'count': count})
        else:
            # Merge: sum counts, select canonical
            total_count = sum(count for _, count in variants)
            canonical = select_canonical_name(variants, crosswalk_canonicals)
            merged_entries.append({'org_name': canonical, 'count': total_count})
            merge_count += 1

    # Sort by count descending (maintains original order convention)
    merged_entries.sort(key=lambda x: x['count'], reverse=True)

    return merged_entries, original_count, merge_count


def write_merged_csv(entries: list[dict], output_path: str):
    """Write merged entries to CSV."""
    with open(output_path, 'w', encoding='utf-8', newline='') as f:
        writer = csv.DictWriter(f, fieldnames=['org_name', 'count'])
        writer.writeheader()
        writer.writerows(entries)


def main():
    base_dir = Path(__file__).parent
    in_crosswalk_path = base_dir / "org_name_subsets_for_cleaning" / "org_names_in_crosswalk.csv"
    not_in_crosswalk_path = base_dir / "org_name_subsets_for_cleaning" / "org_names_not_in_crosswalk.csv"

    print("Loading crosswalk matcher...")
    matcher = CrosswalkMatcher()
    print(f"Loaded {matcher.normalized_name_count} normalized names from crosswalk")

    # Process in_crosswalk file
    print(f"\nProcessing {in_crosswalk_path}...")
    merged_in, orig_in, merge_in = merge_duplicates(str(in_crosswalk_path), matcher)
    print(f"  Original entries: {orig_in}")
    print(f"  Duplicate groups merged: {merge_in}")
    print(f"  Final entries: {len(merged_in)}")
    write_merged_csv(merged_in, str(in_crosswalk_path))
    print(f"  Wrote to {in_crosswalk_path}")

    # Process not_in_crosswalk file
    print(f"\nProcessing {not_in_crosswalk_path}...")
    merged_not_in, orig_not_in, merge_not_in = merge_duplicates(str(not_in_crosswalk_path), matcher)
    print(f"  Original entries: {orig_not_in}")
    print(f"  Duplicate groups merged: {merge_not_in}")
    print(f"  Final entries: {len(merged_not_in)}")
    write_merged_csv(merged_not_in, str(not_in_crosswalk_path))
    print(f"  Wrote to {not_in_crosswalk_path}")

    print("\nDone!")


if __name__ == "__main__":
    main()
