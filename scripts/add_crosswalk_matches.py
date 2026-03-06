#!/usr/bin/env python3
"""
Add entries from crosswalk_matches.json as alternate_spelling children
of their matched crosswalk clusters.

These are entries from org_names_not_in_crosswalk.csv that match existing
crosswalk organizations via enhanced normalization (stripping corporate
suffixes, "the" prefix, etc.) but weren't caught by basic normalization.
"""

import json
import sys
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(PROJECT_ROOT))

from org_matching_utils import normalize_for_matching

CROSSWALK_PATH = PROJECT_ROOT / "2_webapp" / "org_clusters_crosswalk.json"
MATCHES_PATH = PROJECT_ROOT / "grouping_output" / "crosswalk_matches.json"


def main():
    # Load crosswalk
    print("Loading crosswalk...")
    with open(CROSSWALK_PATH, "r", encoding="utf-8") as f:
        crosswalk = json.load(f)

    clusters = crosswalk["clusters"]
    print(f"  {len(clusters):,} clusters")

    # Build canonical -> cluster index for fast lookup
    canonical_to_idx = {}
    for i, cluster in enumerate(clusters):
        canonical_to_idx[cluster["canonical"].upper()] = i

    # Also build normalized canonical -> index
    norm_canonical_to_idx = {}
    for i, cluster in enumerate(clusters):
        norm = normalize_for_matching(cluster["canonical"])
        if norm not in norm_canonical_to_idx:
            norm_canonical_to_idx[norm] = i

    # Load matches
    print(f"Loading {MATCHES_PATH.name}...")
    with open(MATCHES_PATH, "r", encoding="utf-8") as f:
        matches = json.load(f)

    print(f"  {len(matches)} canonical groups with new children")
    total_children = sum(len(m["new_children"]) for m in matches)
    print(f"  {total_children} total new children to add")

    # Collect all existing names (to avoid adding duplicates)
    existing_names = set()

    def collect_names(node):
        if "canonical" in node:
            existing_names.add(node["canonical"].upper())
        if "name" in node:
            existing_names.add(node["name"].upper())
        for child in node.get("children", []):
            collect_names(child)

    for cluster in clusters:
        collect_names(cluster)

    # Add children to their matched clusters
    added_count = 0
    skipped_count = 0
    not_found = 0

    for match_group in matches:
        canonical = match_group["canonical"]

        # Find the cluster
        idx = canonical_to_idx.get(canonical.upper())
        if idx is None:
            # Try normalized lookup
            norm = normalize_for_matching(canonical)
            idx = norm_canonical_to_idx.get(norm)

        if idx is None:
            print(f"  WARNING: Could not find cluster for canonical '{canonical}'")
            not_found += 1
            continue

        cluster = clusters[idx]
        if "children" not in cluster:
            cluster["children"] = []

        for child_info in match_group["new_children"]:
            child_name = child_info["org_name"]

            # Skip if this name already exists in the crosswalk
            if child_name.upper() in existing_names:
                skipped_count += 1
                continue

            cluster["children"].append({
                "name": child_name,
                "relationship": "alternate_spelling",
            })
            existing_names.add(child_name.upper())
            added_count += 1

    print(f"\nResults:")
    print(f"  Added: {added_count}")
    print(f"  Skipped (already exists): {skipped_count}")
    if not_found:
        print(f"  Canonical not found: {not_found}")

    # Write updated crosswalk
    print(f"\nWriting updated crosswalk...")
    with open(CROSSWALK_PATH, "w", encoding="utf-8") as f:
        json.dump(crosswalk, f, indent=2, ensure_ascii=False)
        f.write("\n")

    print(f"  Done! {len(clusters):,} clusters")

    # Show some examples
    print(f"\nSample additions:")
    shown = 0
    for match_group in matches:
        if shown >= 5:
            break
        canonical = match_group["canonical"]
        for child_info in match_group["new_children"]:
            print(f"  '{child_info['org_name']}' -> '{canonical}' (alternate_spelling)")
            shown += 1
            if shown >= 5:
                break


if __name__ == "__main__":
    main()
