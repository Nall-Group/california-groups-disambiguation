#!/usr/bin/env python3
"""
Process batch files from grouping_output/ and merge new family entries into
the crosswalk JSON.

Each batch file contains families (canonical + children) to add as new
crosswalk clusters. This script:
1. Reads all batch_*.json files
2. Converts each family to crosswalk cluster format
3. Deduplicates against existing crosswalk entries
4. Inserts new clusters in alphabetical order
5. Writes updated crosswalk
"""

import json
import sys
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(PROJECT_ROOT))

from org_matching_utils import normalize_for_matching

CROSSWALK_PATH = PROJECT_ROOT / "2_webapp" / "org_clusters_crosswalk.json"
OUTPUT_DIR = PROJECT_ROOT / "grouping_output"


def main():
    # Load crosswalk
    print("Loading crosswalk...")
    with open(CROSSWALK_PATH, "r", encoding="utf-8") as f:
        crosswalk = json.load(f)

    clusters = crosswalk["clusters"]
    original_count = len(clusters)
    print(f"  {original_count:,} existing clusters")

    # Build set of existing normalized canonicals for dedup
    existing_canonicals = set()
    existing_all_names = set()

    def collect_names(node):
        if "canonical" in node:
            existing_all_names.add(normalize_for_matching(node["canonical"]))
        if "name" in node:
            existing_all_names.add(normalize_for_matching(node["name"]))
        for child in node.get("children", []):
            collect_names(child)

    for cluster in clusters:
        existing_canonicals.add(normalize_for_matching(cluster["canonical"]))
        collect_names(cluster)

    print(f"  {len(existing_canonicals):,} normalized canonicals")
    print(f"  {len(existing_all_names):,} total normalized names")

    # Read all batch files
    batch_files = sorted(OUTPUT_DIR.glob("batch_*.json"))
    print(f"\nReading {len(batch_files)} batch files...")

    all_families = []
    for batch_path in batch_files:
        with open(batch_path, "r", encoding="utf-8") as f:
            families = json.load(f)
        all_families.extend(families)
        print(f"  {batch_path.name}: {len(families):,} families")

    print(f"  Total families: {len(all_families):,}")

    # Convert families to crosswalk clusters and deduplicate
    print("\nConverting to crosswalk clusters...")
    new_clusters = []
    skipped_dup = 0
    skipped_empty = 0

    for family in all_families:
        canonical = family["canonical"]
        norm_canonical = normalize_for_matching(canonical)

        # Skip if canonical already exists in crosswalk
        if norm_canonical in existing_canonicals:
            skipped_dup += 1
            continue

        # Skip empty canonical names
        if not canonical.strip():
            skipped_empty += 1
            continue

        # Build cluster entry
        cluster = {
            "canonical": canonical,
            "status": "active",
            "children": [],
        }

        # Add children as alternate spellings
        for child in family.get("children", []):
            child_name = child["name"]
            norm_child = normalize_for_matching(child_name)

            # Skip if child already exists somewhere in crosswalk
            if norm_child in existing_all_names:
                continue

            cluster["children"].append({
                "name": child_name,
                "relationship": "alternate_spelling",
            })
            existing_all_names.add(norm_child)

        new_clusters.append(cluster)
        existing_canonicals.add(norm_canonical)
        existing_all_names.add(norm_canonical)

    print(f"  New clusters to add: {len(new_clusters):,}")
    print(f"  Skipped (duplicate canonical): {skipped_dup:,}")
    if skipped_empty:
        print(f"  Skipped (empty canonical): {skipped_empty:,}")

    multi_member = sum(1 for c in new_clusters if c["children"])
    singleton = sum(1 for c in new_clusters if not c["children"])
    print(f"  Multi-member: {multi_member:,}")
    print(f"  Singleton: {singleton:,}")

    # Add new clusters
    clusters.extend(new_clusters)

    # Sort all clusters alphabetically by canonical name (case-insensitive)
    clusters.sort(key=lambda c: c["canonical"].upper())
    crosswalk["clusters"] = clusters

    # Write updated crosswalk
    print(f"\nWriting updated crosswalk...")
    with open(CROSSWALK_PATH, "w", encoding="utf-8") as f:
        json.dump(crosswalk, f, indent=2, ensure_ascii=False)
        f.write("\n")

    final_count = len(clusters)
    print(f"  {original_count:,} -> {final_count:,} clusters (+{final_count - original_count:,})")

    # Show some sample additions
    print(f"\nSample new clusters:")
    shown = 0
    for c in new_clusters:
        if shown >= 10:
            break
        children_desc = ""
        if c["children"]:
            children_desc = f" ({len(c['children'])} children)"
        print(f"  {c['canonical']}{children_desc}")
        shown += 1


if __name__ == "__main__":
    main()
