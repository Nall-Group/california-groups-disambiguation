#!/usr/bin/env python3
"""
Clean metadata suffixes like (Co-Source), (Co-Sponsor), (Sponsor) from org names
in the crosswalk JSON.

After cleaning, deduplicates children:
- If a cleaned child normalizes the same as a sibling, merge (remove the duplicate)
- If a cleaned child normalizes the same as its parent canonical, remove it

Uses clean_org_name() and normalize_for_matching() from org_matching_utils.py.
"""

import json
import sys
from pathlib import Path

# Add project root so we can import org_matching_utils
PROJECT_ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(PROJECT_ROOT))

from org_matching_utils import clean_org_name, load_cleaning_patterns, normalize_for_matching

CROSSWALK_PATH = PROJECT_ROOT / "2_webapp" / "org_clusters_crosswalk.json"


def clean_children(children, canonical_norm, patterns, stats):
    """Recursively clean children names and deduplicate.

    Returns a new list of children with cleaned names and duplicates removed.
    """
    cleaned_children = []
    seen_norms = {}  # normalized -> index in cleaned_children

    for child in children:
        orig_name = child["name"]
        cleaned_name = clean_org_name(orig_name, patterns)

        if cleaned_name != orig_name:
            stats["names_cleaned"] += 1
            stats["details"].append(f"  Cleaned: \"{orig_name}\" -> \"{cleaned_name}\"")

        norm = normalize_for_matching(cleaned_name)

        # Check if redundant with canonical
        if norm == canonical_norm:
            stats["removed_redundant"] += 1
            stats["details"].append(
                f"  Removed (redundant with canonical): \"{orig_name}\" -> \"{cleaned_name}\""
            )
            continue

        # Check if duplicate of an already-seen sibling
        if norm in seen_norms:
            stats["removed_duplicate"] += 1
            existing = cleaned_children[seen_norms[norm]]["name"]
            stats["details"].append(
                f"  Removed (duplicate of \"{existing}\"): \"{orig_name}\" -> \"{cleaned_name}\""
            )
            continue

        # Build new child entry
        new_child = dict(child)
        new_child["name"] = cleaned_name

        # Recursively clean nested children
        if "children" in new_child:
            new_child["children"] = clean_children(
                new_child["children"], canonical_norm, patterns, stats
            )
            # Remove empty children list
            if not new_child["children"]:
                del new_child["children"]

        seen_norms[norm] = len(cleaned_children)
        cleaned_children.append(new_child)

    return cleaned_children


def merge_clusters(target, source):
    """Merge source cluster's children into target cluster."""
    target_children = target.get("children", [])
    source_children = source.get("children", [])

    # Build set of normalized names already in target
    existing_norms = set()
    canonical_norm = normalize_for_matching(target["canonical"])
    existing_norms.add(canonical_norm)

    def collect_norms(children):
        for child in children:
            existing_norms.add(normalize_for_matching(child["name"]))
            if "children" in child:
                collect_norms(child["children"])

    collect_norms(target_children)

    # Add source children that aren't duplicates
    for child in source_children:
        norm = normalize_for_matching(child["name"])
        if norm not in existing_norms:
            target_children.append(child)
            existing_norms.add(norm)

    if target_children:
        target["children"] = target_children


def main():
    print("Loading cleaning patterns...")
    patterns = load_cleaning_patterns()
    print(f"  Loaded {len(patterns)} patterns\n")

    print("Loading crosswalk...")
    with open(CROSSWALK_PATH, "r", encoding="utf-8") as f:
        data = json.load(f)
    print(f"  {len(data['clusters']):,} clusters\n")

    stats = {
        "names_cleaned": 0,
        "removed_redundant": 0,
        "removed_duplicate": 0,
        "canonicals_cleaned": 0,
        "clusters_merged": 0,
        "details": [],
    }

    # First pass: clean canonical names and detect collisions
    canonical_norm_map = {}  # normalized -> index of first cluster with that norm
    merge_targets = {}  # index -> target index (for clusters that should be merged)

    for i, cluster in enumerate(data["clusters"]):
        orig_canonical = cluster["canonical"]
        cleaned_canonical = clean_org_name(orig_canonical, patterns)
        if cleaned_canonical != orig_canonical:
            stats["canonicals_cleaned"] += 1
            stats["details"].append(
                f"  Canonical cleaned: \"{orig_canonical}\" -> \"{cleaned_canonical}\""
            )
            cluster["canonical"] = cleaned_canonical

        norm = normalize_for_matching(cleaned_canonical)

        if norm in canonical_norm_map:
            # This cluster's canonical now matches an existing one — mark for merge
            merge_targets[i] = canonical_norm_map[norm]
            stats["clusters_merged"] += 1
            stats["details"].append(
                f"  Cluster merge: \"{cleaned_canonical}\" (was \"{orig_canonical}\") "
                f"-> merging into \"{data['clusters'][canonical_norm_map[norm]]['canonical']}\""
            )
        else:
            canonical_norm_map[norm] = i

    # Merge duplicate clusters (in reverse order to preserve indices)
    if merge_targets:
        for source_idx in sorted(merge_targets.keys(), reverse=True):
            target_idx = merge_targets[source_idx]
            merge_clusters(data["clusters"][target_idx], data["clusters"][source_idx])
            del data["clusters"][source_idx]

    # Second pass: clean and deduplicate children
    for cluster in data["clusters"]:
        canonical_norm = normalize_for_matching(cluster["canonical"])

        if "children" in cluster:
            cluster["children"] = clean_children(
                cluster["children"], canonical_norm, patterns, stats
            )

    # Write back
    print("Writing cleaned crosswalk...")
    with open(CROSSWALK_PATH, "w", encoding="utf-8") as f:
        json.dump(data, f, indent=2, ensure_ascii=False)
        f.write("\n")

    # Summary
    print("\n" + "=" * 60)
    print("SUMMARY")
    print("=" * 60)
    print(f"  Canonical names cleaned:       {stats['canonicals_cleaned']}")
    print(f"  Clusters merged:               {stats['clusters_merged']}")
    print(f"  Child names cleaned:           {stats['names_cleaned']}")
    print(f"  Children removed (redundant):  {stats['removed_redundant']}")
    print(f"  Children removed (duplicate):  {stats['removed_duplicate']}")
    total_removed = stats["removed_redundant"] + stats["removed_duplicate"]
    print(f"  Total children removed:        {total_removed}")

    if stats["details"]:
        print(f"\nDetails:")
        for detail in stats["details"]:
            print(detail)


if __name__ == "__main__":
    main()
