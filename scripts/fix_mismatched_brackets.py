#!/usr/bin/env python3
"""
Fix mismatched brackets in org names in the crosswalk JSON.

Handles five categories of mismatched brackets:
  1. Missing closing bracket: "Org (ACRONYM" -> "Org (ACRONYM)"
  2. Metadata remnants: "Org (Oppose" -> "Org"
  3. Stray closing bracket with acronym: "Org ACRONYM)" -> "Org (ACRONYM)"
  4. Stray closing bracket other: "Org)" -> "Org"
  5. Leading fragment: "2007) IFPTE" -> "IFPTE"

After fixing, deduplicates children:
- If a fixed child normalizes the same as a sibling, merge (remove the duplicate)
- If a fixed child normalizes the same as its parent canonical, remove it

Also handles canonical merges when Category 5 fixes create duplicate canonicals.

Uses fix_mismatched_brackets() and normalize_for_matching() from org_matching_utils.py.
"""

import json
import sys
from pathlib import Path

# Add project root so we can import org_matching_utils
PROJECT_ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(PROJECT_ROOT))

from org_matching_utils import fix_mismatched_brackets, normalize_for_matching

CROSSWALK_PATH = PROJECT_ROOT / "2_webapp" / "org_clusters_crosswalk.json"


def has_mismatched_brackets(name):
    """Check if a name has mismatched round or square brackets."""
    if name.count('(') != name.count(')'):
        return True
    if name.count('[') != name.count(']'):
        return True
    return False


def fix_children(children, canonical_norm, stats):
    """Recursively fix children names and deduplicate.

    Returns a new list of children with fixed names and duplicates removed.
    """
    fixed_children = []
    seen_norms = {}  # normalized -> index in fixed_children

    for child in children:
        orig_name = child["name"]
        fixed_name = fix_mismatched_brackets(orig_name)

        if fixed_name != orig_name:
            stats["names_fixed"] += 1
            stats["details"].append(f"  Fixed: \"{orig_name}\" -> \"{fixed_name}\"")

        norm = normalize_for_matching(fixed_name)

        # Check if redundant with canonical
        if norm == canonical_norm:
            stats["removed_redundant"] += 1
            stats["details"].append(
                f"  Removed (redundant with canonical): \"{orig_name}\" -> \"{fixed_name}\""
            )
            continue

        # Check if duplicate of an already-seen sibling
        if norm in seen_norms:
            stats["removed_duplicate"] += 1
            existing = fixed_children[seen_norms[norm]]["name"]
            stats["details"].append(
                f"  Removed (duplicate of \"{existing}\"): \"{orig_name}\" -> \"{fixed_name}\""
            )
            continue

        # Build new child entry
        new_child = dict(child)
        new_child["name"] = fixed_name

        # Recursively fix nested children
        if "children" in new_child:
            new_child["children"] = fix_children(
                new_child["children"], canonical_norm, stats
            )
            # Remove empty children list
            if not new_child["children"]:
                del new_child["children"]

        seen_norms[norm] = len(fixed_children)
        fixed_children.append(new_child)

    return fixed_children


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
    print("Loading crosswalk...")
    with open(CROSSWALK_PATH, "r", encoding="utf-8") as f:
        data = json.load(f)
    print(f"  {len(data['clusters']):,} clusters\n")

    # First pass: count mismatched brackets before fixing
    before_count = 0
    for cluster in data["clusters"]:
        if has_mismatched_brackets(cluster["canonical"]):
            before_count += 1
        for child in cluster.get("children", []):
            if has_mismatched_brackets(child["name"]):
                before_count += 1
            for grandchild in child.get("children", []):
                if has_mismatched_brackets(grandchild["name"]):
                    before_count += 1
    print(f"Found {before_count} names with mismatched brackets\n")

    stats = {
        "names_fixed": 0,
        "canonicals_fixed": 0,
        "removed_redundant": 0,
        "removed_duplicate": 0,
        "clusters_merged": 0,
        "details": [],
    }

    # Fix canonical names and build merge map for Category 5 duplicates
    # canonical_norm -> index of first cluster with that norm
    canonical_norm_map = {}
    merge_targets = {}  # index -> target index (for clusters that should be merged)

    for i, cluster in enumerate(data["clusters"]):
        orig_canonical = cluster["canonical"]
        fixed_canonical = fix_mismatched_brackets(orig_canonical)

        if fixed_canonical != orig_canonical:
            stats["canonicals_fixed"] += 1
            stats["details"].append(
                f"  Canonical fixed: \"{orig_canonical}\" -> \"{fixed_canonical}\""
            )
            cluster["canonical"] = fixed_canonical

        norm = normalize_for_matching(fixed_canonical)

        if norm in canonical_norm_map:
            # This cluster's canonical now matches an existing one — mark for merge
            merge_targets[i] = canonical_norm_map[norm]
            stats["clusters_merged"] += 1
            stats["details"].append(
                f"  Cluster merge: \"{fixed_canonical}\" (was \"{orig_canonical}\") "
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

    # Fix and deduplicate children
    for cluster in data["clusters"]:
        canonical_norm = normalize_for_matching(cluster["canonical"])

        if "children" in cluster:
            cluster["children"] = fix_children(
                cluster["children"], canonical_norm, stats
            )
            if not cluster["children"]:
                del cluster["children"]

    # Verify: count remaining mismatched brackets
    after_count = 0
    remaining = []
    for cluster in data["clusters"]:
        if has_mismatched_brackets(cluster["canonical"]):
            after_count += 1
            remaining.append(("CANONICAL", cluster["canonical"]))
        for child in cluster.get("children", []):
            if has_mismatched_brackets(child["name"]):
                after_count += 1
                remaining.append(("child", child["name"]))
            for grandchild in child.get("children", []):
                if has_mismatched_brackets(grandchild["name"]):
                    after_count += 1
                    remaining.append(("grandchild", grandchild["name"]))

    # Write back
    print("Writing fixed crosswalk...")
    with open(CROSSWALK_PATH, "w", encoding="utf-8") as f:
        json.dump(data, f, indent=2, ensure_ascii=False)
        f.write("\n")

    # Summary
    print("\n" + "=" * 60)
    print("SUMMARY")
    print("=" * 60)
    print(f"  Mismatched brackets before:     {before_count}")
    print(f"  Canonical names fixed:          {stats['canonicals_fixed']}")
    print(f"  Child names fixed:              {stats['names_fixed']}")
    print(f"  Children removed (redundant):   {stats['removed_redundant']}")
    print(f"  Children removed (duplicate):   {stats['removed_duplicate']}")
    print(f"  Clusters merged:                {stats['clusters_merged']}")
    print(f"  Mismatched brackets remaining:  {after_count}")

    if remaining:
        print(f"\nWARNING: {after_count} names still have mismatched brackets:")
        for role, name in remaining:
            print(f"  [{role}] {repr(name)}")

    if stats["details"]:
        print(f"\nDetails ({len(stats['details'])} changes):")
        for detail in stats["details"]:
            print(detail)


if __name__ == "__main__":
    main()
