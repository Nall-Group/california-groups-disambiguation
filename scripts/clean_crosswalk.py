#!/usr/bin/env python3
"""
Clean metadata suffixes like (Co-Source), (Co-Sponsor), (Sponsor) from org names
in the crosswalk JSON.

After cleaning, deduplicates children:
- If a cleaned child normalizes the same as a sibling, merge (remove the duplicate)
- If a cleaned child normalizes the same as its parent canonical, remove it

Uses clean_org_name() and normalize_for_matching() from org_matching_utils.py.
"""

import argparse
import copy
import csv
import json
import sys
from collections import defaultdict, namedtuple
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


# ---------------------------------------------------------------------------
# Global cross-level / cross-tree duplicate merge pass
# ---------------------------------------------------------------------------
#
# The existing clean_children() only dedups siblings. This pass finds every
# normalized-name duplicate across the WHOLE forest and merges them into a
# single home, per the owner-approved rule:
#   - ancestor-descendant within a tree -> remove the descendant (resolved first)
#   - within-tree                       -> deepest occurrence wins
#   - cross-tree                        -> bigger tree (more nodes) wins
#   - canonicals are NOT protected; when two things normalize the same we DROP
#     the loser's name (only its children are re-parented, never lost).

Occurrence = namedtuple(
    "Occurrence", "node parent_list cluster_idx path depth is_canonical"
)


def node_name(node):
    """Display name of a node, whether it's a canonical root or a child."""
    return node["canonical"] if "canonical" in node else node["name"]


def subtree_size(node):
    return 1 + sum(subtree_size(c) for c in node.get("children", []))


def has_metadata(node, is_canonical):
    """True if dropping this node would lose metadata fields worth flagging."""
    skip = {"name", "canonical", "children", "relationship", "status"}
    return any(k not in skip for k in node.keys())


def merge_into(winner, loser):
    """Re-parent loser's children under winner, deduping by normalized name.

    The loser's own name node is dropped by the caller (detach / cluster
    delete). Here we only move its children; on a normalized-name collision we
    recurse so duplicate *names* are dropped but whole subtrees never are.
    """
    children = winner.setdefault("children", [])
    by_norm = {}
    for c in children:
        by_norm.setdefault(normalize_for_matching(node_name(c)), c)

    for child in list(loser.get("children", [])):
        cn = normalize_for_matching(node_name(child))
        if cn in by_norm:
            merge_into(by_norm[cn], child)  # collision: drop dup name, keep subtree
        else:
            children.append(child)
            by_norm[cn] = child
    loser["children"] = []
    if not winner["children"]:
        del winner["children"]


def is_ancestor_path(a_path, b_path):
    """True if a_path is a strict ancestor of b_path (same cluster assumed)."""
    return len(a_path) < len(b_path) and b_path[: len(a_path)] == a_path


def detach(node, parent_list):
    for i, n in enumerate(parent_list):
        if n is node:
            del parent_list[i]
            return True
    return False


def build_index(clusters):
    """norm -> [Occurrence]; also returns per-cluster tree sizes (by idx)."""
    index = defaultdict(list)
    sizes = {}

    for ci, cluster in enumerate(clusters):
        sizes[ci] = subtree_size(cluster)
        index[normalize_for_matching(cluster["canonical"])].append(
            Occurrence(cluster, None, ci, (), 0, True)
        )

        def walk(node, path):
            for i, child in enumerate(node.get("children", [])):
                index[normalize_for_matching(node_name(child))].append(
                    Occurrence(child, node["children"], ci, path + (i,), len(path) + 1, False)
                )
                walk(child, path + (i,))

        walk(cluster, ())

    return index, sizes


def _do_merge(winner_occ, loser_occ, clusters, dirty, deletions, report, action):
    """Merge loser into winner, updating dirty set / pending deletions / report.

    Returns True if performed, False if deferred (a node was already dirty)."""
    w, l = winner_occ.node, loser_occ.node
    if id(w) in dirty or id(l) in dirty:
        return False

    report.append(
        {
            "norm": normalize_for_matching(node_name(w)),
            "action": action,
            "winner": node_name(w),
            "winner_cluster": winner_occ.cluster_idx,
            "winner_size": subtree_size(w),
            "loser": node_name(l),
            "loser_cluster": loser_occ.cluster_idx,
            "loser_is_canonical": loser_occ.is_canonical,
            "metadata_dropped": has_metadata(l, loser_occ.is_canonical),
        }
    )

    # Lock every node in both subtrees against further mutation this pass.
    def lock(node):
        dirty.add(id(node))
        for c in node.get("children", []):
            lock(c)

    lock(w)
    lock(l)

    merge_into(w, l)
    if loser_occ.is_canonical:
        deletions.add(loser_occ.cluster_idx)  # cluster removed after the pass
    else:
        detach(l, loser_occ.parent_list)
    return True


def _apply_deletions(clusters, deletions):
    for ci in sorted(deletions, reverse=True):
        del clusters[ci]


def ancestor_descendant_pass(clusters, report):
    """Remove descendant self-copies; merge them UP into the ancestor."""
    index, _ = build_index(clusters)
    dirty, deletions = set(), set()
    changed = False

    for occs in index.values():
        if len(occs) < 2:
            continue
        # group by cluster; ancestry only matters within one tree
        by_cluster = defaultdict(list)
        for o in occs:
            by_cluster[o.cluster_idx].append(o)
        for group in by_cluster.values():
            if len(group) < 2:
                continue
            # deepest-first so chains collapse one link per sweep
            group.sort(key=lambda o: o.depth, reverse=True)
            for i, b in enumerate(group):
                for a in group:
                    if a is b:
                        continue
                    # a is an ancestor of b?
                    if len(a.path) < len(b.path) and b.path[: len(a.path)] == a.path:
                        if _do_merge(a, b, clusters, dirty, deletions, report,
                                     "remove-descendant"):
                            changed = True
                        break

    _apply_deletions(clusters, deletions)
    return changed


def within_tree_pass(clusters, report):
    """Deepest occurrence within a single tree wins."""
    index, _ = build_index(clusters)
    dirty, deletions = set(), set()
    changed = False

    for occs in index.values():
        if len(occs) < 2:
            continue
        by_cluster = defaultdict(list)
        for o in occs:
            by_cluster[o.cluster_idx].append(o)
        for group in by_cluster.values():
            if len(group) < 2:
                continue
            winner = max(
                group,
                key=lambda o: (
                    o.depth,
                    subtree_size(o.node),
                    len(o.node.keys()),
                    tuple(-x for x in o.path),  # smallest path last -> wins ties
                ),
            )
            for loser in group:
                if loser is winner:
                    continue
                # Never merge an ancestor into its own descendant. The ancestor
                # pass (deepest-first, dirty-locked) may not have collapsed every
                # ancestor-descendant pair yet; defer those to a later sweep.
                if is_ancestor_path(loser.path, winner.path):
                    changed = True  # keep looping so the ancestor pass resolves it
                    continue
                if _do_merge(winner, loser, clusters, dirty, deletions, report,
                             "within-tree"):
                    changed = True

    _apply_deletions(clusters, deletions)
    return changed


def cross_tree_pass(clusters, report):
    """Bigger tree wins; merge occurrences from smaller trees into it."""
    index, sizes = build_index(clusters)
    dirty, deletions = set(), set()
    changed = False

    for occs in index.values():
        clusters_present = {o.cluster_idx for o in occs}
        if len(clusters_present) < 2:
            continue
        # pick the winning cluster: largest size, then a canonical occ, then idx
        canon_clusters = {o.cluster_idx for o in occs if o.is_canonical}
        win_ci = max(
            clusters_present,
            key=lambda ci: (sizes[ci], ci in canon_clusters, -ci),
        )
        win_group = [o for o in occs if o.cluster_idx == win_ci]
        # target = deepest occurrence in the winning cluster
        target = max(win_group, key=lambda o: o.depth)
        for loser in occs:
            if loser.cluster_idx == win_ci:
                continue
            if _do_merge(target, loser, clusters, dirty, deletions, report,
                         "cross-tree-fold" if loser.is_canonical else "cross-tree"):
                changed = True

    _apply_deletions(clusters, deletions)
    return changed


def global_dedup(data, patterns, report, max_sweeps=25):
    """Loop ancestor -> within-tree -> cross-tree -> sibling-dedup to a fixpoint."""
    clusters = data["clusters"]
    for sweep in range(1, max_sweeps + 1):
        changed = False
        changed |= ancestor_descendant_pass(clusters, report)
        changed |= within_tree_pass(clusters, report)
        changed |= cross_tree_pass(clusters, report)

        # sibling-dedup + canonical-redundant cleanup (reuse existing pass)
        sib_stats = {"names_cleaned": 0, "removed_redundant": 0,
                     "removed_duplicate": 0, "details": []}
        for cluster in clusters:
            cn = normalize_for_matching(cluster["canonical"])
            if "children" in cluster:
                cluster["children"] = clean_children(
                    cluster["children"], cn, patterns, sib_stats
                )
        if sib_stats["removed_redundant"] or sib_stats["removed_duplicate"]:
            changed = True

        print(f"  sweep {sweep}: {'changes applied' if changed else 'no changes (fixpoint)'}")
        if not changed:
            break
    else:
        print(f"  WARNING: hit max_sweeps={max_sweeps} without reaching a fixpoint")


def write_report(report, path):
    fields = ["norm", "action", "winner", "winner_cluster", "winner_size",
              "loser", "loser_cluster", "loser_is_canonical", "metadata_dropped"]
    with open(path, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=fields)
        w.writeheader()
        for row in report:
            w.writerow(row)
    print(f"  Wrote merge report: {path} ({len(report)} merges)")


def main():
    parser = argparse.ArgumentParser(description="Clean and deduplicate the org crosswalk.")
    parser.add_argument(
        "--global-dedup", action="store_true",
        help="Run the global cross-level/cross-tree duplicate merge pass (OFF by "
             "default so the routine RA pipeline is unchanged). PREREQUISITE: only "
             "run after SEIU has been extracted from AFL-CIO (task 1427).",
    )
    parser.add_argument(
        "--dry-run", action="store_true",
        help="Run the global-dedup pass in memory and write the merge report, but do "
             "NOT modify the JSON. Implies --global-dedup.",
    )
    parser.add_argument(
        "--report", default=str(PROJECT_ROOT / "merge_report.csv"),
        help="Path for the global-dedup merge report CSV.",
    )
    args = parser.parse_args()
    run_global_dedup = args.global_dedup or args.dry_run

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

    # Third pass: global cross-level / cross-tree duplicate merge (opt-in via flag)
    clusters_before = len(data["clusters"])
    report = []
    if run_global_dedup:
        print("\nRunning global duplicate-name merge pass...")
        global_dedup(data, patterns, report)
        write_report(report, args.report)
    else:
        print("\n(Skipping global-dedup pass; pass --global-dedup to enable it.)")

    # Write back (unless dry-run)
    if args.dry_run:
        print("\n[DRY RUN] JSON not modified. Review the merge report above.")
    else:
        print("Writing cleaned crosswalk...")
        # Keep top-level canonicals alphabetized (case-insensitive), matching merge_additions.py
        data["clusters"].sort(key=lambda c: (c.get("canonical") or "").upper())
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
    print(f"  Global-dedup merges:           {len(report)}")
    print(f"  Clusters: {clusters_before:,} -> {len(data['clusters']):,} "
          f"({clusters_before - len(data['clusters']):,} folded)")

    if stats["details"]:
        print(f"\nDetails:")
        for detail in stats["details"]:
            print(detail)


if __name__ == "__main__":
    main()
