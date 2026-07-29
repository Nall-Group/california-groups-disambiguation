#!/usr/bin/env python3
"""
Step 4 of LEGINFO_IMPORT.md — resolve every leginfo org string to its crosswalk
canonical and write the `*_canonical` columns.

Run ONCE, by the import driver, after step 3 has finalized the crosswalk. No worker RA
task ever touches the leginfo source file.

TWIN OUTPUT, NOT IN-PLACE
-------------------------
The playbook describes this as rewriting `leginfo_metadata.csv` in place. This script
instead writes a TWIN file and leaves the source pristine, because:
  * the source lives in a separate repo (Nall-Group/leginfo) that isn't ours to mutate;
  * an in-place rewrite replaces the original supporter text with resolved org names, so
    the evidence a future re-diagnosis would need is destroyed;
  * a twin makes step 4 re-runnable — when the crosswalk improves, regenerate from the
    pristine source instead of trying to re-resolve already-resolved cells.
The original org columns are therefore copied through UNCHANGED; the resolved names land
in the five new `*_canonical` columns, which is what downstream analysis reads.

RESOLUTION ORDER (per `;`-separated part of each org cell)
----------------------------------------------------------
1. Mapping files (the ONLY source of truth — the retired per-run rewrites.tsv ledger is no
   longer consulted), matched on normalized text — a part that is a known prose string or a
   known conjoined string is replaced by the org(s) it maps to. The conjoined map yields
   MULTIPLE orgs, which is how a fused string's bill count reaches every component
   instead of being dropped. A blank narrative `mapped_org` means the prose named no org:
   the part is dropped and counts nothing.
2. The cleaning regexes (`cleaning_patterns.txt`) strip trailing metadata — the same
   deterministic clean step as step 1, re-applied because no cleaned value was ever saved.
3. Crosswalk lookup: exact, then punctuation-normalized. Every node name in a cluster
   (the canonical plus all descendants at any depth) resolves to that cluster's CANONICAL
   name — these columns hold canonicals, never the literal leginfo string.

Duplicate canonicals within one cell are collapsed (several locals of the same union, or
a conjoined split whose components share a canonical, must not double-count).

Usage:
    python3 scripts/build_canonical_columns.py [--limit N] [--out PATH]

    --limit  process only the first N rows (smoke test)
    --out    twin destination (default: leginfo_metadata_canonical.csv beside the source)
"""

import argparse
import csv
import json
import os
import sys
from collections import Counter
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from org_matching_utils import (  # noqa: E402
    normalize_for_matching,
    clean_org_name,
    load_cleaning_patterns,
    strip_embedded_tables,
)

PROJECT_ROOT = Path(__file__).resolve().parent.parent
LEGINFO_PATH = Path("/Users/ruthgracewong/leginfo/extract_all_leginfo_metadata/leginfo_metadata.csv")
CROSSWALK_PATH = PROJECT_ROOT / "2_webapp" / "org_clusters_crosswalk.json"
SUBSETS_DIR = PROJECT_ROOT / "org_names_for_cleaning"
NARRATIVE_MAP = SUBSETS_DIR / "narrative_text_mapping_to_orgs.csv"
CONJOINED_MAP = SUBSETS_DIR / "conjoined_text_mapping_to_orgs.csv"

ORG_COLUMNS = [
    "support",
    "opposition",
    "opposition_unless_amended",
    "support_with_amendments",
    "sponsor",
]

csv.field_size_limit(sys.maxsize)


def load_canonical_lookup(path):
    """name -> cluster canonical, for every node in every cluster (exact + normalized).

    Children can nest to any depth (an alternate spelling of a chapter of an org), so the
    whole subtree is walked and every name maps to the TOP-LEVEL canonical — that is what
    the canonical columns are supposed to hold.
    """
    with open(path, "r", encoding="utf-8") as f:
        data = json.load(f)

    exact = {}
    normalized = {}

    def add(name, canonical):
        if not name:
            return
        exact.setdefault(name, canonical)
        norm = normalize_for_matching(name)
        if norm:
            normalized.setdefault(norm, canonical)

    def walk(node, canonical):
        if isinstance(node, dict):
            add(node.get("name"), canonical)
            for child in node.get("children") or []:
                walk(child, canonical)

    for cluster in data.get("clusters", []):
        canonical = cluster.get("canonical")
        if not canonical:
            continue
        add(canonical, canonical)
        for child in cluster.get("children") or []:
            walk(child, canonical)

    return exact, normalized


def load_routed_non_orgs():
    """Normalized strings already routed to a non-org bucket (invalid/individual/partial).

    These are supposed to resolve to no canonical, so they must not be reported as losses.
    """
    routed = set()
    for name in ("org_names_invalid.csv",
                 "org_names_partial.csv",
                 "org_names_that_are_actually_individuals.csv"):
        path = SUBSETS_DIR / name
        if not path.exists():
            continue
        with open(path, "r", encoding="utf-8", newline="") as f:
            reader = csv.reader(f)
            next(reader, None)
            for row in reader:
                if not row:
                    continue
                norm = normalize_for_matching(row[0])
                if norm:
                    routed.add(norm)
    return routed


def load_mappings():
    """normalized source string -> [org, ...]. Conjoined rows yield several orgs."""
    mapping = {}

    if NARRATIVE_MAP.exists():
        with open(NARRATIVE_MAP, "r", encoding="utf-8", newline="") as f:
            reader = csv.reader(f)
            next(reader, None)
            for row in reader:
                if not row:
                    continue
                norm = normalize_for_matching(row[0])
                mapped = row[1].strip() if len(row) > 1 else ""
                if norm:
                    # mapped_org may name SEVERAL orgs, ';'-separated ("CMHDA ; CSAC") —
                    # a prose sentence can credit more than one, and that is allowed here.
                    # Split it like the conjoined map so each org gets the count; treating
                    # "A ; B" as one literal name would match nothing and drop both.
                    # Blank mapped_org = prose naming no org: resolves to nothing.
                    mapping[norm] = [c.strip() for c in mapped.split(";") if c.strip()]

    if CONJOINED_MAP.exists():
        with open(CONJOINED_MAP, "r", encoding="utf-8", newline="") as f:
            reader = csv.reader(f)
            next(reader, None)
            for row in reader:
                if not row or len(row) < 2:
                    continue
                norm = normalize_for_matching(row[0])
                comps = [c.strip() for c in row[1].split(";") if c.strip()]
                if norm and comps:
                    mapping[norm] = comps

    return mapping


def resolve_cell(cell, mapping, cleaning_patterns, exact, normalized, routed, stats):
    """One org cell -> deduplicated list of canonical names."""
    if not cell or not cell.strip():
        return []

    # A statistical table appended inside the cell is data, not positions — drop it before
    # splitting, or every district/city name in it is credited with a position it never took.
    trimmed = strip_embedded_tables(cell)
    if trimmed != cell:
        stats["cells_with_embedded_table_trimmed"] += 1
        cell = trimmed
    if not cell.strip():
        return []

    canonicals = []
    seen = set()

    for raw_part in cell.split(";"):
        part = raw_part.strip()
        if not part:
            continue

        # Mapping lookup: try the RAW part first, then the CLEANED form. The maps are keyed
        # by the string as step 1/step 2 saw it, which is the CLEANED name (extract_org_names
        # counts orgs by cleaned name, so that is what the scan diagnosed and recorded). A raw
        # part carrying trailing metadata therefore misses its own mapping row unless we also
        # try the cleaned key — that silently stranded 361 already-diagnosed prose strings.
        candidates = mapping.get(normalize_for_matching(part))
        if candidates is None:
            cleaned_key = clean_org_name(part, cleaning_patterns)
            if cleaned_key and cleaned_key != part:
                candidates = mapping.get(normalize_for_matching(cleaned_key))
        if candidates is None:
            candidates = [part]
        else:
            stats["parts_via_mapping"] += 1
            if not candidates:
                stats["parts_mapped_to_nothing"] += 1

        for candidate in candidates:
            cleaned = clean_org_name(candidate, cleaning_patterns)
            if not cleaned:
                stats["parts_empty_after_clean"] += 1
                continue

            # A part with no alphanumeric content at all ("* * *", "---") cannot be an org and
            # cannot be looked up either — it normalizes to the empty string, so it would sit
            # in the unaccounted bucket forever no matter which CSV it were routed to.
            if not normalize_for_matching(cleaned):
                stats["parts_empty_after_clean"] += 1
                continue

            canonical = exact.get(cleaned)
            if canonical is None:
                canonical = normalized.get(normalize_for_matching(cleaned))

            if canonical is None:
                # A part that matches a routing CSV is a KNOWN non-org (invalid text, a
                # person, an ambiguous fragment). It is supposed to resolve to nothing, so
                # it is not a loss — separate it from genuinely unaccounted-for strings,
                # which are the only ones worth investigating.
                if normalize_for_matching(cleaned) in routed:
                    stats["parts_known_non_org"] += 1
                else:
                    stats["parts_unmatched"] += 1
                    stats["unmatched_examples"][cleaned] += 1
                continue

            stats["parts_matched"] += 1
            if canonical not in seen:
                seen.add(canonical)
                canonicals.append(canonical)

    return canonicals


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--limit", type=int, default=0, help="process only the first N rows")
    parser.add_argument("--out", default=None, help="twin output path")
    parser.add_argument("--dump-unaccounted", default=None,
                        help="write every distinct unaccounted-for string + occurrence count to this CSV")
    args = parser.parse_args()

    out_path = Path(args.out) if args.out else LEGINFO_PATH.with_name("leginfo_metadata_canonical.csv")
    if out_path.resolve() == LEGINFO_PATH.resolve():
        parser.error("refusing to overwrite the source file — step 4 writes a twin")

    print("Loading cleaning patterns...")
    cleaning_patterns = load_cleaning_patterns()

    print("Loading crosswalk canonicals...")
    exact, normalized = load_canonical_lookup(CROSSWALK_PATH)
    print(f"  {len(exact)} exact names -> canonical, {len(normalized)} normalized")

    print("Loading already-routed non-org strings...")
    routed = load_routed_non_orgs()
    print(f"  {len(routed)} known non-org strings")

    print("Loading prose/conjoined mappings...")
    mapping = load_mappings()
    print(f"  {len(mapping)} source strings mapped")

    stats = Counter()
    stats["unmatched_examples"] = Counter()

    tmp_path = out_path.with_suffix(out_path.suffix + ".tmp")
    rows_processed = 0

    print(f"\nStreaming {LEGINFO_PATH} -> {out_path}")
    with open(LEGINFO_PATH, "r", encoding="utf-8", errors="replace", newline="") as src, \
            open(tmp_path, "w", encoding="utf-8", newline="") as dst:
        reader = csv.DictReader(src)
        if not reader.fieldnames:
            print("ERROR: source has no header", file=sys.stderr)
            return 1

        missing = [c for c in ORG_COLUMNS if c not in reader.fieldnames]
        if missing:
            print(f"ERROR: source is missing org column(s): {missing}", file=sys.stderr)
            return 1

        canonical_columns = [f"{c}_canonical" for c in ORG_COLUMNS]
        already = [c for c in canonical_columns if c in reader.fieldnames]
        if already:
            print(f"ERROR: source already has canonical column(s) {already} — "
                  "regenerate the twin from a pristine source", file=sys.stderr)
            return 1

        writer = csv.DictWriter(dst, fieldnames=list(reader.fieldnames) + canonical_columns)
        writer.writeheader()

        for row in reader:
            for col in ORG_COLUMNS:
                canonicals = resolve_cell(
                    row.get(col), mapping, cleaning_patterns, exact, normalized, routed, stats
                )
                row[f"{col}_canonical"] = "; ".join(canonicals)
                if canonicals:
                    stats["cells_with_canonicals"] += 1
            writer.writerow(row)

            rows_processed += 1
            if rows_processed % 50000 == 0:
                print(f"  processed {rows_processed} rows...")
            if args.limit and rows_processed >= args.limit:
                break

    os.replace(tmp_path, out_path)

    print(f"\nWrote {rows_processed} rows to {out_path}")
    print("\nResolution stats:")
    print(f"  cells given canonicals:      {stats['cells_with_canonicals']}")
    print(f"  cells with an embedded table trimmed: {stats['cells_with_embedded_table_trimmed']}")
    print(f"  parts resolved via mappings: {stats['parts_via_mapping']}")
    print(f"    of which mapped to no org: {stats['parts_mapped_to_nothing']}")
    print(f"  parts matched to a canonical:{stats['parts_matched']}")
    print(f"  parts empty after cleaning:  {stats['parts_empty_after_clean']}")
    print(f"  parts that are known non-orgs:{stats['parts_known_non_org']}  (routed to an invalidity CSV — expected)")
    print(f"  parts UNACCOUNTED FOR:       {stats['parts_unmatched']}  (these lose their bill count)")

    if args.dump_unaccounted:
        with open(args.dump_unaccounted, "w", encoding="utf-8", newline="") as f:
            w = csv.writer(f)
            w.writerow(["unaccounted_string", "occurrences"])
            for name, n in stats["unmatched_examples"].most_common():
                w.writerow([name, n])
        print(f"\n  Wrote {len(stats['unmatched_examples'])} distinct unaccounted strings to {args.dump_unaccounted}")

    if stats["unmatched_examples"]:
        print("\n  Top unaccounted-for strings — not in the crosswalk and not routed anywhere:")
        for name, n in stats["unmatched_examples"].most_common(25):
            print(f"    {n:6}  {name[:80]}")

    return 0


if __name__ == "__main__":
    sys.exit(main())
