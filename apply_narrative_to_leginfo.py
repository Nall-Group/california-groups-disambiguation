#!/usr/bin/env python3
"""
Apply narrative_org_mapping.tsv to leginfo_metadata.csv.

For each row, in the five org columns (the four stance columns plus `sponsor`), any
';'-separated item that is a known narrative fragment is recorded in a new trailing
column `narrative_orgs`:
  - the *parsed* org name (from the mapping), tagged with its source column, or the
    literal 'None parsed' when no org could be extracted;
  - multiple entries joined by ' || '.

The original stance/sponsor cell is left UNTOUCHED — the narrative prose stays in place
and is never reused as an alt spelling. Only the parsed org name flows forward, via the
`narrative_orgs` column.

Detection reuses the gaps pipeline's split('; ') + clean() transform, matching the
master fragment by raw / cleaned / normalized form (same priority as _narrative_detect.py).

Writes in place via temp-file-then-rename. Use --dry-run to validate without writing.

Usage:
  python3 apply_narrative_to_leginfo.py --dry-run [--limit N]
  python3 apply_narrative_to_leginfo.py --apply
"""
import argparse
import csv
import os
import sys
import tempfile

from org_matching_utils import CrosswalkMatcher, normalize_for_matching

csv.field_size_limit(sys.maxsize)

LEGINFO_PATH = "/Users/ruthgracewong/leginfo/extract_all_leginfo_metadata/leginfo_metadata.csv"
MAPPING_PATH = "/Users/ruthgracewong/california-groups-disambiguation/narrative_org_mapping.tsv"
ORG_COLUMNS = ["support", "opposition", "opposition_unless_amended", "support_with_amendments", "sponsor"]
NARR_COL = "narrative_orgs"
NONE_TOKEN = "None parsed"


def load_mapping(matcher):
    """Return dicts keyed by raw / normalized fragment -> replacement.

    NOTE: matching is RAW-exact + NORMALIZED only. The clean() tier is deliberately
    NOT used: clean() strips trailing stance words ("oppose"/"also"), leading "and ",
    and placeholders, which collapses legitimate clean org names onto narrative
    fragments (e.g. clean('California Nurses Association') == clean('California Nurses
    Association also')). Raw + normalized keep those words, so they only match the
    genuinely-flagged item, never the clean org sharing the same root.
    """
    by_raw, by_norm = {}, {}
    n = 0
    with open(MAPPING_PATH, encoding="utf-8") as f:
        reader = csv.reader(f, delimiter="\t")
        header = next(reader, None)
        if header != ["fragment", "extracted_org", "status"]:
            raise SystemExit(f"unexpected mapping header: {header!r}")
        for row in reader:
            if not row:
                continue
            frag = row[0]
            org = row[1] if len(row) > 1 else ""
            status = row[2] if len(row) > 2 else ""
            parsed = status.strip().lower() == "parsed" and org.strip() != ""
            repl = org.strip() if parsed else NONE_TOKEN
            by_raw[frag] = repl
            nz = normalize_for_matching(frag)
            if nz:
                by_norm.setdefault(nz, repl)
            n += 1
    print(f"loaded {n} mapping rows", file=sys.stderr)
    return by_raw, by_norm


def lookup(item, matcher, by_raw, by_norm):
    """Return replacement string if `item` is a narrative fragment, else None."""
    if item in by_raw:
        return by_raw[item]
    nz = normalize_for_matching(item)
    if nz and nz in by_norm:
        return by_norm[nz]
    return None


def process_row(row, matcher, by_raw, by_norm, stats):
    """Detect narrative fragments and record parsed orgs in NARR_COL.

    The source cell is left UNTOUCHED; only the parsed org name (or NONE_TOKEN) is
    recorded, tagged with its source column.
    """
    narr_entries = []
    for col in ORG_COLUMNS:
        val = row.get(col) or ""
        if not val.strip():
            continue
        for part in val.split(";"):
            item = part.strip()
            if not item:
                continue
            repl = lookup(item, matcher, by_raw, by_norm)
            if repl is None:
                continue
            narr_entries.append(f"{col}: {repl}")
            stats["frag_hits"] += 1
            if repl == NONE_TOKEN:
                stats["none_parsed"] += 1
    row[NARR_COL] = " || ".join(narr_entries)
    if narr_entries:
        stats["rows_changed"] += 1
    return row


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--apply", action="store_true", help="write changes in place")
    ap.add_argument("--dry-run", action="store_true", help="do not write; report only")
    ap.add_argument("--limit", type=int, default=0, help="process only first N rows (dry-run)")
    args = ap.parse_args()
    if not args.apply and not args.dry_run:
        ap.error("specify --apply or --dry-run")

    matcher = CrosswalkMatcher()
    by_raw, by_norm = load_mapping(matcher)
    stats = {"rows_changed": 0, "frag_hits": 0, "none_parsed": 0, "rows": 0}
    examples = []

    out_dir = os.path.dirname(LEGINFO_PATH)
    tmp = None
    writer = None
    if args.apply:
        tmp = tempfile.NamedTemporaryFile("w", dir=out_dir, delete=False,
                                          newline="", encoding="utf-8", suffix=".tmp")

    with open(LEGINFO_PATH, encoding="utf-8", errors="replace", newline="") as f:
        reader = csv.DictReader(f)
        fieldnames = list(reader.fieldnames)
        if NARR_COL not in fieldnames:
            fieldnames.append(NARR_COL)
        if args.apply:
            writer = csv.DictWriter(tmp, fieldnames=fieldnames)
            writer.writeheader()
        for row in reader:
            stats["rows"] += 1
            if args.limit and stats["rows"] > args.limit:
                break
            src_cells = {c: row.get(c) for c in ORG_COLUMNS if (row.get(c) or "").strip()}
            row = process_row(row, matcher, by_raw, by_norm, stats)
            if writer:
                writer.writerow(row)
            if row[NARR_COL] and len(examples) < 20:
                examples.append((src_cells, row[NARR_COL]))
            if stats["rows"] % 50000 == 0:
                print(f"...{stats['rows']} rows", file=sys.stderr)

    if args.apply:
        tmp.close()
        os.replace(tmp.name, LEGINFO_PATH)
        print(f"wrote {LEGINFO_PATH}", file=sys.stderr)

    print(f"rows processed:   {stats['rows']}")
    print(f"rows changed:     {stats['rows_changed']}")
    print(f"fragment hits:    {stats['frag_hits']}")
    print(f"  -> None parsed: {stats['none_parsed']}")
    print("\n=== examples (source cells left intact; parsed orgs -> narrative_orgs) ===")
    for src_cells, narr in examples[:12]:
        print(f"narrative_orgs: {narr}")
        for c, v in src_cells.items():
            print(f"  {c}: {v!r}")
        print()


if __name__ == "__main__":
    main()
