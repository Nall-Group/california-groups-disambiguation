#!/usr/bin/env python3
"""
Group entries from org_names_not_in_crosswalk.csv into families and prepare
batch files for parallel crosswalk additions.

Three-layer grouping:
1. Match against existing crosswalk using enhanced normalization
2. Group remaining entries among themselves by enhanced normalization
3. Flag remaining invalid entries for removal

Output (in grouping_output/):
- crosswalk_matches.json: entries that match existing crosswalk orgs
- families_for_crosswalk.json: new families to add
- flagged_invalid.json: entries categorized as invalid
- batch_00.json through batch_09.json: families split for parallel processing
"""

import csv
import json
import re
import sys
from collections import defaultdict
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(PROJECT_ROOT))

from org_matching_utils import (
    CrosswalkMatcher,
    normalize_for_matching,
    clean_org_name,
    load_cleaning_patterns,
)

SUBSETS_DIR = PROJECT_ROOT / "org_name_subsets_for_cleaning"
NOT_IN_CROSSWALK = SUBSETS_DIR / "org_names_not_in_crosswalk.csv"
OUTPUT_DIR = PROJECT_ROOT / "grouping_output"
CROSSWALK_PATH = PROJECT_ROOT / "2_webapp" / "org_clusters_crosswalk.json"

NUM_BATCHES = 10

# Corporate suffixes to strip for enhanced normalization
CORPORATE_SUFFIXES = re.compile(
    r'\b(?:'
    r'inc|incorporated|corp|corporation|llc|llp|ltd|limited|'
    r'co|company|lp|pllc|pa|pc|plc|sa|na'
    r')$',
    re.IGNORECASE
)


def enhanced_normalize(name: str) -> str:
    """
    Enhanced normalization: base normalize, then strip leading 'the '
    and trailing corporate suffixes.
    """
    s = normalize_for_matching(name)
    # Strip leading "the "
    if s.startswith("the "):
        s = s[4:]
    # Strip trailing corporate suffixes (may need multiple passes for "inc co")
    for _ in range(3):
        stripped = CORPORATE_SUFFIXES.sub('', s).strip()
        if stripped == s:
            break
        s = stripped
    return s


# ---------------------------------------------------------------------------
# Invalid entry detection
# ---------------------------------------------------------------------------

# Honorifics that strongly indicate individual names.
# Excludes "Justice", "Chief", "Dr", "Director", "Secretary", "President",
# "Supervisor" etc. as these appear too often in org names.
HONORIFICS = re.compile(
    r'^(?:Senator|Assemblymember|Assemblywoman|Assemblyman|'
    r'Representative|Congressman|Congresswoman|Congressmember|'
    r'Governor|Attorney General|'
    r'Mr|Mrs|Ms|Miss|Rev|Reverend)\b',
    re.IGNORECASE
)

# Titles that need a personal name after them to count as individual.
# These are checked in two steps: first case-insensitive title match,
# then case-sensitive check for a capitalized personal name following.
# Excludes "Justice", "Chief", "Director", "Supervisor" - too many org name false positives
WEAK_TITLES = re.compile(
    r'^(?:Mayor|Judge|Sheriff|Treasurer|Controller|'
    r'Superintendent|Lieutenant Governor|Commissioner|'
    r'Councilmember|Councilwoman|Councilman|Alderman|Alderwoman|Dr\.?)\b',
    re.IGNORECASE
)

# Pattern for "Firstname Lastname" — disabled due to too many false positives
# Two capitalized words matches way too many org names like "Environmental Defense"
# Only use honorific-based detection for individuals
PERSON_NAME_PATTERN = None  # Not used

# Org indicator words that suggest it's NOT a person name
ORG_INDICATORS = re.compile(
    r'\b(?:Association|Society|Council|Committee|Commission|Board|'
    r'Foundation|Institute|Center|Centre|Agency|Authority|Bureau|'
    r'Department|Division|Office|Corporation|Company|Group|'
    r'Alliance|Coalition|Federation|Union|League|Network|'
    r'Club|Organization|Organisation|Trust|Fund|Program|Project|'
    r'University|College|School|Academy|Library|Museum|Hospital|'
    r'Church|Temple|Synagogue|Mosque|Ministry|Parish|Diocese|'
    r'Chamber|Exchange|Guild|Lodge|Chapter|Local|District|'
    r'County|City|Town|Village|State|National|International|'
    r'Inc|Corp|LLC|LLP|Ltd|Co)\b',
    re.IGNORECASE
)

# Number/date patterns
NUMBERS_DATES_PATTERN = re.compile(r'^[\d\s.,/\-:;()+]+$')
# Date start: month name must be followed by a space and a digit (actual date)
DATE_START_PATTERN = re.compile(
    r'^(?:(?:January|February|March|April|May|June|July|August|'
    r'September|October|November|December)\s+\d'
    r'|\d{1,2}/\d{1,2})',
    re.IGNORECASE
)

# Legislative bill patterns — require bill prefix + number as the whole name
# (or followed by space + metadata like author name)
BILL_PATTERN = re.compile(
    r'^(?:AB|SB|HR|SR|ACR|SCR|AJR|SJR|ACA|SCA)\s*\d+\b'
    r'(?:\s|$|[,\-\(])',
    re.IGNORECASE
)

# Procedural text patterns — use word boundaries to avoid prefix-matching real words
PROCEDURAL_PATTERN = re.compile(
    r'^(?:GOVERNOR\'?S?\s+VETO\b|ENROLLED\b|CHAPTERED\b|AMENDED\b|'
    r'VETOED\b|PASSED\b|FAILED\b|DIED\b|HELD\b|REFERRED\b|FISCAL\b)'
    r'(?:\s|$|[,\-])',
    re.IGNORECASE
)


def classify_invalid(org_name: str) -> str | None:
    """
    Classify an entry as invalid if it matches known junk patterns.

    Returns category string or None if the entry looks valid.
    """
    stripped = org_name.strip()

    if not stripped:
        return "invalid"

    # Numbers/dates: pure numeric strings
    if NUMBERS_DATES_PATTERN.match(stripped):
        return "numbers_dates"

    # Date start
    if DATE_START_PATTERN.match(stripped):
        return "numbers_dates"

    # Starts with parentheses
    if stripped.startswith("("):
        return "starts_with_parens"

    # Too short: 3 chars or fewer, unless all-caps acronym
    if len(stripped) <= 3:
        if stripped.isalpha() and stripped.isupper() and len(stripped) >= 2:
            pass  # keep — looks like an acronym (e.g., "AFL", "CTA")
        else:
            return "partial"

    # Legislative bills — but not if the name contains org indicators
    # (e.g., "Ab 540 Ally Training Project" is a real org)
    if BILL_PATTERN.match(stripped):
        if not ORG_INDICATORS.search(stripped):
            return "invalid"

    # Procedural text — but not if it contains org indicators
    # (e.g., "Fiscal Credit Union" is a real org)
    if PROCEDURAL_PATTERN.match(stripped):
        if not ORG_INDICATORS.search(stripped):
            return "invalid"

    # Narrative text: very long entries (>100 chars) that look like prose
    if len(stripped) > 100:
        # Count lowercase words — prose has lots of short lowercase words
        words = stripped.split()
        if len(words) > 10:
            lowercase_words = sum(1 for w in words if w[0].islower())
            if lowercase_words / len(words) > 0.5:
                return "narrative_text"

    # Not capitalized: first significant word starts with lowercase
    # Skip leading articles/prepositions for the check
    first_word = stripped.split()[0] if stripped.split() else ""
    if first_word and first_word[0].islower():
        # Allow known lowercase-start org names like "eBay", "iPhone"
        # But flag things like "the Valley" or "care"
        if not re.match(r'^[a-z][A-Z]', first_word):  # not camelCase like eBay
            return "not_capitalized"

    # Individual names: starts with strong honorific (Senator, Congressman, etc.)
    if HONORIFICS.match(stripped):
        if not ORG_INDICATORS.search(stripped):
            return "individual"

    # Titles followed by a capitalized personal name (Mayor John Smith, etc.)
    # Two-step check: case-insensitive title, then case-sensitive name check
    title_match = WEAK_TITLES.match(stripped)
    if title_match:
        remainder = stripped[title_match.end():].lstrip()
        # Check remainder starts with a capitalized word (personal name)
        if remainder and remainder[0].isupper():
            if not ORG_INDICATORS.search(stripped):
                return "individual"

    # Conjoined: contains " and " joining what look like two separate orgs
    # Be careful — many real org names contain "and"
    # Only flag if both sides look org-like (capitalized multi-word)
    if "/" in stripped:
        parts = stripped.split("/")
        if len(parts) == 2:
            p1, p2 = parts[0].strip(), parts[1].strip()
            if (len(p1) > 5 and len(p2) > 5 and
                    p1[0].isupper() and p2[0].isupper() and
                    ORG_INDICATORS.search(p1) and ORG_INDICATORS.search(p2)):
                return "conjoined"

    return None


# ---------------------------------------------------------------------------
# CSV I/O
# ---------------------------------------------------------------------------
def read_csv(filepath):
    """Read CSV and return list of (org_name, count) tuples."""
    rows = []
    with open(filepath, "r", encoding="utf-8", newline="") as f:
        reader = csv.reader(f)
        next(reader)  # skip header
        for row in reader:
            if not row:
                continue
            rows.append((row[0], int(row[1])))
    return rows


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
def main():
    OUTPUT_DIR.mkdir(exist_ok=True)

    print("Loading crosswalk matcher...")
    matcher = CrosswalkMatcher()
    print(f"  {matcher.exact_name_count:,} exact names, "
          f"{matcher.normalized_name_count:,} normalized names")

    # Also load the crosswalk directly to build enhanced-normalized lookup
    print("Building enhanced normalization index from crosswalk...")
    with open(CROSSWALK_PATH, "r", encoding="utf-8") as f:
        crosswalk_data = json.load(f)

    # Build enhanced-normalized -> canonical mapping
    enhanced_to_canonical = {}

    def walk_cluster(cluster):
        canonical = cluster["canonical"]
        en = enhanced_normalize(canonical)
        if en and en not in enhanced_to_canonical:
            enhanced_to_canonical[en] = canonical

        def walk_children(children):
            for child in children:
                en_child = enhanced_normalize(child["name"])
                if en_child and en_child not in enhanced_to_canonical:
                    enhanced_to_canonical[en_child] = canonical
                if "children" in child:
                    walk_children(child["children"])

        walk_children(cluster.get("children", []))

    for cluster in crosswalk_data["clusters"]:
        walk_cluster(cluster)

    print(f"  {len(enhanced_to_canonical):,} enhanced-normalized entries")

    # Read not-in-crosswalk entries
    print(f"\nReading {NOT_IN_CROSSWALK.name}...")
    entries = read_csv(NOT_IN_CROSSWALK)
    print(f"  {len(entries):,} entries")

    # Load cleaning patterns for metadata cleanup
    cleaning_patterns = load_cleaning_patterns()

    # ---------------------------------------------------------------------------
    # Layer 0: Flag invalid entries
    # ---------------------------------------------------------------------------
    print("\nFlagging invalid entries...")
    flagged_invalid = defaultdict(list)  # category -> [(org_name, count)]
    valid_entries = []

    for org_name, count in entries:
        # Clean metadata first
        cleaned = clean_org_name(org_name, cleaning_patterns)
        category = classify_invalid(cleaned)
        if category:
            flagged_invalid[category].append((org_name, count))
        else:
            valid_entries.append((org_name, count))

    total_flagged = sum(len(v) for v in flagged_invalid.values())
    print(f"  Flagged invalid: {total_flagged:,}")
    for cat, items in sorted(flagged_invalid.items(), key=lambda x: -len(x[1])):
        print(f"    {cat}: {len(items):,}")
        for name, count in items[:3]:
            print(f"      e.g. {name!r} ({count})")

    # Save flagged invalid
    flagged_output = {}
    for cat, items in flagged_invalid.items():
        flagged_output[cat] = [{"org_name": n, "count": c} for n, c in items]
    with open(OUTPUT_DIR / "flagged_invalid.json", "w", encoding="utf-8") as f:
        json.dump(flagged_output, f, indent=2, ensure_ascii=False)
        f.write("\n")

    print(f"\n  Valid entries remaining: {len(valid_entries):,}")

    # ---------------------------------------------------------------------------
    # Layer 1: Match against existing crosswalk (enhanced normalization)
    # ---------------------------------------------------------------------------
    print("\nMatching against existing crosswalk (enhanced normalization)...")
    crosswalk_matches = []  # [(org_name, count, canonical)]
    unmatched = []

    for org_name, count in valid_entries:
        cleaned = clean_org_name(org_name, cleaning_patterns)
        # First try standard matching
        result = matcher.match(org_name)
        if result.is_match:
            crosswalk_matches.append((org_name, count, result.canonical))
            continue

        # Try enhanced normalization
        en = enhanced_normalize(cleaned)
        if en in enhanced_to_canonical:
            crosswalk_matches.append((org_name, count, enhanced_to_canonical[en]))
            continue

        unmatched.append((org_name, count))

    print(f"  Matched to existing crosswalk: {len(crosswalk_matches):,}")
    print(f"  Unmatched: {len(unmatched):,}")

    # Save crosswalk matches
    matches_output = []
    # Group by canonical
    matches_by_canonical = defaultdict(list)
    for org_name, count, canonical in crosswalk_matches:
        matches_by_canonical[canonical].append({"org_name": org_name, "count": count})

    for canonical, members in sorted(matches_by_canonical.items()):
        matches_output.append({
            "canonical": canonical,
            "new_children": sorted(members, key=lambda x: -x["count"]),
        })

    with open(OUTPUT_DIR / "crosswalk_matches.json", "w", encoding="utf-8") as f:
        json.dump(matches_output, f, indent=2, ensure_ascii=False)
        f.write("\n")

    # Show some examples
    print(f"\n  Top crosswalk matches by canonical:")
    for item in matches_output[:5]:
        total = sum(c["count"] for c in item["new_children"])
        print(f"    {item['canonical']}: {len(item['new_children'])} variants, total count={total}")
        for child in item["new_children"][:3]:
            print(f"      - {child['org_name']} ({child['count']})")

    # ---------------------------------------------------------------------------
    # Layer 2: Group unmatched entries among themselves
    # ---------------------------------------------------------------------------
    print(f"\nGrouping {len(unmatched):,} unmatched entries by enhanced normalization...")

    # Group by enhanced normalized form
    groups = defaultdict(list)  # enhanced_norm -> [(org_name, count)]
    for org_name, count in unmatched:
        cleaned = clean_org_name(org_name, cleaning_patterns)
        en = enhanced_normalize(cleaned)
        groups[en].append((org_name, count))

    # Build families: highest count member is canonical
    families = []
    for en_key, members in groups.items():
        members.sort(key=lambda x: -x[1])
        canonical_name = members[0][0]
        canonical_count = members[0][1]

        children = []
        for name, count in members[1:]:
            children.append({"name": name, "count": count})

        family = {
            "canonical": canonical_name,
            "canonical_count": canonical_count,
            "total_count": sum(c for _, c in members),
            "children": children,
        }
        families.append(family)

    # Sort families by total count descending
    families.sort(key=lambda x: -x["total_count"])

    multi_families = [f for f in families if len(f["children"]) > 0]
    singletons = [f for f in families if len(f["children"]) == 0]

    print(f"  Total families: {len(families):,}")
    print(f"  Multi-member families: {len(multi_families):,}")
    print(f"  Singletons: {len(singletons):,}")

    # Save all families
    with open(OUTPUT_DIR / "families_for_crosswalk.json", "w", encoding="utf-8") as f:
        json.dump(families, f, indent=2, ensure_ascii=False)
        f.write("\n")

    # Show top families
    print(f"\n  Top multi-member families:")
    for fam in multi_families[:10]:
        print(f"    {fam['canonical']} (count={fam['canonical_count']}, "
              f"total={fam['total_count']}, {len(fam['children'])} variants)")
        for child in fam["children"][:3]:
            print(f"      - {child['name']} ({child['count']})")

    # ---------------------------------------------------------------------------
    # Split into batches (round-robin by rank)
    # ---------------------------------------------------------------------------
    print(f"\nSplitting {len(families):,} families into {NUM_BATCHES} batches...")

    batches = [[] for _ in range(NUM_BATCHES)]
    for i, family in enumerate(families):
        batches[i % NUM_BATCHES].append(family)

    for i, batch in enumerate(batches):
        batch_path = OUTPUT_DIR / f"batch_{i:02d}.json"
        with open(batch_path, "w", encoding="utf-8") as f:
            json.dump(batch, f, indent=2, ensure_ascii=False)
            f.write("\n")
        total_count = sum(fam["total_count"] for fam in batch)
        multi_count = sum(1 for fam in batch if len(fam["children"]) > 0)
        print(f"  batch_{i:02d}.json: {len(batch):,} families "
              f"({multi_count:,} multi-member), total count={total_count:,}")

    # ---------------------------------------------------------------------------
    # Summary
    # ---------------------------------------------------------------------------
    print("\n" + "=" * 60)
    print("SUMMARY")
    print("=" * 60)
    print(f"  Input entries:              {len(entries):>8,}")
    print(f"  Flagged invalid:            {total_flagged:>8,}")
    print(f"  Matched existing crosswalk: {len(crosswalk_matches):>8,}")
    print(f"  New families:               {len(families):>8,}")
    print(f"    Multi-member:             {len(multi_families):>8,}")
    print(f"    Singletons:               {len(singletons):>8,}")
    print(f"  ---")
    accounted = total_flagged + len(crosswalk_matches) + sum(
        1 + len(f["children"]) for f in families
    )
    print(f"  Total accounted for:        {accounted:>8,}")
    if accounted != len(entries):
        print(f"  WARNING: mismatch! Input was {len(entries):,}")
    else:
        print(f"  Verified: all entries accounted for")

    print(f"\nOutput files in {OUTPUT_DIR}/:")
    print(f"  crosswalk_matches.json")
    print(f"  families_for_crosswalk.json")
    print(f"  flagged_invalid.json")
    for i in range(NUM_BATCHES):
        print(f"  batch_{i:02d}.json")


if __name__ == "__main__":
    main()
