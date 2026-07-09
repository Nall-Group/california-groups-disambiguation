#!/usr/bin/env python3
"""
Clean one org name (or a ';'-separated list) with the crosswalk cleaning patterns.

Usage:
    python3 scripts/clean_name.py "Sierra Club (co-sponsor)"
    python3 scripts/clean_name.py "Org A (sponsor); Org B (previous version)"

Prints the cleaned name(s), one per line (empty results dropped). Loads only
`cleaning_patterns.txt` — NOT the crosswalk — so it returns instantly.
"""

import sys
from pathlib import Path

# Allow importing the shared helpers from the repo root.
sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
from org_matching_utils import load_cleaning_patterns, clean_org_name


def main():
    if len(sys.argv) < 2:
        print('usage: clean_name.py "<org name or ;-separated list>"', file=sys.stderr)
        sys.exit(1)

    patterns = load_cleaning_patterns()
    raw = sys.argv[1]
    for part in raw.split(";"):
        part = part.strip()
        if not part:
            continue
        cleaned = clean_org_name(part, patterns)
        if cleaned:
            print(cleaned)


if __name__ == "__main__":
    main()
