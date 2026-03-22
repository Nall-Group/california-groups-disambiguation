#!/usr/bin/env python3
"""Find all canonicals that are acronym-only.

Criteria:
- 3+ consecutive capital letters
- Total name length ≤ 15 characters
- All uppercase (no lowercase letters)

Output: list grouped by first character, written to acronym_canonicals.txt
"""

import json
import re
from collections import defaultdict

def is_acronym_only(name):
    """Check if a canonical name is acronym-only.

    An acronym-only name is something like "AARP", "IBEW", "AAAJ-CA".
    NOT things like "A BETTER WAY" or "ABODE SERVICES" (those are
    all-caps org names, not acronyms).
    """
    # Must be ≤ 15 characters
    if len(name) > 15:
        return False
    # Must have no lowercase letters
    if any(c.islower() for c in name):
        return False
    # Must have 3+ consecutive capital letters
    if not re.search(r'[A-Z]{3,}', name):
        return False
    # Split by spaces — if any word is > 4 chars, it's likely a regular
    # word (not an acronym), so reject
    words = name.split()
    for word in words:
        # Strip non-alpha for length check
        alpha_only = re.sub(r'[^A-Z]', '', word)
        if len(alpha_only) > 4 and len(words) > 1:
            return False
    return True

def main():
    with open('2_webapp/org_clusters_crosswalk.json', 'r') as f:
        data = json.load(f)

    clusters = data.get('clusters', data) if isinstance(data, dict) else data

    acronyms = defaultdict(list)
    for cluster in clusters:
        canonical = cluster['canonical']
        if is_acronym_only(canonical):
            first_char = canonical[0] if canonical[0].isalpha() else '#'
            acronyms[first_char].append(canonical)

    # Sort within each group
    for key in acronyms:
        acronyms[key].sort()

    # Write output
    total = sum(len(v) for v in acronyms.values())
    with open('acronym_canonicals.txt', 'w') as f:
        f.write(f"Acronym-only canonicals: {total} total\n")
        f.write("=" * 60 + "\n\n")
        for key in sorted(acronyms.keys()):
            entries = acronyms[key]
            f.write(f"## {key} ({len(entries)} entries)\n")
            for entry in entries:
                f.write(f"  {entry}\n")
            f.write("\n")

    print(f"Found {total} acronym-only canonicals")
    print(f"Output written to acronym_canonicals.txt")

    # Print summary by group
    for key in sorted(acronyms.keys()):
        print(f"  {key}: {len(acronyms[key])}")

if __name__ == '__main__':
    main()
