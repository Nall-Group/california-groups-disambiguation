#!/usr/bin/env python3
"""
Generate stats JSON for README badges.
Run this after making changes to the crosswalk or org name files.
"""

import json
import csv

def generate_stats():
    stats = {}

    # 1. Canonical org names (clusters in crosswalk)
    with open('2_webapp/org_clusters_crosswalk.json', 'r') as f:
        data = json.load(f)
        stats['canonical_orgs'] = len(data['clusters'])

        # Total names in crosswalk (canonicals + all nested children)
        def count_children(children):
            n = len(children)
            for child in children:
                n += count_children(child.get('children', []))
            return n

        total = len(data['clusters'])
        for cluster in data['clusters']:
            total += count_children(cluster.get('children', []))
        stats['total_in_crosswalk'] = total

    # 2. Org names not in crosswalk
    with open('org_names_for_cleaning/org_names_not_in_crosswalk.csv', 'r') as f:
        reader = csv.reader(f)
        next(reader)  # skip header
        stats['not_in_crosswalk'] = sum(1 for row in reader if row)

    # 3. Invalid org names (sum of all invalid files)
    invalid_files = [
        'org_names_invalid.csv',
        'org_names_partial.csv',
        'org_names_that_are_actually_individuals.csv',
        # conjoined strings now live in the mapping (retired the flat
        # org_names_conjoined.csv on 2026-07-16); count its rows here so the
        # non-org total stays consistent.
        'conjoined_text_mapping_to_orgs.csv',
    ]

    invalid_count = 0
    for filename in invalid_files:
        try:
            with open(f'org_names_for_cleaning/{filename}', 'r') as f:
                reader = csv.reader(f)
                next(reader)  # skip header
                count = sum(1 for row in reader if row)
                invalid_count += count
        except FileNotFoundError:
            pass

    stats['invalid_entries'] = invalid_count

    # Write JSON
    with open('stats.json', 'w') as f:
        json.dump(stats, f, indent=2)

    print(f"Stats updated in stats.json:")
    print(f"  Canonical orgs: {stats['canonical_orgs']:,}")
    print(f"  Total in crosswalk: {stats['total_in_crosswalk']:,}")
    print(f"  Not in crosswalk: {stats['not_in_crosswalk']:,}")
    print(f"  Invalid entries: {stats['invalid_entries']:,}")

    return stats

if __name__ == "__main__":
    generate_stats()
