#!/usr/bin/env python3
"""
Find organization name pairs where one starts with "THE " and the other doesn't.
Only includes pairs where BOTH versions exist in the data.
"""

import csv

def find_the_pairs():
    input_file = "org_name_subsets_for_cleaning/org_names_capitalized.csv"
    output_file = "the_pairs.csv"

    # Load all org names into dicts for lookup
    org_names = {}  # name -> count
    org_names_upper = {}  # UPPER name -> original name

    with open(input_file, 'r', encoding='utf-8') as f:
        reader = csv.DictReader(f)
        for row in reader:
            name = row['org_name']
            count = int(row['count'])
            org_names[name] = count
            org_names_upper[name.upper()] = name

    print(f"Loaded {len(org_names)} organization names")

    # Find pairs where "The X" and "X" both exist (case-insensitive)
    pairs = []
    the_names = [name for name in org_names.keys() if name.upper().startswith("THE ")]
    print(f"Found {len(the_names)} names starting with 'The ' (case-insensitive)")

    for the_name in the_names:
        # Get the part after "The " (case-insensitive)
        non_the_upper = the_name.upper()[4:]  # Remove "THE " prefix
        if non_the_upper in org_names_upper:
            non_the_name = org_names_upper[non_the_upper]
            pairs.append({
                'the_version': the_name,
                'non_the_version': non_the_name,
                'the_count': org_names[the_name],
                'non_the_count': org_names[non_the_name]
            })

    print(f"Found {len(pairs)} pairs where both versions exist")

    # Sort by combined count (most common pairs first)
    pairs.sort(key=lambda x: x['the_count'] + x['non_the_count'], reverse=True)

    # Write output
    with open(output_file, 'w', encoding='utf-8', newline='') as f:
        writer = csv.DictWriter(f, fieldnames=['the_version', 'non_the_version', 'the_count', 'non_the_count'])
        writer.writeheader()
        writer.writerows(pairs)

    print(f"Wrote {len(pairs)} pairs to {output_file}")

    # Show top 10 examples
    print("\nTop 10 pairs by combined count:")
    for pair in pairs[:10]:
        print(f"  {pair['the_version']} ({pair['the_count']}) <-> {pair['non_the_version']} ({pair['non_the_count']})")

if __name__ == "__main__":
    find_the_pairs()
