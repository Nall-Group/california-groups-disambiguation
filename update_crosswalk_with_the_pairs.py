#!/usr/bin/env python3
"""
Update the crosswalk CSV to add mappings for THE/non-THE pairs.
Maps "THE X" -> "X" (non-THE version is canonical).
All names are converted to uppercase to match crosswalk format.
"""

import csv

def update_crosswalk():
    pairs_file = "the_pairs.csv"
    crosswalk_file = "crosswalk.standardizenames.manualedits_clean.csv"

    # Load existing crosswalk
    existing_mappings = {}  # originalname -> editedname
    with open(crosswalk_file, 'r', encoding='utf-8') as f:
        reader = csv.DictReader(f)
        for row in reader:
            existing_mappings[row['originalname']] = row['editedname']

    print(f"Loaded {len(existing_mappings)} existing mappings from crosswalk")

    # Load the pairs
    pairs = []
    with open(pairs_file, 'r', encoding='utf-8') as f:
        reader = csv.DictReader(f)
        for row in reader:
            pairs.append(row)

    print(f"Loaded {len(pairs)} THE/non-THE pairs")

    # Build set of new mappings to add (uppercase)
    new_mappings = {}  # THE_VERSION_UPPER -> NON_THE_VERSION_UPPER
    already_exists = 0
    already_mapped_differently = 0

    for pair in pairs:
        the_upper = pair['the_version'].upper()
        non_the_upper = pair['non_the_version'].upper()

        if the_upper in existing_mappings:
            if existing_mappings[the_upper] == non_the_upper:
                already_exists += 1
            else:
                already_mapped_differently += 1
                print(f"  Warning: {the_upper} already maps to {existing_mappings[the_upper]}, not {non_the_upper}")
        else:
            new_mappings[the_upper] = non_the_upper

    print(f"\nResults:")
    print(f"  Already correctly mapped: {already_exists}")
    print(f"  Mapped to different target: {already_mapped_differently}")
    print(f"  New mappings to add: {len(new_mappings)}")

    # Append new mappings to crosswalk
    with open(crosswalk_file, 'a', encoding='utf-8', newline='') as f:
        writer = csv.writer(f)
        for the_name, non_the_name in sorted(new_mappings.items()):
            writer.writerow([the_name, non_the_name])

    print(f"\nAppended {len(new_mappings)} new mappings to {crosswalk_file}")

    # Show some examples
    print("\nSample new mappings added:")
    for i, (the_name, non_the_name) in enumerate(sorted(new_mappings.items())):
        if i >= 10:
            break
        print(f"  {the_name} -> {non_the_name}")

if __name__ == "__main__":
    update_crosswalk()
