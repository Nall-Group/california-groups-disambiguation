#!/usr/bin/env python3
"""
Migration script: Convert crosswalk CSV to org_clusters.json
Run this script once to generate the initial JSON file.
"""

import csv
import json
from collections import defaultdict

# Read the crosswalk CSV
crosswalk = defaultdict(list)

with open('crosswalk.standardizenames.manualedits_clean.csv', 'r', encoding='utf-8') as f:
    reader = csv.DictReader(f)
    for row in reader:
        original = row['originalname']
        edited = row['editedname']
        crosswalk[edited].append(original)

# Build clusters - one per canonical name
clusters = []

for canonical, originals in crosswalk.items():
    # Filter out the canonical name itself (if it appears as an alias)
    aliases = [name for name in originals if name != canonical]

    # Create children list - all are alternate_spelling initially
    children = [
        {"name": alias, "relationship": "alternate_spelling"}
        for alias in aliases
    ]

    clusters.append({
        "canonical": canonical,
        "status": "active",
        "children": children
    })

# Sort clusters by canonical name for easier browsing
clusters.sort(key=lambda x: x['canonical'])

# Create the final output structure
output = {
    "version": "1.0",
    "clusters": clusters
}

# Write to JSON file
with open('2_webapp/org_clusters.json', 'w', encoding='utf-8') as f:
    json.dump(output, f, indent=2, ensure_ascii=False)

print(f"Migration complete!")
print(f"Created: {len(clusters)} clusters")
print(f"Output file: 2_webapp/org_clusters.json")
