#!/usr/bin/env python3
"""
Move misplaced entries from org_names_not_in_crosswalk.csv to appropriate category files.
Also adds missing organizations to the crosswalk JSON.

Categories:
- conjoined: multiple orgs joined together
- partial: incomplete/fragment names
- invalid: not organizations at all
- individuals: entries starting with "Numerous "
"""

import csv
import json
import os

BASE_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SUBSETS_DIR = os.path.join(BASE_DIR, "org_name_subsets_for_cleaning")
CROSSWALK_PATH = os.path.join(BASE_DIR, "2_webapp", "org_clusters_crosswalk.json")

# Source file
NOT_IN_CROSSWALK = os.path.join(SUBSETS_DIR, "org_names_not_in_crosswalk.csv")

# Destination files
CONJOINED = os.path.join(SUBSETS_DIR, "org_names_conjoined.csv")
PARTIAL = os.path.join(SUBSETS_DIR, "org_names_partial.csv")
INVALID = os.path.join(SUBSETS_DIR, "org_names_invalid.csv")
INDIVIDUALS = os.path.join(SUBSETS_DIR, "org_names_that_are_actually_individuals.csv")


def read_csv(filepath):
    """Read CSV file, return list of (org_name, count) tuples."""
    rows = []
    with open(filepath, "r", encoding="utf-8", newline="") as f:
        reader = csv.reader(f)
        next(reader)  # Skip header
        for row in reader:
            if not row:
                continue
            org_name = row[0]
            count = int(row[1])
            rows.append((org_name, count))
    return rows


def write_csv(filepath, rows):
    """Write CSV file with header."""
    with open(filepath, "w", encoding="utf-8", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(["org_name", "count"])
        for name, count in rows:
            writer.writerow([name, count])


# ============================================================
# Category definitions
# ============================================================

# --- Conjoined: explicit org_name matches ---
CONJOINED_EXACT = {
    "CIGNA and UnitedHealthcare",
    "IBM and IBM Credit Corporation",
    "CHP and MADD",
    "ACLU and California Public Defenders",
    "WHO and UNICEF",
    "Giffords Law Center to Prevent Gun Violence and National Network Of Hospital-Based Violence Intervention Programs (NNHVIP)",
    "Center for Public Interest Law/children's Advocacy Institute/university of San Diego",
    "Embarcadero Coalition of San Diego Environmental Center of San Diego Planning and Conservation League",
    # Multi-chapter ACLU listings
    "ACLU of /Northern California/Southern California/San Diego & Imperial Counties",
    "ACLU of Northern California, Southern California, and San Diego and Imperial Counties",
    "ACLU Members for LPS Reform - Los Angeles Alliance for the Mentally Ill - Coachella Valley",
    # Giffords + another org
    "Giffords Law Center, Youth ALIVE",
}

# Conjoined: prefix matches
CONJOINED_PREFIXES = [
    "Chambers of Commerce:",
]

# --- Partial: explicit org_name matches ---
PARTIAL_EXACT = {
    # Geographic fragments
    "Los", "Sacramento", "Orange", "Santa", "La", "El",
    # Org type words alone
    "Association", "District", "Services", "Industry", "Concerns",
    "Bureau", "Peace", "Crime", "Region", "Health", "Section",
    "Officials", "Division", "Public", "Insurance", "Court", "Engine", "School",
    # Other fragments
    "County of", "Assoc", "Doris", "Chapters",
    # Common English words
    "care", "home", "laws", "vote", "note", "also", "as", "b", "only",
    "this", "who", "h", "all", "pe", "that",
    # Numbers
    "2", "4.", "3", "6", "8", "9",
    # Short abbreviations
    "BW", "MM", "DK", "DG", "HR", "JC", "RT", "FL", "LG",
}

# --- Invalid: explicit org_name matches ---
INVALID_EXACT = {
    # Legislative/procedural
    "END --", "Amended", "Oppose", "Version", "Neutral",
    # Placeholders
    "None Registered", "None Submitted", "No Known", "Not Known", "Unknown",
    # Legislative reference
    "this bill",
}

# Invalid: prefix matches
INVALID_PREFIXES = [
    "FISCAL COMMITTEE:",
]

# --- Individuals: prefix match ---
INDIVIDUALS_PREFIX = "Numerous "


def categorize_entry(org_name):
    """Determine what category an entry belongs to.

    Returns: 'conjoined', 'partial', 'invalid', 'individuals', or None.
    Checks are ordered so more specific matches win over broader ones.
    """
    # Check individuals first (Numerous prefix)
    if org_name.startswith(INDIVIDUALS_PREFIX):
        return "individuals"

    # Check invalid exact matches
    if org_name in INVALID_EXACT:
        return "invalid"

    # Check invalid prefixes
    for prefix in INVALID_PREFIXES:
        if org_name.startswith(prefix):
            return "invalid"

    # Check conjoined exact matches
    if org_name in CONJOINED_EXACT:
        return "conjoined"

    # Check conjoined prefixes
    for prefix in CONJOINED_PREFIXES:
        if org_name.startswith(prefix):
            return "conjoined"

    # Check partial exact matches (last, since these are broad single-word matches)
    if org_name in PARTIAL_EXACT:
        return "partial"

    return None


def add_crosswalk_entries(crosswalk):
    """Add missing organizations to the crosswalk."""
    clusters = crosswalk["clusters"]

    # Build lookup of all names (canonical + children at all depths)
    def collect_names(node, names_set):
        if "canonical" in node:
            names_set.add(node["canonical"].upper())
        if "name" in node:
            names_set.add(node["name"].upper())
        for child in node.get("children", []):
            collect_names(child, names_set)

    all_names = set()
    for cluster in clusters:
        collect_names(cluster, all_names)

    # New canonical entries to add
    new_canonicals = [
        "UnitedHealthcare",
        "World Health Organization",
        "UNICEF",
        "Giffords Law Center to Prevent Gun Violence",
        "National Network of Hospital-Based Violence Intervention Programs",
        "University of San Diego",
        "Embarcadero Coalition of San Diego",
        "Environmental Center of San Diego",
    ]

    added_canonicals = []
    for name in new_canonicals:
        if name.upper() not in all_names:
            entry = {
                "canonical": name,
                "status": "active",
                "children": [],
            }
            # Add alternate spellings where appropriate
            if name == "World Health Organization":
                entry["children"].append(
                    {"name": "WHO", "relationship": "alternate_spelling"}
                )
            elif name == "National Network of Hospital-Based Violence Intervention Programs":
                entry["children"].append(
                    {"name": "NNHVIP", "relationship": "alternate_spelling"}
                )
            clusters.append(entry)
            added_canonicals.append(name)

    # New children to add to existing clusters
    children_to_add = {
        "IBM": [
            {"name": "IBM Credit Corporation", "relationship": "chapter"},
        ],
        "ACLU CALIFORNIA": [
            {"name": "ACLU of Northern California", "relationship": "chapter"},
            {"name": "ACLU of Southern California", "relationship": "chapter"},
            {
                "name": "ACLU of San Diego and Imperial Counties",
                "relationship": "chapter",
            },
        ],
    }

    added_children = []
    for parent_canonical, new_children in children_to_add.items():
        for cluster in clusters:
            if cluster["canonical"].upper() == parent_canonical.upper():
                if "children" not in cluster:
                    cluster["children"] = []
                for child in new_children:
                    if child["name"].upper() not in all_names:
                        cluster["children"].append(child)
                        added_children.append(
                            f"{child['name']} -> {parent_canonical}"
                        )
                break

    return added_canonicals, added_children


def main():
    # Read source file
    print(f"Reading {NOT_IN_CROSSWALK}...")
    source_rows = read_csv(NOT_IN_CROSSWALK)
    source_count = len(source_rows)
    print(f"  Source entries: {source_count}")

    # Read destination files
    print("Reading destination files...")
    conjoined_rows = read_csv(CONJOINED)
    partial_rows = read_csv(PARTIAL)
    invalid_rows = read_csv(INVALID)
    individuals_rows = read_csv(INDIVIDUALS)

    conjoined_before = len(conjoined_rows)
    partial_before = len(partial_rows)
    invalid_before = len(invalid_rows)
    individuals_before = len(individuals_rows)

    print(f"  Conjoined: {conjoined_before}")
    print(f"  Partial: {partial_before}")
    print(f"  Invalid: {invalid_before}")
    print(f"  Individuals: {individuals_before}")

    # Categorize entries
    remaining = []
    moved = {
        "conjoined": [],
        "partial": [],
        "invalid": [],
        "individuals": [],
    }

    for org_name, count in source_rows:
        category = categorize_entry(org_name)
        if category:
            moved[category].append((org_name, count))
        else:
            remaining.append((org_name, count))

    # Print what's being moved
    print("\n--- Entries to move ---")
    for cat, entries in moved.items():
        print(f"\n  {cat}: {len(entries)} entries")
        for name, count in sorted(entries, key=lambda x: -x[1])[:15]:
            print(f"    {name},{count}")
        if len(entries) > 15:
            print(f"    ... and {len(entries) - 15} more")

    # Append to destination files
    conjoined_rows.extend(moved["conjoined"])
    partial_rows.extend(moved["partial"])
    invalid_rows.extend(moved["invalid"])
    individuals_rows.extend(moved["individuals"])

    # Write all files
    print("\n--- Writing files ---")
    write_csv(NOT_IN_CROSSWALK, remaining)
    print(f"  Not in crosswalk: {source_count} -> {len(remaining)}")

    write_csv(CONJOINED, conjoined_rows)
    print(f"  Conjoined: {conjoined_before} + {len(moved['conjoined'])} = {len(conjoined_rows)}")

    write_csv(PARTIAL, partial_rows)
    print(f"  Partial: {partial_before} + {len(moved['partial'])} = {len(partial_rows)}")

    write_csv(INVALID, invalid_rows)
    print(f"  Invalid: {invalid_before} + {len(moved['invalid'])} = {len(invalid_rows)}")

    write_csv(INDIVIDUALS, individuals_rows)
    print(f"  Individuals: {individuals_before} + {len(moved['individuals'])} = {len(individuals_rows)}")

    # Verify counts
    total_moved = sum(len(entries) for entries in moved.values())
    removed_from_source = source_count - len(remaining)
    print(f"\n--- Verification ---")
    print(f"  Removed from source: {removed_from_source}")
    print(f"  Added to destinations: {total_moved}")
    assert removed_from_source == total_moved, (
        f"Count mismatch! Removed {removed_from_source} but added {total_moved}"
    )
    print(f"  Counts match!")

    # Update crosswalk
    print("\n--- Updating crosswalk ---")
    with open(CROSSWALK_PATH, "r", encoding="utf-8") as f:
        crosswalk = json.load(f)

    added_canonicals, added_children = add_crosswalk_entries(crosswalk)

    with open(CROSSWALK_PATH, "w", encoding="utf-8") as f:
        json.dump(crosswalk, f, indent=2, ensure_ascii=False)
        f.write("\n")

    print(f"  New canonical entries: {len(added_canonicals)}")
    for name in added_canonicals:
        print(f"    + {name}")

    print(f"  New children: {len(added_children)}")
    for desc in added_children:
        print(f"    + {desc}")

    print("\nDone! Run scripts/regenerate_org_subsets.py to sync crosswalk with CSVs.")


if __name__ == "__main__":
    main()
