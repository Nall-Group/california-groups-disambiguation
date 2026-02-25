# Claude Notes

## Goal

We are building and cleaning a map of organization names so we can "crosswalk" how different organization names relate to each other. For each organization there is a canonical name, and other organization names may be alternate spellings or chapters of an organization. Basically, it's a forest of tree structures. The trees can be nested to any depth (e.g. you can have an alternate spelling of a chapter)

Note that you shouldn't remove entries unless they are exact duplicates since we need to be able to categorize all organizations that appear in the dataset, even if they are spelled wrong. It's ok to move organizations around like in or out of the crosswalk.

## Handling Invalid Entries

When an entry in the crosswalk turns out to not be a real organization, two things need to happen:

1. **Remove it from the crosswalk JSON** (`2_webapp/org_clusters_crosswalk.json`)
2. **Move its CSV row** (including the count) from whichever source file it's in (`org_name_subsets_for_cleaning/org_names_in_crosswalk.csv` or `org_name_subsets_for_cleaning/org_names_not_in_crosswalk.csv`) to the appropriate invalidity file in `org_name_subsets_for_cleaning/`:

| File | What goes here |
|------|---------------|
| `org_names_that_are_actually_individuals.csv` | People's names (e.g. "Attorney General Rob Bonta") |
| `org_names_partial.csv` | Incomplete/fragment names (e.g. "LOS", "SAN") |
| `org_names_conjoined.csv` | Multiple orgs joined together (e.g. "Sierra Club Planning and Conservation League") |
| `org_names_invalid.csv` | Not organizations at all (e.g. legislative bills, procedural text like "GOVERNOR'S VETO MESSAGE") |
| `org_names_not_capitalized.csv` | Improper capitalization |
| `org_names_embedded_in_narrative_text.csv` | Org names buried in longer prose |
| `org_names_that_start_with_parens.csv` | Names starting with parentheses |
| `org_names_that_are_dates_or_phone_numbers.csv` | Dates or phone numbers |

All CSVs use the same format: `org_name,count` — move the entire row including the count.

If the entry only exists in the JSON and not in any CSV file, just remove it from the JSON.

## Crosswalk Data

- `crosswalk.standardizenames.manualedits_clean.csv` - original source, DO NOT EDIT
- `2_webapp/org_clusters_crosswalk.json` - live file, all updates go here
- For the CSV file some of the items in the file contain commas and you need to parse it properly. Don't just parse it by comma use CSV parsing libraries 
