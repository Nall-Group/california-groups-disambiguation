# Claude Notes

## Goal

We are building and cleaning a map of organization names so we can "crosswalk" how different organization names relate to each other. For each organization there is a canonical name, and other organization names may be alternate spellings or chapters of an organization. Basically, it's a forest of tree structures. The trees can be nested to any depth (e.g. you can have an alternate spelling of a chapter)

## Crosswalk Data

- `crosswalk.standardizenames.manualedits_clean.csv` - original source, DO NOT EDIT
- `2_webapp/org_clusters_crosswalk.json` - live file, all updates go here
- For the CSV file some of the items in the file contain commas and you need to parse it properly. Don't just parse it by comma use CSV parsing libraries 
