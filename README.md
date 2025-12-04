# California Groups Disambiguation

![Canonical Orgs](https://img.shields.io/badge/dynamic/json?url=https%3A%2F%2Fraw.githubusercontent.com%2FNall-Group%2Fcalifornia-groups-disambiguation%2Fmain%2Fstats.json&query=%24.canonical_orgs&label=canonical%20orgs&color=blue)
![Total in Crosswalk](https://img.shields.io/badge/dynamic/json?url=https%3A%2F%2Fraw.githubusercontent.com%2FNall-Group%2Fcalifornia-groups-disambiguation%2Fmain%2Fstats.json&query=%24.total_in_crosswalk&label=total%20in%20crosswalk&color=green)
![Not in Crosswalk](https://img.shields.io/badge/dynamic/json?url=https%3A%2F%2Fraw.githubusercontent.com%2FNall-Group%2Fcalifornia-groups-disambiguation%2Fmain%2Fstats.json&query=%24.not_in_crosswalk&label=not%20in%20crosswalk&color=orange)
![Invalid Entries](https://img.shields.io/badge/dynamic/json?url=https%3A%2F%2Fraw.githubusercontent.com%2FNall-Group%2Fcalifornia-groups-disambiguation%2Fmain%2Fstats.json&query=%24.invalid_entries&label=invalid%20entries&color=red)

## 1_gather_orgs

Here we have gathered a canonical list of groups in California that support or oppose legislation related to housing

## 2_webapp

We also provide a disambiguation tool hosted on [Ruth todo - fill in github web app link once it's ready]

Future feature: add fuzzy search feature to disambiguation tool

## data cleaning

directory: org_name_subsets_for_cleaning/

0. Use extract_org_names.py to get all org names from leginfo_metadata.csv in https://github.com/Nall-Group/leginfo. Clean these by removing stuff matching the regex in cleaning_patterns.txt.

1. Remove all organization names that do not follow the rule where all words above four characters long are capitalized. These are likely text fragments. These organizations are moved into org_names_not_capitalized.csv. Make sure to exclude organizations where the item that is longer than four characters and not capitalized is after some punctuation like a hyphen or inside parentheses at the end of the organization name.

2. Remove all organization names that are dates or phone numbers to org_names_that_are_dates_or_phone_numbers.csv

3. Remove all organization names that start with parentheses to org_names_that_start_with_parens.csv

4. Take all organizations that start with the word "the". Check if there is an organization entry that is the same except without the word "the". If so, check if the organization is already in the crosswalk. If the organization is not already in the crosswalk, add it to the crosswalk 

5. Remove all organizations that start with lower case "to" and "with" to org_names_partial.csv

6. Remove all date and positions in parentheses such as (3/19/98) or (Oppose) or (Co-Sponsor) from the end of org names in org_names_capitalized.csv

7. Add all org names that Looks like an org name that is in the crosswalk except it has its own acronym in brackets at the end to the crosswalk as alternate spelling

8. Separate org_names_capitalized.csv into org_names_in_crosswalk.csv and org_names_not_in_crosswalk.csv so we can work on getting them all into the crosswalk

9. Add orgs that have "Inc" at the end of the name where the same org without "Inc" also exists in the dataset, to the crosswalk

10. Add orgs that are the same as another org except for the addition of ' California" or ' of California' or ' of CA' or ' CA' at teh end as orgs with a chapter relationship to the base name

11. Add orgs that are unambiguous acronyms as an alternate spelling

12. Add orgs that start with California but otherwise the same as another org as a chapter

13. Make sure that orgs that look like valid names except for stuff in brackets are moved from org_names_not_capitalized.csv to org_names_not_in_crosswalk.csv

14. Remove all orgs that start with digits and then a closing square bracket, e.g. '2] 2015-16 Budget: "Capital Outlay Program Review". LAO, May 14, 2014' because these are all citations

15. Add all orgs that have "Local" in the MIDDLE of the name where it's another org name, Local, and then a number. These orgs are chapters of the prefix org.

16. Add orgs that start with Local, a number, and then another org name -- these are also chapters.

17. Consolidate Fire Fighter organizations as children of International Association of Fire Fighters. Under this is California Professional Firefighters which is a chapter. Under that are the city/county chapters in format city name followed by Fire Fighters, and any other version of this is an alternate spelling.

18. Move all organizations that end in the word "individuals" along with one individual and Mayor to org_names_that_are_actually_individuals.csv

XX. Move things that are obviously not organization names (like sponsor or SB) to invalid organization names list 


## Crosswalk Relationship Types

- canonical
- alternate_spelling
- chapter

## Cleaning Patterns

`cleaning_patterns.txt` contains regex patterns that should be removed from the end of organization names. These are metadata about bill relationships (e.g., sponsor status), not part of the actual org name.

Patterns include:
- (CO-SPONSOR), (SPONSOR), (Sponsors)
- (Co-Source)
- (PRIOR VERSION)
- (OPPOSE), (OPPOSE UNLESS AMENDED)
- ("In Concept")
- (Principal Co-Sponsor), (Co-Sponsors)

## Invalid Organization Names

`org_names_invalid.csv` contains placeholder values and metadata fragments that are not actual organization names. These include:
- "None on file", "NONE RECEIVED", "None Known", "None to date", etc.
- "END", "Inc", "Oppose", "Unknown"
- Empty strings

