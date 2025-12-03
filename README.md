# California Groups Disambiguation

## 1_gather_orgs

Here we have gathered a canonical list of groups in California that support or oppose legislation related to housing

## 2_webapp

We also provide a disambiguation tool hosted on [Ruth todo - fill in github web app link once it's ready]

Future feature: add fuzzy search feature to disambiguation tool

## data cleaning

directory: org_name_subsets_for_cleaning/

0. Use extract_org_names.py to get all org names from leginfo_metadata.csv in https://github.com/Nall-Group/leginfo

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

