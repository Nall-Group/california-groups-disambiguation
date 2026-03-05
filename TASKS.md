# Tasks

## Write Queue

Only the RA at the top of this list has write access. Add yourself to the bottom when you're ready to write. Remove yourself when done.

(empty)

## Task List

| # | Task | Status | Assignee | Notes |
|---|------|--------|----------|-------|
| 1 | Consolidate California Democratic Party delegate entries in the crosswalk JSON. There are entries like "550 Delegates of the California Democratic Party" and "California Democratic Party Delegates (97)" that should be consolidated under a single canonical entry (likely "California Democratic Party" or similar). Search for all related entries and organize them properly. | Done | RA-Alpha | Consolidated "550 Delegates of the California Democratic Party", "California Democratic Party Delegates (97)", and "California State Democratic Party" as alternate_spelling children of "CALIFORNIA DEMOCRATIC PARTY". |
| 2 | Add a pattern to the cleaning script to strip the "None on file." prefix from org names (e.g., "None on file.CA Professional Firefighters" -> "CA Professional Firefighters"). There are 30+ such entries. After adding the pattern, run the cleaning and deduplication. | Done | RA-Beta | Cleaned 84 names, merged 59 clusters. Also removed 2 invalid entries ("There is none on file", "Oppose None on file as of March 19, 2010"). |
| 3 | Add a pattern to the cleaning script to handle "COMMENTS :" / "ARGUMENTS IN SUPPORT:" entries (~70 total). These contain real org names embedded in analysis text. Look at examples in the crosswalk JSON to understand the pattern, then add cleaning logic and run the cleaning and deduplication pipeline. | Done | RA-Beta | Cleaned 78 names (62 COMMENTS/ARGUMENTS + 16 According to), merged 16 clusters. Also handled "Staff Comments:" prefix. |
| 4 | Research number + union entries like "20, AFL-CIO", "535, SEIU", "63, Marine Clerks" in the crosswalk JSON. Search the web to determine if these are specific union locals (e.g., "Local 20, AFL-CIO"). If so, group them under the correct canonical union entry in the crosswalk. | Done | RA-Alpha | Consolidated 12 entries under parent unions. "29 AFL-CIO & CLC"/"29 AFL-CIO and CLC" left as-is (identity unclear, question posted in QUESTIONS.md). |
| 5 | Handle conjoined/list org entries prefixed with "Opponents" that contain multiple orgs in one string (e.g., "Opponents (CalChamber, Associated of General Contractors, California Manufacturers & Technology Association, the California)"). Get a comprehensive list from the crosswalk JSON. For each: (1) parse out the individual org names, (2) check if each individual org already exists in the crosswalk, (3) add it as canonical/alt spelling/chapter if not, (4) move the conjoined entry from the crosswalk to `org_names_conjoined.csv`. | Done | RA-Beta | Removed 6 "Opponents" entries. 5 moved to conjoined CSV, 1 ("Opponents of this bill") to invalid. Added "California Deposition Agency" as alt spelling of CDRA. |
| 6 | Same as Task 5 but for "Support"/"Supporters" prefixed conjoined entries instead of "Opponents". Search the crosswalk for entries like "Supporters (OrgA, OrgB, ...)" or "Support (OrgA, OrgB, ...)". For each: (1) parse out the individual org names, (2) check if each already exists in the crosswalk, (3) add as canonical/alt spelling/chapter if not, (4) move the conjoined entry to `org_names_conjoined.csv`. | In Progress | RA-Alpha | |
