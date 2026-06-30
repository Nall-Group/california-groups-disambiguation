# California Groups Disambiguation

![Canonical Orgs](https://img.shields.io/badge/dynamic/json?url=https%3A%2F%2Fraw.githubusercontent.com%2FNall-Group%2Fcalifornia-groups-disambiguation%2Fmain%2Fstats.json&query=%24.canonical_orgs&label=canonical%20orgs&color=blue&cacheSeconds=0)
![Total in Crosswalk](https://img.shields.io/badge/dynamic/json?url=https%3A%2F%2Fraw.githubusercontent.com%2FNall-Group%2Fcalifornia-groups-disambiguation%2Fmain%2Fstats.json&query=%24.total_in_crosswalk&label=total%20in%20crosswalk&color=green&cacheSeconds=0)
![Not in Crosswalk](https://img.shields.io/badge/dynamic/json?url=https%3A%2F%2Fraw.githubusercontent.com%2FNall-Group%2Fcalifornia-groups-disambiguation%2Fmain%2Fstats.json&query=%24.not_in_crosswalk&label=not%20in%20crosswalk&color=orange&cacheSeconds=0)
![Invalid Entries](https://img.shields.io/badge/dynamic/json?url=https%3A%2F%2Fraw.githubusercontent.com%2FNall-Group%2Fcalifornia-groups-disambiguation%2Fmain%2Fstats.json&query=%24.invalid_entries&label=invalid%20entries&color=red&cacheSeconds=0)

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

9. Add orgs that have "Inc" at the end of the name where the same org without "Inc" also exists in the dataset, to the crosswalk. Do the same for Corp and LLC

10. Add orgs that are the same as another org except for the addition of ' California" or ' of California' or ' of CA' or ' CA' at teh end as orgs with a chapter relationship to the base name

11. Add orgs that are unambiguous acronyms as an alternate spelling

12. Add orgs that start with California but otherwise the same as another org as a chapter

13. Make sure that orgs that look like valid names except for stuff in brackets are moved from org_names_not_capitalized.csv to org_names_not_in_crosswalk.csv

14. Remove all orgs that start with digits and then a closing square bracket, e.g. '2] 2015-16 Budget: "Capital Outlay Program Review". LAO, May 14, 2014' because these are all citations

15. Add all orgs that have "Local" in the MIDDLE of the name where it's another org name, Local, and then a number. These orgs are chapters of the prefix org.

16. Add orgs that start with Local, a number, and then another org name -- these are also chapters.

17. Consolidate Fire Fighter organizations as children of International Association of Fire Fighters. Under this is California Professional Firefighters which is a chapter. Under that are the city/county chapters in format city name followed by Fire Fighters, and any other version of this is an alternate spelling. Add fire fighter orgs to crosswalk that aren't already there.

19. Move all organizations that end in the word "individuals" along with one individual and Mayor to org_names_that_are_actually_individuals.csv

19. Merge all case insensitive duplicates

20. Manually audit Sherriff/Police caninicals to merge to just these:
Alameda County Deputy Sheriffs' Activities League
CALIFORNIA COALITION FOR SHERIFF OVERSIGHT
CALIFORNIA POLICE ACTIVITIES LEAGUE
CHIEF OF POLICE
California Council of Police and Sheriffs
Concerns of Police Survivors
DEPUTY SHERIFFS' ASSOCIATION
END POLICE VIOLENCE COLLECTIVE
FRATERNAL ORDER OF POLICE
HOLLYWOOD COMMUNITY-POLICE ADVISORY BOARD
LOS ANGELES POLICE PROTECTIVE LEAGUE
LOS ANGELES SCHOOL POLICE MANAGEMENT ASSOCIATION
NATIONAL BLACK POLICE ASSOCIATION
Newport Beach Police Management Association
ORGANIZATION OF POLICE AND SHERIFFS
POLICE DEPARTMENT
Police Chief's Association
Police Officer's Association
Police Officers' Research Association
SHERIFF
SHERIFF'S ASSOCIATION
SHERIFF'S DEPARTMENT / Sheriffs
SHERIFF'S EMPLOYEE BENEFITS ASSOCIATION

Also add Sheriff and Police organizations that are not in the crosswalk, to the crosswalk.

21. Remove CO-SPONSOR, SPONSOR, Co-Source prefix or suffix from org names and merge duplicates, including if it is in brackets. Afterwards, consolidate resulting duplicate organizations.

22. Consolidate YIMBY orgs in crosswalk to one org. Add YIMBY orgs to the crosswalk that are not there yet.

23. Conslidate place-based Chambers of Commerce as chapters of the California Chamber of Commerce (CALCHAMBER). Add Chambers of Commerce to the crosswalk that are not there yet.

24. Separate from the Industry-specific labor organizations make an AFL-CIO canonical that has just one chapter: the California Federation of Labor. Put place (but not industry) specific federations of labor as children. and AFL as a pre-merge child

25. Consolidate "100 Black Men" organizations

26. Merge CAL Fire/CDF entries into "CALIFORNIA DEPARTMENT OF FORESTRY AND FIRE PROTECTION" canonical. Merge California State Firefighters' Association entries into that canonical. Merge FIRE DISTRICTS ASSOCIATION OF CALIFORNIA into that canonical.

27. Strip "According to the" from org names and consolidate any resulting duplicates

28. Consolidate PEACE OFFICER STANDARDS AND TRAINING, Peace Officers' Association, PEACE OFFICER'S RESEARCH ASSOCIATION OF CALIFORNIA, CALFIORNIA CORRECTIONAL PEACE OFFICERS ASSOCIATION

29. Consolidate all the climate orgs under "350" in crosswalk

30. clean org names with malformed brackets e.g. "blah) org name" or "org name (blah". Let AI decide if it's an acronym in the brackets where the matching bracket should be added, or if it's an artifact like support/oppose or the date or soenthign where the bracket and the stuff in it should be removed.

31. consolidate SEIU orgs (not industry specific ones just ethnic group / place based ones) and make SEIU a pre-merge child of AFL-CIO

32. Consolidate Indivisible orgs both within and from org names not in the crosswalk, into the crosswalk

33. Consolidate Sierra Club orgs. Move conjoined entries to conjoined CSV, narrative text to embedded_in_narrative_text CSV, merge chapters/groups/task forces/sections under SIERRA CLUB canonical.

34. Consolidate AFSCME orgs, NAACP orgs, Planned Parenthood, CALPIRG, Audubon

35. Asked claude code (using opus 4.6 now) to go through 2_webapp/org_clusters_crosswalk.json and consolidate duplicate canonicals, move things that are not organizations (e.g. 7 individual letters), and consolidate local chapters of industry-specific unions

36. add orgs that start with "and " to cleaning patterns

37. Asked claude code to go through crosswalk and not in crosswalk and remove partial and conjoined entries. For conjoined entries, splitting them and adding individual organizations back into crosswalk.

38. Asked claude code to look through org_names_not_capitalized.csv to see if there are any real looking org names and add them to crosswalk

39. asked claude code to remove other invalid entry times from org_names_not_in_crosswalk.csv

40. asked claude code to come up with a list of orgs from most frequent to least in org_names_not_in_crosswalk.csv and add them to the crosswalk (or invalid CSVs as appropriate instead)

41. More cleaning: remove Continued from the end of org names, remove parens in entries like (AFSCME), clean digit followed by literal dot e.g. "1. The Performance Institute"

XX. Move things that are obviously not organization names (like sponsor or SB) to invalid organization names list 



## Managing Robot RAs (Claude Code Workers)

This project supports parallel task execution using multiple Claude Code sessions as "Research Assistants" (RAs), coordinated through `TASKS.md` and `QUESTIONS.md`. See `CLAUDE.md` for the full protocol.

### Setup

You need three terminal sessions running Claude Code in this directory:

1. **Management Assistant** — coordinates between you and the workers
2. **Worker RA sessions** (2+) — do the actual tasks

### Starting the Management Assistant

```
claude
```

Then prompt:

```
You are the Management Assistant. Read CLAUDE.md and follow the Management Assistant instructions. Poll QUESTIONS.md every 10 seconds for unanswered RA questions and present them to me. Also report task status (done/in progress/blocked/not started) with each check. When I send you a task description, add it to TASKS.md.
```

### Starting Worker RAs

Open separate terminals, run `claude` in each, and prompt:

```
You are RA-Alpha. Read CLAUDE.md and follow the Worker RA instructions. Pick a task from TASKS.md and get started.
```

```
You are RA-Beta. Read CLAUDE.md and follow the Worker RA instructions. Pick a task from TASKS.md and get started.
```

Use a different name for each session (RA-Alpha, RA-Beta, RA-Gamma, etc.).

### Day-to-Day Workflow

1. **Adding tasks**: Describe tasks to the Management Assistant in plain language. It formats and adds them to `TASKS.md`.
2. **Answering questions**: The manager polls `QUESTIONS.md` and surfaces RA questions to you. Give your answer and the manager writes it back.
3. **Status checks**: Ask the manager for a status update — it reads `TASKS.md` and summarizes.
4. **Blocked tasks**: When an RA gets stuck, it posts a question, unassigns itself, and moves on. Any RA can pick up a blocked task once the question is answered.

### Key Files

| File | Purpose |
|------|---------|
| `CLAUDE.md` | Full protocol for both roles |
| `TASKS.md` | Task list and write queue |
| `QUESTIONS.md` | Q&A between RAs and human |

## Crosswalk Relationship Types

- canonical
- alternate_spelling
- chapter
- renamed - org was renamed (e.g., Sierra Club Legal Defense Fund → Earthjustice)
- pre-merge - org that later merged into this one (e.g., AFL is a pre-merge of AFL-CIO)
- previously_known_as - prior name of the org

## Cleaning Patterns

`cleaning_patterns.txt` contains regex patterns that should be removed from the end of organization names. These are metadata about bill relationships (e.g., sponsor status), not part of the actual org name.

Patterns include:
- (CO-SPONSOR), (SPONSOR), (Sponsors)
- (Co-Source)
- (PRIOR VERSION)
- (OPPOSE), (OPPOSE UNLESS AMENDED)
- ("In Concept")
- (Principal Co-Sponsor), (Co-Sponsors)

## Cleaning Pipeline

The cleaning/deduplication/stats pipeline is documented in `CLAUDE.md`
("Cleaning & deduplication pipeline"). Run these in order whenever the crosswalk
or `cleaning_patterns.txt` changes:

1. `python3 scripts/clean_crosswalk.py` — apply `cleaning_patterns.txt`, dedup children, merge clusters that normalize identically.
2. `python3 scripts/regenerate_org_subsets.py` — re-check all org names against the crosswalk and redistribute the `org_name_subsets_for_cleaning/` CSVs.
3. `python3 generate_stats.py` — update `stats.json`.

## Invalid Organization Names

`org_names_invalid.csv` contains placeholder values and metadata fragments that are not actual organization names. These include:
- "None on file", "NONE RECEIVED", "None Known", "None to date", etc.
- "END", "Inc", "Oppose", "Unknown"
- Empty strings

