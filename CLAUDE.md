# Claude Notes

## Goal

We are building and cleaning a map of organization names so we can "crosswalk" how different organization names relate to each other. For each organization there is a canonical name, and other organization names may be alternate spellings or chapters of an organization. Basically, it's a forest of tree structures. The trees can be nested to any depth (e.g. you can have an alternate spelling of a chapter). It's very important that items in the crosswalk be properly consolidated with the correct canonical organization.

Note that you shouldn't remove entries unless they are exact duplicates since we need to be able to categorize all organizations that appear in the dataset, even if they are spelled wrong. It's ok to move organizations around like in or out of the crosswalk.

## Handling Invalid Entries

When an entry in the crosswalk turns out to not be a real organization, two things need to happen:

1. **Remove it from the crosswalk JSON** (`2_webapp/org_clusters_crosswalk.json`)
2. **Move its CSV row** (including the count) from whichever source file it's in (`org_name_subsets_for_cleaning/org_names_in_crosswalk.csv` or `org_name_subsets_for_cleaning/org_names_not_in_crosswalk.csv`) to the appropriate invalidity file in `org_name_subsets_for_cleaning/`:

| File | What goes here |
|------|---------------|
| `org_names_that_are_actually_individuals.csv` | People's names (e.g. "Attorney General Rob Bonta"). **Exception:** If the person holds a leadership role (Mayor, President, Director, Sheriff, Chief, Superintendent, CEO, Chair, etc.) at an identifiable org, make the entry an alternate spelling of that org instead. Only move to individuals CSV if no org is identifiable or the person is just a member/employee, not a leader. |
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

## Coordination Queues

To prevent concurrent edits from overwriting each other, `TASKS.md` and `QUESTIONS.md` each have their own write queue (in addition to the existing data file write queue). **Everyone** — management assistant and worker RAs alike — must join the relevant queue before editing these files.

### TASKS.md Write Queue
Located at the top of `TASKS.md` under "## TASKS.md Write Queue". To edit TASKS.md (mark tasks in-progress/done, add tasks, update the data write queue, etc.):
1. Add your name to the bottom of the TASKS.md Write Queue.
2. Wait until your name is at the top.
3. Make your edits.
4. Remove yourself from the queue.

### QUESTIONS.md Write Queue
Located at the top of `QUESTIONS.md` under "## QUESTIONS.md Write Queue". To edit QUESTIONS.md (post questions, write answers, etc.):
1. Add your name to the bottom of the QUESTIONS.md Write Queue.
2. Wait until your name is at the top.
3. Make your edits.
4. Remove yourself from the queue.

**Important:** These queues are separate from the data file Write Queue (for crosswalk JSON and CSVs). You may hold a position in multiple queues simultaneously.

## Management Assistant Role

The management assistant is a dedicated Claude Code session that coordinates between the human supervisor and worker RAs. It does NOT do worker tasks itself.

**Responsibilities:**
- Take task descriptions from the human and format them into `TASKS.md` (with task number, description, status "Not Started")
- Monitor `QUESTIONS.md` for open questions from worker RAs
- Present open questions to the human supervisor and collect answers
- Write the human's answers back into `QUESTIONS.md` and change status to "Answered"
- Give the human status updates by reading `TASKS.md` (what's done, in progress, blocked)
- Continuously scan `2_webapp/org_clusters_crosswalk.json` for issues and create new tasks

**Workflow:**
1. Human gives task descriptions -> management assistant adds them to `TASKS.md`
2. Management assistant periodically checks `QUESTIONS.md` for unanswered questions -> presents them to the human
3. Human answers -> management assistant writes answers to `QUESTIONS.md`
4. Human asks for status -> management assistant reads `TASKS.md` and summarizes
5. Management assistant scans the crosswalk JSON for issues (invalid entries, duplicates, etc.) and proposes new tasks

**Scanning protocol:** The management assistant scans `2_webapp/org_clusters_crosswalk.json` in 5000-line chunks using background agents. Progress is tracked in the memory file `scan_status.md`.

**Task proposal format:** When presenting proposed tasks to the human for review:
- For each task, briefly explain what the RA will DO (the workflow/instructions), not just list the entries
- List org names VERTICALLY (one per line, bulleted) so they are easy for a human to scan — never inline in a paragraph
- Always show specific org names so the human can review before approval

**Categorization rules for scan findings:**
- **Leadership roles** that make an entry an alt spelling of the org (not an individual): Mayor, President, Director (of whole org), Sheriff, Chief, Superintendent, CEO, Chair, Owner of a business
- **NOT leadership** — these are plain individuals: Councilmembers, Supervisors, Commissioners, Trustees (unless Chair), Legislators, Vice/Deputy/Associate roles, and department-level Directors (e.g. "Director of Strategic Planning" or "Chief of a branch/division")
- **Out-of-state orgs** that lobby the CA legislature are legitimate — keep in crosswalk
- **Truncated entries**: Search the crosswalk AND the internet before moving to partial. If the full org name is unambiguous, add the full name as canonical (if not already present) and make the truncated version an alternate spelling. Only move to partial if truly ambiguous after both searches.
- **Dirty entries**: After cleaning (stripping metadata/prefixes/suffixes), check if the clean version is still invalid (might be an individual, fragment, etc.) and move to the appropriate CSV
- **Conjoined entries**: Split out the individual orgs and ensure each one is present in the crosswalk
- **OCR/typo entries**: Make the typo version an alternate spelling under the correct canonical

## General Crosswalk Workflow Principles

These apply to ALL task types (consolidation, OCR fixes, conjoined splitting, narrative extraction, dirty cleaning, etc.):

1. **Always search the crosswalk first.** Before adding any new canonical, search the crosswalk thoroughly. The org you're looking for is most likely already present — as a canonical, chapter, or alternate spelling. Only create a new canonical if the org genuinely doesn't exist anywhere in the crosswalk.

2. **Place entries at the correct hierarchy level.** The crosswalk is a forest of trees. When inserting an entry, decide whether it should be:
   - An **alternate spelling** of a canonical (e.g. "JCPenny" → alt spelling of "JC Penney")
   - A **chapter** of a canonical (e.g. "Sierra Club, San Francisco Chapter" → chapter of "Sierra Club")
   - An **alternate spelling of a chapter** (e.g. "SF Sierra Club" → alt spelling of the San Francisco chapter, not of the national canonical)
   Don't default to making everything a flat alt spelling of the top-level canonical.

3. **Preserve org names — don't discard them.** Even dirty, truncated, or narrative-embedded entries that contain identifiable org names should not just be moved to an invalidity CSV — the org name must be preserved in the crosswalk. For dirty/truncated entries, make the entry an alternate spelling (or chapter) of the real org. For narrative-embedded entries (e.g. "In to the bill, the California Hospital"), **extract** the clean org name from the narrative text; don't use the narrative text itself as an alt spelling. Ensure the extracted org exists in the crosswalk (search first — it's most likely already there), then move the narrative entry to the appropriate CSV.

4. **Location suffixes may be chapter information.** Don't strip location data from org names (e.g. "Inner City Law Center, Los Angeles") — these may indicate chapters or regional offices. Only strip clearly extraneous metadata like dates, phone numbers, counts, or person names.

5. **Conjoined entries: check before adding.** When splitting a conjoined entry, search the crosswalk for each individual org. They're most likely already present. Only add new canonicals for orgs that genuinely aren't anywhere in the crosswalk.

6. **Narrative/dirty entries with extractable org names:** Identify the embedded org and search the crosswalk (most likely already present). For dirty entries, make the dirty version an alt spelling or chapter of the clean org — placed at the correct hierarchy level. For narrative entries, **extract** the clean org name (don't use the narrative text as an alt spelling) and ensure the org exists in the crosswalk. Move the CSV row to the appropriate invalidity CSV. If the org genuinely isn't in the crosswalk, add it as a new canonical.

## Worker RA Role

Each worker RA session is given a name by the user (e.g. "RA-Alpha", "RA-Beta").

**Task workflow:**
1. Join the TASKS.md Write Queue to mark a task "In Progress" with your name.
2. **Plan phase (read-only)**: Read all relevant files, research the task, and plan out exactly what changes you will make. Do NOT edit any project files yet.
3. **Join the data Write Queue**: Add your name to the bottom of the Data Write Queue in `TASKS.md`.
4. **Wait for your turn**: Periodically re-read `TASKS.md`. When your name is at the top of the data queue, you have write access to project data files.
5. **Execute**: Make all your changes and commit with a descriptive message.
6. **Release**: Join the TASKS.md Write Queue again to remove yourself from the data write queue and mark your task "Done".

**CSV handling rules:**
- **Consolidating within the crosswalk** (reorganizing existing entries): No CSV changes needed.
- **Adding or removing an item from the crosswalk**: Figure out which CSV in `org_name_subsets_for_cleaning/` the org name should be in, move the row (including its frequency count) to the correct CSV, and remove it from the original CSV to avoid duplicates.
- **If orgs are moved in or out of the crosswalk**: Run the cleaning/dedup/stats pipeline before committing (see below).

**Cleaning & deduplication pipeline** (run in this order):
1. `python scripts/clean_crosswalk.py` — applies regex patterns from `cleaning_patterns.txt` to strip metadata suffixes, deduplicates children, and merges clusters whose canonicals normalize identically.
2. `python scripts/regenerate_org_subsets.py` — re-checks all org names against the current crosswalk, redistributes names between CSVs, and deduplicates within and across all CSV files.
3. `python generate_stats.py` — updates `stats.json` with current counts.

**Commit discipline:** One task per commit. Keep commits atomic and descriptive. Delete any temporary/processing scripts you created before committing — only commit the data changes.

**Blocked tasks:**
- If a task is ambiguous or you're unsure how to proceed, use the TASKS.md Write Queue to mark it "Blocked" and **clear the Assignee field**.
- Use the QUESTIONS.md Write Queue to post your question with the task number and your RA name.
- Remove yourself from the data write queue if you're in it.
- Move on to another task.

**Picking up blocked tasks:** Any RA (not just the original one) can pick up a "Blocked" task. Before picking a new "Not Started" task, check `QUESTIONS.md` for answered questions on blocked tasks. If a blocked task's question has been answered, you can claim it — mark it "In Progress" with your name and resume work on it.

**When all tasks are done or blocked:** Poll `TASKS.md` every 10 seconds to see if new tasks have been added or if blocked tasks have been unblocked (check `QUESTIONS.md` for answered questions). Pick up any available work.
