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

## Management Assistant Role

The management assistant is a dedicated Claude Code session that coordinates between the human supervisor and worker RAs. It does NOT do worker tasks itself.

**Responsibilities:**
- Take task descriptions from the human and format them into `TASKS.md` (with task number, description, status "Not Started")
- Monitor `QUESTIONS.md` for open questions from worker RAs
- Present open questions to the human supervisor and collect answers
- Write the human's answers back into `QUESTIONS.md` and change status to "Answered"
- Give the human status updates by reading `TASKS.md` (what's done, in progress, blocked)

**Workflow:**
1. Human gives task descriptions -> management assistant adds them to `TASKS.md`
2. Management assistant periodically checks `QUESTIONS.md` for unanswered questions -> presents them to the human
3. Human answers -> management assistant writes answers to `QUESTIONS.md`
4. Human asks for status -> management assistant reads `TASKS.md` and summarizes

## Worker RA Role

Each worker RA session is given a name by the user (e.g. "RA-Alpha", "RA-Beta").

**Task workflow:**
1. Read `TASKS.md` and pick a task that is "Not Started". Mark it "In Progress" with your name.
2. **Plan phase (read-only)**: Read all relevant files, research the task, and plan out exactly what changes you will make. Do NOT edit any project files yet.
3. **Join the write queue**: Add your name to the bottom of the Write Queue in `TASKS.md`.
4. **Wait for your turn**: Periodically re-read `TASKS.md`. When your name is at the top of the queue, you have write access.
5. **Execute**: Make all your changes and commit with a descriptive message.
6. **Release**: Remove yourself from the write queue and mark your task "Done" in `TASKS.md`.

**Commit discipline:** One task per commit. Keep commits atomic and descriptive.

**Blocked tasks:**
- If a task is ambiguous or you're unsure how to proceed, mark it "Blocked" in `TASKS.md`.
- Post your question in `QUESTIONS.md` with the task number and your RA name.
- Remove yourself from the write queue if you're in it.
- Move on to another task.
- Periodically check `QUESTIONS.md` for answers to your questions. When answered, you can pick the task back up.

**Checking for answers:** Before picking a new task, check `QUESTIONS.md` to see if any of your blocked tasks have been answered. If so, update the task status back to "In Progress" and resume work on it.
