# Claude Notes

## Goal

We are building and cleaning a map of organization names so we can "crosswalk" how different organization names relate to each other. For each organization there is a canonical name, and other organization names may be alternate spellings or chapters of an organization. Basically, it's a forest of tree structures. The trees can be nested to any depth (e.g. you can have an alternate spelling of a chapter). It's very important that items in the crosswalk be properly consolidated with the correct canonical organization.

Note that you shouldn't remove entries unless they are exact duplicates since we need to be able to categorize all organizations that appear in the dataset, even if they are spelled wrong. It's ok to move organizations around like in or out of the crosswalk.

## Handling Invalid Entries

When an entry in the crosswalk turns out to not be a real organization, two things need to happen:

1. **Remove it from the crosswalk JSON** (`2_webapp/org_clusters_crosswalk.json`)
2. **Move its CSV row** (including the count) from whichever source file it's in (`org_names_for_cleaning/org_names_in_crosswalk.csv` or `org_names_for_cleaning/org_names_not_in_crosswalk.csv`) to the appropriate invalidity file in `org_names_for_cleaning/`:

| File | What goes here |
|------|---------------|
| `org_names_that_are_actually_individuals.csv` | People's names (e.g. "Attorney General Rob Bonta"). **Exception:** If the person holds a leadership role (Mayor, President, Director, Sheriff, Chief, Superintendent, CEO, Chair, etc.) at an identifiable org, make the entry an alternate spelling of that org instead. Only move to individuals CSV if no org is identifiable or the person is just a member/employee, not a leader. |
| `org_names_partial.csv` | Incomplete/fragment names (e.g. "LOS", "SAN") |
| `org_names_conjoined.csv` | Multiple orgs joined together (e.g. "Sierra Club Planning and Conservation League") |
| `org_names_invalid.csv` | Not organizations at all (e.g. legislative bills, procedural text like "GOVERNOR'S VETO MESSAGE", dates, phone numbers). Also extraction artifacts that are just a stray leading parenthetical (the retired `org_names_that_start_with_parens.csv` bucket was folded in here). |

These four CSVs use the same format: `org_name,count` — move the entire row including the count.

**Narrative-embedded prose is a special case** — it goes to `narrative_text_mapping_to_orgs.csv`, which has a **different schema (`narrative_text,mapped_org`, no count)**. Use it when a crosswalk entry is a chunk of bill-position prose with no org name of its own (e.g. "California Coalition of Travel Agents to improve the operation of California's Seller of Travel Law…"). Don't reuse the prose as an alt spelling: **extract** the org it describes, make sure that org is in the crosswalk (search first — most likely already there; add a new canonical only if genuinely absent), remove the prose node from the JSON, and append a `narrative_text,mapped_org` row. **`mapped_org` is never blank** — if the prose names no org at all, it's not a narrative mapping, it's just a non-org: route it to `org_names_invalid.csv` instead of this file. This file is read by `extract_org_names.py` (to skip re-diagnosing known prose) and `LEGINFO_IMPORT.md` step 4 (to attribute the prose's bill count to the org); it is **never** redistributed by `regenerate_org_subsets.py`. See `org_names_for_cleaning/README.md` and `LEGINFO_IMPORT.md`.

If the entry only exists in the JSON and not in any CSV file, just remove it from the JSON.

## Crosswalk Data

- `crosswalk.standardizenames.manualedits_clean.csv` - original source, DO NOT EDIT
- `2_webapp/org_clusters_crosswalk.json` - live file, all updates go here
- For the CSV file some of the items in the file contain commas and you need to parse it properly. Don't just parse it by comma use CSV parsing libraries

## Coordination Queues

To prevent concurrent edits from overwriting each other, `TASKS.md` and `QUESTIONS.md` each have their own write queue (in addition to the existing data file write queue). **Everyone** — management assistant and worker RAs alike — must join the relevant queue before editing these files. To prevent blocking other agents, Please only add yourself to the write queue when you're ready to write and have finished processing.

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
1. Human gives task descriptions -> management assistant adds them to `TASKS.md`, commits ONLY `TASKS.md` immediately (e.g. `git add TASKS.md && git commit -m "Add tasks 530-535"`), and does so while still holding the TASKS.md Write Queue
2. Management assistant periodically checks `QUESTIONS.md` for unanswered questions -> presents them to the human
3. Human answers -> management assistant writes answers to `QUESTIONS.md`
4. Human asks for status -> management assistant reads `TASKS.md` and summarizes
5. Management assistant scans the crosswalk JSON for issues (invalid entries, duplicates, etc.) and proposes new tasks

**What the scan is:** The crosswalk JSON is the live forest of org-name trees, assembled from messy real-world source data, so it contains many entries that aren't clean canonical orgs — individuals, partial/truncated names, conjoined orgs, OCR typos, invalid non-orgs (bills, dates, phone numbers), misplaced hierarchy, duplicates, and org names buried in narrative prose. The scan is the issue-discovery engine that reads through the JSON, finds these problems, and turns each into a worker-RA task. It's what keeps the task queue full. The fix is almost never deletion: preserve every org name (consolidate as an alt spelling/chapter), and only route genuine non-orgs to the appropriate invalidity CSV (see "Handling Invalid Entries" above).

**Scanning protocol:** The management assistant scans `2_webapp/org_clusters_crosswalk.json` in 5000-line chunks using background agents. Progress is tracked in the memory file `scan_status.md`. **Always auto-launch the next scan batch immediately after creating tasks from the current batch — don't wait to be asked.** If no scan is currently running, launch one.

**Recurring dirty patterns → propose a cleaning pattern:** When a scan surfaces **3+ entries sharing the same strippable boilerplate** — a suffix, a prefix, OR text in the **middle** of the string (e.g. an embedded page footer/header, "Page N of M", scan metadata, `, sponsor of the bill`, `(N letters)`, `- dated 3/1/2024`) — don't just file individual cleanup tasks. Post a reusable-regex proposal to QUESTIONS.md for supervisor sign-off (regex + 2-3 before→after examples + 1-2 near-misses it must NOT match + affected-entry count). If you're unsure whether it's a real recurring pattern or how to scope it, just ask in QUESTIONS.md. Never edit `cleaning_patterns.txt` directly; once approved it becomes a task that adds the pattern and runs the full pipeline. See "Proposing new cleaning patterns" in the Worker RA Role.

**Narrative-embedded text (always check for this):** On every scan, flag org names buried in longer prose (e.g. "In to the bill, the California Hospital Association writes in support..."). For each, create a task whose RA workflow is: (1) **extract** the real org name from the prose; (2) **search the crosswalk first** — it's most likely already present as a canonical/chapter/alt; add a new canonical only if it genuinely isn't anywhere; (3) remove the narrative string from the JSON and never reuse the narrative prose itself as an alt spelling; (4) run the clean/dedup/stats pipeline before committing and follow the Data Write Queue.

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

6. **Narrative/dirty entries with extractable org names:** Identify the embedded org and search the crosswalk (most likely already present). For dirty entries, make the dirty version an alt spelling or chapter of the clean org — placed at the correct hierarchy level. For narrative entries, **extract** the clean org name (don't use the narrative text as an alt spelling) and ensure the org exists in the crosswalk. If the org genuinely isn't in the crosswalk, add it as a new canonical.

7. **Clean the org name before adding it to the crosswalk.** Strip bill/position metadata so the crosswalk holds the clean name, never a dirty string — e.g. `California Hospital Association (sponsor)` → `California Hospital Association`; also drop trailing `(previous version)`, counts, dates, and similar annotations. Run the string through the cleaning regexes with `python3 scripts/clean_name.py "<org name or ;-separated list>"` (fast — loads only `cleaning_patterns.txt`, not the crosswalk; prints the cleaned name(s), one per line). Keep meaningful parts intact: do **not** strip location/chapter suffixes (principle 4) or `dba` names.

## Worker RA Role

Each worker RA session is given a name by the user (e.g. "RA-Alpha", "RA-Beta").

**Task workflow:**
1. Join the TASKS.md Write Queue to mark a task "In Progress" with your name.
2. **Plan phase (read-only)**: Read all relevant files, research the task, and plan out exactly what changes you will make. Do NOT edit any project files yet.
3. **Join the data Write Queue**: Add your name to the bottom of the Data Write Queue in `TASKS.md`.
4. **Wait for your turn**: Periodically re-read `TASKS.md`. When your name is at the top of the data queue, you have write access to project data files.
5. **Execute**: Make all your changes.
6. **Commit while you still hold the write queue**: Stage ONLY the files you modified (use `git add <specific files>`, never `git add -A` or `git add .`). Commit with a descriptive message. This must happen before you release the queue.
7. **Release**: Join the TASKS.md Write Queue again to remove yourself from the data write queue and mark your task "Done". Commit the TASKS.md change immediately with a message like "Mark task N done".

**CSV handling rules:**
- **Consolidating within the crosswalk** (reorganizing existing entries): No CSV changes needed.
- **Adding or removing an item from the crosswalk**: Figure out which CSV in `org_names_for_cleaning/` the org name should be in, move the row (including its frequency count) to the correct CSV, and remove it from the original CSV to avoid duplicates.
- **If orgs are moved in or out of the crosswalk**: Run the cleaning/dedup/stats pipeline before committing (see below).

**Cleaning & deduplication pipeline** (run in this order):
1. `python3 scripts/clean_crosswalk.py` — applies regex patterns from `cleaning_patterns.txt` to strip metadata suffixes, deduplicates children, and merges clusters whose canonicals normalize identically.
2. `python3 scripts/regenerate_org_subsets.py` — re-checks all org names against the current crosswalk, redistributes names between CSVs, and deduplicates within and across all CSV files.
3. `python3 generate_stats.py` — updates `stats.json` with current counts.

**Commit discipline:** One task per commit. Keep commits atomic and descriptive. Delete any temporary/processing scripts you created before committing — only commit the data changes.

**Proposing new cleaning patterns:** When you notice **3 or more** dirty entries that share the same strippable boilerplate, don't just clean them one by one — propose a reusable regex for `cleaning_patterns.txt` so the whole class is stripped globally (now and for future entries). The boilerplate does **not** have to be a suffix or prefix — it can sit **in the middle** of the string too (e.g. an embedded page footer/header, a repeated "Page N of M", scan metadata, or boilerplate spliced into the org name). Examples: `, sponsor of the bill`, `(N letters)`, `(N individuals)`, `- dated 3/1/2024`, a mid-string footer like `... Printed 5/1/24 Page 2 ...`. **If you're not sure whether something is a recurring strippable pattern (or how to scope the regex), just ask** — post the question to QUESTIONS.md rather than guessing. **Do NOT edit `cleaning_patterns.txt` directly or apply the pattern yourself** — global regexes can over-strip legitimate org names, so they need supervisor sign-off first. Instead, join the QUESTIONS.md Write Queue and post a proposal containing:
- the proposed **regex** (and what it strips);
- **2-3 example matches** with before → after;
- **1-2 near-misses it must NOT match** (legit org names the regex must leave intact);
- the approximate **count** of crosswalk entries the pattern would affect.

The supervisor reviews and answers in QUESTIONS.md. Once approved, it becomes a normal task: add the pattern to `cleaning_patterns.txt`, then run the full clean/dedup/stats pipeline. A true one-off (fewer than 3 similar entries) doesn't need a pattern — just clean it manually.

**Blocked tasks:**
- If a task is ambiguous or you're unsure how to proceed, use the TASKS.md Write Queue to mark it "Blocked" and **clear the Assignee field**.
- Use the QUESTIONS.md Write Queue to post your question with the task number and your RA name.
- Remove yourself from the data write queue if you're in it.
- Move on to another task.

**Picking up blocked tasks:** Any RA (not just the original one) can pick up a "Blocked" task. Before picking a new "Not Started" task, check `QUESTIONS.md` for answered questions on blocked tasks. If a blocked task's question has been answered, you can claim it — mark it "In Progress" with your name and resume work on it.

**When all tasks are done or blocked:** Poll `TASKS.md` every 10 seconds to see if new tasks have been added or if blocked tasks have been unblocked (check `QUESTIONS.md` for answered questions). Pick up any available work.

## Headless Fleet Runner (`scripts/fleet.sh`)

Instead of running worker RAs as long-lived interactive sessions (which accumulate context and degrade), the **headless fleet** spawns a brand-new `claude -p` process for **every single task**. Each task starts with a completely clean context — no manual `/clear`, no degradation — and the worker exits after one task so the loop launches a fresh one for the next.

**Scripts (in `scripts/`):**
- `run_ra.sh <NAME>` — one RA: loops forever, spawning a fresh `claude -p` worker per task. Each worker is told (via the prompt) to follow the Worker RA Role workflow above, do EXACTLY ONE task, commit, and exit.
- `run_fleet.sh <N|name…>` — launches N `run_ra.sh` loops in parallel (staggered so they don't collide on the queues).
- `fleet.sh {start|status|errors|logs|stop}` — the control panel (use this).

**Operating it:**
```
scripts/fleet.sh start          # launch the default 3 RAs, detached (returns immediately)
scripts/fleet.sh start 2        # launch 2 (or: DEFAULT_RAS=2 scripts/fleet.sh start)
scripts/fleet.sh status         # running? per-RA completed counts
scripts/fleet.sh errors         # classified error log (category counts, fatal hits, recent)
scripts/fleet.sh logs           # tail all RA logs
scripts/fleet.sh stop           # hard-stop all loops + workers
```
`start` runs detached (`nohup`), so it does not block the caller; a supervising/management session can launch it and keep working. Default fleet size is **3** (`DEFAULT_RAS`). NOTE: launching from inside another `claude` Bash tool requires `dangerouslyDisableSandbox` (nested `claude` can't create its session dir under the parent sandbox); from a plain terminal it just works.

**Prevent the Mac from sleeping while the fleet runs (do this every time):** A detached fleet keeps working only if the machine stays awake, so on macOS pair `start` with `caffeinate`. Tie the assertion to the launcher pid so it auto-releases on `stop`:
```
scripts/fleet.sh start                                   # note the "launcher pid" it prints
nohup caffeinate -i -m -s -w <launcher_pid> >/dev/null 2>&1 & disown
```
`-i` prevents idle sleep, `-m` prevents disk idle sleep, `-s` prevents system sleep on AC power, and `-w <launcher_pid>` makes `caffeinate` exit automatically when the fleet launcher dies (i.e. on the next `fleet.sh stop`). Get the launcher pid from the `start` output or `scripts/fleet.sh status`. Verify with `pgrep -fl caffeinate`. (If you don't have the pid handy, a standalone `nohup caffeinate -i -m -s >/dev/null 2>&1 & disown` also works but must be killed manually when you stop the fleet.)

**Coordination:** fleet workers use the SAME `TASKS.md`/`QUESTIONS.md`/Data Write Queues as everyone else, so they interoperate with interactive RAs. They poll the Data Write Queue **actively within their turn** (a `claude -p` process is one-shot and cannot be suspended/resumed — it must never "schedule a wakeup" or "await a signal," which would exit the process and strand a queue slot).

**Resilience (built into `run_ra.sh`):**
- **No idle/silence timeout.** A quiet worker is usually just waiting on a slow tool call (e.g. loading the 1M-line crosswalk), NOT hung. The watchdog kills ONLY on a real stream signal — a `rate_limit_event` whose status is a genuine block (not `allowed`/`allowed_warning`). A 45-min absolute cap is the last-resort infinite-hang guard.
- **Never permanently stops on errors.** On any failure it backs off (longer after repeated failures) and keeps retrying — so the fleet **auto-resumes** when a usage/monthly-spend limit clears or the API recovers, instead of dying. Transient API errors are left to Claude's own internal retry.
- **Error classification + logging.** Every failure is classified (`usage_limit`, `rate_limit`, `overloaded`, `api_connection`, `timeout_kill`, `unknown`, …) and appended to `ra_logs/errors.log`; view with `scripts/fleet.sh errors`. Fatal = usage/spend limit or auth.
- **Clean process lifecycle.** `stop`/`start` hard-reap (SIGTERM→SIGKILL) loops AND workers — hung workers ignore SIGTERM and reparent to init, so a plain `pkill` would leave orphans that collide under the same RA name. `start` also reaps stragglers from a prior launch before starting.

**Config knobs (env vars):** `DEFAULT_RAS` (3), `MAX_TASKS` (0=unlimited), `TASK_TIMEOUT` (2700s absolute cap), `STREAM_POLL` (20s), `FATAL_BACKOFF` (300s), `MODEL`. Logs: `ra_logs/<NAME>.log`; classified errors: `ra_logs/errors.log`.

**Usage/spend limits:** workers run on the account's quota. A monthly **spend** limit (claude.ai/settings/usage) does NOT clear by waiting — raise it or wait for the billing reset; the fleet auto-resumes once it clears. A 5-hour/7-day **rate** limit resets on its own. If all RAs error simultaneously, it's an account-wide cap, not a bug.

**Orphan cleanup:** if a worker is killed mid-task (or exits after a queue-block without resetting), its claim can linger as a stale "In Progress" / Data-Write-Queue entry. These are detected by checking whether the owning RA is live + whether the task has committed data; reset to "Not Started" if no data was committed (or mark Done if data was committed but the done-mark was missed).
