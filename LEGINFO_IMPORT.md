# Importing Data from Leginfo Metadata

A runnable playbook for pulling organization names out of the Leginfo bill metadata,
resolving them against the crosswalk, and routing whatever's left to the right place.

The goal: every org that supported or opposed a bill in Leginfo ends up either (a) in
the crosswalk at the correct hierarchy level, or (b) in the correct `org_names_*.csv`
invalidity file in `org_names_for_cleaning/`. No org string is ever discarded.

> ### ⚠️ Read every entry. This is AI work, not a script.
>
> Except for the single deterministic exact-match pass in step 1, **every judgment in this
> playbook must be made by an AI actually reading the entry** — one at a time. Is this cell a
> clean list or narrative prose? What organization is buried in the prose? Is this string
> even an organization? Is it the same org as one already in the crosswalk? Read it and
> decide.
>
> **Never substitute a regex, keyword list, capitalization rule, fuzzy-match threshold,
> lookup table, or any other heuristic for that reading.** Heuristics silently miss cases and
> cannot be trusted here — a prose-detection regex we tried flagged ~8,000 cells when the
> true count was closer to ~30,000. There are no shortcuts in step 2: when in doubt, read it,
> every single one.

---

## 0. Background

- **Source data:** `leginfo_metadata.csv` (built from the [`Nall-Group/leginfo`](https://github.com/Nall-Group/leginfo) repo).
  Local path used by the scripts: `/Users/ruthgracewong/leginfo/extract_all_leginfo_metadata/leginfo_metadata.csv`.
- **The org-bearing columns** we pull orgs from — the four stance columns plus `sponsor`:
  - `support`
  - `opposition`
  - `opposition_unless_amended`
  - `support_with_amendments`
  - `sponsor`
- Each cell is a `;`-separated list of org names (often dirty: OCR typos, trailing
  metadata, narrative prose, multiple orgs mashed together). Some cells are not a list at
  all but a chunk of **narrative prose** (e.g. "In support of the bill, the California
  Hospital Association writes…"). Those are handled in the resolution scan (step 2), not
  up front.
- **Destination:** the crosswalk JSON (`2_webapp/org_clusters_crosswalk.json`) plus the
  routing CSVs in `org_names_for_cleaning/`.

See `CLAUDE.md` ("General Crosswalk Workflow Principles") and
`org_names_for_cleaning/README.md` for the rules this playbook builds on.

---

## 1. Straight matching (deterministic)

Pull and clean every org name from the five org columns and match each against the
crosswalk. This pass is **pure script — no AI, no narrative handling.**

> ### ⚠️ Reset `org_names_not_in_crosswalk.csv` first (the script APPENDS to it)
>
> `extract_org_names.py` **appends** every new unmatched org to
> `org_names_for_cleaning/org_names_not_in_crosswalk.csv`. If you re-run without clearing it,
> a second run's unmatched pile is left sitting on top of the first (stale rows that may no
> longer belong there). So before every run, **truncate this one file back to a header-only
> file** — actually rewrite its contents, not `git checkout` (a checkout only undoes
> *uncommitted* changes and does nothing once a prior run is committed):
>
> ```bash
> printf 'org_name,count\n' > org_names_for_cleaning/org_names_not_in_crosswalk.csv
> ```
>
> **Nothing else needs resetting.** The other routing CSVs (`invalid`, `partial`,
> `individuals`, `conjoined`, …) are **overwritten in place idempotently** — the script
> rewrites each matching org's count to the current leginfo value, so re-running reproduces
> the same numbers. `stats.json` is regenerated at the end of every run, and
> `org_names_import_summary.csv` is fully overwritten — none of these need a manual reset.

```bash
python3 extract_org_names.py
```

What it does:
- Reads `leginfo_metadata.csv` line by line (the file is large — streamed, not loaded).
- For each of the five org columns, splits the cell on `;`.
- Cleans each name with the regex patterns in `cleaning_patterns.txt` (strips trailing
  metadata like dates, positions, counts).
- **Counts each org by the number of bill analyses (rows) it appears in.** Within a single
  row an org is counted once even if it shows up in more than one stance column; the same
  underlying bill still counts once per analysis row (multiple analyses → multiple counts).
- Checks each cleaned name against the crosswalk — a **plain name-set membership test**
  (exact or punctuation-normalized). No canonical is resolved here; canonicals are a step-4
  concern. **Matched orgs are done** for now (their canonical name gets written in step 4).
- **Filters out already-routed orgs** — compares against all existing `org_names_*.csv`
  files in `org_names_for_cleaning/` (invalids, individuals, partials, conjoined, etc.).
  Orgs already in one of those files are skipped (not re-added to `not_in_crosswalk.csv`).
- **Updates counts** in existing CSVs — for any org that appears in both leginfo and an
  existing routing CSV, replaces the old count with the new leginfo count.
- Writes `org_names_import_summary.csv` with status for every org: `in_crosswalk`,
  `already_routed` (with which CSV), or `unmatched`.
- **Routes genuinely new unmatched orgs** into
  `org_names_for_cleaning/org_names_not_in_crosswalk.csv`.

**Prose falls through here on purpose.** A narrative cell won't cleanly match the
crosswalk, so it lands in the unmatched pile like any other non-match. We do **not** try
to detect or fix prose in this pass — that happens in step 2, where the subagent can read
each cell whole.

---

## 2. Resolution scan

Everything that didn't straight-match in step 1 is resolved by a **parallel diagnosis scan**.
Sub-agents read the unmatched items and decide what each one needs — the per-item logic is in
"How each item is diagnosed" below. The hard part, the judgment, is parallelized across the
scan.

**Division of labor (stated once — the routing details are below).** The **scanner applies
every routing-CSV and source-file write itself** and records the prose/conjoined rewrites for
step 4. The **only** thing handed to the fleet is a bundled *"add these valid orgs to the
crosswalk"* task per batch, which a worker RA works with the usual Worker RA discipline,
editing **only the crosswalk JSON**. A worker RA never opens, greps, or writes a routing CSV
or `leginfo_metadata.csv` at any point.

### The diagnosis scan (parallel sub-agents)

Partition step 1's deduped unmatched worklist (`org_names_not_in_crosswalk.csv`) into
**~15-item batches, one sub-agent per batch.** Small batches so every item gets a careful
read, a crosswalk search, and a web lookup when needed.

**This is AI reading, not a script.** Each sub-agent reads every item in its batch
individually. For each item it makes one judgment first: **is this an organization name, or
is it narrative prose?** Only when it's prose does the agent grep the string in
`leginfo_metadata.csv` to pull the whole original cell for context — prose is long and
distinctive, so it lands on its own cell, and grepping a whole org name (or a short fragment)
would just match many unrelated cells. Everything else is already a whole org string and goes
straight to triage. No regex, keyword list, or heuristic decides anything — automation we've
tried on this data has been consistently and badly inaccurate, so every item is read, one at a
time.

### How each item is diagnosed

**Is it an org name or prose?** Read the item and decide: an organization name, or narrative
prose (e.g. "In support of the bill, the California Hospital Association writes…")?

- **Prose** → grep the string in `leginfo_metadata.csv` to recover the whole original cell,
  then **read the cell and figure out which organization(s) it's describing, and replace the
  prose with that organization's name** — and if it describes more than one, with a
  **`;`-separated list of their names** (the standard cell format). This is the LLM reading and
  reasoning — never a script, regex, or extraction heuristic. The scanner **records the cell's resolved org(s)**
  (used in step 4 to fill the canonical column and rewrite the cell) and runs those orgs
  through the checks below. The prose itself is discarded — never kept, never reused as an alt
  spelling. If the prose names no organization, route the original string to
  `org_names_invalid.csv` so it's still accounted for.
- **An org name** → go straight to the checks below; no grep needed.

After this step each candidate is a clean org name — whether it arrived as one or was the
organization a prose cell was describing.

**Clean each org and check the crosswalk.** Normalize each candidate org with the cleaning
regexes (`cleaning_patterns.txt`) — the same strip step as straight matching, removing
trailing/parenthetical metadata (dates, positions, vote counts, `(sponsor)`,
`(previous version)`, …) — and look the cleaned name up in the crosswalk. **If it's already
there, you're done with that org** — nothing to add. This cleaning is **only for the lookup**;
do **not** write the cleaned value anywhere. Cleaning is deterministic and cheap, so the
canonical-matching pass (step 4) simply re-cleans the source when it needs to — there's no
cell to rewrite here. (Standalone parentheticals collapse to nothing when cleaned, so there's
no "starts with a paren" case to handle.)

**Triage: invalid, partial, or conjoined.** Read each candidate org yourself and filter it
through three checks — this is the pass that removes everything that isn't a single, clean,
valid organization. Judge each one individually; do **not** script, regex, or heuristic this
classification — actually read it. Handle **conjoined first**, because splitting produces new
strings that must themselves be triaged.

- **Conjoined** — the string is actually multiple organizations mashed together (e.g.
  "Sierra Club Planning and Conservation League"). Split it into its individual orgs; the
  scanner **records them as the cell's resolved orgs** (applied in step 4). **Do not preserve
  the conjoined string** — it isn't a real org, and its bill count carries forward on the
  split-out orgs. Do **not** route it to any CSV: a conjoined string sitting in a routing CSV
  gets treated as already-routed by straight matching and is silently skipped instead of split,
  which corrupts the counts. Feed each split-out org back through this triage.
- **Invalid** — the string isn't a real organization at all (bill text, vote tallies,
  procedural text, dates, phone numbers; a fragment or generic word; a bare person's name
  **who is not a leader representing an org** — see the note below). Route it to the matching
  CSV below and drop it from the working list.
- **Partial** — a truncated/fragment name (e.g. "California Coalition for"). Try to
  disambiguate it to its full org name (search the crosswalk **and** the web). If it resolves
  unambiguously, **leave the cell as-is** — it's added as an **alternate spelling** of the
  full org when the plan is applied. **Only if it stays ambiguous after both
  searches**, move it to `org_names_partial.csv`.

The scanner appends each invalid (or ambiguous-partial) string, with its count, to the correct
CSV below, and drops it from the working list. If it's already in that CSV, its count is
updated rather than duplicated. (Valid orgs are **not** routed to a CSV — they become RA tasks
that add them to the crosswalk JSON.)

Two cases that look like "already handled" but aren't:

- **Org name already present under a *different* spelling.** By definition every worklist item
  failed the deterministic match, so its exact/normalized string is **not** a crosswalk node —
  even when the org clearly exists under another spelling. Do **not** drop it as "already done."
  It's a **valid crosswalk-add**: the RA adds *this exact leginfo spelling* as an
  `alternate_spelling` (or chapter) of the existing canonical, so the spelling is preserved and
  its bill count can be attributed in step 4.
- **Accidental prose found *in* the crosswalk.** If, while searching, you discover a node that
  is itself narrative prose wrongly added as an org (e.g. a node literally named "we strongly
  support this bill"), it becomes an **RA task to DELETE that node from
  `2_webapp/org_clusters_crosswalk.json`.** It is **deleted, never moved to a routing CSV** — a
  CSV entry could later be matched as if it were a real org. (Diagnosis agents surface these via
  a `delete_from_crosswalk` field; the scanner files the deletion task.)

The routing CSVs in `org_names_for_cleaning/`:

| File | What goes here |
|------|---------------|
| `org_names_that_are_actually_individuals.csv` | a person's name with no identifiable leadership-org |
| `org_names_partial.csv` | fragments, generic single words (`Author`, `County`, `Union`…), `N individuals` placeholders |
| `org_names_invalid.csv` | not an org at all (bill text, vote tallies, procedural text, dates, phone numbers) |
| `orgs_added_to_crosswalk.csv` | valid orgs added to the crosswalk (tracking) |

> **Leadership ≠ individual.** "Person, Org" where the person is a Mayor / President /
> Director-of-whole-org / CEO / Chief / Sheriff / Chair is an **alternate spelling of the
> org**, not an individual. Personal businesses (e.g. "Sonya Yruel Photography") are real
> orgs too.

**Valid → a crosswalk-add task.** Whatever survives triage is a genuine, single organization.
The scan does **not** place it — that's standard RA work. Searching the crosswalk first,
choosing the right hierarchy level (alternate spelling / chapter / alternate spelling of a
chapter), and creating a new canonical only when the org is genuinely absent are already
spelled out in the **General Crosswalk Workflow Principles** and **Worker RA Role** in
`CLAUDE.md` — which every fleet RA reads before starting. So the directive is simply *add this org (with its count) to the crosswalk*, and the RA does the placement per those rules.

**One bundled task per batch.** All of a batch's valid orgs go into a **single** RA task, not
one task per org. This is deliberate: the bottleneck is *write access* (the data write queue),
not commits, so the RA grabs the queue **once**, adds every valid org in the batch, and commits
**once** with a message that **enumerates every change** — deferring the clean/dedup/stats
pipeline to step 3. Two leginfo-specific notes for the task:

- **Disambiguate before merging.** A fuzzy/string resemblance is a hint, not a decision; do
  web research when needed to confirm what the org actually is (acronym ↔ full name, e.g.
  `ACLU` ↔ `American Civil Liberties Union`; aka / former name / merger / parent) before
  merging it onto an existing canonical.
- **Track it.** Append a row to `org_names_for_cleaning/leginfo_added_to_crosswalk.csv`
  (`org_name,bills_supported`) so we record what was incorporated.

---

## 3. Finalize (batched, once)

The step-2 tasks already made their crosswalk, CSV, and source edits; they only deferred the
heavy pipeline. Once the fleet has drained all the step-2 tasks, run it **once**:

```bash
python3 scripts/clean_crosswalk.py        # apply cleaning regexes, dedup, merge identical canonicals
python3 scripts/regenerate_org_subsets.py # re-check names against crosswalk, redistribute CSVs
python3 generate_stats.py                 # update stats.json
```

Then commit — stage **only** the files that changed. Never `git add -A`.

---

## 4. Apply source rewrites & fill the canonical columns

This is the **single pass over `leginfo_metadata.csv`** — the only time the source file is rewritten. **It is run once by the import driver (the scan/management side), not a worker RA —
no RA task ever touches `leginfo_metadata.csv`.** Stream it once, and for each cell:

1. Apply the prose/conjoined cell rewrites the scanner recorded in step 2 (so prose becomes the
   organization(s) it was describing and conjoined strings become their split `;`-list).
2. Clean each org (the same regexes — no cleaned value was saved earlier) and match it against
   the finalized crosswalk.
3. Write the matched **canonical name(s)** into the corresponding column:

| Org column | New canonical-name column |
|------------|---------------------------|
| `support` | `support_canonical` |
| `opposition` | `opposition_canonical` |
| `opposition_unless_amended` | `opposition_unless_amended_canonical` |
| `support_with_amendments` | `support_with_amendments_canonical` |
| `sponsor` | `sponsor_canonical` |

**Deduplicate within each cell.** Before writing, drop duplicate canonicals inside a single
cell — multiple Leginfo orgs in the same cell can resolve to the same canonical (e.g. a
conjoined entry that was split into two orgs which share one canonical, or several locals
of the same union). Each canonical should appear at most once per cell.

> **Canonicals only.** These columns hold the crosswalk **canonical name**, never the
> literal Leginfo org string. After this pass, the canonical columns are a complete,
> deduplicated view of who supported/opposed each bill — every org that exists in the
> crosswalk is represented by its canonical name.
