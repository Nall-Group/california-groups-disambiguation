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

> ### ⚠️ Nothing needs resetting — the run is idempotent
>
> **Note (retired 2026-07-17):** `org_names_not_in_crosswalk.csv` is a **transient handoff
> only** — not a committed, persistent artifact. `extract_org_names.py` writes it, the
> resolution scan (step 2) drains it to the definite buckets, and when empty it is left
> header-only. `regenerate_org_subsets.py` no longer manages it (it reconciles only the
> permanent invalidity buckets), and `generate_stats.py` treats a missing file as 0.
>
> **Update (2026-07-27): the manual truncate step is gone.** `extract_org_names.py` used to
> *append* to the worklist, so a re-run stacked a second unmatched pile on top of the first
> and you had to blank the file by hand beforehand. It now **rewrites the worklist from
> scratch every run** (deduped, atomic temp-file + replace), because the unmatched pile is a
> pure function of (leginfo source, crosswalk, routing CSVs). Re-running with a previous
> worklist still on disk reproduces it byte-for-byte. The worklist is also no longer counted
> among the permanent routing buckets, so last run's still-undiagnosed items can never be
> mistaken for `already_routed` and dropped.
>
> **Nothing else needs resetting either.** The other routing CSVs (`invalid`, `partial`,
> `individuals`, …) are **overwritten in place idempotently** — the script
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
  files in `org_names_for_cleaning/` (invalids, individuals, partials, etc.) **plus the two
  mapping files** (`narrative_text_mapping_to_orgs.csv`, `conjoined_text_mapping_to_orgs.csv`,
  matched on their source-string column). Orgs/strings already in one of those are skipped
  (not re-added to `not_in_crosswalk.csv`).
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
  through the checks below. **If the prose resolved to at least one org, append the pairing to
  `org_names_for_cleaning/narrative_text_mapping_to_orgs.csv`** (`narrative_text,mapped_org` —
  the raw prose string and the org it resolved to). This is the one place the prose string is
  preserved: it is **never** kept in the crosswalk and **never** reused as an alt spelling, but
  recording it here means a later re-import recognizes the exact prose as already-diagnosed
  (step 1 marks it `already_routed`) and attributes its bill count to `mapped_org` in step 4
  instead of re-reading it from scratch. `mapped_org` may name **several orgs, `;`-separated**,
  when the prose credits more than one; step 4 splits the count across them. **`mapped_org` is
  never blank** — if the prose names
  **no** organization, it does **not** go in this file at all; route the original string to
  `org_names_invalid.csv` (`org_name,count`) so it's counted as a non-org and nothing else.
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
  "Sierra Club Planning and Conservation League", or a **lost-separator straddle** where the
  extractor dropped a `;`/line-break between two adjacent supporter-list entries, e.g.
  `Mayor of San Leandro, City of Albany`). Split it into its individual orgs and feed each back
  through this triage. Then:
  - **A component is NOT yet in the crosswalk** -> add it (as a crosswalk-add) so its
    support/opposition isn't lost.
  - **Record the split** in `org_names_for_cleaning/conjoined_text_mapping_to_orgs.csv` (schema
    `conjoined_text,mapped_orgs`: the raw fused string + a ` ; `-separated list of its component
    org names). This is the analog of the narrative mapping — it preserves the fused string
    (never discarded), skips it on re-import (col 0 -> `already_routed`), and lets **step 4
    rewrite the cell to its components and split the bill count onto each**, so no count is lost.
    Every row's `mapped_orgs` names real orgs; a string that names none is `invalid`, not conjoined.

  **Standing rule for RAs (approved 2026-07-16, closes the Q32-Q49 "lost-separator" class):** a
  lost-separator conjoined straddle (extractor dropped a `;`/line-break between two adjacent
  supporter-list entries) -> split it, add any missing component, and record
  `conjoined_text,mapped_orgs` in `conjoined_text_mapping_to_orgs.csv`. **Handle it inline — do
  not block or file a question.** Confirm the split by grepping the source cell (alphabetical
  supporter-list order + a control row where a component appears standalone make the merge
  unambiguous). *(The old flat `org_names_conjoined.csv` bucket was retired 2026-07-16 and
  migrated into this mapping.)*
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
  support this bill", or "California Coalition of Travel Agents to improve the operation of…"),
  it becomes an **RA task to remove that node from `2_webapp/org_clusters_crosswalk.json`**,
  handled per the narrative-entry rules in `CLAUDE.md` ("Handling Invalid Entries") — the RA
  records it in `narrative_text_mapping_to_orgs.csv` if it maps to an org, or
  `org_names_invalid.csv` if it names none. It is **not** left in the crosswalk. (Diagnosis
  agents surface these via a `delete_from_crosswalk` field; the scanner files the task.)

The routing CSVs in `org_names_for_cleaning/`:

| File | What goes here |
|------|---------------|
| `org_names_that_are_actually_individuals.csv` | a person's name with no identifiable leadership-org |
| `org_names_partial.csv` | fragments, generic single words (`Author`, `County`, `Union`…), `N individuals` placeholders |
| `org_names_invalid.csv` | not an org at all (bill text, vote tallies, procedural text, dates, phone numbers) |

*(The `leginfo_added_to_crosswalk.csv` tracking log was retired 2026-07-17 — nothing consumed it; orgs added to the crosswalk live in the crosswalk JSON, and their bill counts are re-derivable from the source + mapping files.)*

Plus one file that is **not** a standard `org_name,count` bucket:

| File | Schema | What goes here |
|------|--------|---------------|
| `narrative_text_mapping_to_orgs.csv` | `narrative_text,mapped_org` | Narrative-prose strings (no real org name of their own) paired with the org each describes. **Every row maps to a real org — `mapped_org` is never blank; prose that names no org goes to `org_names_invalid.csv`, not here.** **A row MAY name several orgs, `;`-separated** (`CMHDA ; CSAC`) — one prose sentence can credit more than one, and step 4 splits the bill count across them exactly as it does for the conjoined map. Read by step 1 only to mark the prose `already_routed`, and by step 4 to attribute the prose's bill count. Never redistributed by `regenerate_org_subsets.py`; never treated as a real org.

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

## 4. Resolve every org to its canonical & build the canonical twin

```bash
python3 scripts/build_canonical_columns.py            # full run
python3 scripts/build_canonical_columns.py --limit 300 --out "$TMPDIR/smoke.csv"   # smoke test
```

This is the **single pass over `leginfo_metadata.csv`**. **It is run once by the import driver
(the scan/management side), not a worker RA — no RA task ever touches `leginfo_metadata.csv`.**

> ### The output is a TWIN, not an in-place rewrite (changed 2026-07-27)
>
> This step used to be specified as rewriting the source in place — adding the canonical
> columns *and* replacing each org cell's text with the resolved org names. It now streams the
> pristine source to a **twin file** (default `leginfo_metadata_canonical.csv`, beside the
> source) and copies the original org columns through **unchanged**; only the five new
> `*_canonical` columns hold resolved names. Why:
>
> - The source is a tracked file in a **separate repo** (`Nall-Group/leginfo`) that isn't ours
>   to mutate.
> - Rewriting cells **destroys the original supporter text** — the very context step 2's
>   diagnosis agents grep the source for. Once a prose cell has become an org name, a future
>   re-diagnosis has nothing left to read.
> - A twin keeps step 4 **re-runnable**: when the crosswalk improves, regenerate from the
>   pristine source instead of trying to re-resolve already-resolved cells.
>
> The script refuses to write to the source path, and refuses a source that already has
> canonical columns. Cost: ~800 MB of disk for the twin.

Stream the source once, and for each `;`-separated part of each org cell:

1. Apply the prose/conjoined mappings — the two persistent files
   `narrative_text_mapping_to_orgs.csv` and `conjoined_text_mapping_to_orgs.csv`, plus the
   per-run `leginfo_scan_state/rewrites.tsv` ledgers (which only fill gaps; a curated mapping
   row always wins). Any part whose normalized text matches a mapping row's source string
   resolves to that row's mapped org(s) (`mapped_org` for narrative, the `;`-list in
   `mapped_orgs` for conjoined). This is what carries a *previously diagnosed* prose/conjoined
   string's bill count onto the real org(s) — splitting a conjoined string's count across
   **all** its components — without the scan having to re-read it. (A blank narrative
   `mapped_org` means the prose named no org: drop the part, count nothing.)
2. Clean each org (the same regexes — no cleaned value was saved earlier) and match it against
   the finalized crosswalk. Every node name in a cluster — the canonical plus all descendants at
   any depth — resolves to that cluster's **canonical**.
3. Write the matched **canonical name(s)** into the corresponding new column:

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

**Read the run's closing report — it is the audit.** The script classifies every part it
could not resolve to a canonical:

- *known non-orgs* — the part matches a routing CSV (`invalid` / `individuals` / `partial`).
  It is **supposed** to resolve to nothing; not a loss.
- *UNACCOUNTED FOR* — the part is neither in the crosswalk nor routed anywhere. **Every one of
  these silently loses its bill count**, so this number should be at or near zero. The report
  lists the top offenders by frequency; each is either a missing alt spelling (add it to the
  crosswalk) or a bad conjoined split (fix the component list in the mapping file), then re-run.

Because the mapping files feed this step, a conjoined row whose `mapped_orgs` names an org that
isn't in the crosswalk drops that component's share of the count. Check for those **before**
running step 4 — see the orphaned-component check in the crosswalk-gap initiative.
