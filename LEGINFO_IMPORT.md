# Importing Data from Leginfo Metadata

A runnable playbook for pulling organization names out of the Leginfo bill metadata,
resolving them against the crosswalk, and routing whatever's left to the right place.

The goal: every org that supported or opposed a bill in Leginfo ends up either (a) in
the crosswalk at the correct hierarchy level, or (b) in the correct `org_names_*.csv`
invalidity file in `org_names_for_cleaning/`. No org string is ever discarded.

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
  metadata, narrative prose, multiple orgs mashed together).
- **Destination:** the crosswalk JSON (`2_webapp/org_clusters_crosswalk.json`) plus the
  routing CSVs in `org_names_for_cleaning/`.

See `CLAUDE.md` ("General Crosswalk Workflow Principles") and
`org_names_for_cleaning/README.md` for the rules this playbook builds on.

---

## 1. Resolve narrative text with AI

Some org cells contain prose instead of a clean org name (e.g.
"In support of the bill, the California Hospital Association writes…"). Narrative text
appears in all five org columns (worst in `opposition_unless_amended` at ~23%, least in
`sponsor` at ~0.4%).

**This is not a script.** There is no lookup table or automated matching. Subagents
individually read each `;`-separated item from the five org columns and classify it:

- **Clean org name** → copy as-is into the `narrative_orgs` column.
- **Narrative text with extractable org(s)** → extract the real org name(s) and write
  those into the `narrative_orgs` column. Do not use the narrative prose itself.
- **Narrative text with no extractable org** → record `None parsed` in the
  `narrative_orgs` column.

The `narrative_orgs` column uses ` || ` as a separator between entries. Each entry is
tagged with its source column (e.g. `support: California Hospital Association`).

The original org columns are **never modified** — they stay exactly as the parser
produced them. The narrative prose is never reused as an alt spelling; only the parsed
org name flows forward via `narrative_orgs`.

---

## 2. Pull the orgs

Extract and clean every org name from the five org columns (the four stance columns plus
`sponsor`) **and the new `narrative_orgs` column**, and check each against the crosswalk.

```bash
python3 extract_org_names.py
```

What it does:
- Reads `leginfo_metadata.csv` line by line (the file is large — streamed, not loaded).
- For each of the five org columns plus `narrative_orgs`, splits the cell on `;`.
- Cleans each name with the regex patterns in `cleaning_patterns.txt` (strips trailing
  metadata like dates, positions, counts).
- Checks each cleaned name against the crosswalk.
- **Filters out already-routed orgs** — compares against all existing `org_names_*.csv`
  files in `org_names_for_cleaning/` (invalids, individuals, partials, conjoined, etc.).
  Orgs already in one of those files are skipped (not re-added to `not_in_crosswalk.csv`).
- **Updates counts** in existing CSVs — for any org that appears in both leginfo and an
  existing routing CSV, replaces the old count with the new leginfo count.
- Writes `org_names_import_summary.csv` with status for every org: `in_crosswalk`,
  `already_routed` (with which CSV), or `unmatched`.
- **Routes genuinely new unmatched orgs** into
  `org_names_for_cleaning/org_names_not_in_crosswalk.csv` so they can be resolved.

---

## 3. See what's already in the crosswalk

In this step we just **identify** which orgs already match the crosswalk and which don't.
For each of the five org columns, match every org against the crosswalk. The five
canonical columns we're working toward (filled later, in step 6) are:

| Org column | New canonical-name column |
|------------|---------------------------|
| `support` | `support_canonical` |
| `opposition` | `opposition_canonical` |
| `opposition_unless_amended` | `opposition_unless_amended_canonical` |
| `support_with_amendments` | `support_with_amendments_canonical` |
| `sponsor` | `sponsor_canonical` |

**Keep track of the orgs that did not match** — record every unmatched org (with its
source column and count) so nothing is lost. These are the input to step 4. The matched
orgs need no further work beyond being noted as matched.

---

## 4. Resolve the remaining orgs

The orgs that did **not** match the crosswalk in step 3 are now in
`org_names_for_cleaning/org_names_not_in_crosswalk.csv` (placed there by
`extract_org_names.py`). Each one either gets **added to the crosswalk** or it belongs in
one of the `org_names_*.csv` invalidity files in `org_names_for_cleaning/`.

> **No shortcuts.** Examine every org individually and confirm it's genuinely the same
> organization before merging — a fuzzy/string resemblance is a hint, not a decision.

Work in this order.

### 4a. Partials and conjoined entries first

Before anything else, check each unmatched org for two cases that need to be resolved into
real org name(s):

- **Conjoined** — one string is actually multiple organizations combined (e.g. "Sierra
  Club Planning and Conservation League"). Split it into its individual orgs.
- **Partial** — a truncated/fragment name (e.g. "California Coalition for"). If the full
  org name is unambiguous (search the crosswalk and the web), resolve it to that full org.
  Only route to `org_names_partial.csv` if it's truly ambiguous after both searches.

Once a partial or conjoined entry is resolved into one or more real organizations:

1. **Fix the source `leginfo_metadata.csv`** so each resolved org is a separate
   `;`-separated entry in the source cell (mirroring how the narrative step writes back to
   the source).
2. Move the original partial/conjoined string (with its count) into
   `org_names_for_cleaning/org_names_partial.csv` or `org_names_conjoined.csv`.
3. Each resolved org then flows through step 4b / 4c like any other org — search the
   crosswalk first; most are already present.

### 4b. Invalid orgs → CSV

Handle the invalid (non-org) strings in two passes:

1. **Filter out already-routed entries.** Compare the remaining list against **all** existing
   entries in the `org_names_*.csv` files in `org_names_for_cleaning/`. Drop anything that
   already matches one — it's already routed, so remove it from the working list (don't
   add a duplicate).
2. **Route newly-found invalids.** Of what's left, identify any string that isn't a real
   organization, append it to the correct CSV (each `org_name,bills_supported`), and filter
   it out of the working list too.

The routing CSVs in `org_names_for_cleaning/`:

| File | What goes here |
|------|---------------|
| `org_names_that_are_actually_individuals.csv` | a person's name with no identifiable leadership-org |
| `org_names_partial.csv` | fragments, generic single words (`Author`, `County`, `Union`…), `N individuals` placeholders |
| `org_names_invalid.csv` | not an org at all (bill text, vote tallies, procedural text, dates, phone numbers) |
| `org_names_that_start_with_parens.csv` | names starting with parentheses |
| `org_names_conjoined.csv` | multiple orgs joined together |
| `leginfo_added_to_crosswalk.csv` | valid orgs added to the crosswalk (tracking) |

> **Leadership ≠ individual.** "Person, Org" where the person is a Mayor / President /
> Director-of-whole-org / CEO / Chief / Sheriff / Chair is an **alternate spelling of the
> org**, not an individual. Personal businesses (e.g. "Sonya Yruel Photography") are real
> orgs too.

### 4c. Valid orgs → crosswalk

1. **Judgment check first.** Before anything else, use judgment to confirm the string
   actually looks like a real organization. If it doesn't, route it to an invalid CSV
   (step 4b) — or just let it drop if it was already handled there. Only continue with the
   steps below for strings that plausibly are real orgs.
2. **Search the crosswalk first** — it's most likely already there, possibly under a very
   different string (acronym ↔ full name, e.g. `ACLU` ↔ `American Civil Liberties Union`).
   Try acronym/full-name swaps and word reorderings.
3. **If the crosswalk search comes up empty or ambiguous, web-search the org** to learn
   what it actually is — its full name, acronyms, any other names (aka / former name /
   merger / parent org). Then **re-search the crosswalk** using those alternate names
   before concluding it's not there.
4. Add it at the **correct hierarchy level**:
   - **alternate spelling** of an existing canonical (OCR typo, abbreviation, punctuation
     variant),
   - **chapter** of a canonical (location/regional variant), or
   - **alternate spelling of a chapter**.
5. Only **create a new canonical** if the org genuinely isn't anywhere in the crosswalk
   after both searches (this is rare).
6. Append a row to `org_names_for_cleaning/leginfo_added_to_crosswalk.csv`
   (`org_name,bills_supported`) so we track what was incorporated.

---

## 5. Pipeline & commit

After editing the crosswalk JSON and CSVs, run the pipeline in this order:

```bash
python3 scripts/clean_crosswalk.py        # apply cleaning regexes, dedup, merge identical canonicals
python3 scripts/regenerate_org_subsets.py # re-check names against crosswalk, redistribute CSVs
python3 generate_stats.py                 # update stats.json
```

Then commit — stage **only** the files you changed (crosswalk JSON, the `org_names_*.csv`
files in `org_names_for_cleaning/`, `stats.json`). Never `git add -A`. One unit of work
per commit.

---

## 6. Fill the canonical columns

This is the single pass that populates the canonical columns — for **every** org, both the
ones that already matched in step 3 and the ones resolved/added in step 4.

Re-match every org in the five org columns against the finalized crosswalk and write
the matched org's canonical name into the corresponding column — `support_canonical` /
`opposition_canonical` / `opposition_unless_amended_canonical` /
`support_with_amendments_canonical` / `sponsor_canonical`.

**Deduplicate within each cell.** Before writing, drop duplicate canonicals inside a single
cell — multiple Leginfo orgs in the same cell can resolve to the same canonical (e.g. a
conjoined entry that was split into two orgs which share one canonical, or several locals
of the same union). Each canonical should appear at most once per cell.

> **Canonicals only.** These columns hold the crosswalk **canonical name**, never the
> literal Leginfo org string. After this pass, the canonical columns are a complete,
> deduplicated view of who supported/opposed each bill — every org that exists in the
> crosswalk is represented by its canonical name.

