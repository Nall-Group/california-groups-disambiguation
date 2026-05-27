# Leginfo Gap Incorporation — RA Protocol

This folder is the workspace for incorporating the **58,812 leginfo support-orgs that the
crosswalk failed to match** when the crosswalk was applied to `leginfo_metadata.csv` to build
the co-support graph. Every task numbered "leginfo gap" references this file.

## ⚠️ NO SHORTCUTS — examine every org individually

Supervisor directive: **do NOT bulk-process or rubber-stamp these.** Look at each individual org
string, one at a time, and work with it deliberately:
- Read the actual `leginfo_org` / `org_name` value and think about what it really is.
- Find and inspect the real candidate entry in the crosswalk before merging — confirm it is genuinely
  the same organization, not just a high fuzzy score. **This applies even to the ≥97 band.**
- A high WRatio is a hint, not a decision. False positives exist at every confidence level.
- When uncertain, search the crosswalk and the web before acting; if still unsure, mark the task
  Blocked and post a question (per CLAUDE.md) rather than guessing.

## Worklist files (all sorted by `bills_supported` DESC — high-impact first)

| File | Rows | Columns |
|------|------|---------|
| `leginfo_gaps_all.csv` | 58,812 | `org_name, bills_supported` — every unmatched org (master) |
| `leginfo_gap_candidates.csv` | 28,601 | candidates w/ fuzzy match (master) |
| `leginfo_cand_ge97.csv` | 3,557 | candidates, WRatio ≥ 97 (near-certain) |
| `leginfo_cand_90to97.csv` | 24,314 | candidates, WRatio 90–97 (probable) |
| `leginfo_cand_75to90.csv` | 730 | candidates, WRatio 75–90 (low-confidence) |
| `leginfo_novel.csv` | 30,211 | no fuzzy candidate (real-org judgment / junk) |

Candidate columns: `leginfo_org, leginfo_bills, suggested_canonical, matched_name_in_crosswalk, matched_via, wratio, token_set_ratio`. `matched_via` ∈ `canonical` / `alt(alternate_spelling)` / `alt(chapter)`.

## Where things go (supervisor decision)

- **Real organizations → the crosswalk JSON** (`2_webapp/org_clusters_crosswalk.json`), placed at
  the correct hierarchy level (alternate_spelling / chapter / new canonical). Also append a row to
  `org_name_for_cleaning/leginfo_added_to_crosswalk.csv` (`org_name,bills_supported`) so we can track
  what was incorporated.
- **Non-orgs → separate CSVs IN THIS FOLDER** (NOT the original `org_name_subsets_for_cleaning/`),
  each `org_name,bills_supported`. Create the file if it doesn't exist yet:
  - `leginfo_individuals.csv` — a person's name (no identifiable leadership-org)
  - `leginfo_partial.csv` — fragments / generic single words / "N individuals" placeholders
  - `leginfo_invalid.csv` — not an org at all (bill text, vote tallies, procedural text)
  - `leginfo_conjoined.csv` — multiple orgs mashed together
  - `leginfo_not_capitalized.csv`, `leginfo_narrative.csv`, `leginfo_dates_phones.csv`, `leginfo_starts_with_parens.csv` — as in CLAUDE.md
  This keeps leginfo `bills_supported` counts separate from the original crosswalk source counts.

## General principles (see CLAUDE.md "General Crosswalk Workflow Principles")

1. **Always search the crosswalk first** — the org is most likely already present, possibly under a
   very different string (acronym ↔ full name, e.g. `ACLU` ↔ `American Civil Liberties Union`;
   `SEIU` ↔ `Service Employees International Union`). Only create a new canonical if it genuinely
   isn't anywhere.
2. **Correct hierarchy level** — alternate_spelling vs chapter vs alt-of-a-chapter (children nest).
3. **Preserve names; route, don't discard.** Even junk org-strings get a row in a `leginfo_*.csv`.
4. Location suffixes may be chapter info. Leadership roles (Mayor/President/Director-of-whole-org/
   CEO/Chief/Sheriff/Chair) make a "Person, Org" string an alt of the org, not an individual.

## Per-band handling

### Candidate bands (`leginfo_cand_ge97 / 90to97 / 75to90`)
For each row, the `leginfo_org` is a **suggested** variant of an existing crosswalk entry. **Examine
each org individually and confirm it is truly the same org** — no bulk/mechanical merging at ANY
confidence, including ≥97 (see the NO SHORTCUTS section above). Look at the actual string, find the
real entry in the crosswalk, decide deliberately:

- `matched_via=canonical`: find that canonical in the JSON. **If the leginfo spelling is the clean/
  correct one and the existing canonical is OCR-corrupted** (e.g. canonical `Cailfornia Federation of
  Teachers`, leginfo `California Federation of Teachers`) → **rename the canonical to the leginfo
  spelling and demote the old name to an `alternate_spelling` child.** Otherwise add `leginfo_org`
  as an `alternate_spelling` under that canonical.
- `matched_via=alt(alternate_spelling)`: add `leginfo_org` as another `alternate_spelling` under the
  same canonical (sibling of `matched_name_in_crosswalk`).
- `matched_via=alt(chapter)`: add `leginfo_org` as an `alternate_spelling` nested under that chapter,
  or as its own `chapter` if it's a distinct location.
- **False positive** (not actually the same org): handle it as a novel entry (below).

### Novel set (`leginfo_novel.csv`)
No fuzzy candidate. For each org:
1. **Search the crosswalk** (try acronym↔full-name, word reorderings). If found → add at correct level.
2. Else **web-search** to identify. If a real org → add as a **new canonical** at correct hierarchy.
3. Else **route to a `leginfo_*.csv`**: `Numerous/N/An individuals` placeholders → `leginfo_partial.csv`;
   single generic words (`Author`, `County`, `Association`, `CIO`, `Union`…) → `leginfo_partial.csv`;
   real person names → `leginfo_individuals.csv`; multiple orgs → `leginfo_conjoined.csv`; etc.

## Pipeline & commit (per task)
After editing the crosswalk JSON, run in order:
`python scripts/clean_crosswalk.py` → `python scripts/regenerate_org_subsets.py` → `python generate_stats.py`.
Then `git add` ONLY the files you changed (crosswalk JSON, the `leginfo_*.csv` you wrote, `stats.json`,
and any regenerated original subset CSVs) — never `git add -A`. One task per commit.
