# Crosswalk Gaps Processing — STATUS & RESUME GUIDE

> **A fresh session with zero context can resume from this file alone.** Read it top to bottom.

## What this program is
`crosswalk_gaps_all_stances.csv` = ~67,609 org names that appear in CA legislative bill-position
data but are NOT cleanly in the crosswalk (`2_webapp/org_clusters_crosswalk.json`). Goal: triage
EVERY org, decide its disposition, and group the results so worker RAs can insert/route them.
The management assistant does the triage (via parallel sub-agents); RAs apply the results to the
crosswalk and delete the handled rows from `crosswalk_gaps_all_stances.csv`.

## THE CURSOR (where we are)
- **`gaps_needs_audit.txt`** = the work queue, sorted by bill count (priority). One org per line.
  Whatever is in this file is NOT yet triaged. Currently: **7,962 remaining**.
- Each completed wave removes its orgs from this file (the cursor advances). When it hits 0, done.
- **Next wave number to use: 124.** (Waves 1–123 are committed. Wave numbers only label the temp
  chunk files; they don't matter beyond avoiding filename collisions — just use the next integer.)

## THE OUTPUT (accumulating worklists, in repo root)
Each wave's dispositions are appended here. Standing RA tasks 1242–1245 consume these:
| File | Meaning | RA task |
|---|---|---|
| `gaps_master_consolidate.tsv` | `org \t target_canonical \t group` — fold org under existing canonical | 1243 |
| `gaps_master_new_canonicals.txt` | real orgs to add as new canonicals | 1244 |
| `gaps_master_new_chapters.tsv` | `org \t parent` — add as chapter | 1244 |
| `gaps_master_individuals.txt` | → org_names_that_are_actually_individuals.csv | 1245 |
| `gaps_master_partial.txt` | → org_names_partial.csv | 1245 |
| `gaps_master_conjoined.txt` | split + → org_names_conjoined.csv | 1245 |
| `gaps_master_narrative.txt` | → org_names_embedded_in_narrative_text.csv (extract org) | 1245 |
| `gaps_master_parens.txt` | → org_names_that_start_with_parens.csv | 1245 |
| `gaps_master_dates.txt` | → org_names_that_are_dates_or_phone_numbers.csv | 1245 |
| `gaps_master_not_capitalized.txt` | → org_names_not_capitalized.csv | 1245 |
| `gaps_master_invalid.txt` | → org_names_invalid.csv | 1245 |

(Wave 1 used a separate `gaps_wave1_worklist.txt` → tasks 1239–1241. The bulk pass → tasks
1224–1226. Acronym matches → `gaps_acronym_matches.txt` → task 1242.)

## PIPELINE SCRIPTS (committed, repo root)
- **`_agg.py wNN`** — rebuilds ALL master files from every `$TMPDIR/w*_*.out` (idempotent;
  auto-separates narrative/parens/dates/etc.), then removes wave wNN's orgs from
  `gaps_needs_audit.txt`. Run once per wave after the agents finish.
- **`_rebuild_masters.py`** — full rebuild of masters from all `.out` files (retroactive
  re-bucketing; does NOT touch the cursor).

## HOW TO RESUME (exact recipe) — safe in a brand-new session, from repo root
1. Build the next wave's chunks (use the next wave number, e.g. 35):
   ```
   head -480 gaps_needs_audit.txt > $TMPDIR/gaps_wave35.txt
   split -l 60 -d $TMPDIR/gaps_wave35.txt $TMPDIR/w35_
   ```
2. Dispatch 8 parallel sub-agents (Agent tool, general-purpose), one per chunk `w35_00`..`w35_07`.
   Each agent: for each org, grep the crosswalk for it + variants, assign a disposition, and WRITE
   to `$TMPDIR/w35_<chunk>.out` as TAB-separated `org<TAB>DISP<TAB>TARGET<TAB>GROUP`.
   DISP ∈ {CONSOLIDATE, NEW_CANONICAL, NEW_CHAPTER, INVALID:individuals|partial|conjoined|invalid|
   not_capitalized|narrative|parens|dates_phone}. (Reuse the wave-34 agent prompt verbatim.)
   KEY RULE: many entries ARE real orgs even in noisy bands (acronyms, "X Association of California")
   — do NOT lazily mark real orgs invalid.
3. Aggregate + commit:
   ```
   python3 _agg.py w35
   git add gaps_master_*.* gaps_needs_audit.txt && git commit -m "Gaps wave 35 aggregated"
   ```
4. Repeat with wave 36, 37, … until `gaps_needs_audit.txt` is empty.

## NOTES / GOTCHAS
- The file is NOT cleanly value-sorted: same-bill-count orgs fall in alphabetical clumps, so you
  WILL hit localized bands that are ~all bill-refs ("AB 1234"), ~all "and …" narrative fragments,
  or ~all "(acronym)" parens. Expected — they route to invalid/narrative/parens. Real orgs are
  mixed throughout (incl. the low-bill tail), so keep auditing; a band of junk does NOT mean the
  rest is junk. (I wrongly assumed that once and had to be corrected.)
- Git commits may hit `index.lock` from concurrent worker RAs — retry the commit a few times.
- `$TMPDIR` is ephemeral; only committed files (masters + cursor + this doc + scripts) survive a
  /clear. In-flight `.out` files are lost on clear but their orgs are still in the cursor, so they
  are simply re-audited — no corruption.

## PROGRESS
- Done: bulk pass (tasks 1224–1226), 112 acronym matches (1242), wave 1 (1239–1241),
  waves 2–123 → masters above. ~59,650 of 67,609 orgs triaged (~88%).
- Remaining: 7,962 in `gaps_needs_audit.txt`.

## STANDING-TASK SLICING (added wave 118)
The 3 standing tasks 1243/1244/1245 are split into 60 band sub-tasks (1246–1305) via
`gaps_band.py` (committed). Each band = a content-stable alphabetical slice, sized to
per-row effort, filtered to still-pending orgs (auto-shrinks as RAs apply, auto-regrows
as new waves append). RAs: `python3 gaps_band.py list <category>` to find work, then
`python3 gaps_band.py <category> <n>`. This does NOT change the audit loop above.
