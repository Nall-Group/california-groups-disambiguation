# Leginfo Import / Resolution Scan — Resume Guide

Everything needed to run or resume the leginfo import **step 2 (resolution scan)** lives in this
directory (`leginfo_scan_state/`), all committed. A fresh Claude session needs only this repo.

## Run status

- **Run 2 (current) — started 2026-07-27.** New source extraction:
  `/Users/ruthgracewong/leginfo/extract_all_leginfo_metadata/leginfo_metadata.csv`
  (796 MB, dated 2026-07-25). Starting from **step 1**.
- **Run 1 (2026-07-10 → 2026-07-17)** — steps 1-2 ran; **step 4 never ran** (the source CSV
  still has no `*_canonical` columns). Its ledgers are archived in `archive_run1/`
  (`processed_run1.txt`, `rewrites_run1.tsv`). Nothing there needs re-reading for run 2's scan:
  every routing decision it made is already persisted in the permanent CSVs in
  `org_names_for_cleaning/` (invalid / partial / individuals + the two mapping files), which
  step 1 uses to mark strings `already_routed`.

## Starting a fresh run from step 1 (the reset)

Only these things reset. **Never reset** the permanent buckets in `org_names_for_cleaning/`
(`org_names_invalid.csv`, `org_names_partial.csv`,
`org_names_that_are_actually_individuals.csv`, `narrative_text_mapping_to_orgs.csv`,
`conjoined_text_mapping_to_orgs.csv`) — they are the accumulated diagnosis memory and are
updated idempotently in place.

The transient worklist (`org_names_not_in_crosswalk.csv`) needs **no** reset: as of
2026-07-27 `extract_org_names.py` rewrites it from scratch each run instead of appending,
so re-running reproduces it byte-for-byte.

```bash
# archive + clear the scan ledgers (skip-set and step-4 rewrites are per-run)
mkdir -p leginfo_scan_state/archive_runN
git mv leginfo_scan_state/processed.txt  leginfo_scan_state/archive_runN/processed_runN.txt
git mv leginfo_scan_state/rewrites.tsv   leginfo_scan_state/archive_runN/rewrites_runN.tsv
: > leginfo_scan_state/processed.txt
```

`next_batch_num.txt` is **not** reset — the batch counter stays monotonic across runs so batch
files and `[LEGINFO-CROSSWALK-ADD]` task names never collide with a previous run's. (Run 2
starts at batch 1530.)

Why `processed.txt` resets: it is a skip-set of item strings already *diagnosed*. Items whose
diagnosis was "invalid / individual / partial / prose / conjoined" are permanently remembered by
the CSVs above, so they never re-enter the worklist anyway. Items diagnosed "valid → add to
crosswalk" whose RA task never landed **should** be re-diagnosed — keeping the old skip-set would
silently drop them.

## Step 1 (deterministic match)

```bash
python3 extract_org_names.py
```
Rewrites counts in the routing CSVs and `org_names_import_summary.csv`, and fills
`org_names_for_cleaning/org_names_not_in_crosswalk.csv` with the genuinely-new unmatched pile.
It touches shared data files, so **hold the Data Write Queue** while it runs.

## What step 2 is
`LEGINFO_IMPORT.md` step 2: diagnose the unmatched leginfo org names in
`org_names_for_cleaning/org_names_not_in_crosswalk.csv` (the "worklist", count-sorted).
Parallel **Opus** sub-agents judge each item (org-name vs prose), triage it
(valid / already-in-crosswalk / invalid / individual / partial / conjoined), and give a
crosswalk placement. A driver routes invalid/individual/partial rows to the CSVs, records
prose/conjoined→org rewrites for step 4, and files RA tasks for valid crosswalk adds/deletes.

## Files here (all committed)
- `next_batches.py` — pulls the next K unprocessed batches (30 items each) off the worklist → writes batch CSVs to `$TMPDIR/leginfo_scan/batches/` + prints JSON.
- `leginfo_resolution_scan.js` — the **workflow script** (one Opus schema-agent per batch). Prompt has the crosswalk rules **inlined** (agents do NOT read docs). Model MUST be opus.
- `apply_results.py` — per-batch collector: routes CSVs, updates worklist + ledgers, has a 3-tier echo matcher (exact → normalized → containment-for-truncated-prose).
- `process_chunk.py` — runs the whole chunk: writes results, runs apply_results per batch, files `[LEGINFO-CROSSWALK-ADD]`/`[LEGINFO-CROSSWALK-DELETE]` tasks (joins TASKS.md Write Queue), commits.
- `processed.txt` — ledger of diagnosed item names (skip-set). `rewrites.tsv` — prose/conjoined→org, for step 4. `next_batch_num.txt` — batch counter.
- `archive_run1/` — run 1's ledgers, kept for provenance (see "Run status").

## PRIMARY: the auto-restarting daemon `scripts/run_scan.sh`
The scan normally runs UNATTENDED via this daemon — fleet-parity: it loops chunk-by-chunk,
diagnoses each batch with a parallel `claude -p` Opus worker (NOT the Workflow tool, which
needs a live session), processes via `process_chunk.py`, and **backs off + auto-resumes on
the spend limit**. It survives session close and limit resets, just like the RA fleet.
```
CONC=8 nohup scripts/run_scan.sh >ra_logs/scan.out 2>&1 & disown
nohup caffeinate -i -m -s -w $! >/dev/null 2>&1 & disown       # keep Mac awake, auto-release
```
(Launch needs `dangerouslyDisableSandbox:true` from inside a claude Bash tool — nested `claude -p`.)
- **Is it running / progressing?** `tail ra_logs/scan.log` (chunk lines + worklist count) and `git log --oneline` (fresh `Leginfo scan batches …` commits). Do NOT trust process-list checks under the sandbox.
- **Stop it:** `pkill -f run_scan.sh` (from a terminal / sandbox-off Bash).
- Knobs: `CHUNK_BATCHES`(40) `CONC`(8) `FATAL_BACKOFF`(300) `TASK_TIMEOUT`(1800) `MAX_CHUNKS`(0=until drained).
- **Do NOT run the daemon and the manual Workflow loop at the same time** — they'd double-pull batches. Pick one.

## FALLBACK / interactive: drive one chunk by hand via the Workflow tool.
Run SANDBOXED so `$TMPDIR=/tmp/claude-501` stays consistent.
Working scripts run from `$TMPDIR/leginfo_scan/` — first copy the committed ones there:
```
mkdir -p "$TMPDIR/leginfo_scan/batches" "$TMPDIR/leginfo_scan/results"
cp leginfo_scan_state/{next_batches,apply_results,process_chunk}.py "$TMPDIR/leginfo_scan/"
```
1. **Generate a chunk** (40 batches × 30 items = 1200):
   `python3 "$TMPDIR/leginfo_scan/next_batches.py" 40` → writes batch CSVs; build a specs
   array `[{batch, file:"$TMPDIR/leginfo_scan/batches/batch_NNNN.csv"}, ...]`.
2. **Diagnose** — call the Workflow tool:
   `Workflow({scriptPath: "<abs path to leginfo_scan_state/leginfo_resolution_scan.js>", args: <specs>})`.
   Runs 40 Opus agents (≈12 concurrent), ~9-14 min, ~1.5M tokens. When done, note the output file path.
3. **Process** — `python3 "$TMPDIR/leginfo_scan/process_chunk.py" <workflow_output.json>` →
   routes CSVs, files tasks, commits. Prints worklist-remaining.
Repeat until worklist = 0.

## Model rule (hard)
Diagnosis MUST run on **Opus** (`model:'opus'` in the .js). NEVER Sonnet. Quality > tokens.

## Spend-limit behavior
An account-wide **monthly spend limit** (claude.ai/settings/usage) stalls BOTH the scan and
the RA fleet. It does not clear by waiting a few min — raise it or wait for the monthly reset.
- Failed batches return `diagnoses:null`; their items stay in the worklist and auto-retry next chunk. Just re-run the loop when the limit clears.
- **Leave the fleet RUNNING** through a limit — `run_ra.sh` backs off and auto-resumes. (Only `fleet.sh stop` needs a manual restart; if you must stop it, clear orphan lines from BOTH TASKS.md queues afterward and reset orphaned "In Progress" tasks.)

## The RA fleet (parallel task drainer)
`scripts/fleet.sh {start|logs|errors|stop}` — 3 detached Opus `claude -p` RAs that work the
`[LEGINFO-CROSSWALK-ADD/DELETE]` tasks. Pair `start` with caffeinate:
`nohup caffeinate -i -m -s -w <launcher_pid> >/dev/null 2>&1 & disown`.
Launching fleet from inside a claude Bash tool needs `dangerouslyDisableSandbox:true`.

**Checking fleet status (IMPORTANT — do it this way):** there is deliberately NO
`fleet.sh status` command. Process checks (`kill -0`, `pgrep`, `ps`) are BLOCKED under the
Claude Bash sandbox and falsely report "not running." Judge the fleet by on-disk signals,
which work everywhere:
- **Alive?** `git log --oneline -5` — recent RA commits (e.g. "Task NNNN: add … (RA-Fleet-x)") = fleet is working.
- **Stalled?** `scripts/fleet.sh errors` or `tail ra_logs/errors.log` — recent `usage_limit` rows = blocked on the spend limit (fleet auto-resumes when it clears).
- Need a real process check anyway? Run `pgrep -fl run_ra.sh` with `dangerouslyDisableSandbox:true` (or from a plain terminal).
The fleet is detached OS processes independent of any Claude session — closing/reopening a
session doesn't affect it, and a new session sees it via the on-disk signals above.

## After the worklist drains
- **Step 3** finalize pipeline: `clean_crosswalk.py` → `regenerate_org_subsets.py` → `generate_stats.py`.
- **Step 4** `python3 scripts/build_canonical_columns.py` — one pass over `leginfo_metadata.csv`
  that applies the two persistent mapping CSVs (`narrative_text_mapping_to_orgs.csv`,
  `conjoined_text_mapping_to_orgs.csv`) plus the `rewrites.tsv` ledgers (this run's **and**
  `archive_run1/rewrites_run1.tsv`, since run 1's step 4 never ran), then writes the
  `*_canonical` columns. **Output is a TWIN** (`leginfo_metadata_canonical.csv`) — the source is
  never mutated and the original org columns are copied through unchanged; see LEGINFO_IMPORT.md
  step 4 for why. Driver-only — no RA task ever touches `leginfo_metadata.csv`.
  Check the closing report: *UNACCOUNTED FOR* parts are counts being silently dropped.
