# Leginfo Resolution Scan — Resume Guide

Everything needed to resume the leginfo import **step 2 (resolution scan)** lives in this
directory (`leginfo_scan_state/`), all committed. A fresh Claude session needs only this repo.

## What this is
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

## The loop (one chunk). Run SANDBOXED so `$TMPDIR=/tmp/claude-501` stays consistent.
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
- **Step 4** one pass over `leginfo_metadata.csv`: apply `rewrites.tsv` + fill `*_canonical` columns.
