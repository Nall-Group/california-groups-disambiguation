# Management Assistant — Handoff

**Generated:** 2026-06-03 (session handoff). Read this in conjunction with `CLAUDE.md` (defines the project rules) and `org_name_for_cleaning/README.md` (defines the leginfo-gap RA protocol).

## What this project is doing right now

The crosswalk-cleaning operation (827 historical tasks Done) has shifted into a new initiative: **incorporate 58,812 leginfo support-organisations that the crosswalk failed to match** when applied to `leginfo_metadata.csv` to build the co-support graph. Source-of-truth gap files live in the other repo at `/Users/vova/Downloads/leginfo-supportopposeparserfixes/extract_all_leginfo_metadata/` (`crosswalk_gaps_all.csv`, `crosswalk_gap_candidates.csv`) and were copied + split per-band into `org_name_for_cleaning/` for the RAs to work from.

## Current state (as of 2026-06-03 ~10:47)

### Tasks (leginfo-gap pool only — tasks 775–925)
| Status | Count | Notes |
|---|---|---|
| Done | **104 / 151** (69%) | |
| In Progress | 1 | task 879 (RA-Alpha) |
| Not Started | 46 | tasks scattered across remaining ≥97/90-97/novel slices |
| Blocked | 0 | |

### Orgs accounted-for: 9,433 / 58,812 = 16.0%

| Destination | Count |
|---|---|
| `leginfo_added_to_crosswalk.csv` (real org variants now in crosswalk JSON) | 4,178 |
| `leginfo_partial.csv` (fragments / generic words) | 2,365 |
| `leginfo_narrative.csv` (narrative-text fragments) | 1,268 |
| `leginfo_individuals.csv` (people, not orgs) | 1,095 |
| `leginfo_conjoined.csv` (multiple orgs mashed) | 377 |
| `leginfo_invalid.csv` (not orgs at all) | 150 |

### Open questions: 0
The most recent (Q9) was answered 2026-06-01 — officeholder policy ruling that "Office X, Person Y" bill-supporter strings go as `alternate_spelling` under the office canonical, NOT into individuals. Embedded in every Wave 3+ novel task description and in `org_name_for_cleaning/README.md`.

### ⚠️ RA-Beta is likely down
- **RA-Alpha:** last commit **2026-06-03 10:47** (mid-task on 879)
- **RA-Beta:** last commit **2026-06-02 17:15** (~17h silent at handoff time)

If you want full throughput, the user needs to relaunch RA-Beta. The Worker-RA launch prompt:

> *You are **RA-Beta** (or RA-Alpha). Read CLAUDE.md and follow the Worker RA Role instructions. Pick up the next available "Not Started" task from TASKS.md. Many leginfo-gap tasks (775+) are available — they reference `org_name_for_cleaning/README.md` for the protocol (note the **⚠️ NO SHORTCUTS** directive: examine each org individually).*

## Management Assistant role

You (MA) coordinate between the human supervisor and worker RAs. You **do NOT** do worker tasks. CLAUDE.md defines the full role; the operational responsibilities boil down to:

1. **Poll** `QUESTIONS.md` + `TASKS.md` every 10 s in background (Bash `md5 -q` diff of question-status lines + task-row lines). Wake on material changes only — ignore write-queue churn.
2. **Surface unanswered RA questions** (`Status: Open`) to the supervisor via `AskUserQuestion`, then write the answer back into `QUESTIONS.md` (Status → `Answered`).
3. **Report task status** with each material change.
4. **Add new tasks** the supervisor sends → `TASKS.md` as the next-numbered "Not Started" row.
5. **Replenish waves** of leginfo-gap tasks (15/wave) when the Not-Started buffer runs low so the RAs never idle.
6. **Crash recovery**: if an RA session is closed mid-task, reset orphaned "In Progress" rows back to "Not Started" and clear the RA from any write queues.

## File layout for this initiative

```
california-groups-disambiguation/
├── CLAUDE.md                          # Project rules. READ FIRST.
├── TASKS.md                           # The task queue. Tasks 775–925 are leginfo-gap.
├── QUESTIONS.md                       # RA questions inbox.
├── 2_webapp/org_clusters_crosswalk.json   # The crosswalk (shared write target).
├── org_name_for_cleaning/             # Leginfo-gap workspace.
│   ├── README.md                      # RA protocol — has the NO SHORTCUTS directive.
│   ├── leginfo_gaps_all.csv           # Master worklist (58,812 rows).
│   ├── leginfo_gap_candidates.csv     # Master candidates (28,601 rows).
│   ├── leginfo_cand_ge97.csv          # ≥97 band slice (3,557) — FULLY PROCESSED.
│   ├── leginfo_cand_90to97.csv        # 90–97 band slice (24,314) — partially.
│   ├── leginfo_cand_75to90.csv        # 75–90 band slice (730) — FULLY PROCESSED.
│   ├── leginfo_novel.csv              # Novel slice (30,211) — partially.
│   ├── leginfo_added_to_crosswalk.csv # Append-only log of incorporated orgs.
│   ├── leginfo_individuals.csv        # Routing CSV.
│   ├── leginfo_partial.csv            # Routing CSV.
│   ├── leginfo_invalid.csv            # Routing CSV.
│   ├── leginfo_conjoined.csv          # Routing CSV.
│   └── leginfo_narrative.csv          # Routing CSV.
└── org_name_subsets_for_cleaning/     # Original crosswalk-source CSVs — DO NOT mix.
```

## Three write queues — coordination protocol

| Queue | Location | Who writes |
|---|---|---|
| TASKS.md Write Queue | top of `TASKS.md` | MA + RAs (for status changes / task additions) |
| QUESTIONS.md Write Queue | top of `QUESTIONS.md` | MA + RAs (for posting Qs / writing answers) |
| Data Write Queue | inside `TASKS.md` | RAs only (for crosswalk JSON / CSV edits) |

**Protocol:** add your name to the bottom → wait until you're at the top → make edits → remove your name. MA may hold multiple queues simultaneously. **MA commits only `TASKS.md` and `QUESTIONS.md`** (and arguably the coordination README in `org_name_for_cleaning/`). RAs commit data files (`git add <specific files>` only — never `git add -A`).

**File-race pitfall when adding waves:** When two RAs are actively making queue / status edits, the Edit tool can fail with "modified since read" on retry. The workaround I used: atomic Python read-modify-write for queue join/leave + Bash `cat >> TASKS.md` for appending wave rows (safe because I'm holding the queue).

## Three policy decisions in effect

1. **Routing** (chosen 2026-05-27): Real leginfo orgs → crosswalk JSON; non-org leginfo strings → separate `leginfo_*.csv` files **inside `org_name_for_cleaning/`**, format `org_name,bills_supported`. **Do NOT mix** leginfo bill-counts into `org_name_subsets_for_cleaning/` (different metric).

2. **NO SHORTCUTS** directive (set 2026-05-27): RAs must examine every org **individually** — even ≥97 near-certain candidates. A high WRatio is a hint, not a decision; false positives exist at every confidence. Codified in `org_name_for_cleaning/README.md`'s "⚠️ NO SHORTCUTS" section. Holding up well: RA notes routinely document caught false-positive merges.

3. **Officeholder policy / Q9** (set 2026-06-01): "Office X, Person Y" bill-supporter strings (Attorney General Xavier Becerra, Governor Gray Davis, …) for the six statewide constitutional offices go as `alternate_spelling` under the office canonical, NOT into `leginfo_individuals.csv`. Embedded in every Wave 3+ novel task description. Memory: `~/.claude/projects/-Users-vova-Downloads/memory/officeholder-policy.md`.

## Wave plan + what's left

| Wave | Tasks | What it covers |
|---|---|---|
| 1 | 775–789 | Setup + initial slices in all four bands |
| 2 | 790–804 | Continuing |
| (805) | 805 | Q9 reclassification follow-up |
| 3 | 806–820 | Continuing — Wave 3 novel tasks first cite Q9 explicitly |
| 4 | 821–835 | **≥97 band closes out at task 823** |
| 5 | 836–850 | **75–90 band closes out at task 844** |
| 6 | 851–865 | 90–97 + novel only |
| 7 | 866–880 | 9× 90–97 (rows 5851–7200) + 6× novel (rows 3001–3600) |
| 8 | 881–895 | 9× 90–97 (rows 7201–8550) + 6× novel (rows 3601–4200) |
| 9 | 896–910 | 9× 90–97 (rows 8551–9900) + 6× novel (rows 4201–4800) |
| 10 | 911–925 | 9× 90–97 (rows 9901–11250) + 6× novel (rows 4801–5400) |

**After Wave 10 finishes:** 90–97 will be at row 11,250 of 24,314 (still 13,064 rows = ~87 tasks left). Novel will be at row 5,400 of 30,211 (24,811 rows = ~248 tasks left). **Roughly 22 more waves needed to finish everything** (~335 more tasks). Slice sizes can scale up if the user wants larger batches.

## How to resume as MA in a new session

1. `cd /Users/vova/Downloads/california-groups-disambiguation`
2. Read `CLAUDE.md` + `org_name_for_cleaning/README.md` + this file.
3. Get current snapshot:
   ```bash
   grep -c '^| [0-9]* |[^|]*| Done |' TASKS.md          # historical + leginfo Done count
   grep -c '\*\*Status:\*\* Open' QUESTIONS.md          # unanswered Qs
   wc -l org_name_for_cleaning/leginfo_added_to_crosswalk.csv
   ```
4. Start the 10-second material-change poll in background:
   ```bash
   QF=/Users/vova/Downloads/california-groups-disambiguation/QUESTIONS.md
   TF=/Users/vova/Downloads/california-groups-disambiguation/TASKS.md
   sig() { { grep '\*\*Status:\*\*' "$QF"; grep -E '^\| [0-9]+ \|' "$TF"; } | md5 -q; }
   base=$(sig)
   while true; do sleep 10; now=$(sig); [ "$now" != "$base" ] && { echo POLL_WAKE; break; }; done
   ```
   (Run with `run_in_background: true`.)
5. When Not-Started buffer drops below ~20, add the next wave (15 tasks). Atomic queue-join → `cat >> TASKS.md` for rows → commit → atomic queue-release. Reference the band/row ranges in the Wave plan table above.
6. Surface any unanswered question via `AskUserQuestion`, write the supervisor's answer back to QUESTIONS.md, mark `Status: Answered`. If the answer creates a consistency fix (like Q9 did), add a follow-up task.

## Memory files (cross-session context)

Located at `~/.claude/projects/-Users-vova-Downloads/memory/`:

- `management-assistant-coordination.md` — MA role + polling setup
- `leginfo-gap-incorporation.md` — initiative status (slightly stale; numbers in THIS file supersede)
- `leginfo-graph-artifacts.md` — pointer to gap files in the other repo
- `no-shortcuts-directive.md` — quality rule
- `officeholder-policy.md` — Q9 ruling
- `MEMORY.md` — index

## Recent history worth knowing

- **Crash on 2026-05-31:** user closed both RAs mid-task. MA cleaned up — reset orphaned task 791 (In Progress → Not Started), cleared stale `RA-Alpha` from the Data Write Queue, working tree was already clean. Fresh RAs relaunched and resumed without data loss.
- **Wave-replenishment cadence so far:** Waves 1–6 created reactively (~15 tasks/wave). Waves 7–10 created in a single block on user request once the team showed it could keep pace. Cadence and slice size are knobs — adjust if the user asks for bigger batches.
