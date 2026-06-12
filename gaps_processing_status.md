# Crosswalk Gaps Processing Status

Source: `crosswalk_gaps_all_stances.csv` — 69,065 org names (67,609 unique) from legislative
bill-position data, NONE cleanly in the crosswalk. Goal: agent-triage EACH, group by target
canonical, create insertion tasks; RAs insert and remove rows from the gaps CSV as done.

Working file: `gaps_sorted_by_bills.csv` (sorted by total_bills desc = priority order).
Method: waves of ~480 orgs (8 chunks x 60), agent disposition: CONSOLIDATE / NEW_CANONICAL /
NEW_CHAPTER / INVALID:<csv>. Then group findings into tasks.

## Progress
- Wave 1 (orgs ranked 1–480, bills 590→21): DISPATCHED, hit transient 529 — RETRYING.
- Remaining: ~67,000 unprocessed.

## Notes
- ~20% of top-200 near-match an existing node; ~43% exist only as DIRTY VARIANTS (consolidate);
  ~37% truly absent (new canonical/chapter).
- Many low-value rows are invalids: "N individuals", "Author", "County", etc. -> route per CLAUDE.md.

## Update — bulk pass done
- Bulk-classified all 67k (after skim+refine): 785 individuals, 1431 chambers, 93 partial, 2 invalid bulk-routed.
- Tasks created: 1224 (individuals), 1225 (partial+invalid), 1226 (1431 chambers -> Task 1196 hierarchy).
- NEEDS AGENT AUDIT: 66,754 real orgs (gaps_needs_audit.txt, sorted by bill count). Wave 1a (top ~240) dispositions in gaps_wave1_dispositions.txt; grouped tasks for those still TODO.
- NEXT: continue agent-audit waves over gaps_needs_audit.txt (top by bill count), group findings into per-canonical tasks.

## Wave 1 COMPLETE (top-480 by bill count)
- All 480 dispositioned (g1_00-07). Worklist: gaps_wave1_worklist.txt. Tasks: 1239 (Indivisible), 1240 (advocacy/labor/veterans batch), 1241 (new canonicals).
- Chambers from wave 1 -> tasks 1226/1229-1231; individuals/fragments -> 1224/1225; DA offices -> 1198.
- needs_audit cursor advanced; ~66,483 remain.
- IMPROVEMENT for future waves: have disposition agents WRITE output to a TSV file so grouping is scriptable instead of hand-transcribed.

## Overnight session progress (waves 2-12)
- Processed through wave 12. needs_audit remaining tracked in gaps_needs_audit.txt.
- All dispositions accumulate in gaps_master_*.{tsv,txt}; standing tasks 1242-1245 consume them.

## SESSION CHECKPOINT (through wave 22)
- Agent waves 2–22 complete. Orgs dispositioned this session (incl. bulk+acronym): ~12596.
- needs_audit remaining:    56299 (resume = head -480 gaps_needs_audit.txt, run 8 agents writing w<NN>_<chunk>.out, then: python3 _agg.py "$TMPDIR/wNN_*.out" wNN).
- Master worklist totals (fuel for standing tasks 1242-1245):
    consolidate.tsv:     3627
    new_canonicals.txt:      197
    new_chapters.tsv:      109
    individuals.txt:     1581
    partial.txt:      836
    invalid.txt:     3477
    conjoined.txt:      251
- Tail note: beyond ~rank 9,000 by bill count the file is dominated by procedural/narrative/individual junk (invalid+individuals >80% per wave); real-org yield (consolidate/new) has dropped to <15%/wave.
