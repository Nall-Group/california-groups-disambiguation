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
