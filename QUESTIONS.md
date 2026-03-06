# Questions

RAs post questions here when blocked. The human supervisor answers them.

## Open Questions

### Q3 (Task 16, RA-Beta)
**Status:** Open

While scanning for entries similar to "171 - OoCONNELL", I found these potentially invalid entries in the crosswalk that look like legislative bill references, numeric fragments, or garbled text rather than real organizations:

**Bill references (number + bill ID):**
1. "3 - AB 2037"
2. "3 - SB 2096"
3. "4 - SB 1173"
4. "4 - SB 2103"
5. "4 - SB 796"
6. "4 - SBX1 11"
7. "6 - AB 2888"
8. "6 - SB 205"

**Other fragments:**
9. "2- POSITIONS"
10. "3.County CalWORKs plan"

Should I move these to `org_names_invalid.csv` in a follow-up task? Or are any of these legitimate entries that should stay?

## Answered Questions

### Q2 (Task 6, RA-Alpha)
**Status:** Answered

The "Support"/"Supporters" entries in the crosswalk are NOT conjoined multi-org entries like "Supporters (OrgA, OrgB, ...)". Instead, I found:

1. **~90+ "Support. [single org]" entries** (e.g., "Support. California Medical Association (CMA)", "SUPPORT. The California Hospital Association (CHA)") - single org names with "Support." prefix, embedded in analysis text. These are the same pattern as Task 3's "COMMENTS:" entries and should be handled by adding a cleaning pattern.

2. **~16 "Support and Opposition. [org]" entries** - same pattern with "Support and Opposition." prefix.

3. **~5 "Support CalPACE" style entries** - "Support [org]" without period.

4. **Only 1-2 genuinely conjoined entries** like "SUPPORT. Stop AAPI Hate and the California Healthy Nail Salon Collaborative".

5. **Invalid entries**: "Supporters", "Supports", "Supportive" - these are fragments.

How should I proceed?
- A) Add "Support." / "Support:" / "SUPPORT." / "Support and Opposition." cleaning patterns to `cleaning_patterns.txt` (like Task 3) to strip the prefix, then handle the few genuinely conjoined and invalid entries separately?
- B) Something else?

**Answer:** We've broken this into separate tasks. For your current Task 6 (revised scope): just handle item 4 — the 1-2 genuinely conjoined entries. Parse out the individual orgs, check/add them to the crosswalk, and move the conjoined entry to `org_names_conjoined.csv`. The other items are now Tasks 7-10.

### Q1 (Task 4, RA-Alpha)
**Status:** Answered

The entries "29 AFL-CIO & CLC" and "29 AFL-CIO and CLC" don't clearly match any known California union local. Web search didn't turn up a specific "Local 29" affiliated with AFL-CIO/CLC in California. Could these be truncated references to IFPTE Local 29 (which appears to be based at NASA Goddard in Maryland, not California) or something else? Should I:
1. Leave them as-is since the identity is unclear?
2. Move them to `org_names_partial.csv` as incomplete/fragment names?
3. Research further with different search terms?

Also, there's a standalone entry "Local 20" (with no union affiliation specified) which is ambiguous. Should it be consolidated under Engineers and Scientists of California (IFPTE Local 20), or left alone?

**Answer:**
1. Move "29 AFL-CIO & CLC" and "29 AFL-CIO and CLC" to `org_names_partial.csv` as fragments.
2. Consolidate "Local 20" under Engineers and Scientists of California (IFPTE Local 20).
