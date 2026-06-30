# Questions

RAs post questions here when blocked. The human supervisor answers them.

## QUESTIONS.md Write Queue

To edit this file (post questions, write answers), join this queue first. Only the name at the top may edit.

(empty)

## Open Questions

### Q11 (Task 1201, RA-Alpha) — "A Home Away From Home" vs "A Home Away From Homelessness" are DIFFERENT orgs
**Status:** Open

Task 1201 (flagged VERIFY/CAUTION) asks whether to merge "A Home Away From Home" and "A Home Away From Homelessness". Both are bare canonicals, no children. I verified each and they are clearly **two distinct organizations**:
- **"A Home Away From Homelessness"** — a SF Bay Area nonprofit (program of Bay Area Community Resources, 501c3), founded 1994 in partnership with the National Park Service; serves homeless / formerly-homeless children in San Francisco, Marin, and Oakland (programs: The Beach House, The School House, Summer Program). Site: homeaway.org.
- **"A Home Away From Home"** — a Southern California (Santa Monica / Los Angeles) senior-living referral service; helps families find assisted living, independent living, memory/dementia care, and residential care homes. Site: ahomeawayfromhome.com.

Different regions (Bay Area vs SoCal), different missions (homeless children vs senior-living referrals). The task itself cautioned not to merge if distinct.

**Question:** Confirm I should leave these two as **separate canonicals** (do NOT merge)? Marking task 1201 Blocked pending your confirmation. (If you instead know of a third "A Home Away From Home" that IS the same as the Homelessness org, let me know and I'll re-evaluate.)

### Q10 (Task 1200, RA-Alpha) — "A Helping Hand" duplicates may be DIFFERENT orgs
**Status:** Answered

Task 1200 asks to consolidate three canonicals reported by the supervisor as the same org:
- "A Helping Hand"
- "A Helping Hand Counseling"
- "A Helping Hand in Recovery INC"

All three are bare canonicals with no children. I verified each (NO SHORTCUTS) and they appear to be **different** entities sharing a generic name:
- **"A Helping Hand in Recovery INC"** — a 501c3 nonprofit (ahelpinghand.co) that helps addicts/alcoholics pay for treatment-related costs (detox, doctor visits, detox medication). Addiction-recovery financial-assistance charity.
- **"A Helping Hand Counseling"** — a mental-health / marriage-&-family-therapy counseling practice in San Diego (6244 Ferris Square; listed on Yelp/Healthgrades). A counseling provider, not a recovery-funding charity.
- **"A Helping Hand"** (bare) — too generic to attribute to either; could be any of the many CA orgs/businesses using this common name.

These serve different functions (addiction-treatment funding vs. mental-health counseling) and I could not confirm common ownership. Per the task's own caution ("If they appear distinct, do NOT merge — post a question"), I did not force the merge.

**Question:** Should I (A) leave all three as separate canonicals (do not merge — they look like distinct orgs)? (B) Merge only a subset you can confirm are the same? (C) Merge all three anyway under one canonical? If merging, which name should be canonical? Marking task 1200 Blocked pending your answer.

**Answer:** Keep all three separate — do NOT merge. Supervisor confirmed (2026-06-30) that "A Helping Hand", "A Helping Hand Counseling", and "A Helping Hand in Recovery INC" are distinct organizations that merely share a generic name (addiction-recovery funding charity vs. San Diego mental-health counseling practice vs. an unattributable generic name). Leave all three as separate canonicals. Task 1200 is resolved with no data change.

### Q9 (Tasks 785/786, RA-Beta) — recurring policy for the novel waves
**Status:** Answered

RA-Alpha and I diverged on how to handle **statewide constitutional officeholders** appearing as bill supporters in the leginfo novel set, and this pattern recurs in every novel wave (787–804), so a definitive ruling would keep us consistent.

The strings look like: "Attorney General Xavier Becerra", "Governor Gray Davis", "Secretary of State, Alex Padilla", "State Treasurer Phil Angelides", "State Controller Steve Westly", "Lieutenant Governor Gavin Newsom".

- **RA-Alpha (task 785, committed)** routed these to `leginfo_individuals.csv` (treating them as people). This follows the letter of CLAUDE.md, which lists "Attorney General Rob Bonta" as an `individuals` example, and the leadership-exception list does NOT explicitly name AG/Governor/SoS/Treasurer/Controller/Lt-Gov.
- **RA-Beta (task 786, committed)** added them as `alternate_spelling` under the matching **office canonical** (e.g. Becerra → `ATTORNEY GENERAL`; Angelides + J. Chiang → `CALIFORNIA STATE TREASURER`; Padilla → `SECRETARY OF STATE`; Westly + CA State Controller J. Chiang → `Office of State Controller`; Newsom → `LIEUTENANT GOVERNOR OF CALIFORNIA`; Gray Davis → `GOVERNOR'S OFFICE`). Rationale: the crosswalk **already** stores officeholders this way (e.g. `CALIFORNIA STATE TREASURER FIONA MA`/`...JOHN CHIANG` under `CALIFORNIA STATE TREASURER`; `Shirley N. Weber, California Secretary of State` under `SECRETARY OF STATE`; `Todd Gloria - Mayor of San Diego` under `CITY OF SAN DIEGO`), and as a *bill supporter* the string represents the office acting, not the private person.

**Question:** For the remaining novel waves, should statewide constitutional officeholders ("Office X, Person Y") be (A) alternate spellings under the office canonical [RA-Beta's approach, matches existing crosswalk structure], or (B) moved to `leginfo_individuals.csv` [RA-Alpha's approach, matches CLAUDE.md's literal example]? If (A), should RA-Alpha's task-785 entries be reclassified for consistency? Note: a Mayor is already covered by the leadership exception, so this is really about the six statewide constitutional offices.

**Answer:** Approach **(A)** — alt_spelling under the office canonical (RA-Beta's approach). Treat "Office X, Person Y" bill-supporter strings as the office acting, not the private person; this matches how the crosswalk already stores officeholders (e.g. Treasurer Fiona Ma under `CALIFORNIA STATE TREASURER`, Mayor Todd Gloria under `CITY OF SAN DIEGO`). Apply this policy to all remaining novel-band tasks (787–804 and beyond). For consistency, RA-Alpha's task-785 officeholder entries should be reclassified from `leginfo_individuals.csv` to alt-spellings under the appropriate office canonical — see follow-up task 805.

### Q8 (Task 53, RA-Alpha)
**Status:** Answered

While handling Task 53 (move generic single-word entries to partial CSV), I moved "Access" and "Action" as specified. Scanning the crosswalk, I found 52 additional single-word canonicals with NO children that are common English words. Some might be real orgs (e.g., "Amazon" the company, "Homeboy" likely Homeboy Industries, "Habitat" possibly Habitat for Humanity).

Here's the full list — which should be moved to `org_names_partial.csv` as too generic?

Advance, Amazon, Assembly, Balance, Bridge, Change, Common, Connect, Conservation, Democracy, Design, Direct, Excellence, Focus, Freedom, Global, Guardian, Habitat, Harvest, Haven, Home, Homeboy, Hope, Horizon, Insight, Lead, Liberty, Light, Link, Local, Momentum, Opportunity, Options, Partners, Pathway, People, Pioneer, Practice, Quality, Reach, Response, Restore, Rural, Safety, Share, Strategy, Together, Transit, Unity, Village, Voice, Volunteer

**Answer:** We web-searched each one. **Keep only "Amazon"** (Amazon.com, Inc. — major corporation that lobbies CA legislature). Move all other 51 to `org_names_partial.csv` — every one is too generic/ambiguous as a standalone word, and the crosswalk already has separate entries for the full-named organizations they might be fragments of (e.g., "Homeboy Industries", "Habitat for Humanity", etc.).

### Q7 (Task 40, RA-Alpha)
**Status:** Answered

While scanning for other "number (city)" entries that might be unnamed union locals, I found:

1. "261 (CAMPBELL)" — could be a firefighters, IBEW, or other union local in Campbell, CA
2. "3492 (North Antelope)" — could be a mining or energy workers union local at North Antelope (possibly North Antelope Rochelle coal mine in Wyoming, which would make it out-of-scope for CA)

Both have count=1. Should I:
- A) Move them to `org_names_partial.csv` as ambiguous fragments?
- B) Research further to identify the specific unions?
- C) Leave them as-is in the crosswalk?

**Answer:** Option A — move both to `org_names_partial.csv`. Web research found that Laborers' Local 261 (LIUNA) exists but covers SF/San Mateo/Marin, not Campbell — so "CAMPBELL" is ambiguous. No union local 3492 was found in any search, and "North Antelope" doesn't correspond to a California location. Both are too ambiguous to keep.

## Answered Questions

### Q6 (Task 29, RA-Beta)
**Status:** Answered

While scanning for generic/non-specific entries similar to "Board of Trustees", I found these additional questionable entries:

1. **"City Council (12-0)"** (count=1) — Looks like a vote tally fragment (12-0 is a vote count), not a real org. Should this go to `org_names_invalid.csv`?

2. **"Board of Directors Art Naldoza"** (count=4) — Looks like a conjoined entry (generic "Board of Directors" + individual name "Art Naldoza"). Should this go to `org_names_conjoined.csv`?

I'm proceeding with moving the clearly generic entries (Board of Trustees, Board of Directors, Advisory Board, Advisory Commission, City Council, Task Force, Working Group) to `org_names_partial.csv` without waiting for this answer.

**Answer:**
1. Yes, "City Council (12-0)" goes to `org_names_invalid.csv` — it's a vote tally fragment.
2. Yes, "Board of Directors Art Naldoza" goes to `org_names_conjoined.csv` — conjoined generic phrase + individual name.

### Q5 (Task 13, RA-Beta)
**Status:** Answered

Web research for "1065 (PEACE)" was inconclusive. I searched for:
- AFSCME Local 1065 PEACE California
- SEIU Local 1065 California
- "PEACE" as a union acronym with local numbers

No results identify a specific union local 1065 called "PEACE" in California. The entry appears only once in the dataset (count=1). It could be:
1. A truncated/garbled reference to a peace officers union local
2. A data artifact (the number 1065 could be a bill number — AB 1065 exists — combined with "PEACE" from surrounding text)
3. An obscure local chapter not well-documented online

Should I:
- A) Move it to `org_names_partial.csv` as a fragment (since it's ambiguous and low-frequency)?
- B) Move it to `org_names_invalid.csv`?
- C) Leave it in the crosswalk and rename it to something more descriptive if you know what it is?

**Answer:** Option A — move it to `org_names_partial.csv` as a fragment.

### Q4 (Task 21, RA-Alpha)
**Status:** Answered

While scanning for narrative text fragments, I found 19 entries with the pattern "year: OrgName" (e.g., "1998: American Farmland Trust", "2009: American Legion"). These have real org names but with a year prefix, suggesting they were extracted from legislative history text.

Examples:
1. "1997: Board of Trustees"
2. "1997: Universal Studios"
3. "1998: American Farmland Trust"
4. "1998: American Institute of Architects"
5. "1998: Associated Builders & Contractors"
6. "1998: Service Employees International Union"
7. "1999: Associated Builders and Contractors"
8. "1999: City of Santa Cruz Gary Loustalot"
9. "2002: People's Advocate"
10. "2005: Maria Shriver" (individual, not org)
11. "2006: Compline"
12. "2007: Southern Wine and Spirits of America"
13. "2009: American Legion"
14. "2009: California State Firefighters' Association"
15. "2009: Center for Public Interest Law"
16. "2009: Jack O'Connell" (individual, not org)
17. "2011: Gondola Adventures"
18. "2013: American Federation of State" (fragment — cut off)
19. "2013: California Thoroughbred Breeders Association"

Should I:
- A) Move all 19 to `org_names_embedded_in_narrative_text.csv` since they have year prefixes that make them narrative-embedded?
- B) Add a cleaning pattern to strip the "year: " prefix and merge into existing crosswalk entries?
- C) Something else? (Note: #10 and #16 are individuals, #18 is a fragment)

**Answer:** Skip these for now — they're covered by a new Task 22, which will add a cleaning pattern to strip the "year: " prefix. You can mark Task 21 as Done with what you've already completed (the narrative fragments).

## Answered Questions

### Q3 (Task 16/18, RA-Beta)
**Status:** Answered

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

**Answer:** Yes, these are all invalid. The bill references (#1-8) should go to `org_names_invalid.csv`. The other two (#9-10) are fragments and should go to `org_names_partial.csv`. Remove all of them from the crosswalk JSON and move their CSV rows accordingly.

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
