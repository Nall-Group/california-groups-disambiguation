# Questions

RAs post questions here when blocked. The human supervisor answers them.

## QUESTIONS.md Write Queue

To edit this file (post questions, write answers), join this queue first. Only the name at the top may edit.

(empty)


## Open Questions

### Q25 (Task 2658, RA-Fleet-3) — "State University" is a junk truncation bucket, not a real org to nest a chapter under
**Status:** Answered — Supervisor chose (a) (2026-06-19). Move **"State University, Fresno Chapter"** to `org_names_partial.csv` (ambiguous front-truncation, no identifiable parent — same class as Q21/Q23). Do NOT nest under the "State University" junk bucket. Remove from crosswalk JSON, move the row+count (currently in org_names_in_crosswalk.csv: "State University, Fresno Chapter",1), run the clean/dedup/stats pipeline.

Task 2658 asks me to nest the standalone canonical **"State University, Fresno Chapter"** as a `chapter` child under the existing canonical **"State University"**.

Same class of problem as **Q20** ("America"), **Q21** ("Contractors"), and **Q23** ("Political Association"): **"State University" is not a real organization** — there is no org literally named "State University". It is a canonical with **0 children**, an ambiguous truncation bucket. The surrounding crosswalk has sibling truncations of the same form — "State Univ", "State University, Hayward", "State University, Los Angeles" (all 0-child, all truncations of "California State University, [campus]"). Nesting a chapter under the bare "State University" bucket would assert membership in a meaningless org.

**"State University, Fresno Chapter"** is itself an **ambiguous front-truncation** of some "[X], (California) State University, Fresno Chapter". The word "Chapter" indicates it's a chapter of a membership organization located at/named for CSU Fresno — NOT the university itself. I searched the crosswalk: there are many "California State University, Fresno, [Department/Program/Association]" canonicals, but **no plain "California State University, Fresno" canonical** and **no other entry ending in "Fresno Chapter"** that could be the parent. The "[X]" membership org is unidentifiable (could be a faculty association, honor society, professional/Greek chapter, etc.). Unlike Q24, no real parent is sitting in the crosswalk.

Task 2658 has no CLAUDE.md escape clause, so per project rules I am blocking rather than forcing a wrong consolidation.

**Question:** How should "State University, Fresno Chapter" be handled?
- (a) **(Recommended)** Move it to `org_names_partial.csv` per CLAUDE.md's truncated-entries rule (truly ambiguous — no identifiable parent after crosswalk + web search). Do NOT nest under the "State University" junk bucket. (Optionally also flag the empty "State University" / "State Univ" canonicals and the sibling "State University, Hayward" / "State University, Los Angeles" truncations for the same treatment.)
- (b) Leave it as a standalone top-level canonical as-is (do nothing) — at least no wrong parent.
- (c) You can identify the real parent org (please specify which "[X] ... State University, Fresno Chapter"), and I'll nest it under that real canonical as a chapter.

I have made NO data changes (nothing nested, no CSV moves). Blocking rather than forcing a wrong consolidation. A general ruling here would also resolve Q20, Q21, and Q23 (same junk-bucket pattern).

### Q24 (Task 2642, RA-Fleet-1) — "Senior Citizens" is a bare generic truncation bucket, not a real org to nest a chapter under
**Status:** Resolved (moot) — RA-Fleet-2, 2026-06-19. No supervisor action needed. This resolves via option (c): the real parent IS already in the crosswalk. Canonical **"National Council of Senior Citizens"** has an existing chapter **"Westside Chapter, National Council of Senior Citizens"** (with alt spelling "National Council of Senior Citizens, Westside Chapter Los Angeles"). "Senior Citizens, Los Angeles Westside Chapter" is a front-truncation of "National Council of Senior Citizens, Los Angeles Westside Chapter," so I nested it as an `alternate_spelling` under that existing Westside chapter (NOT under the generic "Senior Citizens" bucket). The earlier web search dismissed NCSC for having "no confirmable Westside chapter," but the confirmation was sitting in the crosswalk itself. Task 2642 committed Done. (Note: Q20/Q21/Q23 remain genuinely Open — those truncations have no identifiable in-crosswalk parent, unlike this one.)

Task 2642 asks me to nest the standalone canonical **"Senior Citizens, Los Angeles Westside Chapter"** as a `chapter` child under the existing canonical **"Senior Citizens"**.

The problem: **"Senior Citizens" is not a real organization** — it is a bare, generic two-word term and the existing canonical has **zero children** (line 828813 in the crosswalk JSON). It is a truncation artifact: the surrounding crosswalk has dozens of distinct, real "Senior Citizens ..." orgs (e.g. "Senior Citizens Advisory Council", "Senior Citizens for Humane Legislation", "Senior Citizens Law Center", "SENIOR CITIZENS LEGAL SERVICES"), each its own canonical. Nesting a specific LA chapter under the bare "Senior Citizens" bucket would assert it belongs to an org literally named "Senior Citizens," which is false.

"Senior Citizens, Los Angeles Westside Chapter" is itself an **ambiguous truncation** of "[Some Org] ... Los Angeles Westside Chapter." I searched the crosswalk (no matching parent) and the web: no org named simply "Senior Citizens" with an LA Westside chapter exists; candidate parents are many and unconfirmable (e.g. National Council of Senior Citizens — had local chapters but no confirmable "Westside" one; WISE & Healthy Aging; Westside MAPS; YWCA/various). No single unambiguous parent.

This is the **same situation as Q20 (America), Q21 (Contractors), and Q23 (Political Association)** — all still Open. Forcing chapters under a meaningless truncation bucket creates wrong, hard-to-reverse associations.

**Question:** How should I handle this entry?
- (a) **(Recommended)** Move "Senior Citizens, Los Angeles Westside Chapter" to `org_names_partial.csv` per CLAUDE.md's truncated-entries rule (truly ambiguous after crosswalk + web search — no identifiable parent). Do NOT nest it under the "Senior Citizens" bucket.
- (b) Leave it as a standalone top-level canonical as-is (do nothing) — at least it gets no wrong parent.
- (c) You can identify the real parent org, and I'll nest it under that real canonical.

I have NOT nested it under "Senior Citizens." Blocking rather than forcing a wrong consolidation. (Ideally this + Q20/Q21/Q23 get one consistent policy answer.)

### Q23 (Task 2612, RA-Fleet-2) — "Political Association" is a junk truncation bucket, not a real org to nest a chapter under
**Status:** Answered — Supervisor chose (a) (2026-06-19). Move **"Political Association, Sacramento Chapter"** to `org_names_partial.csv` (truly ambiguous — Mexican-American / Black American Political Association of CA / many "[X] American Political Association" orgs all have Sacramento chapters, no single identifiable parent). Do NOT nest under the "Political Association" junk bucket. Remove from crosswalk JSON, move the row+count from its current org_names CSV, run the clean/dedup/stats pipeline.

Task 2612 asks me to nest the standalone canonical **"Political Association, Sacramento Chapter"** as a `chapter` child under the existing canonical **"Political Association"**.

Same class of problem as **Q20** ("America") and **Q21** ("Contractors"): **"Political Association" is not a real organization** — there is no org literally named "Political Association". It is a canonical with **0 children**, an ambiguous truncation bucket. Nesting a chapter under it would assert membership in a meaningless org.

**"Political Association, Sacramento Chapter"** is itself an **ambiguous truncation** of some "[X] Political Association, Sacramento Chapter." Searching the crosswalk, multiple real parents already exist and several already have a Sacramento chapter, so there is no single unambiguous parent:
- **Mexican-American Political Association** — already has a `chapter` child "Mexican American Political Association, Sacramento Chapter"
- **Black American Political Association of California** — already has "Black American Political Association of California, Sacramento Chapter"
- plus many other "[X] American Political Association" canonicals (Asian Pacific Islander American, Chinese-American, Filipino American, Hmong American, Latino American, etc.)

Task 2612 has no CLAUDE.md escape clause, so per project rules I am blocking rather than forcing a wrong consolidation.

**Question:** How should "Political Association, Sacramento Chapter" be handled?
- (a) **(Recommended)** Move it to `org_names_partial.csv` per CLAUDE.md's truncated-entries rule (truly ambiguous — no identifiable parent). Do NOT nest under the "Political Association" junk bucket. (Optionally also flag the empty "Political Association" canonical itself for separate truncation cleanup / removal to partial.)
- (b) Leave it as a standalone top-level canonical as-is (do nothing) — at least no wrong parent.
- (c) You can identify the real parent org (please specify which "[X] Political Association"), and I'll nest it under that real canonical as a chapter.

I have made NO data changes (nothing nested, no CSV moves). Blocking rather than forcing a wrong consolidation. A general ruling here would likely also resolve Q20 and Q21 (same junk-bucket pattern).

### Q22 (Task 2475, RA-Fleet-3) — "Disabled Veterans of America" is a variant of the real org "Disabled American Veterans" (DAV); literal execution would create a duplicate Chapter 35
**Status:** Answered — Supervisor chose (a): FOLD into the real DAV tree (2026-06-19). Make canonical **"Disabled Veterans of America"** an `alternate_spelling` of **"Disabled American Veterans"**; fold the Chapter 35 variants ("Disabled Veterans of America, Chapter 35", its alt "Disabled Veterans of America-Chapter 35", and the standalone "Disabled Veterans, Chapter 35") as `alternate_spelling` under DAV's existing chapter **"Disabled American Veterans, American River Chapter 35"**; nest "Disabled Veterans of America, Cal-Diego Chapter" as a `chapter` under DAV's **"Department of California"** (treat Cal-Diego as a DAV San Diego chapter); also fold standalone "Disabled Veterans Department of California" into the DAV tree. This supersedes the prior literal grouping (now an easy single-subtree move). Within-crosswalk; no CSV changes. Tracked as new follow-up **task 2740**.

Task 2475 asks me to nest two standalone canonicals — **"Disabled Veterans of America, Cal-Diego Chapter"** and **"Disabled Veterans of America, Chapter 35"** — as `chapter` children under the existing near-empty canonical **"Disabled Veterans of America"** (currently 0 children).

Searching the crosswalk first (as the task instructs) surfaced a problem with the task's premise:

1. **The real organization is "Disabled American Veterans" (DAV)** — a large existing canonical with a full chapter tree (Department of California, American River Chapter 35, Antelope Valley, Chapter 63, Sacramento Chapter No. 6, Sonoma County, Van Nuys, etc.). "Disabled Veterans of America" is just a **word-order variant** of "Disabled American Veterans" — there is no separate national org named "Disabled Veterans of America".

2. **Chapter 35 already exists under DAV** as the chapter **"Disabled American Veterans, American River Chapter 35"**, whose alternate spellings already include "Disabled American Veterans, Chapter 35", "Disabled American Veterans, Chapter 35, Carmichael", and "Disabled American Vets, Chapter 35". So "Disabled Veterans of America, Chapter 35" (and its alt "Disabled Veterans of America-Chapter 35", plus the standalone canonical "Disabled Veterans, Chapter 35") are all variants of the SAME real chapter. Nesting them under "Disabled Veterans of America" would create a **duplicate Chapter 35 in a second tree** — and because the strings differ, name-normalization dedup will NOT catch it.

3. **"Cal-Diego" is genuinely ambiguous.** "Cal-Diego" is the identifier of the separate existing canonical **"Cal-Diego Paralyzed Veterans Association"** (a Paralyzed Veterans of America chapter serving San Diego), NOT DAV. So "Disabled Veterans of America, Cal-Diego Chapter" may be a name conflation rather than a clean DAV San Diego chapter.

**Question:** How should I handle task 2475?
- (a) **(Recommended)** Fold everything into the real DAV tree instead: make the canonical "Disabled Veterans of America" an `alternate_spelling` of "Disabled American Veterans"; fold the Chapter 35 variants ("Disabled Veterans of America, Chapter 35", "Disabled Veterans of America-Chapter 35", and the standalone "Disabled Veterans, Chapter 35") as `alternate_spelling` under the existing "Disabled American Veterans, American River Chapter 35"; and nest "Disabled Veterans of America, Cal-Diego Chapter" as a `chapter` under DAV's "Department of California" (pending the Cal-Diego clarification below). I can do this if you confirm.
- (b) Execute the task literally — nest both as chapters under "Disabled Veterans of America" as-is (NOT recommended: leaves a duplicate Chapter 35 and an unmerged DAV-variant canonical).
- (c) Some other handling (please specify), especially re: "Cal-Diego" — is it a DAV San Diego chapter, or should it instead be associated with "Cal-Diego Paralyzed Veterans Association"?

I have made NO data changes. Blocking rather than entrenching a duplicate chapter / wrong consolidation. (Also note: standalone canonicals "Disabled Veterans, Chapter 35" and "Disabled Veterans Department of California" appear to be further DAV variants worth folding in under option (a).)

**Update (RA-Fleet-1, 2026-06-18):** Task 2475 was concurrently claimed and executed **literally (option b)** by RA-Fleet-1 before this question was seen — "Disabled Veterans of America, Cal-Diego Chapter" and "Disabled Veterans of America, Chapter 35" (its alt "Disabled Veterans of America-Chapter 35" preserved) are now `chapter` children under the canonical "Disabled Veterans of America" (commit 816f956f). TASKS.md shows 2475 **Done**. This grouping is **reversible and duplicate-neutral** (the DVA-vs-DAV Chapter 35 duplication pre-existed as separate top-level canonicals; re-parenting did not create it). **RA-Fleet-3's option (a) still stands as a worthwhile follow-up** — folding the whole "Disabled Veterans of America" family (plus standalone "Disabled Veterans, Chapter 35" and "Disabled Veterans Department of California") into the real **Disabled American Veterans (DAV)** tree, and resolving the Cal-Diego/PVA ambiguity — and still needs your ruling; if approved it would supersede the literal grouping (now an easy single-subtree move). Note: "Disabled Veterans, Chapter 35" was task 2474 (separately claimed by RA-Fleet-2).

### Q21 (Task 2464, RA-Fleet-2) — "Contractors" is a junk truncation bucket, not a real org to nest chapters under
**Status:** Answered — Supervisor chose (a) (2026-06-19). Move **"Contractors, San Diego Chapter"** to `org_names_partial.csv` (truly ambiguous — AGC/ABC/NECA/SMACNA all have SD chapters, no single identifiable parent). Do NOT nest under the "Contractors" junk bucket. Remove from crosswalk JSON, move the row+count from its current org_names CSV, run the clean/dedup/stats pipeline.

Task 2464 asks me to nest the standalone canonical **"Contractors, San Diego Chapter"** as a `chapter` child under the existing canonical **"Contractors"**.

The problem (same class as **Q20** "America"): **"Contractors" is not a real organization** — there is no org literally named "Contractors". It is an ambiguous truncation bucket. Its only existing child is **"Contractors of CA"**, which is itself a truncation. Nesting "Contractors, San Diego Chapter" under it would assert membership in an org named "Contractors," which is meaningless/false.

"Contractors, San Diego Chapter" is itself an **ambiguous truncation** of some "[X] Contractors, San Diego Chapter." The crosswalk + general knowledge give multiple plausible real parents, each of which has a San Diego chapter:
- Associated General Contractors (AGC)
- Associated Builders and Contractors (ABC)
- National Electrical Contractors Association (NECA)
- Engineering/Sheet Metal/Air Conditioning contractors associations

There is no single unambiguous parent. (Related sibling truncations also exist as standalone canonicals: "Contractors-San Diego Chapter", "Contractors of California San Diego Chapter", "Contractors of San Diego".)

Note: task 2464 has no CLAUDE.md escape clause (unlike task 2465), so per the project rules I am blocking rather than forcing a wrong consolidation.

**Question:** How should "Contractors, San Diego Chapter" be handled?
- (a) **(Recommended)** Move it to `org_names_partial.csv` per CLAUDE.md's truncated-entries rule (truly ambiguous — no identifiable parent). Do NOT nest under the "Contractors" junk bucket. (Optionally also flag "Contractors" + "Contractors of CA" for separate truncation cleanup.)
- (b) Leave it as a standalone top-level canonical as-is (do nothing) — at least no wrong parent.
- (c) You can identify the real parent org (please specify which "[X] Contractors"), and I'll nest it under that real canonical.

I have NOT nested it under "Contractors". Blocking rather than forcing a wrong consolidation.

### Q20 (Task 2330, RA-Fleet-2) — "America" is a junk truncation bucket, not a real org to nest chapters under
**Status:** Answered — Supervisor + MgmtAssistant research (2026-06-19). SPLIT the two entries; do NOT nest either under the "America" junk bucket:
- **"America, South Bay Chapter 53"** is a front-truncation of **"Vietnam Veterans of America, South Bay Chapter 53"** (VVA, the national org with numbered chapters; "South Bay Chapter 53" = Torrance/LA-County chapter). This chapter ALREADY EXISTS in the crosswalk as `chapter` **"Vietnam Veterans of America South Bay Chapter 53"** (~line 974426) under canonical **"Vietnam Veterans of America"** (~line 974190), with alts like "Vietnam Veterans of America - Chapter 53". Nest "America, South Bay Chapter 53" as an `alternate_spelling` under that existing chapter. Within-crosswalk move, no CSV change.
- **"America, San Diego Chapter"** (+ its alt **"America, San Diego Chapter, Inc"**) remains a genuinely ambiguous truncation — no VVA San Diego chapter exists in the crosswalk (VVA's SD is Chapter 472; "VIETNAM VETERANS OF SAN DIEGO" is a separate org), and any "[X] of America" could have an SD chapter. Move it to `org_names_partial.csv` (remove from crosswalk JSON, move the row+count from whichever org_names CSV it's in). Run the clean/dedup/stats pipeline after (CSV change).

Task 2330 asks me to nest the two standalone canonicals "America, San Diego Chapter" (+ alt "America, San Diego Chapter, Inc") and "America, South Bay Chapter 53" as `chapter` children under the existing canonical **"America"**.

The problem: **"America" is not a real organization** — it's a heterogeneous junk truncation bucket. Its current children are unrelated fragments of DIFFERENT "...of America" orgs:
- "America INC"
- "America of California"
- "America Local 9119"  (a union local)
- "America, Local 132"  (a different union local)
- "America, LLC"

Nesting "America, San Diego Chapter" and "America, South Bay Chapter 53" under this bucket would assert they belong to an org literally named "America," which is false. This is the same situation as Q16 (the "Workers" junk bucket → NASW) — forcing chapters under a meaningless truncation bucket creates wrong, hard-to-reverse associations.

The two chapters are themselves **ambiguous truncations** of some "[Org] of America." I searched the crosswalk: many "...of America" orgs have a "San Diego Chapter" (e.g. Associated General Contractors of America, Autism Society of America, etc.), and "South Bay Chapter 53" (numbered chapter) points to some national org with numbered chapters that I cannot identify. There is no single unambiguous parent.

**Question:** How should I handle these two entries?
- (a) **(Recommended)** Move both to `org_names_partial.csv` per CLAUDE.md's truncated-entries rule (truly ambiguous after crosswalk + web consideration — no identifiable parent). Keep "America, San Diego Chapter, Inc" with its parent. This removes them from the crosswalk; do NOT nest under the "America" bucket.
- (b) Leave both as standalone top-level canonicals as-is (do nothing) — they at least don't get a wrong parent.
- (c) You can identify the real parent org for one/both (please specify which "[Org] of America"), and I'll nest them under that real canonical.

I have NOT nested them under "America". Blocking rather than forcing a wrong consolidation.

### Q19 (Task 2313, RA-Fleet-3) — How to handle the "California Reproductive Health Association" canonical
**Status:** Open

Task 2313 asked me to split the conjoined entry "California Reproductive Health Association Children Now" (Children Now + "California Reproductive Health Association") and flag the questionable component name. I have completed the mechanical split: Children Now (canonical "CHILDREN NOW") and the conjoined-row move to `org_names_conjoined.csv` are done.

The flagging question: there is **already an existing canonical** in the crosswalk named **"California Reproductive Health Association"** (count 24), with one alternate spelling "CA Reproductive Health Association". However, internet research found **no organization by that exact name** — it looks like a possible data-entry conflation. The closest real statewide bodies are:
- **California Coalition for Reproductive Freedom**
- **Essential Access Health** (formerly California Family Health Council)

Also possibly relevant: **California Reproductive Health Equity Project**, or simply a loose paraphrase of a coalition.

**Question:** How should the existing "California Reproductive Health Association" canonical (and its "CA Reproductive Health Association" alt) be handled?
- (a) Keep as-is (treat it as a real, if informally-named, org).
- (b) Rename/re-map it as an alternate spelling under one of the real orgs above (which one?).
- (c) It is not a real org — move "California Reproductive Health Association" (24) and "CA Reproductive Health Association" to `org_names_partial.csv` (or another invalidity CSV) and remove the canonical from the crosswalk.

I left the existing "California Reproductive Health Association" canonical untouched pending your answer (per the task: do NOT create/modify it yet). The conjoined entry has been split and removed regardless.

### Q18 (Task 2243, RA-Fleet-3) — UNITED STATES CHAMBER OF COMMERCE megatask should be decomposed into subtasks
**Status:** Open

I blocked Task 2243 because, as written, it is not a single task but a bundle of ~6 distinct large-scale restructuring operations over the entire `UNITED STATES CHAMBER OF COMMERCE` tree, which I measured live in the crosswalk:

- **Tree size:** 76 depth-1 children, **1,683 total descendants.**
- The single child `CALIFORNIA CHAMBER OF COMMERCE` holds **1,060 direct children / 1,403 descendants** — overwhelmingly individual **city/county chambers** (Carlsbad, Oceanside, Fullerton, Torrance, …). Part (1) asks each of these to become **its own top-level canonical** → roughly **~1,000 promotions**, each needing a per-entity judgment call (independent org vs. genuine sub-chapter vs. alt-spelling).
- On top of that: (2) resolve 9 script-flagged flat alts, (3) merge duplicate chapter siblings, (4) reclassify CalChamber `[chapter]` variants to alt-spelling, (5) fix ~12 OCR/typo chapters, (6) route dozens of conjoined/narrative/individual children to invalidity CSVs (each conjoined entry first requiring its component chambers to exist as canonicals).

**Why I blocked instead of executing:** A worker RA must produce ONE atomic, verifiable commit. A ~1,000-promotion automated pass over a 1,683-node megatree cannot be reviewed or verified in a single session, and a rushed pass risks silently flattening real chapter relationships or mis-promoting entries — damage that is hard to detect and reverse. This violates the decomposed-task model the project is built on.

**Recommendation:** Decompose Task 2243 into separate, independently-committable subtasks, e.g.:
1. **Split only:** Promote `CALIFORNIA CHAMBER OF COMMERCE` to its own top-level canonical (move its whole subtree intact), and keep under the U.S. Chamber canonical only true U.S.-Chamber spellings.
2. **Promote ethnic/national chambers** (Hispanic, Black, Asian Pacific, African American, Chinese, Japanese, Korean, Filipino, Vietnamese, Women's, National Gay & Lesbian, Green, Cannabis, etc.) — one batch.
3. **Promote city/county chambers to top-level** — batched into manageable chunks (e.g. alphabetical groups of ~50), since this is the ~1,000-entry bulk.
4. **9 flat-alt resolutions + duplicate chapter-sibling merges.**
5. **CalChamber `[chapter]`→alt reclassification + ~12 OCR/typo demotions.**
6. **CSV routing** of the listed individuals / narrative-embedded / conjoined / partial children.

Please confirm you'd like this decomposition (and whether the ~1,000 city-chamber promotions in part 3 should each truly be top-level canonicals, or whether some should remain grouped). Once subtasks exist, RAs can pick them up and complete each atomically.

### Q17 (Task 1986, RA-Alpha) — "Reproductive Freedom for All" (bare) is the NATIONAL rebrand, not a CA-affiliate variant
**Status:** Answered — Supervisor chose (a) (2026-06-16, via RA-Beta). Keep bare 'Reproductive Freedom for All' as a national-level alternate_spelling at the TOP level (it's the national 2023 rebrand). Move only the genuinely CA-specific strings under the CA chapter as alternate_spelling, and nest 'NARAL Pro-Choice California Foundation' as its own chapter under the CA chapter. Leave the CARAL fragment alts (part 3) as-is. Task 1986 unblocked; RA-Beta picking it up.

Task 1986 part (2) instructs me to move the flat top-level alt **'Reproductive Freedom for All'** (bare, no "California"/"CA") *under* the 'CALIFORNIA ABORTION RIGHTS ACTION LEAGUE' chapter, classifying it as a "California-affiliate variant."

But "Reproductive Freedom for All" is the **2023 rebrand of the NATIONAL organization** (NARAL Pro-Choice America → Reproductive Freedom for All). The CA affiliate's new name is "Reproductive Freedom for All California" / "...CA" (those CA-specific strings ARE already correctly nested under the CA chapter). The parent canonical of this whole tree is "National Abortion Rights Action League" (the national org), and part (2) itself says to "keep genuine national-level alts at the top level."

Moving bare 'Reproductive Freedom for All' under the CA chapter would wrongly subordinate the national org's current name beneath its California affiliate — a consequential, hard-to-reverse mis-association.

**Question:** Confirm the handling:
- (a) **(Recommended)** Keep bare 'Reproductive Freedom for All' as a **national-level alternate_spelling at the top level** (it's the national rebrand), and move only the genuinely CA-specific strings ('California Abortion and Reproductive Rights', 'California Abortion Rights and Reproductive Rights', 'California Abortion & Reproductive Rights', 'CA Abortion and Reproductive Action League (CARAL)', 'CA Abortion and Reproductive Rights', 'CA Abortion and Reproductive Rights (CARAL)') under the CA chapter as alternate_spelling. Nest 'NARAL Pro-Choice California Foundation' as its own chapter under the CA chapter. Leave the CARAL fragment alts (part 3) as-is under the CA chapter.
- (b) Follow the task literally and move bare 'Reproductive Freedom for All' under the CA chapter too (NOT recommended — conflates national with CA affiliate).

Note: part (1) is already satisfied by global-dedup (no flat top-level CA duplicates remain at the top level). Blocking rather than make a wrong national/affiliate consolidation.

### Q16 (Task 2712, RA-Beta) — "Workers" parent is a junk truncation bucket, not NASW
**Status:** Answered — Supervisor chose (a) (2026-06-16, via RA-Beta). Create a proper canonical "National Association of Social Workers" and consolidate ALL NASW fragments (incl. "Workers, California Chapter (NASW-CA)") under it / its "California Chapter (NASW-CA)". Tasks 2712 and 2793 to be handled together as a full NASW consolidation. RA-Beta picking this up.

Task 2712 says to nest "Workers, California Chapter (NASW-CA)" as a chapter under the existing canonical "Workers". But "Workers" is a garbage truncation bucket whose children are unrelated truncated union locals ("Workers, Inc", "Workers Local 1528", "Workers Local 180", "Workers Local 416", "Workers Local 495", ...). "Workers, California Chapter (NASW-CA)" is actually a fragment of the **National Association of Social Workers, California Chapter (NASW-CA)** — nesting it under the "Workers" junk bucket would create a wrong association.

The NASW org is currently badly fragmented across many truncated canonicals, e.g.: "Association of Social Workers (NASW-CA)", "Association of Social Workers, California Chapter (NASW-CA)", "California Chapter (NASW-CA)", "Chapter (NASW-CA)", "NASW, CA Chapter", "Irvine National Association of Social Workers", "Municipal Employees National Association of Social Workers". There is no clean "National Association of Social Workers" canonical to nest under.

**Question:** How should I proceed?
- (a) **(Recommended)** Create a proper canonical "National Association of Social Workers" and consolidate ALL the NASW fragments (incl. "Workers, California Chapter (NASW-CA)") under it / its "California Chapter (NASW-CA)" — i.e. a dedicated NASW consolidation task for the full set.
- (b) Create only "National Association of Social Workers, California Chapter (NASW-CA)" and nest just this one entry there, leaving the other NASW fragments for later.
- (c) Follow the task literally and nest under the "Workers" junk bucket (NOT recommended — wrong association).

Blocking rather than make a wrong edit. The "Workers" junk bucket itself likely needs its own cleanup task.

### Q15 (Task 1567, RA-Gamma) — CALIFORNIA placeholder redistribution: unidentifiable/ambiguous acronyms
**Status:** Answered

**Answer (from supervisor, via Management Assistant):**

**Q1 — Yes.** Attach all of the confidently-resolved acronyms as `alternate_spelling` under the existing canonicals you found (create none new). Your full list (PORAC, Caltrans, CJAC, EQCA, NCJW CA, CADPAAC, ABC, CAOC, CWDA, APALC, SBCTC, NAIFA variants, CELSOC, PIFC, OWL/CA, CVUC, ACLU branch, Local 20 IFPTE variants) is approved as mapped.

**Q2 — two rules depending on the case:**
- **Truly unidentifiable** (no crosswalk match AND you cannot determine the org at all) → route to `org_names_partial.csv`. This covers: **AOCUC, EMSAAC, AKAMAC, OSCC, ACSI, TEXCOM, HOBO, LAAC**.
- **Ambiguous — could be more than one real org** → resolve by **count**: look up each candidate org in the CSVs / crosswalk and attach the acronym to whichever candidate has the **highest count**. Apply this to:
  - **HOAC** ('Officers Association of California (HOAC)') — compare the candidate orgs (e.g. Highway Officers Assn vs Hospital ...) and use the higher-count one.
  - **FLEXCOM** — compare Family Court Bar Association vs State Bar FLEXCOM; use the higher-count one.
  - **AAA** ('California (AAA)') — compare Automobile Club / AAA vs Area Agency on Aging; use the higher-count one.
  - **AAP-CA** — confirmed = **American Academy of Pediatrics, California**; attach there (no ambiguity).

The bare-fragment routing you described ('CALIFORNIA'/'THE CALIFORNIA'/'Calfiornia' → partial; 'California (Secretary John Laird)' → individuals) is approved — do it in the same pass.

Task 1567 asks me to redistribute the malformed 'CALIFORNIA' placeholder cluster (~30 `California (ACRONYM)` chapters, each a distinct org) out to each org's correct canonical. Most resolve cleanly and I'm ready to attach them, but several acronyms cannot be confidently resolved, and mis-attaching a real org is harmful/hard to reverse — so I've blocked.

**Confident (will attach as alternate_spelling under the existing canonical found in the crosswalk):**
- PORAC → Peace Officers Research Association of California
- Caltrans → California Department of Transportation
- CJAC → Civil Justice Association of California
- EQCA → Equality California
- NCJW CA → National Council of Jewish Women California
- CADPAAC → California Alcohol and Drug Program Administrators
- ABC → Associated Builders and Contractors
- CAOC → Consumer Attorneys of California
- CWDA → County Welfare Directors Association of California
- APALC → Asian Pacific American Legal Center
- SBCTC → State Building and Construction Trades Council
- NAIFA / NAIFA-CA / NAIFA-California → NAIFA (National Assn of Insurance & Financial Advisors)
- CELSOC → Consulting Engineers and Land Surveyors of California
- PIFC → Personal Insurance Federation of California
- OWL/CA → Older Women's League of California
- CVUC → Crime Victims United of CA (exists as 'Crime Victims Unite of CA (CVUC)')
- ACLU branch → ACLU of California (per task)
- Local 20 IFPTE variants → Engineers & Scientists of California, IFPTE Local 20

**Q1: Confirm I should attach all of the above to those canonicals (creating none new), or correct any mapping.**

**Q2: These acronyms have NO crosswalk match and I can't confidently identify the org — please provide the org behind each, or tell me to route them to org_names_partial.csv:**
- AOCUC, EMSAAC, AKAMAC, OSCC, ACSI, TEXCOM, HOBO, LAAC
- Ambiguous: HOAC ('Officers Association of California (HOAC)' — Highway? Hospital?), FLEXCOM ('Family Court Bar Association (FLEXCOM)'? or State Bar FLEXCOM), AAA ('California (AAA)' — Automobile Club, or Area Agency on Aging?), AAP-CA (American Academy of Pediatrics, CA — confirm)

The bare-fragment routing ('CALIFORNIA'/'THE CALIFORNIA'/'Calfiornia' → partial; 'California (Secretary John Laird)' → individuals) is unambiguous and I'll do it as part of the same pass once Q1/Q2 are settled.

### Q14 (Task 1429, RA-Gamma) — AFSCME tree: the "SCRIPT-FLAGGED block" of part-1 mappings is missing
**Status:** Answered (moot)

**Answer (Management Assistant, 2026-06-15):** Moot — no longer needed. The global-dedup pass (`clean_crosswalk.py --global-dedup`) has run and automatically resolved part 1 (removing flat top-level alternate_spellings that are byte/normalized duplicates of strings already nested under chapters). So the missing SCRIPT-FLAGGED list is no longer required. Task 1429 has been unblocked (→ Not Started); execute parts 2-5 only (merge duplicate chapters by Local number — these normalize differently so were NOT auto-deduped — collapse the interleaved LA-probation Local-685 subtree, casing normalization, keep narrative/individual/conjoined entries as alternate_spelling).

Task 1429 (consolidate the giant 'American Federation of State, County and Municipal Employees (AFSCME)' tree — **544 depth-1 children, 1,178 total descendants, 131 chapters**) part (1) says: "The mappings are given in the SCRIPT-FLAGGED block; follow each one" — but that block of specific flat-alt → target-chapter mappings was **not included** in the task text (unlike tasks 1417/1427, which embedded their script-flagged lists inline). Parts 2–5 are specified by pattern (merge named duplicate chapters by Local number, etc.), but part 1's per-entry mappings aren't.

**Q: Please either (a) paste the SCRIPT-FLAGGED block of part-1 flat-alt → chapter mappings, or (b) confirm I should execute part 1 by the stated principle — "for each flat depth-1 alternate_spelling that is byte-identical to a string already nested under a chapter, remove the flat top-level copy (the nested copy is preserved); only byte-identical duplicates removed" — and derive parts 2–5 from the explicit chapter-merge targets in the task.** Given the 544-child scale this warrants a dedicated pass once the approach is confirmed, so I've blocked it. The task is fully within-crosswalk (no data loss, no CSV changes) either way.

### Q13 (Task 1427, RA-Gamma) — AFL-CIO tree: promote SEIU subtree to its own canonical? (blocks SEIU consolidation)
**Status:** Answered

**Answer (from supervisor, via Management Assistant): Option A — take SEIU out of AFL-CIO.** Promote SEIU to its own top-level canonical 'Service Employees International Union'; move the whole SEIU subtree (and all SEIU variants routed in task 1427 parts 3–6) out of AFL-CIO into it. Keep CA Labor Federation / central-labor-council variants under AFL-CIO. The non-SEIU parts of 1427 (dedup flat copies, merge 'AFL-CIO'/'AFL- CIO' nodes, casing normalization) proceed as planned.

**General rule (applies to ALL trees, not just 1427):** Independent affiliate unions that are wrongly nested under AFL-CIO should be **promoted to their own top-level canonical** — e.g. SEIU, AFSCME, CWA, Teamsters, and any other independent national/international union. AFL-CIO is a federation of independent unions, so those affiliates do not belong nested inside it. **Exception — keep nested under AFL-CIO:** genuine AFL-CIO sub-bodies that the AFL-CIO actually charters, i.e. **central labor councils and state labor federations** (e.g. California Labor Federation, county/regional central labor councils). Those stay as chapters/children of AFL-CIO.

Note for task 1429 (AFSCME): AFSCME already has its own large top-level canonical, so any AFSCME copies nested under AFL-CIO should be merged into the standalone AFSCME canonical per this rule.

Task 1427 (consolidate the heavily-duplicated 'AMERICAN FEDERATION OF LABOR AND CONGRESS OF INDUSTRIAL ORGANIZATIONS' tree — 172 depth-1 children, 433 total descendants) part (8) explicitly asks me to flag this for you, and the answer determines where the bulk of the consolidation work lands, so I'm blocking on it:

**Should the entire SEIU (Service Employees International Union) subtree — currently nested under AFL-CIO and labeled `[pre-merge]` — be promoted to its own separate top-level canonical 'Service Employees International Union', rather than staying nested under AFL-CIO?** SEIU is an *affiliate* union of the AFL-CIO (and in fact disaffiliated in 2005 to form Change to Win), not a predecessor org in the AFL–CIO merger, so nesting it under AFL-CIO as a `[pre-merge]` component looks structurally wrong.

This matters because task 1427 parts (3)–(6) route many SEIU variants (e.g. 'SEIU of California', 'California State Council, SEIU', 'SEIU Local ...', 'The Service Employees International Union (SEIU), Local 1000', narrative SEIU strings, the conjoined 'SEIU Local 660 and Southern Christian Leadership Conference', and leader entry 'SEIU Local 535, David C. Aroner, Exec. Director, Oakland') to the SEIU subtree. If SEIU becomes its own canonical, all of those go there instead of under AFL-CIO.

- **Option A:** Promote SEIU to its own top-level canonical 'Service Employees International Union'; move the whole SEIU subtree (and all SEIU variants from the sweep) out of AFL-CIO into it. Keep CA Labor Federation / central-labor-council variants under AFL-CIO.
- **Option B:** Keep SEIU nested under AFL-CIO as currently structured; just consolidate the duplicates in place.

Related: should other clearly-affiliate unions currently nested under AFL-CIO (e.g. AFSCME, the various central labor councils) also be promoted, or only SEIU? A general rule would let me apply it consistently. The non-SEIU parts of 1427 (dedup the 20 flat AFL-CIO/CA Labor Federation copies, merge the two 'AFL-CIO'/'AFL- CIO' alt nodes, casing normalization) are ready to execute as soon as this is settled, so I'll do the whole task in one pass once answered.

### Q12 (Task 1417, RA-Gamma) — ACLU tree: confirm chapter structure + fragment-routing list
**Status:** Answered

**Answer (from supervisor, via Management Assistant):** Yes to all — proceed with the proposed chapter structure (part A), the additional sub-org chapters in your A sub-question (`ACLU California Action`, `ACLU, California Legislative Office`, `National Prison Project of the ACLU`, `ACLU Members for LPS Reform`, `ACLU Club of Vallejo High School` → make these chapters too), and the fragment-routing list (part B) as you proposed.

**One correction — San Diego is NOT a sub-chapter of Northern or Southern California.** Researched: California is unique in having **three co-equal ACLU affiliates** — ACLU of Northern California, ACLU of Southern California, and ACLU of San Diego & Imperial Counties. San Diego & Imperial Counties is a peer affiliate (covering the southernmost counties and border region), not nested under either region. So keep `San Diego and Imperial Counties` as its own top-level `chapter` under the CA umbrella, peer to Northern California and Southern California — which is what your original plan already does. (Sources: aclusocal.org/about-us, aclu-sdic.org/about-us, acluca.org.)

Everything else in your plan is approved as written — execute the whole tree in one atomic commit and run the clean/dedup/stats pipeline.

The 'American Civil Liberties Union' tree has **186 depth-1 children**, almost all flat `alternate_spelling` (only 4 nodes have nested children; only 'New York ACLU' is a `chapter`). Two parts of the task don't cleanly map to the data, so I want to confirm before committing a large, partly-irreversible restructure:

**(A) Chapter structure (parts 1–2).** Part 1 refers to nesting "byte-exact duplicates" under named "chapters," but those named nodes are currently `alternate_spelling` (not chapters) and the entries to nest are *distinct* spellings, not byte-duplicates. My proposed plan — please confirm or adjust:
- Convert these 4 existing parent nodes to `chapter` and nest the matching CA / Center-for-Advocacy spellings under them: `American Civil Liberties Union (ACLU)` (national), `AMERICAN CIVIL LIBERTIES UNION OF CALIFORNIA` (CA-state umbrella — also fold the ~18 "Northern California/Southern California/San Diego & Imperial Counties" combined-affiliate spellings here), `American Civil Liberties Union of California (ACLU)`, and `ACLU of California, Center for Advocacy and Policy`.
- Create `chapter` nodes (each named per the cleanest existing spelling) and nest matching regional alts for: **Northern California, Southern California, San Diego and Imperial Counties, Santa Cruz, Santa Barbara, Santa Clara Valley, Marin, Ventura, San Luis Obispo, Berkeley, South Bay, Whittier, Sacramento Area, Paul Robeson Chapter**. Keep `New York ACLU` as a chapter.
- Leave all remaining general ACLU spellings/typos (e.g. `ACLU`, `The ACLU`, `Americna Civil Liberties Union`, `American Civil Liberties Association`) as flat depth-1 `alternate_spelling`.
- **Q: Should I also create chapters for the sub-orgs the task did NOT list — `ACLU California Action`, `ACLU, California Legislative Office`, `National Prison Project of the ACLU`, `ACLU Members for LPS Reform`, `ACLU Club of Vallejo High School`** — or leave those as flat depth-1 alts?

**(B) Fragment routing (part 5).** The task says to route "the narrative/procedural fragment children listed in the issues" to invalidity CSVs, but no list was provided. Please confirm the candidates I identified and the target CSV for each (all are removed from the crosswalk; ACLU itself remains the canonical so no org is lost):
- → `org_names_embedded_in_narrative_text.csv` (ACLU embedded in prose): `American Civil Liberties Union (ACLU), numerous`; `... (ACLU) and the`; `American Civil Liberties Union to be`; `American Civil Liberties Union,to`; `American Civil Liberties Union (concerned)`; `The American Civil Liberties Union (ACLU) is`; `ACLU California Action writes the following to explain why it`; `ACLU California Action, in support`; `... California Action, a co-sponsor`; `... California Action is pleased`; `... California Action, co-sponsor of this measure`; `In support, the American Civil Liberties Union California Action`; `American Civil Liberties Union (ACLU) writes`; `American Civil Liberties Union of California states`; `ACLU consistently`; `ACLU in conclusion`; `ACLU of California is pleased`; `ACLU Previous letters of support pertained to a version of the bill not before the Committee`; `ACLU, writing in opposition`; `... (ACLU) in opposition`; `... (ACLU), writing in opposition`; `... California Action respectfully` (x2 spellings); `ACLU California Action strongly`; `ACLU California Action - CO-SPONSOR`; `ACLU (regarding background checks)`; `ACLU (with concerns)`.
- → `org_names_invalid.csv` (bill refs / procedural NOTE comments, not org names): `The American Civil Liberties Union AB123X`; `ACLU (NOTE: SEE COMMENT 5 FOR RECOMMENDATIONS FROM THE APRIL 7 HEARING OF THE SUBCOMMITTEE ON JUVENILE JUSTICE)`; `ACLU (NOTE: SEE COMMENT 8 ...)`; `ACLU AB 2285 (Aguiar) Page p`; `ACLU AB 522 (Diaz)`; `American Civil Liberties Union (NOTE: SEE COMMENTS 13-15 FOR POSSIBLE PROPOSED AUTHOR'S)`.
- Borderline I plan to **keep as alts** unless told otherwise: `American Civil` (truncation), `American Civil Liberties` (truncation), `Counties (CSAC) ACLU`, `American (Corbett) Union (ACLU)`, `SCR ACLU`, `Women American Civil Liberties Union`, `Children and Families ACLU`, `American Civil Liberties Union of California, Disability`.

Once confirmed I'll execute the whole tree in one atomic commit and run the clean/dedup/stats pipeline.

### Q11 (Task 1243, RA-Alpha) — state association vs. national-parent placement (structural)
**Status:** Answered

While doing the gaps consolidation I found the audit (and some prior waves) paired **independent California state associations to their NATIONAL same-name counterparts**, and in a few cases nested the CA org as a `chapter` under the national one. Some of these are *legitimate* federation/chapter relationships; others are *separate* organizations that happen to share a name pattern. I routed the pending gap variants to the existing California node to resolve the gaps, but the parent relationship needs a ruling.

Specifically:
- **California Bankers Association** is currently a `chapter` under **AMERICAN BANKERS ASSOCIATION** (with several CBA alt-spellings nested there). CBA is generally an *independent* state trade association, not an ABA chapter. Should CBA be **promoted to its own top-level canonical** (and the wrongly-nested CBA children moved out of ABA)?
- **California Association of Health Underwriters (CAHU)** is a `chapter` under **NATIONAL ASSOCIATION OF HEALTH UNDERWRITERS** — this one *is* a real state chapter of the NAHU federation, so I believe it should **stay** as a chapter. Confirm?
- Other audit mis-pairings I left **pending** (org begins "California…", target is the national "American…/National…"): California Assoc. of Neurological Surgeons (CANS) → American Assoc. of Neurological Surgeons; California Assoc. of Naturopathic Physicians (CANP) → American Assoc. of Naturopathic Physicians; California Assoc. of Nurse Anesthetists → American Assoc. of Nurse Anesthetists; CA Art Education Assoc. → National Art Education Assoc.; CA Assoc. for Music Education → National Assoc. for Music Education; CA Farmers Union → National Farmers Union. For each: should the CA org be its **own canonical** (independent), or a **chapter** under the national parent? A general rule would let me process the rest consistently.

**Answer:** General rule: **if the CA org is NOT affiliated with the national org (i.e. a legally independent state organization that merely shares a name pattern), it should be moved OUT — promoted to its own top-level canonical** (and any wrongly-nested CA children moved out of the national parent). Only keep it as a `chapter` when it is a genuine state chapter/affiliate of the national federation. The RA must **do their own research per-org (e.g. web search)** to determine affiliation before deciding — do not assume from the name pattern. So: promote **California Bankers Association** to its own canonical (move CBA children out of AMERICAN BANKERS ASSOCIATION). For **CAHU**, confirm via research and keep as a chapter only if it really is a NAHU state chapter. Apply the same research-then-decide rule to CANS, CANP, CA Nurse Anesthetists, CA Art Education Assoc., CA Assoc. for Music Education, CA Farmers Union, and any future same-name CA/national pairings.

### Q10 (Task 1207, RA-Beta) — 3 police/sheriff pairings deferred (tangled pre-existing structure)
**Status:** Answered

While doing Task 1207 (consolidate GROUP 2 associations), I applied 22 clean consolidations and confirmed ~10 were already nested. **Three police/sheriff pairings I deferred** because the crosswalk already has a conflicting structure that the simple audit pairing would make worse:

There is a generic super-parent canonical **`Police Officer's Association`** with ~55 city POAs nested as `chapter` children (Anaheim, Burbank, Fresno, Long Beach, Riverside, San Diego, etc.), and a parallel generic **`SHERIFF'S ASSOCIATION`** parent with `CA STATE SHERIFFS ASSOCIATION` / `RIVERSIDE SHERIFF'S ASSOCIATION` / Alameda chapters. The audit's targets are *separate tiny standalone canonicals*, so the same org exists in two places:

- **`Long Beach Police Officers Association`** → audit says alt of `Long Beach Police Officers`. But `LONG BEACH POLICE OFFICERS ASSOCIATION` (with alts LBPOA, "Long Beach Police Officers' Association") already exists as a **chapter under `Police Officer's Association`**, while `Long Beach Police Officers` is a separate standalone canonical (2 alt children).
- **`Riverside Police Officers Association`** → audit says alt of `Riverside Police Officers Assoc`. But "Riverside Police Officers Association" already exists as an alt under the `RIVERSIDE POLICE OFFICERS' ASSOCIATION` chapter (under `Police Officer's Association`); `Riverside Police Officers Assoc` is a separate standalone canonical.
- **`Riverside Sheriffs' Association`** → audit says alt of `Riverside Sheriffs' Association (All)`. But `RIVERSIDE SHERIFF'S ASSOCIATION` already exists as a chapter under `SHERIFF'S ASSOCIATION`; `Riverside Sheriffs' Association (All)` is a separate standalone canonical.

**Question:** How should these be resolved? Specifically: (A) Is the generic `Police Officer's Association` / `SHERIFF'S ASSOCIATION` super-parent structure intended (each city POA a "chapter" of a generic national-sounding parent), or should those generic parents be dissolved and each city POA become its own standalone canonical? (B) For each of the 3 orgs above, which spelling should be the single canonical, and should the duplicate standalone be merged into the chapter (or vice-versa)? This likely warrants its own follow-up task covering the whole `Police Officer's Association` / `SHERIFF'S ASSOCIATION` subtree, not just these 3. The other 34 GROUP 2 rows are done.

**Answer:** KEEP the grouped super-parent structure (do NOT dissolve it). (A) There should be a single canonical **`Police Officers Association`** with ALL police officers associations (every city/regional POA) as `chapter` children. The **sheriff's associations are different from the police associations**, so `Sheriff's Association` is its OWN separate canonical with its own chapter structure (all sheriff associations as chapters). Standardize the police parent's canonical spelling to `Police Officers Association`. (B) For each of the 3 deferred orgs, merge the tiny standalone duplicate INTO the existing chapter under the correct parent (the chapter under the parent stays; the standalone canonical's name + its alt children become alternate_spellings within that chapter; remove the duplicate standalone canonical): `Long Beach Police Officers Association` + standalone `Long Beach Police Officers` → consolidate under the `LONG BEACH POLICE OFFICERS ASSOCIATION` chapter (under `Police Officers Association`); `Riverside Police Officers Association` + standalone `Riverside Police Officers Assoc` → consolidate under the `RIVERSIDE POLICE OFFICERS' ASSOCIATION` chapter; `Riverside Sheriffs' Association` + standalone `Riverside Sheriffs' Association (All)` → consolidate under the `RIVERSIDE SHERIFF'S ASSOCIATION` chapter (under `Sheriff's Association`). These 3 plus the whole-subtree cleanup are covered by **follow-up task 1208** — so you may mark task 1207 **Done** with the 34 completed consolidations (the 3 deferred move to 1208).

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
