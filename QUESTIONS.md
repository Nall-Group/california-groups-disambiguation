# Questions

RAs post questions here when blocked. The human supervisor answers them.

## QUESTIONS.md Write Queue

To edit this file (post questions, write answers), join this queue first. Only the name at the top may edit.

(empty)

## Open Questions

### Q17 (Task 1986, RA-Alpha) — "Reproductive Freedom for All" (bare) is the NATIONAL rebrand, not a CA-affiliate variant
**Status:** Open

Task 1986 part (2) instructs me to move the flat top-level alt **'Reproductive Freedom for All'** (bare, no "California"/"CA") *under* the 'CALIFORNIA ABORTION RIGHTS ACTION LEAGUE' chapter, classifying it as a "California-affiliate variant."

But "Reproductive Freedom for All" is the **2023 rebrand of the NATIONAL organization** (NARAL Pro-Choice America → Reproductive Freedom for All). The CA affiliate's new name is "Reproductive Freedom for All California" / "...CA" (those CA-specific strings ARE already correctly nested under the CA chapter). The parent canonical of this whole tree is "National Abortion Rights Action League" (the national org), and part (2) itself says to "keep genuine national-level alts at the top level."

Moving bare 'Reproductive Freedom for All' under the CA chapter would wrongly subordinate the national org's current name beneath its California affiliate — a consequential, hard-to-reverse mis-association.

**Question:** Confirm the handling:
- (a) **(Recommended)** Keep bare 'Reproductive Freedom for All' as a **national-level alternate_spelling at the top level** (it's the national rebrand), and move only the genuinely CA-specific strings ('California Abortion and Reproductive Rights', 'California Abortion Rights and Reproductive Rights', 'California Abortion & Reproductive Rights', 'CA Abortion and Reproductive Action League (CARAL)', 'CA Abortion and Reproductive Rights', 'CA Abortion and Reproductive Rights (CARAL)') under the CA chapter as alternate_spelling. Nest 'NARAL Pro-Choice California Foundation' as its own chapter under the CA chapter. Leave the CARAL fragment alts (part 3) as-is under the CA chapter.
- (b) Follow the task literally and move bare 'Reproductive Freedom for All' under the CA chapter too (NOT recommended — conflates national with CA affiliate).

Note: part (1) is already satisfied by global-dedup (no flat top-level CA duplicates remain at the top level). Blocking rather than make a wrong national/affiliate consolidation.

### Q16 (Task 2712, RA-Beta) — "Workers" parent is a junk truncation bucket, not NASW
**Status:** Open

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
