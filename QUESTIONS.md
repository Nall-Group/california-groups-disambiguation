# Questions

RAs post questions here when blocked. The human supervisor answers them.

## QUESTIONS.md Write Queue

To edit this file (post questions, write answers), join this queue first. Only the name at the top may edit.


## Open Questions

### Q51 (Task 4601, RA-Fleet-3) — CLEANING PATTERN PROPOSAL: leading `Oppose ` stance prefix — 7 crosswalk entries
**Status:** Open

Surfaced while doing task 4601 (leginfo batch 924), where 3 of the batch's entries were `Oppose `-prefixed source strings. Per the Worker RA Role I'm proposing a regex rather than cleaning them one by one, and I have **not** touched `cleaning_patterns.txt`. I filed all 3 as exact-spelling alts under their correct canonicals (forward-compatible: if this is approved, `clean_crosswalk.py` reduces each to a still-valid alt or merges it into the canonical).

**The class.** An `opposition` stance column bled into the front of the org name, exactly like the already-approved `Sponsor `/`Support.` prefix class (Q at line ~812). `cleaning_patterns.txt` already strips *trailing* oppose stances (`, oppose$`, `- Oppose$`, `(oppose)$`, `[oppose]$`) but has **no leading-prefix rule**, so these survive. **7 crosswalk entries** match.

**Proposed regex** (leading only, require a letter after the space so it can't eat a parenthetical):
```
(?i)^oppose\s+(?=[A-Za-z])
```

**Example matches (before → after):**
- `Oppose California District Attorneys Association` → `California District Attorneys Association`
- `Oppose Amalgamated Transit Union Local 1555` → `Amalgamated Transit Union Local 1555`
- `Oppose American Forest and Paper Association` → `American Forest and Paper Association`

**Near-misses it must NOT match (verified by running the regex):**
- `OPPOSITION` / `OPPOSE UNLESS AMENDED` → **not matched** (`^oppose\s+` needs a space + letter; `Opposition` has neither, and the all-caps stance row ends there).
- `Oppose (continued): |City of Pismo Beach | Consumer Rights` (a conjoined-CSV row) → **not matched**; the `(?=[A-Za-z])` lookahead blocks it, so the regex won't strip into a leading `(continued)` fragment.
- No legitimate org name in the crosswalk begins with the word `Oppose` — all 7 matches are stance bleed.

**One honest caveat (I ran the regex rather than eyeballing it):** the mixed-case `Oppose Unless Amended` **does** match, yielding `Unless Amended`. That exact string is not in the crosswalk — the stance rows sit in `org_names_invalid.csv` (`OPPOSE UNLESS AMENDED`, `Oppose As Amended`, `Oppose None reported to committee`, ~18 rows) — but since `cleaning_patterns.txt` is applied by `regenerate_org_subsets.py` to the CSVs too, approving this would rewrite those invalid rows into fragments (`As Amended`, `None reported to committee`). They stay invalid either way, so nothing is misclassified, but if you'd rather not churn them I can add a `(?!unless\b|as\b|none\b)` guard to the lookahead.

**Two cases worth your call before this lands:**
1. `Oppose Child Sierra County Children's Services (concerned)` → would become `Child Sierra County Children's Services (concerned)`, which still looks garbled. Fine either way (it's no worse than today), but flagging it.
2. Stripping is **not** required for correctness here — all 7 are already alts mapping to the right canonical, so they're orphan-safe as-is. This is a hygiene/consistency call: approve if you want the class handled globally (and future `Oppose `-prefixed leginfo rows auto-cleaned); decline if 7 entries isn't worth another global regex.

Should I file a follow-up task to add this pattern + run the full clean/dedup/stats pipeline?

### Q50 (Task 4582, RA-Fleet-3) — CLEANING PATTERN PROPOSAL: mid-string PDF page-footer artifacts (`PageB`, `Page n`, `Page 3`) — ~28+ entries
**Status:** Open

Surfaced while doing task 4582 (leginfo batch 906), whose entry `North County Rape Crisis & Child Protection Center (Santa PageB Barbara County)` carries a page footer spliced into the **middle** of the org name. Per the Worker RA Role I am proposing a regex rather than cleaning them one by one, and per the same rule I have **not** touched `cleaning_patterns.txt`. I filed the batch-906 entry as an exact-spelling alt (forward-compatible: if this pattern is approved, `clean_crosswalk.py` reduces it to a still-valid alt).

**The class.** A PDF page footer (`Page` + a single letter/digit) got interleaved into the org name during extraction. ~28 entries match `Page[A-Z]` alone; more match the spaced `Page n` / `Page b` / `Page 3` variants. It appears as prefix, suffix, AND mid-string, so a suffix-only regex won't do.

**Proposed regex** (strip anywhere in string, then collapse whitespace):
```
(?i)\s*\bPage\s?[A-Za-z0-9]\b\s*
```
Note: it must strip to a SINGLE space when mid-string (not empty), else `Santa PageB Barbara` → `SantaBarbara`.

**Example matches (before → after):**
- `North County Rape Crisis & Child Protection Center (Santa PageB Barbara County)` → `North County Rape Crisis & Child Protection Center (Santa Barbara County)`
- `California District Attorneys Association PageC` → `California District Attorneys Association`
- `PageO San Francisco AIDS Foundation` → `San Francisco AIDS Foundation`
- `California Page n Insurance Wholesalers Association` → `California Insurance Wholesalers Association`

**Near-misses it must NOT match** (real orgs/people containing "Page" — these are why I'm asking rather than applying):
- `Page & Turnbull, INC` and `Page & Sohnen` — architecture/law firms; "Page" followed by `&`, so the single-alnum requirement spares them.
- `Greg Page, Energy Management Specialist Temecula Valley Usd` — "Page" followed by a comma.
- `One Page Plan` — "Page" followed by `Plan` (multi-char word), so `[A-Za-z0-9]\b` does not fire.

**Questions for you:**
1. Approve this regex, or scope it tighter (e.g. only `Page[A-Z]` no-space + explicit `Page n|Page b`), given the single-letter wildcard is the risky part?
2. Several of these entries have *other* damage too (`California Attorneys for Criminal Justice [NOTE: SEE COMMENT 8 ...] PageB`, `(Leno) PageB Industrial Hemp Council, Inc`) — should stripping the footer be a separate follow-up task from the bracketed-NOTE / stray-parenthetical junk, or handled together?

---

### Q49 (Task 4570, RA-Fleet-3) — Eleventh instance of the lost-separator conjoined class: `Monterey Peninsula Taxpayers Assoc.` + `Los Angeles County` — the source cell shows FOUR merges in a row
**Status:** Answered — APPROVED (supervisor, 2026-07-16). Route the fused/conjoined string to `org_names_conjoined.csv` (count 1), add nothing to the crosswalk; any prose fragment that names no org → `org_names_invalid.csv`. This is now a **STANDING RULE** (baked into `LEGINFO_IMPORT.md` conjoined section): a lost-separator conjoined straddle whose components are all already in the crosswalk is handled **inline — no blocking, no question**; only escalate if a component is missing. Closes the whole Q32–Q49 lost-separator class.

**Same class as Q35/Q39/Q40/Q44/Q46/Q47.** Task 4570 (leginfo batch 895) had 12 entries. I resolved 11 and committed them: 7 added (6 new canonicals + 1 alt), 4 already present. The 12th is another lost-separator conjoined artifact.

**The entry.** Task 4570 hints: `Monterey Peninsula Taxpayers Assoc. Los Angeles County` (1) -> alternate spelling of **Monterey Peninsula Taxpayers Association**.

**Why the hint is wrong.** The Monterey Peninsula is ~300 miles from Los Angeles County; no such joint entity exists. Grepping the source cell makes the mechanism unambiguous — this is a two-column PDF merge, and the *same cell* contains a run of identical artifacts:

```
... Assoc. of Gelnn County Taxpayers City of Norco; Los Angeles Taxpayers Assoc. County Sanitation;
Monterey Peninsula Taxpayers Assoc. Los Angeles County; Sacramento County Taxpayers League; ...
```

Read as left-column + right-column pairs, every one of these splits cleanly:

| Merged string in source | Left column (org A) | Right column (org B) |
|---|---|---|
| `Assoc. of Gelnn County Taxpayers City of Norco` | Assoc. of Glenn County Taxpayers | City of Norco |
| `Los Angeles Taxpayers Assoc. County Sanitation` | Los Angeles Taxpayers Assoc. | County Sanitation (District) |
| `Monterey Peninsula Taxpayers Assoc. Los Angeles County` | **Monterey Peninsula Taxpayers Assoc.** | **Los Angeles County** |

The neighbours that were NOT merged (`Sacramento County Taxpayers League`, `United Organizations of Taxpayers, Inc`) are clean single orgs — confirming the merge is a column artifact, not real names.

**Why it matters.** Filing the hint as-is would attach `Los Angeles County` boilerplate onto the Monterey Peninsula Taxpayers Association canonical as an alt spelling, which would then match/pollute future leginfo rows for both orgs.

**Both halves are already in the crosswalk:**
- `Monterey Peninsula Taxpayers Association` — CANONICAL (with alt `Monterey Peninsula Taxpayers Assoc`)
- `Los Angeles County` — present under STATE OF CALIFORNIA

**Options:**
- **(a)** Route the merged string to `org_names_conjoined.csv` (count 1). Both component orgs already exist, so nothing else to add. **My recommendation** — consistent with the CLAUDE.md conjoined rule and with what Q35/Q39/Q40/Q44/Q46/Q47 all propose.
- **(b)** Follow the hint and add it as an alt of Monterey Peninsula Taxpayers Association. I think this is wrong for the reasons above.

**Meta-ask (raised in Q35/Q39/Q40/Q44/Q46/Q47, still unanswered):** this is now the **eleventh** confirmed instance, and it is clearly systematic to the two-column PDF stance tables, not incidental. Rather than one blocked task per instance, could we get a **standing rule** — e.g. "leginfo entries whose source cell shows a two-column merge go to `org_names_conjoined.csv` if both halves already exist; only ask when a half is missing"? That would let RAs resolve this class in-task instead of blocking. Task 4570 is marked **Blocked** with the assignee cleared; the 11 resolved entries are already committed, so whoever picks it up only needs to handle this 1.


### Q48 (Task 4565, RA-Fleet-3) — Cleaning-pattern proposal: unhyphenated `(cosponsors)` suffix is not stripped (11 entries)

**Status:** Open

**Type:** reusable cleaning-pattern proposal (per CLAUDE.md "Proposing new cleaning patterns"). Not blocking — task 4565 is committed; this is a global cleanup for sign-off.

**The gap.** `cleaning_patterns.txt` already strips the *hyphenated* forms — line 7 `\(co-sponsor\)`, line 16 `\(co-sponsors\)`. It does **not** strip the *unhyphenated* `(cosponsors)`. Line 35's catch-all `\(...\bsponsor...\)$` also misses it, because in `cosponsors` there is no word boundary between `co` and `sponsors`, so `\bsponsor` never matches. Confirmed with `python3 scripts/clean_name.py "Medical Oncology Association of Southern California (cosponsors)"` -> returns the string unchanged.

**Proposed regex** (strips a trailing `(cosponsor)` / `(cosponsors)` tag):

```
(?i)\s*\(cosponsors?\)\s*$
```

**Example matches (before -> after):**
- `California Hospital Association (cosponsors)` -> `California Hospital Association`
- `American College of Surgeons (cosponsors)` -> `American College of Surgeons`
- `Los Angeles LGBT Center (cosponsors)` -> `Los Angeles LGBT Center`

**Near-misses it must NOT match (verified against the current regex):**
- `California League of Savings (co-sponsor) Institutions` — the tag is **mid-string**, not a suffix, and the org name continues after it. The `$` anchor leaves this untouched (it is also a separate conjoined/lost-text issue, not a suffix strip).
- `Medical Oncology Association of Southern California (MOASC)` — a legitimate trailing **acronym** in parens. Per [[scan_valid_vs_dirty_affixes]] trailing `(ACRONYM)` is a valid org-name part; the regex only matches the literal word `cosponsor(s)`, so acronyms are safe.

**Affected entries: 11** (all currently in `2_webapp/org_clusters_crosswalk.json`):
- `American Cancer Society Cancer Action Network (cosponsors)`
- `American College of Surgeons (cosponsors)`
- `Association of Northern California Oncologists (cosponsors)`
- `California Chronic Care Coalition (cosponsors)`
- `California Hospital Association (cosponsors)`
- `California Pharmacists Association (cosponsors)`
- `California Professional Firefighters (cosponsors)`
- `Emergency Medical Services Administrator's Association of California (cosponsors)`
- `Leukemia Lymphoma Society (cosponsors)`
- `Los Angeles LGBT Center (cosponsors)`
- `Medical Oncology Association of Southern California (cosponsors)`

**One caveat worth a decision.** These 11 dirty strings are real source rows. Stripping them globally will collapse each onto its clean canonical, which orphans the dirty source string to `not_in_crosswalk.csv` (the [[prefix_strip_orphan]] / task 1716 failure mode). The established fix is to keep BOTH spellings as alts. **Question: should the approved pattern task (a) strip and re-add each dirty string as an `alternate_spelling` of its clean canonical, or (b) just strip and let `regenerate_org_subsets.py` re-route the orphans?** I recommend (a), consistent with task 1716 and how task 4518/4565 kept the dirty string as an alt so the source row maps.

### Q47 (Task 4560, RA-Fleet-3) — Tenth instance of the lost-separator conjoined class: `Mayor of San Leandro` + `City of Albany` — full source cell retrieved

**Status:** Answered — APPROVED (supervisor, 2026-07-16). Route the fused/conjoined string to `org_names_conjoined.csv` (count 1), add nothing to the crosswalk; any prose fragment that names no org → `org_names_invalid.csv`. This is now a **STANDING RULE** (baked into `LEGINFO_IMPORT.md` conjoined section): a lost-separator conjoined straddle whose components are all already in the crosswalk is handled **inline — no blocking, no question**; only escalate if a component is missing. Closes the whole Q32–Q49 lost-separator class.

**Same class as Q32/Q33/Q34/Q35/Q38/Q39/Q40/Q44/Q46.** Task 4560 (leginfo batch 885) had 15 entries. I resolved 14 and committed them (`e6575f18`): 13 added as alternate spellings, 1 (`Mayor pro Tem Mary England, City of Lemon Grove`) already present as a case variant. The 15th is another lost-separator conjoined artifact.

**The entry.** Task 4560 hints: `Mayor of San Leandro, City of Albany` (1) -> alternate spelling of **CITY OF SAN LEANDRO**.

**Why that hint is wrong.** San Leandro and Albany are two different Alameda County cities. "CITY OF SAN LEANDRO" is not also known as "Mayor of San Leandro, City of Albany", so making this an alt spelling would assert a false identity between two unrelated cities.

**The smoking gun — I pulled the whole source cell** (column `support`, from `leginfo_metadata.csv`). It is a semicolon-separated, alphabetically-ordered supporter list:

> ... Humboldt County Department of Health and Human Services; International Longshore and Warehouse Union; **Kaiser Permanente; Mayor of San Leandro, City of Albany; National Latino Tobacco Control Network**; National Lawyers Guild Labor and Employment Committee; Professional and Technical Engineers, Local 21; ...

Two independent confirmations:
1. **Alphabetical order.** The list sorts K -> M -> N. A genuine supporter named "City of Albany" would sort under **C** (the same list has "County of Los Angeles" in the C run). "City of Albany" sitting after "Mayor of San Leandro" is only explicable as a lost `;` separator, not as part of one org's name.
2. **A control case in the same corpus.** Other rows carry `Mayor of San Leandro` as its own standalone semicolon item, with no "City of Albany" attached — e.g. `... Mayor of Berkeley; Mayor of Oakland; Mayor of Richmond; Mayor of San Leandro; Oakland African American Chamber of Commerce; ...`

**Both constituents are already in the crosswalk**, so no org name is lost by routing the artifact:
- `STATE OF CALIFORNIA > CITY OF SAN LEANDRO > Mayor of San Leandro`
- `STATE OF CALIFORNIA > CITY OF ALBANY`

**Conflict with the task instructions.** Task 4560 says "Do NOT route any to a CSV" — but following that literally would assert that San Leandro is also known as the City of Albany. Blocking rather than deviating, consistent with Q32-Q46.

**Proposed action (same as the rest of the class):** add `"Mayor of San Leandro, City of Albany",1` to `org_names_conjoined.csv`, add nothing to the crosswalk, and close task 4560 with no further crosswalk change.

**Meta:** this is the **tenth** instance, and all ten (Q32/Q33/Q34/Q35/Q38/Q39/Q40/Q44/Q46/Q47) are still Open. A single blanket ruling on the class — "lost-separator conjoined straddle, both constituents already present -> route to `org_names_conjoined.csv`, no crosswalk change, no question needed" — would unblock tasks 4126, 4187, 4196, 4205, 4338, 4402, 4407, 4512, 4541 and 4560 at once and let future RAs handle these inline instead of spending a round-trip each. Ten round-trips on one mechanical pattern is the strongest argument yet for the blanket ruling.

Task 4560 is marked **Blocked** with the assignee cleared; the 14 resolved entries are already committed, so whoever picks it up only needs to handle this 1.

### Q46 (Task 4541, RA-Fleet-2) — Ninth instance of the lost-separator conjoined class: `Los Angeles Area Chamber of Commerce` + `Management & Capital Group` — with a decisive alphabetical smoking gun

**Status:** Answered — APPROVED (supervisor, 2026-07-16). Route the fused/conjoined string to `org_names_conjoined.csv` (count 1), add nothing to the crosswalk; any prose fragment that names no org → `org_names_invalid.csv`. This is now a **STANDING RULE** (baked into `LEGINFO_IMPORT.md` conjoined section): a lost-separator conjoined straddle whose components are all already in the crosswalk is handled **inline — no blocking, no question**; only escalate if a component is missing. Closes the whole Q32–Q49 lost-separator class.

**Same class as Q32/Q33/Q34/Q35/Q38/Q39/Q40/Q44.** Task 4541 (leginfo batch 864) had 20 entries. I resolved 19 and committed them (`bb78ae68`): 11 added as alternate spellings, 8 were already present verbatim. The 20th is another lost-separator conjoined artifact.

**The entry.** Task 4541 hints: `Los Angeles Area Chamber of Management & Capital Group` (1) -> **new canonical** of the same name.

**Why the hint would inject bad data.** There is no such organization. The leginfo source cell is a semicolon-separated, **alphabetized** supporter list of accounting/consulting firms, and the separator between two entries was lost:

> `... Kuebler, Thomas & Co; Los Angeles Area Chamber of Management & Capital Group; Michael Freedman Accountancy; M. J. Murphy & Associates; Petersen Associates; PriceWaterhouseCoopers LLP; ...`

The alphabetical ordering is decisive: between `Kuebler, Thomas & Co` and `Michael Freedman Accountancy`, the list must contain `Los Angeles Area Chamber of Commerce` followed by `Management & Capital Group`. The word `Commerce` was dropped at the line break and the two neighbours fused. Following the hint would mint a fake "chamber of management" that no source supports.

**Both constituents are already in the crosswalk:**
- `LOS ANGELES AREA CHAMBER OF COMMERCE` — under `UNITED STATES CHAMBER OF COMMERCE > CALIFORNIA CHAMBER OF COMMERCE`
- `Management & Capital Group` — top-level canonical

**Conflict with the task instructions.** Task 4541 says "Do NOT route any to a CSV" — but following that literally would create a canonical for an organization that does not exist. Blocking rather than deviating, consistent with Q32-Q44.

**Proposed action (same as the rest of the class):** add `"Los Angeles Area Chamber of Management & Capital Group",1` to `org_names_conjoined.csv`, add nothing to the crosswalk, and close task 4541 with no further crosswalk change.

**Meta:** this is the **ninth** instance, and all nine (Q32/Q33/Q34/Q35/Q38/Q39/Q40/Q44/Q46) are still Open. A single blanket ruling on the class — "lost-separator conjoined straddle, both constituents already present -> route to `org_names_conjoined.csv`, no crosswalk change, no question needed" — would unblock tasks 4126, 4187, 4196, 4205, 4338, 4402, 4407, 4512 and 4541 at once and let future RAs handle these inline instead of spending a round-trip each.

Task 4541 is marked **Blocked** with the assignee cleared; the 19 resolved entries are already committed, so whoever picks it up only needs to handle this 1.

---

### Q45 (Task 4534, RA-Fleet-1) — CLEANING-PATTERN PROPOSAL: `cosponsor` / `co-sponsor` bill-position boilerplate (~72 entries)

**Status:** Open

**How I found it:** Task 4534 added `Leukemia Lymphoma Society (cosponsors)` as an alt spelling. `scripts/clean_name.py` leaves it untouched, so I swept the crosswalk: **72 nodes** carry `co-sponsor`/`cosponsor` bill-position boilerplate. Historical task **-22** stripped `CO-SPONSOR`/`SPONSOR`/`Co-Source`, but it clearly missed the **unhyphenated `cosponsor`** spelling and the phrase forms below. **I have NOT applied anything** — per CLAUDE.md, global regexes need your sign-off.

Three proposed regexes (all case-insensitive). `co-?sponsors?` requires the literal `co`, so bare-`sponsor` entries are deliberately untouched (see near-misses).

**A. Trailing parenthetical — `\s*\(co-?sponsors?\)\s*$` — 10 entries**
- `Leukemia Lymphoma Society (cosponsors)` → `Leukemia Lymphoma Society`
- `California Hospital Association (cosponsors)` → `California Hospital Association`
- `Ca. Law Revision Comm. (Co-sponsor)` → `Ca. Law Revision Comm.`

**B. Trailing appositive phrase — `,\s*(?:a\s+)?co-?sponsors?\s+(?:of|to)\b.*$` — 30 entries**
- `California Medical Association, a co-sponsor of` → `California Medical Association`
- `California Federation of Teachers (CFT), a co-sponsor of this legislation` → `California Federation of Teachers (CFT)`
- `Steinberg Institute, cosponsor of` → `Steinberg Institute`

**C. Leading prefix — `^co-?sponsors?\s+(?:of\s+(?:this|the)\s+bill,\s*)?` — 32 entries**
- `Co-Sponsor AFSCME Local 2700` → `AFSCME Local 2700`
- `Co-sponsor of this bill, A Place for Mom` → `A Place for Mom`
- `Co-Sponsor California Department of Human Resources (CalHR)` → `California Department of Human Resources (CalHR)`

**Near-misses these must NOT match (verified they don't):**
1. `sponsor Abundant Housing LA` — starts with bare `sponsor`, not `co-sponsor`. A separate class; I left it alone rather than widen scope without your call. **Q: do you want bare-`sponsor` prefix/suffix folded in too?**
2. Legitimate trailing acronyms are preserved, not eaten: B on `American Cancer Society Cancer Action Network (ACS CAN), cosponsor of` → `American Cancer Society Cancer Action Network (ACS CAN)` — keeps `(ACS CAN)` per the valid-vs-dirty-affixes rule. 231 other `sponsor`-containing names are untouched by all three.

**9 leftovers these regexes deliberately do NOT cover** (too irregular / would need over-greedy matching — I'd handle these manually as a follow-up task, not by regex):
- `California District Attorneys Association (CDAA - cosponsor)`
- `Governor's Office Of Criminal Justice Planning (OCJP - cosponsor)`
- `California League of Savings (co-sponsor) Institutions` (boilerplate spliced mid-name)
- `Consumer Attorneys of CA (unless (Co-sponsor) amended)`
- `CO-SPONSORS!Orange County Fire Department Chief` / `...Fire Officers Association`
- `Co-Sponsors: Legal Advocates for Permanent`
- `California State Council of Service Employees International Union co-sponsor`
- `State Teachers Retirement System (STRS) - Co-sponsor (staff recommendation)`

**One caveat worth your attention:** stripping renames the node, which orphans the original dirty source string (the [[prefix_strip_orphan]] / task-1716 failure mode). Several of these dirty strings are real rows in the org_names CSVs, so the follow-up task should **keep both the clean and dirty spellings as alts** under the correct canonical rather than just renaming. Many targets (California Hospital Association, California Medical Association, California Professional Firefighters, AFSCME locals) already exist as canonicals, so cleaning will also create merges — worth running `clean_crosswalk.py --dry-run` first.

**Asks:** (1) Approve A, B, C as written (or a subset)? (2) Fold in bare `sponsor` too, or keep out of scope? (3) Confirm the 9 leftovers should be a separate manual task.

---

### Q44 (Task 4512, RA-Fleet-1) — Eighth instance of the lost-separator conjoined class: `JERICHO` + `National Center for Youth Law` — with a same-cell control case

**Status:** Answered — APPROVED (supervisor, 2026-07-16). Route the fused/conjoined string to `org_names_conjoined.csv` (count 1), add nothing to the crosswalk; any prose fragment that names no org → `org_names_invalid.csv`. This is now a **STANDING RULE** (baked into `LEGINFO_IMPORT.md` conjoined section): a lost-separator conjoined straddle whose components are all already in the crosswalk is handled **inline — no blocking, no question**; only escalate if a component is missing. Closes the whole Q32–Q49 lost-separator class.

**Same class as Q32/Q33/Q34/Q35/Q38/Q39/Q40.** Task 4512 (leginfo batch 837) had 9 entries. I added 8 (committed, `3205cdde`). The 9th is another lost-separator conjoined artifact.

**The entry.** Task 4512 hints: `Jericho: National Center on Youth Law` (1) -> alternate spelling of **NATIONAL CENTER FOR YOUTH LAW**.

**Why the hint is wrong — the source cell.** Grepping `leginfo_metadata.csv` for the whole cell shows a `;`-separated supporter list:

> `... California Nurses Association; Child Care Law Center: Children's Advocacy Institute; Daughters of Charity Health System; Jericho: National Center on Youth Law; National Association of Social Workers, California Chapter; Planned Parenthood Affiliates of California; Western Center of Law and Poverty`

`Jericho` and `National Center on Youth Law` are two **separate supporters**; the `;` between them was rendered as `:`. The **same cell contains the identical artifact twice** — `Child Care Law Center: Children's Advocacy Institute` is also two distinct orgs joined by a stray colon. That in-cell control is the smoking gun: the colon is a corrupted separator, not an org-name character.

**Both constituents are already in the crosswalk:**
- `JERICHO: A VOICE FOR JUSTICE` — canonical (real CA legislative-advocacy org; also `JERICHO` as a canonical)
- `NATIONAL CENTER FOR YOUTH LAW` — canonical

**Corroborating evidence.** `org_names_conjoined.csv` already holds ~15 Jericho straddles from the same supporter-list corruption — e.g. `Jericho, Western Center on Law and Poverty`, `Housing California, JERICHO`, `Hotel Employees, Restaurant Employees International Union Jericho`, `Jericho La Raza Centro Legal, Inc., San Francisco`, `County of Stanislaus, JERICHO`. Jericho co-signs big coalition letters, so it lands adjacent to many orgs and eats separators often.

**Note — likely pre-existing bad data.** The crosswalk already contains `California Housing Law Project: Jericho` as an alt of *California Housing Law Project*, and a childless canonical `Doris Tate Crime Victims Bureau National Center for Youth Law`. Both look like this same artifact already folded in. Worth a cleanup task if you agree with the ruling.

**Conflict with the task instructions.** Task 4512 says "Do NOT route any to a CSV" — but following that literally would make a two-org conjoined string an alt spelling of NCYL, asserting NCYL is also known as "Jericho: ...". Blocking rather than deviating, consistent with Q32-Q40.

**Proposed action (same as the rest of the class):** add `"Jericho: National Center on Youth Law",1` to `org_names_conjoined.csv`, add nothing to the crosswalk, and close task 4512 with no further crosswalk change.

**Meta:** this is the **eighth** instance, and all eight (Q32/Q33/Q34/Q35/Q38/Q39/Q40/Q44) are still Open. A single blanket ruling on the class — "lost-separator conjoined straddle, both constituents already present -> route to `org_names_conjoined.csv`, no crosswalk change, no question needed" — would unblock tasks 4126, 4187, 4196, 4205, 4338, 4402, 4407 and 4512 at once and let future RAs handle these inline.

Task 4512 is marked **Blocked** with the assignee cleared; the 8 resolved entries are already committed, so whoever picks it up only needs to handle this 1.

---

### Q43 (Task 4463, RA-Fleet-1) — Cleaning-pattern proposal: `[SPONSOR]` marker — but it is often a CONJOIN SEPARATOR, so a plain strip would be wrong

**Status:** Open

Not blocking — task 4463 is complete and committed. This is a recurring-pattern proposal per the Worker RA Role ("3+ entries sharing the same strippable boilerplate").

**What I found.** `[SPONSOR]` is a leginfo extraction artifact appearing in **5 crosswalk JSON nodes** and **~25 rows across the `org_names_for_cleaning/` CSVs**. It is a bill-position marker (like the already-stripped `(sponsor)` / `CO-SPONSOR`), so it clearly does not belong in an org name.

**The catch — it has two different meanings**, so I do NOT think a single strip regex is safe:

*Case A — trailing/embedded marker on ONE org (safe to strip):*
- `Education Coalition of CA [SPONSOR]` → `Education Coalition of CA`
- `Orange County Employees Assoc. [SPONSOR]` → `Orange County Employees Assoc.`
- `CA Narcotics Officers Association [SPONSOR]` → `CA Narcotics Officers Association`
- `Geil Enterprises, Inc., d.b.a. [SPONSOR] CA Industrial Services` → `Geil Enterprises, Inc., d.b.a. CA Industrial Services` (this one is a single org — the dba name straddles the marker)

*Case B — `[SPONSOR]` acts as a SEPARATOR joining TWO orgs (stripping would silently CREATE a conjoined name):*
- `City of Hesperia [SPONSOR] CA State Association of Counties`
- `City of South El Monte [SPONSOR]American Civil Liberties Union`
- `CA State Association of Counties [SPONSOR]CA Taxpayer's Association`
- `Attorney's Office [SPONSOR] CA Assoc. of Realtors`
- `American Radio Relay League [SPONSOR]Cities of: Claremont, Moreno Valley, ...`

A blind `\s*\[SPONSOR\]\s*` → `` (or → ` `) would turn Case B into plausible-looking but **fake conjoined orgs** (e.g. `City of Hesperia CA State Association of Counties`), which is exactly the failure mode the pattern rules warn about.

**Proposed regex (Case A only — strip only when the marker is at the END):**
```
\s*\[SPONSOR\]\s*$
```
This affects roughly **8-10** entries and cannot produce a conjoined name, because there is nothing after the marker.

**Near-misses it must NOT match** (regex above correctly leaves these intact):
- `City of Hesperia [SPONSOR] CA State Association of Counties` — Case B, marker is mid-string; must stay for manual conjoined-splitting
- `Geil Enterprises, Inc., d.b.a. [SPONSOR] CA Industrial Services` — single org but marker is mid-string; I handled this one manually in task 4463 (kept both the clean and the original dirty spelling as alts)

**My questions:**
1. Approve the end-anchored `\s*\[SPONSOR\]\s*$` pattern for `cleaning_patterns.txt`?
2. For the mid-string Case B entries (~12-15), should I file a task to **split them as conjoined** (each component to the crosswalk, original row to `org_names_conjoined.csv`)? That seems right per CLAUDE.md, but they arrived via leginfo batches whose task text says "do NOT route any to a CSV" — same tension as Q42.

### Q42 (Task 4430, RA-Fleet-1) — `Industrial District Green Ink People` looks conjoined, but the task says "do NOT route any to a CSV"

**Status:** Open

Not a blocker for the rest of task 4430 — I completed the other 87 of 88 entries and committed them. This one entry is **deferred, not added anywhere**, pending your call.

**The conflict.** Task 4430 (leginfo resolution scan batch 735) diagnosed all 88 entries as valid orgs and says *"Do NOT route any to a CSV."* But `Industrial District Green Ink People` (1) appears to be **two unrelated orgs conjoined by the extractor**, and CLAUDE.md's standing rule says conjoined entries go to `org_names_conjoined.csv`. I didn't want to force a wrong merge or override the task text on my own, so I left it out.

**Why I think it's conjoined** — both components already exist in the crosswalk as separate trees:
- `Industrial District Green` — canonical (childless). A downtown-LA greening project; its fiscal sponsor is **Community Partners**, per communitypartners.org / LA2050.
- `Ink People` — `alternate_spelling` under canonical `Ink People Center for Arts and Culture` (Eureka, Humboldt County arts nonprofit).

Web research turned up **no relationship** between the two (no shared fiscal sponsorship, no shared program, ~600 miles apart, unrelated sectors). The string reads like two adjacent supporter-list entries that got joined during extraction.

**My recommendation (option a).** Route `Industrial District Green Ink People,1` to `org_names_conjoined.csv`. Both component orgs are **already in the crosswalk**, so the conjoined rule's "ensure each component is present" is already satisfied — no new canonicals needed, no org names lost.

**Options:**
- **(a)** Move `Industrial District Green Ink People,1` → `org_names_conjoined.csv`. Nothing else to do. ← recommended
- **(b)** It's actually one real org I couldn't find — tell me the correct canonical/placement and I'll add it.
- **(c)** Something else (e.g. attach as an alt under one of the two components — I'd advise against; it would wrongly associate the two).

If (a), this is a one-line follow-up task.

### Q41 (Task 4410, RA-Fleet-3) — Cleaning-pattern proposal: strip trailing `, unless amended`

**Status:** Open

Not a blocker — task 4410 is committed. Filing per the Worker RA "3 or more dirty entries sharing the same strippable boilerplate" rule. **I have not touched `cleaning_patterns.txt`.**

**The pattern.** `, unless amended` is the tail of an `oppose unless amended` stance bleeding into the org name (the leginfo source literally has an `opposition_unless_amended` column). It is never part of an org's real name.

**Proposed regex:** `,\s*unless\s+amended\s*$`  (strip; anchored to end of string, case-insensitive)

**Affected: 5 crosswalk entries** — all `alternate_spelling` nodes, all leginfo additions:
- `Air Products and Chemicals, unless amended` → `Air Products and Chemicals`
- `California Farm Bureau Federation, unless amended` → `California Farm Bureau Federation`
- `San Diego Gas and Electric Company, unless amended` → `San Diego Gas and Electric Company`
- `Southern California Gas Company, unless amended` → `Southern California Gas Company`
- `County Clerks Association, unless amended` → `County Clerks Association` (added by me in task 4410, kept verbatim pending this answer)

**Near-misses it must NOT match** (all safe — the regex is end-anchored and requires the comma + "unless"):
- `CA First Amendment Coalition`, `FIRST AMENDMENT COALITION`
- `Comgro Soil Amendment Inc`, `Californians To Amend 3 Strikes`, `Move to Amend Coalition`

**Precedent this is right:** `narrative_text_mapping_to_orgs.csv` line 261 already maps `Air Products and Chemicals, unless amended` → `Air Products and Chemicals`.

**No orphan risk** (cf. [[prefix_strip_orphan]] / task 1716): `regenerate_org_subsets.py` runs `clean_org_name()` over the **source rows too**, so both sides get cleaned and the source rows still match. Each stripped name collapses into its existing parent/canonical via the normal child-dedup.

**Q1: Approve this regex?** If yes it becomes a normal task (add pattern + run the full clean/dedup/stats pipeline).

**Q2 (scope check — genuinely unsure, so asking rather than guessing):** there's a **larger adjacent family** of stance-artifact strings I found while grepping. These are messier and I did **not** try to write one regex for them — several would need judgment, and a greedy `amended` regex could damage real names. Do you want a separate proposal for any of these, or leave them to per-entry tasks?
- trailing `if amended` — e.g. `Assoc. of CA School Administrators Inyo Placer Plumas Tehama if amended`, `Apartment Assn. of Los Angeles amended`
- bracketed — e.g. `Acclamation Insurance Management Services [as amended]`, `American Civil Liberties Union (As amended April 13)`
- leading `Amended:` / `Amended` — e.g. `Amended: American Federation of Television and Radio Arts`, `Amended Federation State County Municipal Employees (AFSCME)`
- mid-string / lost-separator — e.g. `amended. Wal-Mart`, `Los Angeles (if amended) Workers`, `AMENDMENT REQUESTED: Mojave Town Council`
- the recurring boilerplate `(THIS ANALYSIS REFLECTS AUTHOR'S AMENDMENTS TO BE OFFERED IN)` (appears on several ACLU/Attorney General entries — looks like a clean strippable suffix class of its own)

---

### Q40 (Task 4407, RA-Fleet-3) — Seventh instance of the lost-separator conjoined class: `Natural Resources Defense Council` + `San Diego BayKeeper`

**Status:** Answered — APPROVED (supervisor, 2026-07-16). Route the fused/conjoined string to `org_names_conjoined.csv` (count 1), add nothing to the crosswalk; any prose fragment that names no org → `org_names_invalid.csv`. This is now a **STANDING RULE** (baked into `LEGINFO_IMPORT.md` conjoined section): a lost-separator conjoined straddle whose components are all already in the crosswalk is handled **inline — no blocking, no question**; only escalate if a component is missing. Closes the whole Q32–Q49 lost-separator class.

**Same class as Q32/Q33/Q34/Q35/Q38/Q39.** Task 4407 is a single-entry LEGINFO-CROSSWALK-ADD batch, and its one entry is another lost-separator conjoined artifact, so the batch has nothing to add. Filing so the entry isn't lost.

**The entry.** Task 4407 hints: `Natural Resources Defense Council, San Diego` (1) -> "new or existing" under **Natural Resources Defense Council**.

**Why the hint is wrong.** At face value the string reads as an NRDC San Diego office, and the crosswalk already has `Natural Resources Defense Council, San Francisco` as an `alternate_spelling` — so the obvious move would be to add the San Diego string the same way. The source says otherwise: the trailing `San Diego` is not a city suffix on NRDC, it is the **first half of the next org, `San Diego BayKeeper`**.

**The source proves it — both occurrences.** `Natural Resources Defense Council, San Diego` appears exactly **twice** in `leginfo_metadata.csv`, and in both the very next token is `BayKeeper`:

```
... Heal the Bay, Natural Heritage; Institute, Natural Resources Defense Council, San Diego BayKeeper, Santa Clara Valley Audubon; Society, Sierra Club, ...
```
```
... Councilmember Donna Frye, City of San Diego, Natural Resources Defense Council, San Diego; BayKeeper, Sierra Club, San Diego Chapter, The Ocean Conservancy, 28 individuals
```

The first is unambiguous (`Natural Resources Defense Council` ; `San Diego BayKeeper`). The second is the mangled one that produced the task string: the `;` landed **inside** `San Diego BayKeeper`, so the `;`-split cut it into `... Natural Resources Defense Council, San Diego` and `BayKeeper, Sierra Club, ...`. There is **no occurrence anywhere in the source of an NRDC San Diego office** — the string only ever exists as this straddle.

**Both constituents are already in the crosswalk**, so nothing is lost by routing the string to `org_names_conjoined.csv`:
- `Natural Resources Defense Council` — top-level canonical with many alts (JSON ~line 622648).
- `San Diego Baykeeper` — an `alternate_spelling` under canonical `San Diego Coastkeeper` (~line 758731; the org renamed Baykeeper -> Coastkeeper in 2008).

The two enclosing `;`-cells are **already** routed to `org_names_conjoined.csv` (lines 5247 and 6733), which corroborates the reading.

**Conflict with the task instructions.** Task 4407 says "Do NOT route any to a CSV" — but the one entry is not a valid org, so following that literally would mint a fake NRDC San Diego office. Blocking rather than deviating, consistent with Q32-Q39.

**Proposed action (same as the rest of the class):** add `"Natural Resources Defense Council, San Diego",1` to `org_names_conjoined.csv`, add nothing to the crosswalk, and close task 4407 with no crosswalk change.

**Meta:** this is the **seventh** instance, and all seven (Q32/Q33/Q34/Q35/Q38/Q39/Q40) are still Open. A single blanket ruling on the class — "lost-separator conjoined straddle, both constituents already present -> route to `org_names_conjoined.csv`, no crosswalk change, no question needed" — would unblock tasks 4126, 4187, 4196, 4205, 4338, 4402 and 4407 at once and let future RAs handle these inline.

---

### Q39 (Task 4402, RA-Fleet-3) — Sixth instance of the lost-separator conjoined class: `California Probation, Parole, and Correctional Association` + `Chief Probation Officers of California`

**Status:** Answered — APPROVED (supervisor, 2026-07-16). Route the fused/conjoined string to `org_names_conjoined.csv` (count 1), add nothing to the crosswalk; any prose fragment that names no org → `org_names_invalid.csv`. This is now a **STANDING RULE** (baked into `LEGINFO_IMPORT.md` conjoined section): a lost-separator conjoined straddle whose components are all already in the crosswalk is handled **inline — no blocking, no question**; only escalate if a component is missing. Closes the whole Q32–Q49 lost-separator class.

**Same class as Q32/Q33/Q34/Q35/Q38.** Filing so the entry isn't lost. 8 of the 9 batch-698 entries are committed (`6c11a982`); only this one is open.

**The entry.** Task 4402 hints: `Correctional Association Chief Probation Officers of California` (1) -> alternate spelling of **Chief Probation Officers of California (CPOC)**.

**The source proves it is conjoined — lost separators.** The `support` cell of SB 858 (1995, sales/use taxes), `leginfo_metadata.csv` line 48946, is a ~4,200-char supporter list whose separators were largely flattened to spaces. Its tail reads:

```
... Fremont-Rideout Health Group California Professional Firefighters California State Firefighters Association California Council of Police and Sheriffs California Probation; Parole; and Correctional Association Chief Probation Officers of California
```

Read with separators restored, the tail is: `California Professional Firefighters` ; `California State Firefighters Association` ; `California Council of Police and Sheriffs` ; `California Probation, Parole, and Correctional Association` ; `Chief Probation Officers of California`. The task's string straddles the last two: it is the tail of one org plus the whole of the next.

**Both constituents are already in the crosswalk**, so nothing is lost by routing the string to `org_names_conjoined.csv`:
- `CALIFORNIA PROBATION PAROLE AND CORRECTIONAL ASSOCIATION` — a top-level canonical with many alts.
- `Chief Probation Officers of California (CPOC)` — a top-level canonical with many alts.

**Why the hint is wrong:** applying it would attribute the California Probation, Parole, and Correctional Association's support of SB 858 to CPOC, which is a different organization.

**Why I'm asking instead of just fixing it:** same conflict as Q32-Q38 — the task says *"Do NOT route any to a CSV"*, but CLAUDE.md says conjoined entries go to `org_names_conjoined.csv`.

**Note:** this artifact class is already *in* the crosswalk from earlier batches — `County of Correctional Association Chief Probation Officers of California` is a live top-level canonical, `California Public Defenders Association Chief Probation Officers of California` is another, and bare `Correctional Association` is a third. All from the same flattening. More examples for the sweep proposed in Q35.

**Question — which do you want?**
- **(a)** Route the row to `org_names_conjoined.csv` (both constituents already present, so no crosswalk change needed). Consistent with CLAUDE.md and with Q32/Q33/Q34/Q35/Q38. **My recommendation.**
- **(b)** Apply the hint (alt of `Chief Probation Officers of California (CPOC)`), accepting the misattribution.
- **(c)** Something else — e.g. alt of `CALIFORNIA PROBATION PAROLE AND CORRECTIONAL ASSOCIATION` (the org the lost separator actually belongs to).

**The general ruling suggested in Q34 would close this too**, with no extra round-trip: *when the leginfo source shows a string is a mis-split of a supporter list, route it to `org_names_conjoined.csv` provided every constituent org is already in the crosswalk.* This is now the **6th** blocked entry of this class; a single ruling would unblock tasks 4126, 4187, 4196, 4205, 4338 and 4402 together.

**Where I did NOT block.** The other 8 batch-698 entries were verified against the source and applied/confirmed (committed): `Corona Animal Hospital` (new canonical — AB 302's cell is a clean `;`-separated vet-hospital list), `Coronado City` (AB 726's cell uses the `<Name> City` form), `Cosponsor Office of the State LTC Ombudsman` and `Cosponsor Planned Parenthood Affiliates of California (PPAC)` (leading `Cosponsor` is the leginfo stance label, not part of the name), plus `City of Corona`, `Dublin San Ramon Services District`, `Tenet Healthcare Corporation`, `UC Davis Medical Center` — all already in the crosswalk.

Task 4402 is marked **Blocked** with the assignee cleared; the 8 resolved entries are already committed, so whoever picks it up only needs to handle this 1.


### Q38 (Task 4338, RA-Fleet-2) — Lost-separator conjoined: `California Veterans Board` + `Veterans Advisory Council, County of Orange` — same class as Q32/Q33/Q34/Q35

**Status:** Answered — APPROVED (supervisor, 2026-07-16). Route the fused/conjoined string to `org_names_conjoined.csv` (count 1), add nothing to the crosswalk; any prose fragment that names no org → `org_names_invalid.csv`. This is now a **STANDING RULE** (baked into `LEGINFO_IMPORT.md` conjoined section): a lost-separator conjoined straddle whose components are all already in the crosswalk is handled **inline — no blocking, no question**; only escalate if a component is missing. Closes the whole Q32–Q49 lost-separator class.

**Fifth instance of the Q32-Q35 class.** Filing so the entry isn't lost. 11 of the 12 batch-630 entries are committed (`3d2b531a`); only this one is open.

**The entry.** Task 4338 hints: `California Veterans Board Veterans Advisory Council, County of Orange` (1) -> alternate spelling of **COUNTY OF ORANGE VETERANS ADVISORY COUNCIL**.

**The source proves it is conjoined — a lost `;`.** The whole `support` cell of AB 3155 (1993, veterans benefits), `leginfo_metadata.csv` line 17001, reads:

```
Department of Veterans Affairs; California Veterans Board Veterans Advisory Council, County of Orange
```

Read with the separator restored, the list is three orgs: `Department of Veterans Affairs` ; `California Veterans Board` ; `Veterans Advisory Council, County of Orange`. The `;` between the 2nd and 3rd was lost — the exact mechanism of Q32 and Q34.

**Both constituents are already in the crosswalk**, so nothing would be lost by routing the string to `org_names_conjoined.csv`:
- `California Veterans Board` — a top-level canonical (3 children). It is a real CA state body (governing board of CalVet).
- `Veterans Advisory Council, County of Orange` — already an `alternate_spelling` under `COUNTY OF ORANGE VETERANS ADVISORY COUNCIL` (and already carries 147 bills in `leginfo_added_to_crosswalk.csv`).

**Why the hint is wrong:** applying it would attribute the California Veterans Board's support of AB 3155 to the Orange County council, which did not support it.

**Why I'm asking instead of just fixing it:** same conflict as Q32-Q35 — the task says *"Do NOT route any to a CSV"*, but CLAUDE.md says conjoined entries go to `org_names_conjoined.csv`.

**Note for the Q35 sweep question:** this artifact class is already *in* the crosswalk. `California Veterans Board Veterans Advisory Council` (the same string, minus the Orange County tail) currently sits as an `alternate_spelling` under canonical `California Veterans Advisory Council` — i.e. a previous batch already placed this exact junk the hinted way. Likewise `Alliance California Trial Lawyers Association` is a live top-level canonical produced by the same column-flattening. Both are examples for the sweep proposed in Q35.

**Question — which do you want?**
- **(a)** Route the row to `org_names_conjoined.csv` (both constituents already present, so no crosswalk change needed). Consistent with CLAUDE.md and with the recommendations in Q32/Q33/Q34/Q35. **My recommendation.**
- **(b)** Apply the hint (alt of `COUNTY OF ORANGE VETERANS ADVISORY COUNCIL`), accepting the misattribution.
- **(c)** Something else — e.g. make it an alt of `California Veterans Board` (the leading org, which the lost separator actually belongs to).

**The general ruling suggested in Q34 would close this too**, with no extra round-trip: *when the leginfo source shows a string is a mis-split of a supporter list, route it to `org_names_conjoined.csv` provided every constituent org is already in the crosswalk.* This is now the 5th blocked entry of this class; a single ruling would unblock tasks 4126, 4187, 4196, 4205 and 4338 together.

**A note on where I did NOT block.** Three other batch-630 entries came from the same column-flattening but I judged them *garbled single-org* strings, not conjoined, and applied the hints (committed):
- `California Teamsters Public Commerce Council` -> alt of `California Teamsters Public Affairs Council` ("Commerce" spliced in from `California Chamber of Commerce` in the adjacent column)
- `California Trial Lawyers Alliance Association` -> alt of `CALIFORNIA TRIAL LAWYERS ASSOCIATION` ("Alliance" spliced in from `California Mobilehome Parkowners Alliance`)
- `California Trial Lawyers Reform Association` -> alt of `CALIFORNIA TRIAL LAWYERS ASSOCIATION` ("Reform" spliced in from `Association for California Tort Reform`)

My reasoning: in each the spliced token is a *fragment* of another org, not a whole org, so CLAUDE.md's conjoined definition ("multiple orgs joined together") doesn't fit, and the real org is unambiguous — CLAUDE.md principle 3 says preserve it as an alt of that org. **Please correct me if you'd rather these also go to `org_names_conjoined.csv`** — they are 1-count entries and trivial to move.


### Q36 (Task 4274, RA-Fleet-1) — Cleaning-pattern proposal: leginfo `None` / `None on file.` stance boilerplate

**Status:** Open

Leginfo batch 568 contained three entries where the source string has a literal `None` / `None on file.` spliced onto the end — it's the leginfo stance field rendering "no position on file", not part of the org name. I added the raw strings as alts (to preserve matching) rather than hand-cleaning, since this is a recurring class.

**Proposed patterns (verified against the live crosswalk — 12 entries contain a trailing `None`):**

1. `\s*None on file\.?\s*$` — strips the "None on file." stance rendering.
2. `(?<=\.)\s*None\s*$` — strips a bare `None` that directly follows an abbreviation period.

Before → after:
- `CA Integrated Waste Management Bd.None on file.` → `CA Integrated Waste Management Bd.`
- `CA Manufacturers Assn. None` → `CA Manufacturers Assn.`
- `CA Manufacturers Assn.None` → `CA Manufacturers Assn.`

**Near-misses these must NOT match (all verified intact):**
- `All of Us or None` — a real org (plus `Riverside's All of Us or None`, `Legal Services for Prisoners With Children / All of Us or None`, `All of Us of None`). This is why I did **not** propose a bare `\s+None$`, which would mangle it.
- `OptionONE` — matches `None` case-insensitively across the word boundary; safe because neither pattern is case-insensitive at that position / both require a preceding period.

**Optional 3rd pattern — needs your call.** Two entries have a bare `None` with no preceding period: `City of San Diego None` and `City of San Diego (sponsor) None`. Stripping those needs `(?<!\bor)(?<!\bof)\s+None\s*$`, which I verified leaves all the "All of Us or None" variants intact — but it is more aggressive and could bite a future org legitimately ending in "None". **Approve patterns 1-2 only, or all three?**

### Q37 (Task 4274, RA-Fleet-1) — `Capistrano Valley` is ambiguous; task said not to route to a CSV

**Status:** Open

Leginfo batch 568 (task 4274) listed `Capistrano Valley` (count 1) with the hint "new or existing", and the task says "Do NOT route any to a CSV". But `Capistrano Valley` on its own is a place name, not an org, and the crosswalk already has several orgs it could be truncated from:
- `BOYS & GIRLS CLUBS OF CAPISTRANO VALLEY`
- `Capistrano Valley Christian School`
- `Capistrano Valley High School`
- `Capistrano Valley Water District`

There's no exact `Capistrano Valley` node. I did **not** add it and did **not** log a CSV row for it (nothing was added, so logging it in `leginfo_added_to_crosswalk.csv` would be false). The other 16 entries in the batch are done and committed.

**Question:** Normally the truncated-entry rule sends a genuinely ambiguous name to `org_names_partial.csv`, but this task overrode that. Should `Capistrano Valley` go to `org_names_partial.csv` after all, or do you want it attached to one of the four orgs above (and if so, which)?

### Q35 (Task 4205, RA-Fleet-2) — Two-column PDF merge produces a conjoined SUPPORTER+OPPONENT string — same class as Q32/Q33/Q34, with the smoking gun

**Status:** Answered — APPROVED (supervisor, 2026-07-16). Route the fused/conjoined string to `org_names_conjoined.csv` (count 1), add nothing to the crosswalk; any prose fragment that names no org → `org_names_invalid.csv`. This is now a **STANDING RULE** (baked into `LEGINFO_IMPORT.md` conjoined section): a lost-separator conjoined straddle whose components are all already in the crosswalk is handled **inline — no blocking, no question**; only escalate if a component is missing. Closes the whole Q32–Q49 lost-separator class.

**Fourth instance of the Q32/Q33/Q34 class**, and I found the underlying cause in the raw source. Filing so the entry isn't lost, and because the evidence here may settle the whole class.

Task 4205 asked me to add 9 leginfo orgs. I added 8 (committed, `8fc06493`). I did **not** add this one:

- `American Assoc. of University Women Alliance of CA` (1) — hint: *alternate spelling of* **American Association of University Women - California**

**Why the hint is wrong.** I pulled the original analysis for the source bill (SB 1448, 1993, Assembly Local Government) out of `pubinfo_1993.zip` → `BILL_ANALYSIS_TBL_23537.lob`. Lines 538-546 are a literal **two-column table**:

```
SUPPORT                                 OPPOSITION

American Assoc. of University Women     Alliance of CA Taxpayers & Involved
Assoc. of CA Water Agencies               Voters
Black Gold Cooperative Library System   CA Apartment Assoc.
CA Library Assoc.                       CA Housing Council
CA Municipal Utilities Assoc.           Capistrano Valley Water District
CA State Assoc. of Counties             Paul Gann's Citizens Committee
Cities of:                              Howard Jarvis Taxpayers Assoc.
```

The extractor read this **line by line across both columns**, so every line fused a supporter with an opponent, then inserted `;` at line breaks. That is where `American Assoc. of University Women Alliance of CA` comes from: it is **`American Assoc. of University Women` (SUPPORT) + `Alliance of CA Taxpayers & Involved Voters` (OPPOSITION)**.

So this string is not just conjoined — it fuses two orgs **on opposite sides of the bill**. Making it an alt spelling of AAUW-California would attribute an *oppose*-side org's presence to AAUW.

**Both components are already in the crosswalk:**
- `American Assoc. of University Women` → already an alt under the **American Association of University Women** canonical
- `ALLIANCE OF CALIFORNIA TAXPAYERS AND INVOLVED VOTERS` (ACTIV) → already a canonical

**The crosswalk already contains other victims of this same column merge** (as junk top-level canonicals), which corroborates the mechanism:
- `Assoc. of CA Water AgenciesAlliance of CA Taxpayers and Involved`
- `Assoc. of CA Water AgenciesAlliance of CA Taxpayers and American Assoc. of Retired Persons`
- `Alliance of CA Taxpayers and Involved` / `Alliance of CA Taxpayers & Involved`
- `Taxpayers & Involved Voters`, `California & Involved Voters`

**My question.** Per CLAUDE.md this is textbook `org_names_conjoined.csv` (both components already exist in the crosswalk, so nothing would be lost). But the task says *"Do NOT route any to a CSV"* — the same conflict as Q32/Q33/Q34. Please confirm:

- **(a)** Route `American Assoc. of University Women Alliance of CA` to `org_names_conjoined.csv` (my recommendation — both components are already present), **or**
- **(b)** Something else (e.g. keep as alt of one side).

**Broader question for the whole Q32-Q35 class:** these all share one root cause — the extractor flattening multi-column / separator-less analysis layouts. Would you like a follow-up task to **sweep the crosswalk for the existing junk canonicals this produced** (the `Assoc. of CA Water AgenciesAlliance of CA Taxpayers...` strings above are already sitting in the crosswalk as canonicals) and route them to `org_names_conjoined.csv`? I can also give the leginfo step-2 scan a heads-up rule so future batches stop emitting these hints as "valid orgs".


### Q34 (Task 4196, RA-Fleet-1) — One more "AFL-CIO <Org>" conjoined artifact, plus a prose fragment that names no org

**Status:** Answered — APPROVED (supervisor, 2026-07-16). Route the fused/conjoined string to `org_names_conjoined.csv` (count 1), add nothing to the crosswalk; any prose fragment that names no org → `org_names_invalid.csv`. This is now a **STANDING RULE** (baked into `LEGINFO_IMPORT.md` conjoined section): a lost-separator conjoined straddle whose components are all already in the crosswalk is handled **inline — no blocking, no question**; only escalate if a component is missing. Closes the whole Q32–Q49 lost-separator class.

**Third instance of the Q32/Q33 class** — the leginfo hint says place/create it, and the task says *"Do NOT route any to a CSV"*, but the source proves the string isn't an org. Answering Q33 answers entry 1 below; entry 2 is a related but distinct case. Filing so the 2 entries aren't lost.

Task 4196 asked me to add 9 batch-488 orgs. I added 4 and found 3 already present (committed, `921d33b3`). The other 2:

**1. `Afl-cio Faculty Association of California Community Colleges` (1)** — hinted as alternate_spelling under node **Faculty Association of California Community Colleges**.

This is the **exact mechanism of Q33 entry 1** (an intra-name comma became a separator). The dirty source cell:
```
... California Federation of Teachers Afl-cio California Labor Federation; Afl-cio Faculty Association of California Community Colleges
```
The **same list appears elsewhere in `leginfo_metadata.csv` with separators intact**:
```
... California Federation of Teachers Afl-cio; California Labor Federation, Afl-cio; Faculty Association of California Community Colleges
```
So the string is the `Afl-cio` tail of **`California Labor Federation, AFL-CIO`** glued to the head of **`Faculty Association of California Community Colleges`**. Corroborating: across all 1,068 source occurrences, FACCC is otherwise **always** preceded by a clean `; ` separator, and FACCC is an independent professional association — it is not an AFL-CIO affiliate, so "Afl-cio FACCC" is not a real alternate spelling.

Both constituents are already in the crosswalk (`California Labor Federation, AFL-CIO` ✓; `Faculty Association of California Community Colleges` ✓ under `Association of California Community Colleges`), so nothing is lost by routing the row to `org_names_conjoined.csv` — which **already holds 12+ rows of this exact shape**, including the same lowercase form: `Afl-cio Consumer Attorneys of California United Food and Commercial Workers,1`, `AFL-CIO California Tax Reform Association,5`, `AFL-CIO Cement Masons,4`.

**2. `Advocates for Reasonable Treatment` (1)** — hinted as **new canonical**.

Not an org. Its single source occurrence is a **truncated prose sentence** sitting in the `opposition` column of one row (AB 255, 2025 — the Supportive-Recovery Residence Program), and that is the entire cell:
```
Advocates for Reasonable Treatment are supportive of the goal of this bill but are
```
The plural verb ("advocates ... **are** supportive") shows it's a common-noun phrase — i.e. *people who advocate for reasonable treatment* — not a named entity. Web search finds no organization by this name (nearest hits are the unrelated Treatment Advocacy Center / Patient Advocate Foundation), and it never appears as a `;`-delimited list item anywhere in the source. Creating it as a canonical would invent an org and misattribute a bill count.

Per CLAUDE.md this is narrative-embedded prose that names **no** org, so the rule points to `org_names_invalid.csv` (**not** `narrative_text_mapping_to_orgs.csv`, whose `mapped_org` is never blank — there's no org to map it to).

**Question — which do you want?**
- **(a)** Route entry 1 to `org_names_conjoined.csv` and entry 2 to `org_names_invalid.csv`. Constituents for entry 1 are already present, so no crosswalk change is needed for either. Matches CLAUDE.md + the existing conjoined-CSV precedent. **My recommendation** (and consistent with RA-Fleet-3's recommendation in Q33).
- **(b)** Follow the hints: add entry 1 as an alt of FACCC and entry 2 as a new canonical. I'd advise against both — they'd inject junk names and misattribute counts.
- **(c)** Something else.

**Suggested general ruling** (would close Q32, Q33 and Q34 together, and unblock this whole recurring class without another round-trip): *when the leginfo source shows a string is a mis-split of a supporter list (a separator lost at an intra-name comma), the "Do NOT route to a CSV" instruction is overridden by CLAUDE.md — route it to `org_names_conjoined.csv` provided every constituent org is already in the crosswalk.*

Task 4196 is marked **Blocked** with the assignee cleared; the 4 adds are already committed, so whoever picks it up only needs to handle these 2.


### Q33 (Task 4187, RA-Fleet-3) — Two "AFL-CIO <Org>" entries are conjoined artifacts, not new canonicals — same class as Q32

**Status:** Answered — APPROVED (supervisor, 2026-07-16). Route the fused/conjoined string to `org_names_conjoined.csv` (count 1), add nothing to the crosswalk; any prose fragment that names no org → `org_names_invalid.csv`. This is now a **STANDING RULE** (baked into `LEGINFO_IMPORT.md` conjoined section): a lost-separator conjoined straddle whose components are all already in the crosswalk is handled **inline — no blocking, no question**; only escalate if a component is missing. Closes the whole Q32–Q49 lost-separator class.

**This is the same question as Q32** (leginfo hint says "new canonical" + "Do NOT route any to a CSV", but the source proves the string is conjoined). Answering Q32 almost certainly answers this too — I'm filing it so the 2 entries aren't lost.

Task 4187 asked me to add 5 batch-477 orgs. I added 3 (committed, `769cf51c`). The other 2 were both hinted as **new canonicals**, but neither is a real organization:

**1. `AFL-CIO California State Legislative Board` (1)**

The batch-477 supporter list reads:
```
California Labor Federation; AFL-CIO California State Legislative Board; Smart - Transportation Division California Teamsters Public Affairs Council
```
The **same list appears elsewhere in `leginfo_metadata.csv` with separators intact**:
```
... California Labor Federation, AFL-CIO; California State Legislative Board of Sheet Metal, Air, Rail and Transportation Workers; Transportation Division; ...
```
So the string is the tail of **`California Labor Federation, AFL-CIO`** glued to the head of **`California State Legislative Board of SMART-TD`** (an intra-name comma became a separator). Note the third item in the dirty list is *also* visibly conjoined (`Smart - Transportation Division` + `California Teamsters Public Affairs Council`), confirming the mis-split.

**2. `AFL-CIO International, Inc` (1)**

Raw source cell (a Prop 51 / tort-reform **defense-side** supporter list — accountancies, chambers, businesses; the AFL-CIO would be a very odd member):
```
Aerospace Dynamics; AFL-CIO International, Inc; Arthur Andersen & Co. LLP; CA Association for Tort Reform; CA Business Properties Assn; CA Chamber of Commerce; ...
```
**`Aerospace Dynamics International, Inc`** is a real Valencia, CA company and appears **14×** in the source with separators intact, e.g.:
```
Adams-Campbell Co; Aerospace Dynamics International, Inc; Arete Precision, Inc; Autronics Corporation; ...
```
The list is alphabetical (`Aerospace…` < `AFL-CIO` < `Arthur Andersen`), so the true items are `Aerospace Dynamics International, Inc` and `AFL-CIO`; the `International, Inc` tail got attached to the wrong item. There is no entity called "AFL-CIO International, Inc".

**Why I'm asking instead of just fixing it:** same conflict as Q32 — the task says *"Do NOT route any to a CSV"*, but CLAUDE.md says conjoined entries go to `org_names_conjoined.csv`.

**Relevant precedent (unlike Q32, this class already has one):** `org_names_conjoined.csv` **already** contains this exact `AFL-CIO <NextOrg>` shape, including a near-identical sibling of entry 1:
- `AFL-CIO California State Legislative Board of Sheet Metal,1`  ← same string, more of the tail
- `AFL-CIO California Tax Reform Association,5`
- `AFL-CIO California State Council of Laborers,3`
- `AFL-CIO Judy Michaels,6`

No `AFL-CIO <Org>` string is currently placed as an alt/canonical in the crosswalk, so — unlike Q32 — there's no competing convention to undo.

**All constituent orgs are already in the crosswalk**, so nothing is lost by routing the 2 strings to `org_names_conjoined.csv`: `California Labor Federation, AFL-CIO` ✓, `California State Legislative Board of Sheet Metal…` ✓ (under SMART), `AFL-CIO` ✓, `Aerospace Dynamics International, Inc` ✓.

**Question — which do you want?**
- **(a)** Route both rows to `org_names_conjoined.csv` (constituents already present; no crosswalk change needed). Matches CLAUDE.md + the 4 existing precedent rows. **My recommendation.**
- **(b)** Follow the hints and create both as new canonicals — I'd advise against: both are junk names and would misattribute bill counts.
- **(c)** Something else.

Task 4187 is marked **Blocked** with the assignee cleared; the 3 unambiguous orgs are already committed, so whoever picks it up only needs to handle these 2.


### Q32 (Task 4126, RA-Fleet-2) — "Women For: <Org>" entries are conjoined, not alternate spellings
**Status:** Answered — APPROVED (supervisor, 2026-07-16). Route the fused/conjoined string to `org_names_conjoined.csv` (count 1), add nothing to the crosswalk; any prose fragment that names no org → `org_names_invalid.csv`. This is now a **STANDING RULE** (baked into `LEGINFO_IMPORT.md` conjoined section): a lost-separator conjoined straddle whose components are all already in the crosswalk is handled **inline — no blocking, no question**; only escalate if a component is missing. Closes the whole Q32–Q49 lost-separator class.

Task 4126 asked me to add 8 leginfo orgs. I added 5 (committed, `05b9450e`). I did **not** add the other 3, because the leginfo source shows the task's placement hints would inject bad data:

- `Women FOR: Commission on the Status of Women` (2) — hint: alt of **CALIFORNIA COMMISSION ON THE STATUS OF WOMEN**
- `Women For CJA` (2) — hint: **new canonical** "Women For CJA"
- `Women For: Sonoma County Public Health Department` (2) — hint: alt of chapter **Sonoma County Public Health Department**

**Evidence — the same supporter list appears in `leginfo_metadata.csv` both ways.** The lists are `;`-separated, and `Women For` is its own list item:

```
... Older Women's League of California; Women For; CJA; National Council of Jewish Women; ...   <-- separator intact
... Older Women's League of California; Women For CJA; National Council of Jewish Women; ...    <-- separator lost
```
```
... California State Employees' Association; Women FOR: Commission on the Status of Women; California State Student Association; ...
... Women For; Commission on the Status of Women; California State Student Association; ...     <-- same list, separator intact
```

So these strings are **conjoined**: the org `Women For` (a real LA/Beverly Hills org — already in the crosswalk as canonical `WomenFor` > `Women For`, plus `Women For (Beverly Hills)`) glued to the next org in the list. `Women For CJA` is **not** an org at all — it is `Women For` + `CJA` (already an alt of `California Judges Association`). Creating it as a canonical would be a junk entry.

**Why I'm asking instead of just fixing it:** (1) the task says *"Do NOT route any to a CSV"*, but CLAUDE.md says conjoined entries go to `org_names_conjoined.csv` — direct conflict; (2) this is a **recurring class**, and entries already in the crosswalk were placed the hinted way, so a decision here is retroactive:
- `Women for: California Women's Law Center` — currently alt under `CALIFORNIA WOMEN'S LAW CENTER`
- `Women For: Women's Energy Matters` — currently alt under `Women's Energy Matters`
- `Women for: Orange County, Irvine` — currently a **top-level canonical** (+ child `WOMEN FOR: ORANGE COUNTY`)

**Question — which do you want?**
- **(a)** Treat all `Women For[:;] <Org>` strings as conjoined: route the 3 rows to `org_names_conjoined.csv`, make sure both `Women For` and `<Org>` are in the crosswalk, and file a follow-up task to re-do the ~3 already-placed entries above the same way.
- **(b)** Keep the existing convention (alt of the trailing org) and add the 3 as hinted, accepting that `Women For`'s bill counts get misattributed and that `Women For CJA` becomes a canonical.
- **(c)** Something else — e.g. keep them as alts of `Women For` itself (the leading org), since that is the item the separator actually belongs to.

My recommendation is **(a)**, with the `Women For CJA` row specifically **not** becoming a canonical under any option. Task 4126 is marked **Blocked** with the assignee cleared; the 5 unambiguous orgs are already committed, so whoever picks it up only needs to handle these 3.


### Q31 (Task 4117, RA-Fleet-2) — Cleaning-pattern proposal: trailing "…, sponsor of this measure / of the bill"
**Status:** Open

While doing task 4117 I hit `Three Valleys Municipal Water District, sponsor of this measure` and found this is a **recurring** class, not a one-off: **40 crosswalk entries + 92 CSV rows**. `cleaning_patterns.txt` already strips trailing sponsor tags, but only where the string *ends* at the word "sponsor" (lines 54-55, 62) or where it's parenthesized (lines 7-9, 35). Nothing handles a trailing **prepositional phrase** — `sponsor **of the bill**`. So these 40 stay dirty today. Per CLAUDE.md I'm not editing `cleaning_patterns.txt` myself — requesting sign-off.

**Proposed regex** (deliberately modeled on the existing line 62 so the two read as a family):
```
(?i)[\s,/"-]+(?:is\s+|also\s+)?(?:(?:a|the|one\s+of\s+the)\s+)?(?:principal\s+)?(?:co[\s-]?)?sponsors?\s+of\s+(?:this|the)\s+(?:measure|bill|legislation|act|resolution)s?\s*$
```
It strips a trailing "(a/the) (co-)sponsor of this/the measure|bill|legislation|act|resolution". Anchored to `$` only — never touches mid-string text.

**Example matches (before → after):**
- `Crown Castle, sponsor of this measure` → `Crown Castle`
- `California Federation of Teachers, a co-sponsor of the bill` → `California Federation of Teachers`
- `California School Employees Association (CSEA), AFL-CIO, a sponsor of this measure` → `California School Employees Association (CSEA), AFL-CIO`

**Near-misses it must NOT match (verified it does not — real orgs with "Sponsor" in the name):**
- `Borrego Sponsor Group`, `Jacumba Sponsor Group`, `Pala-Pauma Sponsor Group` (San Diego County planning groups — "Sponsor Group" is their actual name)
- `Asylum Sponsorship Project`, `California Housing Sponsorship`, `Hollywood Interfaith Sponsoring Committee`
- `American Society of Composers, (sponsor) Authors and Publishers` (mid-string, unanchored → untouched)

I ran it across all 210,697 crosswalk names: **40 match, all genuine bill-relationship metadata; 0 false positives.**

**Three things to flag before you approve:**
1. **8 of the 40 clean up to a string that is already an existing canonical** (e.g. `California Nurses Association, sponsor of the bill` → `California Nurses Association`). `clean_crosswalk.py` will merge those automatically — intended, but it means the pattern is a real consolidation, not just cosmetic.
2. **Stripping does not by itself make an entry valid.** Two cleaned results are named officials needing per-case routing under [[named_officials_per_case]]: `Mayor Eric Garcetti, City of Los Angeles` and `State Insurance Commissioner Ricardo Lara` (both leadership → alt of the body). Should I file a follow-up task for those, or leave them?
3. **Scope question — adjacent variants I deliberately left OUT** because they'd widen the regex past what I can verify safely. Want them in, or a separate proposal?
   - dangling/truncated: `…, a co-sponsor of`, `…, cosponsor of`, `…, sponsors of` (no object — arguably partials)
   - different preposition: `…, sponsor to the bill`, `…, the sponsor for the bill`
   - bill-numbered: `State Center CCD, the sponsor of AB 2104 (Soria)`

If approved I'll file it as a normal task: add the pattern, run the full clean/dedup/stats pipeline.

### Q30 (Task 3846, RA-Fleet-2) — PART B university sub-unit nesting needs a parent-canonical ruling before it can run safely
**Status:** Answered
**Answer (supervisor, relayed by Management-Assistant-2, 2026-07-04):** Approve all three of RA-Fleet-2's proposals. (1) CREATE each of the ~15 missing parent institutions as a new top-level canonical, then nest the sub-unit under it. (2) Use the shortest existing clean UC-campus parent form where one exists (`UCLA`, `UC Berkeley`, `UCSF`, `UC Davis`, `UC San Diego`, `University of California, Merced`, `University of California Santa Cruz`, `UC Hastings College of the Law`); CREATE `UC Riverside`, `University of California, Irvine`, and `University of California Global Health Institute` where none exists. Leave the `UC Davis`/`University of California, Davis` and `UC San Diego`/`University of California, San Diego` duplicate parent trees as-is — a dedicated global-dedup task (newly created) will merge them. (3) For sub-units mapping to multiple duplicate canonicals, nest ALL matching variants under the campus parent (global-dedup will merge later); SKIP truly unresolvable shorthand (e.g. UCSD 'Center for Public') and list what was skipped in the commit message. Task 3846 unblocked (set to Not Started).

Task 3846 says "find each sub-unit's parent university/college canonical and MERGE-append it as a `chapter`." Executing it, I found the task presumes the parents exist, but many don't, and the UC campuses have **duplicate/inconsistent** parent trees. Before touching ~90 canonicals I need three rulings:

**1. ~15 parent institutions have NO canonical at all.** These sub-units' parents genuinely don't exist anywhere in the crosswalk:
- Pasadena City College · Rio Hondo College · Norco College · Santa Ana College · Mission College · Saint Mary's College of California · Springfield College · University of Toronto · Ohio State University · Indiana State University · Yeshiva University · University of Michigan · University of Vienna · University of Kansas · University of Alabama at Birmingham (UAB)

Per CLAUDE.md principle #1 I read this as: **create each missing parent as a new top-level canonical, then nest the sub-unit under it.** Confirm? (e.g. create `University of Michigan` → nest `Institute for Social Research, University of Michigan` as a chapter.)

**2. UC campus parent form is duplicated/inconsistent — which exact string is the parent for each campus?** Current state:
- UCLA → `UCLA` exists (clean)
- UC Berkeley → `UC Berkeley` exists (clean)
- UCSF → `UCSF` exists (clean)
- UC Davis → BOTH `UC Davis` **and** `University of California, Davis` exist as separate trees
- UC San Diego → BOTH `UC San Diego` **and** `University of California, San Diego` exist as separate trees
- UC Merced → `University of California, Merced` exists
- UC Santa Cruz → `University of California Santa Cruz` (no comma) exists; no `UC Santa Cruz` parent
- UC Irvine → **no clean parent** (only a `University of California Irvine` stub + scattered `University of California, Irvine …` fragments)
- UC Riverside → **no clean parent**
- UC Hastings → `UC Hastings College of the Law` and `University of California Hastings College of the Law` both exist
- UCGHI → no clean parent (only two `University of California Global Health Institute …` center canonicals)

My proposed rule: use the shortest existing clean form where one exists (`UCLA`, `UC Berkeley`, `UCSF`, `UC Davis`, `UC San Diego`, `University of California, Merced`, `University of California Santa Cruz`, `UC Hastings College of the Law`); **create** `UC Riverside`, `University of California, Irvine`, and `University of California Global Health Institute` where none exists. (The `UC Davis` vs `University of California, Davis` and `UC San Diego` vs `University of California, San Diego` duplicate trees are a pre-existing issue I'd leave for global-dedup — I'd just pick the short form.) Confirm the form per campus, or tell me which to canonicalize on.

**3. The grouped worklist section (lines 73-86) is partly unresolvable/ambiguous.** Several shorthand entries don't map cleanly:
- UCSD "Center for Public" → no clean match in the JSON
- UCSF "Diabetes Center" and "Department of Psychiatry" each map to **two** duplicate canonicals (`UCSF Diabetes Center` **and** `University of California, San Francisco Diabetes Center`); same for Psychiatry
- Many campus sub-units exist under 2-3 naming variants each

Proposal: for duplicates, nest **all** matching variants under the campus parent (global-dedup will later merge them); **skip** truly unresolvable shorthand (e.g. UCSD "Center for Public") and I'll list what I skipped. OK, or should the grouped section (lines 73-86) be split into its own separately-enumerated task with exact canonical strings?

**Recommendation:** approve #1 (create missing parents) + #2 (my per-campus form list) + #3 (nest-all-variants, skip-unresolvable) and I'll run the whole of PART B in one pass. Task 3846 left **Blocked**, assignee cleared, pending this ruling.

### Q29 (Task 3582, RA-Fleet-2) — Task 3582 directly conflicts with task 3587 on where LASPOA (`Los Angeles School Peace Officers Association`) belongs
**Status:** Answered

**Answer (supervisor, relayed by Management-Assistant-2, 2026-07-02):** Option (b) — KEEP the `Police Officers Association` umbrella (158 chapters; same model as the POLICE DEPARTMENT umbrella). Fold LASPOA's Peace/Police variants INTO the umbrella's `LOS ANGELES SCHOOL POLICE OFFICERS ASSOCIATION` chapter (per task 3582). Task 3587 had made LASPOA an independent top-level canonical — that is REVERSED: re-nest `Los Angeles School Peace Officers Association` + `LA School Peace Officers Association` as `alternate_spelling`s under that umbrella chapter. Task 3614's 'junk parent / promote out' premise is OVERRULED — the umbrella stands; 3614 is narrowed to just fold the LA & San Diego School POA Peace/Police OCR variants into their respective umbrella chapters and route emptied generic stubs to partial. Tasks 3582 & 3614 unblocked (Not Started).

Tasks **3582** and **3587** are both Not Started and give **contradictory instructions for the exact same two entities** (`Los Angeles School Peace Officers Association` and `LA School Peace Officers Association`):

- **Task 3582** ([POA-CONSOLIDATE MA-2]): "'Peace' vs 'Police' is the same LAUSD school-police union, LASPOA" → **fold both** `LA School Peace Officers Association` and `Los Angeles School Peace Officers Association` as `alternate_spelling`s **INTO the existing chapter** `LOS ANGELES SCHOOL POLICE OFFICERS ASSOCIATION`, which is nested under the generic `Police Officers Association` umbrella.
- **Task 3587** (supervisor-flagged, MA web-verified): LASPOA is the **INDEPENDENT** rank-and-file union and is **NOT a chapter of any parent union — do NOT nest it**. Keep `Los Angeles School Peace Officers Association` as its **own top-level canonical**, with `LA School Peace Officers Association` folded in as its `alternate_spelling`.

So 3582 wants LASPOA nested under the `Police Officers Association` umbrella (merged with the "Police" spelling), while 3587 wants it kept independent (separate from the "Police" umbrella chapter). Both cannot be done. Current JSON state: `LA School Peace Officers Association` (line ~537677) and `Los Angeles School Peace Officers Association` (line ~573331) are both childless top-level canonicals; the umbrella chapter `LOS ANGELES SCHOOL POLICE OFFICERS ASSOCIATION` already exists under `Police Officers Association` (line ~719650).

**Question:** Which model wins?
- **(a)** Follow **3587** (LASPOA independent): keep `Los Angeles School Peace Officers Association` top-level, fold `LA School Peace Officers Association` into it, and **close/void 3582** (or narrow 3582 to only handle the generic `School Police Officers Association` / `Schools Peace Officers Association` fragments). This is my recommendation since 3587 is the later supervisor-flagged web-verified correction.
- **(b)** Follow **3582** (fold LASPOA "Peace" spellings into the umbrella's `LOS ANGELES SCHOOL POLICE OFFICERS ASSOCIATION` chapter), and adjust 3587 accordingly.

Task 3582 left **Blocked** pending this answer.

### Q28 (Task 3454, RA-Fleet-1) — Batch-26 FINAL "strip dirty tails/prefixes from children" hits the identical already-folded/orphan situation as Q27
**Status:** Answered
**Answer (supervisor, relayed by Management-Assistant-2, 2026-06-25):** Option (a) — these are already done. The targeted dirty strings are correctly preserved as orphan-safe `alternate_spelling`/`chapter` children under their right canonicals, which is the desired end state (preserve org names as alts; hand-stripping would orphan CSV source rows). No per-item work needed. Task 3454 closed as Done. (The only orphan-safe global strip would be a `cleaning_patterns.txt` regex; none required here — these dirty bits are heterogeneous one-offs, not a clean regex class.)

Task 3454 ("Strip narrative tails / bill-position prefixes from children (Pass-2 batch 26, FINAL)") is the same class as task 3415 (see Q27). I verified every targeted entry against the live JSON + CSVs:

- The dirty-**canonical** examples in the task (`...SB 1973 (Wesson)`, `Worksafe. AB 1249 (House)`, `WORKSAFE! AB 1127 (Steinberg)`, `The Women's Zionist Organization of SB 1555 (Speier) America`, `: GOVERNOR'S VETO MESSAGE` tails) are **NOT FOUND** — already removed by prior passes + global dedup.
- The dirty entries that DO still exist are **already correctly folded as `alternate_spelling`/`chapter` children under their correct canonical**, e.g.:
  - `Wine Institute Allison Jordan`, `Wine Institute 33 wineries`, `City of Wine Institute` → alts of **Wine Institute**
  - `Womens Cancer Resource Center: Arlene Wong` → alt of **Women's Cancer Resource Center**
  - `Wireless Radiation Education & Defense Www.gmoscience.org` → alt of **Wireless Radiation Education and Defense**
  - `Yimbyadu, INC. "jumpstart Adu` → alt of **Yimbyadu, Inc. dba. Jumpstart ADU**
  - `Zayante Fire Protection District. Jim Andresen` → alt of **Zayante Fire Protection District**
  - `Western Center on Law and Poverty (co-spource)` → alt of **Western Center on Law and Poverty**
  - `YWCA of Silicon Valley (25 letters from different individuals)` → alt of **YWCA of Silicon Valley**
  - `Youth Leadership Institute: Letters from Individuals` → alt of **Youth Leadership Institute**
- Each of these dirty strings is a **real source row in `org_names_in_crosswalk.csv`**, so hand-removing the alt to "collapse to the bare canonical" would orphan the source row to `not_in_crosswalk.csv` (per [[prefix_strip_orphan]] / task 1716). They are therefore already in the correct, **orphan-safe** end state required by CLAUDE.md General Principle 3 ("preserve org names — make the dirty entry an alternate spelling of the real org"). The only orphan-safe way to actually strip them is a `cleaning_patterns.txt` regex (strips JSON name + CSV source uniformly), which needs supervisor sign-off.

**This is the same decision as Q27.** Please rule once for the whole batch-26 FINAL class: **(a)** leave these dirty strings folded as alts and **close tasks 3454/3415 as already-done** (recommended — it matches the preserve-as-alt principle and is orphan-safe), or **(b)** want specific recurring prefixes/tails proposed as `cleaning_patterns.txt` regexes (e.g. `^City of `-on-a-child, `(NN letters from different individuals)$`, `: Letters from Individuals$`) for orphan-safe global stripping? I've left task 3454 **Blocked** pending this answer.

### Q27 (Task 3415, RA-Fleet-1) — Cleaning-pattern proposal: leading "Neutral:" stance prefix + how to treat already-folded dirty alts
**Status:** Answered
**Answer (supervisor, relayed by Management-Assistant-2, 2026-06-25):** These are already done — leave the already-folded dirty alts in place (orphan-safe, correctly mapped source→canonical) and close task 3415 as Done. On Q27 Q2 (mid-string bill tokens, "Per Senate/Assembly Committee" prefixes, one-off tails, narrative tails already in the narrative CSV): leave them in place too — all are correctly classified and orphan-safe. On Q27 Q1 (the `^Neutral:` regex): verified safe and not overly broad (22 JSON + 22 CSV matches, all bill-stance boilerplate on real orgs; colon-anchored so "Neutral Posture"/"Neutral Bay" are left intact) — it is **pre-approved** as an optional global cleanup, but NOT being run right now (the backlog is fully drained and the fleet is idle; spinning it up for one cosmetic pattern isn't worth it). It can be applied any time as a standalone (add the regex to `cleaning_patterns.txt`, run clean/dedup/stats) if/when desired.

While working task 3415 ("strip narrative tails / bill-position prefixes from children"), I found that **almost all the dirty entries the task targets are ALREADY folded as `alternate_spelling` children under their correct canonicals** (e.g. "Neutral: California Medical Association" is already an alt of "California Medical Association"; the Sierra Club bill-position alts "Sierra Club: Local Governments", "Sierra Club (coho provisions)", "Sierra Club OPPOSED::" are already alts of SIERRA CLUB). The verbatim dirty-*canonical* examples in the task ("Sheriff, AB 854 (Koretz)...", "California SB 453 (Poochigian)...", "SB Sierra Club", etc.) are **NOT FOUND** — already folded/removed by prior passes + global dedup.

These dirty strings are **real source rows in `org_names_in_crosswalk.csv`** (e.g. `Neutral: California Medical Association,1`). Per [[prefix_strip_orphan]] (task 1716 lesson), **hand-renaming them in the JSON to strip the prefix would orphan the source row to `not_in_crosswalk.csv`** — so they should be left as alts UNLESS stripped via a `cleaning_patterns.txt` regex (which strips the JSON name AND the CSV source string uniformly, no orphan). So per CLAUDE.md I'm proposing a pattern rather than hand-editing.

**Proposed pattern — leading "Neutral:" stance prefix (same class as the already-approved "Support."/"SUPPORT."/"Concerns:" prefixes at cleaning_patterns.txt lines 222–227):**

Regex (anchored to START, case-insensitive, tolerates a space before the colon):
```
(?i)^\s*NEUTRAL\s*:\s*
```
Strips the bill-stance annotation "Neutral:" that legislative analyses prepend onto a real org name.

**Examples (before → after):**
- `Neutral: California Medical Association (CMA)` → `California Medical Association (CMA)`
- `NEUTRAL: City of Long Beach` → `City of Long Beach`
- `Neutral : Center for Public Interest Law` → `Center for Public Interest Law`

**Near-misses it must NOT match (left intact — no colon, so safe):**
- `Neutral Posture` (ergonomic furniture company)
- `Neutral Bay ...` (place-name orgs)

**Affected count:** ~20 crosswalk JSON entries + ~23 CSV rows (≈30 unique strings after dedup). All are bill-stance boilerplate on identifiable orgs; residue after stripping merges with the existing canonical via the normal dedup step.

**Two questions:**
1. **Approve the `^Neutral:` pattern?** If yes, it becomes a follow-up task (add the regex, run clean/dedup/stats), like Q26→task 3247.
2. **The remaining task-3415 dirty alts** — mid-string bill tokens ("AB 563 - Honda", "SCA 14 - Morrow ..."), "Per Senate/Assembly Committee" prefixes (~6), one-off tails (" strongly", "(conceptual support)", "OPPOSITION"), and "...is proud" narrative tails (already in `org_names_embedded_in_narrative_text.csv`) — are **already correctly classified** as alts under the right canonical (or already in the narrative CSV). They map source→canonical correctly and are orphan-safe as-is. **OK to leave them in place** (and close task 3415 once the Neutral: pattern lands), or do you want each handled individually in a follow-up? I've left task 3415 **Blocked** pending this answer.

**[RA-Fleet-2, 2026-06-25] Question 2 also governs task 3454** (Pass-2 batch 26 FINAL, the W-Y-Z analogue of 3415). I verified every dirty entry that task targets is already correctly folded as an `alternate_spelling`/`chapter` under its correct clean canonical (e.g. "Wine Institute Allison Jordan"/"Wine Institute 33 wineries"/"City of Wine Institute" → WINE INSTITUTE; "Zayante Fire Protection District. Jim Andresen" → Zayante FPD; "Womens Cancer Resource Center: Arlene Wong" → Women's Cancer Resource Center; "Wireless Radiation Education & Defense Www.gmoscience.org" → Wireless Radiation Education and Defense; "Yimbyadu, INC. \"jumpstart Adu" → Yimbyadu; "Dairymen, Yuba-Sutter County Farm Bureau" → Yuba-Sutter County Farm Bureau; "Bros., Inc., Zanker Road Landfill" → Zanker Road Landfill; "Western Center on Law and Poverty (co-spource)" → Western Center on Law and Poverty; "Wright Chapel … Several individuals" → Wright Chapel AME Church; "YWCA of Glendale (Two individuals)" → YWCA tree). The bill-token dirty *canonicals* (Zionist…SB 1555, Working Alternatives…SB 1973, Worksafe…AB 1249/AB 1127) and the ": GOVERNOR'S VETO MESSAGE" tails are already gone (prior passes + global dedup). All survivors are orphan-safe alts ([[prefix_strip_orphan]]). Task 3454 is **Blocked** on the same answer: leave-in-place → close both 3415 and 3454; handle-individually → both need a follow-up. (No new pattern needed for 3454 — its dirty bits are heterogeneous one-offs, not a clean regex class.)

### Q26 (Task 3207, RA-Fleet-3) — Cleaning-pattern proposal: bill-position / legislative prefixes
**Status:** APPROVED — Mgmt-Asst-2 independent verification (2026-06-21). Re-ran all 5 regexes against the full crosswalk (213,171 entries): **81 matches, 0 false positives, 0 empty results**, and every named near-miss confirmed NOT matched (Page and Turnbull, Page Avenue Mutual Water Company, Sponsors for Educational Opportunity, In-Home Supportive Services, SBC, Page Mill Partners). **Approve all 5 patterns, including pattern (5) OPPOSITION (UNLESS AMENDED)** (1 entry, but same class + zero risk → include for full coverage). Execute via follow-up **task 3247** (add the 5 regexes to `cleaning_patterns.txt`, run the full clean/dedup/stats pipeline; residue fragments like "Sponsor American Federation of State" → "American Federation of State" will dedup/merge or route to partial as normal). **On the bare `^Page <Org>` / `^SB <Org>` class (page-letter / bill-number missing): do NOT write a global regex** — too risky (would damage real orgs like "Page and Turnbull"). Those are already covered per-entry by the per-batch bill-position-prefix tasks (3203/3219/3233/3241), where the embedded org is identified and re-homed individually. (Approved by Mgmt-Asst-2 on the supervisor's behalf per the standing delegation to verify cleaning-pattern regexes aren't over-broad; supervisor can review/revert.)

Per the 2026-06-20 ruling, leading "THE " and trailing " (ACRONYM)" are VALID and are NOT proposed for stripping. This proposal covers ONLY the approved bill-position / legislative-prefix class. I scanned `2_webapp/org_clusters_crosswalk.json`; the affixes below recur and prepend legislative-analysis boilerplate onto a real, identifiable org name. I have NOT edited `cleaning_patterns.txt` — requesting sign-off before it becomes a follow-up task to add the regex(es) and run the full clean/dedup/stats pipeline.

**Total affected: ~80 crosswalk entries**, across 5 regexes (all anchored to the START of the string and case-sensitive as written):

**(1) Page-letter header — `^Page [A-Z] ` — ~37 entries.** A single capital letter (the analysis page section) follows "Page".
- `Page D AIDS Project Los Angeles` → `AIDS Project Los Angeles`
- `Page J California Federation of Teachers` → `California Federation of Teachers`
- `Page K Friends Committee on Legislation of California` → `Friends Committee on Legislation of California`
- **Must NOT match (near-misses, both real orgs):** `Page and Turnbull` (architecture firm), `Page Avenue Mutual Water Company`. The single-letter-then-space requirement leaves these intact (the next token is a multi-letter word, not `[A-Z] `).

**(2) Bill-number + Page — `^SB \d+, Page ` — ~12 entries.**
- `SB 103, Page Older Women's League of California` → `Older Women's League of California`
- `SB 1807, Page American Agents Alliance` → `American Agents Alliance`
- `SB 921, Page National Council of Jewish Women, Sacramento` → `National Council of Jewish Women, Sacramento`
- **Must NOT match:** any legit org containing "SB" without the `\d+, Page` shape (e.g. `SBC ...`). Scoped to require digits + ", Page".

**(3) Sponsor prefix — `^Sponsor(?: (?:of|and supporter of) the bill)?[\s,:\-]+(?:the )?` — ~18 entries.** Handles the bare "Sponsor " plus the "Sponsor of the bill, " / "Sponsor and supporter of the bill, " / "Sponsor - " connective variants, and consumes a trailing lowercase "the ".
- `Sponsor American Cancer Society` → `American Cancer Society`
- `Sponsor of the bill, Contra Costa County` → `Contra Costa County`
- `Sponsor - California Energy Commission` → `California Energy Commission`
- **Must NOT match (near-miss):** `Sponsors for Educational Opportunity` (real national nonprofit) — the trailing "s" in "Sponsors" is not in the required `[\s,:\-]` class right after "Sponsor", so it is left intact.

**(4) In support of the bill — `^In support of the bill,? (?:the )?` — ~12 entries.**
- `In support of the bill, General Atomics` → `General Atomics`
- `In support of the bill, the California Medical Association` → `California Medical Association`
- `In support of the bill the California Association of Nonprofits` (no comma) → `California Association of Nonprofits`
- **Must NOT match:** `In-Home Supportive Services` and similar — the literal phrase "In support of the bill" must be present.

**(5) Opposition header — `^OPPOSITION \(UNLESS AMENDED\) ` — 1 entry** (`OPPOSITION (UNLESS AMENDED) Acclimation Insurance Management Services` → `Acclimation Insurance Management Services`). Only 1 occurrence (below the 3+ threshold on its own), but it is the same bill-position class and was named in the task; include only if you want full coverage, otherwise I'll clean it as a one-off.

**Flagged for a SEPARATE decision (NOT proposed here — too risky to auto-strip):** there are also many bare `Page <OrgName>` (page letter missing, e.g. `Page AARP`, `Page Latino Issues Forum`) and bare `SB <OrgName>` (bill number stripped, e.g. `SB American Cancer Society`, `SB AIDS Project Los Angeles`) entries. A bare `^Page ` or `^SB ` strip would damage legitimate orgs (`Page and Turnbull`, `Page Avenue Mutual Water Company`, an org literally named "SB ..."), so I deliberately did NOT include them. Want a follow-up task to handle those individually (manual review), or a tighter regex?

**Note:** after stripping, a few residues become fragments (e.g. `Sponsor American Federation of State` → `American Federation of State`) — the dedup pipeline will merge exact normalized matches into existing canonicals; any remaining true fragments get routed to `org_names_partial.csv` as normal follow-up. No data changes made yet.

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
**Status:** Answered — Mgmt-Asst-2 web research (2026-06-20). A web search confirms **no organization exists by the exact name "California Reproductive Health Association"** — the name appears nowhere. The only statewide body in that space is the **California Coalition for Reproductive Freedom** (a distinct org), alongside others (ACCESS RJ, Essential Access Health, Reproductive Freedom for All California). Because the 24 source references cannot be confidently mapped to one specific real org, do NOT guess a parent (wrong-association risk) and do NOT keep it as a standalone canonical. **RESOLUTION = option (c):** remove the "California Reproductive Health Association" canonical and its "CA Reproductive Health Association" alt from the crosswalk JSON, and move both rows (with counts) from `org_names_in_crosswalk.csv` to `org_names_partial.csv`. Execute via follow-up **task 3218**.

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
**Status:** Answered — Supervisor (2026-06-19): **Do NOT promote the city/county/ethnic chambers out to top-level canonicals — the nesting under the UNITED STATES CHAMBER OF COMMERCE tree (incl. the CALIFORNIA CHAMBER OF COMMERCE subtree) is INTENTIONAL.** Local chambers are not necessarily related to each other, but we deliberately keep them grouped in one organization tree because it fits how we organize everything else (better than ~1,000 standalone single-entity canonicals). This makes the original Task 2243 premise (part 1: ~1,000 promotions) wrong — that part is CANCELLED. Parts 2-3 (script-flagged flat-alt / duplicate-sibling dedup) are already handled by the global-dedup run — skip. The ONLY remaining valid work, all done WITHIN the existing tree without un-nesting any chamber: (4) reclassify any CalChamber `[chapter]` variants that are really alt-spellings to `alternate_spelling`; (5) demote ~12 OCR/typo chapter names to `alternate_spelling` under their correct chapter; (6) route the genuinely invalid children (conjoined / narrative-embedded / individuals) to the appropriate invalidity CSVs per CLAUDE.md (for conjoined, ensure each component chamber exists somewhere in the tree first). Task 2243 reframed accordingly and set Not Started. See project memory: chambers-intentionally-nested.

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
