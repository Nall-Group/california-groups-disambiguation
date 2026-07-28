#!/usr/bin/env python3
"""Collector: apply one batch's diagnosis JSON to the routing CSVs + worklist,
record step-4 rewrites, and surface crosswalk-add and prose-deletion work.

Usage: apply_results.py <batch_file.csv> <results.json>

Per-item handling (LEGINFO_IMPORT.md step 2 + locked decisions 2026-07-09):
- classification invalid/individual/partial (target_csv): append (original,count) to that
  CSV (dedup), remove original from worklist.
- classification conjoined: record rewrite(original -> extracted_orgs) for step 4, remove
  original from worklist (NO csv), each extracted org becomes a crosswalk-add.
- judgment prose + valid/already_in_crosswalk: record rewrite(original -> extracted_orgs)
  for step 4, remove the prose from the worklist (NO csv). Add an extracted org as a
  crosswalk-add ONLY if it is absent (relation != already_present / classification valid).
- judgment org_name + valid/already_in_crosswalk: crosswalk-add of the ORIGINAL spelling
  (add as alt/chapter per placement). Leave in worklist (step-3 regenerate sweeps it).
- delete_from_crosswalk (array on the item, optional): accidental prose nodes found IN the
  crosswalk -> emit a delete-from-crosswalk RA task. NEVER routed to a CSV.
Items with no matching diagnosis are left unprocessed (retry), not marked processed.

Writes: routing CSVs, worklist, $SCAN/processed.txt, $SCAN/rewrites.tsv (append), AND the
two persistent mapping CSVs (narrative_text_mapping_to_orgs.csv / conjoined_text_mapping_
to_orgs.csv) — those are the durable record; rewrites.tsv is only this run's ledger.
Prints JSON: {batch, valid:[...], routed:{}, removed, rewrites:[[orig,[orgs]]], mapped:{},
deletes:[...], unresolved:[...]}
"""
import csv, json, os, re, sys
from pathlib import Path

PROJECT = Path("/Users/ruthgracewong/california-groups-disambiguation")
SUB = PROJECT / "org_names_for_cleaning"
WORKLIST = SUB / "org_names_not_in_crosswalk.csv"
STATE = PROJECT / "leginfo_scan_state"          # durable (committed) ledgers
PROCESSED = STATE / "processed.txt"
REWRITES = STATE / "rewrites.tsv"
# The persistent prose/conjoined maps — the record that survives a run. Step 1 reads these
# to mark a string already_routed; step 4 reads them to attribute its bill count.
NARRATIVE_MAP = SUB / "narrative_text_mapping_to_orgs.csv"
CONJOINED_MAP = SUB / "conjoined_text_mapping_to_orgs.csv"

CSV_FOR = {"invalid": "org_names_invalid.csv", "partial": "org_names_partial.csv",
           "individual": "org_names_that_are_actually_individuals.csv"}

batch_file, results_file = sys.argv[1], sys.argv[2]
batch_num = int(Path(batch_file).stem.split("_")[1])

items = {}
with open(batch_file, newline="") as f:
    for row in csv.reader(f):
        if row: items[row[0]] = int(row[1])

diags = json.load(open(results_file))
by_orig = {d.get("original"): d for d in diags if isinstance(d, dict)}

def _norm(s):
    # strip ALL punctuation (quotes, trailing commas/periods, brackets) so minor echo
    # differences still map; batch items are unique enough that this is collision-safe
    s = re.sub(r"[^0-9a-zA-Z%\s]", " ", s or "")
    return re.sub(r"\s+", " ", s).strip().casefold()

# normalized fallback so quote/whitespace echo differences still map (batch names are unique)
by_norm = {}
for d in diags:
    if isinstance(d, dict):
        by_norm.setdefault(_norm(d.get("original")), d)

# containment fallback: on very long prose the agent often echoes a TRUNCATED prefix of
# the original string, so exact/norm both miss and the item never leaves the worklist.
# Match when one normalized string is a prefix of the other (>=40 chars) AND exactly one
# diag qualifies (uniqueness guards against near-duplicate prose sharing a long prefix).
_diag_norms = [(_norm(d.get("original")), d) for d in diags if isinstance(d, dict)]
def _contain_match(name):
    nn = _norm(name)
    if len(nn) < 40:
        return None
    uniq = []
    for dn, d in _diag_norms:
        if len(dn) >= 40 and (nn.startswith(dn) or dn.startswith(nn)):
            if all(d is not u for u in uniq):
                uniq.append(d)
    return uniq[0] if len(uniq) == 1 else None

remove_from_worklist = set()
csv_appends = {}      # fn -> [(name,count)]
valid = []            # crosswalk-adds
rewrites = []         # (original, [orgs]) for step 4
deletes = []          # accidental prose nodes to delete from crosswalk
unresolved = []
processed_now = []

for name, count in items.items():
    d = by_orig.get(name) or by_norm.get(_norm(name)) or _contain_match(name)
    if d is None:
        unresolved.append(name); continue
    processed_now.append(name)

    # accidental prose already in the crosswalk -> delete task (independent of class)
    for node in (d.get("delete_from_crosswalk") or []):
        if node: deletes.append(node)

    cls = (d.get("classification") or "").lower()
    judg = (d.get("judgment") or "").lower()
    orgs = d.get("extracted_orgs") or []
    placement = d.get("crosswalk_placement") or {}
    rel = (placement.get("relation") or "").lower()

    if cls in ("invalid", "individual", "partial"):
        fn = d.get("target_csv") if d.get("target_csv") in CSV_FOR.values() else CSV_FOR[cls]
        csv_appends.setdefault(fn, []).append((name, count))
        remove_from_worklist.add(name)

    elif cls == "conjoined":
        rewrites.append((name, orgs)); remove_from_worklist.add(name)
        for org in orgs:
            valid.append({"name": org, "count": count, "relation": "new_or_existing",
                          "canonical": None, "attach_to_node": None,
                          "notes": f"split from conjoined: {name}"})

    elif judg == "prose" and cls in ("valid", "already_in_crosswalk"):
        rewrites.append((name, orgs)); remove_from_worklist.add(name)
        if cls == "valid" and rel != "already_present":
            for org in orgs:
                valid.append({"name": org, "count": count, "relation": rel or "new_or_existing",
                              "canonical": placement.get("canonical"),
                              "attach_to_node": placement.get("attach_to_node"),
                              "notes": f"extracted from prose: {name[:60]}"})

    elif judg == "org_name" and cls in ("valid", "already_in_crosswalk"):
        # present under a different spelling OR genuinely new -> add the exact spelling.
        # "already_present" is not an actionable relation for an add (the exact string is NOT
        # a node, else step 1 would have matched it) -> instruct the RA to add it as an alt.
        add_rel = "alternate_spelling" if rel in ("", "already_present", None) else rel
        valid.append({"name": name, "count": count, "relation": add_rel,
                      "canonical": placement.get("canonical"),
                      "attach_to_node": placement.get("attach_to_node"),
                      "notes": d.get("notes", "")})
        # leave in worklist; step-3 regenerate moves it to in_crosswalk once the RA adds it
    # anything else: leave in worklist, no action

# ---- apply CSV appends (dedup) ----
routed = {}
for fn, pairs in csv_appends.items():
    p = SUB / fn
    existing = set()
    if p.exists():
        with open(p, newline="") as f:
            existing = {row[0] for row in csv.reader(f) if row}
    added = 0
    with open(p, "a", newline="") as f:
        w = csv.writer(f)
        for nm, cnt in pairs:
            if nm in existing: continue
            w.writerow([nm, cnt]); existing.add(nm); added += 1
    routed[fn] = added

# ---- remove routed/prose/conjoined originals from worklist ----
removed = 0
if remove_from_worklist:
    with open(WORKLIST, newline="") as f:
        r = list(csv.reader(f))
    header, body = r[0], r[1:]
    kept = [row for row in body if row and row[0] not in remove_from_worklist]
    removed = len(body) - len(kept)
    with open(WORKLIST, "w", newline="") as f:
        w = csv.writer(f); w.writerow(header); w.writerows(kept)

# ---- append rewrites ledger (for step 4) ----
if rewrites:
    with open(REWRITES, "a", newline="") as f:
        w = csv.writer(f, delimiter="\t")
        for orig, orgs in rewrites:
            w.writerow([orig, ";".join(orgs)])

# ---- append the PERSISTENT mapping CSVs (the durable record) ----
# rewrites.tsv alone is NOT enough: it is a per-run ledger that nothing else reads, so a
# prose/conjoined string diagnosed here would be re-diagnosed from scratch on the next
# import (extract_org_names.py marks a string already_routed only if it appears in one of
# these two mapping files). Run 1 recorded 7,076 resolutions in the ledger and only 362 in
# the mapping files, which cost a full re-scan of ~5,700 items — hence this write.
#   single org  -> narrative_text_mapping_to_orgs.csv (narrative_text,mapped_org)
#   several     -> conjoined_text_mapping_to_orgs.csv (conjoined_text,mapped_orgs, " ; "-joined)
mapped_counts = {}
if rewrites:
    buckets = {NARRATIVE_MAP: [], CONJOINED_MAP: []}
    for orig, orgs in rewrites:
        orgs = [o.strip() for o in orgs if o and o.strip()]
        if not orgs:
            continue  # names no org: it was routed to org_names_invalid.csv, not mapped here
        if len(orgs) == 1:
            buckets[NARRATIVE_MAP].append([orig, orgs[0]])
        else:
            buckets[CONJOINED_MAP].append([orig, " ; ".join(orgs)])

    # A source string must not land in both files, so dedup across the pair.
    seen_all = set()
    for path in (NARRATIVE_MAP, CONJOINED_MAP):
        if path.exists():
            with open(path, newline="") as f:
                rdr = csv.reader(f); next(rdr, None)
                seen_all |= {_norm(row[0]) for row in rdr if row}

    for path, rows in buckets.items():
        if not rows:
            continue
        new_file = not path.exists()
        added = 0
        with open(path, "a", newline="") as f:
            w = csv.writer(f)
            if new_file:
                w.writerow(["narrative_text", "mapped_org"] if path == NARRATIVE_MAP
                           else ["conjoined_text", "mapped_orgs"])
            for src, mapped in rows:
                k = _norm(src)
                if k in seen_all:
                    continue
                w.writerow([src, mapped]); seen_all.add(k); added += 1
        mapped_counts[path.name] = added

# ---- update processed ledger ----
with open(PROCESSED, "a") as f:
    for n in processed_now: f.write(n + "\n")

print(json.dumps({"batch": batch_num, "valid": valid, "routed": routed, "removed": removed,
                  "rewrites": [[o, gs] for o, gs in rewrites], "mapped": mapped_counts,
                  "deletes": deletes, "unresolved": unresolved}))
