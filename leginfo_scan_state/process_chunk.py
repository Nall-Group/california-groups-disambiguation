#!/usr/bin/env python3
"""Process one diagnosis-workflow output end to end.

Usage: process_chunk.py <workflow_output.json>

For each {batch, diagnoses} in output["result"]:
  1. write $SCAN/results/batch_NNNN.json
  2. run apply_results.py  -> routes CSVs, updates worklist + ledgers, returns summary
Then, from the summaries, append TASKS.md rows:
  - [LEGINFO-CROSSWALK-ADD] one per batch that has >=1 valid crosswalk-add
  - [LEGINFO-CROSSWALK-DELETE] one per batch that flagged accidental prose IN the crosswalk
and git-commit the data changes + TASKS.md + leginfo_scan_state/.

Prints a summary. Idempotent-ish: re-running the same output re-writes result files but
apply_results skips already-processed names (ledger), so counts won't double.
"""
import json, os, subprocess, sys, re, time
from pathlib import Path

# --- TASKS.md Write Queue participation (coordinates with the fleet) ---
QNAME = "Driver-Scan"
Q_HDR = "## TASKS.md Write Queue"
Q_END = "## Data Write Queue"

def _q_bounds(lines):
    try:
        s = next(i for i, l in enumerate(lines) if l.strip() == Q_HDR)
        e = next(i for i, l in enumerate(lines) if l.strip() == Q_END)
        return s, e
    except StopIteration:
        return None, None

def _q_name_idxs(lines, s, e):
    return [i for i in range(s, e) if lines[i].strip().startswith("- ")]

def queue_join(tasks_path):
    lines = tasks_path.read_text().split("\n")
    s, e = _q_bounds(lines)
    if e is None:
        return False
    lines.insert(e, f"- {QNAME}")
    tasks_path.write_text("\n".join(lines))
    return True

def queue_wait(tasks_path, timeout=900):
    waited = 0
    while waited < timeout:
        lines = tasks_path.read_text().split("\n")
        s, e = _q_bounds(lines)
        if e is not None:
            idxs = _q_name_idxs(lines, s, e)
            if idxs and lines[idxs[0]].strip() == f"- {QNAME}":
                return True
        time.sleep(3); waited += 3
    return False

def queue_leave(tasks_path):
    lines = tasks_path.read_text().split("\n")
    lines = [l for l in lines if l.strip() != f"- {QNAME}"]
    tasks_path.write_text("\n".join(lines))

def git_commit_retry(cwd, add_files, msg, tries=6):
    for _ in range(tries):
        subprocess.run(["git", "add", *add_files], cwd=cwd, capture_output=True, text=True)
        cp = subprocess.run(["git", "commit", "-m", msg], cwd=cwd, capture_output=True, text=True)
        blob = (cp.stdout or "") + (cp.stderr or "")
        if cp.returncode == 0 or "nothing to commit" in blob:
            return cp
        if "index.lock" in blob or "another git process" in blob:
            time.sleep(2); continue
        return cp
    return cp

PROJECT = Path("/Users/ruthgracewong/california-groups-disambiguation")
SCAN = Path(os.environ["TMPDIR"]) / "leginfo_scan"
RESULTS = SCAN / "results"; RESULTS.mkdir(exist_ok=True)
BATCHES = SCAN / "batches"
APPLY = str(SCAN / "apply_results.py")
TASKS = PROJECT / "TASKS.md"
WORKLIST = PROJECT / "org_names_for_cleaning" / "org_names_not_in_crosswalk.csv"

out = json.load(open(sys.argv[1]))
result = out["result"]

# ---- 1+2: write result files and apply each batch ----
summaries = []
for b in result:
    bn = b["batch"]; diags = b.get("diagnoses") or []
    rf = RESULTS / f"batch_{bn:04d}.json"
    json.dump(diags, open(rf, "w"))
    bf = BATCHES / f"batch_{bn:04d}.csv"
    if not bf.exists():
        print(f"  WARN batch {bn}: batch file missing, skipping"); continue
    r = subprocess.run([sys.executable, APPLY, str(bf), str(rf)],
                       capture_output=True, text=True)
    if r.returncode != 0:
        print(f"  ERROR batch {bn}: {r.stderr.strip()}"); continue
    summaries.append(json.loads(r.stdout))

# ---- next task id ----
ids = [int(m) for m in re.findall(r"^\| *(\d+) *\|", TASKS.read_text(), re.M)]
next_id = max(ids) + 1

def add_task(valid, batch):
    entries = []
    for v in valid:
        rel = (v.get("relation") or "alternate_spelling").replace("_", " ")
        seg = f"`{v['name']}` ({v['count']}) → {rel}"
        if v.get("canonical"): seg += f" of **{v['canonical']}**"
        if v.get("attach_to_node") and v["attach_to_node"] != v.get("canonical"):
            seg += f" [attach under node: {v['attach_to_node']}]"
        entries.append(seg)
    return (f"[LEGINFO-CROSSWALK-ADD] **Add {len(valid)} valid orgs (leginfo resolution scan batch {batch:03d}) "
            f"to the crosswalk.** Unmatched leginfo orgs diagnosed valid. Placements are HINTS — per the Worker RA "
            f"Role, SEARCH THE CROSSWALK FIRST to confirm the exact node, disambiguate with web research before "
            f"merging, place at the correct hierarchy. Do NOT route any to a CSV. Entries → placement: "
            f"{'; '.join(entries)}. DO NOT run the "
            f"clean/dedup/stats pipeline — deferred to step-3 finalize. Grab the Data Write Queue once, add all "
            f"{len(valid)}, commit once while holding the queue.")

def delete_task(nodes, batch):
    lst = "; ".join(f"`{n}`" for n in nodes)
    return (f"[LEGINFO-CROSSWALK-DELETE] **Delete {len(nodes)} accidental-prose node(s) from the crosswalk "
            f"(found during leginfo scan batch {batch:03d}).** These crosswalk nodes are narrative prose wrongly "
            f"added as orgs. For EACH: locate the exact node in `2_webapp/org_clusters_crosswalk.json` and DELETE "
            f"it (if it is a canonical with real children, reparent the children; if it is a leaf/alt, just remove "
            f"it). Do NOT move any of these to a routing CSV — they must be gone so they can't match a real org. "
            f"Nodes: {lst}. DO NOT run the pipeline (deferred to step-3 finalize). Follow the Data Write Queue; "
            f"commit while holding it.")

rows = []
for s in summaries:
    if s.get("valid"):
        rows.append(f"| {next_id} | {add_task(s['valid'], s['batch'])} | Not Started |  |  |"); next_id += 1
    if s.get("deletes"):
        rows.append(f"| {next_id} | {delete_task(s['deletes'], s['batch'])} | Not Started |  |  |"); next_id += 1

if rows:
    # join the TASKS.md Write Queue, wait until we're at the top (no RA editing),
    # append while holding it, then leave — so a concurrent RA can't clobber our rows.
    queue_join(TASKS)
    if not queue_wait(TASKS):
        print("WARN: TASKS.md queue wait timed out (900s) — appending anyway")
    with open(TASKS, "a") as f:
        for r in rows: f.write(r + "\n")
    queue_leave(TASKS)

# ---- commit (retry on concurrent-RA git index.lock) ----
data_files = [
    "TASKS.md",
    "org_names_for_cleaning/org_names_not_in_crosswalk.csv",
    "org_names_for_cleaning/org_names_invalid.csv",
    "org_names_for_cleaning/org_names_partial.csv",
    "org_names_for_cleaning/org_names_that_are_actually_individuals.csv",
    "leginfo_scan_state/processed.txt",
    "leginfo_scan_state/rewrites.tsv",
    "leginfo_scan_state/next_batch_num.txt",
]
batch_lo = min(s["batch"] for s in summaries) if summaries else 0
batch_hi = max(s["batch"] for s in summaries) if summaries else 0
msg = (f"Leginfo scan batches {batch_lo:03d}-{batch_hi:03d}: route CSVs + {len(rows)} tasks\n\n"
       "Generated with [Claude Code](https://claude.ai/code)\nvia [Happy](https://happy.engineering)\n\n"
       "Co-Authored-By: Claude <noreply@anthropic.com>\nCo-Authored-By: Happy <yesreply@happy.engineering>")
cp = git_commit_retry(PROJECT, data_files, msg)

# ---- summary ----
tv = sum(len(s.get("valid", [])) for s in summaries)
tr = sum(sum(s.get("routed", {}).values()) for s in summaries)
trw = sum(len(s.get("rewrites", [])) for s in summaries)
tmap = sum(sum((s.get("mapped") or {}).values()) for s in summaries)
td = sum(len(s.get("deletes", [])) for s in summaries)
tu = sum(len(s.get("unresolved", [])) for s in summaries)
wl = sum(1 for _ in open(WORKLIST)) - 1
print(f"batches processed: {len(summaries)} ({batch_lo}-{batch_hi})")
print(f"valid crosswalk-adds: {tv} | routed-to-CSV: {tr} | rewrites: {trw} (mapped to persistent CSVs: {tmap}) | deletes: {td} | unresolved: {tu}")
print(f"tasks created: {len(rows)} | worklist now: {wl}")
print("commit:", (cp.stdout or cp.stderr).strip().split(chr(10))[0])
if tu:
    print("UNRESOLVED (left in worklist for retry):")
    for s in summaries:
        for n in s.get("unresolved", []): print("   ", n)
