#!/usr/bin/env bash
#
# run_scan.sh — Headless, auto-restarting driver for the leginfo resolution scan (step 2).
#
# Fleet-parity resilience: loops chunk-by-chunk, diagnoses each batch with a parallel
# `claude -p` Opus worker (NOT the Workflow tool — that needs a live interactive session
# to catch its completion notification and can't run unattended), assembles the results,
# processes them via process_chunk.py, and BACKS OFF + auto-resumes on a usage/spend
# limit — exactly like the RA fleet. Fully unattended: no session, no manual relaunch.
#
# Reuses, unchanged: leginfo_scan_state/{next_batches,apply_results,process_chunk}.py
# and the inlined diagnosis rules from leginfo_scan_state/leginfo_resolution_scan.js.
#
# Usage:
#   scripts/run_scan.sh                          # run until the worklist drains
#   MAX_CHUNKS=1 CHUNK_BATCHES=1 scripts/run_scan.sh   # smoke test (1 batch)
#
# Detached (like the fleet) — pair with caffeinate so the Mac stays awake:
#   nohup scripts/run_scan.sh >ra_logs/scan.out 2>&1 & disown
#   nohup caffeinate -i -m -s -w $! >/dev/null 2>&1 & disown
#
# Env knobs:
#   CHUNK_BATCHES  batches per chunk (default 40; batch SIZE=30 is set in next_batches.py)
#   CONC           concurrent diagnosis workers (default 10)
#   MODEL          model alias for diagnosis — MUST be opus (default: opus)
#   FATAL_BACKOFF  seconds to wait after a usage/spend-limit hit, then retry (default 300)
#   TASK_TIMEOUT   per-batch worker hard cap in seconds (default 1800)
#   MAX_CHUNKS     stop after N chunks (default 0 = until worklist empty)
#   IDLE_SLEEP     seconds to wait+recheck when the worklist is momentarily empty (default 60)

set -uo pipefail

REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_DIR"
STATE="$REPO_DIR/leginfo_scan_state"
WORKLIST="$REPO_DIR/org_names_for_cleaning/org_names_not_in_crosswalk.csv"
LOG_DIR="$REPO_DIR/ra_logs"; mkdir -p "$LOG_DIR"
RUN_LOG="$LOG_DIR/scan.log"
ERROR_LOG="$LOG_DIR/errors.log"

# Stable, PERSISTENT work dir (survives a crash so a restart resumes cleanly). Kept out
# of the sandbox-specific /tmp/claude-501 on purpose — this runs headless, not sandboxed.
export TMPDIR="$REPO_DIR/.scan_work"
WORK="$TMPDIR/leginfo_scan"
mkdir -p "$WORK/batches" "$WORK/results"
cp "$STATE"/next_batches.py "$STATE"/apply_results.py "$STATE"/process_chunk.py "$WORK/"

CHUNK_BATCHES="${CHUNK_BATCHES:-40}"
CONC="${CONC:-10}"
MODEL="${MODEL:-opus}"
FATAL_BACKOFF="${FATAL_BACKOFF:-300}"
TASK_TIMEOUT="${TASK_TIMEOUT:-1800}"
MAX_CHUNKS="${MAX_CHUNKS:-0}"
IDLE_SLEEP="${IDLE_SLEEP:-60}"
# Reasoning effort for the diagnosis workers. Default medium (was inheriting the
# launcher's CLAUDE_EFFORT=high). Set both var names so it wins regardless of which
# the CLI reads; override with e.g. EFFORT=high scripts/run_scan.sh.
EFFORT="${EFFORT:-medium}"
export CLAUDE_EFFORT="$EFFORT" CLAUDE_CODE_EFFORT_LEVEL="$EFFORT"

worklist_count() { echo $(( $(wc -l < "$WORKLIST") - 1 )); }

# The inlined diagnosis rules — identical in spirit to leginfo_resolution_scan.js, but the
# worker outputs ONLY a bare JSON array (no {diagnoses:...} wrapper, no schema tool).
diag_prompt() {  # $1 = absolute path to the batch CSV
  cat <<EOF
You are a diagnosis sub-agent for the leginfo import resolution scan (step 2). DO NOT EDIT ANY FILES and do NOT open any project docs — every rule you need is inlined below. Diagnose only.

KEY CROSSWALK RULES: (a) SEARCH THE CROSSWALK FIRST (\`grep -i\` in /Users/ruthgracewong/california-groups-disambiguation/2_webapp/org_clusters_crosswalk.json) — the org is usually already present as a canonical, chapter, or alternate spelling; use relation "new_canonical" ONLY if it genuinely is nowhere. (b) Place at the CORRECT hierarchy level — alternate_spelling vs chapter vs alt_of_chapter (a city Chamber of Commerce -> chapter under the U.S./California Chamber tree; an AFSCME/SEIU/union local -> under that union; a Mayor/Sheriff/City Attorney/City Manager -> under that city/county office canonical). (c) PRESERVE every real org name — a dirty, truncated, or OCR-typo spelling becomes an alternate_spelling of the clean org, never discarded. (d) Do NOT strip location/chapter suffixes ("Inner City Law Center, Los Angeles" may be a chapter) or "dba" names; DO strip bill/position metadata (SB ###, "(sponsor)", "in support", "(previous version)", dates, counts). (e) Out-of-state orgs that lobby the CA legislature are legitimate — keep them. (f) Truncated/ambiguous fragments: search the crosswalk AND the web; classify "partial" ONLY if still ambiguous after both.

Your batch is a CSV file (columns: org_name,count — NO header, org_name may be quoted). Read every row from: $1
Diagnose ALL of its rows (about 30). Echo each "original" EXACTLY as it appears in the file, with the same count.

For EACH item:
1. ORG NAME or NARRATIVE PROSE? Prose = a sentence/fragment (e.g. "we strongly", "While we", "supported by the board"). If prose: grep the exact string in /Users/ruthgracewong/leginfo/extract_all_leginfo_metadata/leginfo_metadata.csv to recover the whole cell, then extract the real org(s) into extracted_orgs. If the prose names a real org -> classification "valid" (or "conjoined" if several); if it names NO org -> "invalid", target_csv "org_names_invalid.csv".
2. CLEAN & SEARCH THE CROSSWALK for the org (or its parent); find the exact existing node if present.
3. TRIAGE (conjoined FIRST): conjoined = multiple orgs mashed together -> list them in extracted_orgs, classification "conjoined", no target_csv. individual = a person who is NOT a leader of an identifiable org -> "individual", target_csv "org_names_that_are_actually_individuals.csv" (LEADERSHIP EXCEPTION = "valid" alt of the org, NOT individual: Mayor/City Attorney/District Attorney/Sheriff/Chief of Police/President/CEO/Superintendent/Chair/Director-of-whole-org; Councilmember/Supervisor/Commissioner/Trustee/Board member = individual). partial = truncated/ambiguous after crosswalk+web -> "partial", target_csv "org_names_partial.csv". invalid = not an org -> "invalid", target_csv "org_names_invalid.csv". valid = a single clean real org to add -> set crosswalk_placement {canonical, relation one of alternate_spelling|chapter|alt_of_chapter|new_canonical|already_present, attach_to_node = the EXACT existing node to attach under, or null for new_canonical}.

PROSE THAT NAMES A REAL ORG: judgment "prose", extracted_orgs = the org(s), classification "already_in_crosswalk" (relation "already_present") if present, else "valid" with placement.
ORG NAME ALREADY PRESENT UNDER A DIFFERENT SPELLING: classification "valid"/"already_in_crosswalk" with the existing node's placement — this exact leginfo spelling gets added as an alternate.
delete_from_crosswalk: if while grepping you find a node ALREADY IN the crosswalk that is itself accidental NARRATIVE PROSE (a sentence wrongly added as an org), put that EXACT node string in delete_from_crosswalk. Normally [].

Actually grep the crosswalk — do not guess. For non-valid, non-conjoined items set extracted_orgs [] and crosswalk_placement fields null.

OUTPUT FORMAT (STRICT): print ONLY a JSON array — no prose, no markdown, no code fences — of one object per row, each with EXACTLY these keys: original (string, echoed exactly), count (number), judgment ("org_name"|"prose"), classification ("valid"|"already_in_crosswalk"|"invalid"|"individual"|"partial"|"conjoined"), extracted_orgs (array of strings), target_csv (string|null), crosswalk_placement (object {canonical, relation, attach_to_node}, values may be null), notes (string), delete_from_crosswalk (array of strings). The FINAL thing you output must be the closing ] of that array and nothing after it.
EOF
}

# Extract a bare JSON array from a claude stream-json transcript file -> stdout (or "null").
extract_json() {  # $1 = stream-json transcript file
  python3 - "$1" <<'PY'
import sys, json, re
result=None; texts=[]
try:
    for line in open(sys.argv[1], errors="ignore"):
        line=line.strip()
        if not line: continue
        try: o=json.loads(line)
        except Exception: continue
        if not isinstance(o, dict): continue
        if o.get("type")=="result" and isinstance(o.get("result"), str): result=o["result"]
        msg=o.get("message") or {}
        for blk in (msg.get("content") or []):
            if isinstance(blk, dict) and blk.get("type")=="text": texts.append(blk.get("text",""))
except Exception: pass
txt = result if result is not None else "\n".join(texts)
# strip code fences if present, then slice the outermost [...] and validate
txt = re.sub(r"```(?:json)?", "", txt)
i, j = txt.find("["), txt.rfind("]")
if i!=-1 and j!=-1 and j>i:
    try:
        arr=json.loads(txt[i:j+1])
        if isinstance(arr, list): print(json.dumps(arr)); sys.exit(0)
    except Exception: pass
print("null")
PY
}

log(){ echo "$(date '+%Y-%m-%d %H:%M:%S') $*" | tee -a "$RUN_LOG"; }

# Diagnose ONE batch: run a claude -p worker, write results/batch_NNNN.json (array or null),
# and echo "LIMIT" on stdout if it hit a usage/spend limit so the chunk can back off.
diagnose_batch() {  # $1 = batch number, $2 = batch file
  local bn="$1" bf="$2"
  local out err; out="$(mktemp "$WORK/.diag.$bn.out.XXXXXX")"; err="$(mktemp "$WORK/.diag.$bn.err.XXXXXX")"
  # macOS has no `timeout`; use a portable background-process + sleep-kill watchdog.
  claude -p "$(diag_prompt "$bf")" \
      --dangerously-skip-permissions --model "$MODEL" \
      --output-format stream-json --include-partial-messages --verbose \
      >"$out" 2>"$err" &
  local cpid=$!
  ( sleep "$TASK_TIMEOUT"; kill -9 "$cpid" 2>/dev/null ) &
  local kpid=$!
  wait "$cpid" 2>/dev/null
  kill "$kpid" 2>/dev/null; wait "$kpid" 2>/dev/null
  local blob; blob="$( { tail -c 4000 "$err"; echo; tail -c 4000 "$out"; } | tr '\n' ' ' )"
  if grep -qiE "usage limit|spend limit|monthly spend|hit your .*(usage|spend|limit)|claude\.ai/settings/usage" <<<"$blob"; then
    printf 'null' > "$WORK/results/batch_$(printf '%04d' "$bn").json"
    echo LIMIT
  else
    extract_json "$out" > "$WORK/results/batch_$(printf '%04d' "$bn").json"
  fi
  rm -f "$out" "$err"
}

log "=== run_scan starting (chunk_batches=$CHUNK_BATCHES conc=$CONC model=$MODEL) worklist=$(worklist_count) ==="
chunks=0
while true; do
  [[ "$MAX_CHUNKS" -gt 0 && "$chunks" -ge "$MAX_CHUNKS" ]] && { log "reached MAX_CHUNKS=$MAX_CHUNKS, stopping"; break; }
  wl="$(worklist_count)"
  if [[ "$wl" -le 0 ]]; then
    log "worklist empty — scan DONE (run step 3 finalize pipeline + step 4 source rewrites next)"; break
  fi

  # 1. generate a chunk -> batch CSVs + list of batch numbers
  pend="$WORK/pending.json"
  if ! python3 "$WORK/next_batches.py" "$CHUNK_BATCHES" > "$pend" 2>>"$RUN_LOG"; then
    log "next_batches failed — backing off ${FATAL_BACKOFF}s"; sleep "$FATAL_BACKOFF"; continue
  fi
  batchnums=()
  while IFS= read -r _bn; do [[ -n "$_bn" ]] && batchnums+=("$_bn"); done < <(python3 -c "import json,sys; print('\n'.join(str(b['batch']) for b in json.load(open('$pend'))))")
  [[ "${#batchnums[@]}" -eq 0 ]] && { log "no batches emitted (all processed?) — recheck in ${IDLE_SLEEP}s"; sleep "$IDLE_SLEEP"; continue; }
  _last="${batchnums[$(( ${#batchnums[@]} - 1 ))]}"
  log "chunk $((chunks+1)): batches ${batchnums[0]}..${_last} (${#batchnums[@]} batches), worklist=$wl"

  # 2. diagnose all batches in concurrency-capped groups (bash 3.2-portable: no `wait -n`).
  limit_hit=0
  gi=0; ntot="${#batchnums[@]}"
  while [[ "$gi" -lt "$ntot" ]]; do
    for bn in "${batchnums[@]:$gi:$CONC}"; do
      bf="$WORK/batches/batch_$(printf '%04d' "$bn").csv"
      { diagnose_batch "$bn" "$bf" > "$WORK/.limit.$bn" 2>/dev/null; } &
    done
    wait
    gi=$(( gi + CONC ))
  done
  for bn in "${batchnums[@]}"; do
    [[ -f "$WORK/.limit.$bn" ]] && grep -q LIMIT "$WORK/.limit.$bn" 2>/dev/null && limit_hit=$((limit_hit+1))
    rm -f "$WORK/.limit.$bn"
  done

  # 3. assemble combined output {result:[{batch,diagnoses}]} and process it
  combined="$WORK/combined.json"
  python3 - "$pend" "$WORK/results" "$combined" <<'PY'
import json, sys, os
pend=json.load(open(sys.argv[1])); resdir=sys.argv[2]; out=sys.argv[3]
res=[]
for b in pend:
    bn=b["batch"]; f=os.path.join(resdir, f"batch_{bn:04d}.json")
    diags=None
    if os.path.exists(f):
        try:
            d=json.load(open(f)); diags=d if isinstance(d,list) else None
        except Exception: diags=None
    res.append({"batch":bn,"diagnoses":diags})
json.dump({"result":res}, open(out,"w"))
PY
  if python3 "$WORK/process_chunk.py" "$combined" >>"$RUN_LOG" 2>&1; then
    chunks=$((chunks+1)); log "chunk processed (limit_hits=$limit_hit) worklist=$(worklist_count)"
  else
    log "process_chunk failed — see scan.log"
  fi

  # 4. if the spend limit blocked any batch, record + back off long, then AUTO-RESUME
  if [[ "$limit_hit" -gt 0 ]]; then
    printf '%s\tScan-Driver\tusage_limit\tfatal=1\texit=1\t%d batch(es) hit spend limit — backing off, auto-resume\n' \
      "$(date '+%Y-%m-%dT%H:%M:%S')" "$limit_hit" >> "$ERROR_LOG"
    log "$limit_hit batch(es) hit the spend limit — backing off ${FATAL_BACKOFF}s, then auto-resuming"
    sleep "$FATAL_BACKOFF"
  fi
done
log "=== run_scan exiting (chunks this run: $chunks) ==="
