#!/usr/bin/env bash
#
# run_ra.sh — Headless worker-RA loop.
#
# Spawns a BRAND-NEW `claude -p` process for every task, so each task starts
# with a completely fresh context (the whole point: no context accumulation,
# no manual /clear, no degradation over a long backlog).
#
# Each iteration tells Claude to claim and complete EXACTLY ONE task following
# the worker RA workflow in CLAUDE.md, commit, and exit. The loop then starts a
# fresh process for the next task. Coordination across parallel RAs happens
# through the existing TASKS.md write queue + data write queue — same as today.
#
# Usage:
#   scripts/run_ra.sh RA-Delta            # run until the backlog is drained
#   MAX_TASKS=5 scripts/run_ra.sh RA-Delta # stop after 5 tasks (smoke test)
#
# Env knobs:
#   MAX_TASKS      stop after N completed tasks (default: unlimited)
#   MAX_EMPTY      consecutive "no work" results before giving up (default: 3)
#   EMPTY_BACKOFF  seconds to wait after a "no work" result (default: 30)
#   MODEL          model alias to pass to claude (default: inherit)

set -uo pipefail

RA_NAME="${1:-}"
if [[ -z "$RA_NAME" ]]; then
  echo "usage: $0 <RA-NAME>   (e.g. $0 RA-Delta)" >&2
  exit 1
fi

REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_DIR"

MAX_TASKS="${MAX_TASKS:-0}"        # 0 = unlimited
MAX_EMPTY="${MAX_EMPTY:-3}"
EMPTY_BACKOFF="${EMPTY_BACKOFF:-30}"
# Hang/error handling is by REAL STREAM SIGNALS, not silence. Silence is NOT a
# hang — a worker waiting on a slow Bash command (e.g. loading the 1M-line
# crosswalk JSON) emits nothing for minutes yet is perfectly healthy. So we do
# NOT kill on idle. Instead the watchdog scans the stream for actual error
# signals (real throttling / error events) and only then kills so the loop can
# back off and relaunch. Claude retries transient API errors internally; if it
# can't recover it exits on its own and the loop classifies the exit.
STREAM_POLL="${STREAM_POLL:-20}"       # how often the watchdog scans the stream
# Generous absolute runtime cap (NOT an idle timer): last-resort kill for a truly
# infinite hang that never errors and never exits. Set high so no real task hits it.
TASK_TIMEOUT="${TASK_TIMEOUT:-2700}"   # 45 minutes
# Backoff after a FATAL error (usage limit, auth). Long, and fatal errors retry
# indefinitely (without counting toward MAX_EMPTY) so the fleet AUTO-RESUMES when
# a usage limit resets instead of dying — which is what stranded it overnight.
FATAL_BACKOFF="${FATAL_BACKOFF:-300}"  # 5 minutes
MODEL_ARG=()
[[ -n "${MODEL:-}" ]] && MODEL_ARG=(--model "$MODEL")

LOG_DIR="$REPO_DIR/ra_logs"
mkdir -p "$LOG_DIR"
RUN_LOG="$LOG_DIR/${RA_NAME}.log"
ERROR_LOG="$LOG_DIR/errors.log"        # shared, tab-separated classified error log

# Classify a worker failure into "category|fatal(0/1)|one-line description".
# Inputs: $1 = worker stderr file, $2 = explicit kill reason (idle|timeout|"").
# Fatal = show-stopping (won't fix itself by immediate retry): usage limit, auth.
classify_error() {
  local errfile="$1" killreason="$2" txt
  case "$killreason" in
    # Watchdog detected a real throttle/error event in the stream. Treat as fatal
    # so it backs off long and retries indefinitely (auto-resumes when it clears).
    stream_error) echo "stream_throttle|1|rate-limit/error signal in stream (back off, auto-resume)"; return;;
    timeout)      echo "timeout_kill|0|exceeded ${TASK_TIMEOUT}s absolute runtime cap"; return;;
  esac
  txt="$(tail -c 6000 "$errfile" 2>/dev/null | tr '\n' ' ')"
  if   grep -qiE "usage limit|usage_limit|reached your .*(usage|limit)" <<<"$txt"; then echo "usage_limit|1|account usage limit reached"
  elif grep -qiE "invalid x-api-key|authentication|unauthorized|401|oauth.*expired|please run /login" <<<"$txt"; then echo "auth_error|1|authentication / credential failure"
  elif grep -qiE "rate limit|rate_limit|\b429\b" <<<"$txt"; then echo "rate_limit|0|API rate limited (transient)"
  elif grep -qiE "overloaded|\b529\b" <<<"$txt"; then echo "overloaded|0|API overloaded (transient)"
  elif grep -qiE "connection closed while thinking|connection error|econnreset|fetch failed|socket hang up|network error" <<<"$txt"; then echo "api_connection|0|connection / network error (transient)"
  elif grep -qiE "prompt is too long|context.{0,12}length|too many tokens|context_length" <<<"$txt"; then echo "context_overflow|0|prompt/context too large"
  else echo "unknown|0|non-zero exit (see ${RA_NAME}.log)"
  fi
}

# The model ends every run by printing EXACTLY ONE of these tokens on its own
# line, so the loop can distinguish the three outcomes without trusting exit
# code alone (a process can exit 0 having done nothing — e.g. a broken env):
#   RESULT:DONE <n>   -> a task was completed (or correctly Blocked) and committed
#   RESULT:NONE       -> nothing was claimable right now (drained / all in-progress)
# Anything else (neither token) is treated as an anomaly => failure + backoff.
DONE_SENTINEL="RESULT:DONE"
NONE_SENTINEL="RESULT:NONE"

PROMPT="You are worker RA \"$RA_NAME\". Read CLAUDE.md and follow the Worker RA Role workflow EXACTLY.

CRITICAL EXECUTION MODEL — READ FIRST. You are a ONE-SHOT, NON-INTERACTIVE process (\`claude -p\`). You CANNOT be resumed, woken, or notified later. There is NO REPL, NO background poll, and NO scheduled wakeup. If you 'end your turn to await a signal', 'schedule a fallback wakeup', or 'pause until notified', the PROCESS SIMPLY DIES — and if you were holding a Data Write Queue slot, you DEADLOCK the whole fleet. Therefore:
- NEVER use ScheduleWakeup, background polling, or any 'wait for notification' mechanism. NEVER end your turn while waiting.
- To wait for the Data Write Queue, POLL ACTIVELY WITHIN THIS TURN: re-read TASKS.md, and if your name is not at the TOP of the queue, run a Bash 'sleep 15' and re-read, looping in-process until you reach the top. Keep doing real work in the same turn — do not yield.
- BEFORE ANY EARLY EXIT for any reason (can't get the queue in time, error, environment problem, nothing to do), you MUST first remove your name from BOTH the Data Write Queue and any In-Progress claim you made, so you never leave a ghost entry.

Do EXACTLY ONE task, then exit. Specifically:
1. Pick ONE available task: first check QUESTIONS.md for a now-answered Blocked task you can resume; otherwise claim the lowest-numbered 'Not Started' task. Join the TASKS.md Write Queue, mark it 'In Progress' with your name '$RA_NAME', and release that queue.
2. Plan, then join the Data Write Queue. Wait your turn by ACTIVE IN-TURN POLLING (sleep 15 + re-read, in a loop — never yield the turn). When you reach the top, execute the changes and commit ONLY the files you changed (never 'git add -A').
3. Join the TASKS.md Write Queue, mark the task 'Done' (or 'Blocked' with a QUESTIONS.md entry if you truly cannot proceed), remove yourself from the Data Write Queue, and commit the TASKS.md change.
4. Then STOP and exit. Do NOT pick up a second task.

If after about 10 minutes of active in-turn polling you still cannot reach the top of the Data Write Queue, remove yourself from the Data Write Queue, set your task back to 'Not Started' with a cleared assignee (commit that), and exit with RESULT:NONE.

If there is NO task you can claim right now (everything is Done, In Progress by others, or Blocked with no answer), do NOT poll or wait — exit immediately with RESULT:NONE.

CRITICAL — the very last line of your output MUST be exactly one of these tokens, and you must only print '$DONE_SENTINEL' if you genuinely committed a task to git:
  '$DONE_SENTINEL <task-number>'  -> you completed (or correctly Blocked) and committed one task
  '$NONE_SENTINEL'                -> there was no task you could claim
If your environment is broken or you could not commit, do NOT print '$DONE_SENTINEL'; print '$NONE_SENTINEL' and a one-line reason instead."

count=0
empty_streak=0

# Clean up this RA's stream/err/kill temp files on exit (incl. when the loop is
# killed mid-iteration by a fleet stop/restart — otherwise they leak in ra_logs).
cleanup_temps() { rm -f "$LOG_DIR/.${RA_NAME}".out.* "$LOG_DIR/.${RA_NAME}".err.* "$LOG_DIR/.${RA_NAME}".out.*.kill 2>/dev/null; }
trap cleanup_temps EXIT

echo "=== [$RA_NAME] starting headless loop in $REPO_DIR ===" | tee -a "$RUN_LOG"

while true; do
  if [[ "$MAX_TASKS" -gt 0 && "$count" -ge "$MAX_TASKS" ]]; then
    echo "=== [$RA_NAME] reached MAX_TASKS=$MAX_TASKS, stopping ===" | tee -a "$RUN_LOG"
    break
  fi

  ts="$(date '+%Y-%m-%d %H:%M:%S')"
  echo "--- [$RA_NAME] $ts launching fresh task process (#$((count+1))) ---" | tee -a "$RUN_LOG"

  # Fresh process every iteration => clean context. --dangerously-skip-permissions
  # is required for unattended runs; safe here because all work is file-based in git.
  #
  # Streaming output + IDLE watchdog. The reliable "is it stuck?" signal is output
  # activity, not wall-clock: a working model streams tokens/tool-events continuously
  # (so the stream file keeps growing), while a hung call ("Connection closed while
  # thinking") goes totally silent. We SIGKILL the worker if the stream file stops
  # growing for IDLE_TIMEOUT (a true hang is caught in minutes, but a genuinely slow
  # task that's still streaming is NEVER killed). TASK_TIMEOUT is just a far backstop.
  tmpout="$(mktemp "$LOG_DIR/.${RA_NAME}.out.XXXXXX")"
  tmperr="$(mktemp "$LOG_DIR/.${RA_NAME}.err.XXXXXX")"
  killflag="${tmpout}.kill"; killreason=""
  claude -p "$PROMPT" \
      --dangerously-skip-permissions \
      --output-format stream-json --include-partial-messages --verbose \
      ${MODEL_ARG[@]+"${MODEL_ARG[@]}"} >"$tmpout" 2>"$tmperr" &
  cmdpid=$!
  (
    while kill -0 "$cmdpid" 2>/dev/null; do
      sleep "$STREAM_POLL"
      now="$(date +%s)"
      st="$(stat -f %B "$tmpout" 2>/dev/null || echo "$now")"
      # Scan the tail of the stream for a REAL error signal (not silence):
      #   - a rate_limit_event whose status != "allowed"  => actual throttling
      #   - a "type":"error" event                        => API error
      # Only these kill the worker (so the loop backs off + relaunches). A worker
      # that is merely quiet (waiting on a slow tool call) is left alone.
      if tail -c 60000 "$tmpout" 2>/dev/null | python3 -c "
import sys, json
bad=False
for ln in sys.stdin:
    ln=ln.strip()
    if not ln: continue
    try: o=json.loads(ln)
    except Exception: continue
    if not isinstance(o, dict): continue
    if o.get('type')=='rate_limit_event':
        s=(o.get('rate_limit_info') or {}).get('status')
        if s and s!='allowed': bad=True
    if o.get('type')=='error': bad=True
sys.exit(0 if bad else 1)
" 2>/dev/null; then
        echo stream_error >"$killflag"
        echo "!!! [$RA_NAME] stream shows a real error/throttle signal — killing to back off + restart !!!" >>"$RUN_LOG"
        kill -9 "$cmdpid" 2>/dev/null; break
      fi
      # Last-resort absolute cap (NOT idle): only a truly infinite hang hits this.
      if (( now - st >= TASK_TIMEOUT )); then
        echo timeout >"$killflag"
        echo "!!! [$RA_NAME] exceeded ${TASK_TIMEOUT}s absolute runtime cap — killing !!!" >>"$RUN_LOG"
        kill -9 "$cmdpid" 2>/dev/null; break
      fi
    done
  ) >/dev/null 2>&1 &
  watchpid=$!
  wait "$cmdpid" 2>/dev/null
  status=$?
  kill "$watchpid" 2>/dev/null; wait "$watchpid" 2>/dev/null
  [[ -f "$killflag" ]] && { killreason="$(cat "$killflag" 2>/dev/null)"; rm -f "$killflag"; }
  # Fold the worker's stderr into the run log for the full record.
  cat "$tmperr" >> "$RUN_LOG" 2>/dev/null
  # Extract the final result text from the stream-json transcript for sentinel matching.
  output="$(python3 - "$tmpout" <<'PY'
import sys, json
result=None; texts=[]
try:
    for line in open(sys.argv[1], errors="ignore"):
        line=line.strip()
        if not line: continue
        try: o=json.loads(line)
        except Exception: continue
        if not isinstance(o, dict): continue
        if o.get("type")=="result" and isinstance(o.get("result"), str):
            result=o["result"]
        msg=o.get("message") or {}
        for blk in (msg.get("content") or []):
            if isinstance(blk, dict) and blk.get("type")=="text":
                texts.append(blk.get("text",""))
except Exception: pass
print(result if result is not None else "\n".join(texts))
PY
)"
  rm -f "$tmpout"

  echo "$output" >> "$RUN_LOG"

  if [[ $status -ne 0 ]]; then
    # Classify, record to the shared error log, and pick backoff by severity.
    ci="$(classify_error "$tmperr" "$killreason")"
    ecat="${ci%%|*}"; erest="${ci#*|}"; efatal="${erest%%|*}"; esnip="${erest#*|}"
    printf '%s\t%s\t%s\tfatal=%s\texit=%s\t%s\n' \
      "$(date '+%Y-%m-%dT%H:%M:%S')" "$RA_NAME" "$ecat" "$efatal" "$status" "$esnip" >> "$ERROR_LOG"
    echo "!!! [$RA_NAME] ERROR [$ecat] (fatal=$efatal): $esnip !!!" | tee -a "$RUN_LOG"
    rm -f "$tmperr"
    if [[ "$efatal" == "1" ]]; then
      # Show-stopping (usage limit / auth): back off long and KEEP retrying so the
      # fleet auto-resumes when it clears. Do NOT count toward the stop streak.
      echo "!!! [$RA_NAME] FATAL [$ecat] — backing off ${FATAL_BACKOFF}s and retrying (auto-resume) !!!" | tee -a "$RUN_LOG"
      sleep "$FATAL_BACKOFF"
      continue
    fi
    empty_streak=$((empty_streak+1))
    [[ "$empty_streak" -ge "$MAX_EMPTY" ]] && { echo "=== [$RA_NAME] too many failures, stopping ===" | tee -a "$RUN_LOG"; break; }
    sleep "$EMPTY_BACKOFF"
    continue
  fi
  rm -f "$tmperr"

  # Read the LAST sentinel line the model printed.
  last_result="$(grep -oE "($DONE_SENTINEL[^\n]*|$NONE_SENTINEL)" <<<"$output" | tail -1)"

  if [[ "$last_result" == "$DONE_SENTINEL"* ]]; then
    empty_streak=0
    count=$((count+1))
    echo "=== [$RA_NAME] completed task process #$count ($last_result) ===" | tee -a "$RUN_LOG"
  elif [[ "$last_result" == "$NONE_SENTINEL" ]]; then
    empty_streak=$((empty_streak+1))
    echo "=== [$RA_NAME] no task available (streak $empty_streak/$MAX_EMPTY) ===" | tee -a "$RUN_LOG"
    [[ "$empty_streak" -ge "$MAX_EMPTY" ]] && { echo "=== [$RA_NAME] backlog drained, stopping ===" | tee -a "$RUN_LOG"; break; }
    sleep "$EMPTY_BACKOFF"
  else
    # Exit 0 but no sentinel => something went wrong (broken env, crash, etc).
    empty_streak=$((empty_streak+1))
    echo "!!! [$RA_NAME] no result sentinel — treating as failure (streak $empty_streak/$MAX_EMPTY) !!!" | tee -a "$RUN_LOG"
    [[ "$empty_streak" -ge "$MAX_EMPTY" ]] && { echo "=== [$RA_NAME] too many anomalies, stopping ===" | tee -a "$RUN_LOG"; break; }
    sleep "$EMPTY_BACKOFF"
  fi
done

echo "=== [$RA_NAME] done. $count task(s) completed this run. ===" | tee -a "$RUN_LOG"
