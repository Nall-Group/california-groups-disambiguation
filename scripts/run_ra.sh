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
# Hang detection. The primary signal is OUTPUT SILENCE, not wall-clock: a worker
# that produces no streaming output for IDLE_TIMEOUT is hung and gets killed, so
# the loop relaunches. A still-streaming task is never killed no matter how long.
IDLE_TIMEOUT="${IDLE_TIMEOUT:-180}"    # 3 min of no output => hung
IDLE_POLL="${IDLE_POLL:-20}"           # how often the watchdog checks
# Far backstop on total task wall-time (catches pathological non-silent loops).
TASK_TIMEOUT="${TASK_TIMEOUT:-2400}"   # 40 minutes
MODEL_ARG=()
[[ -n "${MODEL:-}" ]] && MODEL_ARG=(--model "$MODEL")

LOG_DIR="$REPO_DIR/ra_logs"
mkdir -p "$LOG_DIR"
RUN_LOG="$LOG_DIR/${RA_NAME}.log"

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
  claude -p "$PROMPT" \
      --dangerously-skip-permissions \
      --output-format stream-json --include-partial-messages --verbose \
      ${MODEL_ARG[@]+"${MODEL_ARG[@]}"} >"$tmpout" 2>>"$RUN_LOG" &
  cmdpid=$!
  (
    while kill -0 "$cmdpid" 2>/dev/null; do
      sleep "$IDLE_POLL"
      now="$(date +%s)"
      mt="$(stat -f %m "$tmpout" 2>/dev/null || echo "$now")"
      st="$(stat -f %B "$tmpout" 2>/dev/null || echo "$now")"
      if (( now - mt >= IDLE_TIMEOUT )); then
        echo "!!! [$RA_NAME] no output for ${IDLE_TIMEOUT}s — killing hung worker !!!" >>"$RUN_LOG"
        kill -9 "$cmdpid" 2>/dev/null; break
      fi
      if (( now - st >= TASK_TIMEOUT )); then
        echo "!!! [$RA_NAME] task exceeded ${TASK_TIMEOUT}s backstop — killing worker !!!" >>"$RUN_LOG"
        kill -9 "$cmdpid" 2>/dev/null; break
      fi
    done
  ) >/dev/null 2>&1 &
  watchpid=$!
  wait "$cmdpid" 2>/dev/null
  status=$?
  kill "$watchpid" 2>/dev/null; wait "$watchpid" 2>/dev/null
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
    echo "!!! [$RA_NAME] claude exited $status; backing off ${EMPTY_BACKOFF}s !!!" | tee -a "$RUN_LOG"
    empty_streak=$((empty_streak+1))
    [[ "$empty_streak" -ge "$MAX_EMPTY" ]] && { echo "=== [$RA_NAME] too many failures, stopping ===" | tee -a "$RUN_LOG"; break; }
    sleep "$EMPTY_BACKOFF"
    continue
  fi

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
