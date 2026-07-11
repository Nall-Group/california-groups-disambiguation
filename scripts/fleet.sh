#!/usr/bin/env bash
#
# fleet.sh — Control panel for the headless RA fleet.
#
# Designed to be invoked by the management agent: `start` returns IMMEDIATELY
# (the fleet runs fully detached via nohup), so the management session stays
# free to keep talking to the user while the RAs grind through the backlog.
#
# Commands:
#   scripts/fleet.sh start [N|name...]   start a detached fleet (default N=3)
#   scripts/fleet.sh logs                tail all RA logs (Ctrl-C to stop tailing)
#   scripts/fleet.sh errors              show classified error log (usage-limit stalls, etc.)
#   scripts/fleet.sh stop                stop the whole fleet
#
# NOTE: there is deliberately NO `status` command. Process-liveness checks
# (kill -0 / pgrep) are blocked under the Claude Bash sandbox and report a FALSE
# "not running" even when the fleet is fine. Judge the fleet by on-disk signals
# that work everywhere: `git log --oneline` (fresh RA commits = alive) and
# `scripts/fleet.sh errors` / `tail ra_logs/errors.log` (recent usage_limit = stalled).
#
# Env knobs (passed through to run_ra.sh): MAX_TASKS, MODEL, MAX_EMPTY, EMPTY_BACKOFF

set -uo pipefail

REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_DIR"
LOG_DIR="$REPO_DIR/ra_logs"
mkdir -p "$LOG_DIR"
MASTER_LOG="$LOG_DIR/fleet.out"
PID_FILE="$LOG_DIR/fleet.pid"

# Default fleet size when `start` is given no count. Override per-invocation
# (scripts/fleet.sh start 5) or persistently (DEFAULT_RAS=4 scripts/fleet.sh start).
DEFAULT_RAS="${DEFAULT_RAS:-3}"

# Fully reap the fleet: SIGTERM first, then escalate to SIGKILL for anything
# still alive — hung workers ignore SIGTERM and get reparented to init (ppid 1),
# so a plain pkill leaves orphans that collide with a fresh fleet. This sweeps
# launchers, loops, AND worker processes (including orphans) with -9.
_reap_fleet() {
  pkill -f "run_fleet.sh" 2>/dev/null || true
  pkill -f "run_ra.sh RA-" 2>/dev/null || true
  pkill -f "claude -p You are worker RA" 2>/dev/null || true
  sleep 2
  # escalate: SIGKILL whatever survived (orphaned/hung workers included)
  pkill -9 -f "run_fleet.sh" 2>/dev/null || true
  pkill -9 -f "run_ra.sh RA-" 2>/dev/null || true
  pkill -9 -f "claude -p You are worker RA" 2>/dev/null || true
  sleep 1
  # Sweep leftover per-worker temp files (a SIGKILL'd loop can't run its own trap).
  rm -f "$LOG_DIR"/.RA-*.out.* "$LOG_DIR"/.RA-*.err.* "$LOG_DIR"/.RA-*.out.*.kill 2>/dev/null || true
  local left
  left="$(pgrep -f 'claude -p You are worker RA' | wc -l | tr -d ' ')"
  [[ "$left" != "0" ]] && echo "warning: $left worker(s) still not dead (likely stuck in a syscall; will exit on socket timeout)"
}

cmd="${1:-help}"
shift || true

case "$cmd" in
  start)
    if [[ -f "$PID_FILE" ]] && kill -0 "$(cat "$PID_FILE")" 2>/dev/null; then
      echo "fleet already running (pid $(cat "$PID_FILE")). Use 'stop' first."
      exit 1
    fi
    # Guard against orphaned loops from a prior launch whose launcher died
    # (e.g. killed by a usage limit) while its run_ra.sh loops kept retrying.
    # Starting on top of those would run TWO loops per RA name, which collide
    # on the write queue under a shared identity. Reap any stragglers first.
    if pgrep -f "run_ra.sh RA-" >/dev/null 2>&1 || pgrep -f "claude -p You are worker RA" >/dev/null 2>&1; then
      echo "found stragglers from a previous launch — hard-reaping before start..."
      _reap_fleet
    fi
    args=("$@")
    [[ "${#args[@]}" -eq 0 ]] && args=("$DEFAULT_RAS")   # default fleet size
    # nohup + & + disown => survives this shell exiting and does NOT block the caller.
    nohup bash "$REPO_DIR/scripts/run_fleet.sh" "${args[@]}" >"$MASTER_LOG" 2>&1 &
    echo $! >"$PID_FILE"
    disown 2>/dev/null || true
    echo "fleet started detached (pid $(cat "$PID_FILE"), args: ${args[*]})."
    echo "alive? -> git log --oneline (fresh RA commits)  |  stalls -> scripts/fleet.sh errors  |  stop -> scripts/fleet.sh stop"
    ;;

  logs)
    tail -n 20 -f "$LOG_DIR"/*.log
    ;;

  errors)
    EL="$LOG_DIR/errors.log"
    if [[ ! -s "$EL" ]]; then echo "no classified errors recorded (errors.log empty)"; exit 0; fi
    echo "=== error counts by category (all time) ==="
    awk -F'\t' '{c[$3]++} END{for(k in c) printf "  %-16s %d\n", k, c[k]}' "$EL" | sort -k2 -rn
    echo "=== FATAL errors (show-stoppers) ==="
    grep -E "fatal=1" "$EL" | tail -8 || echo "  none"
    echo "=== most recent 12 errors ==="
    tail -12 "$EL"
    ;;

  stop)
    _reap_fleet
    [[ -f "$PID_FILE" ]] && rm -f "$PID_FILE"
    echo "fleet stopped (loops + workers hard-killed)."
    ;;

  help|--help|-h)
    echo "usage: $0 {start [N|name...]|logs|errors|stop}"
    echo "  (no 'status' — check 'git log --oneline' for fresh RA commits; 'scripts/fleet.sh errors' for stalls)"
    ;;

  *)
    echo "usage: $0 {start [N|name...]|logs|errors|stop}" >&2
    exit 1
    ;;
esac
