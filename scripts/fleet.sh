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
#   scripts/fleet.sh status              show whether RAs are running + task counts
#   scripts/fleet.sh logs                tail all RA logs (Ctrl-C to stop tailing)
#   scripts/fleet.sh stop                stop the whole fleet
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

cmd="${1:-status}"
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
    if pgrep -f "run_ra.sh RA-" >/dev/null 2>&1; then
      echo "found orphaned RA loops from a previous launch — reaping before start..."
      pkill -f "run_fleet.sh" 2>/dev/null
      pkill -f "run_ra.sh RA-" 2>/dev/null
      pkill -f "claude -p You are worker RA" 2>/dev/null
      sleep 3
    fi
    args=("$@")
    [[ "${#args[@]}" -eq 0 ]] && args=("$DEFAULT_RAS")   # default fleet size
    # nohup + & + disown => survives this shell exiting and does NOT block the caller.
    nohup bash "$REPO_DIR/scripts/run_fleet.sh" "${args[@]}" >"$MASTER_LOG" 2>&1 &
    echo $! >"$PID_FILE"
    disown 2>/dev/null || true
    echo "fleet started detached (pid $(cat "$PID_FILE"), args: ${args[*]})."
    echo "logs: ra_logs/*.log   |   status: scripts/fleet.sh status   |   stop: scripts/fleet.sh stop"
    ;;

  status)
    if [[ -f "$PID_FILE" ]] && kill -0 "$(cat "$PID_FILE")" 2>/dev/null; then
      echo "fleet: RUNNING (launcher pid $(cat "$PID_FILE"))"
    else
      echo "fleet: not running"
    fi
    # Count DISTINCT RA names (a loop briefly forks a subshell during the
    # claude command-substitution, which would otherwise double-count).
    workers=$(pgrep -fl "run_ra.sh RA-" 2>/dev/null | grep -oE "RA-[A-Za-z0-9-]+" | sort -u | wc -l | tr -d ' ')
    echo "active RA loops: $workers"
    if compgen -G "$LOG_DIR/RA-*.log" >/dev/null; then
      echo "per-RA completed-task counts this session:"
      for f in "$LOG_DIR"/RA-*.log; do
        name="$(basename "$f" .log)"
        done_n=$(grep -c "completed task process" "$f" 2>/dev/null); done_n="${done_n:-0}"
        echo "  $name: $done_n"
      done
    fi
    ;;

  logs)
    tail -n 20 -f "$LOG_DIR"/*.log
    ;;

  stop)
    pkill -f "run_fleet.sh" 2>/dev/null || true
    pkill -f "run_ra.sh" 2>/dev/null || true
    [[ -f "$PID_FILE" ]] && rm -f "$PID_FILE"
    echo "fleet stopped (in-flight claude processes finish their current task)."
    ;;

  *)
    echo "usage: $0 {start [N|name...]|status|logs|stop}" >&2
    exit 1
    ;;
esac
