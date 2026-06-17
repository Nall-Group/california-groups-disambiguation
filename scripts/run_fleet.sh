#!/usr/bin/env bash
#
# run_fleet.sh — Launch N headless worker RAs in parallel.
#
# Each RA is an independent run_ra.sh loop spawning fresh per-task processes.
# They coordinate through the TASKS.md write queue + data write queue, exactly
# like the interactive RAs do today — so concurrent edits stay safe.
#
# Usage:
#   scripts/run_fleet.sh 3                 # RA-Fleet-1, RA-Fleet-2, RA-Fleet-3
#   scripts/run_fleet.sh RA-Delta RA-Echo  # explicit names
#
# Env knobs are passed through to run_ra.sh (MAX_TASKS, MODEL, etc.).
# Logs: ra_logs/<RA-NAME>.log . Stop everything with: pkill -f run_ra.sh

set -uo pipefail

REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_DIR"

names=()
if [[ "${1:-}" =~ ^[0-9]+$ ]]; then
  for i in $(seq 1 "$1"); do names+=("RA-Fleet-$i"); done
else
  names=("$@")
fi

if [[ "${#names[@]}" -eq 0 ]]; then
  echo "usage: $0 <N> | <name1> <name2> ..." >&2
  exit 1
fi

pids=()
for name in "${names[@]}"; do
  echo ">>> launching $name"
  bash "$REPO_DIR/scripts/run_ra.sh" "$name" &
  pids+=("$!")
  sleep 3   # small stagger so they don't grab the TASKS.md queue at the same instant
done

echo ">>> ${#names[@]} RA(s) running: ${names[*]}"
echo ">>> tail logs:  tail -f ra_logs/*.log"
echo ">>> stop all:   pkill -f run_ra.sh"

wait "${pids[@]}"
echo ">>> fleet finished."
