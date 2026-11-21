#!/usr/bin/env bash
# lib-land-record.sh — land_record, land-direct.sh's report of a landing it
# made to the daemon (agentrepl.v1.RecordDirectLanding), which records it with
# the path its own state names: the deploy mediator's, the queue bypass
# holder's, or a direct one. Every landing onto a main branch is recorded at
# landing time, so `agent-repld merge-queue landings` answers what landed
# through the queue and what landed otherwise without anyone counting commits.
#
# The caller is the workspace whose worktree the script runs in:
# `agent-repld` resolves it from the working directory, so land_record runs
# where land-direct.sh was started, never after a cd.

# LAND_RECORD_UNRECORDED is land_record's answer for a landing the daemon did
# not record: the branch HAS landed.
LAND_RECORD_UNRECORDED=5

# land_record MAIN BRANCH — report BRANCH's landing on MAIN's checked-out
# branch at its head now. Answers 0 once recorded; LAND_RECORD_UNRECORDED,
# printing the landed head and the command that records it, when the daemon
# did not record it; 2 when the head cannot be read.
land_record() {
  local main="$1" branch="$2" head
  head=$(git -C "$main" rev-parse HEAD) || return 2
  if ! agent-repld merge-queue record-landing -repository "$main" -branch "$branch" -head "$head" >&2; then
    echo "$branch landed as $head, but the daemon did not record the landing; once the daemon answers, record it: agent-repld merge-queue record-landing -repository $main -branch $branch -head $head"
    return "$LAND_RECORD_UNRECORDED"
  fi
}
