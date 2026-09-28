---
name: merge-queue
description: Land work on master through the agent-repl merge queue — the ONLY way into master in this repository. Enqueue this workspace's own branch (then end the turn), or land another workspace or a subagent's branch and wait for the outcome: landed, parked awaiting guidance, or failed with its reason. Use whenever work is finished and must reach master, whenever you would otherwise run git merge, git commit, git cherry-pick or git push onto master, when a subagent reports a finished branch, or when invoked as /merge-queue.
argument-hint: "own | workspace <worktree-dir> | branch <branch-name>"
allowed-tools: Bash(.claude/skills/merge-queue/run.sh:*)
---

## What This Skill Does

Hands a finished branch to the merge queue, which merges it, runs the test gate, and fast-forwards master only when the gate passes. It reports the outcome and never merges by hand.

## Arguments

| Argument | Behaviour |
|---|---|
| `own` | Enqueue the merge of the workspace this session runs in. The merge starts once this turn ends. |
| `workspace <worktree-dir>` | Land ANOTHER workspace (for example a one-shot a subagent ran in) and wait for the outcome. |
| `branch <branch-name>` | Land a branch that is no workspace (a subagent's `Agent`-tool branch) and wait for the outcome. |

## Steps

0. Confirm the work is ready.
  - Every change is committed, and the applicable tests passed before each commit.
  - **Why this lives here**: the queue lands exactly what is committed, and a red gate only comes back as a repair round.

1. Dispatch on the argument.
  - a. If `own`: call `.claude/skills/merge-queue/run.sh --enqueue-own`.
    - `EXIT CODE 0:` The merge is queued. Continue to step 2.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
    - `EXIT CODE 3:` The merge parked. Go to step 3.
    - `EXIT CODE 4:` The merge failed. Go to step 4.
    - `EXIT CODE 5:` The daemon refused the merge. Go to step 5.
    - `EXIT CODE 6:` Uncommitted work remains. Commit it (with its tests passing), then restart step 1a.
  - b. If `workspace <worktree-dir>`: call `.claude/skills/merge-queue/run.sh --land-workspace <worktree-dir>`.
    - `EXIT CODE 0:` LANDED. Report the landing and stop.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
    - `EXIT CODE 3:` The merge parked. Go to step 3.
    - `EXIT CODE 4:` The merge failed. Go to step 4.
    - `EXIT CODE 5:` The daemon refused the merge. Go to step 5.
    - `EXIT CODE 7:` The directory is this session's own workspace. Restart step 1 with `own`.
  - c. If `branch <branch-name>`: call `.claude/skills/merge-queue/run.sh --land-branch <branch-name>`.
    - `EXIT CODE 0:` LANDED. Report the landing, then remove the subagent's branch and worktree, and stop.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
    - `EXIT CODE 3:` The merge parked. Go to step 3.
    - `EXIT CODE 4:` The merge failed. Go to step 4.
    - `EXIT CODE 5:` The daemon refused the merge. Go to step 5.

2. End the turn (`own` only).
  - Say in one line that the merge is queued, then END THE TURN IMMEDIATELY.
  - CRITICAL: the merge cannot start while this turn runs, so do NOT wait, poll, or check on it.
  - *NOTE*: conflict and test-failure repairs arrive in this session as new prompts; answer each on this workspace's own branch and commit.

3. Parked.
  - Report the printed parked line verbatim and STOP.
  - The merge waits for guidance typed into the merging workspace; it is not yours to resolve by hand.

4. Failed.
  - Report the printed reason verbatim and STOP.

5. Refused.
  - Report the printed refusal verbatim and STOP.

## Notes

- **CRITICAL: NEVER merge, commit, cherry-pick, rebase, reset or push onto master by hand.** The queue is the only path, and the repository's hook refuses the rest.
- **CRITICAL: On parked, failed or refused, report and stop.** Never retry by hand, never work around the queue.
- **IMPORTANT NOTE: A subagent's branch goes through `branch`, the same sequence.** Never fold it into master yourself.
- **CRITICAL NOTE: Do not self-remediate a `run.sh` failure or read its internals.** React only to the documented exit codes.
