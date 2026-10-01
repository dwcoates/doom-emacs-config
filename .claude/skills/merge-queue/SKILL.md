---
name: merge-queue
description: Land work on master through the agent-repl merge queue — the ONLY way into master in this repository. From this workspace, request the merge of its own branch, another workspace's branch, a subagent's branch, or a branch already merged upstream, then end the turn; the merge runs in this workspace and its outcome (landed, or failed in conflicts, tests or other) and any conflict or test-failure repairs arrive in this session. Use whenever work is finished and must reach master, whenever you would otherwise run git merge, git commit, git cherry-pick or git push onto master, when a subagent reports a finished branch, or when invoked as /merge-queue.
argument-hint: "own [keep-open] | workspace <worktree-dir> | branch <branch-name> | pr-merged"
allowed-tools: Bash(.claude/skills/merge-queue/run.sh:*)
---

## What This Skill Does

Asks the merge queue, from this workspace, to land a finished branch on master. The merge runs in this workspace once the turn ends, and its outcome reports into this session; nothing merges by hand.

## Arguments

| Argument | Behaviour |
|---|---|
| `own` | Merge this workspace's own branch; this workspace closes once it lands. |
| `own keep-open` | Merge this workspace's own branch and keep this workspace open once it lands. |
| `workspace <worktree-dir>` | Merge ANOTHER workspace's branch (for example a one-shot a subagent ran in); that workspace closes once it lands. |
| `branch <branch-name>` | Merge a branch that is no workspace (a subagent's `Agent`-tool branch). |
| `pr-merged` | This workspace's branch already merged upstream; update master from upstream and close this workspace. |

## Steps

0. Confirm the work is ready.
  - Every change is committed, and the applicable tests passed before each commit.
  - **Why this lives here**: the queue lands exactly what is committed, and a red test gate comes back as a repair prompt in this session.

1. Dispatch on the argument.
  - a. If `own`: call `.claude/skills/merge-queue/run.sh --enqueue-own`.
    - *NOTE*: with `own keep-open`, call `.claude/skills/merge-queue/run.sh --enqueue-own --keep-open` instead.
    - `EXIT CODE 0:` The merge is requested. Continue to step 2.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
    - `EXIT CODE 5:` The merge was refused. Go to step 4.
    - `EXIT CODE 6:` Uncommitted work remains. Commit it (with its tests passing), then restart step 1a.
  - b. If `workspace <worktree-dir>`: call `.claude/skills/merge-queue/run.sh --land-workspace <worktree-dir>`.
    - `EXIT CODE 0:` The merge is requested. Continue to step 2.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
    - `EXIT CODE 5:` The merge was refused. Go to step 4.
    - `EXIT CODE 7:` The directory is this session's own workspace. Restart step 1 with `own`.
  - c. If `branch <branch-name>`: call `.claude/skills/merge-queue/run.sh --land-branch <branch-name>`.
    - `EXIT CODE 0:` The merge is requested. Continue to step 2.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
    - `EXIT CODE 5:` The merge was refused. Go to step 4.
  - d. If `pr-merged`: call `.claude/skills/merge-queue/run.sh --pr-merged`.
    - `EXIT CODE 0:` The update is requested. Continue to step 2.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
    - `EXIT CODE 5:` The update was refused. Go to step 4.

2. End the turn.
  - Say in one line that the merge is requested, then END THE TURN IMMEDIATELY.
  - CRITICAL: the merge cannot start while this turn runs, so do NOT wait, poll, or check on it.

3. Answer the merge's prompts (later turns).
  - a. A conflict-resolution prompt: resolve the conflicted files in the worktree the prompt names and `git add` them.
    - CRITICAL: NEVER run `git rebase --continue`, `--abort` or `--skip`; the queue continues the rebase itself.
  - b. A test-failure prompt: fix the failure in the worktree the prompt names and commit the fix with its tests.
  - c. A LANDED outcome for a `branch` merge: call `.claude/skills/merge-queue/run.sh --remove-branch <branch-name>`.
    - `EXIT CODE 0:` The subagent's worktree and branch are gone. Report the landing and stop.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
  - d. A failed outcome (conflicts, tests or other): report it in one line and STOP.

4. Refused.
  - Report the printed refusal verbatim and STOP.

## Notes

- **CRITICAL: NEVER merge, commit, cherry-pick, rebase, reset or push onto master by hand.** The queue is the only path, and the repository's hook refuses the rest.
- **CRITICAL: On a failed or refused merge, report and stop.** Never retry by hand, never work around the queue.
- **IMPORTANT NOTE: A subagent's branch goes through `branch`, the same sequence.** Never fold it into master yourself.
- **CRITICAL NOTE: Do not self-remediate a `run.sh` failure or read its internals.** React only to the documented exit codes.
