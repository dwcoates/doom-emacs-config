---
name: merge-queue
description: Land work on master through the agent-repl merge queue — the ONLY way into master in this repository. From this workspace, request the merge of its own branch, another workspace's branch, a subagent's branch, or a branch already merged upstream, then end the turn; the merge runs in this workspace and its outcome (landed, or failed in conflicts, tests or other) and any conflict or test-failure repairs arrive in this session. Also controls the queue itself, taking this workspace's or another workspace's merge off the queue and pausing or resuming the queue for one repository or all. Use whenever work is finished and must reach master, whenever you would otherwise run git merge, git commit, git cherry-pick or git push onto master, when a subagent reports a finished branch, whenever a queued merge must be removed or the queue paused or resumed, or when invoked as /merge-queue.
argument-hint: "own [keep-open] | workspace <worktree-dir> | branch <branch-name> | pr-merged | dequeue own | dequeue workspace <worktree-dir> | pause [<repo-dir>] | resume [<repo-dir>]"
allowed-tools: Bash(.claude/skills/merge-queue/run.sh:*)
---

## What This Skill Does

Asks the merge queue, from this workspace, to land a finished branch in its merge target: master for a workspace made off the main checkout, the PARENT workspace's checkout for a workspace created explicitly as a child of another. The merge runs in this workspace once the turn ends, and its outcome reports into this session; nothing merges by hand. It also removes a queued merge and pauses or resumes the queue, answering at once.

## Arguments

| Argument | Behaviour |
|---|---|
| `own` | Merge the branch checked out in this workspace's worktree now, into master, or into its parent workspace when it is a child; this workspace closes once it lands. |
| `own keep-open` | Merge the branch checked out in this workspace's worktree now, into master, or into its parent workspace when it is a child, and keep this workspace open once it lands. |
| `workspace <worktree-dir>` | Merge the branch checked out in ANOTHER workspace's worktree (for example a one-shot a subagent ran in), into master, or into that workspace's parent when it is a child; that workspace closes once it lands. |
| `branch <branch-name>` | Merge a branch that is no workspace (a subagent's `Agent`-tool branch). |
| `pr-merged` | This workspace's branch already merged upstream; update master from upstream and close this workspace. |
| `dequeue own` | Take this workspace's own merge off the queue. |
| `dequeue workspace <worktree-dir>` | Take ANOTHER workspace's merge off the queue, by its worktree. |
| `pause [<repo-dir>]` | Pause the queue of the repository whose main checkout is `<repo-dir>`, else of every repository. |
| `resume [<repo-dir>]` | Resume a paused queue, scoped as `pause` is. |

## Steps

0. Confirm the work is ready (merge arguments only; skip to step 1 for `dequeue`, `pause` and `resume`).
  - Every change is committed, and the applicable tests passed before each commit.
  - The branch to land is the one checked out in the worktree being merged; a worktree with no branch checked out is refused.
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
  - e. If `dequeue own`: call `.claude/skills/merge-queue/run.sh --dequeue-own`.
    - `EXIT CODE 0:` The removal is applied. Go to step 5.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
    - `EXIT CODE 5:` The removal was refused. Go to step 4.
    - `EXIT CODE 8:` No answer arrived in time. Go to step 6.
  - f. If `dequeue workspace <worktree-dir>`: call `.claude/skills/merge-queue/run.sh --dequeue-workspace <worktree-dir>`.
    - `EXIT CODE 0:` The removal is applied. Go to step 5.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
    - `EXIT CODE 5:` The removal was refused. Go to step 4.
    - `EXIT CODE 8:` No answer arrived in time. Go to step 6.
  - g. If `pause [<repo-dir>]`: call `.claude/skills/merge-queue/run.sh --pause-queue [<repo-dir>]`.
    - `EXIT CODE 0:` The pause is applied. Go to step 5.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
    - `EXIT CODE 5:` The pause was refused. Go to step 4.
    - `EXIT CODE 8:` No answer arrived in time. Go to step 6.
  - h. If `resume [<repo-dir>]`: call `.claude/skills/merge-queue/run.sh --resume-queue [<repo-dir>]`.
    - `EXIT CODE 0:` The resume is applied. Go to step 5.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
    - `EXIT CODE 5:` The resume was refused. Go to step 4.
    - `EXIT CODE 8:` No answer arrived in time. Go to step 6.

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

5. Applied (`dequeue`, `pause`, `resume`).
  - Report the printed outcome (`evicted`, `not_queued`, `paused`, `resumed`, or `unread`) and any other printed lines verbatim, then STOP.
  - *NOTE*: removing a merge that is not queued is applied with `not_queued`, not refused.

6. Unanswered (`dequeue`, `pause`, `resume`).
  - Report the printed lines verbatim and STOP.
  - CRITICAL: NEVER re-issue the request; it stays pending.

## Notes

- **CRITICAL: NEVER merge, commit, cherry-pick, rebase, reset or push onto master by hand.** The queue is the only path into master, and any other path is refused.
- **CRITICAL: On a failed or refused merge, report and stop.** Never retry by hand, never work around the queue.
- **IMPORTANT NOTE: A subagent's branch goes through `branch`, the same sequence.** Never fold it into master yourself.
- **CRITICAL: NEVER request a workspace merge for a branch that is not checked out in that worktree.** Check the branch out there first, or use `branch <branch-name>` for a branch that is no workspace.
- **CRITICAL: Removing a merge from the queue, or pausing or resuming the queue, goes through this skill, NEVER by hand.**
- **CRITICAL NOTE: Do not self-remediate a `run.sh` failure or read its internals.** React only to the documented exit codes.
