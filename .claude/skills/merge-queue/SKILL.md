---
name: merge-queue
description: Land work on master through the agent-repl merge queue — the ONLY way into master in this repository. From this workspace, request the merge of its own branch, only some commits of its own branch (a fix made amid unfinished work), another workspace's branch, a subagent's branch, or a branch already merged upstream, then end the turn. Its own branch merges detached by default, in a merge workspace forked from this one, so this workspace stays free and the outcome arrives here as a report; every other merge runs in this workspace, and its outcome (landed, or failed in conflicts, tests or other) and any conflict or test-failure repairs arrive in this session. Any merge may be asked for at the FRONT of the queue, ahead of every ordinary merge waiting (a hotfix). Also controls the queue itself, taking this workspace's or another workspace's merge off the queue and pausing or resuming the queue for one repository or all. Use whenever work is finished and must reach master, whenever you would otherwise run git merge, git commit, git cherry-pick or git push onto master, when a subagent reports a finished branch, whenever a queued merge must be removed or the queue paused or resumed, or when invoked as /merge-queue.
argument-hint: "own [in-place | keep-open] [front] | workspace <worktree-dir> [front] | branch <branch-name> [front] | commits <commit>... [front] | pr-merged [front] | dequeue own | dequeue workspace <worktree-dir> | pause [<repo-dir>] | resume [<repo-dir>]"
allowed-tools: Bash(.claude/skills/merge-queue/run.sh:*)
---

## What This Skill Does

Asks the merge queue, from this workspace, to land a finished branch in its merge target: master for a workspace made off the main checkout, the PARENT workspace's checkout for a workspace created explicitly as a child of another. Once the turn ends, this workspace's own branch is merged DETACHED by default: in a merge workspace forked from this conversation, while this workspace stays free, with the outcome reported back into this session. Every other merge, and an own-branch merge asked for in place, runs in this workspace and reports into this session; nothing merges by hand. It also removes a queued merge and pauses or resumes the queue, answering at once.

## Arguments

| Argument | Behaviour |
|---|---|
| `own` | Merge the branch checked out in this workspace's worktree as it stands now, into master, or into its parent workspace when it is a child, DETACHED: in a merge workspace forked from this one, leaving this workspace free; the outcome is reported back into this session. |
| `own in-place` | Merge the branch checked out in this workspace's worktree in THIS workspace, holding its prompts until the merge ends; this workspace closes once it lands. |
| `own keep-open` | Merge the branch checked out in this workspace's worktree in THIS workspace, and keep this workspace open once it lands. |
| `workspace <worktree-dir>` | Merge the branch checked out in ANOTHER workspace's worktree (for example a one-shot a subagent ran in), into master, or into that workspace's parent when it is a child; that workspace closes once it lands. |
| `branch <branch-name>` | Merge a branch that is no workspace (a subagent's `Agent`-tool branch). |
| `commits <commit>...` | Merge ONLY the named commits of this workspace's branch, rebased onto master, leaving the rest of the branch unmerged; this workspace stays open. |
| `pr-merged` | This workspace's branch already merged upstream; update master from upstream and close this workspace. |
| `front` | Added to any merge argument above (`own`, `workspace`, `branch`, `commits`, `pr-merged`): the merge goes to the FRONT of its queue once it is put in line, ahead of every ordinary merge waiting and behind any earlier front merge, never ahead of the merge already running. |
| `dequeue own` | Take this workspace's own merge off the queue. |
| `dequeue workspace <worktree-dir>` | Take ANOTHER workspace's merge off the queue, by its worktree. |
| `pause [<repo-dir>]` | Pause the queue of the repository whose main checkout is `<repo-dir>`, else of every repository. |
| `resume [<repo-dir>]` | Resume a paused queue, scoped as `pause` is. |

## Steps

0. Confirm the work is ready (merge arguments only; skip to step 1 for `dequeue`, `pause` and `resume`).
  - Every change is committed, and the applicable tests passed before each commit.
  - The branch to land is the one checked out in the worktree being merged; a worktree with no branch checked out is refused.
  - **Why this lives here**: the queue lands exactly what is committed, and a red test gate comes back as a repair prompt in the session the merge runs in.

1. Dispatch on the argument.
  - *NOTE*: when `front` was given with a merge argument (steps 1a–1e), append `--front` to that step's `run.sh` call; the exit codes are unchanged.
  - a. If `own`: call `.claude/skills/merge-queue/run.sh --enqueue-own`.
    - *NOTE*: with `own in-place`, call `.claude/skills/merge-queue/run.sh --enqueue-own --in-place` instead.
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
  - d. If `commits <commit>...`: call `.claude/skills/merge-queue/run.sh --land-commits <commit>...`.
    - `EXIT CODE 0:` The merge of those commits is requested. Record the printed branch name for step 3c, then continue to step 2.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
    - `EXIT CODE 5:` The merge was refused. Go to step 4.
    - `EXIT CODE 9:` The commits do not apply on their own onto master. Report the printed line and STOP.
  - e. If `pr-merged`: call `.claude/skills/merge-queue/run.sh --pr-merged`.
    - `EXIT CODE 0:` The update is requested. Continue to step 2.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
    - `EXIT CODE 5:` The update was refused. Go to step 4.
  - f. If `dequeue own`: call `.claude/skills/merge-queue/run.sh --dequeue-own`.
    - `EXIT CODE 0:` The removal is applied. Go to step 5.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
    - `EXIT CODE 5:` The removal was refused. Go to step 4.
    - `EXIT CODE 8:` No answer arrived in time. Go to step 6.
  - g. If `dequeue workspace <worktree-dir>`: call `.claude/skills/merge-queue/run.sh --dequeue-workspace <worktree-dir>`.
    - `EXIT CODE 0:` The removal is applied. Go to step 5.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
    - `EXIT CODE 5:` The removal was refused. Go to step 4.
    - `EXIT CODE 8:` No answer arrived in time. Go to step 6.
  - h. If `pause [<repo-dir>]`: call `.claude/skills/merge-queue/run.sh --pause-queue [<repo-dir>]`.
    - `EXIT CODE 0:` The pause is applied. Go to step 5.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
    - `EXIT CODE 5:` The pause was refused. Go to step 4.
    - `EXIT CODE 8:` No answer arrived in time. Go to step 6.
  - i. If `resume [<repo-dir>]`: call `.claude/skills/merge-queue/run.sh --resume-queue [<repo-dir>]`.
    - `EXIT CODE 0:` The resume is applied. Go to step 5.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
    - `EXIT CODE 5:` The resume was refused. Go to step 4.
    - `EXIT CODE 8:` No answer arrived in time. Go to step 6.

2. End the turn.
  - Say in one line that the merge is requested, then END THE TURN IMMEDIATELY.
  - CRITICAL: the merge cannot start while this turn runs, so do NOT wait, poll, or check on it.

3. Answer the merge's prompts (later turns).
  - *NOTE*: a detached `own` merge sends its conflict and test-failure prompts to its merge workspace, never to this session; this session receives only its report of how the merge ended, which needs no answer beyond step 3d.
  - a. A conflict-resolution prompt: resolve the conflicted files in the worktree the prompt names and `git add` them.
    - CRITICAL: NEVER run `git rebase --continue`, `--abort` or `--skip`; the queue continues the rebase itself.
  - b. A test-failure prompt: fix the failure in the worktree the prompt names and commit the fix with its tests.
  - c. A LANDED outcome for a `branch` or `commits` merge: call `.claude/skills/merge-queue/run.sh --remove-branch <branch-name>`.
    - `EXIT CODE 0:` The subagent's worktree and branch are gone. Report the landing and stop.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
  - d. A failed outcome (conflicts, tests or other), or a detached merge's report of how it ended: report it in one line and STOP.

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
- **IMPORTANT NOTE: To land a fix without the unfinished work around it, use `commits` with only the fix's commits.** Never cherry-pick them onto master by hand.
- **IMPORTANT NOTE: A merge that must land before the work already waiting is asked for with `front`.** Never pause, dequeue or reorder other merges by hand to get ahead of them.
- **CRITICAL: NEVER request a workspace merge for a branch that is not checked out in that worktree.** Check the branch out there first, or use `branch <branch-name>` for a branch that is no workspace.
- **CRITICAL: Removing a merge from the queue, or pausing or resuming the queue, goes through this skill, NEVER by hand.**
- **CRITICAL NOTE: Do not self-remediate a `run.sh` failure or read its internals.** React only to the documented exit codes.
