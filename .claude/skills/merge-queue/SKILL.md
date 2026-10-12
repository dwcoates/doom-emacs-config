---
name: merge-queue
description: Land work on master through the agent-repl merge queue — the ONLY way into master in this repository. From this workspace, request the merge of its own branch, only some changes of its own branch (a fix made amid unfinished work), another workspace's branch, a subagent's branch, or a branch already merged upstream, then end the turn. It also announces, from anywhere inside a turn, a one-shot merge of this workspace that runs once the turn ends and closes this workspace once it lands. Merges are SYNC by default, running in this workspace, and the outcome (landed, or failed in conflicts, tests or other) and any conflict or test-failure repairs arrive in this session. Its own branch can instead merge ASYNC, in a hidden merge workspace named by the caller, so this workspace stays free; reserved for genuinely tangential or explicitly detached work. Any merge may be asked for at the FRONT of the queue, ahead of every ordinary merge waiting (a hotfix). Also controls the queue itself, taking this workspace's or another workspace's merge off the queue, pausing or resuming the queue for one repository or all, and holding or releasing the queue's rollouts (merges still land, but a landing no longer redeploys the running runtime). Use whenever work is finished and must reach master, whenever you would otherwise run git merge, git commit, git cherry-pick or git push onto master, when a subagent reports a finished branch, whenever a queued merge must be removed or the queue paused or resumed, whenever a hand-deployed build (a profiling build) must not be replaced by the next landing, or when invoked as /merge-queue.
argument-hint: "own [in-place | sync | keep-open] [front] | own async <name> [pick <instruction>] [front] | one-shot [front] | workspace <worktree-dir> [front] | branch <branch-name> [front] | commits <commit>... [front] | pr-merged [front] | dequeue own | dequeue workspace <worktree-dir> | pause [<repo-dir>] | resume [<repo-dir>] | hold-rollouts | release-rollouts"
allowed-tools: Bash(.claude/skills/merge-queue/run.sh:*)
---

## What This Skill Does

Asks the merge queue, from this workspace, to land a finished branch in its merge target: master for a workspace made off the main checkout, the PARENT workspace's checkout for a workspace created explicitly as a child of another. Once the turn ends, the merge runs SYNC by default: in this workspace, reporting into this session, with this session's agent resolving conflicts and test failures. An `own async` merge runs ASYNC instead: in a hidden merge workspace forked from this one under a name the caller supplies, while this workspace stays free; its progress streams into the merge bubble here, and the merge workspace is revealed when it fails, asks a question or permission, is stopped from the bubble, or is selected. Nothing merges by hand. It also removes a queued merge, pauses or resumes the queue, and holds or releases the queue's rollouts, answering at once.

## Arguments

| Argument | Behaviour |
|---|---|
| `own` | Merge the branch checked out in this workspace's worktree as it stands now, into master, or into its parent workspace when it is a child, SYNC: in THIS workspace, holding its prompts until the merge ends; this workspace closes once it lands. |
| `own in-place` | The same as `own`, said explicitly. |
| `own sync` | The same as `own`, said explicitly. |
| `own keep-open` | Merge this workspace's branch SYNC, and keep this workspace open once it lands. |
| `own async <name>` | Merge this workspace's whole branch ASYNC, in a hidden merge workspace named `<name>` (a short slug naming what is merged), leaving this workspace free and open. |
| `own async <name> pick <instruction>` | Merge ASYNC only the changes `<instruction>` names (free text: commits, or hunks of them), which the merge workspace's agent cherry-picks onto the target before the merge joins the queue; this workspace stays open. |
| `one-shot` | Announce NOW, without ending the turn, that this workspace merges its branch in place once the current turn ends and closes once it lands; this workspace is marked a one-shot at once. |
| `workspace <worktree-dir>` | Merge the branch checked out in ANOTHER workspace's worktree (for example a one-shot a subagent ran in), into master, or into that workspace's parent when it is a child; that workspace closes once it lands. |
| `branch <branch-name>` | Merge a branch that is no workspace (a subagent's `Agent`-tool branch). |
| `commits <commit>...` | Merge ONLY the named whole commits of this workspace's branch, rebased onto master, leaving the rest of the branch unmerged; this workspace stays open. |
| `pr-merged` | This workspace's branch already merged upstream; update master from upstream and close this workspace. |
| `front` | Added to any merge argument above (`own`, `one-shot`, `workspace`, `branch`, `commits`, `pr-merged`): the merge goes to the FRONT of its queue once it is put in line, ahead of every ordinary merge waiting and behind any earlier front merge, never ahead of the merge already running. |
| `dequeue own` | Take this workspace's own merge off the queue. |
| `dequeue workspace <worktree-dir>` | Take ANOTHER workspace's merge off the queue, by its worktree. |
| `pause [<repo-dir>]` | Pause the queue of the repository whose main checkout is `<repo-dir>`, else of every repository. A paused queue advances nothing: merges still enqueue, but every merge running in it is suspended at once (it resumes at the step it recorded), and nothing is admitted, promoted or resumed until `resume`. |
| `resume [<repo-dir>]` | Resume a paused queue, scoped as `pause` is. |
| `hold-rollouts` | Hold the queue's ROLLOUTS, daemon-wide and durably: merges keep landing, but a landing in the daemon's own checkout no longer redeploys, hot-reloads or bounces the running runtime. |
| `release-rollouts` | End the hold; the next landing redeploys. Nothing that landed while held is deployed until then. |

## Steps

0. Confirm the work is ready (merge arguments only; skip to step 1 for `one-shot`, `dequeue`, `pause`, `resume`, `hold-rollouts` and `release-rollouts`).
  - Every change is committed, and the applicable tests passed before each commit.
  - The branch to land is the one checked out in the worktree being merged; a worktree with no branch checked out is refused.
  - For every `own` form except `own async <name> pick`: this workspace's requested changes are FULLY implemented.
    - Every ruling the user gave in this workspace is implemented.
    - No part of a ruling is deferred as an "open question".
    - An open question you raised that the user's rulings already answer is unfinished work.
    - Nothing the user asked for is left unfinished.
    - CRITICAL: if anything is unfinished, finish it, or STOP and surface it to the user. NEVER request the merge instead.
  - For `own async <name> pick`: every change the instruction names is FULLY implemented and committed.
  - Choose the form of an own-branch merge by these rules, in order.
    - i. The user named a form: use it.
    - ii. This workspace is a one-shot: use `one-shot` while work remains in the turn, else `own`.
    - iii. Only a little requested work remains: finish it, then use `own`.
    - iv. The changes to land are genuinely tangential to this workspace's remaining work, or the user asked for them detached: use `own async <name>`, adding `pick <instruction>` when only some changes land.
    - v. Otherwise: use `own`.
  - **Why this lives here**: the queue lands exactly what is committed, and a red test gate comes back as a repair prompt in the session the merge runs in. An own-branch merge lands the branch as it stands when the turn ends, so a ruling left half-done lands half-done.

1. Dispatch on the argument.
  - *NOTE*: when `front` was given with a merge argument (steps 1a–1e and 1j), append `--front` to that step's `run.sh` call; the exit codes are unchanged.
  - a. If `own`: call `.claude/skills/merge-queue/run.sh --enqueue-own`.
    - *NOTE*: with `own in-place`, call `.claude/skills/merge-queue/run.sh --enqueue-own --in-place` instead.
    - *NOTE*: with `own sync`, call `.claude/skills/merge-queue/run.sh --enqueue-own --sync` instead.
    - *NOTE*: with `own keep-open`, call `.claude/skills/merge-queue/run.sh --enqueue-own --keep-open` instead.
    - *NOTE*: with `own async <name>`, call `.claude/skills/merge-queue/run.sh --enqueue-own --async <name>` instead.
    - *NOTE*: with `own async <name> pick <instruction>`, call `.claude/skills/merge-queue/run.sh --enqueue-own --async <name> --pick "<instruction>"` instead, quoting the instruction as one argument.
    - `EXIT CODE 0:` The merge is requested. Continue to step 2.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
    - `EXIT CODE 5:` The merge was refused. If the refusal says the name is taken, restart step 1a with another `<name>`; otherwise go to step 4.
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
  - j. If `one-shot`: call `.claude/skills/merge-queue/run.sh --announce-one-shot`.
    - `EXIT CODE 0:` The one-shot merge is announced. Go to step 7.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
    - `EXIT CODE 5:` The announcement was refused. Go to step 4.
  - k. If `hold-rollouts` or `release-rollouts`: call `.claude/skills/merge-queue/run.sh --hold-rollouts` or `.claude/skills/merge-queue/run.sh --release-rollouts`.
    - `EXIT CODE 0:` The hold or release is applied. Go to step 5.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
    - `EXIT CODE 5:` Refused: the rollouts were already held, or were not held. Go to step 4.
    - `EXIT CODE 8:` No answer arrived in time. Go to step 6.

2. End the turn.
  - Say in one line that the merge is requested, then END THE TURN IMMEDIATELY.
  - CRITICAL: the merge cannot start while this turn runs, so do NOT wait, poll, or check on it.

3. Answer the merge's prompts (later turns).
  - *NOTE*: an `own async` merge sends its pick, conflict and test-failure prompts to its merge workspace, never to this session; this session receives only its report of how the merge ended, which needs no answer beyond step 3d.
  - a. A conflict-resolution prompt: resolve the conflicted files in the worktree the prompt names and `git add` them.
    - CRITICAL: NEVER run `git rebase --continue`, `--abort` or `--skip`; the queue continues the rebase itself.
  - b. A test-failure prompt: fix the failure in the worktree the prompt names and commit the fix with its tests.
  - c. A LANDED outcome for a `branch` or `commits` merge: call `.claude/skills/merge-queue/run.sh --remove-branch <branch-name>`.
    - `EXIT CODE 0:` The subagent's worktree and branch are gone. Report the landing and stop.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
  - d. A failed outcome (conflicts, tests or other), or an async merge's report of how it ended: report it in one line and STOP.

4. Refused.
  - Report the printed refusal verbatim and STOP.

5. Applied (`dequeue`, `pause`, `resume`, `hold-rollouts`, `release-rollouts`).
  - Report the printed outcome (`evicted`, `not_queued`, `paused`, `resumed`, `held`, `released`, or `unread`) and any other printed lines verbatim, then STOP.
  - *NOTE*: removing a merge that is not queued is applied with `not_queued`, not refused.

6. Unanswered (`dequeue`, `pause`, `resume`, `hold-rollouts`, `release-rollouts`).
  - Report the printed lines verbatim and STOP.
  - CRITICAL: NEVER re-issue the request; it stays pending.

7. Announced (`one-shot`).
  - Say in one line that the one-shot merge is announced, then CONTINUE the turn's work.
  - Commit every change, with its tests passing, before the turn ends.
    - *NOTE*: work left uncommitted when the turn ends fails the merge, and this workspace stays open.
  - End the turn normally once the work is done.
  - CRITICAL: NEVER also request `own` for this workspace; the announcement is its merge request.
  - Answer the merge's later prompts as in step 3.

## Notes

- **CRITICAL: NEVER merge, commit, cherry-pick, rebase, reset or push onto master by hand, nor remove a merge from the queue or pause or resume the queue by hand.** The queue is the only path into master, and this skill is the only way to control it.
- **CRITICAL: NEVER request an own-branch merge while anything the user asked for in this workspace is unfinished** (for a `pick`, anything the instruction names). A ruling partly implemented, with the rest relabelled as an open question, is unfinished; finish it, or stop and surface it.
- **CRITICAL: On a failed or refused merge, report and stop.** Never retry by hand, never work around the queue.
- **IMPORTANT NOTE: Merge SYNC by default; `own async` is only for genuinely tangential or explicitly detached work.** Never fold any branch into master yourself.
- **IMPORTANT NOTE: A subagent's branch goes through `branch`, and a fix without the unfinished work around it goes through `own async <name> pick` or `commits`.** Never cherry-pick onto master by hand.
- **IMPORTANT NOTE: A merge that must land before the work already waiting is asked for with `front`.** Never pause, dequeue or reorder other merges by hand to get ahead of them.
- **CRITICAL: NEVER request a workspace merge for a branch that is not checked out in that worktree.** Check the branch out there first, or use `branch <branch-name>` for a branch that is no workspace.
- **CRITICAL: Removing a merge from the queue, pausing or resuming the queue, or holding or releasing its rollouts, goes through this skill, NEVER by hand.**
- **IMPORTANT NOTE: While the rollouts are held, a landing does NOT redeploy.** Merges land on master exactly as before, but the running daemon, shims, webapp and Emacs keep the build they run; whoever holds it deploys by hand (`/manage-agent-repl-runtime deploy`) when they want what landed. The hold exists for perf work: a hand-deployed profiling build that the next landing would otherwise replace. It is durable (a restart or a handover keeps it) and daemon-wide, so release it when that work ends rather than leaving it standing; a landing that was held is recorded at INFO as `daemon.merge.self_reload` "...while the rollouts are held; the deploy is not told".
- **CRITICAL NOTE: Do not self-remediate a `run.sh` failure or read its internals.** React only to the documented exit codes.
