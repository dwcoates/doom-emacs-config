---
name: merge-queue
description: 'Land work on master through the agent-repl merge queue — the ONLY way into master in this repository. A merge is one of two modes. A ONE-SHOT concludes this workspace: its own branch merges SYNC, in place once the current turn ends, holding the feed, and this workspace closes once it lands (a failure leaves it open and is reported back to it). An ASYNC merge lands a NAMED BRANCH in a hidden merge workspace forked from this conversation while this workspace keeps working, optionally preparing the branch first from a free-text instruction (rebase onto master, drop unlanded commits, combine branches), closing another workspace (a child''s or a subagent''s) and removing the branch''s worktree once it lands, running only the before and after merge prompts the request names, and reporting how it ended back here. Any merge may be asked for at the FRONT of the queue (a hotfix). A merge onto agent-repl''s master names how it moves the version (minor or patch) and the changelog line its version tag carries. Also controls the queue itself, taking this workspace''s or another workspace''s merge off the queue, moving a waiting merge one place up or down the queue, pausing or resuming the queue for one repository or all, and holding or releasing the queue''s rollouts (merges still land, but a landing no longer redeploys the running runtime). Use whenever work is finished and must reach master, whenever a tangential fix or an attempt to test on master should land while work goes on, whenever you would otherwise run git merge, git commit, git cherry-pick or git push onto master, when a subagent reports a finished branch, whenever a queued merge must be removed, moved up or down, or the queue paused or resumed, whenever a hand-deployed build (a profiling build) must not be replaced by the next landing, or when invoked as /merge-queue.'
argument-hint: "one-shot [pr-merged] [front] [minor|patch <changelog-line>] | async <name> <branch> [instruction <text>] [close <worktree-dir>] [cleanup] [compact-after] [before-merge <prompt>] [after-merge <prompt>] [front] [minor|patch <changelog-line>] | dequeue own | dequeue workspace <worktree-dir> | move-up own | move-up workspace <worktree-dir> | move-down own | move-down workspace <worktree-dir> | pause [<repo-dir>] | resume [<repo-dir>] | hold-rollouts | release-rollouts"
allowed-tools: Bash(~/.claude/skills/merge-queue/run.sh:*)
---

## What This Skill Does

Asks the merge queue, from this workspace, to land work in its merge target: master for a workspace made off the main checkout, the PARENT workspace's checkout for a workspace created explicitly as a child of another. A `one-shot` merges this workspace's own branch SYNC once the turn ends, in this workspace, and closes it once it lands. An `async` merge lands a named branch in a hidden merge workspace forked from this conversation, while this workspace stays free; its progress streams into the merge bubble here, the merge workspace is revealed when it fails, asks a question or permission, is stopped from the bubble, or is selected, and how it ended is reported back here. Nothing merges by hand. It also removes a queued merge, moves a waiting merge one place up or down the queue, pauses or resumes the queue, and holds or releases the queue's rollouts, answering at once.

## Arguments

| Argument | Behaviour |
|---|---|
| `one-shot` | Announce NOW, without ending the turn, that this workspace's own branch merges SYNC, in place, once the current turn ends, holding its feed, and that this workspace CLOSES once it lands; it is marked a one-shot at once. A failed one-shot leaves this workspace open and is reported back to it. Refused in the repository's main worktree. |
| `one-shot pr-merged` | This workspace's branch already merged upstream: update master from upstream and close this workspace. |
| `async <name> <branch>` | Merge `<branch>` ASYNC, in a hidden merge workspace named `<name>` (a short slug naming what is merged) forked from this conversation onto `<branch>`'s tip; this workspace stays free and open, and is told how the merge ended (a landing on agent-repl's master once its deploy has run). `<branch>` is NEVER the branch checked out in this workspace. |
| `instruction <text>` | With `async`: free text the merge workspace carries out on `<branch>` before the merge joins the queue (the bubble's resolving tab), e.g. "rebase onto master, dropping every commit this workspace's branch shares with it", "combine agent-a and agent-b", "fail the merge if you cannot tell which commits are the fix". |
| `close <worktree-dir>` | With `async`: ANOTHER workspace (a child's, a subagent's) to close once the merge lands. Never this workspace. |
| `cleanup` | With `async`: once the merge lands, remove the worktree that has `<branch>` checked out (never this workspace's, the main worktree or any workspace's; branches are kept). |
| `compact-after` | With `async`: once the merge lands, compact this workspace's conversation. Used only when the user explicitly asks for it. |
| `before-merge <prompt>` | With `async`: a prompt the merge workspace runs before the landing; its failure fails the merge. A merge workspace runs ONLY the merge actions its request names, never this workspace's own configured ones. |
| `after-merge <prompt>` | With `async`: a prompt the merge workspace runs after the branch lands; it never fails the merge. A merge workspace runs ONLY the merge actions its request names, never this workspace's own configured ones. |
| `front` | With `one-shot` or `async`: the merge goes to the FRONT of its queue once it is put in line, ahead of every ordinary merge waiting and behind any earlier front merge, never ahead of the merge already running. |
| `minor <changelog-line>` | With `one-shot` or `async` onto agent-repl's master: the landing moves the version's MINOR number (a new capability or a changed behavior, breaking or not), and its version tag carries the one-line changelog entry. |
| `patch <changelog-line>` | With `one-shot` or `async` onto agent-repl's master: the landing moves the version's PATCH number (a fix or an internal change), and its version tag carries the one-line changelog entry. |
| `dequeue own` | Take this workspace's own merge off the queue. |
| `dequeue workspace <worktree-dir>` | Take ANOTHER workspace's merge off the queue, by its worktree. |
| `move-up own` | Move this workspace's own waiting merge one place up the queue, past the waiting merge directly ahead of it. |
| `move-up workspace <worktree-dir>` | Move ANOTHER workspace's waiting merge one place up the queue, by its worktree. |
| `move-down own` | Move this workspace's own waiting merge one place down the queue, past the waiting merge directly behind it. |
| `move-down workspace <worktree-dir>` | Move ANOTHER workspace's waiting merge one place down the queue, by its worktree. |
| `pause [<repo-dir>]` | Pause the queue of the repository whose main checkout is `<repo-dir>`, else of every repository. A paused queue advances nothing: merges still enqueue, but every merge running in it is suspended at once (it resumes at the step it recorded), and nothing is admitted, promoted or resumed until `resume`. |
| `resume [<repo-dir>]` | Resume a paused queue, scoped as `pause` is. |
| `hold-rollouts` | Hold the queue's ROLLOUTS, daemon-wide and durably: merges keep landing, but a landing in the daemon's own checkout no longer redeploys, hot-reloads or bounces the running runtime. |
| `release-rollouts` | End the hold; the next landing redeploys. Nothing that landed while held is deployed until then. |

## Steps

0. Choose the mode and confirm the work is ready (merge arguments only; skip to step 1 for `dequeue`, `move-up`, `move-down`, `pause`, `resume`, `hold-rollouts` and `release-rollouts`).
  - Choose the mode by these rules, in order.
    - i. The user named a mode: use it.
    - ii. This workspace's worktree is the repository's MAIN worktree: use `async`. NEVER `one-shot`.
    - iii. This workspace concludes once the merge lands (nothing more is to be done in it, since work after a one-shot is lost): use `one-shot`.
    - iv. Otherwise: use `async`. It is the mode for:
      - a tangential or piecemeal fix landed while this workspace keeps working on its main task;
      - an implementation attempt landed so this workspace and the user can test it on master;
      - the main workspace landing its subagents' branches, or combining several of them through an `instruction`, and deciding whether to have the branch rebased onto master so the main workspace's own unlanded commits stay out.
  - For `one-shot`: this workspace's requested changes are FULLY implemented.
    - Every ruling the user gave in this workspace is implemented.
    - No part of a ruling is deferred as an "open question".
    - An open question you raised that the user's rulings already answer is unfinished work.
    - Nothing the user asked for is left unfinished.
    - CRITICAL: if anything is unfinished, finish it, or STOP and surface it to the user. NEVER request the merge instead.
  - For `async`:
    - The branch to land holds exactly the work to land, committed, with the applicable tests passed; where it does not, the `instruction` says how the merge workspace makes it so.
    - CRITICAL: the branch is NEVER the one checked out in this workspace's worktree. Land a subagent's branch from the subagent's worktree, or cut a branch for the work to land.
    - Give `cleanup` unless the branch's worktree must be kept.
    - Give `close <worktree-dir>` when the work came from another workspace that is done once it lands (a child's, a subagent's).
  - Add `compact-after` ONLY when the user explicitly asked for it; NEVER add it on your own.
  - Add `before-merge <prompt>` or `after-merge <prompt>` ONLY when the user asked for a merge action on this merge; NEVER copy this workspace's own configured merge actions into them on your own.
  - Choose the version increment by these rules, in order.
    - i. The merge lands on agent-repl's master (this repository's default branch, from a workspace that is not a child of another workspace): give EXACTLY ONE of `minor` or `patch`, with a one-line release note for the changelog line.
      - `minor`: the work adds a capability or changes behavior, INCLUDING any breaking change.
      - `patch`: the work fixes a defect or changes nothing a user sees.
      - The changelog line is one line in the voice of a release note ("Merges record a version tag").
    - ii. Any other merge (another repository, or a child workspace merging into its parent): give NEITHER.
    - CRITICAL: a MAJOR increment is NEVER available. Only a human moves the major version, by tagging master by hand.
  - **Why this lives here**: the queue lands exactly what is committed, a one-shot closes this workspace and loses any work after it, and a red test gate comes back as a repair prompt in the session the merge runs in.

1. Dispatch on the argument.
  - *NOTE*: when `front` was given with a merge argument (steps 1a and 1b), append `--front` to that step's `run.sh` call; the exit codes are unchanged.
  - *NOTE*: when `minor <changelog-line>` or `patch <changelog-line>` was chosen in step 0 (steps 1a and 1b), append `--minor "<changelog-line>"` or `--patch "<changelog-line>"` to that step's `run.sh` call, quoting the line as one argument; the exit codes are unchanged.
  - a. If `one-shot`: call `~/.claude/skills/merge-queue/run.sh --one-shot`.
    - *NOTE*: with `one-shot pr-merged`, call `~/.claude/skills/merge-queue/run.sh --one-shot --pr-merged` instead.
    - `EXIT CODE 0:` The one-shot merge is announced. Go to step 7.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
    - `EXIT CODE 5:` The announcement was refused. Go to step 4.
  - b. If `async <name> <branch>`: call `~/.claude/skills/merge-queue/run.sh --async <name> <branch>`.
    - *NOTE*: with `instruction <text>`, append `--instruction "<text>"`, quoting the text as one argument.
    - *NOTE*: with `close <worktree-dir>`, append `--close <worktree-dir>`.
    - *NOTE*: with `cleanup`, append `--cleanup`.
    - *NOTE*: with `compact-after`, append `--compact-after`.
    - *NOTE*: with `before-merge <prompt>`, append `--before-merge "<prompt>"`, quoting the prompt as one argument.
    - *NOTE*: with `after-merge <prompt>`, append `--after-merge "<prompt>"`, quoting the prompt as one argument.
    - *NOTE*: the merge workspace runs ONLY the merge actions named here; with neither, it runs none.
    - `EXIT CODE 0:` The merge is requested. Continue to step 2.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
    - `EXIT CODE 5:` The merge was refused. If the refusal says the name is taken, restart step 1b with another `<name>`; otherwise go to step 4.
  - c. If `dequeue own`: call `~/.claude/skills/merge-queue/run.sh --dequeue-own`.
    - `EXIT CODE 0:` The removal is applied. Go to step 5.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
    - `EXIT CODE 5:` The removal was refused. Go to step 4.
    - `EXIT CODE 8:` No answer arrived in time. Go to step 6.
  - d. If `dequeue workspace <worktree-dir>`: call `~/.claude/skills/merge-queue/run.sh --dequeue-workspace <worktree-dir>`.
    - `EXIT CODE 0:` The removal is applied. Go to step 5.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
    - `EXIT CODE 5:` The removal was refused. Go to step 4.
    - `EXIT CODE 8:` No answer arrived in time. Go to step 6.
  - e. If `pause [<repo-dir>]`: call `~/.claude/skills/merge-queue/run.sh --pause-queue [<repo-dir>]`.
    - `EXIT CODE 0:` The pause is applied. Go to step 5.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
    - `EXIT CODE 5:` The pause was refused. Go to step 4.
    - `EXIT CODE 8:` No answer arrived in time. Go to step 6.
  - f. If `resume [<repo-dir>]`: call `~/.claude/skills/merge-queue/run.sh --resume-queue [<repo-dir>]`.
    - `EXIT CODE 0:` The resume is applied. Go to step 5.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
    - `EXIT CODE 5:` The resume was refused. Go to step 4.
    - `EXIT CODE 8:` No answer arrived in time. Go to step 6.
  - g. If `hold-rollouts` or `release-rollouts`: call `~/.claude/skills/merge-queue/run.sh --hold-rollouts` or `~/.claude/skills/merge-queue/run.sh --release-rollouts`.
    - `EXIT CODE 0:` The hold or release is applied. Go to step 5.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
    - `EXIT CODE 5:` Refused: the rollouts were already held, or were not held. Go to step 4.
  - h. If `move-up own` or `move-down own`: call `~/.claude/skills/merge-queue/run.sh --move-up-own` or `~/.claude/skills/merge-queue/run.sh --move-down-own` respectively.
    - `EXIT CODE 0:` The move is applied. Go to step 5.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
    - `EXIT CODE 5:` The move was refused. Go to step 4.
    - `EXIT CODE 8:` No answer arrived in time. Go to step 6.
  - i. If `move-up workspace <worktree-dir>` or `move-down workspace <worktree-dir>`: call `~/.claude/skills/merge-queue/run.sh --move-up-workspace <worktree-dir>` or `~/.claude/skills/merge-queue/run.sh --move-down-workspace <worktree-dir>` respectively.
    - `EXIT CODE 0:` The move is applied. Go to step 5.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
    - `EXIT CODE 5:` The move was refused. Go to step 4.
    - `EXIT CODE 8:` No answer arrived in time. Go to step 6.

2. End the turn (`async`).
  - Say in one line that the merge is requested, then END THE TURN IMMEDIATELY.
  - CRITICAL: the merge cannot start while this turn runs, so do NOT wait, poll, or check on it.

3. Answer the merge's prompts (later turns).
  - *NOTE*: an `async` merge sends its resolving, conflict and test-failure prompts to its merge workspace, never to this session; this session receives only the report of how the merge ended, which needs no answer beyond step 3c.
  - a. A conflict-resolution prompt (a `one-shot`): resolve the conflicted files in the worktree the prompt names and `git add` them.
    - CRITICAL: NEVER run `git rebase --continue`, `--abort` or `--skip`; the queue continues the rebase itself.
  - b. A test-failure prompt (a `one-shot`): fix the failure in the worktree the prompt names and commit the fix with its tests.
  - c. A report of how a merge ended (landed, failed, or did not run): report it in one line and STOP.
    - *NOTE*: a report of a landing on agent-repl's master arrives only once the build that carries it is running, this session's own included, and says so; NEVER wait for, poll, or check the restart before acting on it.
    - *NOTE*: a report saying the rollouts are held or the deploy failed means the running build does NOT carry the landing.

4. Refused.
  - Report the printed refusal verbatim and STOP.

5. Applied (`dequeue`, `move-up`, `move-down`, `pause`, `resume`, `hold-rollouts`, `release-rollouts`).
  - Report the printed outcome (`evicted`, `moved_up`, `moved_down`, `unmoved`, `not_queued`, `paused`, `resumed`, `held`, `released`, or `unread`) and any other printed lines verbatim, then STOP.
  - *NOTE*: removing or moving a merge that is not queued is applied with `not_queued`, not refused.
  - *NOTE*: a move with no waiting merge to pass on that side is applied with `unmoved`.

6. Unanswered (`dequeue`, `move-up`, `move-down`, `pause`, `resume`, `hold-rollouts`, `release-rollouts`).
  - Report the printed lines verbatim and STOP.
  - CRITICAL: NEVER re-issue the request; it stays pending.

7. Announced (`one-shot`).
  - Say in one line that the one-shot merge is announced, then CONTINUE the turn's work.
  - Commit every change, with its tests passing, before the turn ends.
    - *NOTE*: work left uncommitted when the turn ends fails the merge, and this workspace stays open.
  - End the turn normally once the work is done.
  - CRITICAL: NEVER request the one-shot a second time; the announcement is its merge request.
  - Answer the merge's later prompts as in step 3.

## Notes

- **CRITICAL NOTE: Always call the INSTALLED script, `~/.claude/skills/merge-queue/run.sh`, never a worktree's own `.claude/skills/merge-queue/run.sh`.** A worktree's copy is as old as its branch's base, and refuses forms master's copy accepts.
- **CRITICAL: NEVER merge, commit, cherry-pick, rebase, reset or push onto master by hand, nor remove, move, pause, resume or hold anything in the queue by hand.** The queue is the only path into master, and this skill is the only way to control it.
- **CRITICAL: A `one-shot` closes this workspace once it lands; use it ONLY when this workspace concludes there, NEVER from the main workspace, and NEVER while anything the user asked for in this workspace is unfinished.** Every other merge is `async`, and `async` is NEVER given the branch checked out in this workspace's worktree.
- **CRITICAL: On a failed or refused merge, or a `run.sh` failure, report and stop.** Never retry by hand, never work around the queue, never read `run.sh`'s internals; react only to the documented exit codes, and ask for a merge that must land before the work already waiting with `front`.
- **CRITICAL: Every merge onto agent-repl's master carries exactly one of `minor` or `patch` with its changelog line, and no other merge carries either.** A major increment is never requested; a merge that breaks this rule is refused.
- **IMPORTANT NOTE: While the rollouts are held, a landing does NOT redeploy.** Whoever holds them deploys by hand (`/manage-agent-repl-runtime deploy`) when they want what landed, and releases the hold when that work ends.
