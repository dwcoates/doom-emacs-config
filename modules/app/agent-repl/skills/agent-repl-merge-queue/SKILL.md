---
name: agent-repl-merge-queue
description: 'Land work through the agent-repl merge queue, the ONLY way into master in this repository, and control the queue. A merge is one of two kinds. Finishing THIS WORKSPACE (`workspace finished here`) deploys its own branch as its repository''s `.agent-repl/lifecycle.yaml` says (for agent-repl: landed where its landing target says, then the runtime rolled out), in place once the current turn ends, holding the feed, and closes this workspace once the deploy succeeds (a failure leaves it open and is reported back). An ASYNC MERGE (`workspace async-merge`) lands a NAMED BRANCH (a subagent''s, one cut by hand, or any workspace''s, whoever owns it) in a hidden merge workspace forked from a workspace''s conversation, while this workspace keeps working, optionally preparing the branch first from a free-text instruction, closing another workspace and removing the branch''s worktree once it lands, and reporting how it ended back here. Any merge may go to the FRONT of the queue (a hotfix), and a merge that must run before the one ALREADY RUNNING commandeers its spot. A deploy onto a versioned repository''s main branch (agent-repl''s master) names how it moves the version (minor or patch) and its changelog line. Also reads the queue''s status, takes a merge off it, moves a waiting merge up or down, pauses, hard-stops or resumes a queue, and subscribes this workspace to a queue''s changes. Use whenever work is finished and must reach master or a parent workspace, whenever a tangential fix or an attempt to test on master should land while work goes on, whenever you would otherwise run git merge, commit, cherry-pick or push onto master, when a subagent reports a finished branch, whenever a queued merge must be removed or moved, or the queue paused, hard-stopped, resumed or followed. Holding a repository''s deploys is /agent-repl-runtime''s.'
argument-hint: "here [front] [minor|patch <changelog-line>] | async <name> <branch> ... | status | dequeue | move-up | move-down | pause [-hard] | resume | subscribe | unsubscribe"
allowed-tools: Bash(~/.claude/skills/agent-repl-merge-queue/agent-repld:*)
---

## What This Skill Does

Every operation is one `agent-repld` command over the serving daemon's RPCs,
run through `~/.claude/skills/agent-repl-merge-queue/agent-repld`. The DAEMON
owns each command's flags and refusals: run `agent-repld workspace` or
`agent-repld merge-queue` with no verb for their usage. This skill says WHEN
and WHY. Nothing merges by hand. What a deploy does once it reaches the front
is its repository's lifecycle config, which `/agent-repl-workspaces` documents.

## 0. Choose the kind of merge

Choose by these rules, in order.

1. The user named one: use it.
2. This workspace's worktree is the repository's MAIN worktree: NEVER merge
   `here`. Land work from it as an async merge of a branch cut for it.
3. This workspace concludes once the merge lands (nothing more is to be done in
   it; work after the landing is lost): finish `here`.
4. Otherwise: an ASYNC MERGE. It is the kind for:
   - a tangential or piecemeal fix landed while this workspace keeps working;
   - an implementation attempt landed so this workspace and the user can test
     it on master;
   - a round of an instrumentation loop (step 3e);
   - landing a subagent's finished branch, or combining several through an
     `-instruction`.

Before finishing `here`:

- Every ruling the user gave in this workspace is implemented, and nothing the
  user asked for is unfinished. CRITICAL: if anything is, finish it or STOP and
  surface it. NEVER finish the workspace instead.
- Every change is committed with its tests passing; work left uncommitted when
  the turn ends fails the merge.

Before an async merge:

- The branch holds exactly the work to land, committed, with its tests passed;
  where it does not, `-instruction` says how the merge workspace makes it so.
- Any existing branch may be queued, whichever workspace owns it, open or
  closed; who owns it is never a refusal. To land THIS workspace's own work,
  finish the workspace (`workspace finished here`) unless it must land while
  this workspace keeps working.
- Give `-cleanup-worktree` unless the branch's worktree must be kept, and
  `-close-after WS` when the work came from another workspace that is done
  once it lands.
- A subagent's branch names the subagent: `-maker-subagent ID` with
  `-context-fork` naming the workspace that spawned it; otherwise
  `-maker WS`. `-target primary` lands on the main branch, `-target maker`
  into the maker's branch.

Either kind:

- `-placement front` only for a merge that must land before the work already
  waiting (a hotfix); `-placement behind=MERGE` to land directly after a named
  merge in line (the name `merge-queue status` lists), taking its front mark;
  otherwise `-placement back`. A name not in line is refused
  (`placement_behind_not_in_line`).
  - A merge placed `behind=MERGE` waits on MERGE: it is not admitted, and its
    `-instruction` does not run, until MERGE has left the queue. So an
    instruction may rely on MERGE's work being on the target, and the merge
    workspace is told to rebase onto it first.
  - If MERGE leaves without landing, the merge behind it fails too, naming
    MERGE, and its requester is told.
- `-compact-after`, and the hook flags `-pre-deploy-prompt`,
  `-pre-deploy-script`, `-post-deploy-prompt` and `-post-deploy-script` (which
  replace the repository's hooks of that stage), ONLY when the user asked for
  them on this merge; never copy this workspace's own deploy hooks in.
- The version increment, by these rules, in order:
  1. The merge lands on the main branch of a repository whose lifecycle config
     sets `version_tag` (agent-repl's master): give `-version-part minor` (a
     new capability or a changed behavior, breaking or not) or
     `-version-part patch` (a fix, or nothing a user sees), with
     `-changelog-line` in the voice of a release note.
  2. Any other merge (an unversioned repository, or into a parent workspace):
     neither.
  - CRITICAL: a MAJOR increment is never available; only a human tags it.

## 1. Request it

- Finish this workspace: `workspace finished here -placement back|front
  [-version-part ... -changelog-line ...]`.
- Async: `workspace async-merge -name NAME -branch BRANCH ...`, NAME a short
  slug naming what lands. A name already taken is refused; ask again with
  another.
- Refused: report the printed arm verbatim and STOP.

## 2. End the turn

- After finishing `here`: say in one line that the deploy is requested, finish
  the turn's work, and end the turn normally. When it ends, the repository's
  finish-stage hooks run first, before the deploy is put in line
  (`/agent-repl-workspaces`); the deploy starts after them.
  NEVER request it a second time.
- After an async merge: say so in one line and END THE TURN. The merge cannot
  start while this turn runs; do NOT wait, poll or check on it.

## 3. Answer the merge's prompts (later turns)

- a. A conflict-resolution prompt: resolve the conflicted files in the worktree
  the prompt names and `git add` them. CRITICAL: NEVER run
  `git rebase --continue`, `--abort` or `--skip`; the queue continues the
  rebase itself.
- b. A test-failure prompt: remediate as a TARGETED loop over exactly the
  failing cases it hands over: run them with the commands it gives, fix, rerun,
  until every one passes; then commit the fix with its tests and end the turn.
  CRITICAL: NEVER run a whole suite or `bin/test-all.sh` while remediating, and
  NEVER end the turn while a handed-over case still fails.
- c. A report of how a merge ended (landed, failed or evicted): report it in
  one line, then go to step 3e for a round of an instrumentation loop;
  otherwise STOP. An evicted merge's merge workspace is closed and its branch
  kept: queue it again with `agent-repld workspace async-merge -branch <the
  merge workspace's branch>`. A report of a landing
  on agent-repl's master arrives once the build carrying it runs; a report that
  the repository's deploy stage is held or the rollout failed means the
  running build does NOT carry it.
- d. A notice that a subscribed queue changed: act only as far as the work the
  subscription was taken for calls for; otherwise report it in one line and
  STOP. NEVER poll the queue between notices. A notice that waited behind your
  turns lists every change since the last one you were told, oldest first, and
  its queue is the queue as it stands when it reaches you. A notice saying the
  subscription ended is the last; subscribe again only if the work you
  subscribed for still needs it.
- e. A landing report for an instrumentation round: reproduce, read the records
  the round added, and either remediate (landing the fix as the next round) or
  add the records still missing and land them as the next async merge. While a
  round's merge runs, work the next open question; never wait on it.
- f. A report that a deploy HOOK failed (a script that exited non-zero, ran
  past its timeout or could not start, or a prompt hook that did not
  complete): investigate it from the hook's tab and its log, fix the cause,
  and finish again. A failure after the local merge landed keeps the landing.

## 4. Controlling the queue

- `merge-queue status [-repository here|DIR]` reads every queue, or one.
- `merge-queue dequeue MERGE`, `merge-queue move-up MERGE` and
  `merge-queue move-down MERGE` take a merge off the queue or move it one
  place. MERGE is the name `merge-queue status` lists it under (an async
  merge's `-name`), or `here` for this workspace's own merge.
- `merge-queue pause` and `merge-queue resume [-repository here|DIR]` suspend
  or resume a queue.
- `merge-queue pause -hard [-repository here|DIR]` is a HARD STOP: it pauses
  the queue and also interrupts the agent turn of every merge stopped at a
  fixing attempt or a conflict resolution, and of every waiting merge's
  background rebase. A turn at any other step (a pick, a deploy-stage prompt)
  is left to finish. A queue already paused is not refused. On resume, each
  interrupted step is prompted again without spending an attempt.
  - CRITICAL: hard-stop ONLY when the user asks to stop the queue's work now.
- Each queue holds at most three active merges, one per slot: the HEAD (the
  one merge running the test gate and landing), FIXING TESTS (one merge fixing
  its failing tests out of line) and RESOLVING CONFLICTS (one merge resolving
  its rebase's conflicts out of line).
- A merge whose next step waits on an agent turn of its own STEPS ASIDE, for
  one of two reasons: its WHOLE test suite finished with any failure (its
  session fixes them), or its rebase onto the target's tip hit conflicts (its
  session resolves them).
  - The head steps aside for either reason; no other member steps aside.
  - It takes the repair's slot, or waits for it out of line, in order, when
    another merge holds it; the next merge in line takes the head at once.
  - Its repairing brief is delivered once it holds the slot, not before
    stepping aside; nothing is handed its session until the holder's return
    grants it the slot.
  - `merge-queue status` lists it at stage `fixing_tests`,
    `resolving_conflicts` or `awaiting_repair` (with the slot awaited and its
    1-based place). The sidebar draws the holders "fixing tests" and
    "resolving conflicts", and the waiters "waiting to fix tests" and
    "waiting to resolve conflicts", under the head.
  - A waiting merge's background rebase runs git only: a conflict aborts the
    rebase and steps the merge aside for the resolving-conflicts slot
    (holding it or waiting for it); the slot's holder then runs the resolving
    rebase in its session and returns to the line.
  - Once its turn ends it goes back in line at the first waiting place, next
    after the merge being worked on, never evicting it. It then rebases onto
    the tip and reruns its whole suite when it heads the queue.
  - A test step aside spends one of its fixing attempts: a failing suite
    after its last one fails it out. A conflict step aside spends no fixing
    attempt, and if its rebase on return conflicts again it steps aside again.
  - A merge with nobody in line behind it repairs in place, holding the head.
  - It survives a daemon restart.
- `merge-queue subscribe [-repository here|DIR]` has this workspace told as
  the queue changes (to land after it drains, to report a landing) instead of
  polling it. It is told, in one short line each, a merge put in line, a
  merge leaving the queue (landed, failed or taken off), and each deploy of
  the daemon's own checkout ending (its build running, or failed), naming the
  landings it carries; never a merge's start or steps. It ends on
  `merge-queue unsubscribe`, on the workspace's close, and on its own when a
  queue in scope depletes (a merge leaves and none is left in line), that
  notice being its last; subscribe again to follow the next work, and to hear
  a deploy that ends after the depletion.
- Holding or releasing a repository's deploys is `/agent-repl-runtime`'s.

### Commandeering the running spot

The user wants a merge to run BEFORE the one already running, not merely be
next (owner ruling 2026-10-23). CRITICAL: do this only when the user asked for
it; it discards the running merge's test progress.

1. Put the commandeering merge at the front: a new request uses
   `-placement front`; a waiting one is moved with `merge-queue move-up MERGE`
   until it sits directly behind the running merge (`move-up` never passes the
   running merge).
2. `merge-queue status` and note the running merge's name and kind. Before
   dequeuing it, record what re-requesting it needs: a workspace's own merge
   (`kind: ownBranch`) needs its `-version-part` and `-changelog-line` from the
   request in that workspace's conversation; an async merge needs its full
   async-merge request.
3. `merge-queue dequeue RUNNING`. The commandeering merge starts.
4. Once `merge-queue status` shows the commandeering merge running, put the
   displaced merge back at the FRONT so it runs right after:
   - a workspace's own merge is re-requested only by that workspace (the
     daemon refuses another workspace's `workspace finished WS` with
     `requesterMayNotLand`): `workspace prompt WS TEXT` that workspace, saying
     it was displaced by an owner instruction, nothing wrong with its branch,
     and giving the exact command: `workspace finished here -placement front`
     with its original `-version-part` and `-changelog-line`.
   - an async merge is re-requested by its requester with `-placement front`
     and the same arguments as before.
5. Report which merge was displaced and that it was re-queued (or that its
   workspace was prompted to).

- **CRITICAL: between the dequeue (step 3) and the re-request (step 4), a
  different front placement can slip ahead of the displaced merge.** Nothing
  closes this gap short of a daemon verb that does all of it atomically.

## Notes

- **CRITICAL: Always call the INSTALLED front door,
  `~/.claude/skills/agent-repl-merge-queue/agent-repld`, never a worktree's own
  copy.**
- **CRITICAL: NEVER merge, commit, cherry-pick, rebase, reset or push onto
  master by hand, nor change the queue by hand, nor poll it in place of a
  subscription.** The queue is the only path into master.
- **CRITICAL: On a failed or refused merge, report and stop.** Never retry by
  hand or work around the queue.
  - The one refusal answered in place is `no_layout_facts`: the workspace
    records no merge geometry. Restate its landing target with
    `workspace set WS -lands-into main` (or `-lands-into W`), as the refusal
    says, then finish it again. That records the geometry explicitly, and
    nothing is ever inferred (`/agent-repl-workspaces`, "NO MERGE GEOMETRY").
