---
name: agent-repl-merge-queue
description: 'Land work through the agent-repl merge queue, the ONLY way into master in this repository, and control the queue. A merge is one of two kinds. Finishing THIS WORKSPACE (`workspace finished here`) deploys its own branch as its repository''s `.agent-repl/deploy.yaml` says (for agent-repl: landed where its landing target says, then the runtime rolled out), in place once the current turn ends, holding the feed, and closes this workspace once the deploy succeeds (a failure leaves it open and is reported back). An ASYNC MERGE (`workspace async-merge`) lands a NAMED BRANCH no workspace owns (a subagent''s, or one cut by hand) in a hidden merge workspace forked from a workspace''s conversation, while this workspace keeps working, optionally preparing the branch first from a free-text instruction, closing another workspace and removing the branch''s worktree once it lands, and reporting how it ended back here. Any merge may go to the FRONT of the queue (a hotfix), and a merge that must run before the one ALREADY RUNNING commandeers its spot. A deploy onto a versioned repository''s main branch (agent-repl''s master) names how it moves the version (minor or patch) and its changelog line. Also reads the queue''s status, takes a merge off it, moves a waiting merge up or down, pauses, hard-stops or resumes a queue, disables or enables racing, and subscribes this workspace to a queue''s changes. Use whenever work is finished and must reach master or a parent workspace, whenever a tangential fix or an attempt to test on master should land while work goes on, whenever you would otherwise run git merge, commit, cherry-pick or push onto master, when a subagent reports a finished branch, whenever a queued merge must be removed or moved, or the queue paused, hard-stopped, resumed or followed. Holding a repository''s deploys is /agent-repl-runtime''s.'
argument-hint: "here [front] [minor|patch <changelog-line>] | async <name> <branch> ... | status | dequeue | move-up | move-down | pause [-hard] | resume | disable-racing | enable-racing | subscribe | unsubscribe"
allowed-tools: Bash(~/.claude/skills/agent-repl-merge-queue/agent-repld:*)
---

## What This Skill Does

Every operation is one `agent-repld` command over the serving daemon's RPCs,
run through `~/.claude/skills/agent-repl-merge-queue/agent-repld`. The DAEMON
owns each command's flags and refusals: run `agent-repld workspace` or
`agent-repld merge-queue` with no verb for their usage. This skill says WHEN
and WHY. Nothing merges by hand. What a deploy does once it reaches the front
is its repository's deploy config, which `/agent-repl-workspaces` documents.

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
- A child whose owner lands it never finishes itself: its owner lands it (the
  daemon refuses `here` with `owner_lands`).

Before an async merge:

- The branch holds exactly the work to land, committed, with its tests passed;
  where it does not, `-instruction` says how the merge workspace makes it so.
- CRITICAL: the branch is NEVER the one checked out in this workspace, nor one
  any workspace owns. A workspace's branch lands only into that workspace.
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
- `-compact-after`, and the hook flags `-pre-deploy-prompt`,
  `-pre-deploy-script`, `-post-deploy-prompt` and `-post-deploy-script` (which
  replace the repository's hooks of that stage), ONLY when the user asked for
  them on this merge; never copy this workspace's own deploy hooks in.
- The version increment, by these rules, in order:
  1. The merge lands on the main branch of a repository whose deploy config
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
  the turn's work, and end the turn normally; the deploy starts when it ends.
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
- c. A report of how a merge ended: report it in one line, then go to step 3e
  for a round of an instrumentation loop; otherwise STOP. A report of a landing
  on agent-repl's master arrives once the build carrying it runs; a report that
  the repository's deploy stage is held or the rollout failed means the
  running build does NOT carry it.
- d. A notice that a subscribed queue changed: act only as far as the work the
  subscription was taken for calls for; otherwise report it in one line and
  STOP. NEVER poll the queue between notices. A notice that waited behind your
  turns lists every change since the last one you were told, oldest first, and
  its queue is the queue as it stands when it reaches you.
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
- `merge-queue disable-racing` and `merge-queue enable-racing`: on AC power the
  top two merges race at the tests; disabled, every queue works its head alone.
- A head whose WHOLE test suite finishes with any failure STEPS ASIDE: the
  next merge in line takes the head at once, and the failed merge fixes its
  failures out of line (`merge-queue status` lists it `remediating`; the
  sidebar draws it under the head). Once its fixing turn ends it goes back
  in line next after the merge being worked on, never evicting it, and
  reruns its whole suite when it heads the queue. It still has one fixing
  attempt: a second failing suite fails it out. A head with nobody behind it
  fixes in place.
- `merge-queue subscribe -detail every_change|deploys|advances|depletes
  [-repository here|DIR]` has this workspace told as the queue changes (to land
  after it drains, to report a landing) instead of polling it;
  `merge-queue unsubscribe` ends it. `deploys` tells each merge leaving the
  queue and each deploy of the daemon's own checkout ending (its build
  running, or failed), naming the reported landings it carries: the level
  for watching what a landing's rollout did.
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
