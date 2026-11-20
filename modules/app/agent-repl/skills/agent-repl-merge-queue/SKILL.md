---
name: agent-repl-merge-queue
description: 'Land work through the agent-repl merge queue, the ONLY way into master in this repository, and control the queue. A merge is one of two kinds. Finishing THIS WORKSPACE (`workspace finished here`) deploys its own branch as its repository''s `.agent-repl/lifecycle.yaml` says (for agent-repl: landed where its landing target says, then the runtime rolled out), in place once the current turn ends, holding the feed, and closes this workspace once the deploy succeeds (a failure leaves it open and is reported back). An ASYNC MERGE (`workspace async-merge`) lands a NAMED BRANCH (a subagent''s, one cut by hand, or any workspace''s, whoever owns it) in a hidden merge workspace forked from a workspace''s conversation, while this workspace keeps working, optionally preparing the branch first from a free-text instruction, closing another workspace and removing the branch''s worktree once it lands, and reporting how it ended back here. Any merge may go to the FRONT of the queue (a hotfix), and a merge that must run before the one ALREADY RUNNING commandeers its spot. A deploy onto a versioned repository''s main branch (agent-repl''s master) names how it moves the version (minor or patch) and its changelog line. Also reads the queue''s status, takes a merge off it, puts a merge that left the queue without landing back in line as itself (requeue), discards one, moves a waiting merge up or down, pauses, hard-stops or resumes a queue, and subscribes this workspace to a queue''s depletion, pause and resume (following one workspace''s merge is `workspace subscribe`, `/agent-repl-workspaces`''). Use whenever work is finished and must reach master or a parent workspace, whenever a tangential fix or an attempt to test on master should land while work goes on, whenever you would otherwise run git merge, commit, cherry-pick or push onto master, when a subagent reports a finished branch, whenever a queued merge must be removed or moved, a merge that failed or was evicted must go back in line or be discarded, or the queue paused, hard-stopped, resumed or followed. Holding a repository''s deploys is /agent-repl-runtime''s.'
argument-hint: "here [front] [minor|patch <changelog-line>] | async <name> <branch> ... | status | dequeue | requeue | discard | move-up | move-down | pause [-hard] [-to-land] | resume | subscribe | unsubscribe"
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
  otherwise `-placement back`. MERGE may also be an async merge this
  workspace asked for and has not handed off yet, such as one asked for
  earlier in the same turn. A turn may ask for any number of async merges;
  they join the line in the order asked for, so the later one lands directly
  behind it. Any other name not in line is still refused
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
  the prompt names, so the result builds, and `git add` them. CRITICAL: NEVER
  run `git rebase --continue`, `--abort` or `--skip`; the queue continues the
  rebase itself.
- b. A test-failure prompt: remediate as a TARGETED loop over exactly the
  failing cases it hands over: run them with the commands it gives, fix, rerun,
  until every one passes; then commit the fix with its tests and end the turn.
  CRITICAL: NEVER run a whole suite or `bin/test-all.sh` while remediating, and
  NEVER end the turn while a handed-over case still fails. A turn that commits
  nothing fails the merge without another run.
- When a conflict or a failure cannot be repaired correctly, give the merge up
  from that turn: `merge-queue give-up -reason TEXT`, then end the turn saying
  why. The merge leaves the queue at once and no test run starts. CRITICAL:
  NEVER leave a repair broken, unresolved or uncommitted on purpose to make the
  merge fail; that spends a whole test run on a merge that cannot land.
- c. A report of how a merge ended (landed, failed, or given up under its
  workspace closing or the daemon shutting down; a merge the user or the
  operator evicted or dequeued is never reported back): report it in one line,
  then go to step 3e for a round of an instrumentation loop;
  otherwise STOP. A merge that left without landing stays off the queue with
  its request: requeue it with `merge-queue requeue NAME` (NEVER re-request it,
  NEVER make a new workspace), or end it with `merge-queue discard NAME`. A
  report of a landing
  on agent-repl's master arrives once the build carrying it runs; a report that
  the repository's deploy stage is held or the rollout failed means the
  running build does NOT carry it.
- d. A notice that a subscribed queue depleted, was paused or was resumed: act
  only as far as the work the subscription was taken for calls for; otherwise
  report it in one line and STOP. A paused notice means the queue is held; the
  resumed notice that follows says it runs again. NEVER poll the queue between
  notices. A notice that waited behind your turns lists every change since the
  last one you were told, oldest first, and its queue is the queue as it stands
  when it reaches you. A notice saying the subscription ended is the last;
  subscribe again only if the work you subscribed for still needs it.
- e. A landing report for an instrumentation round: reproduce, read the records
  the round added, and either remediate (landing the fix as the next round) or
  add the records still missing and land them as the next async merge. While a
  round's merge runs, work the next open question; never wait on it.
- f. A report that a deploy HOOK failed (a script that exited non-zero, ran
  past its timeout or could not start, or a prompt hook that did not
  complete): investigate it from the hook's tab and its log, fix the cause,
  and finish again. A failure after the local merge landed keeps the landing.

## 4. Controlling the queue

- `merge-queue status [-repository here|DIR]` reads every queue, or one. Each
  queue's `offQueue` lists the merges that left it without landing, with name,
  branch, kind and cause.
- `merge-queue dequeue MERGE`, `merge-queue move-up MERGE` and
  `merge-queue move-down MERGE` take a merge off the queue or move it one
  place. MERGE is the name `merge-queue status` lists it under (an async
  merge's `-name`), or `here` for this workspace's own merge.
- A merge's request lives until it lands or is discarded. Any exit short of a
  landing (dequeue, failure, a restart that could not resume it, hidden at its
  cold gate, the merge ahead not landing) keeps it OFF THE QUEUE with its
  cause, keeping its branch, version part, changelog line, hooks and cleanup
  flags; an async merge's hidden merge workspace stays open under its name.
- `merge-queue requeue MERGE|here [-placement back|front|behind=MERGE]
  [-version-part minor|patch -changelog-line TEXT] [-model M]` puts the SAME
  request back in line as itself (same workspace or merge workspace, same
  branch and values). Placement defaults to back.
  - `-version-part` and `-changelog-line` are ONLY for a merge whose cause is
    `adopted` (a merge workspace left open before requests were kept, which
    recorded no values); a versioned main-branch landing of an adopted merge
    asked by an agent is refused `version_increment_required` without them,
    and giving them for any other merge is refused `version_already_recorded`.
  - `-model M` makes the merge's session resume on model M in the same step,
    so a cold conversation is read once on it, with no separate SetModel; M
    not in the login's catalog is refused `not_in_catalog`.
  - A merge whose workspace is parked at its cold gate is requeued too: it
    goes in line skipped, and the daemon answers the gate at once by a
    compaction of the whole conversation with Haiku, keeping the prompts held
    at the gate. The merge resumes at its place once the session is up.
  - Refusals: `unknown_merge`, `still_queued`, `branch_gone`,
    `already_landed`, `workspace_closed`, `placement_behind_not_in_line`,
    `not_in_catalog`.
- `merge-queue discard MERGE|here` ends a request off the queue. An async
  merge's merge workspace is closed and the worktree its request asked to
  clean up is removed when clean; a workspace's own merge loses only its
  request. No branch is ever deleted. A merge still in line is refused
  `still_queued` (dequeue first).
- `workspace async-merge` under a name an off-queue merge holds is refused
  `async_merge_name_off_queue`, naming that merge: requeue it instead.
- `merge-queue pause -repository here|DIR|all` and `merge-queue resume
  -repository here|DIR|all` suspend or resume a queue. `-repository` is
  required; `all` names every repository's queue.
- `merge-queue pause -hard -repository here|DIR|all` is a HARD STOP: it pauses
  the queue and also interrupts the agent turn of every merge stopped at a
  fixing attempt or a conflict resolution, and of every waiting merge's
  background rebase. A turn at any other step (a pick, a deploy-stage prompt)
  is left to finish. A queue already paused is not refused. On resume, each
  interrupted step is prompted again without spending an attempt.
  - CRITICAL: hard-stop ONLY when the user asks to stop the queue's work now.
- `merge-queue pause -to-land -repository here|DIR|all` marks a pause made to land a change: the only
  pause the queue bypass holder's turn end lets through. A pause without it,
  such as one the user asked for, stands until it is resumed explicitly.
- A queue the HOST paused (`by the host` in its hover) stands for a merge
  test gate whose runner could not be built or started: nothing of the branch
  was judged and no fixing attempt was spent. Resume it once the cause named
  is fixed; the merge's gate then runs again from its start.
- Each queue has two exclusive slots, each held by at most one merge: the
  HEAD (the one merge running the test gate and landing) and FIXING TESTS (one
  merge fixing its failing tests out of line).
  - RESOLVING CONFLICTS takes no slot: every merge whose rebase conflicted
    resolves its conflicts out of line at once, and none waits for another.
- A merge whose next step waits on an agent turn of its own STEPS ASIDE, for
  one of two reasons: its WHOLE test suite finished with any failure (its
  session fixes them), or its rebase onto the target's tip hit conflicts (its
  session resolves them).
  - The head steps aside for either reason; no other member steps aside.
  - A merge fixing tests takes the fixing-tests slot, or waits for it out of
    line, in order, when another merge holds it; a merge resolving conflicts
    starts at once. The next merge in line takes the head at once.
  - A merge waiting for the fixing-tests slot has its brief delivered once it
    holds the slot, not before; nothing is handed its session until the
    holder's return grants it the slot.
  - `merge-queue status` lists it at stage `fixing_tests`,
    `resolving_conflicts` or `awaiting_repair` (the fixing-tests slot awaited,
    and its 1-based place). The sidebar draws "fixing tests", each
    "resolving conflicts", and each "waiting to fix tests", under the head.
  - A waiting merge's background rebase runs git only: a conflict aborts the
    rebase and steps the merge aside to resolve; it runs the resolving
    rebase in its session at once and returns to the line.
  - Once its turn ends it goes back in line at the first waiting place, next
    after the merge being worked on, never evicting it. It then rebases onto
    the tip and reruns its whole suite when it heads the queue.
  - A test step aside spends one of its fixing attempts: a failing suite
    after its last one fails it out. A conflict step aside spends no fixing
    attempt, and if its rebase on return conflicts again it steps aside again.
  - A merge with nobody in line behind it repairs in place, holding the head.
  - It survives a daemon restart.
- `merge-queue subscribe [-repository here|DIR]` has this workspace told, in
  one short line naming the queue, when the queue AS A WHOLE depletes, is
  paused, or is resumed, instead of polling it. It is never told what one
  merge does (put in line, started, stepped, landed, failed, taken off,
  deployed). It ends on `merge-queue unsubscribe`, on the workspace's close,
  and on its own when a queue in scope depletes, that notice being its last
  (subscribe again to follow the next work).
- To watch ONE workspace's merge land, deploy or be evicted, subscribe to that
  workspace, not the queue: `workspace subscribe WS` (`/agent-repl-workspaces`)
  holds `-events lifecycle` by default, which tells landed, deployed (the
  running build carries the landing; a failed deploy is told as such, and
  means the running build does NOT carry it), evicted (with the reason),
  closed (as closed, killed or nuked) and cold_gated. WS need not have a merge
  in line yet, and each workspace watched takes its own subscription, which
  ends on its terminal notice or on `workspace unsubscribe WS`. Waiting for
  'after X lands' is this, never a queue subscription.
- A repository whose `.agent-repl/lifecycle.yaml` sets `deploy_checks` holds
  its queue's head after every deploy until the deploy is checked.
  - Deploys in a row form a chain. The repository keeps ONE standing deploy
    mediator across every deploy, on Opus at low effort: it checks each
    deploy, fixes what it dispatches to itself in the same turn (finishing on
    its own), and settles an escalation with the user (finishing once they
    agree). It stays open, warm and in sight in its repository's section for
    good, and the queue's purple label opens it too; its
    conversation is compacted by Haiku the moment three quiet checks follow a
    dispatch or an escalation, never cleared.
  - The sidebar labels the repository in purple: "HOT-LOADING ZETTEL" while
    a build rolls out (a landing's or a landed fix's), "ASSESSING ZETTEL
    HEALTH" while the mediator checks it, "FIXING ZETTEL" while the
    mediator fixes it or what its gate failed on, "TESTING
    ZETTEL FIXES" while a finished fix runs its gate and lands. A click
    selects the workspace at work.
  - `merge-queue status` answers its advance as `mediating`, with its stage
    (`rolling_out`, `assessing` or `fixing`, naming the mediator) and
    `remediations_in_a_row`; `paused` wins while the queue
    is also paused.
  - Only the mediator decides, from the turn its check's brief started, with
    `merge-queue mediation approve|escalate -summary TEXT`, or
    `merge-queue mediation dispatch -summary TEXT` reading the
    problems from standard input as a JSON list of `{"statement", "evidence"}`.
  - The mediator also keeps the kinds of log record the checks leave out,
    with `merge-queue mediation ignore -operation OP [-message TEXT] -why TEXT`
    and `merge-queue mediation unignore -operation OP [-message TEXT]`; a
    check whose window holds no other warning or error runs no mediator.
  - Refusals: `not_mediator` (not the workspace the check was told to) and
    `no_mediation_owed` (a fix is dispatched already). A dispatch is never
    refused for what the deploy holds.
  - An escalated check holds the queue until the escalation's fix deploys and
    is approved, or the user releases it: the sidebar's "release" beside
    "ZETTEL NEEDS YOU", or `merge-queue release-escalation -repository
    here|DIR`, refused `no_escalation_held` when no escalation holds the
    queue. The mediator stays open.
  - The mediator's `workspace finished` answers `landing_directly`:
    it never joins the queue. Its gate, direct landing and deploy run
    unattended; a failed gate comes back to it as a resumed turn.
- Holding or releasing a repository's deploys is `/agent-repl-runtime`'s.

### Commandeering the running spot

The user wants a merge to run BEFORE the one already running, not merely be
next (owner ruling 2026-10-23). CRITICAL: do this only when the user asked for
it; it discards the running merge's test progress.

1. Put the commandeering merge at the front: a new request uses
   `-placement front`; a waiting one is moved with `merge-queue move-up MERGE`
   until it sits directly behind the running merge (`move-up` never passes the
   running merge).
2. `merge-queue status` and note the running merge's name. Its request is
   kept when it is dequeued; nothing needs recording.
3. `merge-queue dequeue RUNNING`. The commandeering merge starts.
4. Once `merge-queue status` shows the commandeering merge running,
   `merge-queue requeue RUNNING -placement front` puts the displaced merge
   back at the FRONT so it runs right after. Anyone may run it: NEVER prompt
   the workspace, and NEVER re-request an async merge.
5. Report which merge was displaced and that it was requeued.

- **CRITICAL: between the dequeue (step 3) and the requeue (step 4), a
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
