# daemon/internal/workspace/merge/

The workspace-merge subsystem. Two components, with strictly separated
responsibilities:

- `merge.Driver` — the git rebase-and-merge layer (`git -C <dir>`): rebase the
  branch's commits onto the target's head IN A TEMPORARY WORKTREE, commit by
  commit, detect conflicts, run the target repository's test suite ONCE on the
  head that replay produced, and then move the target EXACTLY ONCE with a
  `--no-ff` merge commit. Stateless per call, and the ONLY component that shells
  out to git.
- `merge.Coordinator` — the per-repository singleton that owns the merge
  QUEUE, the shim exclusivity lease, and conflict resolution. It is the only
  caller of `merge.Driver`.

## THE TARGET IS NOT THE WORKBENCH

This is the load-bearing decision of the whole subsystem, and everything below
is downstream of it.

`merge.Driver` used to cherry-pick the branch's commits INTO THE TARGET
CHECKOUT one at a time and `reset --hard` it back when the gate failed. Two
consequences followed, and both were structural rather than incidental:

- the tree every other workspace cuts from CARRIED UNTESTED COMMITS for the
  whole of a merge, including for the length of an agent turn spent resolving a
  conflict or fixing a suite;
- the rollback was a `reset --hard` fired at the end of that unbounded window,
  and it TWICE DESTROYED unrelated commits that had reached the target
  meanwhile. Guarding it with the head the merge left (`Result.TestedHead`)
  converted the hazard into a refusal; it did not remove it.

THE REBASE REMOVES BOTH. `merge.Driver.Merge` creates a TEMPORARY WORKTREE of
the target's repository at the target's current head (`git worktree add
--detach`), replays `merge-base..branch` into it commit by commit, and gates the
resulting HEAD there. Nothing reaches the target until every commit has landed
and the head has passed. The choreography:

1. PRECONDITIONS, before any transition: the source worktree is clean, the
   branch exists, and the target carries neither sequencer residue nor an
   unfinished merge of its own.
2. STALE TREES ARE PRUNED. Merges are serialized per REPOSITORY, so any rebase
   worktree still registered when one starts belongs to a daemon that died
   mid-merge. It is removed and loud-logged — otherwise a bounce leaks one
   registration per interrupted merge.
3. THE REBASE, in the temp worktree, and THE ONE TEST GATE on the head it
   reaches (below).
4. THE ONE TARGET MOVE (`landOnTarget`), and only if the rebase reached the end
   and its head passed:
   - THE GUARD. The target must still be on `Request.BaseHead`, the head the
     rebase based itself on. Everything the gate certified was certified against
     that head. A target that moved is `errTargetMoved`, and `Merge` answers it
     by RESTARTING THE WHOLE CYCLE on the target's new head — fresh rebase
     worktree, replay, suite selection, suite, land — UNBOUNDEDLY, as often as
     the target moves. There is no attempt cap: a moved target is another writer
     landing first, which is the ordinary condition of a shared repository
     rather than a defect in the branch. The loop exits only by landing, by a
     REAL failure (conflict, escalated gate, refused merge commit), or by a
     cancelled context.
   - THE BRANCH REF MOVES to the rebased line, compare-and-swap against the
     value this merge read, logged with both SHAs, and the source worktree is
     re-synced onto it. That is what makes the merge commit's SECOND PARENT the
     workspace's BRANCH rather than an anonymous commit, so `git log --graph` on
     the target shows the workspace by name. A source worktree that became dirty
     meanwhile FAILS the merge here, before the target is touched.
   - `git merge --no-ff`. It cannot conflict — the target is on the head the
     rebased line descends from — so a non-zero exit is the target holding work
     of its own the merge would overwrite. That aborts the half-made merge,
     RESTORES the branch ref, and is reported as a terminal `merge_failed`
     (`errMergeRefused`), with the target still holding exactly what it held.
5. AN EMPTY BRANCH NO-OPS. A rebase that produces nothing over the target head
   is marked merged WITHOUT a merge commit: an empty merge commit would record a
   topology saying work arrived when none did.

**THERE IS NO ROLLBACK, AND ITS ABSENCE IS THE POINT.** `merge.Driver.Rollback`
is RETIRED. A failure at any pre-merge stage — an unresolved conflict, a suite
that failed twice, an abandoned merge, a daemon bounce — leaves the target
BYTE-FOR-BYTE as the merge found it, so there is nothing to reset and no window
in which resetting could destroy somebody else's work. The refusal logic did not
disappear with it: it MIGRATED to `landOnTarget`'s `BaseHead` guard, where what
was a last-resort protection against a destructive reset is now a precondition
of a purely additive one. Every terminal failure cause says "the target was
NEVER MODIFIED", because that is the fact a user needs.

**THE TEMPORARY WORKTREE IS CLEANED UP ON EVERY PATH, THROUGH ONE IDEMPOTENT
FUNNEL** (`rebasecleanup.go`). `merge.Driver` removes its own on any outcome it
does not park — a landed merge, a failure for good, an escalation, and the
SUPERSEDED cycle of every `errTargetMoved` restart. A PARKED outcome hands the
tree to `merge.Coordinator` along with the responsibility, which discharges it
from a `defer` over `settle` — the one function that owns the whole of a merge's
life after `Merge` returns, so no exit path (terminal, abandon, shutdown, panic
unwinding) can miss it. Eviction needs none: `EvictWaiting` only ever removes
entries that never reached a driver call. A cleanup failure can never un-merge a
merge; it is loud-logged and nothing more.

- BOTH CALL SITES ARE CORRECT, and neither can be deleted, so removing an
  ALREADY-REMOVED worktree is a SILENT NO-OP. It used to be exit 128 from
  `git worktree remove --force` against a path git had already forgotten, and a
  loud FAILED line for a directory that was already gone — pure noise following
  the REMOVED line the first call had just logged.
- SUCCESS IS JUDGED ON THE POSTCONDITION, not on any one step: the tree is gone,
  the TEMP PARENT `os.MkdirTemp` made is gone with it, and `git worktree prune`
  has run. A `remove` that refused a tree the filesystem removal then took away
  is administrative noise; a tree that is STILL THERE is a real failure and stays
  loud. Removing only the `rebase` leaf and leaving the parent is what filled
  `$TMPDIR` with 893 `agent-repl-merge-rebase-*` directories.

**AND WHAT A DEAD DAEMON LEFT IS SWEPT AT BOOT** (`rebasesweep.go`). A temp
worktree dies with its process in every sense except the bytes, and the
per-merge prune only ever sees what is still REGISTERED against its own target.
`merge.Driver.SweepOrphanRebaseWorktrees` walks the driver's REBASE ROOT once at
boot, removes every `agent-repl-merge-rebase-*` directory the retention set does
not name, and prunes the repositories they belonged to — read off each orphan's
own linked worktree `.git` file, so a boot with an empty queue still clears a
repository's accumulated registrations. It logs ONE summary line: a sweep of
hundreds that says a line per directory is a sweep nobody reads. The retention
set is `merge.Coordinator.RetainedRebaseWorktrees` — the trees live merges are
working in, above all a CONFLICT-PARKED one, whose tree is the resolution's
workbench. It is not derivable from the durable records, which deliberately
carry no temp worktree at all. The sweep never fails the boot.

- THE ROOT IS INJECTED AND REQUIRED (`Config.RebaseRoot`, threaded from
  `server.AgentShimConfig.RebaseRoot`, which `main` sets to the process temp
  dir). `createRebaseWorktree` makes its parents under the SAME field, so what
  the pipeline creates and what the sweep scans cannot diverge. It used to be
  `os.TempDir()` resolved at sweep time, and that made a catastrophe
  representable: any TEST that reached the sweep — the `internal/server` boot
  wiring tests above all — swept the REAL temp dir with a TEST coordinator's
  retention set and deleted the LIVE daemon's rebase worktrees, including the
  tree a merge gate's own `go test ./...` was running inside (the suite lost its
  package source mid-run, the entrypoint exited 127, the flake re-run could not
  resolve the toplevel, and the workspace fell out `merge_failed`). An empty
  root is a construction error at BOTH layers; every test passes `t.TempDir()`.
- AND THE `.git` FILE IS CHECKED BEFORE THE REMOVAL, not merely read for the
  prune: a leftover naming a repository outside `SweepScope.Repos` (the
  coordinator's `ManagedRepos`, plus the repositories the retained trees vouch
  for) is KEPT and counted `kept_unknown_repo` in the summary. Keeping a
  directory too many leaks bytes; removing another daemon's live tree destroys a
  running merge.

**THE REBASE WORKTREE TRAVELS ON `Request.WorkDir` / `Request.BaseHead`**, set
by `Merge` on the `Result` and echoed back by `merge.Coordinator` into `Resume`,
`ContinueAfterTestFix` and `Cleanup`. Neither is part of the DURABLE queue
payload: a temp worktree does not survive the process that made it, so a boot
replay starts a fresh rebase rather than resuming into a directory that is gone.
The steps that work inside an existing rebase REFUSE to run without it
(`validateRebase`) — falling back to the target is precisely the behavior this
design removed.

**THE WIRE IS UNCHANGED.** `MergeStatus`'s `cherry_picking` oneof arm and its
fields (`current_sha`, `current_subject`, `commits_total`, `commits_landed`)
carry the REBASE's progress: the figures map one-for-one, and a rebase is
replaying the same planned commits the picks did. Only the human-readable cause
text changed (`rebasing 3/7: abc123`). Renaming the arm would have been a
schema change every frontend had to land in lockstep for no new information.

## The replay is per-commit; the gate is ONE run at the head

`merge.Driver` replays the range one commit at a time (`rev-list --reverse
--no-merges`, then `cherry-pick -x <sha>` each) IN THE REBASE WORKTREE, and then
runs the target repository's test suite ONCE, on the head that replay produced,
immediately before the target move.

THE TWO HALVES HAVE DIFFERENT REASONS, and neither implies the other:

- THE REPLAY IS PER-COMMIT because a conflict has to be PARKED on the commit
  that caused it — that is what a resolver is handed and what `Resume`
  continues — and because the loop's restartability is derived from the `-x`
  annotation each pick leaves behind.
- THE GATE IS ONE RUN AT THE HEAD because the user chose one suite run per merge
  over per-commit attribution. A full suite per replayed commit is the merge's
  whole cost multiplied by the range's length, and the head is the only tree
  `git merge --no-ff` puts on the target: an intermediate commit that would have
  failed on its own is not a fact about what the target receives. The price is
  that a failure names the merge rather than a culprit commit, and that is the
  trade that was made deliberately.

`cherry-pick -x` is HOW A REBASE IS SPELLED here, not a leftover. `git rebase`
is itself a sequence of picks and offers no point between two commits at which
this pipeline could hand a conflict to an agent and come back. The `-x`
annotation is retained because the loop's own restartability reads it and
because it records which branch commit each rebased commit came from.

- THE SUITE IS A PORT, AND IT RUNS IN THE REBASE WORKTREE.
  `merge.SuiteRunner` (`suiterunner.go`) resolves the repository's own
  entrypoint — `modules/app/agent-repl/bin/test-all.sh` relative to its
  toplevel — in the tree it is handed, which is a full checkout of exactly the
  content the merge proposes to land. A target that declares none SKIPS the gate with a
  loud log naming the absence; this machinery serves repositories that have no
  agent-repl suite, and a skip is never reported as a pass.
- THE LOOP IS RESTARTABLE BY CONSTRUCTION, GATE INCLUDED. It derives its work
  from git alone (the rebase base, which advances past every `-x` annotation,
  plus a per-commit patch-id probe), so re-entering it after a resume or a test
  fix skips what already landed. THE GATE IS THE LOOP'S TAIL rather than a step
  inside it, so a re-entry whose replay is already complete runs the gate and
  replays nothing — "the range is landed but the head never passed" needs not one
  byte of side-channel state to re-enter correctly. A whole daemon BOUNCE is
  different again: the temp worktree died with the process and the target kept
  nothing, so the boot replay re-rebases the range from scratch onto a target
  that is exactly where it was.
- A MERGE THAT CHANGES NOTHING RUNS NO SUITE. An empty range, a range every
  commit of which is already incorporated, and a range whose every replay went
  empty all short-circuit to the no-op merged path: the tree the merge proposes
  is the tree the target already has, so there is nothing for a suite to
  testify about.
- MERGE COMMITS ARE FLATTENED. `--no-merges` drops them: a merge commit carries
  no patch of its own and both of its sides are already in the range. The
  whole-range driver this replaced failed outright on such a branch, because
  `git cherry-pick` refuses a range containing a merge.
- THE SUITE SET IS A FUNCTION OF THE PATHS THE MERGE TOUCHES (`suiteselect.go`).
  The gate used to hand the entrypoint no arguments, so every merge ran all
  eighteen suites — and a four-file webapp-only merge was denied by a shell
  harness and by a Go suite that shared not one line with the change. A suite
  that cannot be affected by a change cannot testify about it; it can only add a
  way for the merge to fail. `SelectSuites` maps the range's paths onto suite
  names and the runner passes them as `bin/test-all.sh --suites a,b,c`.
  - UNKNOWN BEATS WRONG. `suiteRules` names only the roots whose blast radius is
    known; ANY path matching none of them selects the FULL set, as does a change
    whose paths could not be read. Adding a directory to the repository can only
    make the gate more conservative.
  - AN EMPTY SELECTION IS "EVERYTHING", and it reaches the entrypoint as NO
    ARGUMENTS — which is both that script's own default and the only shape a
    foreign repository's entrypoint is guaranteed to accept.
  - THE RANGE, NOT THE HEAD COMMIT. The gate runs once, but selects from the
    whole `merge-base..branch` range, so a branch whose last commit touches the
    webapp is not gated on the webapp suite alone while the daemon change it
    also carries goes untested. A re-gate after a resolution turn reaches the
    same answer, widened by the fix commit's own paths.
  - `merge.allSuites` and `bin/test-all.sh`'s `ALL_SUITES` are two lists that
    can disagree, and a name the script does not declare is rejected at run
    time. `TestRosterMatchesTheEntrypointScript` reads the script and holds them
    identical.
- A FAILING SUITE IS RE-RUN EXACTLY ONCE, on the same tree, with the same
  selection. A gate that denies on a first failure denies on every flake, and
  these suites share a machine with a live daemon and whatever else is running.
  A pass on the re-run is a FLAKE: both verdicts, both durations and BOTH
  archives are logged loudly and the merge proceeds. A second failure is
  genuine and takes the pre-existing path; its result carries the re-run's tail
  with the first run's archive path appended, so neither half is lost. It stops
  at one because a gate that keeps retrying eventually passes anything.
- THE GATE MOVED HERE FROM THE PRE-COMMIT HOOK. `.githooks/pre-commit` used to
  run the whole unified suite before any agent-authored commit. That taxed every
  intermediate commit on a workspace's own branch and said nothing about the
  TARGET, which is the tree everyone else works from. The hook now runs only the
  grep-only external-boundary lint.

## A test failure gets ONE agent attempt, and the target never sees it

A head gate that fails parks exactly the way a conflict does, and the merging
workspace's OWN session is asked to fix it through `merge.TestFailureResolver`
(`testfailureresolver.go`), IN THE REBASE WORKTREE. `merge.Coordinator` then
commits whatever that turn staged as a FOLLOW-UP commit (never an amend: an
amend would rewrite the `-x` annotation the replay's restartability keys on) and
re-enters the replay, which re-gates the head the fix produced.

- EXACTLY ONE ATTEMPT PER FAILURE. `merge.Coordinator` keys that accounting on
  `Result.FailingCommit`, and the driver PINS that identity across the re-gate:
  the fix commit moves the head, so reporting the new head's sha would read as a
  brand-new failure and hand out another attempt, and another, for as long as the
  agent keeps committing something that does not fix the suite. A repeat failure,
  a resolver error, and a driver error all fail the merge.
- **THE TARGET IS NEVER MODIFIED.** The failing head exists only on the
  rebased line in the temp worktree; the target carries not one line of it. The
  run emits `merge_failed` carrying that head and the suite's output
  tail, says so in the cause, discards the temp worktree, and releases
  everything per the ordinary terminal path. Nothing is lost — the source branch
  still holds every commit and the merge can be retried once the work is fixed
  there.
- THE AGENT IS POINTED AT THE REBASE WORKTREE, in the resolution prompt and in
  `TestFailureResolution.TargetDir`. That field keeps its name because the
  user-editable prompt's `{{target_dir}}` placeholder is what it fills; its
  documentation says plainly which tree it names.
- **THE SUITE'S COMPLETE OUTPUT IS ARCHIVED, and the cause names the file.**
  The tail is clamped, and the repository's entrypoint keeps running suites
  after one fails, so the retained bytes are routinely the LAST suites'
  coverage tables rather than the failure. `merge.SuiteRunner` writes the whole
  run to a file and reports its path on `SuiteResult.OutputPath`.

  THAT FILE IS THE CHILD'S STDOUT, NOT A COPY OF IT, and that is load-bearing
  rather than incidental. Capturing into a `bytes.Buffer` makes `os/exec`
  manufacture an `os.Pipe` whose write end EVERY descendant of the suite
  inherits, and `Wait` blocks until the last of them closes it — so one
  background daemon an e2e suite leaks holds the merge queue head forever, with
  `suite RUNNING` logged and no verdict ever. An `*os.File` is dup2'd straight
  into the child: no pipe, no copying goroutine, and `Wait` returns when the
  CHILD is reaped. `WaitDelay` and `Cancel` are set so a future `StdoutPipe`
  degrades into a bounded wait rather than back into the wedge, and the suite is
  spawned `Setpgid` so whatever it leaks is SIGKILLed with its own process group
  — only ever that group — and loud-logged when there was anything to kill.

  Because the file must exist before the spawn, a file that cannot be created
  is an UNRUNNABLE SUITE surfaced as an error, not a lost archive; a tail that
  cannot be read back is logged loudly and named in the tail, never dropped.

## Conflict resolution is shim-driven first, human second

When the replay conflicts, `merge.Coordinator` parks it IN THE REBASE WORKTREE
AND hands the conflict to the merging workspace's OWN agent session — the one that wrote the
conflicting commits, and the one whose shim the coordinator already holds the
`merge.Lease` over. That handoff goes through `merge.ConflictResolver`
(`conflictresolver.go`), the package's outbound port:

- `merge.ConflictResolution` carries the facts (workspace, request id, conflict
  commit, source branch, and the REBASE WORKTREE the conflict is parked in) and
  OWNS the prompt text (`Prompt()`), because what the agent may do with a paused
  replay is merge-subsystem knowledge: resolve and `git add`, never `--continue`
  and never commit — the coordinator resumes the replay itself. A conflict a
  human abandons leaves the temp tree to be discarded with the run; the target
  was never touched, so there is nothing to clean up there.
- `Resolve` returns only once the resolution TURN HAS ENDED. The implementation
  is `(*sessioncontroller.Manager).ResolveMergeConflict`, reached through the
  server's `PromptRouter` so the fleet that serves the user's prompts is
  necessarily the fleet a merge drives. This package never imports the session
  controller.
- The wait is TWO PHASES, BIND then WORK. The turn must START within a short
  bind bound (`sessioncontroller.mergeResolutionTurnBindBound`); only a turn
  that started gets the long one. A prompt that produced no turn — parked on the
  queue because the workspace is BUSY with a turn of the user's own, or
  forwarded to a shim that never began one — fails within the bind bound with a
  cause naming the submit's own disposition, instead of holding the merge and
  the workspace's shim lease for a window sized for an agent that is working.
  A busy workspace's parked prompt is taken back off the queue with the failure,
  so it cannot be delivered after the merge has already failed.
- EXACTLY ONE attempt. A resolver error, a refused submit, a turn that never
  starts, a turn that never ends, or a resume that is still conflicted all leave
  the park STANDING for the human path (`conflict_resolved_continue`, or abandonment by closing the
  workspace). Nothing is ever marked merged on a failed attempt.
- The attempt's resume rides the same `park.calls` rendezvous a human's resume
  does, so a human resume or an abandon arriving mid-attempt is serialized
  against it rather than racing it.

## What happens after a merge lands: `merge.PostMergeHook`

A merged workspace can require process-level aftermath. `merge.PostMergeHook`
(`posthook.go`) owns that boundary, including rebuilding and restarting the
running stack after a self-merge. Separately, a workspace created with a
`postprocessing_prompt` has that
  prompt run as a TURN IN ITS OWN SESSION, under the merge lease, once every
  commit has landed and before the queue entry is retired
  (`merge.AfterActionSource` and `merge.AfterActionRunner`).

The port's contract:

- It fires on the `merged` terminal outcome ONLY — never on `merge_failed`,
  never on a conflict, and never on a conflict a user abandoned. A merge that
  landed only after a resolution is merged all the same and fires it too.
- It fires AFTER the queue entry is dropped and the lease released.
- It runs OFF the drain goroutine, on its own `c.wg`-tracked goroutine bounded
  by the coordinator's context. A slow rebuild must never stall the
  repository's queue — the next merge starts immediately.
- A hook error CANNOT un-merge the merge. It is loud-logged and retained as a
  `merge.PostMergeFailure` (readable via `PostMergeFailures`), never turned into
  a `merge_failed` transition: the commits are on the target either way, and
  saying otherwise would make the pushed state lie about the tree.
- A merge whose durable entry could NOT be dropped does not fire the hook at
  all, because the next boot's `Drain` replays it and aftermath must run once.

The after-action source is `internal/workspace/postmerge` (`postmerge.Source`),
reached through the server's wiring, so this package never imports creation
storage.

THE TERMINAL `merged` STATUS IS `merge.Coordinator`'s, NOT `merge.Driver`'s.
The driver finishes the replay and returns `OutcomeMerged`; the coordinator then
runs the after-action and publishes `merged` ONCE, carrying the action's failure
as `after_action_error`. Publishing it from the driver put the run's terminal
word on the wire before the `after_action` phase existed, so every frontend saw
the merge finish and then watched a phase begin after it.

## The terminal WORD is as durable as the merge

A merge's outcome survives anything: the commits are on the target and the
durable queue entry is still there. Its terminal STATUS used to survive
nothing — a sink that refused `merged` or `merge_failed` got a loud log, the
entry was acked anyway, and the run's last word could never reach a frontend.
The two are now retired together.

- ONLY A TERMINAL PUBLICATION THAT LANDED ACKS THE ENTRY. `DurableQueue.Complete`
  is the ack and nothing else calls it.
- A PUBLICATION THAT DID NOT LAND MARKS THE ENTRY. `DurableQueue.MarkTerminal`
  writes the word onto the OUTSTANDING head entry (`pending_terminal`: the
  outcome, the cause, and a merged run's `after_action_error`), the lease goes
  back — a lease never outlives the merge that took it — and the repository's
  drain HALTS, exactly as it does for a `Complete` that failed.
- THE NEXT BOOT REPLAYS THE WORD, NOT THE MERGE. A marked entry belongs to a run
  whose outcome was already reached, so `merge.Coordinator` publishes the
  recorded status under the id the entry was admitted with and retires it. No
  lease, no session bring-up, no rebase: every one of those would redo work
  that is over. The post-merge hook fires there for the first time, because the
  entry was never dropped before.
- A REPLAY THAT ITSELF CANNOT PUBLISH CHANGES NOTHING. The entry stays marked and
  outstanding for the boot after it. The word is only ever dropped by a
  publication that landed.
- THE OUTCOME IS NEVER ROLLED BACK BY A PUBLISH FAILURE. A merged run stays
  merged and a failed run stays failed; what was missing was the saying of it,
  and that is the only thing this recovers.

A MERGE OF A DELETED SESSION IS REFUSED. `merge.SessionDeaths` (the pipeline's
fourth outbound port) reports whether the workspace's newest session is terminal
by deletion, and is asked BEFORE `merge.SessionBringUp` -- asking after would be
asking about a session the bring-up had already spawned back. A hibernated
session is rehydrated and merged; a deleted one fails the run with a cause that
names the deletion.

Every merge-state transition (`merge_enqueuing`, `merging`, `merge_queued`,
`merge_conflict`, `merge_failed`, `merged`) is written to the SSM. The
replay loop and its gate both reuse `merging` rather than adding vocabulary:
their cause strings carry the progress (`rebasing 2/3: abc123def456`, then
`testing the rebased head abc123d after 3 commits [daemon,webapp]`), so a user
watching a merge sees where it is without a new phase or a new proto enum value. Transitions are written to the SSM — never to
the shim-store, which is agent-interaction-only.

`merge_enqueuing` is the ONE phase this package does not emit itself. The
frontend command handler emits it the instant a merge command arrives, before
the geometry is resolved and before `merge.Coordinator.Enqueue`, so the very
first thing a merge attempt does is become visible. It is also the one phase
with NO durable queue entry behind it, which is why
`merge.Coordinator.Drain` sweeps any workspace still resting on it with no
entry to `merge_failed` at boot: a daemon that died in that window genuinely
lost the attempt, and nothing else would ever advance the phase. The sweep
reads the phase back through `merge.PhaseSource`, the package's third
outbound port.

`merge_enqueuing` is not the only phase that can outlive its merge, so `Drain`
carries a SECOND sweep beside it: every workspace resting on `merge_queued`,
`merging`, `merge_before_action` or `merge_conflict` with NO durable queue
entry, NO open `merge.Lease` and NO live run gets a terminal `merge_failed`
naming `orphaned_by_restart`. A daemon killed while a replay was parked on
a conflict leaves exactly that state, and a non-terminal merge axis refuses
every later prompt, fails the revive path's synchronous-prompt invariant, and
holds the workspace's teardown guard shut forever. ANY ONE of the three facts
retains the workspace — the entry is a merge this boot replays, an open lease is
one the drain reconstructs, a live run is the opposite of orphaned — and every
decision is logged, retentions included. `merge_after_action` is deliberately
NOT swept: every commit is on the target by then, and failing it would deny a
merge that demonstrably happened.

## An interrupt EVICTS the workspace's waiting merges

A stop from a frontend means "stop what this workspace is doing", and a merge
waiting its turn used to be the one piece of that work no user action could
reach: the composer is gated on the merge lease, the stop went to a shim with no
turn to end, and the merge ran anyway some minutes later.
`merge.Coordinator.Evict` is the interrupt's queue half — the frontend command
handler calls it on every interrupt it does not challenge, before the stop
reaches the shim.

- ONLY WAITING ENTRIES GO. `DurableQueue.EvictWaiting` never touches a
  repository's HEAD: that entry has already been handed to the drain, holds the
  shim lease, and may be mid-rebase, so removing its record would strand a
  running merge with nothing left to `Complete`. The verbs for a merge in flight
  are unchanged — resolve it, or close the workspace, which abandons it.
- EACH REPOSITORY IS EVICTED UNDER ITS ADVANCE GATE, the mutex `Enqueue` holds
  across its durable publish and its `enqueued` status. An eviction therefore
  cannot land between an admission's write and the status announcing it, so a
  run can never be told it was evicted and then told it is queued.
- THE ENTRY GOES FIRST, THE WORD SECOND. The durable file is removed before the
  terminal `failed` status is published, exactly as `finish` retires a terminal
  merge — and there is no `MarkTerminal` beside it, because keeping the entry so
  a later boot could re-announce the removal would put back the very merge the
  user took off the queue.
- THE WORD RIDES THE SAME RUN. The evicted status publishes under the run id the
  `enqueued` status the user is looking at carried, so the admission and its
  removal are one run rather than two unrelated events.

The queue is keyed by REPOSITORY, not by worktree: sibling worktrees of one repo
merge into the same target, so one queue serializes them all. Single ownership
is what makes a same-target merge race structurally impossible rather than
merely improbable, and it is also what lets a starting merge conclude that any
rebase worktree it finds registered is abandoned.

Emacs owns NOTHING here: no geometry, no handler resolution, no queue, no
merge state. It is informed of status by the daemon and renders it.

## Naming

NEVER refer to these types by their bare names. Always write the
package-qualified form — `merge.Coordinator`, `merge.Driver`, `merge.Lease`,
`merge.PostMergeHook`, `merge.SuiteRunner`, `merge.TestFailureResolver` — in
code comments, commit messages, design docs, and
prose alike.

`Coordinator`, `Driver`, and `Lease` are generic words that appear across the
daemon in unrelated roles, so a bare mention forces the reader to work out
which subsystem is meant. The qualifier is one token and removes the ambiguity
entirely.

Dependencies: `daemon/internal/ssm/`, git (via the daemon's exec wrappers).
