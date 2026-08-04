# daemon/internal/workspace/merge/

The workspace-merge subsystem. Two components, with strictly separated
responsibilities:

- `merge.Driver` — the git cherry-pick layer (`git -C <dir>`): replay the range
  COMMIT BY COMMIT, run the target repository's test suite after each landing,
  detect conflicts, finalize a merged workspace, resume after a resolution.
  Stateless per call, and the ONLY component that shells out to git.
- `merge.Coordinator` — the per-repository singleton that owns the merge
  QUEUE, the shim exclusivity lease, and conflict resolution. It is the only
  caller of `merge.Driver`.

## The replay is per-commit, and every landing is tested

`merge.Driver` picks the range one commit at a time (`rev-list --reverse
--no-merges`, then `cherry-pick -x <sha>` each) and runs the target
repository's test suite after each commit lands. A single whole-range pick
could only be tested once, at the end, which names no culprit and gives a
resolution attempt nothing narrower than the whole range to reason about.

- THE SUITE IS A PORT. `merge.SuiteRunner` (`suiterunner.go`) resolves the
  TARGET repository's own entrypoint — `modules/app/agent-repl/bin/test-all.sh`
  relative to its toplevel. A target that declares none SKIPS the gate with a
  loud log naming the absence; this machinery serves repositories that have no
  agent-repl suite, and a skip is never reported as a pass.
- THE LOOP IS RESTARTABLE BY CONSTRUCTION. It derives its work from git alone
  (the cherry-pick base, which advances past every `-x` annotation, plus a
  per-commit patch-id probe), so re-entering it after a resume, a test fix, or a
  whole daemon bounce skips what already landed. That is what makes the durable
  queue's boot replay a no-op for the commits the previous daemon got through.
- MERGE COMMITS ARE FLATTENED. `--no-merges` drops them: a merge commit carries
  no patch of its own and both of its sides are already in the range. The
  whole-range driver this replaced failed outright on such a branch, because
  `git cherry-pick` refuses a range containing a merge.
- THE GATE MOVED HERE FROM THE PRE-COMMIT HOOK. `.githooks/pre-commit` used to
  run the whole unified suite before any agent-authored commit. That taxed every
  intermediate commit on a workspace's own branch and said nothing about the
  TARGET, which is the tree everyone else works from. The hook now runs only the
  grep-only external-boundary lint.

## A test failure gets ONE agent attempt, then the target is ROLLED BACK

A suite that fails after a landing parks exactly the way a conflict does, and
the merging workspace's OWN session is asked to fix it through
`merge.TestFailureResolver` (`testfailureresolver.go`). `merge.Coordinator`
then commits whatever that turn staged as a FOLLOW-UP commit (never an amend:
an amend would rewrite the `-x` annotation the replay's restartability keys on)
and re-runs the suite.

- EXACTLY ONE ATTEMPT PER FAILING COMMIT. A repeat failure on the same commit,
  a resolver error, and a driver error all go straight to the rollback path. A
  failure on a LATER commit is a different failure and earns its own attempt.
- **THE TARGET IS ROLLED BACK TO ITS PRE-MERGE HEAD.** `merge.Driver.Merge`
  records that head before it lands anything and returns it on every Result;
  `merge.Coordinator` resets the target to it, emits `merge_failed` carrying the
  failing commit and the suite's output tail, and releases everything per the
  ordinary terminal path. This is the load-bearing decision of the whole test
  gate: the target worktree is what every other workspace cuts from and merges
  into, so leaving it carrying commits that break its suite converts one
  workspace's failure into everyone else's. Nothing is lost — the source branch
  still holds every commit and the merge can be retried once the work is fixed
  there. The rollback deliberately does NOT `git clean`: untracked files in the
  target may be a human's own work.
- A ROLLBACK THAT ITSELF FAILS still fails the merge, and the `merge_failed`
  cause names the failed reset. A merge whose Result carries no pre-merge head
  (which no valid `merge.Driver` Merge produces) is failed with that absence
  named rather than papered over.

## Conflict resolution is shim-driven first, human second

When a cherry-pick conflicts, `merge.Coordinator` parks it AND hands the
conflict to the merging workspace's OWN agent session — the one that wrote the
conflicting commits, and the one whose shim the coordinator already holds the
`merge.Lease` over. That handoff goes through `merge.ConflictResolver`
(`conflictresolver.go`), the package's outbound port:

- `merge.ConflictResolution` carries the facts (workspace, request id, conflict
  commit, source branch, target dir) and OWNS the prompt text (`Prompt()`),
  because what the agent may do with a paused cherry-pick is merge-subsystem
  knowledge: resolve and `git add`, never `--continue` and never commit — the
  coordinator resumes the pick itself.
- `Resolve` returns only once the resolution TURN HAS ENDED. The implementation
  is `(*sessioncontroller.Manager).ResolveMergeConflict`, reached through the
  server's `PromptRouter` so the fleet that serves the user's prompts is
  necessarily the fleet a merge drives. This package never imports the session
  controller.
- EXACTLY ONE attempt. A resolver error, a refused submit, a turn that never
  ends, or a resume that is still conflicted all leave the park STANDING for the
  human path (`conflict_resolved_continue`, or abandonment by closing the
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
  lease, no session bring-up, no cherry-pick: every one of those would redo work
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
per-commit loop reuses `merging` rather than adding vocabulary: its cause
strings carry the progress (`testing 3/7 after cherry-pick of abc123def456`), so
a user watching a merge sees where it is without a new phase or a new proto
enum value. Transitions are written to the SSM — never to
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

The queue is keyed by REPOSITORY, not by worktree: sibling worktrees of one
repo cherry-pick into the same target, so one queue serializes them all.
Single ownership is what makes a same-target cherry-pick race structurally
impossible rather than merely improbable.

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
