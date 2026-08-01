# daemon/internal/workspace/merge/

The workspace-merge subsystem. Two components, with strictly separated
responsibilities:

- `merge.Driver` — the git cherry-pick layer (`git -C <dir>`): run the pick,
  detect conflicts, finalize a merged workspace, resume after a resolution.
  Stateless per call, and the ONLY component that shells out to git.
- `merge.Coordinator` — the per-repository singleton that owns the merge
  QUEUE, the shim exclusivity lease, and conflict resolution. It is the only
  caller of `merge.Driver`.

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

A merged workspace is not the end of the story, and the two things that follow
it were Emacs's job until the editor strip deleted them. `merge.PostMergeHook`
(`posthook.go`) is the package's second outbound port, and it re-homes both
daemon-side:

- CHILD TO PARENT. A workspace spawned from another workspace merges into its
  PARENT's worktree, not into the repository's main checkout. The parent's
  agent session is told its child merged.
- THE POSTPROCESSING PROMPT. A workspace created with a `postprocessing_prompt`
  has that prompt run in the parent once the merge fully finishes.

The port's contract:

- It fires on the `merged` terminal outcome ONLY — never on `merge_failed`,
  never on a conflict, and never on a conflict a user abandoned. A merge that
  landed only after a resolution is merged all the same and fires it too.
- It fires AFTER the queue entry is dropped and the lease released. That
  ordering is load-bearing: the hook prompts another workspace's session, and
  doing it under a lease the merge still held would have the submit refused by
  the very exclusivity the merge took.
- It runs OFF the drain goroutine, on its own `c.wg`-tracked goroutine bounded
  by the coordinator's context. A slow, hung, or unreachable parent must never
  stall the repository's queue — the next merge starts immediately.
- A hook error CANNOT un-merge the merge. It is loud-logged and retained as a
  `merge.PostMergeFailure` (readable via `PostMergeFailures`), never turned into
  a `merge_failed` transition: the commits are on the target either way, and
  saying otherwise would make the pushed state lie about the tree.
- A merge whose durable entry could NOT be dropped does not fire the hook at
  all, because the next boot's `Drain` replays it and the parent must not be
  handed the same child twice.

The implementation is `internal/workspace/postmerge` (`postmerge.Notifier`),
reached — like `merge.ConflictResolver` — through the server's wiring, so this
package still never imports the session controller.

Every merge-state transition (`merge_enqueuing`, `merging`, `merge_queued`,
`merge_conflict`, `merge_failed`, `merged`) is written to the SSM — never to
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
`merge.PostMergeHook` — in code comments, commit messages, design docs, and
prose alike.

`Coordinator`, `Driver`, and `Lease` are generic words that appear across the
daemon in unrelated roles, so a bare mention forces the reader to work out
which subsystem is meant. The qualifier is one token and removes the ambiguity
entirely.

Dependencies: `daemon/internal/ssm/`, git (via the daemon's exec wrappers).
