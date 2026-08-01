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

Every merge-state transition (`merging`, `merge_queued`, `merge_conflict`,
`merge_failed`, `merged`) is written to the SSM — never to the shim-store,
which is agent-interaction-only.

The queue is keyed by REPOSITORY, not by worktree: sibling worktrees of one
repo cherry-pick into the same target, so one queue serializes them all.
Single ownership is what makes a same-target cherry-pick race structurally
impossible rather than merely improbable.

Emacs owns NOTHING here: no geometry, no handler resolution, no queue, no
merge state. It is informed of status by the daemon and renders it.

## Naming

NEVER refer to these types by their bare names. Always write the
package-qualified form — `merge.Coordinator`, `merge.Driver`, `merge.Lease` —
in code comments, commit messages, design docs, and prose alike.

`Coordinator`, `Driver`, and `Lease` are generic words that appear across the
daemon in unrelated roles, so a bare mention forces the reader to work out
which subsystem is meant. The qualifier is one token and removes the ambiguity
entirely.

Dependencies: `daemon/internal/ssm/`, git (via the daemon's exec wrappers).
