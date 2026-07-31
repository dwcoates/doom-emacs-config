# daemon/internal/workspace/merge/

The workspace-merge subsystem. Two components, with strictly separated
responsibilities:

- `merge.Driver` — the git cherry-pick layer (`git -C <dir>`): run the pick,
  detect conflicts, finalize a merged workspace, resume after a resolution.
  Stateless per call, and the ONLY component that shells out to git.
- `merge.Coordinator` — the per-repository singleton that owns the merge
  QUEUE, the shim exclusivity lease, and conflict resolution. It is the only
  caller of `merge.Driver`.

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
