# daemon/internal/workspace/merge/

The workspace-merge engine, ported from the Emacs `merge-handlers.el`
producers. Responsibility: run the cherry-pick driver (`git -C <dir>`),
detect conflicts, finalize merged workspaces, and resume after a human
resolves a conflict (the resolve-and-continue handoff arrives as a
`FrontendCommand`). Every merge-state transition (`merging`, `merge_queued`,
`merge_conflict`, `merge_failed`, `merged`) is written to the SSM — never to
the shim-store, which is agent-interaction-only.

Emacs keeps only the reactive conflict-resolution UX.

Dependencies: `daemon/internal/ssm/`, git (via the daemon's exec wrappers).
