# daemon/internal/workspace/

Daemon-owned workspace operations. Houses the workspace-lifecycle logic that
is the daemon's (not the shim ecosystem's and not Emacs's) responsibility:

- `create/` — durable workspace creation (worktree, session, initial prompt).
- `merge/` — the workspace-merge port (`merge.Driver` / `merge.Coordinator`).
- `geometry/` — the workspace -> merge-geometry map the merge command resolves
  through, recorded at creation and derived at boot for older workspaces.

Workspace state transitions produced here flow into `daemon/internal/ssm/`.

Dependencies: `daemon/internal/ssm/`, git (via the daemon's exec wrappers).
