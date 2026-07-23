# daemon/internal/workspace/

Daemon-owned workspace operations. Houses the workspace-lifecycle logic that
is the daemon's (not the shim ecosystem's and not Emacs's) responsibility;
currently `merge/` (the workspace-merge port). Workspace state transitions
produced here flow into `daemon/internal/ssm/`.

Dependencies: `daemon/internal/ssm/`, git (via the daemon's exec wrappers).
