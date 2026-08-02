# daemon/internal/workspace/

Daemon-owned workspace operations. Houses the workspace-lifecycle logic that
is the daemon's (not the shim ecosystem's and not Emacs's) responsibility:

- `create/` — durable workspace creation (worktree, session, initial prompt).
- `merge/` — the workspace-merge port (`merge.Driver` / `merge.Coordinator`).
- `geometry/` — the workspace -> merge-geometry map the merge command resolves
  through, recorded at creation and derived at boot for older workspaces.

## The command-file ingress is a ROUTER, and nothing else

`create.Inbox` claims a `workspace_commands_*.json` file, parses it, persists
every entry durably, routes each entry to its owner, and deletes the claim. It
NEVER runs a job state machine. It used to: `Manager.Resume` ran inline on the
same goroutine, so one job wedged inside its state machine froze all ingestion
until the daemon was bounced (observed in production).

Three goroutines, and the channels between them carry IDS ONLY — the durable
stores stay the source of truth, so a daemon bounce replays from disk:

- THE ROUTER (`Inbox.Run`) — claim, parse, persist, route.
- THE CREATION WORKER (`Manager.RunCreationWorker`) — the single owner of
  `Manager.Process`. Every path that wants a job advanced routes its id:
  command-file ingestion, an interactive `createWorkspace`, a materialization
  ACK, and boot resume all feed the same channel. One poisoned job is contained
  here (`ErrJobFailed` is already durable, logged, and surfaced) and the worker
  takes the next id.
- THE HOST-ACTION WORKER (`Manager.RunHostActionWorker`) — the single owner of
  `Manager.DrainHostActions`. A host slow to accept an action delays neither the
  router nor a workspace creation.

`Manager.RouteJob` NEVER BLOCKS. A full route buffer degrades to a coalesced
store sweep rather than to a blocked router or a dropped id.

## The merge verb is DAEMON-OWNED

A `{"type":"merge"}` entry is routed straight onto the merge queue for its
repository. It used to be a host action: Emacs received it, resolved the
workspace name heuristically (literal name, then branch tail, then project_dir),
and sent a `merge_workspace` command back. That round trip is deleted.

- THE ONLY KEY is the entry's `project_dir`, absolute and matched EXACTLY
  (`filepath.Clean` only) against `geometry.Store` — the same map a frontend
  merge resolves through, so the two can never disagree about a target.
- AN ENTRY MISSING `project_dir`, CARRYING A RELATIVE ONE, OR NAMING NO RECORDED
  WORKSPACE IS REJECTED: quarantined, and logged with both the workspace name
  and the project_dir. There is no name resolution, no branch tail, and no
  legacy support. That is an explicit product decision, not an omission.
- The repository the request lands under is derived by `merge.Coordinator` from
  the resolved geometry's target worktree (`git rev-parse --git-common-dir`),
  never from parsing a worktree path.

Workspace state transitions produced here flow into `daemon/internal/ssm/`.

Dependencies: `daemon/internal/ssm/`, git (via the daemon's exec wrappers).
