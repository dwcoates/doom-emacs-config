# daemon/internal/workspace/geometry/

THE daemon's workspace -> merge-geometry map: which branch a workspace's
commits are cherry-picked from (`SourceBranch`), which worktree they live in
(`SourceDir`), and which worktree they land in (`TargetDir`).

Three components:

- `geometry.Store` — the durable map, a table in the SHARED state store
  (`internal/statedb`), beside the session registry and the session-state
  manager's log. Keyed by the daemon's workspace key (the session cwd).
- `geometry.Deriver` — git-fact derivation for workspaces that predate the
  daemon owning the map. Env-stripped `git -C <dir>`, mirroring
  `merge.gitCmd`'s stripping for the same reason.
- `geometry.Backfiller` — the boot pass that gives those older workspaces a
  record, once, from `geometry.Deriver`.

## One owner, and no guessing

Emacs used to compute all three coordinates and ride them on every
`merge_workspace` command. Two owners of one map is how a merge landed against
a target the daemon had never heard of, so the command is now a bare request
keyed by workspace and this package is the only answer.

A workspace with NO record is never guessed at. The merge is refused on the
ack with an explanation naming the workspace. A cherry-pick against a
synthesized target writes commits into a repository nobody asked for, which is
strictly worse than a refused merge.

`OriginCreated` (observed while creating the worktree) outranks
`OriginBackfilled` (derived later from git): an observed record replaces a
disagreeing derived one, and a derived record never displaces an observed one
(`ErrGeometryConflict`).

## Naming

Always write the package-qualified form — `geometry.Store`, `geometry.Record`,
`geometry.Deriver`, `geometry.Backfiller` — in code comments, commit messages,
and prose, the same rule `internal/workspace/merge/AGENTS.md` states for its
own types.

Dependencies: `daemon/internal/statedb/`, `daemon/internal/dlog/`, git.
