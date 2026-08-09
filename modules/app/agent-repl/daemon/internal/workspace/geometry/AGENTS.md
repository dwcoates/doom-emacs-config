# daemon/internal/workspace/geometry/

THE daemon's workspace -> merge-geometry map: which branch a workspace's
commits are rebased from (`SourceBranch`), which worktree they live in
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
ack with an explanation naming the workspace. A merge against a
synthesized target writes commits into a repository nobody asked for, which is
strictly worse than a refused merge.

`OriginCreated` (observed while creating the worktree) outranks
`OriginBackfilled` (derived later from git): an observed record replaces a
disagreeing derived one, and a derived record never displaces an observed one
(`ErrGeometryConflict`).

## Retired columns

`workspace_merge_geometry.before_action` is RETIRED. It once carried a second
copy of the workspace's `before_ws_merge` prompt; nothing writes or reads it now.
That prompt is a field of the create Request, and the merge resolves it through
the one accessor over the one store it is written into
(`create.BeforeWSMergePromptFor`, reached as
`WorkspaceCreationBridge.BeforeWSMergePrompt`). A copy here was a duplicated
creation-time fact with one writer and no reader, which is how a writer and a
reader came to name two different columns in the first place.

The `ALTER TABLE ... ADD COLUMN before_action` migration STAYS: databases in the
field already have the column and it is `NOT NULL`, so dropping the migration
would leave fresh and existing databases with two different table shapes. It is
additive and idempotent, and its `DEFAULT ''` is what lets the writes omit the
column. Do not re-add a `geometry.Record` field for it.

## Naming

Always write the package-qualified form — `geometry.Store`, `geometry.Record`,
`geometry.Deriver`, `geometry.Backfiller` — in code comments, commit messages,
and prose, the same rule `internal/workspace/merge/AGENTS.md` states for its
own types.

Dependencies: `daemon/internal/statedb/`, `daemon/internal/dlog/`, git.
