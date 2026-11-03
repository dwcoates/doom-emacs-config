# FindWorkspaces: finding workspaces, open or closed

The owner asked (2026-10-26) that `agent-repld` and its harness support finding
workspaces, including ones no longer open: a list of workspaces, narrowed by a
specification such as a timeframe or a repository, each with its name and its
summary (the line the webapp's topbar draws as the title). The roster only
carries open, hidden and recently merged workspaces, so nothing an agent could
call answered "which workspace did X last week".

The owner pre-approved contract changes for this work (standing ruling,
2026-10-13: decide and carry proto changes through, report after), so the
shapes below were decided in-session rather than iterated with the owner.

## Landed changes

### `agentrepl.v1.FindWorkspaces`, an AGENT API read

- WHAT: a new unary rpc in the AGENT API section of `service.proto`, declared
  in `endpoint_find_workspaces.proto`, marked `NO_SIDE_EFFECTS` so
  `agent-repld -dry-run` lets it through. The request narrows by repository,
  activity window, standing (open, closed, landed) and case-insensitive text
  over name, branch and summary, with an optional limit. The answer is the
  matches, most recently active first, each with its ref, name, branch,
  repository, summary, creation and last-active instants, and standing.
- WHY: closed workspaces stay in the WSM registry (`workspaces` rows with
  `closed = 1`, 833 of them on the owner's machine on 2026-10-26) together
  with their persisted summaries (`workspace_summaries`, 373 rows), so the
  facts exist; only a read was missing.
- CONSEQUENCES:
  - Zettel chat threads and merge workspaces are registry rows too, and are
    deliberately not answered: neither is a piece of work an agent looks for.
  - The summary is the persisted synthesized summary only. The topbar also
    falls back to the vendor's ai-title while no distillation has landed;
    that title lives only in a live session's state and is not persisted, so
    a found workspace with no distilled summary has its summary UNSET.
  - LAST ACTIVE is derived, not stored: the latest of creation, the last turn
    edge (`last_activity_at`), the close instant and the landing instant. The
    window is an overlap test against `[created_at, last_active]`.
  - Completeness is the `extent` oneof (complete, partial with `omitted`), so
    a caller never compares counts to learn whether `limit` cut it short.
  - Finding nothing is a complete extent with no workspaces, never an error;
    the only error arm is an unknown repository ref.
  - Implementation: `daemon/internal/workspacefind` is the pure filter, fed by
    `wsm.ListWorkspaces`, `wsm.ListRepositories` and the new
    `wsm.ListWorkspaceSummaries`; `daemon/internal/server/findworkspaces.go`
    serves it; `agent-repld workspace find` calls it.

### Login by account root, and the turn filter (2026-11-24)

- WHAT: `FindWorkspacesLogin` gains a second arm, `account_root`, naming a
  login by its Claude config dir rather than by one of its workspaces
  (`of_workspace`). `FindWorkspacesRequest.turned`
  (`FindWorkspacesTurnWindow`) keeps only workspaces that had an agent turn
  starting inside the window. `FoundWorkspace` gains `config_dir` and
  `last_turn_at_ms`. A root the daemon does not know is the new
  `unknown_login_root` arm, which carries the roots it does know.
- WHY: the explanation-engine `/standup` skill asks only the workspaces of
  the work login that actually worked inside the standup window, and names
  the login by its root because it runs outside every work workspace.
  `window` matches every kind of activity (a close, a landing, a turn's
  end).
- CONSEQUENCES:
  - ONE LOGIN FILTER. A first cut (never landed) added a separate
    `config_dir` filter matching each session's own config dir, while master
    gained `login`, which matches by repository routing plus children. Two
    filters with two meanings of "login" would answer different sets for the
    same account, so the root became an arm of `login` and takes its meaning:
    the repositories `AccountRootFor` routes to that root, plus their
    workspaces' children.
  - `FoundWorkspace.config_dir` is the session's own config dir, where it
    last came up. It is a fact of the session and is NOT what the login
    filter matches, so a child in an unrouted repository is found by its
    parent's login while answering its own session's root.
  - The root is compared verbatim, as SelectAccount compares it; the CLI
    expands a leading `~/`.
  - An agent turn is a `turns` row whose origin set the agent working and
    whose text is not a session command the daemon acts on itself. Restart
    re-drives (`RESUME_AFTER_RESTART`, `MERGE_DISPLACED_TURN_RESUME`) and
    compactions (`WORKSPACE_COMPACT`, `MERGE_COMPACT_AFTER`, a typed
    `/compact` or `/clear`) are not. Gate answers, renames and status
    changes write no turn at all.
  - A turn window with neither end set is accepted, and matches every
    workspace that ever took an agent turn, as an empty `window` is.
