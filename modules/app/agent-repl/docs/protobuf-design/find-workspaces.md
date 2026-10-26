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
