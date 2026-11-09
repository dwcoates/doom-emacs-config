# `workspace list` answers the FindWorkspaces record

The owner wants `agentrepld workspace list` to return each workspace's summary
(the purple topbar title) so workspaces understand each other better. It does so
by emitting the SAME record `workspace find` answers (agentrepl.v1.FoundWorkspace),
one shared shape, and it must lose no information: every fact a `list` line
carried (name, id, dir, live status, section, calling) is still answered.

Mode: autonomous (owner pre-approved the proto change).

## Assumptions (step 1)

- Objective: one workspace record for `list` and `find`, gaining summary,
  branch, repository, standing and timestamps for `list`.
- Systems: the daemon (FindWorkspaces handler, `agent-repld workspace list` and
  `workspace find` CLI). No frontend component: the record is an agent-API
  answer, not a drawn view, so figma→idl does not govern it.
- Non-additive changes: `workspace list`'s output changes shape from its
  hand-rolled `listedWorkspace` JSON to protojson FoundWorkspace lines. Every
  consumer of the old keys is updated in the same change.
- Before this change `list` read the roster client-side and `find` read the
  registry server-side; the roster excludes chat threads and hidden merge
  workspaces, and so does find (verified: daemon/internal/resolve/sidebar/resolver.go
  sorts both out of every section; workspacefind answers agent workspaces only).
  So `list`'s set (open, hidden, recently merged roster rows) is a subset of
  find's, and an `on_roster` criterion reproduces it exactly.

## Iteration sequence

One increment: FindWorkspaces request and FoundWorkspace record, landed
together because they share the roster facts.

## Landed changes

### FindWorkspaces gains on_roster and caller; FoundWorkspace gains roster and calling

- WHAT: FindWorkspacesRequest.on_roster (FindWorkspacesOnRoster, presence is
  the criterion) and FindWorkspacesRequest.caller (WorkspaceRef). FoundWorkspace
  .roster (FoundWorkspaceRoster: a section oneof open/hidden/merged and a
  FoundWorkspaceRosterStatus) and FoundWorkspace.calling (bool). A new error
  arm unknown_caller.
- WHY: the owner's no-information-loss rule. `status`, `section` and `calling`
  were the three facts the record lacked.
- Status is the proto field NAME of frontend.v1.RosterRow.status's arm, in a
  dedicated wrapper. Alternatives rejected:
  - Re-declaring the ~30 status arms as a second oneof: a vocabulary respelling
    with no mechanism that detects drift; a new roster arm would go unanswered.
  - Embedding frontend.v1.RosterRow whole: the encompassing message, but a
    drawn sidebar row of ~40 view props, which makes every `list` line
    kilobytes long for agents that want one word, and puts a figma→idl view
    into an agent-API record.
  - Accepted cost: the arm name is a string; a test asserts every answered name
    is a real arm of frontend.v1.RosterRow.status.
- Section covers the three roster sections `list` printed. The roster's closed
  list is not a section here: a closed workspace's standing already says it is
  closed, and `list` never printed those rows.
- calling is request-relative: set only for the record of the request's
  caller. Both CLI verbs pass the caller when the shell sits inside a
  workspace, so `find` gains `calling` too.
- Consequences:
  - The daemon reads the latest published roster
    (sidebar.Resolver.Topic().Latest()) in FindWorkspaces. No published roster
    yet fails the call (Internal) rather than answering records with no roster,
    which would read as "drawn nowhere".
  - `list` orders records as find does, most recently active first, not in
    sidebar draw order. The section arm still says where each is drawn.
  - `list`'s JSON keys change: `.name`, `.workspace.id`, `.workspace.dir`,
    `.roster.status.arm`, `.roster.section` (an arm object), `.calling`
    (omitted when false, as before). Consumers updated in the same change.

## Sweep

Obviated: the CLI's `listedWorkspace` struct and `rowStatus` (deleted with the
change). Nothing in the proto package is obviated.
