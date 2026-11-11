# Create: born hidden, born placed

An intake workspace (a card's `?` question, a Slack emoji ack) must never
appear in the sidebar, and the intake files the workspace it creates into the
right sidebar subsection, so the created workspace lands there at once instead
of filing itself as a preliminary task (owner request, 2026-11-11).

## Assumptions (autonomous design)

- OBJECTIVE: the row of an intake is hidden from the transaction that inserts
  it, and the row of a created workspace is placed in the transaction that
  inserts it.
- WHAT IS BUILT: two optional fields on `agentrepl.v1.CreateWorkspaceRequest`,
  the typed refusals they need, one register transaction in wsm, and three CLI
  flags on `workspace create`.
- FRONTEND: none. The roster's `hidden` and subsection facts already exist.
- NON-ADDITIVE CHANGES: none on the wire. Both fields are new and optional.
  `wsm.MarkIntake` (a second write after the insert) is retired in the daemon.

## Landed changes

### `agentrepl.v1.CreateWorkspaceRequest.hidden`

- WHAT: `optional bool hidden = 15`, presence-based, true = born hidden.
- WHY: a hide written after the insert leaves a window in which a roster
  publication draws the row. The window is the flash. A column written by the
  insert has no window, so the failure is structurally impossible.
- ALTERNATIVES REJECTED: hiding earlier in the same turn (still two writes);
  suppressing the roster until the hide lands (a global lock to defend one
  row).
- CONSEQUENCES: refused as invalid beside `async_merge`, whose workspace the
  queue creates hidden already.

### `agentrepl.v1.CreateWorkspaceRequest.placement`

- WHAT: `optional agentrepl.v1.CreateWorkspacePlacement placement = 16` with a
  `target` oneof: `string subsection` (an existing subsection of the
  repository, by name) or `agentrepl.v1.CreateWorkspaceNewSubsection
  new_subsection` (`name`, `repeated string gather`).
- WHY: the creator knows where the workspace belongs. Asking the created
  workspace to file itself costs it a first turn and shows it unsectioned
  until then.
- CONSEQUENCES:
  - The register transaction writes the assignment, or the new subsection and
    the gathered moves, so the row is never seen unplaced.
  - A create with a placement does not add the subsection placement note to
    the first message.
  - A new subsection is agent-created (`created_by` is the requester), so it
    is pruned when no open workspace sits in it, as every agent subsection is.
  - Gathered workspaces are named, and must be open, unsectioned, top-level
    workspaces of the create's repository and no main workspace.

### Refusals (`agentrepl.v1.CreateWorkspaceError`, tags 55 to 61)

- `placement_on_child` (`agentrepl.v1.CreateWorkspacePlacementOnChild`): the
  create has a parent. A child sits wherever its owner does.
- `placement_on_merge_workspace`
  (`agentrepl.v1.CreateWorkspacePlacementOnMergeWorkspace`): a placement beside
  `async_merge`.
- `placement_requires_workspace`
  (`agentrepl.v1.CreateWorkspacePlacementRequiresWorkspace`): the requester is
  the user, who has no agent to own a new subsection.
- `unknown_placement_subsection`
  (`agentrepl.v1.CreateWorkspaceUnknownPlacementSubsection`): no such
  subsection in the repository.
- `blank_placement_subsection_name`
  (`agentrepl.v1.CreateWorkspaceBlankPlacementSubsectionName`).
- `gather_unknown_workspace` (`agentrepl.v1.CreateWorkspaceGatherUnknownWorkspace`):
  a gathered name no workspace of the repository carries.
- `gather_not_gatherable` (`agentrepl.v1.CreateWorkspaceGatherNotGatherable`):
  closed, a child, main, a merge workspace, already in a subsection, or named
  twice. Its `detail` says which.
- Every refusal is an answer of the caller's own input: nothing is created.
  The checks run before the worktree is made, and the register transaction
  repeats them, so a state change in between refuses the register and the
  create unwinds as any failed create does.
