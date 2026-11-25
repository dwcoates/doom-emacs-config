# Finding a workspace's children, and how each ended (2026-11-25)

Designed autonomously, under the owner's ruling of 2026-11-25. Any workspace
reads its children with `agentrepld workspace find`, along with each child's
status and, for one that ended, how and when it ended. The production
mediator's brief lists the children that were open during the span it checks,
so it never dispatches a fix again for work a child already did.

## Landed

`agentrepl.v1.FindWorkspacesRequest` (`endpoint_find_workspaces.proto`):

- `parent = 11` keeps only that workspace's children.
  - A ref the registry does not hold is the new
    `agentrepl.v1.FindWorkspacesError.unknown_parent` arm (6), as
    `created_by`'s `unknown_creator` is.
- `ended_since_ms = 12` keeps only workspaces that had not ended by then: those
  open now, and those that landed or closed at or after it.
  - A close with no recorded instant counts as ended before any instant.
  - It is a filter of its own, not part of `window`. `window` matches when a
    workspace was last active, and a child that landed long after its last
    turn would fall outside it.

`agentrepl.v1.FoundWorkspaceStandingClosed.how = 2` is a
`agentrepl.v1.FoundWorkspaceClosedHow`, whose arm is how the workspace ended:

- `closed`
- `killed`
- `folded`, naming the child it went into
- `unrecorded`

A landed workspace keeps its own `landed` standing. A nuked one is never
found, because its record is gone.

## Decisions

- **Closes record their kind in the store.** A close used to tell its kind to
  the workspace's followers and then drop it. It is now stored in
  `workspaces.closure_how` (layout 214), so a find can tell a kill apart from a
  close. A close recorded before this layout reads `unrecorded`, rather than a
  guessed kind.
- **A fold is the production mediator's record.** The child a folded child
  went into is stored in `production_children.folded_into`. The find answers
  `folded` for any workspace that table names, whatever kind its close
  records.
- **The brief is keyed by the span's start time, not a commit.** Spans are
  already defined by when landings began. A commit would first have to be
  looked up as its landing's time, adding a step and nothing more.
