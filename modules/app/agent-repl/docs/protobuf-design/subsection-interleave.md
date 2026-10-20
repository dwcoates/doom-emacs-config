# A subsection stands anywhere among its repository's workspaces

Owner request, 2026-10-20: "subsections within a repository section of the sidebar's OPEN section should be drag-and-drop orderable within that repository". The owner then gave an example: with workspaces A, B and C and a subsection foo holding D and E at the bottom, dragging foo onto A's line draws A, then foo with D and E, then B and C. The owner also asked that the subsection a dragged workspace is over be highlighted, and that a subsection's workspaces be indented.

Before this change, a repository section could only say "the rows, then the subsections". Its rows and its subsections were two lists, and the store kept the subsections' order apart from the workspaces' order.

Contract changes are pre-approved by the owner (2026-10-13 ruling: decide and carry through, report after), so this change was landed without the per-increment selections.

## Landed changes

1. `frontend.v1.RosterRepoSection` retires `open_rows` (8) and `subsections` (7) for `repeated frontend.v1.RosterRepoEntry entries = 9`. Each `frontend.v1.RosterRepoEntry` is `oneof entry { frontend.v1.RosterOpenRow row; frontend.v1.RosterSubsection subsection }`.
   - Why: the drawn order is the tab bar's order. One list in drawn order means a reader that walks it cannot get the order wrong.
     - Two lists plus a position field would let a reader walk the rows and then the subsections and silently misnumber every tab.
   - Why one entry per row rather than runs of rows: a run would need an empty run between two adjacent subsections to say where a dropped row lands. A row entry is one drawn box, which is the figma-to-IDL rule.
   - Considered and rejected: `uint32 after_rows` on `frontend.v1.RosterSubsection`. A count can be out of range, and the order would still be a fact a reader must assemble.

2. `agentrepl.v1.ArrangeMoveSubsection.to` gains `workspace.v1.WorkspaceRef before_workspace = 4`, which must be a root in no subsection of the same repository.
   - `agentrepl.v1.ArrangePlaceWorkspace.at` and `agentrepl.v1.ArrangePlaceWorkspaces.at` gain `agentrepl.v1.SubsectionRef before_subsection = 6`, legal only into `no_subsection`.
   - `end` now means after every entry of the destination: the bottom of the repository for `no_subsection`.
   - Why: these are the two drops the interleave adds, a subsection onto a row's line and a row just above a subsection.

3. `agentrepl.v1.ArrangedRepository` retires `workspaces` (3) and `subsections` (4) for `repeated agentrepl.v1.ArrangedEntry entries = 5`, for the same reason as change 1.

## Behavior decided with the contract

- `agentrepl.v1.ArrangeDeleteSubsection` now leaves the subsection's workspaces where it stood. Before, they moved to after the repository's other unsectioned workspaces.
- A new subsection still lands at the bottom of its repository. A new workspace lands at its registry place, so it stands wherever that place falls among the subsections.

## Store

- One nullable column, `subsections.slot` (layout 66). A subsection stands just before the first unsectioned root whose registry place is at least its slot, and after every root when the slot is `NULL`.
  - Every existing subsection migrates to `NULL`, which is where it already drew.
- The slot names a point in the order, never a workspace. A root leaving its point (dragged away, closed, filed into a subsection) therefore moves no subsection.
- Every arrangement write goes through `daemon.internal.wsm.readWalk()` and `daemon.internal.wsm.writeWalk()`. The pair permutes only the repository's own roots over the places they already held, then restates each slot. No other repository's places move, so no other repository's slots can go stale.
- `daemon.internal.wsm.shiftFrom()` bumps every slot past a freed place. A landing therefore never moves a root across a subsection.
