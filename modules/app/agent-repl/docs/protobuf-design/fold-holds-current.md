# A section holding the current workspace never collapses

Owner request, 2026-10-20: "clicking a repository in the OPEN section should NOT close/fold the repository if you're in a workspace in that repository. folding of repos should only work if the current workspace is not in the corresponding repo."

Collapsing a repository's section (or a subsection's) hides its workspaces' tabs and stands their sessions down, so a collapse of the section the current workspace sits in would stand down the session the user is looking at. The fold is the daemon's view state, so the rule is the daemon's too.

Contract changes are pre-approved by the owner (2026-10-13 ruling: decide and carry through, report after), so this change was landed without the per-increment selections.

## Landed changes

1. `frontend.v1.RosterRepoSectionExpanded` and `frontend.v1.RosterSubsectionExpanded` each gain `oneof collapse { collapsible; holds_current }`, with their own empty arm messages per component.
   - Why: the page must know before it asks, or its instant fold paints a collapse and then reverts on the refusal.
   - The daemon resolves the arm on every push from its own selection, so it follows every switch without a page computing anything.
   - A repository holds the current workspace whether the workspace sits in one of its subsections or not. A subsection holds it only when the workspace sits in that subsection.
   - The COLLAPSED arm carries no such fact: expanding is always allowed, including a section the current workspace sits in.
   - Considered and rejected: the page checking whether its section draws a `data-current` row. The highlight can come from the page's own shown-ahead state rather than the wire, and the rule would then live in two places.

2. `agentrepl.v1.UpdateSidebarViewError` gains `holds_current = 4`, and the superseded `agentrepl.v1.FoldRepositoryError` gains `holds_current = 2`.
   - Why: the roster can be a push behind a switch, so the daemon refuses the collapse itself. The roster arm stops the gesture, and the refusal stops a stale page.
   - Consequence: the daemon's one fold path (`daemon/internal/workspace/foldrepository.go`, `foldTabs`) checks the current workspace under the selection lock (`verbs.selection`), the same lock `selectCurrent` holds. A switch racing the collapse is then wholly before or wholly after the check.
   - Consequence: the rule covers subsections as well as repositories. The owner asked about repositories, but the two folds share one path and one effect (tabs hidden, sessions stood down).

## Update (2026-11-09): a collapse lands the user

The arm's meaning narrowed; its shape did not change.

- `holds_current` now says the current workspace has NOWHERE ELSE TO LAND: no
  other open workspace is drawn outside the section (`sidebar.LandingTarget`,
  one function the roster and the fold verb both ask). A collapse with
  somewhere to land is recorded, and the daemon selects the workspace the user
  was on before, among the open sections, in the same step
  (`workspace.verbs.landOffFold`, under the selection lock).
- The editor's walking chords, the recency walk and the picker offer the tabs
  of folded sections (`agent-repl-roster-navigable-tab-order`); the numerals
  stay on the drawn tabs.

## Update (2026-11-15): a fold opens for nothing; it draws the working and the selected workspace

A workspace whose turn starts while a fold hides it (its repository's, its
subsection's, or an ancestor's children fold) no longer unfolds the fold
(this replaces the 2026-10-29 unfold, `workspace.verbs.UnfoldSubsectionForWork`).

- The fold stays folded and `workspaces.surfaced` (layout 153) is set
  (`wsm.store.SurfaceHeldWork`, from the turn-start edge, `workspace.NewWorkSurfacings`).
  The roster draws that workspace, and only it, inside the fold: its row carries
  `frontend.v1.RosterOpenRow.surfaced` and a numbered tab, so the editor's bar
  draws it too.
- The mark comes off when the turn ends (`workspace.NewWorkSurfacingEnds`,
  `wsm.store.EndSurfacing`) and when the user folds a section over the
  workspace (`workspace.verbs.recordFold`, `wsm.store.ClearSurfaced`), so a fold
  always hides everything under it, and a turn already running at fold time is
  not drawn.
- Selecting a workspace a fold hides opens nothing either (this replaces the
  2026-10-25 opening and the peek that refolded it). The current workspace is
  drawn inside the fold exactly as a working one is (`wsm.Workspace.DrawnInsideFold`:
  `Surfaced` or `Current`), and is hidden again the moment the user switches
  away, so no mark or refold is stored for a selection.
- The webapp draws no other row of the fold: `.surfaced` in
  `webapp/src/styles.css`, with the rows between the fold and a surfaced
  descendant keeping only their `.kids` box.
