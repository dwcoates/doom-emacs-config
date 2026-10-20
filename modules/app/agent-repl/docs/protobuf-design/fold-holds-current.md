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
