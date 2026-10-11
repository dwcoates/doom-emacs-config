# Workspace place swap

The owner wants `SPC TAB p` (`agent-repl-workspace-place`) to read one digit
0-9 without a return, naming a drawn tab slot (0 = slot 10), and swap that
workspace's roster place with the current workspace's, so both the tab bar and
the sidebar draw the two swapped. Roster order today is computed by the
daemon's resolver (priority, then name, then id) and nothing stores a place,
so a swap needs a stored place and a daemon rpc to exchange two of them.

## Core design principles

### A stored place orders the roster; priority only picks where a workspace lands

- **Principle (owner, 2026-10-11):** priority only controls where a new or
  re-opened workspace lands by default in the tab-bar and sidebar order; the
  user may reorder freely so that workspaces of one priority need not be
  contiguous.
- **Consequences:** every workspace carries a persisted place; roster order
  among siblings is place order; creating or re-opening a workspace assigns a
  place derived from its priority; setting a priority later does not move the
  workspace.
- **Reopens:** `claude-repld.internal.resolve.sidebar.lessWorkspace()`'s
  priority-then-name order, and the `SetWorkspacePriority` comment "the roster
  orders by it".
- **Does not claim:** that priority is retired; it still draws its badge and
  still decides the landing place.

### Swaps happen only between siblings, and the tab bar stays the sidebar's walk

- **Principle (owner, 2026-10-11):** a swap between workspaces with different
  parents is refused; tabs keep following the sidebar's depth-first walk.
- **Consequences:** one stored place serves both surfaces; the daemon refuses
  a swap of two workspaces that are not siblings in the repository view.
- **Does not claim:** anything about the recently-merged section's order,
  which stays merge-time ordered.

### Digit 0 names slot 10

- Owner ruling, 2026-10-11: the digit prompt maps 1-9 to slots 1-9 and 0 to
  the tenth drawn tab.

## Landed changes
