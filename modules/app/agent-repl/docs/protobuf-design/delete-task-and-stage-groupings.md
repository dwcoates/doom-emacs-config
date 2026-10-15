# Delete the task and stage groupings of the sidebar roster

The webhost's sidebar rail drew every grouping (repository, task, maturity)
on every roster push, each with its own copy of the hidden and recently-merged
bands: 598 rows and 28k elements rebuilt per push (2026-10-15, perf
investigation on the agent-repl-perf-regression branch). The owner ruled that
the task and stage (maturity) groupings are not wanted at all, and that the
rail shows only open workspaces, hidden workspaces and the 100 most recently
merged workspaces, the merged ones static.

## Core design principles

### The rail shows open workspaces, hidden workspaces and the 100 most recent merges, and nothing else

- In the owner's terms (2026-10-15): "It's a leak if it's showing anything
  other than open workspaces, hidden workspaces, and the top 100 recently
  merged workspaces, and it shouldn't be really doing any work whatsoever for
  the recently merged ones, because they should be static."
- Consequences for the contract: the roster carries one grouping (the
  repository grouping) and so no "which grouping is shown" choice; the
  recently-merged band is the 100 newest merges, with no time window.
- Reopens: `frontend.v1.WorkspaceRoster.task`, `.maturity` and the `shown`
  oneof; `agentrepl.v1.UpdateSidebarViewRequest.show_grouping`; the task and
  maturity arms of `agentrepl.v1.SidebarViewFoldSection.section`; the week
  window of the recently-merged band (owner request 2026-10-11, superseded).
- Does not claim: anything about the task feature itself (creating, renaming,
  completing and assigning tasks) beyond its grouping; those are listed below
  for the owner's ruling.

## Landed changes

### 1. The task and stage groupings are deleted (owner ruling, 2026-10-15)

- WHAT: `frontend.v1.WorkspaceRoster` loses `task` (2), `maturity` (7) and the
  `shown` oneof (5, 6, 8); the repository grouping is the one grouping.
  Deleted with them: `RosterShownRepository`, `RosterShownTask`,
  `RosterShownMaturity`, `RosterTaskView`, `RosterMaturityView`,
  `RosterTaskSection` and its fold arms, `RosterTaskKey`,
  `RosterTaskSectionHeader`, `RosterTaskDone`, `RosterMaturitySection` and its
  fold arms, `RosterMaturityKey`. `agentrepl.v1.UpdateSidebarViewRequest` loses
  `show_grouping` and `SidebarViewShowGrouping` with its three grouping arms;
  `SidebarViewFoldSection` loses its `task` and `maturity` arms;
  `UpdateSidebarViewError` loses `unknown_task`.
- WHY: the owner does not want the groupings, and drawing them tripled the
  rail.
- CONSEQUENCES: every field number removed is reserved, with its name. The
  daemon's grouping view state (the durable "shown grouping" and the task and
  stage folds) has no reader or writer left. The rail's grouping picker goes.
  `RosterOpenCount` is now the repository view's count only.

### 2. The recently-merged band is the 100 newest merges (owner ruling, 2026-10-15)

- WHAT: the band lists the 100 most recently merged workspaces, newest first;
  the 7-day window and the re-render armed for a merge aging out of it are
  retired.
- WHY: the owner's principle above; a week of merges was 178 rows.
- CONSEQUENCES: a merge leaves the band when a 101st newer one lands, which a
  roster push already carries, so no timer exists.

## Open for the owner's ruling: the task feature beyond its grouping

The task grouping was the task feature's only drawn surface. Still in the
contract, now with no drawn surface:

- `agentrepl.v1.CreateTask`, `agentrepl.v1.UpdateTask`,
  `agentrepl.v1.AssignWorkspaceTask` and `agentrepl.v1.TaskRef`.
- The workspace's task assignment in the daemon's registry.
- `workspace.v1.MaturityStage`, if nothing but the maturity grouping reads it.
