# Workspace relationship: child or peer, separate from where a workspace lands

## Problem

Today "child" means only where a workspace's work lands: `-topology child -owner WS`
cuts it from WS's branch and lands it into WS, and `-topology primary` cuts it
from and lands it on the main branch. Whether the creating workspace and the
created one keep coordinating is recorded nowhere, so a workspace created by
another to work on part of its work (for example `agent-slack-thread-task`) can
end up as an unrelated top-level workspace with no recorded creator.

The user wants the hierarchy to mean the working relationship:

- a **child** works on part of its creator's work and keeps corresponding and
  coordinating with it;
- a **peer** is tangential and self-sufficient, the fire-and-forget kind.

Where a workspace lands becomes a second, independent setting.

## Settled before design (from the conversation, 2026-10-30)

- The relationship (child or peer) and the landing target are two separate
  create settings, replacing the single topology choice. The conversation
  context (fresh or fork) stays a third, independent setting.
- The name of the fire-and-forget relationship is "peer".
- A child gets the coordination wiring automatically: the parent link is
  recorded, it nests under its parent (under a repository divider when it is
  in another repository, as `agent-repl-ack-emoji` landed), the parent is
  subscribed to its turns, questions, landing and close, the parent reviews and
  closes it, and a workspace's status lists its children.
- A peer sits at the top level with no subscription and is the natural
  one-shot. Only who created it is recorded, for diagnosis.
- A child lands into its parent by default. Landing on the main branch is the
  explicit opt-out (a parallel piece the parent waits on, or a child in another
  repository, which can only land on its own repository's main branch).
- A child's landing target may be a workspace other than its parent (a fix sent
  into another workspace's unlanded branch, coordinated by its creator).
- Finishing or closing a parent is refused while any of its children is open.
  The user or the agent accounts for every child first; children are never
  silently orphaned. (Existing evidence, tier: declared types and code:
  `agentrepl.v1.WorkspaceHasOpenChildren` in `family.proto` already refuses
  hide, close, kill, nuke, and a merge that closes the parent while a child is
  open.)
- The daemon's finish verb refuses a parent's deploy until every child landing
  into it has landed.
- A child that lands on the main branch leaves its parent to pick up the main
  branch before relying on that work.
- Sequencing: `agent-repl-ack-emoji` (cross-repository parents, the repository
  divider, derived cross-repository landing) landed on master first; this
  change is built on top of it. Its derived landing target (same repository ->
  the parent's branch, another repository -> its own main branch) becomes the
  default of the explicit landing setting, and `MergeTarget.CrossRepository`
  plus the "parent and merge target are one fact" comment in the daemon's
  `wsm/parent.go` are replaced by a stored landing target separate from the
  parent.

## Core design principles

- **The orchestrator owns the design end to end (user, 2026-10-30).** "You take
  the whole implementation e2e, don't ask me for any feedback on design."
  Consequence: no increment of this change is put to the user for agreement;
  the orchestrator decides each shape on the settled rulings above and records
  it here. It does not claim the user waived the rulings already made above:
  those bind every shape.
- **A child is coordinated by its parent; a peer stands alone.** Consequence:
  the parent link drives nesting, the parent's implicit following, ownership
  and the family refusals; landing is a separate fact. It does not claim a
  child must land into its parent: landing defaults there and is overridable.

## Landed changes

### 1. Relationship and landing target replace the topology (agentrepl.v1)

WHAT. `WorkspaceTopology` (one oneof naming source branch and merge target, with
the child arm also naming the owner) is removed from `workspace_topology.proto`
and replaced by three messages: `WorkspaceRelationship` (child{parent} | peer),
`WorkspaceLandingTarget` (primary{from_origin} | workspace{workspace}), and
`WorkspaceOwnerNotice`. `CreateWorkspaceStandard` and `CreateWorkspaceOneShot`
retire tag 5 `topology` and carry `relationship` (required), `landing_target`
(optional; unset = the relationship's default), and, on the standard form,
`owner_notice`. `CreateWorkspaceError` gains `unknown_landing_target` (46).
`SetWorkspaceParentRequest` gains the `unchanged` arm and an optional
`landing_target`; its error gains `unknown_landing_target`.
`WorkspaceHasOpenChildren` gains `landing_into`. `WorkspaceStatus` gains
`relationship`, `owner`, `created_by` and `children`; its merge target's `owner`
arm becomes `workspace` (`WorkspaceStatusMergeTargetWorkspace`, retiring
`owner_lands`, which moved to `WorkspaceStatus.owner`).

WHY. The user's ruling that relationship (coordination) and landing are
independent. Landing keeps ONE arm for source and target together, so the old
ownership rule (cut from what you land into) stays structural.

DECISIONS AND WHY ALTERNATIVES LOST.
- OWNER is the child's parent, and only that. A peer has no owner and always
  lands itself. Owner-lands (only the owner may land) = a child whose context
  was not forked from its parent. Alternative "owner = landing workspace" lost
  because a cross-repository child (landing on its own main branch) is still
  owned and landed by its parent, which `agent-repl-ack-emoji` shipped and the
  skill relies on.
  - CORRECTION (same day): the first landing of this entry also made the
    workspace a peer lands into its owner. Retracted before any code used it:
    the workspace a branch lands into is read back from the stored
    `creation_jobs.target_dir` (one fact, no second column to drift), and that
    row is keyed to the new workspace's id only AFTER registration, so an
    owner derived from it would be absent in the window between registering and
    re-keying, and the `owner_lands`-needs-an-owner decode invariant would
    fail on a read in that window. Root cause: deriving an invariant-bearing
    fact from a row written in a later transaction. The workspace landed into
    may still LAND the work (it may act on it), but it does not own it.
- The landing default lives in the daemon, not the CLI, so every client (Emacs,
  webapp drag-to-reparent, CLI) gets the same rule.
- `SetWorkspaceParent` keeps its name and gains `unchanged` rather than a new
  `SetWorkspaceLandingTarget` rpc: a retarget of either fact must queue ONE
  notice to the workspace (the retarget package's one-operation guarantee), and
  the default landing follows the relationship, so the two facts are set
  together. Elisp and the webapp call it with the parent arms and no landing
  target, which keeps their behavior (default landing re-applied).
- The child's implicit following by its parent is NOT a stored subscription
  row: it is derived at delivery time from `parent_id`, so a reparent can never
  leave a stale follow behind (structurally impossible, versus kept in sync).

CONSEQUENCES (accepted).
- Every client building a create request changes (CLI, integration harness,
  e2e tests, the elisp fake daemon). Elisp itself builds no create topology.
- A workspace landing into a non-parent workspace blocks that workspace's close
  and finish exactly as a child does (`landing_into`).
