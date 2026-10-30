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

## Landed changes
