# A workspace's source and its context

Owner request, 2026-10-15: settle the terms and the modes before any change to
`agentrepl.v1.CreateWorkspaceRequest`, and ask the agent designing it
(agent-repl-skill-migration) for its opinion on which modes to support. The
owner answers in that workspace.

## Terms

- **SOURCE**: the branch a workspace branches from, and merges back into. The
  two are always the same branch.
- **CONTEXT**: the conversation a workspace starts with: a fresh one, or a fork
  of some workspace's transcript.
- **REQUESTER**: the workspace whose agent (or whose user) asks for the create.

Source and context are independent axes. The owner's principles about their
defaults:

- The source is almost always the main branch. Only an explicitly requested
  CHILD workspace takes the requester's branch as its source.
- The context is almost always a fork of the requester's own transcript. A
  fresh context is the rare case.
- So in practice the two are inversely related: the common create is main
  source with a forked context, which today's defaults make the hardest to ask
  for.

## The modes

Rows are the source; columns are the context. Each cell gives a proposed name,
whether it is supported today (from agent-repl-skill-migration's report of
2026-10-15), and whether we want it (open, for the owner's ruling).

| source \ context | fresh | fork of the requester | fork of another workspace X |
|---|---|---|---|
| **main** | name: fresh workspace. Today: yes (`SPC TAB n/N`, `--master`, one-shot). Want: yes (rare). | name: fork. Today: partly (`SPC TAB f`; the async merge workspace); the skill cannot ask for it. Want: yes, the DEFAULT. | name: fork of X. Today: RPC only (`fork_source`), unverified. Want: open. |
| **the requester's branch** | name: child. Today: yes (the skill's default from any workspace that is not on main). Want: yes, explicit opt-in. | name: forked child. Today: yes (`SPC TAB c/C`, `CreateWorkspaceParent.fork`). Want: yes, explicit opt-in. | name: child forked from X. Today: no (a fork must come from the parent). Want: open. |
| **another workspace X's branch** | name: child of X. Today: RPC (`parent` = X), not from the skill. Want: open. | name: child of X, forked from the requester. Today: no. Want: open. | name: forked child of X. Today: yes through the RPC (`parent` = X with `fork`). Want: open. |

## Questions for the designing agent

1. Which cells should the API support, and which should it refuse? Give the
   reason for each refusal (a real invariant, not cost).
2. Are the proposed names right? A name should say both axes or be the agreed
   default.
3. What are the defaults per entry point (the skill, `SPC TAB`, the webapp form,
   the async merge workspace)? The owner's principles above say: source main,
   context fork of the requester.
4. Which invariants change? For example the merge target is always the source,
   a fork child's inherited priority, and the merge workspace's `MergeOf` check.
