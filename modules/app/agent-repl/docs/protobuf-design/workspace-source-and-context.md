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

Three binary dimensions, each against the main branch:

- **BRANCHES FROM**: main, or another branch.
- **MERGES INTO**: main, or another branch.
- **CONTEXT**: fresh, or forked from another workspace's transcript.

Each cell says whether it is supported today (from agent-repl-skill-migration's
report of 2026-10-15). "Another branch" in both of the first two dimensions
means the same other branch; from one other branch into a different one is not
supported anywhere.

Fresh context:

| branches from \ merges into | main | another branch |
|---|---|---|
| **main** | yes (`SPC TAB n/N`, `--master`, a one-shot) | no |
| **another branch** | no | yes (a child: the skill's default from any workspace not on main) |

Forked context:

| branches from \ merges into | main | another branch |
|---|---|---|
| **main** | partly (`SPC TAB f`; the skill cannot ask for it) | no |
| **another branch** | internal only (the async merge workspace: the branch's tip, into the requester's target, forked from the requester) | yes (a forked child, `SPC TAB c/C`; the fork must come from that same branch's workspace) |

The "no" cells are refused by the contract's rule that a workspace is never cut
from one branch and merged into another; the async merge workspace is the one
exception, and it is internal.

## Questions for the designing agent

1. Which cells should the API support, and which should it refuse? Give the
   reason for each refusal (a real invariant, not cost).
2. What should each supported cell be called?
3. What are the defaults per entry point (the skill, `SPC TAB`, the webapp form,
   the async merge workspace)? The owner's principles above say: source main,
   context fork of the requester.
4. Which invariants change? For example the merge target is always the source,
   a fork child's inherited priority, and the merge workspace's `MergeOf` check.
