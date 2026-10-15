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

Scope: THE AGENT API only, which is what an agent can ask for: the
`/create-or-update-workspace` command file and `/merge-queue`. The editor's and
the webapp's create commands are out of scope here.

Each cell says whether the agent API supports it today (from
agent-repl-skill-migration's report of 2026-10-15). "Another branch" in both of
the first two dimensions means the same other branch; from one other branch
into a different one is not supported.

Fresh context:

| branches from \ merges into | main | another branch |
|---|---|---|
| **main** | yes (`create --master`) | no |
| **another branch** | no | yes (a child: `create`'s default from a requester not on main) |

Forked context:

| branches from \ merges into | main | another branch |
|---|---|---|
| **main** | no (`fork_from` forces the forked workspace as parent and refuses a base) | no |
| **another branch** | only as `/merge-queue` async's own merge workspace, never as a create | yes (`create` with `fork_from`: a forked child of the forked workspace) |

The "no" cells are refused by the contract's rule that a workspace is never cut
from one branch and merged into another, except "forked, main into main", which
is refused only because the command file couples a fork to its parent.

## Questions for the designing agent

1. Which cells should the API support, and which should it refuse? Give the
   reason for each refusal (a real invariant, not cost).
2. What should each supported cell be called?
3. What are the agent API's defaults (`create`, and `/merge-queue` async's merge
   workspace)? The owner's principles above say: branch from and merge into
   main, with the context forked from the requester.
4. Which invariants change? For example the merge target is always the source,
   a fork child's inherited priority, and the merge workspace's `MergeOf` check.
