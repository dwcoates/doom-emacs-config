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
- **CONTEXT**: fresh, forked from the requester's own transcript, or forked from
  another workspace's transcript. Forking from another workspace is sometimes
  what is wanted, so it is its own value, not a variant of forking from self.

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

Context forked from the requester itself:

| branches from \ merges into | main | another branch |
|---|---|---|
| **main** | no (`fork_from` forces the forked workspace as parent and refuses a base) | no |
| **another branch** | only as `/merge-queue` async's own merge workspace (the requester's branch tip, into its merge target), never as a `create` | yes, the requester's own branch (`create` with `fork_from` naming the requester: a forked child of itself) |

Context forked from another workspace X:

| branches from \ merges into | main | another branch |
|---|---|---|
| **main** | no (same coupling) | no |
| **another branch** | no | yes, but only X's own branch (`create` with `fork_from` naming X: a forked child of X) |

In every forked cell today, "another branch" can only be the forked
workspace's own branch: the command file makes the forked workspace the parent.

The "no" cells are refused by the contract's rule that a workspace is never cut
from one branch and merged into another, except the two forked "main into main"
cells, which are refused only because the command file couples a fork to its
parent.

## The owner's target (2026-10-15)

Fresh context: as today.

Context forked from the requester itself:

| branches from \ merges into | main | another branch |
|---|---|---|
| **main** | YES, the default | no |
| **another branch** | yes (rare) | yes, any branch |

Context forked from another workspace X:

| branches from \ merges into | main | another branch |
|---|---|---|
| **main** | YES | no |
| **another branch** | YES | YES, any branch |

Consequences:

- A fork no longer decides the branch: which workspace's transcript is forked
  is independent of what is branched from and merged into.
- "Another branch into main" is supported in its own right, so the contract's
  rule that a workspace is never cut from one branch and merged into another
  no longer holds as stated.
- "Any branch" (open): whether "another branch into another branch" means the
  same branch it was cut from, or may also mean a different one.

## One fork-source field (owner ruling, 2026-10-15)

Forked from the requester and forked from another workspace have the same
target matrix, so the API does not distinguish them: a create carries ONE
optional fork source, the workspace whose transcript it forks, which may be
the requester itself. Unset is a fresh context. So a create has three
independent fields: branches from, merges into, and an optional fork source.

## The merge queue, one-shot and async, the skill and the API

What "merges into" means for each landing path:

- ONE-SHOT: the workspace merges its own branch, in place, into its merge
  target; the target is fixed when the workspace is created. So a create's
  "merges into" is exactly what a later one-shot lands in.
- ASYNC: a hidden merge workspace lands a named branch. Today that merge
  workspace is itself a create in the "forked from the requester, another
  branch into main" cell: it is cut from the branch's tip, forks the
  requester's context, and merges into the requester's merge target.
- Open for the owner: whether async's merge workspace counts toward "rare" in
  that cell (async merges are common), and whether a merge's target is fixed at
  creation or may be chosen when the merge is asked for (one-shot or async).
- Which of the three fields each landing path has:
  - ONE-SHOT makes no workspace: it lands the requester's own branch in place,
    in the requester's own conversation. Its branch and context are the
    requester's already, so the only field it could take is merges into (if
    the target may be chosen at merge time).
  - ASYNC makes a workspace, so it has all three: branches from (the branch
    being landed, any branch), merges into (its target), and the optional fork
    source (today always the requester). An async merge is then a create with
    those three fields plus "land this when done", and needs no separate shape.
- SKILL AND API: `/create-or-update-workspace` writes a command file the daemon
  reads, and `/merge-queue` enqueues through the same ingress; neither calls the
  daemon's RPCs directly. Every supported cell should be expressible the same
  way at both levels, with the skill a thin caller of the API, so the skill
  cannot refuse a mode the API supports (as the command file's fork-to-parent
  coupling does today).

## Questions for the designing agent

1. Which cells should the API support, and which should it refuse? Give the
   reason for each refusal (a real invariant, not cost).
2. What should each supported cell be called?
3. What are the agent API's defaults (`create`, and `/merge-queue` async's merge
   workspace)? The owner's principles above say: branch from and merge into
   main, with the context forked from the requester.
4. Which invariants change? For example the merge target is always the source,
   a fork child's inherited priority, and the merge workspace's `MergeOf` check.
5. Against the owner's target above: what the agent API (the create skill and
   `/merge-queue`, one-shot and async) must change to support each cell, where
   the merge target is decided, and how the async merge workspace fits.
