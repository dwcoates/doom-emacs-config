# Creating a workspace: relationship and landing target

NOTHING IS IMPLIED. Every create names its repository (`-repository here` or
a main-checkout directory), its relationship, its context, and, when it is
not the relationship's default, its landing target:

- `-relationship` is how the new workspace stands to its creator's work:
  - `child -parent WS` works on part of WS's work and keeps coordinating
    with it. WS is its parent and OWNER: the sidebar nests it under WS (under
    a divider naming its repository when that is not WS's), WS FOLLOWS it
    automatically without `workspace subscribe` (WS is told its questions,
    its landing and its close; NOT its turn ends, see "TURN-END
    SUBSCRIPTIONS" in `subscribe.md`), and WS reviews it and closes
    it. WS cannot be closed, backlogged, finished or merged away while the child
    is open.
  - `peer` is tangential, self-sufficient, fire-and-forget: top level,
    followed by nobody, with no owner, and it always lands itself. The
    natural one-shot. Who created it is still recorded (`createdBy` in its
    status).
- `-lands-into` is the landing target, SEPARATE from the relationship and
  optional. A workspace is always cut from what it lands into.
  - Unset is the relationship's default: a child lands into its parent when
    the parent is of the same repository, and on its own repository's main
    branch otherwise (no branch crosses repositories); a peer lands on main.
  - `-lands-into main` opts out of the default, e.g. a parallel piece the
    parent waits on and picks up from main.
  - `-lands-into WS` lands into any open workspace of the SAME repository,
    e.g. a fix sent into another workspace's unlanded branch.
  - `-origin-master` cuts the workspace from origin's main branch, fetched as
    the create runs, when the work must start from what is pushed rather than
    the local main branch; it goes only with `-lands-into main`.

Choose the combination from what the work needs (every `fork` row is within
one repository, except the move in `reorganize.md`):

| want | relationship | lands into | context | how common |
|---|---|---|---|---|
| a tangential fix landed while this work goes on, a parallel subtask, a second approach | `peer` | main | `fork -fork here` | common |
| a piece of this workspace's unlanded feature that lands back into it | `child -parent here` | here (default) | `fork -fork here` | common |
| a piece of another workspace W's work, landing into W, with W's whole context | `child -parent W` | W (default) | `fork -fork W` | common |
| a fix on main that needs another workspace Y's context | `peer` | main | `fork -fork Y` | uncommon |
| a truly tangential bug, or a review that must not be biased by this context | `peer` | main | `fresh` | situational |
| fresh work on W's unlanded branch | `child -parent W` | W (default) | `fresh` | situational |
| a piece this workspace waits on and picks up from main | `child -parent here` | `-lands-into main` | `fork -fork here` or `fresh` | situational |
| coordinated work in a different repository (`-repository DIR`) | `child -parent here` | its main (default) | `fresh` | situational |
| separate work in a different repository | `peer` | its main | `fresh` | situational |

- Rework or a fix to a change another workspace made is never a created
  workspace; it is sent to that workspace with `workspace prompt`
  (reopened with `workspace open` first if closed), per the metaprompt's
  "Rework" section.
- EVERY WORKSPACE LANDS ITSELF, whatever its relationship and context,
  one-shot or not: a child still nests under its parent and is followed by
  it, and its parent (or the user) may also land it with
  `workspace finished CHILD`. Any other workspace is refused
  (`requester_may_not_land`).
