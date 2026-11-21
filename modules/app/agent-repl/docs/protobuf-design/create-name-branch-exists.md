# A supplied create name whose branch already exists

A create that supplies a name (`agentrepl.v1.CreateWorkspaceStandard.name`)
whose branch already exists in the repository, while no registered workspace
that has not landed holds it, reached `git worktree add -b` and failed with
git's raw "a branch named ... already exists" (2026-11-03,
`ramdisk-leak-investigation`: the branch a rebased detached landing left
behind). The daemon now refuses that name itself, typed, before any git runs.

## Assumptions (autonomous design)

- Objective: a stale branch under a supplied name is a typed refusal the
  caller can read, never git's untyped failure.
- No frontend component: the arm is decoded by the editor like every other
  create refusal, and rendered as its sentence.
- Systems: the daemon (raises it), the editor lisp (decodes it), the testrun
  RAM disk investigation (a caller of a fixed name; reads only `name_held`).
- Non-additive changes: none. The arm is new.

## Iteration sequence

One increment: the new `CreateWorkspaceError` arm and its message.

## Landed changes

### `CreateWorkspaceError.name_branch_exists` (tag 51)

- WHAT: a new cause arm, `CreateWorkspaceNameBranchExists`, carrying the
  branch the name resolves to and the commit it points at.
- WHY: the branch exists and nothing the daemon serves owns it, which is a
  different fact from `name_held` (a registered unlanded workspace holds it),
  so it is its own arm rather than a `name_held` with no workspace.
- Consequences:
  - The daemon classifies the arm as NOT the caller's input
    (`daemon/internal/workspace/createfailed.go` `createRefusalArms`): a
    branch nothing owns is repository state the main worktree investigates,
    usually a landed branch the landed-branch sweep kept, whose INFO record
    says why.
  - The arm carries no workspace: none holds the branch, by definition. A
    landed workspace that last named it is not carried, because the daemon's
    log already names the kept branch and its reason.
- Obviated-declaration sweep: nothing found.

### `CreateWorkspaceNameBranchExists.landed_workspace` (tag 3, 2026-11-14)

- WHAT: the landed workspace whose name or branch the existing branch is,
  the latest-landed when several were; unset when no registered workspace
  ever held it.
- WHY: the arm's single fixed classification as a fault sent a branch the
  user cut by hand, under the name they then supplied, to the main worktree
  as a failed create to investigate. Whose branch it is decides that, and the
  refusal is where the daemon reads it from.
- Consequences:
  - Supersedes "the arm carries no workspace" above: the landed workspace is
    carried, because the classification reads it.
  - The daemon classifies the arm by reading the refusal
    (`daemon/internal/workspace/createfailed.go` `createRefusalReaders`): a
    landed workspace set is the sweep's kept branch, a fault the main
    worktree investigates; unset is a branch cut by hand, the caller's own
    input, reported to no one.
  - The editor's decoder (`lisp/wire-verbs.el`) reads it as
    `:landed-workspace`.
- Obviated-declaration sweep: nothing found.
