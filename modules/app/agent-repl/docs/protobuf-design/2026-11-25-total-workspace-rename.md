# A workspace rename is a total rename

The owner ruled (2026-11-25) that renaming a workspace is a total rename: its
previous name becomes available for use again. Before this change,
`agentrepl.v1.SetWorkspaceName` changed the name column alone. The branch and
the worktree directory kept the names the workspace was created with, and
because every registered workspace, closed ones included, holds both its name
and its branch, a renamed workspace blocked its old name for good. The
concrete case was `production-mediator-planning`: renamed from
`production-mediator`, its branch and directory still held
`production-mediator`, so the production mediator could not take that name.

Design mode: autonomous.

## Context (assumptions, derived from the request and the code)

- Objective: a rename moves everything named after the workspace, so its old
  name, branch and directory are held by nothing afterwards.
- What is built: the daemon moves the git branch (`git branch -m`), the
  worktree directory (`git worktree move`), the vendor's transcript folder
  for the directory (`<config root>/projects/<slug of dir>`), and every store
  row that names the old name, branch or directory. Clients (Emacs, the
  webapp) follow a workspace whose directory changed under the same id.
- Frontend presence: none in the contract. The roster already carries each
  workspace's `workspace.v1.WorkspaceRef.dir`, which is display data and not
  identity, so a moved directory reaches every client as a changed field of
  the same row.
- Involved systems: the daemon (workspace verbs, the store, the bounce
  registry and the shim relaunch engine, git, the account transcripts, the
  composer's completion records), Emacs (a tab whose directory changed), the
  webapp (the composer's workspace completions), the CLI's refusal
  describers, and the fakedaemon.
- Non-additive changes: `SetWorkspaceName`'s success now says where the move
  stands, and its refusals gain the cases a move can be refused for. The
  store's `SetName` writes the name alone no longer for a linked worktree.

## Core design principles

- **A live workspace moves only at a point the daemon owns.** The move needs
  the shim down, because the shim runs in the directory and the vendor
  writes its transcript under a folder named for the directory. The move is
  therefore a shim replacement registered with the bounce registry, which
  runs it only once the workspace falls free (no turn, no detached work) and
  drains the workspace's intake while it runs. A rename during a turn is
  answered at once with the move owed; it runs when the turn ends.
  - Consequence: the rename rpc never waits on a turn, and never ends one.
  - It does not claim that a move is instant: until the move runs, the
    workspace's new name stands and its old branch and directory are still
    held by it.
- **An owed move is durable.** It is a store row written with the name, so a
  daemon restart or a handover finishes it: the daemon that comes to serve
  the workspace registers it again. A move that had stood the shim down and
  then crashed is finished before the workspace is brought up, from the row's
  phase, by steps that each check the disk first.

## Landed changes

### 1. SetWorkspaceName moves the branch and the directory, and says where the move stands

- What changed:
  - The file's header now states the total rename.
  - `agentrepl.v1.SetWorkspaceNameSuccess` gains `move`, a oneof of
    `moved` (ran before the answer), `owed` (runs at the workspace's next
    freeness) and `none` (nothing to move).
  - `agentrepl.v1.SetWorkspaceNameError` gains `merging`,
    `branch_published`, `branch_exists` and `directory_exists`.
- Why: the owner's ruling that a rename frees the old name; a move the
  daemon defers needs the caller told it is owed, and the move has refusals
  the name alone never had.
- Why the alternatives lost:
  - Answering only after the move ran would make the rpc wait on a running
    turn, possibly for an hour, and an agent renaming its own workspace
    mid-turn would wait on itself forever.
  - Refusing a rename while a turn runs would make the common case (an agent
    renaming its own workspace) impossible.
  - Moving a branch that tracks a remote would part it from the remote
    branch a pull request names; refusing (`branch_published`) keeps the
    rename from changing anything outside this machine.
  - A merge names the workspace's branch and directory through its whole
    run (its records, its rebase, its worktree removal), so a rename during
    one is refused (`merging`) rather than moved underneath it.
- Consequences:
  - Clients must accept a workspace whose `workspace.v1.WorkspaceRef.dir`
    changes under the same id. The ref's contract already says the dir is
    display data and not identity; Emacs keeps a tab's directory from the
    row that opened it, so it now follows the row's dir on every update.
  - A request echoing the old dir after the move is refused
    `workspace_ref_mismatch`, as any stale ref is.
  - The main checkout keeps name-only renames (`none`).

### 2. A rename during its workspace's own move is refused `moving`

- What changed: `agentrepl.v1.SetWorkspaceNameError` gains `moving`.
- Why: once a move has stood the shim down, its steps (the worktree, the
  branch, the transcript folder) are under way toward one destination, and
  the store completes the move to the destination its row names. Letting a
  second rename change that destination mid-move would record a branch and
  directory the disk does not have. Before the steps begin, a second rename
  simply retargets the owed move.
- Why the alternatives lost: holding the rename rpc until the move ends ties
  the rpc to git and filesystem steps; answering it as an internal failure
  hides an ordinary, momentary state.
- Consequences: the window is the move's own steps, a matter of moments.
