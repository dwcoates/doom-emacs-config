# Registering a repository

Use it when work is in a repository agent-repl does not know yet, BEFORE
creating workspaces there with `-repository DIR`.

- `agent-repld repository register here|PATH`: PATH is any path inside a git
  repository (relative paths count from the cwd); `here` is the cwd, which
  need NOT be an agent-repl workspace.
- One unary call to the daemon's RegisterRepository: it resolves the
  repository's main worktree through git and is idempotent by that dir
  (success prints `already_known: true` when it was already registered). It
  also opens the main worktree as a workspace (`workspace_already_known`).
- Output is protojson. Refusals print an arm and exit 6: `not_in_a_repository`,
  `unreadable_path`, `inside_temporary_directory` (a path under the temp root).
- `-dry-run` prints the request without sending it.
- agent-repl's own repository never needs registering: every daemon boot
  registers it, with its main worktree open, when the registry does not hold
  it, so a first start opens on it.
