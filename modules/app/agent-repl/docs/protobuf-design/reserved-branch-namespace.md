# The daemon's reserved branch namespace

The merge queue's standing deploy-check workspaces were made under fixed names (`deploy-intake`, `deploy-fixer`), and a workspace's branch is its name. A branch cut by hand under one of those names refused the standing intake's create (`name_branch_exists`) and held the whole merge queue (2026-11-17). The daemon now makes every workspace it owns under the branch namespace `agent-repl/`, and the naming rule refuses every other create, rename and async merge name there, so the collision cannot happen.

Design mode: autonomous.

## Landed changes

### `agentrepl.v1.CreateWorkspaceInvalidName.rule` gains the `reserved` arm

- WHAT: a fifth arm, `reserved` (`agentrepl.v1.CreateWorkspaceInvalidNameReserved`, empty), set when a supplied name's first `/`-separated component is `agent-repl`. `component` carries that first component.
- WHY: a name in the daemon's namespace is a naming-rule breach, not a held name or an existing branch. It is refused whether or not anything holds it yet, because only the daemon ever makes a branch there.
- Why a rule arm, not a new `CreateWorkspaceError` arm: the naming rule (`daemon.internal.workspace.ValidateSuppliedName()`) is the one check shared by a create's supplied name, a rename (`agentrepl.v1.SetWorkspaceName`) and the `agentrepl.v1.ValidateWorkspaceName` read. A rule arm reaches all three through one wire type, and a client words it from the arm alone.
- Why `agent-repl` alone is reserved too: as a branch, `refs/heads/agent-repl` would block every ref beneath it, so the daemon's own creates would fail.
- Consequences:
  - Async merge names do not go through the naming rule. `daemon.internal.workspace.ClaimMergeName()` answers a reserved name as taken, which the queue refuses as `async_merge_name_taken`.
  - A reserved branch's worktree directory is dot-led (`.agent-repl-standing-deploy-intake`). A git ref component never starts with `.`, so no other branch's directory can match it.
  - Not covered: an operator `AGENT_WORKSPACE_PREFIX` of `agent-repl` would put minted names in the namespace. That prefix is the daemon host's own configuration, not a branch a person or an agent cuts.
  - Consumers adapted: the elisp decoder (`lisp/wire-verbs.el`) and the refusal sentence (`lisp/verbs.el`). The webapp does not read this message.
