# Supplied workspace names are checked before a create builds anything

A static create (`SPC TAB N`) asks for a name and sends no prompt. On
2026-10-21 a whole prompt was typed into that `Name:` field. Neither the
editor nor the daemon checked a name the user supplied. The daemon accepted
the create, claimed the name, and handed it to `git worktree add`, which
refused it as an invalid branch name. The user got a raw `git` error on the
create's `internal` cause instead of a typed refusal that named the bad name.

## Landed changes

### `agentrepl.v1.CreateWorkspaceError.invalid_name` (tag 42)

- **What changed.** A new arm, `agentrepl.v1.CreateWorkspaceInvalidName`,
  carries the trimmed name, the `/`-separated `component` that broke its rule,
  and a `rule` oneof with two empty arms. `slug` means the last component is
  not a slug. `prefix` means an earlier component is not a prefix.
- **Why.** The owner agreed (2026-10-22) that every supplied name is checked
  by the daemon before it accepts a create. The rules are the slug shape a
  minted name has, without the word and length limits. An optional prefix
  part is still allowed, as `branchFor` already allowed one. The daemon is the
  one place the rules live, and the editor only words the refusal.
- **Rules.** The last component must match the slug shape
  (`agent-repld.internal.workspace.ValidateSlugShape()`). Every earlier
  component must be ASCII letters and digits, optionally joined by single
  `.`, `_` or `-`. Prefix components may be uppercase, because a prefix can
  be a person's initials (`ABC/reconnect-fix` is a supplied name in the
  daemon's own tests).
- **Why a `rule` oneof rather than a reason string.** A client words the
  refusal from the arm, and no reason has to be parsed. The offending
  component sits beside the oneof, because it means something in both arms.
- **Consequences.**
  - The check runs in `agent-repld.internal.workspace.verbs.branchFor()` on
    the supplied-name branch only. A merge workspace's name (`MergeName`) is
    checked by the async-merge request that reserved it, so it is not
    re-checked here.
  - A workspace refusal can now carry a nested arm. The value type is
    `agent-repld.internal.refusalarm.Nested`, extracted from
    `merge.NestedArm` and the server's private `nestedArm` in the commit
    before this one.
  - The editor's hand-written codec
    (`~/.config/doom/modules/app/agent-repl/lisp/wire-verbs.el`) decodes the
    arm, and `agent-repl-verbs--create-refusal-sentence` words it.
  - No row goes in `daemon/ERROR-ARMS.md`, because that ledger holds only
    unlanded arms.
