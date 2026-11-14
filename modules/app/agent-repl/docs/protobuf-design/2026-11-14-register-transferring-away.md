# A registration of a handed-away workspace is refused: `agentrepl.v1.RegisterWorkspaceError.transferring_away`

2026-11-14, branch `tdrh-register`. Designed autonomously.

## The defect

During a handover the successor refuses a workspace it is still adopting with
`not_yet_adopted`, naming the incumbent. Emacs then re-registers the workspace
at the incumbent (`lisp/host.el` `agent-repl-host-view-where-served`), and the
incumbent answered SUCCESS for a workspace it had already handed to the
successor. `internal/workspace` `admit` had stopped claiming serving ownership
back for it, but the registration still succeeded, so Emacs stayed attached
to the incumbent instead of walking on to the successor. `RegisterWorkspaceError`
had no arm that could say "this workspace is served elsewhere now".

## Assumptions (autonomous mode)

- Objective: a registration of a workspace this daemon transferred away is
  answered with where it is served, before anything is written.
- Systems: the daemon (refusal), Emacs lisp (decode and route). The webapp and
  the shim never call RegisterWorkspace.
- Non-additive change: a re-registration of a handed-away workspace stops
  succeeding. The old "registers without claiming" behaviour is retired.

## Landed changes

### `RegisterWorkspaceError.transferring_away` (tag 4)

- WHAT: `RegisterWorkspaceError.cause` gains
  `RegisterWorkspaceTransferringAway transferring_away = 4`, whose one field is
  `string address`, the successor's `127.0.0.1:<port>`. It mirrors every other
  endpoint's `<Rpc>TransferringAway{address}` (SelectWorkspace,
  CreateWorkspace, ...), so the server's one refusal path
  (`daemon/internal/server/refuse.go`, `fill` supplying `address`) sets it by
  arm name with no new mapping.
- RegisterWorkspaces needs no change of its own: its per-dir `refused` arm IS a
  `RegisterWorkspaceError`, so the batch answers the same arm per dir.
- WHY: the refusal is handover news the client acts on (re-register at the
  successor), and an arm with the address is the one way the client learns
  where to go. A bare Connect error with metadata (the unlanded-arm path) would
  log WARN on every handover and leave the client to parse a header.
- Consequences:
  - The refusal is decided before the row is re-opened, any serving claim, and
    any resolver binding (`internal/workspace/register.go` `registerRow`), so a
    refused registration writes nothing.
  - A standing that cannot be read now FAILS the registration (ERROR), where it
    used to fall back to claiming serving. The fallback let the 2026-10-31
    double-serving recur whenever the read failed.
  - RegisterRepository runs the same registration. Its error message is not
    given the arm: it is reached only when the repository's main-worktree row
    exists and was handed away, which Emacs's onboarding never re-announces
    mid-handover. It answers through the unlanded-arm path with the successor
    in the `Agent-Repl-Successor` metadata, recorded in `daemon/ERROR-ARMS.md`.
  - Emacs decodes the arm as `:transferring-away` with `:address` and hands it
    to `agent-repl-host-handle-refusal`, which already walks a workspace onto
    the successor.

Obviated-declaration sweep: nothing in `endpoint_register_workspace.proto` lost
its last reference.
