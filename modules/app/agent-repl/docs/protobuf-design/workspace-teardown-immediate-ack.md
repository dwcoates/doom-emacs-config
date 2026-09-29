# Kill and nuke acknowledge at once (2026-09-29)

## Owner ruling

- Closing, killing or nuking a workspace updates the roster everywhere
  (webapp, daemon, Emacs).
- Emacs dispatches the verb, the daemon acknowledges immediately, and Emacs
  closes the tab immediately; it no longer waits for the teardown.

## Evidence

- A kill on 2026-09-29T17:45:01 answered 386ms after Emacs sent it: the
  shim's session shutdown, then the process stop, ran before the answer.
- A nuke also waits on `git worktree remove` and the branch deletion.
- Close was already fast: its refusal (`blocked`) is judged before the answer,
  and its work is a flag write and a roster republish.

## Landed shape (mirrors CreateWorkspace's option B)

- `agentrepl.v1.KillWorkspaceRequest.op_id` and
  `agentrepl.v1.NukeWorkspaceRequest.op_id` (tag 2, optional): presence opts
  into the immediate ack.
- `agentrepl.v1.KillWorkspaceResponse.accepted` and
  `agentrepl.v1.NukeWorkspaceResponse.accepted` (tag 3), echoing the op id.
  - Answered once every ownership refusal is ruled out and the workspace is
    marked closed and the roster republished.
- `agentrepl.v1.WorkspaceMutationProgress.kill` (tag 4) and `.nuke` (tag 5):
  exactly one terminal push per accepted verb.
  - `agentrepl.v1.WorkspaceKillProgress`: `succeeded` or
    `failed{internal}`.
  - `agentrepl.v1.WorkspaceNukeProgress`: `succeeded` or
    `failed{refusal: agentrepl.v1.NukeWorkspaceError | internal}`; after
    the ack only `git_failed` can arrive as a refusal.
- Unset `op_id` keeps the synchronous form unchanged.

## Implementation notes

- The daemon verbs split into a fast half (`BeginKill`, `BeginNuke`) and a
  `Teardown`; the server runs the teardown detached from the request context
  when the request carries an op id.
- Emacs mints the op id, tears the tab down on `accepted`, and reports a
  failed teardown loudly from the progress push.
