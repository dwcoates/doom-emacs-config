# Diagnosis: daemon state outruns host workspace materialization

Severity: High. Confidence: Certain.

Before `WorkspaceAvailable` was delivered, Emacs received session state for both newly created workspaces:

- `ceac-sandbox-gns-gate`: two render-state updates were skipped and four context-cost updates were dropped between `13:30:39.081` and `13:30:39.868`. `WorkspaceAvailable` arrived at `13:30:40.178`.
- `agent-repl-bubble-breath`: two render-state updates were skipped and four context-cost updates were dropped between `13:38:06.853` and `13:38:07.708`. `WorkspaceAvailable` arrived at `13:38:07.799`.

Four corresponding warnings show both path-form and name-form records routed to the global Emacs sink because the workspaces were unregistered. The log trace explicitly says `DROPPED unresolvable workspace` and `render skipped`.

The schema already defines the intended handshake: `WorkspaceAvailable` is retained until `WorkspaceMaterializedCmd`, and the initial prompt is held until materialization. Session-state publication is not honoring the same gate.

Impact: initial render and context-cost facts are lost, and their diagnostics escape the workspace's canonical log. Later snapshots may repair some state, but the dropped context-cost observations are not proven recoverable.
