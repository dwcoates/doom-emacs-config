# daemon/internal/frontend/

The daemon's frontend surface: serves `agentshim.frontend.v1` frames as
protojson over UDS (Emacs) and WebSocket (webapp). Responsibility: translate
internal events and SSM state into the resolved frontend vocabulary
(`WorkspaceState`, `ConversationDelta`, `TypingDelta`, `TaskCatalog`,
`SessionView`), send `StateSnapshot` on (re)connect, and dispatch inbound
`FrontendCommand`s with `CommandAck`s. Frontends render what this package
sends and never derive state themselves.

## Delivery is UNSEQUENCED, deliberately

Every connected frontend receives every emission immediately: same order, same
content, no frontend's render pace gating another's delivery. `PushWorkspaceState`
fans out under the client-registry lock, so an emission, a connect and a
disconnect are serialized against each other, and that is the whole mechanism.

It used to be a GATE. A `WorkspaceState` travelled resolver → the frontends that
PAINT it → their acknowledgment → the frontends that merely OBSERVE it, so the
Emacs tab bar could never show a state the webview had not drawn. That bought
surface agreement at the price of a viewer-based attestation model, and it had a
hole of its own: an observer's reconnect snapshot was filtered to states a
painter had SETTLED, so a workspace whose first emission was still held was
OMITTED from Emacs entirely.

Both are gone. A workspace's color is connection truth, decided by the SSM's
WIRED axis, and no viewer's render pass has a claim on it.

`ClientKind` remains, and it was never about painting: it names the frontend
product behind a connection, fixed at accept from the endpoint that accepted it,
and it is the authority for the host-only frames and commands Emacs alone may
see.

- `/sessions/{id}/stream` — `ClientKindGUIStream`, the rendering webview.
- the frontend UDS — `ClientKindHost`, the Emacs tab bar.
- `/frontend` — `ClientKindGUIBootstrap`, the webapp's short-lived bootstrap
  socket, which creates a session and closes.

Dependencies: `proto/agentshim/` (generated Go), `daemon/internal/ssm/`.
