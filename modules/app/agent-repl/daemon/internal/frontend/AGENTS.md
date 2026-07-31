# daemon/internal/frontend/

The daemon's frontend surface: serves `agentshim.frontend.v1` frames as
protojson over UDS (Emacs) and WebSocket (webapp). Responsibility: translate
internal events and SSM state into the resolved frontend vocabulary
(`WorkspaceState`, `ConversationDelta`, `TypingDelta`, `TaskCatalog`,
`SessionView`), send `StateSnapshot` on (re)connect, and dispatch inbound
`FrontendCommand`s with `CommandAck`s. Frontends render what this package
sends and never derive state themselves.

## Delivery deltas are unsequenced, freshness is leased

Every connected frontend receives every emission immediately: same order, same
content, no frontend's render pace gating another's delivery. `PushWorkspaceState`
fans out under the client-registry lock, so an emission, a connect and a
disconnect are serialized against each other, and that is the whole mechanism.

GUI stream connections also receive a renewable authoritative `StateSnapshot`
lease. Socket-open does not attest current state. The browser becomes current
only after it has decoded and atomically adopted a snapshot, and it expires all
live-state projections if three lease intervals pass without another snapshot.
The lease snapshot is enqueued under the same delivery lock as deltas, so its
revision gives the browser a bounded freshness proof without reintroducing a
viewer-acknowledgement gate. A full lease queue is a hard disconnect, never a
silent skip.

It used to be a GATE. A `WorkspaceState` travelled resolver → the frontends that
PAINT it → their acknowledgment → the frontends that merely OBSERVE it, so the
Emacs tab bar could never show a state the webview had not drawn. That bought
surface agreement at the price of a viewer-based attestation model, and it had a
hole of its own: an observer's reconnect snapshot was filtered to states a
painter had SETTLED, so a workspace whose first emission was still held was
OMITTED from Emacs entirely.

Both are gone. The SSM emits the same composite session connectivity, session
status, controller generation, and active faults to every frontend; no
viewer's render pass has a claim on those facts.

Emacs remains the authority for workspace membership. In a session webview,
the revisioned `WorkspaceState` is the sole authority for both the footer and
the current workspace's sidebar status. Non-current sidebar rows still use the
Emacs roster. This prevents two asynchronously delivered copies from presenting
different phases for the same current session.

`ClientKind` remains, and it was never about painting: it names the frontend
product behind a connection, fixed at accept from the endpoint that accepted it,
and it is the authority for the host-only frames and commands Emacs alone may
see.

- `/sessions/{id}/stream` — `ClientKindGUIStream`, the rendering webview.
- the frontend UDS — `ClientKindHost`, the Emacs tab bar.
- `/frontend` — `ClientKindGUIBootstrap`, the webapp's short-lived bootstrap
  socket, which creates a session and closes.

Dependencies: `proto/agentshim/` (generated Go), `daemon/internal/ssm/`.
