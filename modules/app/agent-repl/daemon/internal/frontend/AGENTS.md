# daemon/internal/frontend/

The daemon's frontend surface: serves `agentshim.frontend.v1` frames as
protojson over UDS (Emacs) and WebSocket (webapp). Responsibility: translate
internal events and SSM state into the resolved frontend vocabulary
(`WorkspaceState`, `ConversationDelta`, `TypingDelta`, `TaskCatalog`,
`SessionView`), send `StateSnapshot` on (re)connect, and dispatch inbound
`FrontendCommand`s with `CommandAck`s. Frontends render what this package
sends and never derive state themselves.

## Delivery order (paintgate.go)

A `WorkspaceState` is not broadcast. It is SEQUENCED: resolver → the
frontends that PAINT it → their acknowledgment → the frontends that merely
OBSERVE it. That holds for every state in the vocabulary, not only for green,
because two frontends drawing one fact on two surfaces with nothing ordering
them can disagree for as long as either takes to paint.

Each connection's `Role` is fixed at accept, from the endpoint that accepted
it, so no connection is ever registered with an undecided role:

- `/sessions/{id}/stream` — `RolePainter`, the rendering webview.
- the frontend UDS — `RoleObserver`, the Emacs tab bar.
- `/frontend` — `RoleObserver`, the webapp's short-lived bootstrap socket,
  which creates a session and closes without ever rendering.

Every gate decision and every delivery happens under the client-registry lock,
which is what makes the mode transitions race-free rather than merely
unlikely. Nothing in the gate waits on elapsed time: a stale acknowledgment
names an older generation and settles nothing, a frontend that cannot paint
sends `PAINT_OUTCOME_SUSPENDED` and settles the state anyway, and a frontend
that disconnects stops being counted so its held states settle at once.

The two attestation EDGES also live here. A painter attaching or leaving calls
`Config.WithdrawPaint` (bound to `ssm.ApplyPaintLost`), so a renderer that is
no longer on screen cannot keep a workspace green and a fresh one has to draw
the history it was handed before green is claimed again.

Dependencies: `proto/agentshim/` (generated Go), `daemon/internal/ssm/`.
