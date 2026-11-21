# Notices and agent-sent prompts are delivered on arrival

Owner ruling, 2026-11-21: a daemon notice to an agent (merge queue, workspace,
pull request and every other daemon notice) and a prompt one workspace's agent
sends another must never be held as a prompt on a workspace. They are
delivered when they arrive. Where one genuinely must wait, the wait is named
in code with its reason and bounded by the event it waits on, never an
undrawn hold.

Design mode: autonomous.

## Assumptions

- The change is documentation only on the wire: no message, field, enum value
  or oneof arm is added, removed or renamed. The new state (an arrival wait)
  is a daemon-internal store row (`wsm.ArrivalWait`, table `arrival_waits`),
  not a wire type, because nothing outside the daemon reads it: the held
  section never draws it, and `agent-repld workspace prompt` reports a minted
  turn either way.
- "Delivered when they arrive" into a running turn means a join after the
  turn's current tool call (`shim.v1.StartTurnRequest.join_running_turn`),
  the one way to reach a running turn without interrupting it.

## Landed changes

### `conversation.v1.PromptOrigin` states the arrival rule once

- What: the enum's header gains a "delivered on arrival" paragraph naming
  every arrival origin and every reason one can wait. Each arrival origin's
  comment that said "submitted through the prompt queue as a deferred prompt,
  so it waits behind a running turn" now says it is delivered on arrival and
  joins a running turn. `PROMPT_ORIGIN_AGENT_SENT` states that a standing cold
  gate refuses it, so its sender learns it did not go.
- Why: the comments described the hold the ruling removed.
- Consequences: `PROMPT_ORIGIN_MERGE_COMPACT_AFTER` still runs behind the
  handback's turn; when the handback joins a running turn, the compaction
  runs after that turn, which now carries the handback.

### The subscription, create, main-sync and submit endpoints say the same

- What: the delivery paragraphs of `endpoint_subscribe_merge_queue.proto`,
  `endpoint_subscribe_workspace.proto`, `endpoint_subscribe_pull_request.proto`,
  `endpoint_subscribe_github_merge_queue.proto`, `pull_request.proto`,
  `github_merge_queue.proto`, `main_sync.proto` and
  `endpoint_create_workspace.proto` say the notice is delivered on arrival.
  `agentrepl.v1.SubmitPromptSuccess.turn` says a prompt delivered on arrival is
  never held: it went, or waits under a named arrival wait no section draws.
- Why: the same.

## Obviated declarations

None on the wire. In the daemon, the held machinery's handling of prompts of
these origins (the tray's skip of an unseen hold, `holdfold`'s author and
unseen checks, the fold refusal `ErrAboveOtherAuthor`) can no longer be
reached, because the store refuses to hold such a prompt
(`wsm.ErrHeldOnArrival`). They are kept pending the owner's ruling, since
removing a refusal path needs sign-off.
