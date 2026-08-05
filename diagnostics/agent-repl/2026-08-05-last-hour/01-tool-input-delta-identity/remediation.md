# Remediation: carry and reconcile stable tool identity

1. Add an optional `tool_use_id` to `core.v1.ContentDelta`. The shim must bind API block index to the tool-use ID learned at `content_block_start` and include it on every `input_json` chunk.
2. Relay the augmented `ContentDelta` unchanged through the existing `frontend.v1.TypingDelta` envelope.
3. Index preview and finalized tool items by `tool_use_id`. A late delta may update the correlated preview or be classified as superseded by an already-authoritative final. It must never attach to an unrelated open tool.
4. Abort and log loudly when an input delta lacks tool identity after the schema rollout. Do not select an ambient tool by position.
5. Add shuffled-order tests for ephemeral-before-durable, durable-before-ephemeral, multiple tool blocks in one API message, reconnect, and exact redelivery.
6. Add structured diagnostics containing workspace, both session identities, API message ID, block index, tool-use ID, chunk length, current item phase, and chosen branch.

Success criteria: zero `no open tool to grow` warnings under randomized cross-plane ordering, exact final tool input after every ordering, and no positional tool selection in `streaming.ts`.

Protobuf decision: required. The existing message and block fields cannot identify a tool call once the durable record has settled under its own tool-use identity.
