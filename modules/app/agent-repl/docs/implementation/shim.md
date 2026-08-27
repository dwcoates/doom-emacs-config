# Shim implementation planning

## Dead code to remove
- `src/uds/framing.ts`'s envelope half (`MessageConn`, `encodeMessage`,
  `decodeEnvelope`, `envelopeType`, `unpackAs`): unexercised since the
  protocol.v1 wire layer died; remove with the shim.v1 Connect service
  implementation (the codec half may survive if the new transport reuses it).
- `runUdsMode`'s loud-throw stub: replaced by the shim.v1 service entrypoint.
- `agent-shim/claude/shim/AGENTS.md`'s three-surface story
  (conversation.v1/protocol.v1/agentshim.v1) — rewrite against the six-surface
  model when the service lands.

## Replacement integration-test specs
(Replacing the 14 deleted suites' subjects under the new contract; unit specs
deliberately absent.)
- shim.v1 service: one integration spec per rpc suite — session lifecycle
  (StartSession cold-gate refusal path included), turn (StartTurn one-in-
  flight refusal, WatchAgent open-with-page), detached work (per-kind watch/
  stop), history (ReadHistory first/after).
- store.v1 client: WriteBatch durable-ack + spill retirement on success;
  failure = nothing committed, spill replays (replaces store-spill/
  store-replay suites' subjects).
- SDK→conversation.v1 conversion: golden transcripts (real captures per
  deferred vetting item 5) driven through the converter, asserting the
  Agent* frames — replaces convert/delta/extras suites.
- Reattach: daemon reconnect via WatchAgent known_through catch-up (replaces
  reattach.test.ts's subject).
