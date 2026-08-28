# Sidecar implementation planning

## Dead code to remove (with the conversion port)
- convert.UnportedEntry and its "__unported_conversion" discriminator: a
  loud reconciliation stub — every vendor→conversation.v1 conversion now
  writes through it; the port replaces it with the Agent* frame conversion
  and the discriminator enumerates exactly what must be ported.
- The raw length-prefixed Any-over-UDS transport, when the store.v1 Connect
  port lands (messages are already repointed; the transport is not).
- Client.Health's ErrNoHealthProbe stub and main.go's beat-timer teardown
  around it (see blockers).

## Blockers / decisions owed (surfaced at reconciliation)
- AGENT-ID MINTING FROM A TRANSCRIPT: nothing tells a file reader how to
  mint an AgentId from a Claude transcript — the design record's identity
  entries (shim-minted main_agent_id, vendor agent_id for subagents,
  meta.json for workflow agents) are the spec; the sidecar needs the shim's
  minting discipline or a shared rule. LEAD-LEVEL: cross-produces with shim.
- LOAD-BEARING: the sidecar currently CANNOT hold its store link up — the
  beat timer tears down on the always-failing health probe. The port must
  either give it a real probe route or remove probing (streams+transport own
  liveness per the design ruling), FIRST.
- No session attribution on StoreEntry (old ExternalEntry.session_id/
  produced_at_ms have no successor): confirm the new addressing (agent-keyed
  spine) covers every reporting path, or surface a contract gap.
- No bookkeeping arm: sidecar self-diagnostics have no typed home — confirm
  the pulled-diagnostics model covers them or surface a gap.
- No authoritative open-task snapshot (OpenTaskState died): staleness
  tracking / boot LOST sweep / spool-owner seeding re-derive from the store's
  live-work reads (GetLiveWork) per Owed A/H.
- Read WriteBatchResponse (recommended at reconciliation): restores the
  batch-rejection error branch that has been unreachable — take it.
- Workflow per-agent transcript ingestion (Owed E): discovery must glob
  workflows/wf_*/agent-*.jsonl + meta.json.

## Replacement integration-test specs
(Unit specs deliberately absent per the mapping convention.)
- Vendor transcript → Agent* frames: golden real captures driven end to end
  into StoreEntry writes (replaces the ~34 deleted convert tests' subjects
  under the new model; rides item 5's capture harness).
- Detached-work lifecycle from files: spool bytes → AgentBashUpdate deltas;
  EXIT marker → terminal; journal started/result → workflow announcements
  (replaces the handler suites' subjects).
- Restart recovery: cursors from GetSidecarCursors + live-work re-announce
  with original start instants recovered from the store (Owed A).
- WriteBatch ack handling: success retires spill; failure replays.
