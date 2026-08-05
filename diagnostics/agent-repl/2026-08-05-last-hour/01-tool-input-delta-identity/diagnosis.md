# Diagnosis: tool-input delta identity is insufficient

Severity: High. Confidence: Certain.

`agent-repl-bubble-breath` logged 104 dropped `input_json` deltas across 21 blocks, `ceac-sandbox-gns-gate` logged 732 across 71 blocks, and `slack-ceac-tech-ptn` logged 72 across 8 blocks. The first and last occurrences were `13:30:51.035` and `13:49:13.033` local time.

A representative bubble trace shows the durable assistant tool-use record for `msg_011Cdk1hn2t1ogJ844STTVhh` reaching the store before subsequent ephemeral input chunks. The webapp adapts the durable tool with `inputDone: true`, then `growToolInput` searches only for the latest tool with `inputDone: false` and reports `no-open-tool`.

The underlying contract gap is explicit in `modules/app/agent-repl/proto/agentshim/core/v1/core.proto:699`: `ContentDelta` has message UUID and block index, but no tool-use ID. `modules/app/agent-repl/webapp/src/streaming.ts:295` consequently ignores delta identity and searches by open state. This is unsafe because durable store delivery and ephemeral bypass delivery are independently ordered.

Impact: live tool input is dropped from the typing surface. The final durable record may still contain authoritative input, but current telemetry cannot prove that for each warning, so visible data loss cannot be ruled out.

Observability gap: the warning omits session ID, message ID as a structured field, block index, tool-use ID, delta length, and whether an authoritative final was already present.
