# Diagnosis: reset timestamp jitter is mistaken for a new window

Severity: Medium. Confidence: Certain.

Five turn-end samples were provably misclassified as window changes. Four occurred in `doom`; one occurred in `ceac-sandbox-gns-gate`. Every pair referred to the same `19:20` reset boundary but differed by fractional seconds. One `doom` turn changed utilization from 17 to 18 while its delta was suppressed.

Examples include `19:20:00.557120` versus `19:20:00.939811`, and `19:19:59.908698` versus `19:20:00.502647`. `modules/app/agent-repl/agent-shim/claude/shim/src/uds/uds-session.ts:1694` uses exact string equality for `resetsAt`.

Root cause: a display timestamp with provider-side sampling jitter is being used as a stable window identity. The implementation has no canonicalization step tied to the provider contract.

Impact: valid per-turn five-hour utilization deltas are reported unavailable, including turns where utilization changed. One additional unavailable GNS sample had genuinely absent observations and is not part of this bug count.
