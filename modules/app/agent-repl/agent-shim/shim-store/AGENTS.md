# agent-shim/shim-store/

The event store (Go, singleton, launchd-managed). Responsibility: sole owner
of the event database (SQLite/WAL). It ingests protocol events from producers
(shims, the sidecar), assigns each session's gapless `seq`, dedups
stream-plane/file-plane overlap on `dedup_key`, commits events plus producer
cursors in one transaction, and serves `Subscribe{session_id, from_seq}`
replay-then-live-tail subscriptions. EPHEMERAL-class events are fanned out to
live subscribers but never persisted.

The store is deliberately tiny and frozen: schema, seq, dedup, fan-out —
nothing else. Payloads are opaque to it (no vendor knowledge, no parsing, no
interpretation); only envelope columns are extracted for indexing.

Dependencies: `proto/agentshim/` (generated Go), SQLite.
