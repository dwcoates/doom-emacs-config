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

## `session_id` is the VENDOR session id, never a daemon/shim id

Every `session_id` in this store — the seq scope, the `(session_id, dedup_key)`
dedup index, the fan-out routing key, and `Subscribe{session_id, from_seq}` —
is the **vendor** session id: Claude's uuid, which is also its transcript
filename. It has to be, because two producers write the same conversation and
must agree on its name: the shim (stream plane) reads `session_id` off the SDK
message, and the shim-sidecar (file plane) derives it from `<uuid>.jsonl` — the
sidecar never talks to the daemon and cannot know a daemon `s_…` id. Disagree
and the dedup that merges the two planes cannot fire.

Fan-out is an exact map lookup (`f.subs[ev.GetSessionId()]`), so subscribing
under any other id registers a subscriber on a channel nothing publishes to:
writes still succeed, and replay plus live-tail silently return nothing. That
is precisely the 2026-07-25 bug — the shim subscribed under its `--session-id`,
so only EPHEMERAL events (which bypass the store) ever reached the daemon, and
prompts never rendered while responses arrived structureless.

## Any transaction that writes must BEGIN IMMEDIATE

`Ingest` reads (`SELECT MAX(seq)`) before it inserts, so a DEFERRED
transaction — Go's `database/sql` default — takes a WAL read snapshot and then
tries to upgrade to a writer. SQLite will not run the busy handler for an
upgrade: it returns `SQLITE_BUSY_SNAPSHOT` (517) when another connection
committed since the snapshot, or `SQLITE_BUSY` (5) when a writer holds the
lock, both immediately, so `busy_timeout` never applies. One store process
serves every live shim on its own goroutine and pooled connection, which makes
those collisions routine, and a rejected batch is PERMANENT loss (the shim's
store-client drops it — no spill, no retry). The DSN therefore carries
`_txlock=immediate`; keep it, and never add a read-then-write transaction that
begins DEFERRED.

Dependencies: `proto/agentshim/` (generated Go), SQLite.
