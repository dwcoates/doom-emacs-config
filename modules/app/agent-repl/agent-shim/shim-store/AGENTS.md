# agent-shim/shim-store/

The record store (Go, singleton, launchd-managed): the sole owner of the record
database (SQLite/WAL) and the only server of `store.v1.ShimStore`. Its callers
are exactly the shim and the sidecar.

**THE DAEMON NEVER IMPORTS OR CALLS `store.v1`.** The daemon's read path is
`shim.v1`; `check-conversation-isolation.sh` refuses the import at codegen.

The protos under `proto/src/store/v1/` are the authoritative documentation —
read them at the symbol you are implementing. Nothing here ever changes a
`.proto`; a needed change goes up the orchestration chain.

## Transport: Connect over a unix domain socket

- `net.Listen("unix", path)` plus `http.Server{Handler: h2c.NewHandler(mux,
  &http2.Server{})}`, so HTTP/1.1 and prior-knowledge HTTP/2 both reach the
  same handler on one cleartext socket, and both Connect codecs (binary and
  JSON) come for free. `storev1connect.NewShimStoreHandler` does the routing.
- **THERE IS NO HEALTH VERB, BY DESIGN.** Streams and the transport own
  liveness. `agent-shim-doctor` probes the Connect endpoints directly. The old
  `Subscribe`/`EntryDelivery`/`ConnectionHeartbeat`/`HealthCheck` dial
  protocol, the `agentrepl/wire` framing, `internal/healthcheck` and the
  `-health-check`/`-health-request-id`/`-health-timeout` trio are all DELETED.
  Do not reintroduce a health rpc, a heartbeat, or a write ack.
- **The Serve/Close race is gone.** Nothing tracks connections by hand
  (`trackConn` after `Accept` used to race the `Close` snapshot);
  `http.Server` owns connection lifetime and `Server.Shutdown(ctx)` is the only
  stop. Shutdown closes a `done` channel FIRST so standing watches return, then
  drains — a pure tail would otherwise make `http.Server.Shutdown` wait
  forever. Tests must not barrier around any of this.
- A watch over the **Connect protocol is half-duplex**: the client's
  `WatchAgentSession` call does not return until the server writes response
  headers, which it does on its first frame. So **acceptance is silent** — a
  standing watch on a quiet agent simply has not answered yet — and **refusal
  is `CodeNotFound` on the first `Receive`**. That is acceptable because the
  consumer opened with a page first. A test that must observe a subscribed but
  undelivered watch runs the call off its own goroutine.

## Flags

| flag | default |
| --- | --- |
| `--socket` | `$AGENT_REPL_STORE_SOCKET`, else `~/.cache/agent-repl/sock/store.sock` (`XDG_CACHE_HOME` honored) |
| `--db` | `…/store/events.db` |
| `--log` | `…/log/shim-store.log` (also mirrored to stderr) |
| `--pprof` | `$AGENT_REPL_STORE_PPROF_ADDR`, else OFF |
| `--watch-buffer` | `8192` frames per subscriber |

`AGENT_REPL_STORE_SOCKET` is only the flag's DEFAULT, so an explicit `--socket`
always beats it. That is how a test harness points every participant at a
private store without editing a command line.

**Boot order is load-bearing: the pprof surface is opened BEFORE the
database**, so a store wedged recreating its schema or on a cold first read is
still profilable. `--pprof` is opt-in and local-only (a unix socket path, or an
explicitly loopback `host:port`); a wildcard or routable bind is refused rather
than served and hoped about, and a configured surface that cannot bind is a
hard error. Both outcomes are recorded (`store.pprof.disabled` /
`store.pprof.enabled`).

## The four tables

- `agent` — one row per `AgentId`, main agent included. THE home of agent
  metadata: `spawned_by_agent` XOR `spawned_by_workflow` (main: neither), the
  unpacked `AgentSubagentStart` fields, `started_at_ms`, `ended_at_ms`
  (NULL = live), `terminal`.
- `entry` — the spine: `position` (FIRST-insert order — the page order and the
  pointer; an upsert keeps it), `upsert_key` UNIQUE, `write_id` UNIQUE,
  `write_seq` (store-internal global write ordinal, bumped on every insert AND
  every upsert), `plane`, `kind`, `book_agent_id` (NULL for every never-served
  row — the keep-alive/residue index), `top_level`, and `frame`, the serialized
  `StoreEntry` the store never opens beyond routing.
- `detached_work` — one row per detached non-agent run (bash today): handle,
  kind, origin unit, owner agent, latest state, `ended_at_ms`, terminal.
- `workflow` — the table exists and **NOTHING routes into it this wave**.

`agent`/`workflow`/`detached_work` are UNPACKED to columns because the store
filters and joins on them; `entry`'s frame stays a BLOB because activity
vocabulary is content, and unpacking it would drag every `conversation.v1`
change into DDL.

### The store is NUKED, never migrated

`db.Open` checks `schema_meta.version`; any mismatch, or any pre-existing table
set that differs, DROPS every table and recreates. There is no `ALTER`, ever,
and no migration code. Writing migration code, or preserving a stored shape on
durable-compatibility grounds, is forbidden.

### Any transaction that writes must BEGIN IMMEDIATE

The DSN carries `_txlock=immediate` (plus WAL, `busy_timeout`,
`synchronous(NORMAL)`). A write batch reads before it inserts, so a DEFERRED
transaction takes a WAL read snapshot and then tries to upgrade — and SQLite
will not run the busy handler for an upgrade: it returns `SQLITE_BUSY_SNAPSHOT`
(517) or `SQLITE_BUSY` (5) immediately, so `busy_timeout` never applies. One
store process serves every live producer on its own pooled connection, which
makes those collisions routine. Keep the DSN, and never add a read-then-write
transaction that begins DEFERRED.

## Routing, orderings, pointers, tokens

- **Pageability is the PRODUCER's decision**, read from exactly one place:
  `StoreAgentUpdate.agent_info`. `serveable_frame` names its book
  (`page_agent_id`) and is the ONLY thing a page can ever return;
  `unserved_item` (keepalive / vendor_specific / unknown / unparsed) is durable
  and never served; `bash` and `workflow` are structurally not page lines.
- One transaction per batch, and **failure commits nothing**. Per entry, in
  producer order: absorb by `write_id` (a hit is success), else upsert by
  `upsert_key`; then route by arm. `success`/`failure` frames are a DUAL WRITE —
  the page line AND the agent row's terminal columns, in the one transaction.
  `detached_work` announcements go to the lifecycle table only. `cursor_advance`
  upserts in the same transaction, which is the whole exactly-once guarantee.
- `agent_update.workflow` lands durably as `kind=workflow` with a WARNING that
  workflow ingestion is not implemented this wave. Nothing touches the workflow
  table. `GetWorkflow` answers the typed not-implemented failure.
- **Order is by FIRST insert, never last write**, so a `StoreItemPointer` is
  stable across upserts and a unit settling mid-walk cannot teleport across a
  continuation. Pointers are opaque encodings of `position`, echoed verbatim; a
  pointer that names no row IN THAT BOOK is a stale-pointer refusal. No `seq`
  exists on any wire.
- `OpenAgentSession` answers the page plus a store-minted `AgentSessionToken`
  (128 random bits from `crypto/rand`, hex). The token is **SINGLE-USE**:
  `WatchAgentSession` consumes it, and an unknown, consumed, or
  previous-process token is refused. Tokens live in memory and do not survive a
  restart — "you already used this" and "the store restarted" are the same
  recovery for the caller: re-open. **An agent with no rows is a legal EMPTY
  book** (empty page, floor, valid token); "unknown agent" is only an empty
  agent value.
- The watch pin is the global `write_seq` at the moment the page was read,
  taken INSIDE that read transaction. `WatchAgentSession` then **subscribes to
  the fan-out BEFORE running the replay query** and dedupes by `write_seq`, so
  the replay-to-live handoff is gapless and duplicate-free. Upserts of old rows
  stream at their ORIGINAL pointer. No keep-alive or residue row is ever
  streamed, and there is no terminal frame: the stream ends only when the
  client cancels or the store shuts down.
- Backpressure: a bounded per-subscriber channel of `--watch-buffer` frames.
  Publishing is a non-blocking send under the fan-out lock, so one slow reader
  can never stall a writer's acknowledgement. On overflow the store logs a
  WARNING and **ENDS that stream with `CodeResourceExhausted`**; recovery is a
  re-open with `known_through`. Never silently thin a subscriber.
- `GetLiveWork` is the `ended_at IS NULL` scans. "Live" is a claim about the
  RECORD — a start was written and no terminal ever was — so it is timeless and
  cannot go stale. Main agents are never listed; `live_workflows` is empty this
  wave.

## Refusal sites

Every response is `oneof result { success | failure }`. `detail` strings are
for humans and logs and are **never switched on**. The sites below are the real
places the store says no; they are the vocabulary the proto's failure `kind`
arms are derived from, and each one is logged once with `refusal_site`.

`producer_empty`, `batch_missing`, `batch_empty`, `entry_plane_unset`,
`entry_write_id_empty`, `entry_upsert_key_empty`, `entry_arm_unset`,
`cursor_file_id_empty`, `agent_id_empty`, `page_size_zero`, `pointer_empty`,
`token_empty`, `unknown_watch_token`, `file_id_empty`,
`store_refused_request`, `stale_pointer`, `database_failure`,
`workflow_not_implemented`, `watch_buffer_overflow`.

- Validation runs **before the store is touched**: a refused request never
  opens a transaction. An unset non-optional field is illegal, an unset oneof
  is an error, and absence is spelled with optional presence — never `""`, `0`
  or `-1`. A batch refusal's detail names the offending entry index and its
  `write_id`.
- **A refusal is an HTTP 200 with the typed failure arm.** `connect.NewError`
  is reserved for transport and malformed-protobuf cases — plus the two rpcs
  that have no failure arm: `WatchAgentSession` (`CodeNotFound` for a refused
  open, `CodeResourceExhausted` for overflow, `CodeInternal` for a replay
  failure).
- Never swallow, default away, or fall back. Fallback persistence, silent
  continue and limp-along modes are forbidden.

## Logging

- `internal/logging` is the store's ONE canonical JSON logging API. Direct
  `fmt`, `log`, `slog` or ad hoc diagnostics are forbidden except the
  documented pre-logger bootstrap failure and the logger's own sink-emergency
  path.
- Every logical branch logs its selection: verbose for the ordinary path,
  `warn` for degraded-but-handled, `error` for failures. **Every error is
  logged exactly once, by its owning layer.** `internal/db` records its own
  storage failures with statement and table context, so `internal/server`
  answers the failure arm with a VERBOSE trace rather than a second error
  record; the refusals the server owns (validation, tokens, overflow) carry its
  `warn` record.
- Correlation keys, each in its own `context` field and never left to the
  message text: `producer`, `agent_id`, `vendor_session_id`, `book_agent_id`,
  `write_id`, `upsert_key`, `position`, `write_seq`, `watch_token_hash`, `rpc`,
  `refusal_site`, `file_id`, `path`, `offset`, `task_id`, `activity_id`,
  `turn_id`. Top-level `request_id` comes from the `X-Agent-Repl-Request-Id`
  header when a caller sends one. `rpc` is the Connect procedure spelled
  exactly as Connect spells it, leading slash included
  (`/store.v1.ShimStore/WriteBatch`).
- **The retired addressing keys are DEAD**: `claude_session_id`, `seq`,
  `from_seq`, `replay_*_seq` and anything else naming the old `(session_id,
  seq)` addressing. Do not add one back.
- A **watch token is never logged** — only `watch_token_hash`, a sha256 prefix.
  A token is a capability.
- `offset` is a `*int64` on purpose: zero is a meaningful file offset and must
  be reported, not omitted as unset.
- Hot per-record and per-batch success diagnostics go through the VERBOSE
  helper (gated by `AGENT_REPL_LOG_VERBOSE` at startup). Lifecycle, invariant
  violations, refusals and failures are normal verbosity. Slow queries are the
  one deliberate exception: a statement past
  `AGENT_REPL_STORE_SLOW_QUERY_MS` (default 250ms) emits a normal-verbosity
  `warn` at `store.db.slow-query` with `statement`, `duration_ms`, `rows` and
  `threshold_ms`, because by the time an operator knows to look the stall is
  over. `statement` is a FAMILY NAME, never rendered SQL and never bound
  values — the payloads are opaque to the store, and quoting a parameterized
  statement would put session content into the global log. A malformed
  threshold aborts `db.Open`.

## Suites

```bash
cd modules/app/agent-repl/agent-shim/shim-store
go build ./... && go vet ./... && go test -race ./...
go test -race ./internal/server/      # the Connect service, with a fake Store
go test -race ./internal/db/          # the schema, routing and reads
go test ./integration/                # the whole module end to end over a socket
make coverage                         # ../../bin/report-nonlisp-coverage.sh store
```

- Go tests are table-driven, AAA, one edge case per test function, one
  `_test.go` per source file.
- **Never `time.Sleep` for synchronization.** Use channels, `sync.WaitGroup`,
  contexts, or a real signal — a Connect stream delivering a frame is a
  legitimate primitive. `internal/server`'s fake `Store` gates `LinesSince`,
  which is entered strictly after the handler subscribes; that gate is how the
  replay-to-live handoff, buffer overflow and shutdown paths are driven
  deterministically.
- **Unix socket paths must be short.** macOS caps `sun_path` at ~104 bytes, so
  never build one under `t.TempDir()`; use
  `filepath.Join(os.TempDir(), "ar-"+<8 random hex>+".sock")` and remove it in
  cleanup.
- Every test process exports `AGENT_REPL_FORBID_VENDOR_CALLS=1` from `TestMain`.
  Nothing here ever calls a vendor.
- `internal/server` must stay testable with NO database: it declares the
  storage contract as `type Store interface`, whose result types and sentinels
  are ALIASES of `internal/db`'s — so `*db.DB` satisfies it directly (asserted
  at compile time in `internal/server/store.go`) and there is no adapter to
  drift. Add a method to `db` and the interface in one commit, never a
  conversion layer between them.
- Collect every failure in a run before fixing any of them; never fail fast.
- Maintain at least 90% statement coverage. Until the measured store baseline
  reaches that target, never reduce it, report the gap explicitly, and add
  focused tests for every critical branch and every error path changed.
- `modules/app/agent-repl/bin/test-all.sh` (from the repository root) runs
  every tracked suite across the module;
  `bin/report-logging-density.sh store` is a rough review aid, not semantic
  coverage — audit critical branches and errors directly even when its ratio
  rises.

## Dependencies

`agentrepl/proto` (generated Go for `store.v1` and `conversation.v1`, plus the
`storev1connect` handlers and clients), `agentrepl/logging`,
`connectrpc.com/connect` **v1.17.0**, `golang.org/x/net` **v0.43.0**,
`modernc.org/sqlite`. Both pins are project rulings: keep them, keep the module
at `go 1.23.0` with no `toolchain` line so the host toolchain builds it without
a switch, and never edit `proto/gen/go/go.mod`.
