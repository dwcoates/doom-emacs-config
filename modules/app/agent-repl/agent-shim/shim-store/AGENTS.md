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
- **`WatchBashRun` is the one stream with a NATURAL END.** A book never ends —
  an agent can always say more — but a shell run concludes, and its conclusion
  is a row the store recognizes, so the stream closes after it. The caller needs
  no cancellation protocol and no timeout to know it has the whole run.
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
| `--log` | `…/log/shim-store.log` (size-capped, N generations; NOT mirrored to stderr) |
| `--pprof` | `$AGENT_REPL_STORE_PPROF_ADDR`, else OFF |
| `--watch-buffer` | `8192` frames per subscriber |

`--log` is opened through `agentrepl/logging`.`OpenRotating`: it appends to
what it finds and ROLLS AT A BYTE CAP into a fixed number of generations
(`<path>.1` newest through `<path>.N` oldest), so the store's disk footprint is
`(N+1) x cap` however long the process runs. It does NOT roll on open, because
a launchd service is bounced by every deploy and every crash.

THE TERMINAL IS NOT A SECOND LOG. Production builds the logger with
`logging.NewDurableOnly`, so ordinary records reach the durable sink ALONE.
Under launchd stderr is an append-only file the process neither owns nor can
roll, and mirroring every record there is an unbounded second copy of an
already-rotated log. The terminal keeps the BOOTSTRAP errors written before a
logger exists and the SINK-FAILURE record, which is the one thing the durable
sink cannot report about itself.

`AGENT_REPL_STORE_SOCKET` is only the flag's DEFAULT, so an explicit `--socket`
always beats it. That is how a test harness points every participant at a
private store without editing a command line.

**THE SOCKET IS THE STORE'S SINGLETON TOKEN, AND THE KERNEL ARBITRATES IT.**
Before unlinking a socket already at the listen path, the store DIALS it. A path
that accepts is owned by a live store: the boot is refused with one
`store.listen.occupied` error record and a non-zero exit, because unlinking it
would leave the incumbent serving a socket no client can reach while this process
silently took its callers. A path that refuses is debris, reclaimed with the
existing `store.listen.reclaim` warning. A non-socket is never touched at all.

**The signal handler is installed before anything is bound or opened.** The
kernel queues connections from `listen(2)` onward, so a supervisor that waits for
the socket sees a ready store the moment it binds; installed any later, `SIGTERM`
in that window still had its default disposition and killed the process outright,
leaving the socket file behind for the successor to reclaim. "The socket accepts"
now implies "signals are answered", and a slow boot is interruptible rather than
unkillable.

**Boot order is load-bearing: the pprof surface is opened BEFORE the
database**, so a store wedged recreating its schema or on a cold first read is
still profilable. `--pprof` is opt-in and local-only (a unix socket path, or an
explicitly loopback `host:port`); a wildcard or routable bind is refused rather
than served and hoped about, and a configured surface that cannot bind is a
hard error. Both outcomes are recorded (`store.pprof.disabled` /
`store.pprof.enabled`).

## Build reporting

The store has no connection to the daemon at all, so it reports the build it
is running through a FILE: `reportBuild` (`buildreport.go`) writes
`<run dir>/shim-store.build.json` — this process's pid and the content hash
of its own executable — as soon as the canonical logger exists and before
the store starts serving. The run dir is `agentrepl/logging/buildreport`'s
`ResolveDir` (`$AGENT_REPL_LOCK_DIR`, else `~/.cache/agent-repl/run`), the
same directory the kernel locks already live under.

The daemon's deploy reads this file and compares it against the build it just
made to decide whether the launchd-managed store is stale and needs a
restart. A failure at any step (resolving the process's own build, resolving
the run dir, or writing the file) is logged once at `error` through the
canonical logger and swallowed: the store keeps booting regardless, because a
service that cannot report its own build still has every reason to keep
serving the one it has. The daemon simply reads the missing or stale report
as "not running the fresh build".

**Every harness that boots a real store must set `AGENT_REPL_LOCK_DIR` to a
private directory.** Without it, a real `shim-store` spawned by a test
resolves the run dir to the developer's actual `~/.cache/agent-repl/run` and
overwrites their real `shim-store.build.json`.

## The tables

- `agent` — one row per `AgentId`, main agent included. THE home of agent
  metadata: `spawned_by_agent` XOR `spawned_by_workflow` (main: neither), the
  unpacked `AgentSubagentStart` fields, `started_at_ms`, `ended_at_ms`
  (NULL = live), `terminal`. `spawned_by_agent` is written by a spawn's START
  and ALSO by its SUCCESS when that names `created_agent_id`: the file plane
  delivers a synchronous spawn only as its conclusion, and the success is then
  the one frame stating the spawner. The success writes LINEAGE ONLY — never the
  start's metadata columns.
- `entry` — the spine: `position` (FIRST-insert order — the pointer and the
  catch-up order, NOT the page order; an upsert keeps it), `upsert_key` UNIQUE, `write_id` UNIQUE,
  `write_seq` (store-internal global write ordinal, bumped on every insert AND
  every upsert), `plane`, `kind`, `book_agent_id` (NULL for every never-served
  row — the keep-alive/residue index), `run_id` (a bash row's run; NULL
  otherwise — the `WatchBashRun` index, and the bash equivalent of
  `book_agent_id`), `top_level`, and `frame`, the serialized `StoreEntry` the
  store never opens beyond routing — and beyond its `turn`, which is the row's
  FIRST stamp: a later write that omits or contradicts it is rewritten to carry
  it (`carryStoredStamps`), so no plane can move a row between turns. The same
  function keeps the row's FIRST STATED `place`: a write stating another, or
  none, carries the stored one, and a row stored with none takes the first
  later stated place (the one way a row moves within its book). A disagreeing
  place is traced at VERBOSE, because the two planes' copies of one unit
  routinely state different places (the shim's observation instant, the
  sidecar's vendor timestamp). A place whose `at_ms` is not positive is refused
  (`place_not_positive`, on `entries[i].place.at_ms`).
- `entry_place` — THE PAGE ORDER, and the one home of the served place: one
  row per row with a book (`position` PK, `book_agent_id`, `at_ms`, `ordinal`,
  `recorded`), indexed `(book_agent_id, at_ms, ordinal, position)`. `recorded`
  = 1 is the row's first stated place, served as `StoreLineAt.recorded_place`;
  0 is its first-insert receipt instant (`entry.first_inserted_at_ms`, ordinal
  0), served as `received_place`. `placeRow` (`internal/db/place.go`) is its
  one writer, in the write's own transaction, and `lineAt` is the one
  constructor of a served line, so no path serves a line unplaced. A booked
  row found with no place row is a storage failure, never a line skipped. It
  is an IN-PLACE table whose BACKFILL, run once in the transaction that creates
  it, places every row stored before it at `first_inserted_at_ms` on the
  received arm — exact, since no producer stated a place then and no upsert
  rewrites that column; the file plane's next re-derivation states the
  recorded places.
- `write_ledger` — one row per write APPLIED AND STILL RE-READABLE (`write_id`
  PK, `upsert_key`, `write_seq`, `applied_at_ms`, `source_file_id`,
  `source_offset`), written in the same transaction as the row it applied and
  swept once it falls past the retention window below. **ABSORPTION ASKS THIS TABLE, NEVER `entry.write_id`.** `entry` holds
  only the LATEST write applied to a row, so probing it answered "is this the
  write that currently owns the row?" — and a producer replaying w1 after w2
  settled the same `upsert_key` read as never-seen, overwrote the newer content
  and bumped `write_seq`, re-delivering the regression to every live watcher.
- `detached_work` — **THE JOIN AND NOTHING ELSE**: `work_id`, `kind`,
  `origin_unit`, `owner_agent`, `announced_at_ms`, `ended_at_ms`, `terminal`.
  The announcement itself is a page line and is the one durable copy of what was
  announced (the spool, its readability, the detach cause, the timeout);
  unpacking any of that here too would give one fact two homes that can disagree.
- `workflow` — the table exists and **NOTHING routes into it this wave**.
- `vendor_task` — one row per (vendor task locator, agent): the `<id>` of a
  subagent's `agent-<id>.jsonl` (the stream's `task_id`) paired with the agent
  it names (the spawning call's `tool_use_id`). The sidecar states it on every
  batch of a subagent transcript (`EntryBatch.agent_locators`), it commits in
  the FIRST transaction of that batch, and a re-statement is absorbed.
  `GetAgentByVendorTask` reads it scoped to the caller's lineage, exactly as
  `GetLiveWork` is: not-found is an answer recorded at info (only the shim knows
  whether it expected one), and a locator paired with two agents of one lineage
  is refused as a storage failure at ERROR rather than chosen between. It exists
  because a SendMessage resume names the send, not the spawn, and a shim that
  restarted since the spawn has nothing else to name the running agent by.

- `cursor_conversion` — one row per cursor (`file_id` PK, `version`,
  `healing_through` NULL unless a re-derivation is in progress): the
  `CursorConversion` a file-plane cursor advance states, written in the
  advance's own transaction and served back left-joined by
  `GetSidecarCursors`. A cursor with no row here was stored before conversion
  versions existed and is served with its conversion UNSET, which the sidecar
  reads as version 0. It is an IN-PLACE table (`inPlaceTables`), built on a
  matching database the way the lineage indexes are, never by a version bump.

`agent`/`workflow`/`detached_work` are UNPACKED to columns because the store
filters and joins on them; `entry`'s frame stays a BLOB because activity
vocabulary is content, and unpacking it would drag every `conversation.v1`
change into DDL.

### `residue_shapes` — the catalog of what nobody stored

Owner ruling 2026-09-13 (`docs/REALTEST-JUDGEMENT-CALLS.md`, "the
unmodelled-line shape catalog"). The sidecar persists no residue, which takes
the bytes out of the store and with them the only evidence the vendor emits
that line at all. THE SHAPE IS KEPT INSTEAD: `residue_shapes` holds one row per
distinct recursive key structure — `shape_hash` (PK, SHA-256 over the
rendering), `kind`, `key_structure`, `first_example`, `first_seen_ms`,
`last_seen_ms`, `count` — so a human can ask what the vendor is emitting that
this system does not model.

- **THE TABLE HOLDS STRUCTURE, NOT CONTENT.** `key_structure` is key names and
  scalar TYPES with every value dropped; the producer owns the rendering and the
  wildcard rule, documented in the sidecar's AGENTS.md under "the residue shape
  catalog". `first_example` is the one verbatim line, and it is the deliberate
  exception: a shape nobody can read an example of is a shape nobody can act on.
- **NO RETENTION RULE, BY RULING.** The row count is bounded by the number of
  distinct shapes a vendor emits, not by traffic, so there is nothing here for a
  sweep to remove — unlike `write_ledger` below.
- **THE FIRST INSERT OWNS THE EXAMPLE.** A later observation raises `count` and
  takes a MAXIMUM of `last_seen_ms`, and touches nothing else. Maximum, not
  assignment: the producers are not ordered against each other and a replayed
  batch carries an old instant, so an unconditional write would walk a row
  backwards.
- **IT COMMITS IN THE BATCH'S OWN TRANSACTION** (`internal/db/shape.go`,
  `applyShapes`, called from `WriteBatch`). An observation is read from bytes
  whose cursor advance commits there; split them and an advance that survived a
  lost catalog write takes the shape with it, because nothing re-reads bytes
  already past the cursor. Every observation is validated BEFORE the transaction
  opens, so a malformed one refuses the batch whole. A batch carrying nothing
  but shapes is a real write and is accepted.
- **READING IT.** `ListResidueShapes` serves the catalog newest-seen first, with
  an optional `kind` filter (present-but-empty is refused, as everywhere), a
  limit bounded by the store's own default and maximum, and the example OPT-IN
  because it is the one column carrying raw vendor bytes. `make -C
  agent-shim/shim-store shapes` runs it (`cmd/shapes`, `ARGS="--kind unparsed
  --example"`), because a discovery surface reachable only by `sqlite3` is one
  nobody consults. Nothing in the running system reads the catalog.

### The write ledger is retained only as long as absorption can ask

The ledger answers ONE question — "has this write_id already been applied?" —
asked when a producer re-emits bytes it has already sent, so the store absorbs
the replay instead of re-upserting the row, bumping `write_seq` and
re-delivering a regression to every live watcher. It answered that for EVERY
write the store had ever applied, forever: 735k rows and 204 MB with its indexes
on the owner's box, almost none of which any producer could still ask about.

**THE RULE: a ledger row whose source offset is more than
`DefaultLedgerRetentionBytes` behind its file's COMMITTED cursor is removed.**

The safety argument is the sidecar's own bounds, and the exact form of it lives
in the comment at the top of `internal/db/prune.go`:

- The sidecar mints a DETERMINISTIC `write_id` from `file_id` + `offset` +
  discriminator, so only the same bytes at the same offset of the same file ever
  mint the same id.
- It re-reads sent bytes in exactly two ways. THE BOOT REWIND
  (`tail.RewindToTurnStart`, once per file per boot) moves the restored cursor
  back at most `tail.DefaultRewindWindow` — 4 MB — from the committed cursor, in
  ONE bounded backward scan that cannot reach further. THE HOLD advances the
  cursor short of what was read, and the held frame is by definition not yet
  written.
- So a row further behind than that window names bytes nothing will read again.

`DefaultLedgerRetentionBytes` is **16 MB, four times the rewind window on
purpose**: the two numbers live in different modules and are bumped by different
people, and a margin is cheaper than a cross-module coupling.

**WHAT IS NEVER SWEPT.** A row with no source file — every stream-plane write,
and any file-plane batch that advanced no cursor — is stamped NULL and kept: the
shim re-emits from an in-memory retry buffer whose bound the store cannot see,
and there is no structural argument for a cutoff, so there is no cutoff. A row
whose file has NO cursor row is kept too, because no cursor means that file is
re-read FROM ZERO, which is exactly when the ledger is doing the most work.

**THE SWEEP IS BOUNDED AND SHARES THE WRITE SLOT.** `DB.SweepWriteLedger` runs
`PruneWriteLedger` every `DefaultLedgerSweepInterval` (and once at start), which
deletes in transactions of `ledgerPruneBatch` rows, each taking and RELEASING
the serialized write slot — so a producer's batch waits at most one batch of
deletes, never a whole sweep. It is interruptible and commits as it goes; a
sweep cut short by shutdown keeps what it removed and returns the caller's
cancellation, not a storage failure. A sweep that removed rows is one info
record with the counts; one that removed nothing is verbose. `main.go` stops it
BEFORE closing the database.

**AND THE SWEEP'S DELETE IS DRIVEN FROM `cursor`, NEVER FROM THE LEDGER.** The
retention bound is `c.offset - ?` — a column of the OTHER table, not a constant
— so with `write_ledger` outermost `write_ledger_source` is usable for nothing
and SQLite reads the whole covering index probing `cursor` per row. That cost
111ms per batch on a 318k-row ledger and 1464ms on the loaded box for the ONE
batch that removed 870 rows, paid again by every later batch of the same sweep
and by the empty batch that ends it, all of it HOLDING THE WRITE SLOT. Driven
from `cursor` (2844 rows) the same index is an ordinary seek and the same sweep
batch is 3.7ms. The statement is `ledgerPruneDeleteSQL` at package scope and
`CROSS JOIN` states the order so the planner's row estimates cannot flip it
back.

**A PLAN IS THE ASSERTION, NOT A DURATION.** A full scan of that index measures
~200ms on a warm idle box — inside the 400ms write budget — and 1464ms on the
owner's, so a wall-clock bound passes on both plans and only production can tell
them apart. `TestTheSweepsDeleteSeeksTheLedgerRatherThanScanningIt` EXPLAINs the
production statement itself, and
`TestEveryStatementOfAWriteBatchSeeksRatherThanScans` does the same for every
statement in the write transaction, so a column added without the index it is
looked up by fails in the suite rather than in the owner's log. The wall-clock
budgets beside them (`TestAThirtyRowBatchOnAFullSizedCorpusStaysWithinItsOwnBudget`,
`TestASweepBatchAndAProducersBatchTogetherStayWithinTheProducersBudget`) are
proved against a 600k-row synthetic corpus and SKIP under `-race`: the detector
shadows every access, and the same pair that measures 5ms + 3ms uninstrumented
measured 413ms + 50ms under it, which is a number about the detector.

There is no `--` flag: the window is `Options.LedgerRetentionBytes`, which only
a test sets, and a NEGATIVE value disables the sweep entirely.

### The store is NUKED, never migrated

`db.Open` checks `schema_meta.version`; any mismatch, or any pre-existing table
set that differs, REMOVES THE DATABASE FILE and creates a fresh one. There is no
`ALTER`, ever, and no migration code. Writing migration code, or preserving a
stored shape on durable-compatibility grounds, is forbidden.

**THE NUKE IS AN UNLINK, NEVER A `DROP TABLE`.** Emptying a foreign schema in
place walks every page of what it discards: on 2026-09-09 the store met an
11.5 GB `events.db` at a superseded version, ran the DROP for minutes with no
socket listening, and the deploy of the day gave up waiting for `store.sock` and
left the sidecar and the runtime un-bounced. Unlinking costs the same whatever the
file weighs, so `ensureSchema` never drops: it returns a `schemaMismatchError`
naming what it found, and `Open` — the layer that owns the file — closes the
handle, removes the file with its siblings, and reopens onto an empty one. The
mismatch is recorded ONCE, from `Open`, naming the found version and table set
and saying the file was removed.

**WHOSE DATABASE IT WAS DECIDES THE LEVEL.** A stamp BELOW `SchemaVersion` is a
version this binary superseded, and meeting one is what an ordinary deploy that
bumped the schema does; with no retention during development (owner ruling
2026-09-13), recreating it is the convention working, so it is INFO naming both
versions. Everything else is ERROR and still nuked: a stamp at or above this
binary's is a database it cannot have created (a newer store was here, or the
same version carries a shape this one did not write), a database with no
`schema_meta` at all is somebody else's file, and a file this binary cannot read
is damaged. A recreate that fails after the superseded file has been unlinked is
ERROR too — there is no database at all at that point.

### An index is built in place, never by a version bump

AN INDEX IS NOT A SHAPE CHANGE. It holds nothing a query can observe, so adding
one never bumps `SchemaVersion` — a bump would nuke the owner's database to add
a lookup structure SQLite builds in place. The one in-place schema step is
`ensureIndexes` (`internal/db/db.go`): on every open of a matching database it
builds any `lineageIndexes` entry that is missing with `CREATE INDEX IF NOT
EXISTS`, through the write gate in one bulk transaction; a fresh database gets
them in `createSchema`'s own transaction. It only ever ADDS: nothing is dropped,
rebuilt or rewritten, and a database carrying every index is left untouched.

- **WHY THE LINEAGE INDEXES EXIST.** `GetLiveWork` walks a session's lineage
  through `agent.spawned_by_agent`, `agent.spawned_by_workflow`,
  `workflow.spawner_agent` and `detached_work.owner_agent`. With none of them
  indexed SQLite built four AUTOMATIC indexes from full scans on every call,
  264-498 ms each in the owner's log (2026-09-24/25). Each is an optimization and
  carries a comment saying so.
- **A FAILED BUILD NEVER NUKES.** The database it failed on is one this binary
  created, carrying the owner's rows. `ensureIndexes` records the failure once at
  ERROR and returns an `indexMigrationError`, which `Open` returns as-is instead
  of reaching the unlink.
- **A PLAN IS THE ASSERTION.** `assertNoAutomaticIndex` (`db_test.go`) fails a
  plan that builds an AUTOMATIC index, and the live-work, read, bash-run, write
  and sweep statements are each EXPLAINed against it from their production text
  at package scope. The live-work listings drive from the lineage with `CROSS
  JOIN`, because the other order built an automatic index over the materialized
  lineage.
- **A TABLE THAT ADDS A SHAPE NOTHING ON DISK HAS IS BUILT THE SAME WAY**
  (`inPlaceTables`, `CREATE TABLE IF NOT EXISTS`, `ensureInPlaceTables`): the
  rows already stored keep every meaning they had, and the table's reader states
  what its absence means. `cursor_conversion` is one: nuking the database to add
  it would also throw away every stream-plane row the shim wrote live, which no
  producer can rebuild. `shell_run_claim` (landed change 4, 2026-09-30) is
  another: one row per (vendor task id, run), written from
  `store.v1.EntryBatch.shell_run_claims` and answered by
  `GetShellRunClaims`, whose `owner` names the book holding the run's own
  `activity:` row and is unset while no producer has written it. The shape check compares the table set with the
  in-place tables taken out (`shapeTables`), because a database this binary
  created may still lack them.
- **ADDING ONE.** Append to `lineageIndexes` (or a sibling list applied the same
  way) with a comment naming the statement it serves; do not edit `schemaDDL`
  for it and do not bump the version.

### Throwing the database away on purpose: `bin/store-reset.sh`

THE STORE NEEDS NO RETENTION DURING DEVELOPMENT (owner ruling 2026-09-13), and
everything in `events.db` is re-derivable — the sidecar re-reads the vendor's
transcripts from offset zero once its `cursor` rows are gone, and the shim
re-observes the live stream. So a store that has grown past what its host wants
to carry is THROWN AWAY, never pruned: `../../bin/store-reset.sh` stops the
sidecar and the store, removes `events.db` with its `-wal`/`-shm` siblings, then
starts the store, waits for `store.sock`, and starts the sidecar.

- **THE SIDECAR GOES DOWN FIRST AND COMES UP LAST**, which is the daemon's
  deploy's recorded safe order (`daemon/internal/deploy/services.go`). The sidecar's reader positions live in the `cursor` table
  IN THIS FILE: one left running across the unlink writes into a deleted inode
  and holds positions for a database that never saw the records they claim.
- **THE STOP POLLS UNTIL LAUNCHD REPORTS NO PID.** `launchctl kill` returns when
  the signal is delivered, and unlinking the file out from under a store still
  draining a transaction is the race the script exists to avoid. A service that
  will not exit fails the reset with nothing removed.
- **THE GUARD IS `AGENT_REPL_STORE_RESET=1`, AN EXACT MATCH.** The intent is
  stated twice — the script must be asked AND the environment must say yes —
  because this deletes every stored record and the re-read afterwards is the
  owner's whole corpus. `0` and `no` are somebody saying no, which a truthiness
  test would read as yes.
- A DIRECTORY AT THE DATABASE PATH IS REPORTED, NEVER REMOVED — the same guard
  `db.Open` applies before it unlinks, for the same reason. An ABSENT database
  is not an error: the reset's postcondition already holds.
- `--keep-down` removes the files and leaves both services stopped.
- The hermetic harness is `../../bin/test-store-reset.sh` (suite
  `store-reset-harness` in `bin/test-all.sh`); it drives a `launchctl` stub
  through `AGENT_REPL_LAUNCHCTL` and a fixture `XDG_CACHE_HOME`, so no test run
  can ever reach the live launchd or the live database.

**A `--db` FILE THAT IS NOT A DATABASE IS IN THE WAY, SO IT GOES** — the same
answer, because the store holds a cache of what the vendor and the shim already
know how to produce again, and refusing to boot would wedge the service on bytes
nobody can read. It is removed with its `-wal`/`-shm` siblings (a stale WAL
beside a fresh database is how a "recreated" store comes up carrying fragments of
the one it replaced) and recreated, with an ERROR naming the cause. Two guards:
the nuke happens ONCE (a second failure is a real problem — an unwritable
directory, a full disk — and is returned), and only for a REGULAR FILE. A
directory at `--db` is reported, never replaced: unlinking whatever sits at an
operator-supplied path is how a service deletes somebody's data.

### THE STORE IS THE SINGLE WRITER, AND IT SERIALIZES ITS WRITES ITSELF

Nothing else opens this database by design — the store owns the file and serves
every producer over its socket — so every writer SQLite could ever arbitrate
between is one of this process's own goroutines. Leaving that arbitration to
SQLite meant two of the store's connections both issuing `BEGIN IMMEDIATE`, one
waiting out the whole `busy_timeout` and then being REFUSED: on 2026-09-13 the
sidecar's full re-ingestion after a store reset met the shim mid-write and
produced nine `store.db.write-batch` errors reading "begin write transaction:
database is locked (5) (SQLITE_BUSY)", each one a batch handed back to its
caller's retry, plus five `store.db.slow-query` warnings whose whole 5199ms was
the timeout being burned before the refusal.

So there is a PROCESS-WIDE WRITE SLOT (`DB.writes`, `internal/db/writer.go`)
and **`beginWrite` is the only way a write transaction is opened in this
package** — grep for `BeginTx` to check it. A batch WAITS ITS TURN, bounded only
by its own request context, and then writes; it is never refused for a BUSY
caused by a sibling.

- **The slot is a scheduler with a per-waiter grant channel, not a
  `sync.Mutex`**, precisely so a queued writer can be selected against
  `ctx.Done()`. A caller that hangs up while queued gets its own
  `context.Canceled` back — an `info` "abandoned" record, not an `error` — rather
  than a storage failure its producer would retry for a caller that is gone.
- **The release is deferred BEFORE the rollback**, so it runs after it. Handing
  the slot back while the transaction still held the lock would guarantee
  nothing.
- **`lock_wait_ms` now measures the IN-PROCESS QUEUE**, which is both the number
  an operator wanted and a truthful one: before the gate it was time inside
  SQLite's busy handler, which ended either in a write or in a refusal. A wait
  now always ends in a turn. Durations are read from `DB.mono`, an injectable
  monotonic clock, so a test asserts an exact wait by advancing it rather than
  by sleeping.
- **THE QUEUE HAS TWO TIERS, AND INTERACTIVE ALWAYS GOES FIRST** (owner's rule,
  2026-09-23: an interactive write never queues behind a bulk one). Every
  `WriteBatchRequest` carries `write_class` — INTERACTIVE from the shim, BULK
  from the sidecar — and an unset class (message or arm) is REFUSED at
  `write_class_unset`, never defaulted. The store's own writes state theirs in
  code: the ledger sweep, the WAL checkpoint's two WAL-index readings and schema creation are BULK. `DB.writes`
  (`writeScheduler`, `internal/db/writer.go`) hands a released slot DIRECTLY to
  the next waiter under its lock: the oldest interactive waiter, else the
  oldest bulk one.
- **THE FAIRNESS RULE.** Once a bulk writer is waiting, it is granted the slot
  after `InteractiveBurstBeforeBulk` (8) consecutive interactive grants, and
  the count restarts after each bulk grant and whenever no bulk writer is left
  waiting. So bulk gets at least one grant in nine under any interactive load,
  and the most an interactive write can wait on bulk is ONE bounded bulk
  transaction.
- **A BULK BATCH IS SPLIT BY THE STORE, AND THE STORE'S SPLIT IS
  AUTHORITATIVE.** `bulkBounds` (`internal/db/write.go`: 64 rows, 1 MiB of
  frames, 100ms of execution, whichever comes first, checked after each entry so
  a transaction always makes progress) caps one bulk transaction whatever size
  the producer sends, and the writer is yielded between them. The cursor
  advance and the shapes ride the LAST transaction, so a failure part-way may
  leave leading entries committed but never the advance past them; the re-read
  replays them into ledger absorption, and the server still publishes the lines
  that did commit. An interactive batch is one transaction, as before.
- **THE SWEEP IS BOUNDED IN WORK, NOT ONLY IN ROWS REMOVED.** One sweep
  transaction asks about at most `ledgerSweepCursorsPerBatch` (256) cursors,
  paged by `file_id`. Before that, a batch that removed little still walked
  every cursor, and on 2026-09-23 two sweeps held the writer for 137s and 848s
  on the owner's loaded host with no record of their own.
- **EVERY WRITE IS TIMED BY CLASS.** `store.db.write-timing` (verbose) records
  each write transaction's `write_class`, `lock_wait_ms` (the queue) and
  `exec_ms` (the rest), for `write_batch` and `ledger_sweep` alike; the
  slow-query record carries the same three and names the class in its message,
  and its budget window is kept per class so a bulk backlog cannot make an
  interactive spike read as persistent.
- **THE REFUSAL PATH SURVIVES FOR AN OUTSIDE WRITER.** Nothing else is supposed
  to open the file, but `sqlite3` at a shell, a stray second store racing the
  socket singleton check, or a backup tool all still can, and the DSN's
  `busy_timeout` plus the existing storage-failure refusal remain the answer for
  that. What the gate guarantees is only, and exactly, that a BUSY can never have
  come from this process.

### Any transaction that writes must BEGIN IMMEDIATE

The DSN carries `_txlock=immediate` (plus WAL, `busy_timeout`,
`synchronous(NORMAL)`). A write batch reads before it inserts, so a DEFERRED
transaction takes a WAL read snapshot and then tries to upgrade — and SQLite
will not run the busy handler for an upgrade: it returns `SQLITE_BUSY_SNAPSHOT`
(517) or `SQLITE_BUSY` (5) immediately, so `busy_timeout` never applies. One
store process serves every live producer on its own pooled connection, which
makes those collisions routine. Keep the DSN, and never add a read-then-write
transaction that begins DEFERRED.

**AND A PURE READ RUNS ON ITS OWN POOL, WHICH THE WRITE LOCK IS NOT REACHABLE
FROM.** There are TWO `sql.DB` handles for statements on the one file (a third,
the checkpoint connection, runs nothing but the WAL checkpoint; see below): the WRITE handle, capped
at `SetMaxOpenConns(1)` and reachable only through `beginWrite`; and the READ
pool, on a DSN with NO `_txlock` and with `query_only(true)`. `OpenPage`,
`ReadPage`, `BashRun`, `LiveWork` and every other pure read go through
`db.beginRead` or `d.read` directly, and SQLite itself refuses a write on that
pool — so a read path that grows one is a hard error at the first attempt rather
than a silent second writer the gate knows nothing about. The read pool is
opened AFTER the schema exists (`query_only` would refuse the DDL) and closed
with the write handle, neither failure swallowed.

The per-call-site `sql.TxOptions{ReadOnly: true}` below is kept and still
correct; the pool is what makes it unforgettable. `_txlock` is a property of the
CONNECTION, so a single DSN carrying `immediate` made every transaction a
writer, and one forgotten option would queue a page repaint behind a producer's
write exactly as before.

**AND A PURE READ MUST NOT.** `_txlock` is per CONNECTION, so it applied to the
read path too, and a page repaint therefore queued for — and could be REFUSED
by — the write lock: the owner's store answered two `OpenAgentSession` calls
with `store.db.open-page` ERROR "begin read transaction: database is locked (5)
(SQLITE_BUSY)" because a producer was writing. WAL exists precisely so readers
never contend with the writer. `OpenPage`, `ReadPage` and `BashRun` write
nothing, so they have no upgrade to fear, and they all go through
`db.beginRead`, which passes `sql.TxOptions{ReadOnly: true}` — the driver then
issues a plain `BEGIN` and the transaction takes only a read snapshot. The
watch pin is unaffected: a deferred transaction in WAL fixes its snapshot at
its FIRST statement and holds it to the end, so the pin and the page it
accompanies still come from one view of the database. Any NEW pure read opens
through `beginRead`; anything that writes keeps the DSN's `BEGIN IMMEDIATE`.

### Checkpoints are the store's own BULK job, never part of a commit

The write DSN carries `wal_autocheckpoint(0)`. Under SQLite's default, the
commit that took the WAL past 1000 pages checkpointed EVERY page anyone had
appended before its COMMIT returned. On a copy of the owner's 1.1 GB events.db
under concurrent sidecar ingestion, about 4% of interactive commits ran that
checkpoint inline (p50 28ms, p99 ~90ms), paying for bulk pages.

- **`DB.RunCheckpoints` (`internal/db/checkpoint.go`) is the only thing that
  folds the WAL back.** `main.go` runs it beside the ledger sweep and stops it
  BEFORE the database closes.
- **THE CHECKPOINT NEVER HOLDS THE WRITER.** It runs on a THIRD handle, the
  checkpoint connection (`DB.ckpt`): one connection, `query_only(true)`,
  `synchronous(NORMAL)`. A PASSIVE checkpoint takes SQLite's checkpointer lock
  alone and never the WAL write lock, so producers keep committing while it
  copies (`TestTheCheckpointConnectionCheckpointsBesideAnOpenWriteTransaction`,
  `TestAReadAndAWriteCompleteWhileACheckpointIsStalled`). It used to run on
  the write handle holding the one writer for its whole pass: on 2026-10-02,
  with the host at a load average of 33, an idle checkpoint of 114 frames took
  3123ms, the sidecar's batch committing a turn's transcript waited out all of
  it, and `TestPlanModeCoalescesOntoOneBubble` ran out of its 5s for the cursor
  to advance. Reads were never in the way: they have their own pool.
- **ONLY THE TWO WAL-INDEX READINGS TAKE THE WRITER.** A checkpoint reads the
  header before its pass and again after it, each through the BULK tier, for
  one 136-byte read. The header is written by every commit, so it is exact only
  under the writer. The second reading's salt answers the one race the
  separate connection meets: the pass copies the whole log, a producer's commit
  restarts it, and the pass then reads the NEW log's backfill mark. A changed
  salt therefore means the pass copied every frame it saw, because a restart
  needs every frame copied and nothing but this job checkpoints.
- **GROWTH TRIGGER.** Every writer's release (`releaseWrite`, `writer.go`)
  reads the WAL-index header from `-shm` while it still holds the writer. That
  is SQLite's documented WAL-index format: `mxFrame` and `nBackfill`, with both
  header copies compared. The job runs once `DefaultCheckpointPages` (1000)
  frames have been appended since the last checkpoint. The mark is the frame
  count at the last successful checkpoint, so a pass that a reader cut short
  is not re-run on every commit.
- **IDLE TRIGGER.** The job also runs once no writer has released for
  `DefaultCheckpointIdle` (2s) with frames still waiting. It is re-armed by
  every release and armed once at start for a WAL left from a previous run.
  Neither trigger is a bare timer.
- **PASSIVE, always.** FULL, RESTART and TRUNCATE take the WAL write lock and
  run the busy handler until every reader leaves the WAL, so they would block
  producers. PASSIVE never waits and never takes the write lock, and the growth
  trigger bounds its copy.
- **NO DEADLINE, on purpose.** SQLite advances `nBackfill` only after a whole
  pass has been copied and synced. An interrupted checkpoint therefore keeps
  none of its progress, and a deadline would retry it forever against a WAL
  that only grows.
- **`journal_size_limit` (16 MiB) is what shrinks the `-wal` file.** SQLite
  cuts the file back to the limit at the first commit after a complete
  checkpoint restarts the log, so a high-water mark like the owner's 119 MB no
  longer stays on disk. This was verified with the sqlite3 CLI: a 5.2 MB WAL
  stayed 5.2 MB after PASSIVE and dropped to the limit on the next insert.
- **LOGGING.** Each checkpoint that copied pages is one `info` record at
  `store.db.wal-checkpoint`. It carries `statement=wal_checkpoint`,
  `write_class=bulk`, `rows` (pages copied), `duration_ms`, `lock_wait_ms` (the
  two readings' bulk-tier queue, summed) and `exec_ms`, and its message names the trigger and the mode. A pass that
  copied nothing because a reader pinned the frames, and a trigger that found
  nothing waiting, are both verbose. A failed checkpoint is one `error` record
  and is retried at the next trigger: its mark does not move, so the next
  release re-runs it, and the idle trigger is re-armed. A failed WAL-index read
  is also one `error` record, and the idle trigger retries it.
- **A PIN THAT OUTLASTS ANY READ IS A WARNING (`store.db.wal-pin`).** One
  pass that copies nothing is a read in progress. An unbroken run of them that
  lasts `DefaultPinWarnAfter` (1 minute) is a leaked snapshot, and the WAL grows
  until it ends. `walPinWatch` follows the run, and the job writes one `warn`
  record when the run outlasts the policy and one `info` record when a
  checkpoint copies again. Both carry `wal_frames`, `wal_backfilled`,
  `wal_read_marks` (the WAL-index reader slots, from the second reading of the
  pass that copied nothing), `wal_pinned_for_ms`, and the read pool's
  `read_pool_open`/`read_pool_in_use`/`read_pool_idle`. Read mark 0 held with
  `wal_backfilled` 0 means a reader opened while the WAL was fully folded and
  never ended. On 2026-09-28 that went on for four hours with nothing above
  verbose: modernc.org/sqlite before v1.40.1 leaked a stepped statement when
  its context ended mid-query.
- **A READ'S SNAPSHOT ENDS BEFORE THE READ RETURNS.** `beginRead` begins on
  `context.WithoutCancel(ctx)`, so the rollback is always the caller's own
  deferred one, not database/sql's asynchronous rollback of a cancelled
  transaction. Every transaction ends through `endTx`, which records a failed
  rollback at `error`, since a connection left inside its transaction keeps its
  snapshot. `TestEveryTransactionEndsThroughEndTx` holds every begin site to it.
- **THE `-shm` DESCRIPTOR IS NEVER CLOSED WHILE SQLITE HOLDS THE FILE.**
  SQLite locks `-shm` with POSIX fcntl locks, and closing any descriptor a
  process holds on a file drops every fcntl lock that process holds on it. The
  store opens the descriptor once, on first use after the database is fully
  open, and `Close` closes it only after all three handles have closed.
- Test seams: `runCheckpoint` makes a checkpoint fail and then succeed, or
  holds it inside its pass, `checkpointDone` is what a test waits on, and `newCheckpointTimer` fires the
  idle trigger by hand. None of them sleeps.

### The page cache and the map are sized, and this is what they cost

| pragma | connection | value | heap cost |
| --- | --- | --- | --- |
| `cache_size` | writer (one, lives as long as the store) | -65536 (64 MiB) | up to 64 MiB, filled lazily |
| `cache_size` | each reader | -16384 (16 MiB) | up to 16 MiB per open reader; 32 MiB for database/sql's two idle readers |
| `mmap_size` | every connection | 256 MiB | none: this is the kernel's file cache, shared and reclaimable |

- **WHY 64 MiB FOR THE WRITER.** An upsert seeks four indexes by a hashed key
  (`entry.upsert_key`, `entry.write_id`, the ledger's primary key and
  `write_ledger_upsert_key`), which total about 140 MB on the owner's box
  (`dbstat`) and are hit at random. The `write_seq` and position indexes are
  only appended at their right edge. 64 MiB holds every interior page and about
  half of those leaves.
- **WHY READERS KEEP A SMALL CACHE AND USE THE MAP.** The read pool is not
  capped, and a reader's private cache dies with its connection, while the map
  is shared by all of them. Writes still go through the WAL. The file only
  grows (no VACUUM, and a nuke unlinks after closing), so nothing truncates a
  mapped region.
- `TestEveryConnectionCarriesTheCacheAndMapSizes` reads each pragma back on
  the writer and on two readers that are open at the same time.

**MEASURED on a read-only `.backup` copy of the live events.db (2026-09-23,
1.1 GB, schema 7).** The host was shared and at load averages of 60-370, and
test runs are niced, so the tails are noisy. The medians repeated.

| measurement | before | after |
| --- | --- | --- |
| upsert hashed-key probe pair, warm pass, p50 / p90 / p99 | 28.6us / 40us / 213-253us | 23.7us / 27-31us / 58-117us |
| interactive upsert, paced 1/ms, no bulk: p50 / p90 / p99 | 247us / 343us / 6.5ms | 206us / 292us / 5.8ms |
| interactive upsert, paced 1/5ms, concurrent bulk: p50 / p90 / p99 | 9.4ms / 33ms / 87ms | 6.7ms / 23ms / 51ms |
| interactive commits that ran a checkpoint inline (under bulk) | 163-167 of 4000 (p50 28ms, p99 86-90ms) | 0 by construction |
| one checkpoint, quiet (~1000 pages) | 6.7ms inline in the commit | exec p50 6-10ms, as bulk |
| one checkpoint, under bulk ingestion | inline in a bulk or interactive commit | exec p50 23-39ms, queue p50 0-5ms |

In the mixed run, interactive latency is dominated by queueing behind bulk
transactions of up to 100ms, which is the two-tier writer's documented bound
and not something this change touches.

## Routing, orderings, pointers, tokens

- **Pageability is the PRODUCER's decision**, read from exactly one place:
  `StoreAgentUpdate.agent_info`. `serveable_frame` names its book
  (`page_agent_id`) and is the ONLY thing a page can ever return;
  `unserved_item` (vendor_specific / unknown / unparsed) is durable and never
  served; `bash` and `workflow` are structurally not page lines.
- **THE `keepalive` ARM IS REFUSED** (`keepalive_retired`, on
  `entries[i].agent_update.unserved_item.keepalive`). The proto reserves its
  tag, so a stale producer's arm arrives as an unknown field
  (`carriesRetiredKeepalive`) and an old row's frame blob still decodes. Nothing of a keep-alive is
  stored on either plane (2026-09-23), and the rows written before it were not
  harmless: the shim that predated the rule tagged a backgrounded subagent's
  frames arriving during a keep-alive turn, so held keep-alive rows sat under
  real subagent upsert_keys, and the sidecar's page line for each was refused as
  an identity change (`keepalive` → `page_line`), parking the subagent's whole
  transcript.
- **A RETIRED `keepalive` ROW MAY BE SUPERSEDED BY A REAL RECORD OF ANY KIND**
  (`applyIdentityPolicy`), the one kind change that is not
  `upsert_changes_identity`: the real record takes the key and its book, and
  the store records it ONCE per key at INFO with `old_kind` and `new_kind`
  (once by construction — the row then holds a real kind). Every other kind
  change is still refused.
- **THE OTHER HELD KEEP-ALIVE ROWS STAY, AND THEY ARE INERT.** They carry no
  book, so no page, replay or watch can reach them; nothing reads them; and
  nothing can add to them, because the arm is refused. They are not deleted:
  the store is never migrated, and nuking `events.db` would also throw away the
  stream-plane rows the shim wrote live (asks, stream-only frames), which no
  producer can rebuild.
- One transaction per batch, and **failure commits nothing**. Per entry, in
  producer order: absorb by `write_id` against the **write ledger** (a hit is
  success), else check that the upsert does not change the row's IDENTITY, then
  upsert by `upsert_key` and route by arm. `success`/`failure` frames are a DUAL
  WRITE — the page line AND the agent row's terminal columns, in the one
  transaction. `cursor_advance` upserts in the same transaction, which is the
  whole exactly-once guarantee; **a cursor-only batch is LEGAL**, because a
  sidecar that read bytes yielding no entries must still advance or it re-reads
  them forever.
- **AN UPSERT SUPERSEDES CONTENT, NEVER IDENTITY.** A write whose `book_agent_id`
  or `kind` differs from the existing row's is refused
  (`upsert_changes_identity`). `upsert_key` names one thing, and the page model
  rests on it: moving a row between books would leave every pointer already
  served for it naming a line of a book the caller never asked about, and
  changing its kind would turn a served page line into unservable residue under
  a pointer that still exists.
- **A DETACHED-WORK ANNOUNCEMENT IS A PAGE LINE** of the announcing agent's book
  (`AgentFrame.detached_work`, keyed by the producer as `detached:<work id>`),
  and it is the SOURCE of `GetLiveWork.live_detached`. "Work left this stream" is
  the handoff a reader must see; a feed that drew the spawning call without it
  keeps claiming work that is no longer in the turn.
- **A BASH FRAME IS ITS OWN ENTRY ROW** (`kind = bash`, book NULL,
  `run_id = StoreAgentBash.run.value`) as well as a `detached_work` update, so
  the run's rows live in the spine under their own indexed key the way a book's
  do. Producers key the rows (`bash:<run>:start`, `bash:<run>:tail`,
  `bash:<run>:terminal`); **the store never parses a key.** It is still not a
  page line: a run has no book, and its reader is `WatchBashRun`.
- **A BASH RUN'S FILE-PLANE TERMINAL OUTRANKS ITS STREAM-PLANE TERMINAL**
  (owner ruling 2026-09-27, `fileTerminalHeld`). The one exception to "the last
  write supersedes whole": a stream-plane write whose bash frame is terminal
  (`success`/`failure`) landing on a row that holds a FILE-plane terminal is
  absorbed — counted in `Absorbed`, no upsert, no ledger row, no watcher row —
  and recorded ONCE per refused write at INFO. The sidecar's terminal carries
  the spool's exit code and output; the shim's says only that the run ended.
  A file-plane terminal still supersedes a stream-plane one, file supersedes
  file, stream supersedes stream, and every non-terminal or non-bash row is
  unchanged.
- **EVERY FILE-PLANE ROW RECORDS THE CONVERSION THAT PRODUCED IT**
  (`StoreEntry.conversion_version`). A file-plane entry without one (or with 0)
  and a stream-plane entry with one are refused, and a file-plane cursor advance
  must state its `CursorConversion`. A row stored before the field existed
  reads as version 0.
- **A FILE-PLANE WRITE OF UNCHANGED CONTENT IS A RESTAMP**
  (`sameContentBarVersion`, `restamp`). When the only difference from the row
  is the conversion version (and the write id, which digests it), the row takes
  the new write id, version and frame and a ledger row, but its `write_seq` is
  NOT bumped and nothing is published: a re-derivation that changes nothing is
  invisible to every reader.
- **A RE-DERIVATION RETIRES WHAT A RECORD NO LONGER CONVERTS TO**
  (`EntryBatch.retirements`, `retire.go`). Retirements ride the batch's FINAL
  transaction, after every entry and beside the cursor advance they require.
  A row is retired only when it is a page line the FILE plane last wrote under a
  conversion version strictly below the retirement's; an absent key, an
  already-retired row, a non-page-line row, a stream-plane row and a row this
  version produced are left, each traced at VERBOSE, and none is a refusal. A
  line whose content drove a lifecycle table (an activity, a terminal, a
  detached announcement) is never retired and the store says so at ERROR: only
  a prompt, a peer message and a non-activity agent update are. A retired row
  is a `kindRetired` TOMBSTONE, not a delete: its `write_seq` is bumped, it
  keeps its book, position and last frame, every page stops serving it, every
  standing watch is sent it once on the `retired` arm (a watch opened later
  replays it by `write_seq`), its pointer stays valid, the ledger is untouched,
  and a later write of a real record under the same key takes it back in place.
  Each retirement is recorded once at INFO.
- **A LOST TERMINAL NEVER LANDS ON A RUN WHOSE ENDING IS ON RECORD**
  (`internal/db/lostguard.go`, 2026-09-30). A FILE-plane LOST terminal — a
  shell run's `interrupted.cause.lost` bash frame, or a backgrounded
  subagent's `failure.cause.lost` spawn activity — for a run whose
  `detached_work` row (by origin unit) has already ended refuses the batch
  whole as `invalid_request` naming `entries[i]` at `lost_over_settled`. The
  recorded terminal and `ended_at_ms` stand. The sidecar asks
  `GetRunSettlements` before tracking, so this is an INVARIANT VIOLATION and the
  server records it at ERROR, not at the warn an ordinary malformed request
  gets. A LOST over a live row, a stream-plane LOST, and a non-LOST file
  terminal are untouched.
- **ONLY WHAT IS RENDERED IS STORED** (owner ruling 2026-09-23). A run's output
  is ONE rendered-tail row (`AgentBash.tail`) every write supersedes, and a
  tail longer than conversation.v1 `AGENT_BASH_TAIL_CAP_BYTES` — the one
  constant the sidecar cuts by and the daemon draws by — is refused
  (`bash_tail_over_cap`, on `…bash.frame.tail.text`). Every supersession still
  reaches a live `WatchBashRun` watcher as a row of its own; a replay serves
  the newest window at the tail's first-insert position.
- **A RETIRED-ARM ROW IS OUTMODED, NOT DAMAGE.** Rows written under the retired
  contiguous-delta arm (`AgentBash.update`, reserved 2; keys
  `bash:<run>:<from_offset>`) are left in place untouched. They decode to NO
  result arm — which no write can produce, since an armless frame is refused —
  so `BashRun` skips them and states the count ONCE PER REPLAY at INFO, never
  one record per row and never at ERROR. A run holding only such rows replays
  nothing and is the ordinary unknown-run refused open.
- **ONE `detached_work` ROW PER RUN, LOCATED BY ORIGIN UNIT FIRST.** The
  announcement addresses a run by its `DetachedWorkId`; the run's own frames
  address it by its `AgentActivityId`. Both writers resolve the row the same way
  — if any row already carries this origin unit, that row IS the run; otherwise
  the writer's own identity keys it and the later writer finds it through the
  same lookup. Symmetric, because the file plane can observe a spool before the
  stream plane announces it. (Landing 4 mints the two to the same bytes; the
  lookup still converges on one row when they coincide.)
- `agent_update.workflow` lands durably as `kind=workflow` with a WARNING that
  workflow ingestion is not implemented this wave; a workflow-kind ANNOUNCEMENT
  is a page line like any other and raises the same warning. Nothing touches the
  workflow table. `GetWorkflow` answers the typed not-implemented failure.
- **RESIDUE MUST CARRY ITS VERBATIM RECORD.** `vendor_specific` and `unknown`
  with no `raw`, and `unparsed` with empty `raw`, are refused
  (`residue_raw_unset`): a row saying only "there was something here" IS the drop
  the residue arms exist to prevent, and the follow-up work — a converter, a
  model, a parser fix — is impossible without the bytes. A keep-alive is exempt;
  it is a well-formed fact with no book, not material that failed to convert.
- **THE ENVELOPE AND THE FRAME MUST AGREE.** A page line whose `page_agent_id`
  differs from the frame's own agent (`AgentFrame.agent_id`, `AgentPrompt.agent`)
  is refused (`page_book_mismatch`). They are two statements about one line and
  the store cannot pick a winner; accepting either files an agent's words under
  another agent's name.
- **A BOOK IS SERVED IN DESCENDING CONVERSATION PLACE** — `(at_ms, ordinal)`
  from `entry_place`, then `position` as a stable tiebreak that carries no
  meaning — never in arrival order, which diverges from it whenever records
  reach the store out of their own order. A stated place never moves, so a
  unit settling mid-walk cannot teleport across a continuation; only a row with
  no stated place moves, once, when it gains one.
- **A POINTER NAMES AN ITEM, NEVER A PLACE.** Pointers are opaque encodings of
  `position` (first-insert order), echoed verbatim and stable across upserts
  and across the item gaining a recorded place; a pointer that names no row IN
  THAT BOOK is a stale-pointer refusal. No `seq` exists on any wire.
- **THE PAGE POSITIONS.** `ReadAgentPage.after` serves the lines placed
  strictly BEFORE the named line's CURRENT place (read in the page's own
  transaction). `ReadAgentPage.through` serves the newest lines placed AT OR
  BEFORE `through.at_ms`, whatever the ordinal — the book as it stood then — and
  refuses a book with no `agent` row as `unknown_agent` (recorded at `info`, like
  the open's). An unset position (`position_unset`, field `position`) and a
  non-positive bound (`through_not_positive`, field `through.at_ms`) are
  invalid requests. `OpenAgentSession` repaints the newest places; its
  `known_through` catch-up serves the lines FIRST WRITTEN AFTER the mark
  (`position > mark`, write order, not place), ordered by descending place, so
  a late-written line placed earlier is delivered rather than skipped.
- **ONE PAGE SIZE, THE STORE'S: `db.PageSize` = 50 lines** (owner ruling,
  `docs/protobuf-design/feed-paging-on-demand.md` change 1). Every page — the
  open's, a continuation, a `through` read — holds at most that many lines, and
  no request field carries a budget (`page_size` is retired on both verbs), so
  no caller can make a page bigger or smaller. A test that needs a boundary
  writes `db.PageSize`+N lines; nothing restates the number.
- **`tail_only` OPENS ON NO LINES.** The page is empty, the token pins the tail
  at the global `write_seq` read in the same transaction (exactly after the
  newest line as of the open), and the boundary is **`floor`**: `more` must
  name the page's oldest line and an empty page has none, while no pointer can
  name "the top of the book" (`after` reads strictly BEFORE its line). The floor
  of a tail-only page therefore means "nothing to walk FROM this page", not "the
  book is empty"; a reader that later wants history starts with a repaint and
  walks older with `ReadAgentPage` from its `more`. A tail-only open still asks
  the `agent` register, so an unknown agent is `unknown_agent`.
- **EVERY OPEN NAMES THE BOOK'S NEWEST LINE** (`OpenAgentSessionSuccess.newest`,
  feed-paging change 5), for all three openings and for `page_only`; unset is
  an empty book. It is the newest line BY PLACE — the line a repaint leads
  with — read in the same transaction as the page and the pin
  (`newestLineSQL`). A tail-only caller anchors on it: its teardown head, its
  lossless re-open mark (a catch-up from it is a superset of what was written
  since), and whether the book was empty.
- `OpenAgentSession` answers the page plus a store-minted `AgentSessionToken`
  (128 random bits from `crypto/rand`, hex) — **UNLESS THE REQUEST SAID
  `page_only`**, which is the caller stating that no watch follows: nothing is
  minted and `watch` comes back UNSET. The caller has to state it because the
  store cannot work it out — the rpc is unary and the service has no close, so
  a token minted for a page that is then abandoned can never be reclaimed and
  lives for the whole process lifetime (measured at two per turn, 2026-09-10;
  every one-shot read in the shim — the turn's opening page, the teardown's
  book head, the reconciliation read, the live-work re-announcement — now says
  `page_only`). A watch attempted from such an open has no token to present and
  meets the ordinary unknown/empty-token refusal; there is no arm of its own.
  The token is **SINGLE-USE**:
  `WatchAgentSession` consumes it, and an unknown, consumed, or
  previous-process token is refused. Tokens live in memory and do not survive a
  restart — "you already used this" and "the store restarted" are the same
  recovery for the caller: re-open. **An agent the store HAS HEARD OF with no
  rows is a legal EMPTY book** (empty page, floor, valid token) — a freshly
  spawned subagent has an `agent` row from its spawn frame before it says a
  word, so it is openable and watchable immediately.
- **AN AGENT ID THE STORE HOLDS NO `agent` ROW FOR NAMES NO BOOK AND IS
  REFUSED** with `OpenAgentSessionFailure.unknown_agent` (landing 7), never
  served an empty page; the shim maps it to `CodeNotFound`. Serving it told a
  caller with a stale or mistyped target exactly what it told a caller watching
  a live agent that had not spoken yet, so the two were indistinguishable and
  the mistake looked like patience. THE `agent` TABLE IS THE REGISTER that
  separates them — every page-line write ensures a row there, so "no row" is
  "never heard of" — and it is asked BEFORE the pointer, because a
  `known_through` against a book that does not exist is stale only as a
  consequence, and `stale_pointer` would send the caller off to repaint a book
  nobody ever kept. It is its own refusal class (`db.ErrUnknownAgent`) with its
  own site and arm, both spelled `unknown_agent`: the request is well formed and
  respelling it cannot help, so it is neither `invalid_request` nor a race to
  retry. `ReadAgentPage` with `after` carries a pointer, which is already stale
  for a book that does not exist; with `through` it carries none, so it asks
  the register exactly as the open does.
- The watch pin is the global `write_seq` at the moment the page was read,
  taken INSIDE that read transaction. `WatchAgentSession` then **subscribes to
  the fan-out BEFORE running the replay query** and dedupes by `write_seq`, so
  the replay-to-live handoff is gapless and duplicate-free. Upserts of old rows
  stream at their ORIGINAL pointer. No keep-alive or residue row is ever
  streamed, and there is no terminal frame: the stream ends only when the
  client cancels or the store shuts down.
- `WatchBashRun` replays every stored row of a run in **FIRST-INSERT order**
  (`position`), then follows live rows, and ENDS after a `success`/`failure` row
  — including when the terminal was already stored, in which case the replay IS
  the whole answer. **THE REPLAY SERVES EVERY STORED ROW AND THE END FIRES AFTER
  THE LAST OF THEM**, not at the terminal: a delta the sidecar reached only once
  the spool was already closed is first inserted AFTER the terminal's row, and
  stopping there handed the consumer a run that produced less output than it did.
  A terminal re-upsert against an ended stream is absorbed silently — nothing is
  re-sent to watchers that already ended, and a fresh replay still serves the
  terminal exactly once. The order is `position` and not `write_seq` because a run's
  rows are a spool being filled in: a redelivered delta upserts its row and must
  appear where it always was. (`WatchAgentSession` replays by `write_seq` for the
  opposite reason: a book's upsert is NEW INFORMATION about a line the caller has
  already read past.) A run the store holds no row for is a refused open at the
  transport (`CodeNotFound`) — the absence of a row IS the unknown-run signal, so
  no sentinel crosses the db/server contract; an empty run identity is
  `CodeInvalidArgument`, because the caller must fix the request rather than
  conclude the run does not exist. There is no token: a run is addressed by the
  identity the spawning stream already announced.
- Backpressure: a bounded per-subscriber channel of `--watch-buffer` frames.
  Publishing is a non-blocking send under the fan-out lock, so one slow reader
  can never stall a writer's acknowledgement. On overflow the store logs a
  WARNING and **ENDS that stream with `CodeResourceExhausted`**; recovery is a
  re-open with `known_through`. Never silently thin a subscriber.
- **`AgentFrame.detached_work` IS A PAGE LINE FOR EVERY `DetachableWork` KIND,
  WORKFLOW INCLUDED.** "Work left this stream" is the handoff the announcing
  agent's book has to show, and it is the one durable copy of what was announced;
  drawing the spawning call without it leaves the feed claiming work that is
  still in the turn. The workflow kick affects only what is SERVED BACK:
  `live_detached` excludes workflow and `GetWorkflow` answers `not_implemented`,
  while the announcement itself is stored and paged like any other.
- `GetLiveWork` is the `ended_at IS NULL` scans. "Live" is a claim about the
  RECORD — a start was written and no terminal ever was — so it is timeless and
  cannot go stale. Main agents are never listed; `live_workflows` is empty this
  wave.
- **`GetRunSettlements` IS THE SETTLED HALF, BY RUN AND UNSCOPED**
  (`internal/db/settlement.go`, 2026-09-30). The sidecar asks it before its
  LOST policy tracks a detached run's file, by the spawning call's activity id
  (the row's `origin_unit`). A run is answered only when EVERY row its origin
  unit locates has ended, with the latest `ended_at_ms`; a run absent from the
  answer is NOT settled (no row yet, or a row still live), never an error.
  Empty `run_ids` or an empty id is refused (`run_id_empty`). Design record:
  `../../docs/protobuf-design/run-settlements.md`.
- **`GetDetachedWork` IS ONE UNIT'S KIND AND END, BY UNIT AND UNSCOPED**
  (`internal/db/detachedwork.go`, 2026-10-06). The shim asks it before folding a
  task message whose kind it does not hold (a `task_notification` never states
  one), by the spawning call's activity id (the row's `origin_unit`), exactly
  as `GetRunSettlements` locates a run. The kind is served as recorded: the
  `detached` marker is the `unstated` arm, never a guess. No row is
  `not_found`; two rows for one unit, or a kind the store never writes, is a
  storage failure at ERROR. An unset unit is refused (`unit_empty`). Design
  record: `../../docs/protobuf-design/task-kind-from-store.md`.
- **`GetLiveWork` IS SCOPED TO ONE SESSION, AND NEVER ANSWERED UNSCOPED.** This
  store serves every workspace and session on the host, and the shim writes a
  closing terminal for every item its own vendor does not hold — so on
  2026-09-23 an unscoped answer let one workspace's start reap five running
  subagents of another. The request's REQUIRED `session` is the caller's main
  agent; a request without it is refused (`session_empty`, `invalid_request`
  naming `session`) before any statement runs. The answer is that session's
  LINEAGE: `sessionLineageCTE` (`internal/db/live.go`) is the ONE place that
  decides ownership — the main agent plus every agent reached transitively
  through `spawned_by_agent`, or through `spawned_by_workflow` via the
  workflow's `workflow.spawner_agent` or its `detached_work.owner_agent` — and
  detached work counts when its `owner_agent` is in that set.
- **AN OPEN OBLIGATION NO LINEAGE REACHES IS NEVER GUESSED INTO ONE.** A live
  `detached_work` row with a NULL `owner_agent`, or a live agent whose spawn
  column names a spawner the record does not hold, is excluded from every
  answer and written at ERROR (`store.db.live-work.unscoped`) naming each row,
  on every read that finds it. A live agent with NEITHER spawn column is
  indistinguishable from another session's main agent and is not reportable.
- **A DETACHED ROW'S SPECIFIC KIND IS FINAL.** `detached_work.kind` changes
  only out of the unspecific `detached` marker. A later write naming a
  different specific kind (a shell terminal on a subagent's handle) still
  lands and still closes the row, but never relabels it, and the disagreement
  is written at ERROR (`store.db.detached-kind-conflict`) naming both kinds.
  On 2026-09-23 a shim reconciliation that closed spawn units it could not
  find with shell terminals had rewritten eight subagents and three monitors
  as `bash`.

## Refusal sites

Every response is `oneof result { success | failure }`. `detail` strings are
for humans and logs and are **never switched on**. The sites below are the real
places the store says no; they are the vocabulary the proto's failure `kind`
arms are derived from, and each one is logged once with `refusal_site`.

`producer_empty`, `batch_missing`, `batch_empty`, `entry_plane_unset`,
`entry_write_id_empty`, `entry_upsert_key_empty`, `entry_arm_unset`,
`cursor_file_id_empty`, `agent_id_empty`, `pointer_empty`,
`token_empty`, `unknown_watch_token`, `file_id_empty`, `run_empty`,
`unknown_bash_run`, `store_refused_request`, `upsert_changes_identity`,
`keepalive_retired`, `bash_tail_over_cap`,
`page_book_mismatch`, `residue_raw_unset`, `stale_pointer`, `unknown_agent`,
`session_empty`, `write_class_unset`, `database_failure`, `workflow_not_implemented`, `watch_buffer_overflow`,
`listen_occupied`, `place_not_positive`, `through_not_positive`, `position_unset`,
`run_id_empty`, `lost_over_settled`.

- **THE SITE IS NOT THE ARM.** A site says which of the store's many checks said
  no — the vocabulary an operator counts by — while the failure's `kind` arm says
  which typed answer the caller receives, and several sites map to one arm.
  Keeping them apart is what lets a new site be added without inventing a wire
  arm. The sites `internal/db` decides live in `internal/db`, and
  `internal/server` ALIASES the constants rather than restating them.
- **EVERY REFUSAL SETS ITS `kind` ARM AND NAMES THE FIELD IT BLAMES.** `detail`
  is prose; a caller switches on the arm and reads `invalid_request.field`, which
  is the store's own name for what was wrong, with the entry index where one
  applies (`entries[1].upsert_key`).
- **`invalid_request.field` IS A FULL ENVELOPE PATH.** The vocabulary is owned by
  the store — `internal/db` for anything inside an entry, `internal/server` for
  the request around it — and every path is walkable from the message the
  producer sent, so a caller never has to guess the top of it. The forms are:
  request-level (`producer`, `batch`, `agent`, `book`, `session`,
  `known_through`, `after`, `position`, `through.at_ms`, `watch`, `run`, `file_id`, `work`); batch-level
  (`cursor_advance.file_id`); and entry-level, always rooted at
  `entries[i]` — `entries[i].write_id`, `entries[i].upsert_key`,
  `entries[i].plane`, `entries[i].entry`, `entries[i].agent_update…`,
  `entries[i].session_update`. Anything inside a page line's frame carries the
  whole prefix it is reached through:
  `entries[i].agent_update.serveable_frame.agent_item.agent_frame.agent_id`, not
  `agent_frame.agent_id`. A path that starts halfway down names a field that
  appears nowhere in the request. `internal/server` never opens a frame, so
  the site and the field for anything INSIDE one come up from `internal/db`
  through the refusal it returns. Per verb: `WriteBatch`
  invalid_request|storage_failure (a stale pointer is unreachable — the verb
  names no position); `OpenAgentSession`
  invalid_request|stale_pointer|storage_failure|unknown_agent; `ReadAgentPage`
  the same four (`unknown_agent` only for a `through` read); `GetLiveWork`
  invalid_request|storage_failure (the request must name its session); `GetSidecarCursors`
  invalid_request|storage_failure; `GetWorkflow` not_implemented, the one honest
  arm while nothing routes into the workflow table.

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
  path. `logging_bypass_test.go` enforces that boundary; its only sanctioned
  writers are `main.go:reportFatal` and
  `internal/logging/logging.go:writeFull`.
- `AGENT_REPL_LOG_LEVEL` is the ONE process-wide threshold:
  `debug|info|warn|error`, default `info`. An invalid value is a bootstrap
  failure before the log directory or file is created. Production passes the
  parsed `agentrepl/logging.Level` to `logging.NewDurableOnlyAtLevel`; tests
  and foreground harnesses may use `logging.NewAtLevel`.
- The canonical store sink is
  `$XDG_CACHE_HOME/agent-repl/log/shim-store.log` (or
  `~/.cache/agent-repl/log/shim-store.log` when `XDG_CACHE_HOME` is unset),
  unless `--log` overrides it. Production does not mirror ordinary records to
  launchd stderr.
- Every logical branch logs its selection: verbose for the ordinary path,
  `warn` for degraded-but-handled, `error` for failures. **Every error is
  logged exactly once, by its owning layer — and WHO OWNS IT DEPENDS ON WHOSE
  FAULT IT IS.**
  - A REFUSED REQUEST (`ErrInvalid`, `ErrStalePointer`, `ErrUnknownAgent`)
    belongs to the CALL. `internal/db` traces it at VERBOSE — its statement and
    table are context, and it can name neither the rpc nor the request id nor
    the producer — and `internal/server` writes the single normal-level record.
    Emitting both put two normal-level records on one refusal and made the rule
    false wherever anyone counted.
  - **THE LEVEL OF THAT RECORD IS A PROPERTY OF THE REFUSAL CLASS**, read from
    `refusalClass.logLevel` beside `armName`, never written at a call site and
    never switched on a site or a message string. Every class is `warn` —
    something is wrong somewhere — except `unknown_agent` and `unknown_run`,
    which are `info`. `unknown_run` is `WatchBashRun` asked for a run with no
    row yet: the shim that announced the run re-asks until the sidecar's rows
    land, so the refusal is the same ordinary answer (its `refusal_kind` is
    `not_found`, the Connect code, because the verb has no failure arm).
  - **`unknown_agent` IS AN ANSWER, NOT A FAULT.** `OpenAgentSession` is the one
    verb that asks whether a book exists, and the two populations that reach the
    refusal — a consumer opening against an agent whose first row has not landed,
    and a consumer holding a stale or mistyped target — send byte-identical
    requests. The store cannot tell them apart because the expectation lives in
    the CALLER: the shim serves an empty page and traces it at debug when it
    vouches for the agent (`store/reader.ts`'s `openBook` /
    `readFirstPageOnce`), and surfaces the typed arm as a real error when it does
    not. A `warn` claimed a fault the store is not in a position to claim, and it
    fired on every cold bring-up. The record is still written, at normal
    verbosity, with `refusal_site` and `refusal_kind`, so nothing an operator
    counts is lost. If a future caller must be told apart AT THE STORE, that is a
    new request field stating the caller's expectation, not a level change.
  - **A STALE POINTER IS NOT AN ERROR.** It is an ordinary race — the caller
    walked a book that moved — and its recovery is a repaint. It is a `warn` and
    never an `error`, because a healthy store writing error records during normal
    operation is how an error log stops being read.
  - A STORAGE FAILURE is `internal/db`'s own, with statement and table context
    nothing above can supply: its `error` record stays there and the server adds
    only a VERBOSE trace tying the rpc to it.
- **The caller's request id reaches the storage layer through the CONTEXT**
  (`logging.ContextWithRequestID`), not through a logger: the db's logger is built
  once at boot and belongs to the process, while a request id belongs to one call
  in flight. Every statement family the store runs emits a verbose
  `store.db.statement` record carrying it — which is what lets a test assert that
  a REFUSED request never opened a transaction. No `Store` method signature
  changed to carry it.
- Correlation keys, each in its own `context` field and never left to the
  message text: `producer`, `agent_id`, `vendor_session_id`, `book_agent_id`,
  `write_id`, `upsert_key`, `position`, `write_seq`, `watch_token_hash`, `rpc`,
  `refusal_site`, `refusal_kind`, `file_id`, `path`, `offset`, `task_id`,
  `activity_id`, `turn_id`. Top-level `request_id` comes from the `X-Agent-Repl-Request-Id`
  header when a caller sends one. `rpc` is the Connect procedure spelled
  exactly as Connect spells it, leading slash included
  (`/store.v1.ShimStore/WriteBatch`).
- Every request about one agent/book binds `agent_id` and `book_agent_id` to
  the request logger so every record inherits them. An aggregate WriteBatch
  binds the singular keys when unambiguous and sorted `agent_ids` /
  `book_agent_ids` arrays when the batch spans identities; it never chooses an
  arbitrary identity from a mixed batch.
- **EVERY REFUSAL RECORD CARRIES BOTH `refusal_site` AND `refusal_kind`.** The
  site is which of the store's checks said no; the kind is the wire arm the
  caller received (`invalid_request`, `stale_pointer`, `storage_failure`,
  `not_implemented`, `unknown_agent`), derived from the refusal's class in `logRefusal` and never
  restated by hand. Several sites map to one arm, so a record naming only the
  site leaves a reader unable to tell whether the caller could ever have retried,
  and one naming only the arm leaves it unable to find the check that fired.
- **The retired addressing keys are DEAD**: `claude_session_id`, `seq`,
  `from_seq`, `replay_*_seq` and anything else naming the old `(session_id,
  seq)` addressing. Do not add one back.
- A **watch token is never logged** — only `watch_token_hash`, a sha256 prefix.
  A token is a capability.
- `offset` is a `*int64` on purpose: zero is a meaningful file offset and must
  be reported, not omitted as unset.
- Request boundaries, state decisions, and per-record/per-batch success
  diagnostics go through `logging.Logger.LogVerbose`, which emits
  `level=debug`, `verbosity=verbose`. Service lifecycle edges are `info`;
  invariant violations and refusals are `warn`, except the `unknown_agent`
  class, which is the `info` above; owned failures are `error`.
  Slow queries are the
  one deliberate exception: a statement past its budget emits a
  normal-verbosity record at `store.db.slow-query` with `statement`,
  `duration_ms`, `lock_wait_ms`, `rows`, `threshold_ms` and
  `over_budget_recent`/`over_budget_window`, because by the time an operator
  knows to look the stall is over. The budget is NOT one fixed number. A point query's budget is the fixed
  `AGENT_REPL_STORE_SLOW_QUERY_MS` (default 250ms). A `write_batch` is bulk
  background I/O, not a point query — every write-path statement is fully
  indexed (`MAX(write_seq)` is a covering-index seek; the `write_id` and
  `upsert_key` probes and the upsert's conflict target ride unique indexes), so
  a slow batch is index-maintenance and WAL I/O scaling with the row count, not
  a query defect. Its budget is therefore `AGENT_REPL_STORE_BULK_BASE_MS`
  (default 250ms) plus `AGENT_REPL_STORE_BULK_PER_ROW_MS` (default 5ms) per row,
  floored at the interactive threshold, so a healthy large batch on a large
  database does not warn while a pathological per-row cost (a reintroduced scan,
  a lost index) still does; `threshold_ms` on the record is the budget actually
  applied. `statement` is a FAMILY NAME, never rendered SQL and never bound
  values — the payloads are opaque to the store, and quoting a parameterized
  statement would put session content into the global log. A malformed
  threshold or bulk-budget value aborts `db.Open`; a zero base is allowed
  (budget purely per row), but a non-positive per-row budget is refused.
- **`lock_wait_ms` SPLITS THE QUEUE OUT OF THE DURATION, and the record is
  unreadable without it.** A batch's clock starts BEFORE its transaction, and
  every transaction that writes here is `BEGIN IMMEDIATE` — so a write
  queues behind whatever else holds the write lock for as long as
  `busy_timeout` allows, and behind the connection pool before that. Reported
  as one number, that queue read as a slow statement: the owner's store logged
  `write_batch duration_ms=3822 rows=6` for six single indexed seeks, and the
  record's own reasoning blamed index maintenance for time no index spent.
  `duration_ms` and the budget still cover the TOTAL, because a batch nobody
  can start is as slow to its caller as one that runs slowly; `lock_wait_ms` is
  what tells the operator to look at what ELSE is writing rather than for a
  missing index. It is emitted with every statement family, zero included — a
  statement with no wait to measure reports `0`, which is a fact, not an
  omission.
- **A SINGLE OVER-BUDGET SAMPLE IS `info`; A FAMILY THAT IS PERSISTENTLY OVER
  BUDGET IS `warn`.** `duration_ms` is WALL CLOCK, and wall clock on a shared
  host measures the host as much as the statement — modernc's SQLite runs
  in-process on the calling goroutine, so a descheduled goroutine and a slow
  statement produce the same number and the store cannot tell them apart from
  one sample. Measured on 2026-09-13: fifteen `slow-query` warnings in one
  hour, every one `lock_wait_ms=0` and the largest `duration_ms=2543 rows=25`,
  while the SAME batch shape against a byte-for-byte copy of that same 1.65 GB
  database ran in 1-2ms — 400 consecutive 7-row batches with six concurrent
  page readers and a 113 MB WAL had a worst case of 15ms, with begin,
  `MAX(write_seq)`, the probes and the commit each sub-millisecond. What the
  budget exists to catch is a property of the STATEMENT — a lost index, a
  reintroduced scan — so it makes every statement of the family slow; host
  contention takes whichever statement was unlucky. So the store keeps the last
  `BudgetWindow` (16) verdicts per statement family and warns once
  `BudgetWarnAt` (8) of them are over budget. An isolated sample is still
  RECORDED, at normal verbosity and at `info`, carrying the window that says
  why it was not called a defect; nothing is dropped and no threshold was
  loosened.

Read store records and harvest run windows through `../../bin/logs.sh`; the
full path, rotation, attribution, and level-switch table is in
`../../AGENTS.md`.

## Suites

```bash
cd modules/app/agent-repl/agent-shim/shim-store
# every test run goes through ../../bin/background.sh (background priority)
go build ./... && go vet ./... && ../../bin/background.sh go test -race ./...
../../bin/background.sh go test -race ./internal/server/   # the Connect service, with a fake Store
../../bin/background.sh go test -race ./internal/db/       # the schema, routing and reads
../../bin/background.sh go test ./integration/             # the whole module end to end over a socket
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
- **`integration/` asserts on record KIND AND FIELD SET, never on detail prose.**
  A refusal subject scopes to the operation that owns it (`recordsAtOperation`),
  counts it (`assertExactlyOneNormalRecord`, or
  `assertExactlyOneNormalRecordAtLevel` for a class recorded at neither `warn`
  nor `error` — every error is logged exactly once by its owning layer, whatever
  severity its class carries), and asserts the correlation keys the refusal is
  looked up by (`refusal_site` AND `refusal_kind` together, `watch_token_hash`, `rpc`,
  `write_id`, `task_id`) — `assertRefusalKeys` is the helper for the first pair. A level
  filter alone is not an assertion: "some warn was logged" passes for a reclaimed
  socket or a slow query as readily as for the thing under test.
- **`assertNoDatabaseTouch` is scoped by `request_id` and needs `verbose: true`.**
  It proves one refused request never opened a transaction, by finding no
  statement trace carrying that request's id. It carries no per-window positive
  control on purpose — a refusal is often the only call in its window — so the
  control is the global one,
  `TestAnAcceptedRequestDoesLeaveAStatementRecordCarryingItsId`, which fails the
  moment the store stops leaving the mark the scan looks for.
- Store-side fixture keys reproduce the producers' real spellings exactly:
  `bash:<run>:start` / `bash:<run>:tail` / `bash:<run>:terminal`,
  `detached:<work id>` for an announcement,
  `session:<arm>:<uuid>` for a session-plane page line, and
  `residue:<vendor record uuid>` with the `residue:file:<path>:<offset>`
  fallback. A subject that keys an announcement by a unit key is testing a
  spelling no producer uses.
- **The fan-out is generic over its item**, keyed by book for page lines and by
  run for bash rows. Two hand-copied registries would be two places for the
  non-blocking-publish and overflow rules to drift apart. They are still two
  REGISTRIES, not one keyed two ways: a book watcher must never be handed a bash
  row.
- `internal/server` must stay testable with NO database: it declares the
  storage contract as `type Store interface`, whose result types and sentinels
  are ALIASES of `internal/db`'s — so `*db.DB` satisfies it directly (asserted
  at compile time in `internal/server/store.go`) and there is no adapter to
  drift. Add a method to `db` and the interface in one commit, never a
  conversion layer between them.
- **A LEVEL FILTER IS NOT AN ASSERTION.** `len(warnings) != 0` passes for a
  reclaimed socket or a slow query as readily as for the thing under test; narrow
  to the `operation` and assert the context keys. Likewise
  `assertNoDatabaseTouch` is scoped by `request_id` against the verbose statement
  traces (start the store with `verbose: true` and send the header), and a
  POSITIVE CONTROL subject fails if an accepted write ever stops leaving that
  mark — without it the negative assertions rot back into vacuity, which is
  exactly what they had done.
- Fixtures must build CONTRACT-VALID messages, or they test a refusal by
  accident: residue carries its `raw` record, a page line's envelope agrees with
  its frame, and a detached run's handle and unit id are DIFFERENT strings unless
  the subject is specifically about their converging.
- The harness's `restart()` does NOT remove the socket, and a SIGKILL variant
  leaves one behind on purpose: removing it masked the reclaim path entirely, so
  no subject ever reached it.
- Collect every failure in a run before fixing any of them; never fail fast.
- Maintain at least 90% statement coverage. Until the measured store baseline
  reaches that target, never reduce it, report the gap explicitly, and add
  focused tests for every critical branch and every error path changed.
- `modules/app/agent-repl/bin/test-all.sh` (from the repository root) runs
  every tracked suite across the module;
  `bin/report-logging-density.sh store` is a rough review aid, not semantic
  coverage — audit critical branches and errors directly even when its ratio
  rises.

### Test wait bounds

Every harness timeout is a small multiple (~3x) of the healthy max observed on
a clean `go test -race -count=1 -v ./...` run, never a round number picked by
feel. Re-derive them the same way after a change materially alters a suite's
real timing (a new real-process boot, a new outage ladder) rather than nudging
a number that started failing.

| Bound | Where | Old | New | Basis |
| --- | --- | --- | --- | --- |
| `readyTimeout` | `integration/helpers_test.go` | 30s | 2s | `integration` package healthy max 0.62s (the store binary is built once in `TestMain`; every boot lands under 100ms) |
| `shutdownTimeout` | `integration/helpers_test.go` | 20s | 2s | same basis as `readyTimeout` |
| `callTimeout` | `integration/helpers_test.go` | 30s | 2s | same basis as `readyTimeout` |
| `streamTimeout` | `integration/helpers_test.go` | 30s | 2s | same basis as `readyTimeout` |
| `openBound` | `internal/server/flush_test.go` | 10s | 1s | `internal/server` package healthy max 0.01s (in-process `httptest.Server` over a fake `Store`, no real I/O); floored above a literal 3x because the package's timings round to 0.00–0.01s and a sub-30ms bound would be fragile under `-race` scheduler/GC jitter |
| inline shutdown bounds (x3) | `internal/server/server_test.go` | 5s, 10s, 5s | 1s each | same basis and floor as `openBound` |

Per-site exception, deliberately NOT tightened by this pass:

- `pprofBootFailureGrace` (`main.go`, 5s) is production behavior, not a test
  harness default: `TestRunWithLoggerOpensThePprofSurfaceBeforeTheDatabase`
  observes the real hold duration directly, so its ~5s cost is the thing under
  test, not a bound guarding the test. Tightening it would change the store's
  runtime diagnosability contract, which this pass does not touch.
- `shutdownGrace` (`main.go`, 5s), `occupancyDialTimeout`
  (`internal/server/listen.go`, 2s), the `http.Server` `ReadHeaderTimeout`
  values (10s), and `DefaultSlowQuery` (`internal/db/slowquery.go`, 250ms) are
  all production defaults, not test-only harness bounds, and are out of scope.

## Dependencies

`agentrepl/proto` (generated Go for `store.v1` and `conversation.v1`, plus the
`storev1connect` handlers and clients), `agentrepl/logging`,
`connectrpc.com/connect` **v1.17.0**, `golang.org/x/net` **v0.43.0**,
`modernc.org/sqlite`. Both pins are project rulings: keep them, keep the module
at `go 1.23.0` with no `toolchain` line so the host toolchain builds it without
a switch, and never edit `proto/gen/go/go.mod`.

## Tests open in-memory databases unless the FILE is the subject

`newStore`/`newStoreWithClock` open `db.openInMemory` (refused outside a test
binary): one shared memdb database per handle, copied from a schema template
built once per test process. It has no WAL, so readers and a committing writer
take turns, and no `-shm` for the checkpoint job. Tests of the WAL, the
checkpoint, a reader overlapping a held write lock, the full-size-corpus
budgets and the connections' pragmas open real files (`newFileStore`, and
`newDurableFileStore` for the pragma pins). File-backed test stores skip
SQLite's forced flushes; a spawned store does so only with
`AGENT_REPL_TEST_SQLITE_UNSYNCED=1` beside `AGENT_REPL_FORBID_VENDOR_CALLS`,
and refuses to open its database with the flag alone. The module's
`AGENTS.md` ("Test databases stay off the SSD") has the whole rule.
