// Package db owns the shim-store SQLite database: the four-table schema, its
// nuke-never-migrate lifecycle, the transactional WriteBatch routing, and the
// indexed reads the store serves.
//
// WHAT IT PERSISTS IS store.v1.StoreEntry, and it opens that envelope EXACTLY
// far enough to route it — which table the write touches, which book a page
// line belongs to, and which lifecycle row a terminal closes. The frame itself
// stays a serialized blob: the activity vocabulary is conversation CONTENT, and
// unpacking it here would drag every conversation.v1 change into DDL.
//
// THREE ORDERINGS, AND THEY ARE NOT THE SAME ORDERING.
//   - The CONVERSATION PLACE (`entry_place`: at_ms, ordinal, then position as
//     a stable tiebreak) is the PAGE order: where each line sits in its
//     conversation, which a producer states and the store keeps from the first
//     write that stated it. Arrival order diverges from it whenever records
//     reach the store out of their own order.
//   - `position` is FIRST-INSERT order. It is what a StoreItemPointer encodes
//     — a pointer names an ITEM, never a place — so a pointer stays valid across
//     every upsert of the row it names, and a catch-up asks for exactly the
//     rows first written after the caller's mark.
//   - `write_seq` is a GLOBAL monotonic write ordinal, bumped on every insert
//     AND every upsert. It is the watch pin, it never reaches the wire, and it
//     is what makes an upsert of an OLD row stream to a live watcher at its
//     ORIGINAL pointer.
package db

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"net/url"
	"os"
	"path/filepath"
	"sort"
	"sync"
	"time"

	"agentrepl/shim-store/internal/logging"
	_ "modernc.org/sqlite"
)

// SchemaVersion is the shape this binary creates. A database stamped at any
// other value is DROPPED and recreated.
//
// IT IS NOT A MIGRATION LINEAGE. There is no ALTER anywhere in this package
// and there never will be: the store is nuked, never migrated, so the version
// answers exactly one question — "did this binary create what is on disk?" —
// and the only remedy for "no" is to recreate it.
//
// AN INDEX IS NOT A SHAPE CHANGE, SO ADDING ONE NEVER BUMPS THIS. Bumping it
// would nuke the owner's database to add a lookup structure SQLite can build
// in place; the lineage indexes (lineageIndexes) are applied to a matching
// database by ensureIndexes instead.
const SchemaVersion = 8

// THE PAGE CACHE AND THE MAP. SQLite's default cache is 2 MB per connection,
// against an events.db of 1.1 GB on the owner's box, so an upsert's B-tree
// seeks below the top levels were each a read(2) from the OS. Measured on a
// copy of that database (the store's AGENTS.md has the numbers): the upsert's
// hashed-key probes went from p50 29us / p99 ~230us at the default to
// p50 24us / p99 ~60-120us with the sizes below.
//
// WHAT IT COSTS: at most 64 MiB for the writer plus 16 MiB per open reader —
// 96 MiB with database/sql's two idle readers — of heap SQLite fills lazily,
// and a map of up to 256 MiB of the file that is the kernel's own page cache,
// shared, not a copy.
const (
	// WriteCacheKiB is the ONE write connection's page cache, in KiB (SQLite's
	// negative cache_size). An interactive upsert touches the entry and
	// write_ledger B-trees and their nine indexes; the four it seeks by a
	// hashed key (entry.upsert_key, entry.write_id, the ledger's primary key
	// and its upsert_key index) total about 140 MB and are hit at random,
	// while the write_seq and position indexes are only appended at their
	// right edge. 64 MiB holds every interior page and about half of those
	// leaves. The write connection lives as long as the store, so its cache is
	// never discarded.
	WriteCacheKiB = 64 * 1024
	// ReadCacheKiB is each READ connection's page cache. The read pool is not
	// capped, and a reader's private cache dies with its connection, so
	// readers lean on the shared map below and keep a small cache of their
	// own.
	ReadCacheKiB = 16 * 1024
	// MmapSizeBytes maps up to this much of the database file on every
	// connection, so a page read there is a memory access into the kernel's
	// file cache instead of a read(2) plus a copy into a private cache. The
	// map is shared by every connection and reclaimable by the kernel. Writes
	// still go through the WAL, and the file only grows (the store never
	// VACUUMs, and a nuke unlinks after closing), so nothing truncates a
	// mapped region under a reader.
	MmapSizeBytes = 256 * 1024 * 1024
	// JournalSizeLimitBytes is the size the -wal file is cut back to when the
	// log restarts. A checkpoint every DefaultCheckpointPages frames keeps the
	// WAL near 4 MB, so 16 MiB is four checkpoint intervals of headroom: a
	// normal burst never pays to shrink and regrow the file, and a 119 MB
	// high-water left by a pinned reader goes back down at the next restart.
	JournalSizeLimitBytes = 16 * 1024 * 1024
)

// mono reads the DB's monotonic clock — the one every measured duration is
// taken from. A zero-value DB (only constructible inside this package, by a
// test that cares about nothing else) falls back to time.Now rather than
// panicking on a nil field.
func (d *DB) mono() time.Time {
	if d.clock == nil {
		return time.Now()
	}
	return d.clock()
}

// nowMillis is the store's wall clock in unix millis.
func nowMillis() int64 { return time.Now().UnixMilli() }

// DB wraps the SQLite handle plus the store's logger.
type DB struct {
	// sql is the WRITE handle, and it is capped at ONE connection: the store
	// is the single writer process, so a second write connection could only
	// ever contend with the first. Every statement on it goes through
	// beginWrite (writer.go).
	sql *sql.DB
	// read is the READ pool, on its own DSN: no `_txlock=immediate`, and
	// `query_only(true)` so the kernel of SQLite itself refuses a write on it.
	// Reads never queue behind a write, structurally, rather than by every
	// read path remembering to pass sql.TxOptions{ReadOnly: true}.
	read *sql.DB
	// ckpt is the CHECKPOINT connection: one connection, on its own DSN with
	// `query_only(true)`, that runs nothing but the checkpoint job's PASSIVE
	// checkpoint (checkpoint.go). A PASSIVE checkpoint takes none of SQLite's
	// writer locks, so running it here rather than on the write handle means a
	// slow copy can never hold the one writer, and so never delays a write.
	ckpt *sql.DB
	log  *logging.Logger
	// slowQuery is the duration past which a completed statement is reported
	// at warn. Non-positive disables the reporting entirely, which only an
	// explicit Options caller can ask for.
	slowQuery time.Duration
	// bulkBase and bulkPerRow size the write_batch bulk budget: a healthy bulk
	// write is bounded by bulkBase + bulkPerRow*rows rather than the fixed
	// interactive threshold, so a large-but-healthy batch on a large database
	// does not warn while a pathological per-row cost still does.
	bulkBase   time.Duration
	bulkPerRow time.Duration
	// now is the clock every written timestamp is taken from. Injectable so a
	// test can assert an exact instant without sleeping for one.
	now func() int64
	// clock is the MONOTONIC clock every measured duration is taken from — the
	// slow-query elapsed time and the write gate's queue wait. It is separate
	// from `now`, which stamps rows in wall-clock millis, because a duration
	// and a timestamp are different questions. Injectable for the same reason:
	// a test asserts an exact wait by advancing it, never by waiting one out.
	clock func() time.Time
	// writes is the process-wide write slot and its two-tier queue: exactly
	// one write transaction at a time, so a batch never meets a sibling's
	// BEGIN IMMEDIATE, and an interactive writer is always handed the slot
	// before a queued bulk one. Its zero value is a free slot. See writer.go.
	writes writeScheduler
	// queuedForWrite, when set, is called by acquireWrite the moment a writer
	// finds the slot taken and has joined its class's queue. It is the seam a
	// test uses to observe a QUEUED writer — the state the gate exists to
	// create — without sleeping for one. Nil in production; nothing reads it
	// there.
	queuedForWrite func(WriteClass)
	// bulk bounds one bulk transaction: a bulk batch larger than this is
	// committed as several, yielding the writer between them. See write.go.
	bulk bulkBounds
	// bulkEntryApplied, when set, is called after each entry of a BULK
	// transaction is applied, still holding the slot. It is the seam a test
	// uses to advance the monotonic clock across the chunk's time bound, or
	// to queue an interactive writer mid-chunk, without sleeping. Nil in
	// production.
	bulkEntryApplied func()
	// transactionCommitted, when set, is called after each write transaction
	// of a WriteBatch commits, still holding the slot. It is the seam a test
	// uses to observe the order the writer took transactions in. Nil in
	// production.
	transactionCommitted func(WriteClass)
	// ledgerRetention is how far behind a file's committed cursor a write_ledger
	// row must fall before the sweep may remove it. Non-positive keeps
	// everything. See prune.go.
	ledgerRetention int64
	// afterPruneBatch, when set, is called after each sweep batch has committed
	// AND given the write slot back. It is the seam a test uses to prove the
	// sweep does not hold the slot across the whole sweep. Nil in production.
	afterPruneBatch func()
	// ledgerFileSwept, when set, is called after each file a sweep batch
	// removes from, still holding the slot. It is the seam a test uses to
	// advance the monotonic clock across the batch's time bound without
	// sleeping. Nil in production.
	ledgerFileSwept func()
	// budgetMu guards budgets, which holds one rolling window of over-budget
	// verdicts per statement family. Every producer's rpc runs on its own
	// goroutine against this one DB, so the windows are shared state.
	budgetMu sync.Mutex
	budgets  map[string]*budgetWindow
	// path is the database file, which the checkpoint job's WAL-index reading
	// sits beside (checkpoint.go).
	path string
	// wal is the hand-off from every writer's release to the checkpoint job,
	// and walMu guards the reading it carries. Its signal is made only once
	// the database is fully open, so nothing observes a half-opened one.
	wal   walWatch
	walMu sync.Mutex
	// runCheckpoint, when set, replaces the PRAGMA a checkpoint runs. It is
	// the seam a test uses to make a checkpoint FAIL and then succeed — the
	// retry path — which a real WAL will not do on demand. Nil in production.
	runCheckpoint func(context.Context) (busy int, frames, checkpointed int64, err error)
	// checkpointDone, when set, is called by the checkpoint job after every
	// checkpoint it runs, with what it returned. It is the seam a test waits
	// on instead of sleeping. Nil in production.
	checkpointDone func(CheckpointResult, error)
	// newCheckpointTimer, when set, makes the job's idle timer, so a test
	// fires the idle trigger by hand. Nil in production, which is time.Timer.
	newCheckpointTimer func(time.Duration) checkpointTimer
	// closeHandle, when set, replaces (*sql.DB).Close for every handle Close
	// closes. It is the seam a test uses to make one handle's close fail,
	// which a real pool will not do on demand. Nil in production.
	closeHandle func(*sql.DB) error
}

// Options are the injectable knobs Open resolves from the environment.
type Options struct {
	// SlowQuery is the slow-query threshold; non-positive disables reporting.
	SlowQuery time.Duration
	// BulkBase and BulkPerRow size the write_batch bulk budget (base plus a
	// per-row budget). Zero values fall back to the shipped defaults, so an
	// Options caller that only cares about SlowQuery still gets a sane bulk
	// budget rather than a zero one that would flag every bulk write.
	BulkBase   time.Duration
	BulkPerRow time.Duration
	// Now overrides the wall clock rows are stamped from. Zero value means
	// time.Now().UnixMilli.
	Now func() int64
	// Clock overrides the monotonic clock measured DURATIONS are taken from —
	// the slow-query elapsed time and the write gate's queue wait. Zero value
	// means time.Now.
	Clock func() time.Time
	// BulkChunkRows, BulkChunkBytes and BulkChunkTime bound one bulk
	// transaction (see bulkBounds in write.go). Zero falls back to the shipped
	// default; only a test sets them.
	BulkChunkRows  int
	BulkChunkBytes int
	BulkChunkTime  time.Duration
	// LedgerRetentionBytes is how far behind a file's committed cursor a
	// write_ledger row must fall before the sweep removes it. Zero falls back
	// to DefaultLedgerRetentionBytes; NEGATIVE disables the sweep, which is how
	// a caller says "keep every row" without the zero value meaning it by
	// accident.
	LedgerRetentionBytes int64
	// unsynced turns SQLite's forced flushes OFF (synchronous=OFF) on the
	// write and checkpoint connections. It is a TEST-RUN seam: unexported, so
	// outside this package only Open reaches it, and Open sets it only from
	// UnsyncedFromEnv, which refuses it to a store without the vendor guard.
	unsynced bool
}

// Open opens (creating if absent) the store database at path with WAL enabled
// and brings it to SchemaVersion, dropping whatever is there if it does not
// already match.
//
// The slow-query threshold is resolved from the environment here. A malformed
// value aborts the open rather than running the shipped default underneath an
// operator who believes they changed it.
func Open(path string, log *logging.Logger) (*DB, error) {
	slowQuery, err := SlowQueryFromEnv()
	if err != nil {
		if log != nil {
			log.Log(logging.Fields{Operation: "store.db.open", DatabasePath: path, Level: "error", ErrorCause: err.Error()},
				"slow-query threshold rejected: %v", err)
		}
		return nil, err
	}
	bulkBase, bulkPerRow, err := BulkBudgetFromEnv()
	if err != nil {
		if log != nil {
			log.Log(logging.Fields{Operation: "store.db.open", DatabasePath: path, Level: "error", ErrorCause: err.Error()},
				"bulk write budget rejected: %v", err)
		}
		return nil, err
	}
	unsynced, err := UnsyncedFromEnv()
	if err != nil {
		if log != nil {
			log.Log(logging.Fields{Operation: "store.db.open", DatabasePath: path, Level: "error", ErrorCause: err.Error()},
				"the database's durability could not be settled: %v", err)
		}
		return nil, err
	}
	return OpenWithOptions(path, log, Options{SlowQuery: slowQuery, BulkBase: bulkBase, BulkPerRow: bulkPerRow, unsynced: unsynced})
}

// OpenWithOptions is Open with the knobs supplied rather than read from the
// environment. Tests use it to exercise both sides of a threshold without
// mutating process state.
func OpenWithOptions(path string, log *logging.Logger, opts Options) (*DB, error) {
	if log == nil {
		panic("shim-store db: nil logger")
	}
	log.LogVerbose(logging.Fields{Operation: "store.db.open", DatabasePath: path}, "opening SQLite database")
	if opts.unsynced {
		log.Log(logging.Fields{Operation: "store.db.open", DatabasePath: path},
			"test run: the database skips SQLite's forced flushes (synchronous=OFF)")
	}

	// THE LAYER THAT OWNS THE FILE OWNS ITS DIRECTORY. Nothing upstream may
	// create it: doing so would move an unwritable --db path's failure ahead of
	// the profiling surface the boot order opens first, exactly so a wedged or
	// doomed database step stays diagnosable.
	if err := os.MkdirAll(filepath.Dir(path), 0o700); err != nil {
		log.Log(logging.Fields{Operation: "store.db.open", DatabasePath: path, Level: "error", ErrorCause: err.Error()},
			"creating the database directory failed: %v", err)
		return nil, storagef(err, "creating the directory for %q", path)
	}

	// modernc.org/sqlite takes PRAGMAs as _pragma query params. WAL for
	// concurrent readers during a live tail; NORMAL sync is durable under WAL;
	// busy_timeout answers an outside writer (see writer.go), since nothing in
	// this process ever contends with the one write connection.
	//
	// _txlock=immediate makes every Begin() issue BEGIN IMMEDIATE, which is
	// what the WRITE path needs: WriteBatch reads (MAX(write_seq), the write_id
	// probe) before it writes, and under the DEFERRED default such a
	// transaction takes a WAL READ snapshot and only later tries to upgrade to
	// a writer — SQLite refuses to run the busy handler for an upgrade, so a
	// contending writer is an immediate SQLITE_BUSY rather than a wait. Taking
	// the lock at BEGIN removes the upgrade entirely.
	//
	// A PURE READ NEVER TOUCHES THIS DSN — it runs on the read pool below.
	// _txlock is a property of the CONNECTION, so this one reached the read
	// path too and made a page repaint queue for the write lock a producer was
	// holding — and be refused by it. See beginRead in read.go for why a
	// deferred read still pins its watch in the page's own snapshot.
	//
	// wal_autocheckpoint(0): NO COMMIT CHECKPOINTS. The checkpoint is the
	// store's own bulk-class job (checkpoint.go), so an interactive commit
	// never pays for copying pages somebody else appended.
	//
	// journal_size_limit: the -wal file is cut back to this size the next time
	// the log restarts after a checkpoint, rather than staying at whatever
	// high-water a burst or a pinned reader once pushed it to.
	//
	// cache_size and mmap_size: WriteCacheKiB and MmapSizeBytes, at the top of
	// this file, say what they cost and why.
	writeDSN := "file:" + path + "?" + url.Values{
		"_pragma": syncFirstWhenUnsynced(opts, []string{
			"journal_mode(WAL)",
			"busy_timeout(5000)",
			synchronousPragma(opts),
			"foreign_keys(ON)",
			"wal_autocheckpoint(0)",
			fmt.Sprintf("journal_size_limit(%d)", JournalSizeLimitBytes),
			fmt.Sprintf("cache_size(-%d)", WriteCacheKiB),
			fmt.Sprintf("mmap_size(%d)", MmapSizeBytes),
		}),
		"_txlock": {"immediate"},
	}.Encode()

	// THE READ POOL IS A SEPARATE POOL ON A SEPARATE DSN, and that is the
	// structural half of "a read never waits on a write".
	//
	// `_txlock` is a property of the CONNECTION, so one DSN carrying
	// `immediate` made EVERY transaction a writer — a page repaint queued for
	// the write lock a producer held, and could be refused by it, which is
	// precisely the failure WAL is chosen to remove. `beginRead`'s
	// sql.TxOptions{ReadOnly: true} fixed that per call site, and a per-call-
	// site fix is one forgotten option away from coming back. A pool the write
	// lock is not reachable from cannot forget.
	//
	// `query_only(true)` is the second half: SQLite itself refuses a write
	// statement on this pool, so a read path that grew one is a hard error at
	// the first attempt rather than a silent second writer. It is applied LAST
	// so the pragmas ahead of it are not themselves refused, and
	// `journal_mode` is not among them — the journal mode is a durable
	// property of the FILE that the write handle already established, and
	// setting it needs write access this pool does not have.
	readDSN := "file:" + path + "?" + url.Values{
		"_pragma": {
			"busy_timeout(5000)",
			"foreign_keys(ON)",
			fmt.Sprintf("cache_size(-%d)", ReadCacheKiB),
			fmt.Sprintf("mmap_size(%d)", MmapSizeBytes),
			"query_only(true)",
		},
	}.Encode()

	// THE CHECKPOINT CONNECTION IS A THIRD DSN, and that is what keeps a slow
	// checkpoint off the writer. SQLite's PASSIVE checkpoint takes the
	// checkpointer lock alone, never the WAL write lock, so it runs beside an
	// open write transaction (TestTheCheckpointConnectionCheckpointsBesideAnOpenWriteTransaction).
	// Run on the write handle, as it used to be, it could only run while
	// holding the one writer, and a 3s copy on a loaded host was 3s in which
	// no producer could commit.
	//
	// `query_only(true)` makes it unable to be a second writer: the PRAGMA is
	// not a write statement, and everything that is one is refused.
	// synchronousPragma is the write handle's setting, so the checkpoint
	// syncs exactly as it did when it ran there.
	checkpointDSN := "file:" + path + "?" + url.Values{
		"_pragma": {
			"busy_timeout(5000)",
			synchronousPragma(opts),
			"query_only(true)",
		},
	}.Encode()

	clock := opts.Now
	if clock == nil {
		clock = nowMillis
	}

	dsns := poolDSNs{write: writeDSN, read: readDSN, checkpoint: checkpointDSN}
	d, err := openAt(dsns, path, log, opts, clock)
	if err == nil {
		return finishOpen(d, log, path, opts)
	}

	// A FAILED IN-PLACE INDEX BUILD NEVER REACHES THE NUKE. The database it
	// failed on is one THIS binary created, carrying its rows, and an index is
	// an optimization: discarding the owner's record because a lookup
	// structure could not be added would trade the data for its speed. The
	// failure was recorded once by ensureIndexes, and the open fails with it.
	var indexErr *indexMigrationError
	if errors.As(err, &indexErr) {
		return nil, err
	}

	// THE FILE IS IN THE WAY, SO IT GOES. A --db path this binary cannot even
	// read as a database is the SAME situation as a schema this binary did not
	// create, and the store answers BOTH the same way: it REMOVES the file and
	// its WAL siblings and creates a fresh one. The store holds a cache of what
	// the vendor and the shim already know how to produce again, so a file worth
	// nothing costs an unlink to be rid of. Refusing to boot instead would wedge
	// the service permanently on a truncated file or a half-written copy — an
	// outage that needs a human with a shell, in exchange for preserving bytes
	// nobody can read.
	//
	// AN UNLINK, NEVER A DROP. Emptying a foreign schema with DROP TABLE walks
	// every page of whatever was in it: a real deploy met an 11.5 GB events.db
	// stamped at a superseded version, and the DROP ran for minutes with the
	// socket absent while the deploy gave up waiting for it and left the rest of
	// the stack un-bounced. Removing the file is O(1) however large the thing
	// being discarded is, which is the whole point of a store that is nuked
	// rather than migrated.
	//
	// ONLY ONCE, AND ONLY FOR THAT. A second failure after a clean recreate is a
	// real problem — an unwritable directory, a full disk — and is returned.
	//
	// AND ONLY FOR A REGULAR FILE. Unlinking whatever happens to sit at an
	// operator-supplied path is how a service deletes somebody's data: a
	// directory at --db is a misconfiguration to report, not a database to
	// replace, and removing it would silently destroy whatever it contained.
	// The mode is proved before anything is removed, exactly as the listener
	// proves a socket's mode before reclaiming it.
	if !isRegularFile(path) {
		log.Log(logging.Fields{Operation: "store.db.open", DatabasePath: path, Level: "error", ErrorCause: err.Error()},
			"the database path cannot be opened and is not a regular file, so it will not be replaced: %v", err)
		return nil, err
	}
	// WHAT LEVEL A NUKE IS RECORDED AT DEPENDS ON WHOSE DATABASE IT WAS.
	//
	// A stamp BELOW SchemaVersion is a version this binary superseded, and
	// meeting one is the ordinary outcome of a deploy that bumped the schema:
	// the store is a cache with no retention during development (owner ruling
	// 2026-09-13), so recreating it is the documented convention working, not a
	// defect. It is recorded at INFO, naming both versions, because the fact
	// that the database was replaced is worth having and the fact that it
	// HAPPENED is not a fault. Held at warn, every schema bump put a warning in
	// the owner's log for doing exactly what it is supposed to do (found
	// version=5, want version=6, 2026-09-13 16:03:25).
	//
	// ANYTHING ELSE IS AN ERROR, and still nuked. A stamp AT OR ABOVE this
	// binary's is a database this binary cannot have created — a newer store
	// was here, or the same version carries a shape this one did not write —
	// and a file this binary cannot read at all is damaged. Neither is a
	// convention; both discard data that nothing planned to discard.
	var mismatch *schemaMismatchError
	switch {
	case errors.As(err, &mismatch) && supersededVersion(mismatch.version):
		log.Log(logging.Fields{Operation: "store.db.schema", DatabasePath: path, Table: "schema_meta"},
			"the on-disk schema is a superseded version (found version=%d tables=%v, want version=%d tables=%v) — removing the database file and its WAL siblings and recreating; the store is nuked, never migrated",
			mismatch.version, mismatch.tables, SchemaVersion, schemaTables)
	case errors.As(err, &mismatch):
		log.Log(logging.Fields{Operation: "store.db.schema", DatabasePath: path, Table: "schema_meta", Level: "error"},
			"on-disk schema was not created by this binary and is not a version it superseded (found version=%d tables=%v, want version=%d tables=%v) — removing the database file and its WAL siblings and recreating; the store is nuked, never migrated",
			mismatch.version, mismatch.tables, SchemaVersion, schemaTables)
	default:
		log.Log(logging.Fields{Operation: "store.db.schema", DatabasePath: path, Table: "schema_meta", Level: "error", ErrorCause: err.Error()},
			"the database file cannot be read by this binary (%v) — removing it and its WAL siblings and recreating; the store is nuked, never migrated", err)
	}
	if removeErr := removeDatabaseFiles(path); removeErr != nil {
		log.Log(logging.Fields{Operation: "store.db.open", DatabasePath: path, Level: "error", ErrorCause: removeErr.Error()},
			"removing the superseded database failed: %v", removeErr)
		return nil, removeErr
	}
	d, err = reopenAfterNuke(dsns, path, log, opts, clock)
	if err != nil {
		// THE RECREATE IS THE LAST RESORT, so its failure is stated here rather
		// than left to whatever the caller does with the error: at this point
		// the superseded file has already been unlinked and there is no
		// database at all.
		log.Log(logging.Fields{Operation: "store.db.schema", DatabasePath: path, Table: "schema_meta", Level: "error", ErrorCause: err.Error()},
			"the database could not be recreated at version=%d after the superseded file was removed: %v", SchemaVersion, err)
		return nil, err
	}
	return finishOpen(d, log, path, opts)
}

// synchronousPragma is the write and checkpoint connections' sync level:
// NORMAL, which is durable under WAL, unless a test run asked for none.
func synchronousPragma(opts Options) string {
	if opts.unsynced {
		return "synchronous(OFF)"
	}
	return "synchronous(NORMAL)"
}

// syncFirstWhenUnsynced moves the sync level to the FRONT of an unsynced
// connection's pragmas: the driver applies them in order, and the WAL
// conversion journal_mode(WAL) commits would otherwise still run at SQLite's
// default FULL, an F_FULLFSYNC that once stalled a test store's open for
// seconds. A durable connection's pragmas keep their production order.
func syncFirstWhenUnsynced(opts Options, pragmas []string) []string {
	if !opts.unsynced {
		return pragmas
	}
	out := []string{synchronousPragma(opts)}
	for _, p := range pragmas {
		if p != synchronousPragma(opts) {
			out = append(out, p)
		}
	}
	return out
}

// reopenAfterNuke is openAt, reached through a variable so a test can construct
// the one failure the filesystem will not hold still for: a recreate that fails
// AFTER the superseded file has already been unlinked. Every path that could
// make the real open fail (a read-only directory, a full disk) also makes the
// unlink fail, which is a different branch, so the record on this one would
// otherwise be untested.
var reopenAfterNuke = openAt

// finishOpen records the ready state. It is the one exit both the first attempt
// and the post-nuke attempt take.
func finishOpen(d *DB, log *logging.Logger, path string, opts Options) (*DB, error) {
	d.wal.kick = make(chan struct{}, 1)
	log.Log(logging.Fields{Operation: "store.db.open", DatabasePath: path},
		"SQLite database ready schema_version=%d slow_query_threshold_ms=%d bulk_base_ms=%d bulk_per_row_ms=%d",
		SchemaVersion, opts.SlowQuery.Milliseconds(), d.bulkBase.Milliseconds(), d.bulkPerRow.Milliseconds())
	return d, nil
}

// poolDSNs are the three DSNs a store opens its file on: the one write
// connection, the read pool, and the checkpoint connection.
type poolDSNs struct {
	write, read, checkpoint string
}

// openAt opens the handle and brings it to SchemaVersion, closing the handle if
// either step fails so the caller may remove the file underneath it.
func openAt(dsns poolDSNs, path string, log *logging.Logger, opts Options, clock func() int64) (*DB, error) {
	monotonic := opts.Clock
	if monotonic == nil {
		monotonic = time.Now
	}
	// ONE WRITE CONNECTION, ENFORCED BY THE POOL AS WELL AS BY THE GATE. The
	// gate (writer.go) is what a queued caller waits on and what reports its
	// wait; this is what makes a second write connection unrepresentable, so
	// nothing that bypassed the gate could quietly recreate the contention.
	sqldb, err := openPool(dsns.write, 1, fmt.Sprintf("%q", path))
	if err != nil {
		return nil, err
	}
	bulkBase := opts.BulkBase
	if bulkBase == 0 {
		bulkBase = DefaultBulkBase
	}
	bulkPerRow := opts.BulkPerRow
	if bulkPerRow == 0 {
		bulkPerRow = DefaultBulkPerRow
	}
	ledgerRetention := opts.LedgerRetentionBytes
	if ledgerRetention == 0 {
		ledgerRetention = DefaultLedgerRetentionBytes
	}
	d := &DB{
		sql:        sqldb,
		log:        log,
		slowQuery:  opts.SlowQuery,
		bulkBase:   bulkBase,
		bulkPerRow: bulkPerRow,
		now:        clock,
		clock:      monotonic,
		bulk:       resolveBulkBounds(opts),

		ledgerRetention: ledgerRetention,
		path:            path,
	}
	if err := d.ensureSchema(context.Background(), path); err != nil {
		sqldb.Close() //nolint:errcheck // the open already failed
		return nil, err
	}

	// THE READ POOL OPENS AFTER THE SCHEMA EXISTS, because `query_only` would
	// refuse the DDL that creates it and because a pool opened against a file
	// this binary is about to unlink would hold a handle to the discarded
	// inode.
	readdb, err := openPool(dsns.read, 0, fmt.Sprintf("the read pool on %q", path))
	if err != nil {
		sqldb.Close() //nolint:errcheck // the open already failed
		return nil, err
	}
	// THE CHECKPOINT CONNECTION OPENS LAST, for the read pool's reasons: its
	// `query_only` would refuse nothing here, but a handle opened on a file the
	// nuke is about to unlink would checkpoint the discarded inode.
	ckptdb, err := openPool(dsns.checkpoint, 1, fmt.Sprintf("the checkpoint connection on %q", path))
	if err != nil {
		readdb.Close() //nolint:errcheck // the open already failed
		sqldb.Close()  //nolint:errcheck // the open already failed
		return nil, err
	}
	d.read = readdb
	d.ckpt = ckptdb
	return d, nil
}

// openPool is the ONE way a database/sql pool is opened on this store's file:
// it opens the DSN, caps the pool at maxOpen connections (zero leaves it
// uncapped), and proves the pool with a Ping, closing it again if the Ping
// fails so a failed open never leaves a handle behind. `what` names the pool in
// the error.
func openPool(dsn string, maxOpen int, what string) (*sql.DB, error) {
	pool, err := sql.Open("sqlite", dsn)
	if err != nil {
		return nil, storagef(err, "opening %s", what)
	}
	if maxOpen > 0 {
		pool.SetMaxOpenConns(maxOpen)
	}
	if err := pool.Ping(); err != nil {
		pool.Close() //nolint:errcheck // the open already failed
		return nil, storagef(err, "pinging %s", what)
	}
	return pool, nil
}

// isRegularFile reports whether the path is an ordinary file this store may
// replace. A directory, a device, a symlink to either — anything that is not a
// plain file — is somebody else's, and is never removed.
func isRegularFile(path string) bool {
	info, err := os.Lstat(path)
	return err == nil && info.Mode().IsRegular()
}

// removeDatabaseFiles removes the database and the two WAL siblings SQLite
// keeps beside it.
//
// THE SIBLINGS MUST GO TOO. A -wal or -shm left behind belongs to the file that
// was just deleted, and SQLite opening a fresh database next to a stale WAL is
// how a "recreated" store comes up carrying fragments of the one it replaced.
func removeDatabaseFiles(path string) error {
	for _, name := range []string{path, path + "-wal", path + "-shm"} {
		if err := os.Remove(name); err != nil && !errors.Is(err, os.ErrNotExist) {
			return storagef(err, "removing the superseded database file %q", name)
		}
	}
	return nil
}

// Close closes the underlying handle.
// EVERY POOL IS CLOSED, AND NO FAILURE IS SWALLOWED. The read pool and the
// checkpoint connection are closed first because neither ever writes; the
// write handle is closed even if either fails, so their fault cannot leave
// the writer's file handle open, and the first error is the one reported.
func (d *DB) Close() error {
	d.log.LogVerbose(logging.Fields{Operation: "store.db.close"}, "closing SQLite database")
	var firstErr error
	keep := func(err error) {
		if firstErr == nil {
			firstErr = err
		}
	}
	if d.read != nil {
		keep(d.closePool(d.read, "closing the SQLite read pool", "closing the read pool"))
	}
	if d.ckpt != nil {
		keep(d.closePool(d.ckpt, "closing the SQLite checkpoint connection", "closing the checkpoint connection"))
	}
	keep(d.closePool(d.sql, "closing SQLite database", "closing the database"))
	// THE -shm DESCRIPTOR CLOSES LAST, after every SQLite connection has, so
	// its close cannot release a lock SQLite still holds (see readWAL).
	if d.wal.shm != nil {
		if err := d.wal.shm.Close(); err != nil {
			d.log.Log(logging.Fields{Operation: "store.db.close", Level: "error", ErrorCause: err.Error()},
				"closing the WAL-index descriptor failed: %v", err)
			keep(storagef(err, "closing the WAL-index descriptor"))
		}
		d.wal.shm = nil
	}
	if firstErr != nil {
		return firstErr
	}
	d.log.Log(logging.Fields{Operation: "store.db.close"}, "SQLite database closed")
	return nil
}

// closePool closes one of the store's handles, recording a failure once at
// error and returning it as a storage failure. `logged` names the close in the
// record, `wrapped` in the returned error.
func (d *DB) closePool(pool *sql.DB, logged, wrapped string) error {
	closeHandle := d.closeHandle
	if closeHandle == nil {
		closeHandle = (*sql.DB).Close
	}
	if err := closeHandle(pool); err != nil {
		d.log.Log(logging.Fields{Operation: "store.db.close", Level: "error", ErrorCause: err.Error()},
			"%s failed: %v", logged, err)
		return storagef(err, "%s", wrapped)
	}
	return nil
}

// schemaDDL is the WHOLE schema, and it is the only DDL in this package.
//
// NO `IF NOT EXISTS` ANYWHERE, on purpose: this statement only ever runs
// against a database that was just emptied, and a CREATE that silently
// tolerated an existing object is exactly how two binaries end up believing
// they share a shape they do not.
//
// THE ONE EXCEPTION IS lineageIndexes, below: indexes added after a schema
// version shipped, applied IN PLACE to a matching database rather than by a
// version bump that would nuke it.
const schemaDDL = `
CREATE TABLE entry (
  position             INTEGER PRIMARY KEY AUTOINCREMENT,
  upsert_key           TEXT    NOT NULL UNIQUE,
  write_id             TEXT    NOT NULL UNIQUE,
  write_seq            INTEGER NOT NULL,
  plane                INTEGER NOT NULL,
  kind                 TEXT    NOT NULL,
  book_agent_id        TEXT,
  run_id               TEXT,
  top_level            TEXT,
  frame                BLOB    NOT NULL,
  first_inserted_at_ms INTEGER NOT NULL,
  last_written_at_ms   INTEGER NOT NULL
);
CREATE INDEX entry_book_position  ON entry(book_agent_id, position);
CREATE INDEX entry_book_write_seq ON entry(book_agent_id, write_seq);
CREATE INDEX entry_write_seq      ON entry(write_seq);
CREATE INDEX entry_run_position  ON entry(run_id, position);
CREATE INDEX entry_run_write_seq ON entry(run_id, write_seq);

CREATE TABLE agent (
  agent_id              TEXT PRIMARY KEY,
  spawned_by_agent      TEXT,
  spawned_by_workflow   TEXT,
  description           TEXT,
  prompt_text           TEXT,
  subagent_type         TEXT,
  requested_name        TEXT,
  requested_model       TEXT,
  spawn_depth           INTEGER,
  working_dir           TEXT,
  transcript_suppressed INTEGER,
  isolation             TEXT,
  forked_from_caller    INTEGER,
  started_at_ms         INTEGER NOT NULL,
  ended_at_ms           INTEGER,
  terminal              BLOB
);
CREATE INDEX agent_live ON agent(ended_at_ms);

CREATE TABLE workflow (
  run_agent_id  TEXT PRIMARY KEY,
  spawner_agent TEXT,
  origin_unit   TEXT,
  name          TEXT,
  script        BLOB,
  resumed_from  TEXT,
  placement     TEXT,
  started_at_ms INTEGER NOT NULL,
  ended_at_ms   INTEGER,
  terminal      BLOB
);
CREATE INDEX workflow_live ON workflow(ended_at_ms);

-- detached_work holds THE JOIN AND NOTHING ELSE.
--
-- The announcement itself is a PAGE LINE of the announcing agent's book, and
-- that page line is the one copy of what was announced: the spool path, the
-- readability, the detach cause, the timeout. Unpacking any of it here as well
-- would give the same fact two homes that can disagree, and the store would be
-- re-deriving conversation content it is not entitled to interpret. What is
-- left is exactly what the store itself filters and joins on: the handle, the
-- kind, the origin unit a terminal closes the row through, the announcing
-- agent, and the terminal columns.
CREATE TABLE detached_work (
  work_id         TEXT PRIMARY KEY,
  kind            TEXT NOT NULL,
  origin_unit     TEXT,
  owner_agent     TEXT,
  announced_at_ms INTEGER NOT NULL,
  ended_at_ms     INTEGER,
  terminal        BLOB
);
CREATE INDEX detached_work_origin ON detached_work(origin_unit);
CREATE INDEX detached_work_live   ON detached_work(ended_at_ms);

-- vendor_task pairs a subagent's VENDOR TASK LOCATOR (the <id> of
-- agent-<id>.jsonl, and the stream's task_id) with the agent it names (the
-- spawning call's tool_use_id, the cross-plane minting rule). The sidecar
-- writes it with the agent's rows (EntryBatch.agent_locators); the shim reads
-- it through GetAgentByVendorTask when a resume names the call that woke the
-- agent rather than the spawn. One row per (locator, agent), so a re-stated
-- pairing is absorbed and a locator paired with two agents stays visible to
-- the lookup, which refuses it rather than choosing.
CREATE TABLE vendor_task (
  vendor_task_id TEXT    NOT NULL,
  agent_id       TEXT    NOT NULL,
  recorded_at_ms INTEGER NOT NULL,
  PRIMARY KEY (vendor_task_id, agent_id)
);

CREATE TABLE cursor (
  file_id       TEXT PRIMARY KEY,
  path          TEXT    NOT NULL,
  offset        INTEGER NOT NULL,
  carry         BLOB,
  updated_at_ms INTEGER NOT NULL
);

-- write_ledger answers "has this write_id already been applied?" and is
-- RETAINED ONLY AS LONG AS SOMEBODY CAN STILL ASK. source_file_id and
-- source_offset are the batch's own cursor_advance — the file the rows were
-- read from and the offset the producer's NEXT read starts at, which is above
-- every offset the batch covered. They are what lets the sweep decide a row is
-- further behind its file's committed cursor than the sidecar's boot rewind can
-- ever reach. NULL for a write with no file behind it (stream plane, or a batch
-- that advanced no cursor), and a NULL row is never pruned. See prune.go.
CREATE TABLE write_ledger (
  write_id       TEXT    PRIMARY KEY,
  upsert_key     TEXT    NOT NULL,
  write_seq      INTEGER NOT NULL,
  applied_at_ms  INTEGER NOT NULL,
  source_file_id TEXT,
  source_offset  INTEGER
);
CREATE INDEX write_ledger_upsert_key ON write_ledger(upsert_key);
CREATE INDEX write_ledger_source     ON write_ledger(source_file_id, source_offset);

-- residue_shapes is the catalog of key structures observed on lines NO
-- PRODUCER STORED (owner ruling 2026-09-13). A residue line the sidecar no
-- longer persists takes its bytes out of the store with it, and with them the
-- only evidence the vendor emits that line at all; one row per distinct
-- recursive key structure keeps the vendor's API discoverable at a cost that
-- does not grow with traffic.
--
-- THE TABLE HOLDS STRUCTURE, NOT CONTENT. key_structure is the canonical
-- rendering the hash was taken over -- key names and scalar TYPES, every value
-- dropped. first_example is the one verbatim line, written once on the insert
-- and never replaced, because a shape nobody can read an example of is a shape
-- nobody can act on.
--
-- NO RETENTION RULE, by ruling: the row count is bounded by the number of
-- distinct shapes a vendor emits, not by traffic, so there is nothing here for
-- a sweep to remove.
CREATE TABLE residue_shapes (
  shape_hash    TEXT PRIMARY KEY,
  kind          TEXT    NOT NULL,
  key_structure TEXT    NOT NULL,
  first_example BLOB,
  first_seen_ms INTEGER NOT NULL,
  last_seen_ms  INTEGER NOT NULL,
  count         INTEGER NOT NULL
);
CREATE INDEX residue_shapes_kind      ON residue_shapes(kind, last_seen_ms);
CREATE INDEX residue_shapes_last_seen ON residue_shapes(last_seen_ms);

CREATE TABLE schema_meta (version INTEGER NOT NULL);
`

// lineageIndexes are the indexes the session-lineage walk (sessionLineageCTE
// in live.go) seeks by. Each is AN OPTIMIZATION and nothing else: no answer
// changes with or without it, only how SQLite finds the rows.
//
// WHY THEY EXIST. Without them SQLite answered every GetLiveWork by building
// four AUTOMATIC indexes — one per spawn or owner column the recursive step
// joins on — from a full scan of the table, rebuilt on EVERY run and thrown
// away after it: 264-498ms per call in the owner's log (2026-09-24/25,
// statement=live_work over its 250ms budget), for an answer that is usually
// empty. With them each recursive step is a seek.
//
// WHY THEY ARE HERE AND NOT IN schemaDDL. They were added after SchemaVersion 7
// shipped, and a version bump nukes the database. An index is not a shape
// change — it holds nothing a query can observe — so ensureIndexes builds any
// missing one IN PLACE with `CREATE INDEX IF NOT EXISTS`, on a fresh database
// and on the owner's existing one alike. The statements are idempotent, so a
// database that already carries them is left exactly as it is.
var lineageIndexes = []struct{ name, ddl string }{
	// OPTIMIZATION: the lineage's subagent step, `agent.spawned_by_agent =
	// lineage.agent_id`, seeks here instead of building an automatic index
	// over every agent row on every live-work read.
	{"agent_spawned_by_agent", `CREATE INDEX IF NOT EXISTS agent_spawned_by_agent ON agent(spawned_by_agent)`},
	// OPTIMIZATION: both workflow steps of the lineage end in
	// `agent.spawned_by_workflow = <workflow or announcement id>`, which seeks
	// here instead of building an automatic index per read.
	{"agent_spawned_by_workflow", `CREATE INDEX IF NOT EXISTS agent_spawned_by_workflow ON agent(spawned_by_workflow)`},
	// OPTIMIZATION: the lineage's workflow-row step, `workflow.spawner_agent =
	// lineage.agent_id`, seeks here instead of building an automatic index
	// per read.
	{"workflow_spawner_agent", `CREATE INDEX IF NOT EXISTS workflow_spawner_agent ON workflow(spawner_agent)`},
	// OPTIMIZATION: the lineage's announcement step AND the live-work detached
	// listing both join `detached_work.owner_agent = lineage.agent_id`, which
	// seeks here instead of building an automatic index per read.
	{"detached_work_owner_agent", `CREATE INDEX IF NOT EXISTS detached_work_owner_agent ON detached_work(owner_agent)`},
}

// inPlaceTables are the tables added without a SchemaVersion bump, built IN
// PLACE on a matching database exactly as lineageIndexes are, because a version
// bump nukes the owner's database.
//
// A TABLE IS NUKE-WORTHY ONLY WHEN IT CHANGES A SHAPE THAT IS ALREADY THERE.
// These add a shape nothing on disk has, next to the rows that are, so the rows
// already stored keep every meaning they had: a database that predates one
// simply has no bookkeeping in it yet, and each table's reader states what that
// absence means. The statements are idempotent (`CREATE TABLE IF NOT EXISTS`);
// a fresh database gets them in createSchema's own transaction, and nothing
// here is ever altered, dropped or rewritten.
//
// WHY THIS ONE COULD NOT BE A VERSION BUMP. cursor_conversion exists so a
// conversion change heals the stored rows it made wrong (owner ruling
// 2026-09-27: the stale rows go and nothing else changes). Nuking the database
// would also throw away every stream-plane row the shim wrote live — asks,
// stream-only frames — which no producer can rebuild.
//
// A TABLE MAY CARRY A BACKFILL, run in the SAME transaction as its create and
// only then: the statement that gives the rows already stored the bookkeeping
// the table would have held had it existed when they were written. A fresh
// database runs it too, over no rows. It never runs against a database that
// already carries the table, so it is a one-time step per database.
var inPlaceTables = []struct{ name, ddl, backfill string }{
	// THE FILE PLANE'S CONVERSION BOOKKEEPING, one row per cursor (store.v1
	// CursorConversion): the conversion version every byte below the cursor's
	// offset was converted under, and — while a re-derivation is in progress —
	// the offset the older conversion had read to (NULL when none is). It rides
	// the cursor advance's own transaction (upsertCursor). A cursor with NO row
	// here was written before conversion versions existed and is served with
	// the conversion unset, which the reader reads as version 0.
	{"cursor_conversion", `CREATE TABLE IF NOT EXISTS cursor_conversion (
  file_id         TEXT    PRIMARY KEY,
  version         INTEGER NOT NULL,
  healing_through INTEGER
)`, ""},
	// THE CONVERSATION PLACE EVERY BOOK IS ORDERED BY (conversation.v1
	// ConversationPlace), one row per row that has a book: the `entry` row at
	// `position`, its book (copied, because the page order is an index over
	// book-then-place and an index cannot span two tables), the place, and
	// whether a producer STATED it (`recorded` = 1: the row's first stated
	// StoreEntry.place) or the store's first-insert receipt instant stands in
	// (`recorded` = 0: `entry.first_inserted_at_ms`, ordinal 0). It is THE ONE
	// HOME of the served place: every page, catch-up, replay and live line reads
	// its place arm from here, and placeRow (write.go) is the one writer, in the
	// transaction of the write that decided it.
	//
	// WHY IN PLACE AND NOT A VERSION BUMP: a bump nukes the owner's database,
	// and with it every stream-plane row the shim wrote live, which no producer
	// can rebuild.
	//
	// THE BACKFILL'S STAND-IN FOR A ROW STORED BEFORE PLACES EXISTED IS EXACT.
	// Such a row was written by a producer that stated no place (the field did
	// not exist), so its served arm is `received_place`, and its receipt instant
	// is not a guess: `entry.first_inserted_at_ms` has recorded every row's
	// first-insert instant since the table existed, and no upsert rewrites it.
	// The file plane's next re-derivation states the recorded place of every
	// row it re-reads, and the row takes it then (the first stated place).
	{"entry_place", `CREATE TABLE IF NOT EXISTS entry_place (
  position      INTEGER PRIMARY KEY,
  book_agent_id TEXT    NOT NULL,
  at_ms         INTEGER NOT NULL,
  ordinal       INTEGER NOT NULL,
  recorded      INTEGER NOT NULL
);
CREATE INDEX IF NOT EXISTS entry_place_book_order ON entry_place(book_agent_id, at_ms, ordinal, position)`,
		`INSERT INTO entry_place (position, book_agent_id, at_ms, ordinal, recorded)
  SELECT position, book_agent_id, first_inserted_at_ms, 0, 0 FROM entry WHERE book_agent_id IS NOT NULL`},
	// THE SHELL RUN CLAIMS (store.v1 ShellRunClaim): the vendor's task id —
	// the name of a detached shell's spool — paired with the run, as the shim
	// read both off the vendor's task stream (EntryBatch.shell_run_claims). The
	// sidecar reads them back through GetShellRunClaims to claim a spool no
	// transcript line claimed. One row per (task id, run), so a re-stated claim
	// is absorbed and a task id claimed by two runs stays visible to the
	// reader, which refuses it rather than choosing. A database that predates
	// the table simply holds no claims, which reads as "no claim yet".
	{"shell_run_claim", `CREATE TABLE IF NOT EXISTS shell_run_claim (
  vendor_task_id TEXT    NOT NULL,
  run_id         TEXT    NOT NULL,
  recorded_at_ms INTEGER NOT NULL,
  PRIMARY KEY (vendor_task_id, run_id)
)`, ""},
}

// indexMigrationError is a failure to build a missing lineage index or
// in-place table. It is a storage failure (it wraps ErrStorage through its
// cause), and it is its own type so Open can tell it apart from a file it
// should discard: the database it failed on is one this binary created, and is
// never nuked for it.
type indexMigrationError struct{ err error }

func (e *indexMigrationError) Error() string { return e.err.Error() }
func (e *indexMigrationError) Unwrap() error { return e.err }

// schemaTables is the exact table set schemaDDL produces, sorted. It is
// compared against what is on disk so a database carrying the RIGHT version
// stamp on the WRONG shape — a half-applied create, a hand-edited file, a
// binary that crashed between DROP and CREATE — is nuked rather than trusted.
var schemaTables = []string{"agent", "cursor", "cursor_conversion", "detached_work", "entry", "entry_place", "residue_shapes", "schema_meta", "shell_run_claim", "vendor_task", "workflow", "write_ledger"}

// shapeTables is a table set with the in-place tables taken out: the part of a
// database's shape that must match EXACTLY, because the in-place tables are the
// ones a database this binary created may still lack.
func shapeTables(tables []string) []string {
	out := make([]string, 0, len(tables))
	for _, table := range tables {
		inPlace := false
		for _, t := range inPlaceTables {
			if t.name == table {
				inPlace = true
				break
			}
		}
		if !inPlace {
			out = append(out, table)
		}
	}
	return out
}

// supersededVersion reports whether an on-disk stamp names a schema THIS
// binary superseded: any version below its own, and not the 0 of a database
// that carries no `schema_meta` at all (somebody else's file, never a version
// of ours). It is what separates the ordinary deploy-time bump from a database
// this binary has no account of.
func supersededVersion(onDisk int) bool { return onDisk > 0 && onDisk < SchemaVersion }

// schemaMismatchError is an on-disk shape this binary did not create. It is
// NOT a storage failure and nothing outside Open ever sees it: it is the signal
// that carries what was found up to the one layer that owns the file, so that
// layer can discard the file itself.
//
// IT DOES NOT WRAP ErrStorage. Nothing is wrong with the database; it is simply
// somebody else's, and classing it as a storage fault would put a routine
// version bump in the same bucket as a full disk.
type schemaMismatchError struct {
	version int
	tables  []string
}

func (e *schemaMismatchError) Error() string {
	return fmt.Sprintf("on-disk schema version %d tables %v was not created by this binary (want version %d tables %v)",
		e.version, e.tables, SchemaVersion, schemaTables)
}

// ensureSchema brings the database to SchemaVersion, or reports that the file
// underneath it has to go.
//
// THERE IS NO MIGRATION AND THERE IS NO BACKFILL. The store holds a cache of
// what the vendor and the shim already know how to produce again, so a shape
// this binary did not create is worth exactly nothing. Writing an ALTER here
// would be the first half of a compatibility surface the whole design exists to
// not have.
//
// AND THERE IS NO DROP EITHER. This function does not empty a foreign database
// in place: DROP TABLE walks every page of whatever it discards, which on a
// large events.db is minutes of boot with no socket. It returns a
// schemaMismatchError and Open unlinks the file, which costs the same whatever
// the file weighs.
func (d *DB) ensureSchema(ctx context.Context, path string) error {
	current, tables, err := d.inspectSchema(ctx)
	if err != nil {
		d.log.Log(logging.Fields{Operation: "store.db.schema", DatabasePath: path, Table: "schema_meta", Level: "error", ErrorCause: err.Error()},
			"reading the on-disk schema failed: %v", err)
		return err
	}
	if current == SchemaVersion && slicesEqual(shapeTables(tables), shapeTables(schemaTables)) {
		d.log.LogVerbose(logging.Fields{Operation: "store.db.schema", DatabasePath: path, Table: "schema_meta"},
			"schema already current version=%d", current)
		if err := d.ensureInPlaceTables(ctx, path, tables); err != nil {
			return err
		}
		return d.ensureIndexes(ctx, path)
	}
	// AN EMPTY FILE IS A FIRST CREATE, NOT A NUKE. Every fresh store — every
	// launch on a new machine, every test process, and every reopen after Open
	// removed a superseded file — arrives here with no tables at all, and
	// warning about it would bury the one case that genuinely deserves the
	// weight: a shape this binary did not create being DISCARDED with whatever
	// was in it. Open emits that warning, because Open is what discards it.
	if len(tables) != 0 {
		return &schemaMismatchError{version: current, tables: tables}
	}
	d.log.Log(logging.Fields{Operation: "store.db.schema", DatabasePath: path, Table: "schema_meta"},
		"no schema on disk; creating it at version=%d tables=%v", SchemaVersion, schemaTables)
	if err := d.createSchema(ctx); err != nil {
		d.log.Log(logging.Fields{Operation: "store.db.schema", DatabasePath: path, Table: "schema_meta", Level: "error", ErrorCause: err.Error()},
			"creating the schema failed: %v", err)
		return err
	}
	d.log.Log(logging.Fields{Operation: "store.db.schema", DatabasePath: path, Table: "schema_meta"},
		"schema created at version=%d", SchemaVersion)
	return nil
}

// inspectSchema reports the stamped version (0 when there is none) and the
// user tables present, sorted.
func (d *DB) inspectSchema(ctx context.Context) (int, []string, error) {
	rows, err := d.sql.QueryContext(ctx, `SELECT name FROM sqlite_master WHERE type = 'table' AND name NOT LIKE 'sqlite_%'`)
	if err != nil {
		return 0, nil, storagef(err, "listing tables")
	}
	var tables []string
	for rows.Next() {
		var name string
		if err := rows.Scan(&name); err != nil {
			rows.Close() //nolint:errcheck // the scan already failed
			return 0, nil, storagef(err, "scanning a table name")
		}
		tables = append(tables, name)
	}
	if err := rows.Err(); err != nil {
		rows.Close() //nolint:errcheck // the iteration already failed
		return 0, nil, storagef(err, "iterating table names")
	}
	if err := rows.Close(); err != nil {
		return 0, nil, storagef(err, "closing the table listing")
	}
	sort.Strings(tables)

	if !contains(tables, "schema_meta") {
		return 0, tables, nil
	}
	var version int
	switch err := d.sql.QueryRowContext(ctx, `SELECT version FROM schema_meta LIMIT 1`).Scan(&version); {
	case err == nil:
		return version, tables, nil
	case errors.Is(err, sql.ErrNoRows):
		// The table exists and claims nothing. Unstamped is not current.
		return 0, tables, nil
	default:
		// A `schema_meta` whose shape this binary cannot even read is exactly
		// the case the nuke exists for, so it is reported as version 0 rather
		// than failing the open.
		return 0, tables, nil
	}
}

// createSchema applies schemaDDL to an EMPTY database, stamping the version in
// the SAME transaction so the schema and the claim about it can never disagree.
//
// IT NEVER DROPS ANYTHING. Its only caller has already established that there
// are no tables — a foreign shape never reaches here, because Open removed the
// file it was in.
func (d *DB) createSchema(ctx context.Context) error {
	// THROUGH THE GATE LIKE EVERY OTHER WRITE. Nothing else is running yet at
	// open, so this never queues; it goes through beginWrite anyway so that
	// "every write transaction in this package is opened by beginWrite" is a
	// property anyone can check by grepping for BeginTx rather than a habit.
	tx, release, err := d.beginWrite(ctx, WriteBulk)
	if err != nil {
		return storagef(err, "begin schema creation")
	}
	defer release()
	defer d.endTx(tx, logging.Fields{Operation: "store.db.schema", DatabasePath: d.path, Table: "sqlite_master"})

	if _, err := tx.ExecContext(ctx, schemaDDL); err != nil {
		return storagef(err, "creating the schema")
	}
	// A fresh database gets the in-place tables and the lineage indexes in the
	// SAME transaction as the rest, so no database this binary creates is ever
	// without them.
	for _, table := range inPlaceTables {
		if err := createInPlaceTable(ctx, tx, table.name, table.ddl, table.backfill); err != nil {
			return err
		}
	}
	for _, index := range lineageIndexes {
		if _, err := tx.ExecContext(ctx, index.ddl); err != nil {
			return storagef(err, "creating index %s", index.name)
		}
	}
	if _, err := tx.ExecContext(ctx, `INSERT INTO schema_meta(version) VALUES (?)`, SchemaVersion); err != nil {
		return storagef(err, "stamping schema version %d", SchemaVersion)
	}
	if err := tx.Commit(); err != nil {
		return storagef(err, "committing schema creation")
	}
	return nil
}

// ensureIndexes builds, IN PLACE, every lineage index a matching database is
// missing. It is the store's one in-place schema step, and it only ever ADDS an
// index: nothing is dropped, rebuilt or rewritten, and a database carrying
// every index is left untouched (one verbose record, no write transaction).
//
// A failure is recorded here, once, at ERROR, and returned as an
// indexMigrationError, which Open refuses to answer with a nuke.
func (d *DB) ensureIndexes(ctx context.Context, path string) error {
	fields := logging.Fields{Operation: "store.db.schema", DatabasePath: path, Table: "sqlite_master"}
	fail := func(err error) error {
		failed := fields
		failed.Level = "error"
		failed.ErrorCause = err.Error()
		d.log.Log(failed, "building the missing lineage indexes in place failed; the database is left as it was and the open fails: %v", err)
		return &indexMigrationError{err: err}
	}
	present, err := d.indexNames(ctx)
	if err != nil {
		return fail(err)
	}
	var missing []string
	for _, index := range lineageIndexes {
		if !contains(present, index.name) {
			missing = append(missing, index.name)
		}
	}
	if len(missing) == 0 {
		d.log.LogVerbose(fields, "every lineage index is present count=%d", len(lineageIndexes))
		return nil
	}

	started := d.mono()
	tx, release, err := d.beginWrite(ctx, WriteBulk)
	if err != nil {
		return fail(storagef(err, "begin the lineage index build"))
	}
	defer release()
	defer d.endTx(tx, fields)
	for _, index := range lineageIndexes {
		if !contains(missing, index.name) {
			continue
		}
		if _, err := tx.ExecContext(ctx, index.ddl); err != nil {
			return fail(storagef(err, "creating index %s", index.name))
		}
	}
	if err := tx.Commit(); err != nil {
		return fail(storagef(err, "committing the lineage index build"))
	}
	d.log.Log(fields, "built the missing lineage indexes in place indexes=%v duration_ms=%d",
		missing, d.mono().Sub(started).Milliseconds())
	return nil
}

// ensureInPlaceTables builds, IN PLACE, every inPlaceTables entry a matching
// database is missing. Like ensureIndexes it only ever ADDS: a database carrying
// every such table is left untouched (one verbose record, no write
// transaction), and a failure is recorded once at ERROR and returned as an
// indexMigrationError, which Open refuses to answer with a nuke.
func (d *DB) ensureInPlaceTables(ctx context.Context, path string, present []string) error {
	fields := logging.Fields{Operation: "store.db.schema", DatabasePath: path, Table: "sqlite_master"}
	var missing []string
	for _, table := range inPlaceTables {
		if !contains(present, table.name) {
			missing = append(missing, table.name)
		}
	}
	if len(missing) == 0 {
		d.log.LogVerbose(fields, "every in-place table is present count=%d", len(inPlaceTables))
		return nil
	}
	fail := func(err error) error {
		failed := fields
		failed.Level = "error"
		failed.ErrorCause = err.Error()
		d.log.Log(failed, "building the missing in-place tables failed; the database is left as it was and the open fails: %v", err)
		return &indexMigrationError{err: err}
	}
	started := d.mono()
	tx, release, err := d.beginWrite(ctx, WriteBulk)
	if err != nil {
		return fail(storagef(err, "begin the in-place table build"))
	}
	defer release()
	defer d.endTx(tx, fields)
	for _, table := range inPlaceTables {
		if !contains(missing, table.name) {
			continue
		}
		if err := createInPlaceTable(ctx, tx, table.name, table.ddl, table.backfill); err != nil {
			return fail(err)
		}
	}
	if err := tx.Commit(); err != nil {
		return fail(storagef(err, "committing the in-place table build"))
	}
	d.log.Log(fields, "built the missing in-place tables tables=%v duration_ms=%d",
		missing, d.mono().Sub(started).Milliseconds())
	return nil
}

// createInPlaceTable creates one in-place table and runs its backfill, if it
// has one, inside the caller's transaction, so the table never exists without
// the bookkeeping its reader relies on.
func createInPlaceTable(ctx context.Context, tx *sql.Tx, name, ddl, backfill string) error {
	if _, err := tx.ExecContext(ctx, ddl); err != nil {
		return storagef(err, "creating table %s", name)
	}
	if backfill == "" {
		return nil
	}
	if _, err := tx.ExecContext(ctx, backfill); err != nil {
		return storagef(err, "backfilling table %s", name)
	}
	return nil
}

// indexNames lists every index on disk, sorted.
func (d *DB) indexNames(ctx context.Context) ([]string, error) {
	rows, err := d.sql.QueryContext(ctx, `SELECT name FROM sqlite_master WHERE type = 'index'`)
	if err != nil {
		return nil, storagef(err, "listing indexes")
	}
	defer rows.Close() //nolint:errcheck // the deferred close of a read
	var names []string
	for rows.Next() {
		var name string
		if err := rows.Scan(&name); err != nil {
			return nil, storagef(err, "scanning an index name")
		}
		names = append(names, name)
	}
	if err := rows.Err(); err != nil {
		return nil, storagef(err, "iterating index names")
	}
	sort.Strings(names)
	return names, nil
}

func contains(haystack []string, needle string) bool {
	for _, value := range haystack {
		if value == needle {
			return true
		}
	}
	return false
}

func slicesEqual(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

// queryError records one database failure exactly once, at its owning layer,
// and hands the caller an ErrStorage.
func (d *DB) queryError(operation, table string, fields logging.Fields, err error) error {
	fields.Operation = operation
	fields.Table = table
	fields.Level = "error"
	fields.ErrorCause = err.Error()
	d.log.Log(fields, "database statement failed: %v", err)
	return err
}
