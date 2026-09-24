package db

import (
	"context"
	"os"
	"strconv"
	"time"

	"agentrepl/shim-store/internal/logging"
)

// EnvSlowQueryMs is the store's one configuration surface for the slow-query
// threshold, in milliseconds.
const EnvSlowQueryMs = "AGENT_REPL_STORE_SLOW_QUERY_MS"

// DefaultSlowQuery is the duration past which a statement is reported.
//
// A quarter second is far longer than any indexed lookup this schema performs
// and far shorter than the multi-second replays a large store produces, so it
// separates "this database is big" from "this query is the problem" without
// reporting healthy traffic.
const DefaultSlowQuery = 250 * time.Millisecond

// EnvBulkBaseMs and EnvBulkPerRowMs configure the write_batch bulk budget.
const (
	EnvBulkBaseMs   = "AGENT_REPL_STORE_BULK_BASE_MS"
	EnvBulkPerRowMs = "AGENT_REPL_STORE_BULK_PER_ROW_MS"
)

// DefaultBulkBase and DefaultBulkPerRow size the write_batch bulk budget, which
// is a per-row I/O budget on top of a fixed base rather than the interactive
// point-query threshold.
//
// WHY write_batch NEEDS ITS OWN BUDGET. Every write-path statement is fully
// indexed — MAX(write_seq) is a covering-index seek, the write_id and upsert_key
// probes ride unique indexes, and the upsert's conflict target is that same
// unique index (EXPLAIN QUERY PLAN receipts are in the ledger row for this
// change). So a slow write_batch is not a query defect; it is bulk I/O. One
// batch inserts hundreds of rows, and each insert maintains SEVEN indexes on
// `entry` plus two on `write_ledger`, whose B-tree page splits scatter across an
// 11GB file with a large WAL and a cold page cache. That cost grows with the row
// count, so a FIXED quarter-second threshold flags every healthy bulk write on a
// large database while telling an operator nothing they can act on.
//
// AND IT IS STILL NOT THE WHOLE STORY, WHICH IS WHY `lock_wait_ms` EXISTS. A
// batch's measured duration starts before its transaction does, and every
// transaction here is BEGIN IMMEDIATE, so a batch that queued behind another
// caller's write lock blows any row-scaled budget without having done a row's
// worth of work — the owner's store reported 3822ms for SIX rows. The budget
// deliberately still covers the total, because a batch nobody can start is as
// slow to its caller as one that runs slowly; the record's `lock_wait_ms`
// context is what says which of the two happened.
//
// The budget scales with rows so a healthy large batch does not warn, while a
// per-row cost far above the I/O budget — a reintroduced O(n) scan, a lost index
// — still blows past it and warns. The default per-row budget sits comfortably
// above the ~3.5ms/row a healthy bulk write on the owner's 11GB database was
// measured at.
const (
	DefaultBulkBase   = 250 * time.Millisecond
	DefaultBulkPerRow = 5 * time.Millisecond
)

// SlowQueryOperation is the stable operation name every slow-query record
// carries. Operators query THIS rather than message text.
const SlowQueryOperation = "store.db.slow-query"

// Statement families. They name WHAT ran, never the SQL and never its bound
// values: the store's frames are opaque to it, and a record quoting a
// parameterized statement would leak conversation content into the global log.
//
// The families are the four-table schema's own statements. The retired ones
// (`replay`, `max_seq`, `ingest`, `message_page`) named the (session_id, seq)
// addressing and died with it.
const (
	StatementWriteBatch  = "write_batch"
	StatementOpenPage    = "open_page"
	StatementReadPage    = "read_page"
	StatementLinesSince  = "lines_since"
	StatementLiveWork    = "live_work"
	StatementListCursors = "list_cursors"
	StatementBashRun     = "bash_run"
	// StatementResidueShapes is the residue shape catalog listing.
	StatementResidueShapes = "residue_shapes"
	// StatementLedgerSweep is one transaction of the write-ledger sweep. It
	// holds the one writer like any batch does, so it is timed like one: on
	// 2026-09-23 two sweeps held the writer for 137s and 848s and left no
	// record at all, because a sweep that removed nothing was only ever
	// narrated at verbose.
	StatementLedgerSweep = "ledger_sweep"
)

// SlowQueryFromEnv resolves the slow-query threshold from the environment.
//
// A malformed or non-positive value is an ERROR, never a silent fall back to
// the default: an operator who set AGENT_REPL_STORE_SLOW_QUERY_MS=0 meant
// something by it, and running the shipped quarter second while they believe
// they changed the knob is the failure a loud refusal exists to prevent.
func SlowQueryFromEnv() (time.Duration, error) {
	raw := os.Getenv(EnvSlowQueryMs)
	if raw == "" {
		return DefaultSlowQuery, nil
	}
	ms, err := strconv.ParseInt(raw, 10, 64)
	if err != nil {
		return 0, invalidf("%s=%q is not an integer number of milliseconds: %v", EnvSlowQueryMs, raw, err)
	}
	if ms <= 0 {
		return 0, invalidf("%s=%q must be a positive number of milliseconds", EnvSlowQueryMs, raw)
	}
	return time.Duration(ms) * time.Millisecond, nil
}

// BulkBudgetFromEnv resolves the write_batch bulk budget from the environment.
//
// Like SlowQueryFromEnv, a malformed or non-positive value is a loud refusal,
// never a silent fall back to the shipped default: an operator who set the knob
// meant something by it, and running the default underneath them is the failure
// the refusal exists to prevent. The base may be non-negative (a zero base
// budgets purely per row); the per-row budget must be positive so the budget
// actually grows with the batch.
func BulkBudgetFromEnv() (base, perRow time.Duration, err error) {
	base = DefaultBulkBase
	if raw := os.Getenv(EnvBulkBaseMs); raw != "" {
		ms, parseErr := strconv.ParseInt(raw, 10, 64)
		if parseErr != nil {
			return 0, 0, invalidf("%s=%q is not an integer number of milliseconds: %v", EnvBulkBaseMs, raw, parseErr)
		}
		if ms < 0 {
			return 0, 0, invalidf("%s=%q must not be negative", EnvBulkBaseMs, raw)
		}
		base = time.Duration(ms) * time.Millisecond
	}
	perRow = DefaultBulkPerRow
	if raw := os.Getenv(EnvBulkPerRowMs); raw != "" {
		ms, parseErr := strconv.ParseInt(raw, 10, 64)
		if parseErr != nil {
			return 0, 0, invalidf("%s=%q is not an integer number of milliseconds: %v", EnvBulkPerRowMs, raw, parseErr)
		}
		if ms <= 0 {
			return 0, 0, invalidf("%s=%q must be a positive number of milliseconds", EnvBulkPerRowMs, raw)
		}
		perRow = time.Duration(ms) * time.Millisecond
	}
	return base, perRow, nil
}

// budgetFor is the threshold one statement must exceed to be reported. It is the
// fixed interactive threshold for a point query and the row-scaled bulk budget
// for a write_batch, so a healthy large bulk write does not warn while a
// genuinely pathological one still does.
//
// A non-positive slowQuery is the master switch: reporting is disabled entirely,
// bulk included. The bulk budget never drops below the interactive floor, so a
// tiny or empty batch that still blows the point-query threshold is not hidden.
func (d *DB) budgetFor(statement string, rows int64) time.Duration {
	if d.slowQuery <= 0 {
		return 0
	}
	if statement != StatementWriteBatch {
		return d.slowQuery
	}
	budget := d.bulkBase + time.Duration(rows)*d.bulkPerRow
	if budget < d.slowQuery {
		budget = d.slowQuery
	}
	return budget
}

// traceStatement records that one statement family RAN, for this request.
//
// IT IS THE EVIDENCE THAT A REQUEST REACHED STORAGE, and it exists because the
// negative is what the suite needs to assert: "a refused request never opened a
// transaction" is only checkable if a request that DID reach storage leaves a
// mark tied to it. The slow-query record could not serve — it fires only past a
// threshold, so its absence means "fast", not "never ran".
//
// Verbose, because it is per-operation narration on the hot path; the request id
// comes off the context, which is the one parameter that already crosses every
// storage signature and already means "this call".
func (d *DB) traceStatement(ctx context.Context, statement, table string, fields logging.Fields, rows int64) {
	fields.Operation = "store.db.statement"
	fields.Level = "debug"
	fields.Table = table
	fields.Statement = statement
	fields.Rows = rows
	fields.RequestID = logging.RequestIDFrom(ctx)
	d.log.LogVerbose(fields, "statement ran statement=%s rows=%d", statement, rows)
}

// BudgetWindow is how many observations of one statement family the store keeps
// to decide whether being over budget is a DEFECT or a spike, and
// BudgetWarnAt is how many of that window must be over budget before the
// family is reported at `warn`.
//
// WHY A WINDOW AT ALL. `duration_ms` is WALL CLOCK, and wall clock on a shared
// host measures the host as much as the statement. Measured on the owner's box
// on 2026-09-13: fifteen `store.db.slow-query` warnings in one hour, every one
// of them `lock_wait_ms=0` — so not queued — and the largest of them
// `duration_ms=2543 rows=25`. The same batch shape, run against a byte-for-byte
// COPY of that same 1.65 GB database with the same driver and the same DSN,
// takes 1-2ms: 400 consecutive 7-row batches with six concurrent page readers
// and the WAL grown to 113 MB under a pinned reader had a WORST case of 15ms,
// and begin, MAX(write_seq), the probes and the commit were each sub-
// millisecond throughout. Nothing about the statements, the indexes, the
// database size or the WAL explains 1595ms; a loaded host does, and the store
// cannot measure that apart from its own work because modernc's SQLite runs
// in-process, on the calling goroutine, so a descheduled goroutine and a slow
// statement are the same wall clock.
//
// WHAT THE WINDOW DOES MEASURE is the shape of the two causes, which differ
// completely. The defects this budget exists to catch — a lost index, a
// reintroduced O(n) scan — are PROPERTIES OF THE STATEMENT, so they make every
// statement of that family slow and fill the window. Host contention is a tail:
// it takes whichever statement was unlucky. So a family that is persistently
// over budget is reported at `warn` exactly as before, and an isolated sample
// is reported at `info` — recorded, at normal verbosity, with the window state
// that says why it was not called a defect. The observation is never dropped.
const (
	BudgetWindow = 16
	BudgetWarnAt = 8
)

// budgetWindow is one statement family's last BudgetWindow verdicts, as a ring.
type budgetWindow struct {
	verdicts [BudgetWindow]bool
	next     int
	filled   int
	over     int
}

// record adds one verdict and reports how many of the retained window are over
// budget, alongside how many observations that window holds.
func (w *budgetWindow) record(over bool) (int, int) {
	if w.filled == BudgetWindow && w.verdicts[w.next] {
		w.over--
	}
	w.verdicts[w.next] = over
	if over {
		w.over++
	}
	w.next = (w.next + 1) % BudgetWindow
	if w.filled < BudgetWindow {
		w.filled++
	}
	return w.over, w.filled
}

// observeBudget records one family's verdict and reports the window.
//
// It is the ONE place the per-family state is touched, and it is touched under
// the mutex because every producer's rpc runs on its own goroutine against one
// shared DB.
func (d *DB) observeBudget(statement string, over bool) (int, int) {
	d.budgetMu.Lock()
	defer d.budgetMu.Unlock()
	if d.budgets == nil {
		d.budgets = map[string]*budgetWindow{}
	}
	w := d.budgets[statement]
	if w == nil {
		w = &budgetWindow{}
		d.budgets[statement] = w
	}
	return w.record(over)
}

// observeQuery reports one completed statement that took longer than the
// threshold, and says nothing at all about one that did not.
//
// THE RECORD IS NORMAL-VERBOSITY WARN, deliberately. Successful query timing is
// exactly the high-volume per-operation narration the store's verbose gate
// exists to keep out of a singleton global log; a query that blew the threshold
// is the opposite — the operator must see it without having enabled anything in
// advance, because by the time they know to look, the replay or page walk that
// stalled is already over.
//
// rows is what the statement actually produced or touched, which is the term
// that distinguishes a slow query from a large answer, and `fields.LockWait` is
// the term that distinguishes a slow statement from a QUEUED one. The caller
// that can measure a wait sets it before the observation runs; a statement with
// no wait to measure reports zero, which is a fact rather than an omission.
func (d *DB) observeQuery(statement, table string, fields logging.Fields, started time.Time, rows int64) {
	threshold := d.budgetFor(statement, rows)
	if threshold <= 0 {
		return
	}
	elapsed := d.mono().Sub(started)
	// A WRITE'S WINDOW IS KEPT PER CLASS. A bulk transaction and an
	// interactive one share a statement family but not a cause: a backlog of
	// bulk work over budget must not make an interactive write's isolated
	// spike read as a persistent defect, nor the reverse.
	windowKey := statement
	classSuffix := ""
	if fields.WriteClass != "" {
		windowKey = statement + "/" + fields.WriteClass
		classSuffix = " write_class=" + fields.WriteClass
		fields.Exec = elapsed - fields.LockWait
	}
	over, window := d.observeBudget(windowKey, elapsed >= threshold)
	if elapsed < threshold {
		return
	}
	fields.Operation = SlowQueryOperation
	fields.Table = table
	fields.Statement = statement
	fields.Duration = elapsed
	fields.Rows = rows
	fields.Threshold = threshold
	fields.OverBudget = over
	fields.BudgetWindow = window
	if over >= BudgetWarnAt {
		fields.Level = "warn"
		d.log.Log(fields, "SQLite statement family is persistently over its budget statement=%s duration_ms=%d lock_wait_ms=%d rows=%d threshold_ms=%d over_budget_recent=%d/%d%s",
			statement, elapsed.Milliseconds(), fields.LockWait.Milliseconds(), rows, threshold.Milliseconds(), over, window, classSuffix)
		return
	}
	fields.Level = "info"
	d.log.Log(fields, "SQLite statement exceeded its budget on an isolated sample — the family's recent statements are within budget statement=%s duration_ms=%d lock_wait_ms=%d rows=%d threshold_ms=%d over_budget_recent=%d/%d%s",
		statement, elapsed.Milliseconds(), fields.LockWait.Milliseconds(), rows, threshold.Milliseconds(), over, window, classSuffix)
}

// WriteTimingOperation is the operation every per-write timing record carries.
const WriteTimingOperation = "store.db.write-timing"

// traceWriteTiming records one write transaction's queue wait and execution
// time, by class — EVERY write, not only a slow one.
//
// The slow-query record answers "was this one over budget"; this answers "what
// is the writer's queue doing", which needs the healthy writes too: an
// interactive write that waited 40ms behind a bulk transaction is within every
// budget and is exactly the number the two-tier queue exists to keep small.
// It is VERBOSE because it is per-operation narration on the hot path, like
// the statement trace beside it.
func (d *DB) traceWriteTiming(statement string, fields logging.Fields, started time.Time, rows int64, transaction int) {
	elapsed := d.mono().Sub(started)
	fields.Operation = WriteTimingOperation
	fields.Level = "debug"
	fields.Statement = statement
	fields.Duration = elapsed
	fields.Exec = elapsed - fields.LockWait
	fields.Rows = rows
	d.log.LogVerbose(fields, "write timed statement=%s write_class=%s transaction=%d queue_wait_ms=%d exec_ms=%d rows=%d",
		statement, fields.WriteClass, transaction, fields.LockWait.Milliseconds(), fields.Exec.Milliseconds(), rows)
}
