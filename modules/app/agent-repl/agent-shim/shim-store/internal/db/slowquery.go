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
// that distinguishes a slow query from a large answer.
func (d *DB) observeQuery(statement, table string, fields logging.Fields, started time.Time, rows int64) {
	threshold := d.budgetFor(statement, rows)
	if threshold <= 0 {
		return
	}
	elapsed := time.Since(started)
	if elapsed < threshold {
		return
	}
	fields.Operation = SlowQueryOperation
	fields.Level = "warn"
	fields.Table = table
	fields.Statement = statement
	fields.Duration = elapsed
	fields.Rows = rows
	fields.Threshold = threshold
	d.log.Log(fields, "SQLite statement exceeded the slow-query threshold statement=%s duration_ms=%d rows=%d threshold_ms=%d",
		statement, elapsed.Milliseconds(), rows, threshold.Milliseconds())
}
