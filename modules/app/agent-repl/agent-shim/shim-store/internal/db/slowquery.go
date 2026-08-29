package db

import (
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
	if d.slowQuery <= 0 {
		return
	}
	elapsed := time.Since(started)
	if elapsed < d.slowQuery {
		return
	}
	fields.Operation = SlowQueryOperation
	fields.Level = "warn"
	fields.Table = table
	fields.Statement = statement
	fields.Duration = elapsed
	fields.Rows = rows
	fields.Threshold = d.slowQuery
	d.log.Log(fields, "SQLite statement exceeded the slow-query threshold statement=%s duration_ms=%d rows=%d threshold_ms=%d",
		statement, elapsed.Milliseconds(), rows, d.slowQuery.Milliseconds())
}
