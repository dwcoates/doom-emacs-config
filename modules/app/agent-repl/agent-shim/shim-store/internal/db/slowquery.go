package db

import (
	"fmt"
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
// and far shorter than the multi-second replays a multi-gigabyte events.db
// produces, so it separates "this database is big" from "this query is the
// problem" without reporting healthy traffic.
const DefaultSlowQuery = 250 * time.Millisecond

// SlowQueryOperation is the stable operation name every slow-query record
// carries. Operators query THIS rather than message text.
const SlowQueryOperation = "store.db.slow-query"

// Statement families. They name WHAT ran, never the SQL and never its bound
// values: the store's payloads are opaque to it, and a record quoting a
// parameterized statement would leak session content into the global log.
const (
	StatementReplay       = "replay"
	StatementMaxSeq       = "max_seq"
	StatementEventsByTask = "events_by_task"
	StatementOpenTasks    = "open_tasks"
	StatementListCursors  = "list_cursors"
	StatementCursor       = "cursor"
	StatementIngest       = "ingest"
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
		return 0, fmt.Errorf("shim-store db: %s=%q is not an integer number of milliseconds: %w", EnvSlowQueryMs, raw, err)
	}
	if ms <= 0 {
		return 0, fmt.Errorf("shim-store db: %s=%q must be a positive number of milliseconds", EnvSlowQueryMs, raw)
	}
	return time.Duration(ms) * time.Millisecond, nil
}

// observeQuery reports one completed statement that took longer than the
// threshold, and says nothing at all about one that did not.
//
// THE RECORD IS NORMAL-VERBOSITY WARN, deliberately. Successful query timing
// is exactly the high-volume per-operation narration the store's verbose gate
// exists to keep out of a singleton global log; a query that blew the threshold
// is the opposite — the operator must see it without having enabled anything in
// advance, because by the time they know to look the subscription replay or
// backfill that stalled is already over.
//
// rows is what the statement actually produced or touched, which is the term
// that distinguishes a slow query from a large answer.
func (d *DB) observeQuery(statement, table, session string, started time.Time, rows int64) {
	if d.slowQuery <= 0 {
		return
	}
	elapsed := time.Since(started)
	if elapsed < d.slowQuery {
		return
	}
	d.log.Log(logging.Fields{
		Operation: SlowQueryOperation, Level: "warn", Table: table, Session: session,
		Statement: statement, Duration: elapsed, Rows: rows, Threshold: d.slowQuery,
	}, "SQLite statement exceeded the slow-query threshold statement=%s duration_ms=%d rows=%d threshold_ms=%d",
		statement, elapsed.Milliseconds(), rows, d.slowQuery.Milliseconds())
}
