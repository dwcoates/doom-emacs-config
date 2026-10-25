package wsm

import (
	"context"
	"database/sql"
	"errors"
	"reflect"
	"regexp"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/sourcescan"
)

// TestANestedStoreCallRunsOnItsOperationsConnection pins the reuse: the pool
// holds one connection, so a store call made inside an operation that holds
// it could never get a second one. It runs on the outer one instead.
func TestANestedStoreCallRunsOnItsOperationsConnection(t *testing.T) {
	tests := []struct {
		name  string
		outer func(s *store, ctx context.Context, fn func(context.Context) error) error
	}{
		{name: "inside a read", outer: func(s *store, ctx context.Context, fn func(context.Context) error) error {
			return s.read(ctx, "test.outer", dlog.Context{}, fn)
		}},
		{name: "inside a write", outer: func(s *store, ctx context.Context, fn func(context.Context) error) error {
			return s.write(ctx, "test.outer", dlog.Context{}, func(ctx context.Context, _ *sql.Tx) error { return fn(ctx) })
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
			defer cancel()
			var outerConn, innerConn connQuerier

			// Act
			err := tt.outer(s, ctx, func(ctx context.Context) error {
				outerConn = s.q(ctx)
				return s.read(ctx, "test.inner", dlog.Context{}, func(ctx context.Context) error {
					innerConn = s.q(ctx)
					var one int
					return s.q(ctx).QueryRowContext(ctx, `SELECT 1`).Scan(&one)
				})
			})

			// Assert
			if err != nil {
				t.Fatalf("nested store call = %v, want it to run on the outer connection", err)
			}
			if _, ok := outerConn.(*sql.Conn); !ok {
				t.Fatalf("outer querier = %T, want the connection the operation holds", outerConn)
			}
			if innerConn != outerConn {
				t.Fatalf("inner querier = %p, want the outer operation's %p", innerConn, outerConn)
			}
		})
	}
}

// TestOutsideAnOperationTheQuerierIsThePool pins the other side of q: a
// statement run outside any read or write goes to the pool.
func TestOutsideAnOperationTheQuerierIsThePool(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	got := s.q(context.Background())

	// Assert
	if got != connQuerier(s.db()) {
		t.Fatalf("q() = %T, want the pool", got)
	}
}

func TestConnUseRecordsAHoldPastItsBudget(t *testing.T) {
	tests := []struct {
		name     string
		held     time.Duration
		wantHold bool
	}{
		{name: "a hold under the budget records nothing", held: connHoldBudget - time.Millisecond},
		{name: "a hold at the budget is recorded", held: connHoldBudget, wantHold: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			log := dlog.NewTestLogger()
			var u connUse

			// Act
			u.note(log, "daemon.wsm.sample", dlog.Context{"workspace": "w1"}, instant, instant.Add(10*time.Millisecond), instant.Add(10*time.Millisecond+tt.held))

			// Assert
			rec, found := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Operation: opConnHold})
			if found != tt.wantHold {
				t.Fatalf("hold record found = %v, want %v; records = %+v", found, tt.wantHold, log.Records())
			}
			if !found {
				return
			}
			want := dlog.Context{"workspace": "w1", "operation": "daemon.wsm.sample", "held_ms": tt.held.Milliseconds(), "waited_ms": int64(10), "budget_ms": connHoldBudget.Milliseconds()}
			if !reflect.DeepEqual(rec.Context, want) {
				t.Fatalf("hold record context = %+v, want %+v", rec.Context, want)
			}
		})
	}
}

// TestConnUseNamesWhatHeldTheConnectionDuringALongWait pins the pressure
// record: the holds released while the waiter waited, heaviest first, and
// none from before it asked.
func TestConnUseNamesWhatHeldTheConnectionDuringALongWait(t *testing.T) {
	// Arrange: one hold before the wait, three during it.
	log := dlog.NewTestLogger()
	var u connUse
	hold := func(op string, releasedAt, held time.Duration) {
		released := instant.Add(releasedAt)
		u.note(log, op, dlog.Context{}, released.Add(-held), released.Add(-held), released)
	}
	hold("daemon.wsm.before", 0, 50*time.Millisecond)
	hold("daemon.wsm.light", 200*time.Millisecond, 10*time.Millisecond)
	hold("daemon.wsm.heavy", 600*time.Millisecond, 200*time.Millisecond)
	hold("daemon.wsm.heavy", 900*time.Millisecond, 200*time.Millisecond)

	// Act: a waiter that asked after the first hold and waited the budget.
	asked := instant.Add(100 * time.Millisecond)
	taken := asked.Add(connWaitBudget)
	u.note(log, "daemon.wsm.waiter", dlog.Context{"workspace": "w1"}, asked, taken, taken.Add(time.Millisecond))

	// Assert
	rec, found := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Operation: opConnPressure})
	if !found {
		t.Fatalf("no pressure record; records = %+v", log.Records())
	}
	want := dlog.Context{
		"workspace": "w1", "operation": "daemon.wsm.waiter", "waited_ms": connWaitBudget.Milliseconds(),
		"budget_ms": connWaitBudget.Milliseconds(), "holds_meanwhile": 3, "held_meanwhile_ms": int64(410),
		"holders":                      []string{"daemon.wsm.heavy 2×400ms", "daemon.wsm.light 1×10ms"},
		"waits_past_budget_since_last": 0,
	}
	if !reflect.DeepEqual(rec.Context, want) {
		t.Fatalf("pressure record context = %+v, want %+v", rec.Context, want)
	}
}

// TestConnUseWritesOnePressureRecordPerInterval pins the bound: a starved
// connection has every waiter past its budget at once, so one record per
// interval stands for them, and the next says how many it absorbed.
func TestConnUseWritesOnePressureRecordPerInterval(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	var u connUse
	wait := func(at time.Duration) {
		taken := instant.Add(at)
		u.note(log, "daemon.wsm.waiter", dlog.Context{}, taken.Add(-connWaitBudget), taken, taken)
	}

	// Act: three waits inside one interval, then one past it.
	wait(0)
	wait(connPressureEvery / 4)
	wait(connPressureEvery / 2)
	wait(connPressureEvery)

	// Assert
	records := dlog.FilterRecords(log.Records(), dlog.RecordQuery{Operation: opConnPressure})
	if len(records) != 2 {
		t.Fatalf("pressure records = %d, want one per interval (2); records = %+v", len(records), records)
	}
	if got := records[1].Context["waits_past_budget_since_last"]; got != 2 {
		t.Fatalf("absorbed waits = %v, want 2", got)
	}
}

func TestConnUseNamesAtMostTheHeaviestHolders(t *testing.T) {
	// Arrange: more distinct holders than a record names.
	log := dlog.NewTestLogger()
	var u connUse
	for i := range connHoldersNamed + 2 {
		released := instant.Add(time.Duration(i+1) * time.Millisecond)
		u.note(log, "daemon.wsm.op"+strings.Repeat("x", i), dlog.Context{}, released, released, released.Add(time.Duration(i+1)*time.Millisecond))
	}

	// Act
	holders, count, _ := u.holdersBetween(instant, instant.Add(time.Second))

	// Assert
	if count != connHoldersNamed+2 {
		t.Fatalf("holds counted = %d, want every one (%d)", count, connHoldersNamed+2)
	}
	if len(holders) != connHoldersNamed {
		t.Fatalf("holders named = %d, want the heaviest %d", len(holders), connHoldersNamed)
	}
	if !strings.HasPrefix(holders[0], "daemon.wsm.op"+strings.Repeat("x", connHoldersNamed+1)+" ") {
		t.Fatalf("heaviest holder = %q, want the longest hold first", holders[0])
	}
}

// TestAWaitCutShortBeginsNoTransaction pins the write's arm for a connection
// it never got: the caller's context ending while it waited is recorded as
// the transaction that never began, at INFO, and nothing is written.
func TestAWaitCutShortBeginsNoTransaction(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	ran := false

	// Act
	err := s.write(ctx, "daemon.wsm.sample", dlog.Context{"workspace": "w1"}, func(context.Context, *sql.Tx) error {
		ran = true
		return nil
	})

	// Assert
	if !errors.Is(err, context.Canceled) || !strings.Contains(err.Error(), "wsm: begin transaction") {
		t.Fatalf("write err = %v, want the transaction that never began", err)
	}
	if ran {
		t.Fatalf("the write's body ran without the connection")
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Operation: "daemon.wsm.sample", Message: writeCancelledMessage}); !ok {
		t.Fatalf("no INFO cancellation record; records = %+v", log.Records())
	}
}

// TestAConnectionThatCannotBeReturnedIsAnError covers the release's failure:
// a connection the pool will not take back is recorded at ERROR with the
// operation's fields, and the operation answers it.
func TestAConnectionThatCannotBeReturnedIsAnError(t *testing.T) {
	// Arrange: the body returns the connection itself, so the release finds
	// it already gone.
	s, log := testStore(t)

	// Act
	err := s.read(context.Background(), "daemon.wsm.sample", dlog.Context{"workspace": "w1"}, func(ctx context.Context) error {
		return ctx.Value(connKey{}).(*sql.Conn).Close()
	})

	// Assert
	if !errors.Is(err, sql.ErrConnDone) {
		t.Fatalf("read err = %v, want the failed release", err)
	}
	rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.wsm.sample", Message: "could not return the state database's connection to its pool"})
	if !ok {
		t.Fatalf("no ERROR release record; records = %+v", log.Records())
	}
	if rec.Context["workspace"] != "w1" {
		t.Fatalf("release record context = %+v, want the operation's fields", rec.Context)
	}
}

// TestEveryOperationIsAccounted pins that reads and writes alike reach the
// accounting: each leaves its hold in the ring.
func TestEveryOperationIsAccounted(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	before := s.conns.next

	// Act
	if err := s.read(context.Background(), "daemon.wsm.sample_read", dlog.Context{}, func(context.Context) error { return nil }); err != nil {
		t.Fatalf("read: %v", err)
	}
	if err := s.write(context.Background(), "daemon.wsm.sample_write", dlog.Context{}, func(context.Context, *sql.Tx) error { return nil }); err != nil {
		t.Fatalf("write: %v", err)
	}

	// Assert
	if got := s.conns.next - before; got != 2 {
		t.Fatalf("holds accounted = %d, want the read's and the write's", got)
	}
	ops := []string{s.conns.holds[before].op, s.conns.holds[before+1].op}
	if !reflect.DeepEqual(ops, []string{"daemon.wsm.sample_read", "daemon.wsm.sample_write"}) {
		t.Fatalf("holds = %v, want the read then the write", ops)
	}
}

// TestOnlyTheConnectionsOwnersReachThePool pins q as the one way a statement
// finds its connection. An operation holds the pool's only connection, so a
// statement inside it that went to the pool (s.db()) would wait forever for
// the connection its own operation holds. The pool is reached only where no
// operation can be running: the open, the migration, the close, and q itself.
func TestOnlyTheConnectionsOwnersReachThePool(t *testing.T) {
	// Arrange
	owners := map[string]bool{"conn.go": true, "open.go": true, "memory.go": true, "migrate.go": true}
	pool := regexp.MustCompile(`s\.db\(\)`)

	// Act
	var offenders []string
	for _, file := range sourcescan.Production(t) {
		if !owners[file.Name] && pool.Match(file.Source) {
			offenders = append(offenders, file.Name)
		}
	}

	// Assert
	if len(offenders) != 0 {
		t.Fatalf("%v reach the pool with s.db(); run statements on s.q(ctx)", offenders)
	}
}
