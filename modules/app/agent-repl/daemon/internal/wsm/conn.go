package wsm

import (
	"context"
	"database/sql"
	"errors"
	"sort"
	"strconv"
	"sync"
	"time"

	"agent-repld/internal/dlog"
)

// THE STATE DATABASE HAS ONE CONNECTION (SetMaxOpenConns(1)), and every read
// and write of the daemon queues for it. On 2026-10-24's handovers the
// successor's adoption burst starved it: registrations waited out Emacs's 10s
// budget behind it, and nothing said which operations held it. Every operation
// now TAKES the connection explicitly, so the store knows how long each one
// waited for it and held it, and says so when either runs past its budget.

// connHoldBudget is how long one operation may hold the connection before its
// release is recorded. A healthy read or write holds it for milliseconds, and
// while one holds it every other operation of the daemon waits.
const connHoldBudget = 250 * time.Millisecond

// connWaitBudget is how long one operation may wait for the connection before
// the wait is recorded with what held the connection meanwhile. A tenth of
// Emacs's 10s unary budget: a request makes several store calls in turn.
const connWaitBudget = time.Second

// connPressureEvery bounds the pressure records to one per interval: a starved
// connection has every waiter past its budget at once, and one record naming
// the holders says what each of theirs would.
const connPressureEvery = time.Second

// connHoldsKept is how many recent holds the pressure record draws on.
const connHoldsKept = 4096

// connHoldersNamed is how many of the heaviest holders a pressure record names.
const connHoldersNamed = 8

// The connection's operations.
const (
	opConnHold     = "daemon.wsm.conn_hold"
	opConnPressure = "daemon.wsm.conn_pressure"
)

// connKey carries the connection an operation holds, so a store call nested
// inside it runs on that connection instead of queueing for a second one the
// pool of one can never give it.
type connKey struct{}

// connQuerier is what an operation runs its statements on: the connection it
// holds, or the pool when it holds none. *sql.DB and *sql.Conn are both one.
type connQuerier interface {
	QueryContext(ctx context.Context, query string, args ...any) (*sql.Rows, error)
	QueryRowContext(ctx context.Context, query string, args ...any) *sql.Row
	ExecContext(ctx context.Context, query string, args ...any) (sql.Result, error)
	BeginTx(ctx context.Context, opts *sql.TxOptions) (*sql.Tx, error)
}

// q answers the connection ctx's operation holds, or the pool outside one.
func (s *store) q(ctx context.Context) connQuerier {
	if conn, ok := ctx.Value(connKey{}).(*sql.Conn); ok {
		return conn
	}
	return s.db()
}

// withConn runs fn holding one connection of db, and accounts for the wait
// and the hold. A call nested in an operation that already holds one runs on
// it, unaccounted: its time is its outer operation's. acquired reports whether
// the connection was taken; when it was not, err is why.
func (s *store) withConn(ctx context.Context, db *sql.DB, op string, fields dlog.Context, fn func(context.Context) error) (acquired bool, err error) {
	if _, held := ctx.Value(connKey{}).(*sql.Conn); held {
		return true, fn(ctx)
	}
	asked := s.now()
	conn, err := db.Conn(ctx)
	if err != nil {
		return false, err
	}
	taken := s.now()
	err = fn(context.WithValue(ctx, connKey{}, conn))
	if closeErr := conn.Close(); closeErr != nil {
		s.log.Error(op, "could not return the state database's connection to its pool", withError(fields, closeErr))
		err = errors.Join(err, closeErr)
	}
	s.conns.note(s.log, op, fields, asked, taken, s.now())
	return true, err
}

// connUse is the accounting of the connection's recent holds.
type connUse struct {
	mu sync.Mutex
	// holds is a ring of the most recent holds; next is where the next one
	// goes.
	holds []connHold
	next  int
	// lastPressure is when the last pressure record was written, and
	// suppressed how many waits past budget it has absorbed since.
	lastPressure time.Time
	suppressed   int
}

// connHold is one operation's hold of the connection.
type connHold struct {
	op       string
	released time.Time
	held     time.Duration
}

// note accounts one operation's use: asked when it asked for the connection,
// taken when it got it, released when it gave it back.
func (u *connUse) note(log dlog.Logger, op string, fields dlog.Context, asked, taken, released time.Time) {
	waited, held := taken.Sub(asked), released.Sub(taken)
	if held >= connHoldBudget {
		log.Info(opConnHold, "an operation held the state database's one connection past its budget; every other operation waited behind it",
			dlog.Merge(fields, dlog.Context{
				"operation": op, "held_ms": held.Milliseconds(), "waited_ms": waited.Milliseconds(),
				"budget_ms": connHoldBudget.Milliseconds(),
			}))
	}
	u.mu.Lock()
	defer u.mu.Unlock()
	if u.holds == nil {
		u.holds = make([]connHold, connHoldsKept)
	}
	if waited >= connWaitBudget {
		if released.Sub(u.lastPressure) < connPressureEvery {
			u.suppressed++
		} else {
			holders, count, total := u.holdersBetween(asked, taken)
			log.Info(opConnPressure, "an operation waited past its budget for the state database's one connection; the operations named held it meanwhile",
				dlog.Merge(fields, dlog.Context{
					"operation": op, "waited_ms": waited.Milliseconds(), "budget_ms": connWaitBudget.Milliseconds(),
					"holds_meanwhile": count, "held_meanwhile_ms": total.Milliseconds(), "holders": holders,
					"waits_past_budget_since_last": u.suppressed,
				}))
			u.lastPressure, u.suppressed = released, 0
		}
	}
	u.holds[u.next] = connHold{op: op, released: released, held: held}
	u.next = (u.next + 1) % len(u.holds)
}

// holdersBetween names the heaviest holders among the holds released in
// [from, to], each as "operation count×total_ms", heaviest first, with how
// many holds there were and how long they held in all. Called with mu held.
func (u *connUse) holdersBetween(from, to time.Time) (holders []string, count int, total time.Duration) {
	type tally struct {
		op    string
		count int
		held  time.Duration
	}
	byOp := map[string]*tally{}
	for _, h := range u.holds {
		if h.op == "" || h.released.Before(from) || h.released.After(to) {
			continue
		}
		t, ok := byOp[h.op]
		if !ok {
			t = &tally{op: h.op}
			byOp[h.op] = t
		}
		t.count++
		t.held += h.held
		count++
		total += h.held
	}
	tallies := make([]*tally, 0, len(byOp))
	for _, t := range byOp {
		tallies = append(tallies, t)
	}
	sort.Slice(tallies, func(i, j int) bool {
		if tallies[i].held != tallies[j].held {
			return tallies[i].held > tallies[j].held
		}
		return tallies[i].op < tallies[j].op
	})
	if len(tallies) > connHoldersNamed {
		tallies = tallies[:connHoldersNamed]
	}
	holders = make([]string, 0, len(tallies))
	for _, t := range tallies {
		holders = append(holders, t.op+" "+strconv.Itoa(t.count)+"×"+strconv.FormatInt(t.held.Milliseconds(), 10)+"ms")
	}
	return holders, count, total
}
