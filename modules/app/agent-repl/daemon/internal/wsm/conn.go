package wsm

import (
	"context"
	"database/sql"
	"errors"
	"runtime"
	"sort"
	"strconv"
	"strings"
	"sync"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ratewindow"
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

// connCallersNamed is how many of the heaviest callers a pressure record
// names.
const connCallersNamed = 8

// callerFramesNamed is how many frames past the store a hold's caller names
// (callerOutsideStore).
const callerFramesNamed = 4

// wsmFunctionPrefix is the qualified-name prefix of this package's functions,
// which a hold's caller is found past (callerOutsideStore).
const wsmFunctionPrefix = "agent-repld/internal/wsm."

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
	caller := callerOutsideStore()
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
	s.conns.note(s.log, op, caller, fields, asked, taken, s.now())
	return true, err
}

// callerOutsideStore names the functions outside this package that made the
// store call being accounted: the first callerFramesNamed frames past every
// wsm frame on the stack, innermost first, joined by "<-".
//
// A HOLD NAMES ITS CALLER, NOT ONLY ITS OPERATION. On the 2026-11-23 15:35
// handover the pressure records named `daemon.wsm.workspace` 2,372 times in
// one second, and that read has dozens of callers across the daemon: which
// one repeated it could not be read from the logs. Four frames, because the
// nearest callers are often shared helpers (the server's registry read is a
// closure inside a generic read inside readRegistry, three frames deep)
// whose own caller is the one that repeats: two named only the helpers on
// the 19:29 handover.
func callerOutsideStore() string {
	return framesPast(func(function string) bool { return strings.HasPrefix(function, wsmFunctionPrefix) })
}

// framesPast names the first callerFramesNamed frames of its caller's stack
// past the leading run of frames INSIDE reports true for, innermost first,
// joined by "<-", each without its "agent-repld/internal/" prefix.
func framesPast(inside func(function string) bool) string {
	var pcs [24]uintptr
	n := runtime.Callers(2, pcs[:])
	frames := runtime.CallersFrames(pcs[:n])
	var named []string
	for len(named) < callerFramesNamed {
		frame, more := frames.Next()
		if frame.Function != "" && (len(named) > 0 || !inside(frame.Function)) {
			named = append(named, strings.TrimPrefix(frame.Function, "agent-repld/internal/"))
		}
		if !more {
			break
		}
	}
	if len(named) == 0 {
		return "unknown"
	}
	return strings.Join(named, "<-")
}

// connUse is the accounting of the connection's recent holds.
type connUse struct {
	mu sync.Mutex
	// holds is a ring of the most recent holds; next is where the next one
	// goes.
	holds []connHold
	next  int
	// pressure admits one pressure record per connPressureEvery, and counts
	// the waits past budget it absorbed since.
	pressure ratewindow.Window
}

// connHold is one operation's hold of the connection.
type connHold struct {
	op string
	// caller is the function outside the store that made the call
	// (callerOutsideStore).
	caller   string
	released time.Time
	held     time.Duration
}

// note accounts one operation's use, made by CALLER: asked when it asked for
// the connection, taken when it got it, released when it gave it back.
func (u *connUse) note(log dlog.Logger, op, caller string, fields dlog.Context, asked, taken, released time.Time) {
	waited, held := taken.Sub(asked), released.Sub(taken)
	if held >= connHoldBudget {
		log.Info(opConnHold, "an operation held the state database's one connection past its budget; every other operation waited behind it",
			dlog.Merge(fields, dlog.Context{
				"operation": op, "caller": caller, "held_ms": held.Milliseconds(), "waited_ms": waited.Milliseconds(),
				"budget_ms": connHoldBudget.Milliseconds(),
			}))
	}
	u.mu.Lock()
	defer u.mu.Unlock()
	if u.holds == nil {
		u.holds = make([]connHold, connHoldsKept)
	}
	if waited >= connWaitBudget {
		u.pressure.Every = connPressureEvery
		if admitted, suppressed := u.pressure.Admit(released); admitted {
			holders, count, total := u.holdersBetween(asked, taken)
			log.Info(opConnPressure, "an operation waited past its budget for the state database's one connection; the operations named held it meanwhile",
				dlog.Merge(fields, dlog.Context{
					"operation": op, "caller": caller, "waited_ms": waited.Milliseconds(), "budget_ms": connWaitBudget.Milliseconds(),
					"holds_meanwhile": count, "held_meanwhile_ms": total.Milliseconds(), "holders": holders,
					"callers":                      u.callersBetween(asked, taken),
					"waits_past_budget_since_last": suppressed,
				}))
		}
	}
	u.holds[u.next] = connHold{op: op, caller: caller, released: released, held: held}
	u.next = (u.next + 1) % len(u.holds)
}

// holdersBetween names the heaviest holders among the holds released in
// [from, to], each as "operation count×total_ms", heaviest first, with how
// many holds there were and how long they held in all. Called with mu held.
func (u *connUse) holdersBetween(from, to time.Time) (holders []string, count int, total time.Duration) {
	return u.heaviestBetween(from, to, connHoldersNamed, func(h connHold) string { return h.op })
}

// callersBetween names the heaviest callers among the holds released in
// [from, to], each as "operation<-caller count×total_ms", heaviest first.
// Called with mu held.
func (u *connUse) callersBetween(from, to time.Time) []string {
	callers, _, _ := u.heaviestBetween(from, to, connCallersNamed, func(h connHold) string { return h.op + "<-" + h.caller })
	return callers
}

// heaviestBetween tallies the holds released in [from, to] by KEY and names
// the LIMIT heaviest, each as "key count×total_ms", heaviest first, with how
// many holds there were and how long they held in all. Called with mu held.
func (u *connUse) heaviestBetween(from, to time.Time, limit int, key func(connHold) string) (named []string, count int, total time.Duration) {
	type tally struct {
		key   string
		count int
		held  time.Duration
	}
	byKey := map[string]*tally{}
	for _, h := range u.holds {
		if h.op == "" || h.released.Before(from) || h.released.After(to) {
			continue
		}
		k := key(h)
		t, ok := byKey[k]
		if !ok {
			t = &tally{key: k}
			byKey[k] = t
		}
		t.count++
		t.held += h.held
		count++
		total += h.held
	}
	tallies := make([]*tally, 0, len(byKey))
	for _, t := range byKey {
		tallies = append(tallies, t)
	}
	sort.Slice(tallies, func(i, j int) bool {
		if tallies[i].held != tallies[j].held {
			return tallies[i].held > tallies[j].held
		}
		return tallies[i].key < tallies[j].key
	})
	if len(tallies) > limit {
		tallies = tallies[:limit]
	}
	named = make([]string, 0, len(tallies))
	for _, t := range tallies {
		named = append(named, t.key+" "+strconv.Itoa(t.count)+"×"+strconv.FormatInt(t.held.Milliseconds(), 10)+"ms")
	}
	return named, count, total
}
