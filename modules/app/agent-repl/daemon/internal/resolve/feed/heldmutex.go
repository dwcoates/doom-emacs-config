package feed

import (
	"runtime"
	"strings"
	"sync"
	"sync/atomic"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/lockwatch"
	"agent-repld/internal/ratewindow"
)

// lockHoldBudget is how long one hold of the resolver's mutex may last before
// its release is recorded. Every workspace's feed waits while one holds it,
// and a healthy hold composes a page or draws a row in milliseconds; it is
// the state database's hold budget (wsm connHoldBudget), the other one lock
// every workspace queues for.
const lockHoldBudget = 250 * time.Millisecond

// lockHoldEvery bounds the hold records to one per interval: a mutex held
// long once is usually held long by many in a row, and one record with the
// count it absorbed stands for them.
const lockHoldEvery = time.Second

// opLockHeld is the operation a long hold of the resolver's mutex is recorded
// under.
const opLockHeld = "daemon.feed.lock_held"

// heldMutex is the resolver's one mutex: a lockwatch.Mutex, so the stall
// watchdog sees a wedge, whose holds past lockHoldBudget are recorded with
// the function that took it.
//
// THE WATCHDOG SEES ONLY A WEDGE. It reports a hold past 6s
// (lockwatch.DefaultThreshold); on the 2026-11-23 15:35 handover every page's
// feed open waited seconds with none recorded, and whether the mutex every
// workspace's feed shares was what held them could not be read from the logs.
type heldMutex struct {
	lockwatch.Mutex
	// taken and by are the current hold's start and the caller of Lock,
	// written only by the goroutine holding the mutex.
	taken time.Time
	by    [1]uintptr
	// report is told of every hold past lockHoldBudget, after the release.
	report *holdReport
	// held is set for as long as the mutex is held, so the store's guard can
	// tell a call made under it (Held).
	held atomic.Bool
	// pending is the store work the current hold queued (afterUnlock),
	// touched only by the goroutine holding the mutex.
	pending []storeStep
	// owner is the resolver the queued work runs for, and queue the one
	// queue it runs on (storework.go).
	owner *resolver
	queue storeQueue
}

// Lock locks m and notes when, and by whom.
func (m *heldMutex) Lock() {
	m.Mutex.Lock()
	m.held.Store(true)
	m.taken = m.report.now()
	runtime.Callers(2, m.by[:])
}

// Unlock unlocks m, reports the hold when it ran past its budget, and runs
// the store work the hold queued, in release order, before it returns
// (storework.go).
func (m *heldMutex) Unlock() {
	batch := m.release()
	if batch != nil {
		m.queue.submit(m.owner, batch)
	}
}

// unlockInDrain is Unlock for a hold a queued step's settle took: the work
// it queued joins the drain already running, which runs it next, instead of
// being waited on by the drainer itself.
func (m *heldMutex) unlockInDrain() {
	if batch := m.release(); batch != nil {
		m.queue.enqueue(batch)
	}
}

// release ends the hold and answers the store work it queued as a batch, nil
// when it queued none.
func (m *heldMutex) release() *storeBatch {
	held := m.report.now().Sub(m.taken)
	by := m.by[0]
	var batch *storeBatch
	if len(m.pending) > 0 {
		batch = &storeBatch{steps: m.pending, done: make(chan struct{})}
		m.pending = nil
	}
	m.held.Store(false)
	m.Mutex.Unlock()
	if held >= lockHoldBudget {
		m.report.held(held, by)
	}
	return batch
}

// Held reports whether the mutex is held right now, by anyone. THE RESOLVER
// NEVER HOLDS IT ACROSS A STATE-DATABASE CALL (storewrites.go): the feed
// test harness's database fakes fail a test that calls them while it is held.
func (m *heldMutex) Held() bool { return m.held.Load() }

// holdReport records the resolver mutex's long holds, at most one record per
// lockHoldEvery.
type holdReport struct {
	log dlog.Logger
	now func() time.Time

	mu sync.Mutex
	// window admits one record per lockHoldEvery; longest is the longest of
	// the long holds it turned away since the last.
	window  ratewindow.Window
	longest time.Duration
}

// held accounts one hold of HELD taken at the program counter BY.
func (h *holdReport) held(held time.Duration, by uintptr) {
	at := h.now()
	h.mu.Lock()
	h.window.Every = lockHoldEvery
	admitted, suppressed := h.window.Admit(at)
	if !admitted {
		h.longest = max(h.longest, held)
		h.mu.Unlock()
		return
	}
	longest := h.longest
	h.longest = 0
	h.mu.Unlock()
	h.log.Info(opLockHeld, "the resolver's one mutex was held past its budget; every workspace's feed waited behind the hold", dlog.Context{
		"held_ms": held.Milliseconds(), "budget_ms": lockHoldBudget.Milliseconds(), "by": functionAt(by),
		"long_holds_since_last": suppressed, "longest_since_last_ms": longest.Milliseconds(),
	})
}

// functionAt names the function a caller program counter is in, without its
// "agent-repld/internal/" prefix.
func functionAt(pc uintptr) string {
	frames := runtime.CallersFrames([]uintptr{pc})
	frame, _ := frames.Next()
	if frame.Function == "" {
		return "unknown"
	}
	return strings.TrimPrefix(frame.Function, "agent-repld/internal/")
}
