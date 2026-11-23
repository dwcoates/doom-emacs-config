package feed

import (
	"runtime"
	"strings"
	"sync"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/lockwatch"
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
}

// Lock locks m and notes when, and by whom.
func (m *heldMutex) Lock() {
	m.Mutex.Lock()
	m.taken = m.report.now()
	runtime.Callers(2, m.by[:])
}

// Unlock unlocks m and reports the hold when it ran past its budget.
func (m *heldMutex) Unlock() {
	held := m.report.now().Sub(m.taken)
	by := m.by[0]
	m.Mutex.Unlock()
	if held >= lockHoldBudget {
		m.report.held(held, by)
	}
}

// holdReport records the resolver mutex's long holds, at most one record per
// lockHoldEvery.
type holdReport struct {
	log dlog.Logger
	now func() time.Time

	mu sync.Mutex
	// last is when the last record was written; suppressed counts the long
	// holds since then that wrote none, and longest is the longest of them.
	last       time.Time
	suppressed int
	longest    time.Duration
}

// held accounts one hold of HELD taken at the program counter BY.
func (h *holdReport) held(held time.Duration, by uintptr) {
	at := h.now()
	h.mu.Lock()
	if !h.last.IsZero() && at.Sub(h.last) < lockHoldEvery {
		h.suppressed++
		h.longest = max(h.longest, held)
		h.mu.Unlock()
		return
	}
	suppressed, longest := h.suppressed, h.longest
	h.last, h.suppressed, h.longest = at, 0, 0
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
