// Package inflight counts the daemon's work in flight, so a caller can wait
// for every consequence of what it asked to have finished -- on the count
// reaching zero, never on a timer.
//
// WHAT IS COUNTED. Every piece of work the daemon starts as a CONSEQUENCE of
// something -- a request, a push, a shim frame, another piece of work -- is
// begun on the tracker at the instant it is started, by whoever starts it, and
// ended when it finishes:
//   - a goroutine: Go, never a bare `go` statement;
//   - an item handed to a long-running worker: Queue, whose item counts from
//     Put until the worker's done;
//   - anything else: Begin, whose end is called once the work is over.
//
// TWO RULES MAKE ZERO MEAN "NOTHING IS COMING". Work is begun inside the
// request that causes it, before that request answers, so a caller that
// waits after its answer finds it counted. And work that starts more work
// begins the new work before it ends itself, so the count never touches zero
// partway through a chain.
//
// WHAT IS NOT COUNTED. A long-running loop (a standing watch, a sweep) never
// finishes, so it is started with Standing, which says so. Its iterations'
// consequences are counted like any other.
package inflight

import (
	"context"
	"fmt"
	"path/filepath"
	"runtime"
	"sort"
	"strconv"
	"sync"
)

// Tracker is one count of work in flight. The zero Tracker is ready.
type Tracker struct {
	mu sync.Mutex
	n  int
	// sites counts the work in flight by where it was begun (file:line), so
	// a wait that ends while work is still in flight can say which.
	sites map[string]int
	// idle closes when n reaches zero, and is replaced when work begins
	// again.
	idle chan struct{}
	// parent, when set, counts every piece of work this tracker counts, so a
	// narrower wait (a child's) and the daemon-wide one (the parent's) see
	// the same work.
	parent *Tracker
}

// NewChild answers a tracker whose every piece of work is also counted on
// parent.
func NewChild(parent *Tracker) *Tracker {
	if parent == nil {
		panic("inflight: a child tracker needs a parent")
	}
	return &Tracker{parent: parent}
}

// Default is the daemon's one tracker.
var Default = &Tracker{}

// Begin counts one piece of work, answering its end. The end is idempotent:
// a second call does nothing.
func (t *Tracker) Begin() (end func()) {
	return t.begin(2)
}

// begin counts one piece of work begun by the caller SKIP frames up.
func (t *Tracker) begin(skip int) (end func()) {
	site := "unknown"
	if _, file, line, ok := runtime.Caller(skip); ok {
		site = filepath.Base(filepath.Dir(file)) + "/" + filepath.Base(file) + ":" + strconv.Itoa(line)
	}
	t.mu.Lock()
	t.n++
	if t.sites == nil {
		t.sites = map[string]int{}
	}
	t.sites[site]++
	t.mu.Unlock()
	var parentEnd func()
	if t.parent != nil {
		parentEnd = t.parent.begin(skip + 1)
	}
	var once sync.Once
	return func() {
		once.Do(func() {
			t.end(site)
			if parentEnd != nil {
				parentEnd()
			}
		})
	}
}

// end uncounts one piece of work, waking every AwaitIdle once none is left.
func (t *Tracker) end(site string) {
	t.mu.Lock()
	defer t.mu.Unlock()
	t.sites[site]--
	if t.sites[site] == 0 {
		delete(t.sites, site)
	}
	t.n--
	if t.n < 0 {
		panic(fmt.Sprintf("inflight: work ended %d more time(s) than it began", -t.n))
	}
	if t.n == 0 && t.idle != nil {
		close(t.idle)
		t.idle = nil
	}
}

// Go runs f on a goroutine, counted from this call until f returns.
func (t *Tracker) Go(f func()) {
	end := t.begin(2)
	go func() {
		defer end()
		f()
	}()
}

// AwaitIdle waits until no work is in flight, or ctx ends. It blocks on the
// count's own zero, never on a timer.
func (t *Tracker) AwaitIdle(ctx context.Context) error {
	t.mu.Lock()
	if t.n == 0 {
		t.mu.Unlock()
		return nil
	}
	if t.idle == nil {
		t.idle = make(chan struct{})
	}
	idle, n := t.idle, t.n
	t.mu.Unlock()
	select {
	case <-idle:
		return nil
	case <-ctx.Done():
		return fmt.Errorf("inflight: %d piece(s) of work still in flight: %w", n, ctx.Err())
	}
}

// InFlight reports how much work is counted now, for a report.
func (t *Tracker) InFlight() int {
	t.mu.Lock()
	defer t.mu.Unlock()
	return t.n
}

// Go runs f counted on the daemon's tracker (Tracker.Go).
func Go(f func()) {
	end := Default.begin(2)
	go func() {
		defer end()
		f()
	}()
}

// Begin counts one piece of work on the daemon's tracker (Tracker.Begin).
func Begin() (end func()) { return Default.begin(2) }

// InFlightSites answers where the work in flight was begun, "file:line xN",
// sorted, for a report.
func (t *Tracker) InFlightSites() []string {
	t.mu.Lock()
	defer t.mu.Unlock()
	out := make([]string, 0, len(t.sites))
	for site, n := range t.sites {
		out = append(out, site+" x"+strconv.Itoa(n))
	}
	sort.Strings(out)
	return out
}

// Standing runs f, a long-running loop, on a goroutine WITHOUT counting it:
// it never finishes, so counting it would hold the count above zero forever.
// What each of its iterations starts is counted as usual.
func Standing(f func()) { go f() }
