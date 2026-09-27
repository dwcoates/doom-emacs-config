package lockwatch

import (
	"context"
	"errors"
	"fmt"
	"sync"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/goroutinedump"
	"claude-repld/internal/ids"
)

// DefaultThreshold is how long one hold of a watched lock may last before the
// watchdog records it as a stall.
//
// SIZED FROM EVIDENCE, BOTH WAYS:
//
//   - The LONGEST LEGITIMATE HOLD on any watched lock is the prompt queue's
//     delivery lock (wsState.drain) held across an IN-LINE REVIVAL: a
//     hibernated workspace's hold released by the drain sweep is delivered by
//     deliverHeld, which brings the session up under the lock. Every real
//     bring-up in the daemon logs of 2026-09-27 (probe to "the session is up",
//     vendor resume included) took 1.2s to 4.6s. A synchronous delivery
//     (Submit to "forwarded to the queue", StartTurn included) measured 9ms
//     p50, 143ms max over 30 submissions. The session watcher's lock performs
//     no shim I/O at all (daemon/AGENTS.md), and the feed resolver's holds are
//     microseconds per row.
//   - The USER-VISIBLE FAILURE is Emacs's unary timeout,
//     agent-repl-connect-unary-timeout-seconds (10s). Submit takes both the
//     queue's mutex and the workspace's delivery lock and reads the watcher's
//     turn under its lock, so a hold past 10s on any of them IS a
//     SubmitPrompt timeout.
//
// 6s is ~1.3x the slowest legitimate hold. Because a hold is first seen at the
// tick after it begins, a stall is recorded at most threshold + one tick (7s)
// after the lock was taken: three seconds before the editor gives up, so the
// record precedes the failure the user sees.
const DefaultThreshold = 6 * time.Second

// DefaultEvery is the watchdog's tick. It is COARSE on purpose: a tick costs
// one atomic load per watched lock, and the only thing a finer tick buys is a
// tighter bound on WHEN a multi-second stall is noticed.
const DefaultEvery = time.Second

// The watchdog's operations.
const (
	opRun     = "daemon.lockwatch.run"
	opWatch   = "daemon.lockwatch.watch"
	opStall   = "daemon.lockwatch.stall"
	opRelease = "daemon.lockwatch.release"
)

// errTicksClosed is a tick source that ended while the daemon still runs.
var errTicksClosed = errors.New("lockwatch: the tick source closed while the daemon was running")

// Registry is what a lock's owner registers its Mutex with. The returned
// function stops watching it; call it when the lock's owner is retired.
//
// lock names the lock (package.owner.field), ws the workspace it belongs to
// (empty for a daemon-wide lock), and log is where its records go: the
// workspace's logger for a workspace lock, the run log for a daemon-wide one.
type Registry interface {
	Watch(m *Mutex, lock string, ws ids.WorkspaceID, log dlog.Logger) (unwatch func())
}

// Deps are the watchdog's collaborators.
type Deps struct {
	// Log is the run log, for the watchdog's own lifecycle. REQUIRED.
	Log dlog.Logger
	// Threshold is DefaultThreshold when zero.
	Threshold time.Duration
	// Every is the tick, DefaultEvery when zero.
	Every time.Duration
	// Ticks replaces the ticker: each value is one check AT that instant. Nil
	// runs a time.Ticker at Every. Tests drive the watchdog through it and
	// never sleep.
	Ticks <-chan time.Time
	// Dump renders every goroutine's stack; nil is goroutinedump.Render.
	Dump func() (string, int)
}

// Watchdog is the daemon's one lock stall detector.
type Watchdog struct {
	log       dlog.Logger
	threshold time.Duration
	every     time.Duration
	ticks     <-chan time.Time
	dump      func() (string, int)

	// mu guards entries. It is taken by a registration and ONCE per tick for
	// a walk that reads one atomic word per entry, so a workspace opening
	// waits at most one such walk.
	mu      sync.Mutex
	entries []*entry

	// dumps counts the dumps taken, so every stall record of one tick names
	// the one dump they share. Only the Run goroutine touches it.
	dumps uint64
	// found is the per-tick scratch the walk fills and the reporting drains;
	// only the Run goroutine touches it, and it is reused so a quiet tick
	// allocates nothing.
	found []finding
}

// entry is one watched lock and the watchdog's memory of its current hold.
type entry struct {
	m    *Mutex
	lock string
	ws   ids.WorkspaceID
	log  dlog.Logger

	// hold is the hold the last tick saw, since the tick it was first seen
	// at, and reported whether it has been recorded as a stall. Guarded by
	// Watchdog.mu.
	hold     uint64
	since    time.Time
	reported bool
	// gone is set by the unwatch: the next tick settles the entry and drops
	// it.
	gone bool
}

// finding is one record a tick owes, captured under mu and written after it.
type finding struct {
	stall   bool
	lock    string
	ws      ids.WorkspaceID
	log     dlog.Logger
	heldFor time.Duration
	// unwatched marks a release noticed because the owner stopped watching.
	unwatched bool
}

// New builds a watchdog.
func New(deps Deps) (*Watchdog, error) {
	if deps.Log == nil {
		return nil, fmt.Errorf("lockwatch: the watchdog needs the run log")
	}
	if deps.Threshold < 0 || deps.Every < 0 {
		return nil, fmt.Errorf("lockwatch: threshold %v and tick %v must not be negative", deps.Threshold, deps.Every)
	}
	w := &Watchdog{
		log:       deps.Log,
		threshold: deps.Threshold,
		every:     deps.Every,
		ticks:     deps.Ticks,
		dump:      deps.Dump,
	}
	if w.threshold == 0 {
		w.threshold = DefaultThreshold
	}
	if w.every == 0 {
		w.every = DefaultEvery
	}
	if w.dump == nil {
		w.dump = goroutinedump.Render
	}
	return w, nil
}

// Watch registers m. See Registry.
func (w *Watchdog) Watch(m *Mutex, lock string, ws ids.WorkspaceID, log dlog.Logger) func() {
	e := &entry{m: m, lock: lock, ws: ws, log: log}
	w.mu.Lock()
	w.entries = append(w.entries, e)
	watched := len(w.entries)
	w.mu.Unlock()
	log.Debug(opWatch, "watching a lock for stalls", dlog.Context{
		"lock": lock, "workspace_id": string(ws), "watched": watched,
	})
	return func() {
		w.mu.Lock()
		e.gone = true
		w.mu.Unlock()
	}
}

// Run checks every watched lock once per tick until ctx ends.
func (w *Watchdog) Run(ctx context.Context) error {
	ticks := w.ticks
	if ticks == nil {
		ticker := time.NewTicker(w.every)
		defer ticker.Stop()
		ticks = ticker.C
	}
	w.log.Info(opRun, "watching the daemon's hot locks for stalls", dlog.Context{
		"threshold": w.threshold.String(), "every": w.every.String(),
	})
	for {
		select {
		case <-ctx.Done():
			w.log.Debug(opRun, "the lock watchdog stopped with the serving lifetime", nil)
			return nil
		case now, ok := <-ticks:
			if !ok {
				w.log.Error(opRun, "the lock watchdog's tick source closed; stalls are no longer detected", nil)
				return errTicksClosed
			}
			w.check(now)
		}
	}
}

// check is one tick: walk every entry under mu, then write what it found.
func (w *Watchdog) check(now time.Time) {
	w.mu.Lock()
	w.found = w.found[:0]
	kept := w.entries[:0]
	for _, e := range w.entries {
		if e.gone {
			// A RETIRED OWNER'S STALL STILL GETS ITS ENDING: the owner took
			// the lock to retire itself, so the stall is over either way.
			if e.reported {
				w.found = append(w.found, finding{
					lock: e.lock, ws: e.ws, log: e.log, heldFor: now.Sub(e.since), unwatched: true,
				})
			}
			continue
		}
		w.observe(e, now)
		kept = append(kept, e)
	}
	// Clear the tail so a dropped entry's lock and logger are collectable.
	for i := len(kept); i < len(w.entries); i++ {
		w.entries[i] = nil
	}
	w.entries = kept
	w.mu.Unlock()

	if len(w.found) > 0 {
		w.report(now)
	}
}

// observe advances one entry's episode. A hold is the SAME hold while its
// value is unchanged and odd; anything else ends the episode it was in.
func (w *Watchdog) observe(e *entry, now time.Time) {
	hold, held := e.m.hold()
	if held && hold == e.hold {
		if !e.reported && now.Sub(e.since) >= w.threshold {
			e.reported = true
			w.found = append(w.found, finding{
				stall: true, lock: e.lock, ws: e.ws, log: e.log, heldFor: now.Sub(e.since),
			})
		}
		return
	}
	if e.reported {
		w.found = append(w.found, finding{lock: e.lock, ws: e.ws, log: e.log, heldFor: now.Sub(e.since)})
	}
	e.reported = false
	e.hold = hold
	e.since = now
}

// report writes one tick's findings: every stall at ERROR, the FIRST of them
// carrying the tick's one goroutine dump, and every release at INFO.
func (w *Watchdog) report(now time.Time) {
	var (
		dumpID  uint64
		carrier *finding
	)
	for i := range w.found {
		f := &w.found[i]
		if !f.stall {
			w.recordRelease(f)
			continue
		}
		ctx := dlog.Context{
			"lock":              f.lock,
			"held_for":          f.heldFor.String(),
			"held_for_ms":       f.heldFor.Milliseconds(),
			"threshold":         w.threshold.String(),
			"resolution":        w.every.String(),
			"stalled_this_tick": w.stallsFound(),
		}
		if f.ws != "" {
			ctx["workspace_id"] = string(f.ws)
		}
		// ONE DUMP PER TICK, however many locks stalled in it: N wedged
		// workspaces are one process's goroutines, and N megabyte dumps of
		// the same stacks in one burst say nothing the first does not.
		if carrier == nil {
			w.dumps++
			dumpID = w.dumps
			carrier = f
			dump, count := w.dump()
			ctx["goroutines"] = count
			ctx["goroutine_dump"] = dump
		} else {
			ctx["goroutine_dump_carried_by"] = carrier.lock + " " + string(carrier.ws)
		}
		ctx["dump_id"] = dumpID
		f.log.Error(opStall, "a watched lock has been held past the stall threshold; the daemon is wedged behind it", ctx)
	}
}

// recordRelease writes the end of a reported stall.
func (w *Watchdog) recordRelease(f *finding) {
	ctx := dlog.Context{
		"lock":        f.lock,
		"held_for":    f.heldFor.String(),
		"held_for_ms": f.heldFor.Milliseconds(),
		"resolution":  w.every.String(),
	}
	if f.ws != "" {
		ctx["workspace_id"] = string(f.ws)
	}
	message := "a stalled lock was released"
	if f.unwatched {
		message = "a stalled lock's owner was retired; the lock is no longer watched"
	}
	f.log.Info(opRelease, message, ctx)
}

// stallsFound counts the stalls in this tick's findings.
func (w *Watchdog) stallsFound() int {
	n := 0
	for i := range w.found {
		if w.found[i].stall {
			n++
		}
	}
	return n
}
