// Package stallbeat raises an error when the web content process's
// main-thread stalls fall on one phase of a 30-second cycle.
//
// WHY IT EXISTS (2026-10-18). WebKit's MemoryPressureHandler polls the web
// content process's footprint every 30 seconds and, while it stands above
// WebKit's threshold, runs a memory-pressure relief, with a full garbage
// collection, on the main thread. Every workspace page shares that thread, so
// each relief is a stall the user feels, and a stall on every beat is how
// leaked documents showed: on 2026-10-15, 52 of an hour's `webhost.stall`
// records fell on the same second of the cycle. A stall pattern locked to the
// beat is therefore the sign that the process is under sustained memory
// pressure, whatever the cause, and it is raised so a regression surfaces in
// the logs instead of only in a slow switch.
//
// THE THRESHOLD IS MEASURED. Stalls with no beat behind them put at most 4 of a
// 10-minute window's stalls on any one phase (2026-10-18). Sustained relief
// put 14 to 20 there, nearly every one of the window's 20 beats. Beat sets the
// line at 8, twice the chance clustering.
package stallbeat

import (
	"errors"
	"fmt"
	"sync"
	"time"

	"agent-repld/internal/dlog"
)

const op = "daemon.webprocess.stall_beat"

// Period is the cycle WebKit's memory-pressure poll runs on.
const Period = 30 * time.Second

// Window is how far back stalls are counted, and how often the error repeats
// at most while the pattern holds.
const Window = 10 * time.Minute

// Tolerance is how far from the beat's phase a stall still counts as on it.
const Tolerance = 500 * time.Millisecond

// Beat is how many stalls of one window on one phase raise the error.
const Beat = 8

// Deps are the detector's collaborators, every one required.
type Deps struct {
	Log dlog.Logger
}

// stall is one recorded main-thread stall.
type stall struct {
	at      time.Time
	blocked int64
}

// Detector watches the stalls the webhost reports.
type Detector struct {
	log dlog.Logger

	mu     sync.Mutex
	stalls []stall
	raised time.Time
}

// New builds a detector.
func New(deps Deps) (*Detector, error) {
	if deps.Log == nil {
		return nil, errors.New("stallbeat: a logger is required")
	}
	return &Detector{log: deps.Log}, nil
}

// Observe takes one stall: the moment the webhost recorded it and how long the
// main thread was blocked. It raises the error once per Window while Beat or
// more of the window's stalls fall within Tolerance of one phase of Period.
func (d *Detector) Observe(at time.Time, blockedMS int64) {
	d.mu.Lock()
	defer d.mu.Unlock()
	kept := d.stalls[:0]
	for _, s := range d.stalls {
		if at.Sub(s.at) < Window {
			kept = append(kept, s)
		}
	}
	d.stalls = append(kept, stall{at: at, blocked: blockedMS})
	if !d.raised.IsZero() && at.Sub(d.raised) < Window {
		return
	}
	phase, onBeat, blocked := bestBeat(d.stalls)
	if onBeat < Beat {
		return
	}
	d.raised = at
	d.log.Error(op, fmt.Sprintf(
		"%d of the last %s's %d web-process stalls fell on one phase of WebKit's %s memory-pressure cycle: the process is under sustained memory pressure",
		onBeat, Window, len(d.stalls), Period), dlog.Context{
		"stalls_on_beat":     onBeat,
		"stalls_in_window":   len(d.stalls),
		"blocked_ms_on_beat": blocked,
		"phase_ms":           phase.Milliseconds(),
		"window_s":           int(Window.Seconds()),
		"period_s":           int(Period.Seconds()),
	})
}

// bestBeat answers the phase of Period that the most STALLS fall within
// Tolerance of, how many do, and how long they blocked in all.
func bestBeat(stalls []stall) (time.Duration, int, int64) {
	var bestPhase time.Duration
	bestCount, bestBlocked := 0, int64(0)
	for _, candidate := range stalls {
		center := phaseOf(candidate.at)
		count, blocked := 0, int64(0)
		for _, s := range stalls {
			if distance(phaseOf(s.at), center) <= Tolerance {
				count++
				blocked += s.blocked
			}
		}
		if count > bestCount {
			bestPhase, bestCount, bestBlocked = center, count, blocked
		}
	}
	return bestPhase, bestCount, bestBlocked
}

// phaseOf answers where in Period the moment AT falls.
func phaseOf(at time.Time) time.Duration {
	return time.Duration(at.UnixNano() % int64(Period))
}

// distance answers how far apart two phases are around the cycle.
func distance(a, b time.Duration) time.Duration {
	d := a - b
	if d < 0 {
		d = -d
	}
	if Period-d < d {
		return Period - d
	}
	return d
}
