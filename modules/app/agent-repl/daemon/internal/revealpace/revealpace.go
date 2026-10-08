// Package revealpace keeps the daemon's record of how often streamed text
// arrives, per model and per block kind, and turns it into the reveal window the
// webapp paces a response's type-out by (frontend.v1.FeedResponseRevealWindow,
// docs/protobuf-design/streaming-reveal-window.md).
//
// THE DAEMON MEASURES, THE CLIENT ONLY SPREADS. Each key holds a rolling window
// of the WindowSize most recent gaps between consecutive BURSTS of one block
// (see Burst). Once the window is full its average is the expected time from a
// burst's start to the next burst's, and each draw states what is left of it
// (Burst.Window), so the client spreads whatever it has not yet shown across
// the time until the next burst. Until the window is full there is no figure,
// and the client paces with its own default.
//
// WHY BURSTS, NOT FRAGMENTS. The vendor's text deltas reach the daemon in
// clumps: on 2026-10-08 (workspace 096682deb5314ad3, 15:31:25-28) the shim
// received ~11 deltas within 1ms, then nothing for ~450ms, on a steady ~530ms
// period. Measured per fragment, ten 2ms gaps and one 500ms gap averaged to
// ~50ms, so a whole clump was revealed in a frame or two and the reader then
// watched nothing for the rest of the period: no pacing at all to the eye.
//
// The windows live in memory, loaded once at boot, and a key's window is
// written back when the caller says a block of it settled (Persist), so the
// record survives a restart without a write per fragment.
package revealpace

import (
	"context"
	"fmt"
	"math"
	"sync"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/wsm"
)

// WindowSize is how many of the most recent gaps a window holds, and how many
// it must hold before it states an expected gap.
const WindowSize = 25

// Decay is the weight each step of age multiplies a gap's weight by: the
// newest gap weighs 1, the one before it Decay, the one before that Decay², so
// the latest cadence dominates the average without one outlier owning it.
const Decay = 0.85

// BurstThreshold is the longest pause between two fragments of one burst.
//
// Inside a clump the daemon receives fragments as fast as it can fold them:
// 1-4ms apart on the 2026-10-08 logs, the first after the store's write batch
// up to ~8ms, the slowest observed 14-19ms. The vendor's own cadence between
// fragments that are NOT clumped was 40-60ms on the same logs. 20ms sits between
// the two, and is just over one 60fps frame (16.7ms): text arriving within a
// frame of the previous text could not be revealed apart from it anyway.
const BurstThreshold = 20 * time.Millisecond

// Store is the durable record the windows are loaded from and written to
// (wsm.DB).
type Store interface {
	PutRevealGapWindow(ctx context.Context, w wsm.RevealGapWindow) error
	RevealGapWindows(ctx context.Context) ([]wsm.RevealGapWindow, error)
}

// Key names one window: the model the fragments streamed under and the kind of
// block they streamed in.
type Key struct {
	Model string
	Kind  wsm.RevealKind
}

// Pacer holds every window. It is safe for concurrent use.
type Pacer struct {
	store Store
	log   dlog.Logger

	mu      sync.Mutex
	windows map[Key][]int64
}

// Load builds a Pacer from every window the store holds. A store that cannot
// be read fails the load: a pacer that silently started empty would hide a
// broken record behind a fresh one.
func Load(ctx context.Context, store Store, log dlog.Logger) (*Pacer, error) {
	stored, err := store.RevealGapWindows(ctx)
	if err != nil {
		log.Error("daemon.revealpace.load", "the stored reveal gap windows could not be read",
			dlog.Context{"error": err.Error()})
		return nil, fmt.Errorf("revealpace: load the stored windows: %w", err)
	}
	p := &Pacer{store: store, log: log, windows: make(map[Key][]int64, len(stored))}
	for _, w := range stored {
		p.windows[Key{Model: w.Model, Kind: w.Kind}] = newest(w.GapsMs)
	}
	log.Info("daemon.revealpace.loaded", "loaded the stored reveal gap windows",
		dlog.Context{"windows": len(stored)})
	return p, nil
}

// Observe adds one measured gap between two bursts' starts to key's window, dropping the oldest once the
// window is full.
func (p *Pacer) Observe(key Key, gap time.Duration) {
	ms := gap.Milliseconds()
	p.mu.Lock()
	p.windows[key] = newest(append(p.windows[key], ms))
	held := len(p.windows[key])
	p.mu.Unlock()
	p.log.Debug("daemon.revealpace.observed", "recorded a streamed-burst gap", dlog.Context{
		"model": key.Model, "kind": string(key.Kind), "gap_ms": ms, "held": held,
	})
}

// ExpectedGap answers the expected time from one burst's start to the next's
// (weightedAverage of key's window), in whole milliseconds and never below 1,
// once the window is full. False while it is not: too few gaps have been seen
// to predict the next one.
func (p *Pacer) ExpectedGap(key Key) (uint32, bool) {
	p.mu.Lock()
	gaps := p.windows[key]
	p.mu.Unlock()
	if len(gaps) < WindowSize {
		return 0, false
	}
	return weightedAverage(gaps), true
}

// Persist writes key's window to the store. The store's failure is recorded
// here, once, at ERROR, and answered to the caller.
func (p *Pacer) Persist(ctx context.Context, key Key) error {
	p.mu.Lock()
	gaps := append([]int64(nil), p.windows[key]...)
	p.mu.Unlock()
	fields := dlog.Context{"model": key.Model, "kind": string(key.Kind), "gaps": len(gaps)}
	if err := p.store.PutRevealGapWindow(ctx, wsm.RevealGapWindow{Model: key.Model, Kind: key.Kind, GapsMs: gaps}); err != nil {
		fields["error"] = err.Error()
		p.log.Error("daemon.revealpace.persist", "a reveal gap window could not be written", fields)
		return fmt.Errorf("revealpace: persist %s/%s: %w", key.Model, key.Kind, err)
	}
	p.log.Debug("daemon.revealpace.persisted", "wrote a reveal gap window", fields)
	return nil
}

// newest answers the last WindowSize gaps of gaps, oldest first.
func newest(gaps []int64) []int64 {
	if len(gaps) <= WindowSize {
		return gaps
	}
	return append([]int64(nil), gaps[len(gaps)-WindowSize:]...)
}

// weightedAverage answers the Decay-weighted average of gaps (oldest first),
// rounded to whole milliseconds and never below 1: a reveal window of zero
// would mean "reveal in no time", which is a snap, not a pace.
func weightedAverage(gaps []int64) uint32 {
	var sum, weights float64
	weight := 1.0
	for i := len(gaps) - 1; i >= 0; i-- {
		sum += weight * float64(gaps[i])
		weights += weight
		weight *= Decay
	}
	return uint32(max(1, math.Round(sum/weights)))
}

// Burst is one block's record of the burst its text is arriving in: when the
// burst began and when its latest fragment arrived. The zero value is a block
// no fragment has reached yet.
type Burst struct {
	start, last time.Time
}

// Arrive records a text-bearing fragment arriving at now. A fragment more than
// BurstThreshold after the previous one starts a new burst, and the time since
// the previous burst STARTED is a gap sample (the answer, with true). The
// first fragment of a block only starts the clock: its wait is the time to
// first token, not the streaming cadence. Any other fragment joins the burst
// and samples nothing.
func (b *Burst) Arrive(now time.Time) (time.Duration, bool) {
	switch {
	case b.start.IsZero():
		b.start, b.last = now, now
		return 0, false
	case now.Sub(b.last) <= BurstThreshold:
		b.last = now
		return 0, false
	default:
		gap := now.Sub(b.start)
		b.start, b.last = now, now
		return gap, true
	}
}

// Window answers the reveal window a draw at now carries, in whole
// milliseconds and never below 1, given EXPECTED, the expected time from one
// burst's start to the next's.
//
// A DRAW INSIDE A BURST STATES WHAT IS LEFT OF THE BURST'S WINDOW, measured from
// the burst's start, so every push of one clump names the SAME deadline and the
// client spreads the whole clump toward it. Restating the full window on each
// push would push the deadline out by the clump's own length, and a window
// measured per push would shrink it to nothing.
//
// A draw that is not inside a burst — the block's settled whole landing after
// a pause, or a block with no fragment yet — starts afresh: whatever it adds
// is spread across the full window. Window never records anything.
func (b Burst) Window(expected uint32, now time.Time) uint32 {
	if b.start.IsZero() || now.Sub(b.last) > BurstThreshold {
		return expected
	}
	left := int64(expected) - now.Sub(b.start).Milliseconds()
	return uint32(max(1, left))
}
