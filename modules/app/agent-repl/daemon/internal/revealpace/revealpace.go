// Package revealpace keeps the daemon's record of how often streamed fragments
// arrive, per model and per block kind, and turns it into the reveal window the
// webapp paces a response's type-out by (frontend.v1.FeedResponseRevealWindow,
// docs/protobuf-design/streaming-reveal-window.md).
//
// THE DAEMON MEASURES, THE CLIENT ONLY SPREADS. Each key holds a rolling window
// of the WindowSize most recent gaps between consecutive fragments of one
// block. Once the window is full its recency-weighted average is the expected
// gap until the next fragment, and the client spreads whatever it has not yet
// shown across that span. Until it is full there is no figure, and the client
// paces with its own default.
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

// Observe adds one measured gap to key's window, dropping the oldest once the
// window is full.
func (p *Pacer) Observe(key Key, gap time.Duration) {
	ms := gap.Milliseconds()
	p.mu.Lock()
	p.windows[key] = newest(append(p.windows[key], ms))
	held := len(p.windows[key])
	p.mu.Unlock()
	p.log.Debug("daemon.revealpace.observed", "recorded a streamed-fragment gap", dlog.Context{
		"model": key.Model, "kind": string(key.Kind), "gap_ms": ms, "held": held,
	})
}

// ExpectedGap answers the recency-weighted average gap of key's window, in
// whole milliseconds and never below 1, once the window is full. False while
// it is not: too few gaps have been seen to predict the next one.
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
