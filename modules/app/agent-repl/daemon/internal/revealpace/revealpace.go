// Package revealpace keeps the daemon's record of how fast streamed text
// arrives, per model and per block kind, and turns it into the bounds the
// webapp types a response's text out within (frontend.v1.FeedResponseRevealPace,
// docs/protobuf-design/learned-rate-reveal.md).
//
// THE DAEMON MEASURES, THE CLIENT PACES. Each key holds a rolling window of the
// WindowSize most recent BLOCK SPEEDS: a settled block's characters over the
// time from its first fragment to its settle, as the caller measured it. The
// bounds are low percentiles of that window (MinPercentile, MaxPercentile), so
// the type-out runs a little slower than text usually arrives and rarely
// catches up and stalls. A key with fewer than MinBlocks speeds has no figure
// of its own and is given its kind's built-in default.
//
// WHY WHOLE BLOCKS, NOT GAPS. A gap record predicts the time to the next burst
// afresh for every burst, so its errors stay in the total: one 6538ms pause in
// thinking's gap window stretched its reveal to ~25 chars/sec against ~65
// arriving. A block's average speed already contains its pauses, so the pace
// follows the long-run arrival speed and a rare pause costs one stall.
//
// The windows live in memory, loaded once at boot, and a key's window is
// written back when the caller says a block of it settled (Persist), so the
// record survives a restart without a write per fragment.
package revealpace

import (
	"context"
	"fmt"
	"math"
	"slices"
	"sync"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// WindowSize is how many of the most recent block speeds a window holds.
const WindowSize = 50

// MinBlocks is how many block speeds a window, or a kind pooled across models,
// must hold before its own percentiles are trusted over the kind's default.
const MinBlocks = 20

// MinPercentile and MaxPercentile are the percentiles of a window's block
// speeds that become the reveal's minimum and maximum chars/sec (owner,
// 2026-10-10: about 45-55 chars/sec for thinking, which these reproduce on the
// 2026-10-10 store data).
const (
	MinPercentile = 25
	MaxPercentile = 38
)

// BankLimit is how far behind the arrived text the reveal may fall, as time
// spent typing at the maximum (owner, 2026-10-10).
const BankLimit = 1500 * time.Millisecond

// Bounds is the chars/sec range the webapp types arriving text out within.
type Bounds struct {
	// MinCharsPerSecond is the slowest the client types while it holds banked
	// text. Always > 0 and never above MaxCharsPerSecond.
	MinCharsPerSecond uint32
	// MaxCharsPerSecond is the fastest the client types while its bank is
	// within BankLimit. Always > 0.
	MaxCharsPerSecond uint32
}

// defaults is each kind's pace before enough of its blocks have been
// measured: the MinPercentile and MaxPercentile block speeds measured off the
// store on 2026-10-10 (2149 thinking blocks, 845 prose blocks over 7 days).
var defaults = map[wsm.RevealKind]Bounds{
	wsm.RevealKindThinking: {MinCharsPerSecond: 45, MaxCharsPerSecond: 55},
	wsm.RevealKindProse:    {MinCharsPerSecond: 49, MaxCharsPerSecond: 90},
}

// DefaultBounds answers kind's built-in pace. A kind with no default is a bug
// in the caller: the two kinds are a closed set.
func DefaultBounds(kind wsm.RevealKind) Bounds {
	b, ok := defaults[kind]
	if !ok {
		panic(fmt.Sprintf("revealpace: no default pace for reveal kind %q", kind))
	}
	return b
}

// Store is the durable record the windows are loaded from and written to
// (wsm.DB).
type Store interface {
	PutRevealSpeedWindow(ctx context.Context, w wsm.RevealSpeedWindow) error
	RevealSpeedWindows(ctx context.Context) ([]wsm.RevealSpeedWindow, error)
}

// Key names one window: the model the blocks streamed under and the kind of
// block they were.
type Key struct {
	Model string
	Kind  wsm.RevealKind
}

// Pacer holds every window. It is safe for concurrent use.
type Pacer struct {
	store Store
	log   dlog.Logger

	mu      sync.Mutex
	windows map[Key][]float64
}

// Load builds a Pacer from every window the store holds. A store that cannot
// be read fails the load: a pacer that silently started empty would hide a
// broken record behind a fresh one.
func Load(ctx context.Context, store Store, log dlog.Logger) (*Pacer, error) {
	stored, err := store.RevealSpeedWindows(ctx)
	if err != nil {
		log.Error("daemon.revealpace.load", "the stored reveal speed windows could not be read",
			dlog.Context{"error": err.Error()})
		return nil, fmt.Errorf("revealpace: load the stored windows: %w", err)
	}
	p := &Pacer{store: store, log: log, windows: make(map[Key][]float64, len(stored))}
	for _, w := range stored {
		p.windows[Key{Model: w.Model, Kind: w.Kind}] = newest(w.CharsPerSecond)
	}
	log.Info("daemon.revealpace.loaded", "loaded the stored reveal speed windows",
		dlog.Context{"windows": len(stored)})
	return p, nil
}

// Observe adds one settled block's arrival speed, in chars/sec, to key's
// window, dropping the oldest once the window is full. A speed that is not a
// positive finite number is a bug in the caller, which measures only blocks
// that streamed text over a positive time.
func (p *Pacer) Observe(key Key, charsPerSecond float64) {
	if !wsm.ValidRevealSpeed(charsPerSecond) {
		panic(fmt.Sprintf("revealpace: a block speed is a positive finite chars/sec, got %v", charsPerSecond))
	}
	p.mu.Lock()
	p.windows[key] = newest(append(p.windows[key], charsPerSecond))
	held := len(p.windows[key])
	p.mu.Unlock()
	p.log.Debug("daemon.revealpace.observed", "recorded a streamed block's arrival speed", dlog.Context{
		"model": key.Model, "kind": string(key.Kind), "chars_per_second": charsPerSecond, "held": held,
	})
}

// Bounds answers the pace for key: its window's percentiles once it holds
// MinBlocks speeds, its kind's default until then.
func (p *Pacer) Bounds(key Key) Bounds {
	p.mu.Lock()
	speeds := append([]float64(nil), p.windows[key]...)
	p.mu.Unlock()
	if len(speeds) < MinBlocks {
		return DefaultBounds(key.Kind)
	}
	return boundsOf(speeds)
}

// KindBounds answers the pace for a block of kind whose model is not known (a
// subagent's): the percentiles of every model's window of that kind pooled,
// once they hold MinBlocks speeds together, the kind's default until then.
func (p *Pacer) KindBounds(kind wsm.RevealKind) Bounds {
	var pooled []float64
	p.mu.Lock()
	for key, speeds := range p.windows {
		if key.Kind == kind {
			pooled = append(pooled, speeds...)
		}
	}
	p.mu.Unlock()
	if len(pooled) < MinBlocks {
		return DefaultBounds(kind)
	}
	return boundsOf(pooled)
}

// Persist writes key's window to the store. The store's failure is recorded
// here, once, at ERROR, and answered to the caller.
func (p *Pacer) Persist(ctx context.Context, key Key) error {
	p.mu.Lock()
	speeds := append([]float64(nil), p.windows[key]...)
	p.mu.Unlock()
	fields := dlog.Context{"model": key.Model, "kind": string(key.Kind), "speeds": len(speeds)}
	if err := p.store.PutRevealSpeedWindow(ctx, wsm.RevealSpeedWindow{Model: key.Model, Kind: key.Kind, CharsPerSecond: speeds}); err != nil {
		fields["error"] = err.Error()
		p.log.Error("daemon.revealpace.persist", "a reveal speed window could not be written", fields)
		return fmt.Errorf("revealpace: persist %s/%s: %w", key.Model, key.Kind, err)
	}
	p.log.Debug("daemon.revealpace.persisted", "wrote a reveal speed window", fields)
	return nil
}

// newest answers the last WindowSize speeds of speeds, oldest first.
func newest(speeds []float64) []float64 {
	if len(speeds) <= WindowSize {
		return speeds
	}
	return append([]float64(nil), speeds[len(speeds)-WindowSize:]...)
}

// boundsOf answers the MinPercentile and MaxPercentile of speeds, rounded to
// whole chars/sec, never below 1, with the minimum never above the maximum.
// speeds is never empty: both callers hold at least MinBlocks.
func boundsOf(speeds []float64) Bounds {
	sorted := slices.Clone(speeds)
	slices.Sort(sorted)
	lo := uint32(max(1, math.Round(percentile(sorted, MinPercentile))))
	hi := uint32(max(1, math.Round(percentile(sorted, MaxPercentile))))
	return Bounds{MinCharsPerSecond: min(lo, hi), MaxCharsPerSecond: hi}
}

// percentile answers the nearest-rank pct-th percentile of sorted (ascending,
// non-empty): the value at index floor(n * pct / 100), clamped to the last.
func percentile(sorted []float64, pct int) float64 {
	i := min(len(sorted)*pct/100, len(sorted)-1)
	return sorted[i]
}
