package feed

import (
	"context"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/revealpace"
	"claude-repld/internal/wsm"
)

// THE REVEAL WINDOW (docs/protobuf-design/streaming-reveal-window.md). The
// webapp spreads the text of an arriving bubble it has not yet shown across
// the time the daemon expects to pass before the next fragment. The daemon
// learns that time by measuring the gap between consecutive fragments of one
// block, per model and per block kind, in revealpace's rolling window.
//
// ONLY THE MAIN AGENT'S LIVE FRAMES MEASURE, AND ONLY THEY CARRY A WINDOW.
// A replay's frames arrive in one burst, so their gaps say nothing about the
// vendor; and the daemon knows only the session's model (wsState.model), so a
// subagent's gaps could not be filed under the model that produced them. Both
// draw with no window, which the webapp paces with its own default.

// RevealPacing is the record of streamed-fragment gaps the resolver measures
// into and reads the reveal window from (revealpace.Pacer).
type RevealPacing interface {
	Observe(key revealpace.Key, gap time.Duration)
	ExpectedGap(key revealpace.Key) (uint32, bool)
	Persist(ctx context.Context, key revealpace.Key) error
}

// paceKey answers the window a draw of AGENT's block of KIND measures into
// and reads from, and false when the draw is one the window does not apply to
// (see above).
func (r *resolver) paceKey(s *wsState, agent *conversationv1.AgentId, kind wsm.RevealKind) (revealpace.Key, bool) {
	if r.deps.Pacing == nil || s.plane != planeLive || s.model == "" || agent.GetValue() != s.mainAgent {
		return revealpace.Key{}, false
	}
	return revealpace.Key{Model: s.model, Kind: kind}, true
}

// observeFragment records a text-bearing fragment's arrival on FOLD: the gap
// since the fold's previous one is a sample, and the first fragment of a
// block (whose wait is the time to first token, not the streaming cadence)
// only starts the clock.
func (r *resolver) observeFragment(fold *proseState, key revealpace.Key, ok bool) {
	if !ok {
		return
	}
	now := r.deps.Now()
	if !fold.lastFragmentAt.IsZero() {
		r.deps.Pacing.Observe(key, now.Sub(fold.lastFragmentAt))
		fold.sampled = true
	}
	fold.lastFragmentAt = now
}

// revealWindow answers the reveal window a draw carries, nil when the key
// does not apply or its window is not yet full.
func (r *resolver) revealWindow(key revealpace.Key, ok bool) *frontendv1.FeedResponseRevealWindow {
	if !ok {
		return nil
	}
	gap, full := r.deps.Pacing.ExpectedGap(key)
	if !full {
		return nil
	}
	return &frontendv1.FeedResponseRevealWindow{ExpectedGapMs: gap}
}

// settlePacing writes FOLD's window to its durable record once the block
// settles, if the fold added to it. The pacer has logged a failed write at
// ERROR; it is put on the topbar's warning chip so it is not the log's alone.
func (r *resolver) settlePacing(s *wsState, fold *proseState, key revealpace.Key, ok bool) {
	if !ok || !fold.sampled {
		return
	}
	fold.sampled = false
	if err := r.deps.Pacing.Persist(context.Background(), key); err != nil {
		r.logger(s.id).Debug("daemon.feed.reveal_pacing_unsaved",
			"the reveal pacing window could not be saved; raised on the topbar",
			dlog.Context{"model": key.Model, "kind": string(key.Kind)})
		r.raiseWarning(s, "reveal_pacing_unsaved",
			"the streaming reveal pacing record could not be saved: "+err.Error())
	}
}
