package feed

import (
	"context"
	"time"
	"unicode/utf8"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/revealpace"
	"claude-repld/internal/wsm"
)

// THE REVEAL PACE (docs/protobuf-design/learned-rate-reveal.md). The webapp
// types an arriving bubble's text out at the speed it arrives, held between
// the minimum and maximum chars/sec the daemon states on every live draw
// (frontend.v1.FeedResponseRevealPace). The daemon learns those bounds from
// how fast whole blocks arrive, per model and per block kind, in revealpace's
// rolling record.
//
// EVERY LIVE DRAW OF EVERY RESPONSE BUBBLE CARRIES A PACE: prose, interim and
// thinking alike, the main agent's and every subagent's. A replay carries
// none, because its prose finished arriving long ago and is drawn whole.
//
// ONLY THE MAIN AGENT'S LIVE BLOCKS MEASURE, and their pace is their own
// (model, kind) record. A subagent's activities do not name the model that
// produced them, and the daemon knows only the session's model
// (wsState.model), so a subagent's block can neither be filed under its model
// nor read its model's record: it draws with its kind's record pooled across
// every model (revealpace.Pacer.KindBounds). The main agent before its model
// is known draws the same way.

// RevealPacing is the record of block speeds the resolver measures into and
// reads the reveal pace from (revealpace.Pacer).
type RevealPacing interface {
	Observe(key revealpace.Key, charsPerSecond float64)
	Bounds(key revealpace.Key) revealpace.Bounds
	KindBounds(kind wsm.RevealKind) revealpace.Bounds
	Persist(ctx context.Context, key revealpace.Key) error
}

// minMeasuredChars and minMeasuredDuration are the smallest block the daemon
// takes a speed from: a block of a few characters, or one whose text arrived
// within a few frames, says nothing about the vendor's streaming speed.
const (
	minMeasuredChars    = 40
	minMeasuredDuration = 200 * time.Millisecond
)

// revealSource is what one draw of a block measures into and reads its pace
// from.
type revealSource struct {
	// kind is the block's kind.
	kind wsm.RevealKind
	// live is whether the draw carries a pace at all: the live plane, with a
	// pacing record wired.
	live bool
	// measured is whether the block is the main agent's with its model known,
	// so its speed is filed under key and its pace read from key.
	measured bool
	// key is the (model, kind) record a measured block uses.
	key revealpace.Key
}

// revealSourceOf answers the pacing source of a draw of AGENT's block of KIND.
func (r *resolver) revealSourceOf(s *wsState, agent *conversationv1.AgentId, kind wsm.RevealKind) revealSource {
	live := r.deps.Pacing != nil && s.plane == planeLive
	measured := live && s.model != "" && agent.GetValue() == s.mainAgent
	src := revealSource{kind: kind, live: live, measured: measured}
	if measured {
		src.key = revealpace.Key{Model: s.model, Kind: kind}
	}
	return src
}

// revealPace answers the pace a draw from SRC carries: nil off the live plane
// (a replay is drawn whole), the block's own record for a measured block, and
// its kind's record pooled across models otherwise.
func (r *resolver) revealPace(src revealSource) *frontendv1.FeedResponseRevealPace {
	if !src.live {
		return nil
	}
	var b revealpace.Bounds
	if src.measured {
		b = r.deps.Pacing.Bounds(src.key)
	} else {
		b = r.deps.Pacing.KindBounds(src.kind)
	}
	return &frontendv1.FeedResponseRevealPace{
		MinCharsPerSecond: b.MinCharsPerSecond,
		MaxCharsPerSecond: b.MaxCharsPerSecond,
		BankLimitMs:       uint32(revealpace.BankLimit.Milliseconds()),
	}
}

// observeFragment notes a measured block's first text-bearing fragment: the
// moment its streaming starts and how much text that fragment brought, which
// arrived at that moment and so is left out of the speed.
func (r *resolver) observeFragment(fold *proseState, src revealSource, newText string) {
	if !src.measured || !fold.firstFragmentAt.IsZero() {
		return
	}
	fold.firstFragmentAt = r.deps.Now()
	fold.firstFragmentChars = utf8.RuneCountInString(newText)
}

// resetPacing forgets a block's streaming start, for a block that restarts or
// ends without a speed worth keeping.
func resetPacing(fold *proseState) {
	fold.firstFragmentAt = time.Time{}
	fold.firstFragmentChars = 0
}

// settlePacing files a measured block's arrival speed once it settles whole:
// the characters that arrived after its first fragment, over the time since
// it. A block too small to say anything is left out (minMeasuredChars,
// minMeasuredDuration). The window is written to its durable record; the
// pacer has logged a failed write at ERROR, and it is put on the topbar's
// warning chip so it is not the log's alone.
func (r *resolver) settlePacing(s *wsState, fold *proseState, src revealSource) {
	started, firstChars := fold.firstFragmentAt, fold.firstFragmentChars
	resetPacing(fold)
	if !src.measured || started.IsZero() {
		return
	}
	chars := utf8.RuneCountInString(fold.markdown) - firstChars
	elapsed := r.deps.Now().Sub(started)
	if chars < minMeasuredChars || elapsed < minMeasuredDuration {
		r.logger(s.id).Debug("daemon.feed.reveal_block_unmeasured",
			"a settled block was too small to measure its arrival speed",
			dlog.Context{"model": src.key.Model, "kind": string(src.kind), "chars": chars, "elapsed_ms": elapsed.Milliseconds()})
		return
	}
	r.deps.Pacing.Observe(src.key, float64(chars)/elapsed.Seconds())
	if err := r.deps.Pacing.Persist(context.Background(), src.key); err != nil {
		r.logger(s.id).Debug("daemon.feed.reveal_pacing_unsaved",
			"the reveal pacing record could not be saved; raised on the topbar",
			dlog.Context{"model": src.key.Model, "kind": string(src.kind)})
		r.raiseWarning(s, "reveal_pacing_unsaved",
			"the streaming reveal pacing record could not be saved: "+err.Error())
	}
}
