// THE LIVE TYPING RELAY IS A PROMISE, AND A PREVIEW IS RETIRED WHERE IT OPENS.
//
// A ContentDelta is EPHEMERAL: it opens a preview — a streaming text block, or
// a tool card badged "streaming input…" — and that preview is retired by the
// AUTHORITATIVE record of the same block landing on the SAME SURFACE. Nothing
// else retires it. The frontend has no timer and is given none: guessing when a
// stream died is exactly the thing this daemon exists to know instead.
//
// A WINDOW MOVES THE SURFACE. While an async window is open, foldWindows
// diverts the session's conversation items OFF the top-level feed and into the
// window's bubble (asyncwindows.go). The authoritative record of a block
// previewed by a delta relayed during that window therefore never arrives on
// the feed — so a preview opened THERE stands forever, spinning, with no body
// and no way to ever get one. Six of them, consecutive, is what the user saw.
//
// So the preview's DESTINATION is decided by the SAME rule the fold is decided
// by, in one place: a delta previews its record wherever that record will land.
// The one item that stays on the feed while a window is open is the OUTERMOST
// window's own opening call — its card is where the bubble hangs (see
// windowFoldTarget) — so its preview stays on the feed. Every other delta's
// preview is scoped to the bubble its record folds into, and rides out on
// TypingDelta.bubble_id for the frontend to open there.
//
// REFUSING THE RELAY IS NOT THE ANSWER, and was tried: suppressing these deltas
// outright kept every top-level preview retirable, but it also silenced the
// whole session's live typing for the life of the window — for a long skill,
// the entire turn, with the page showing nothing at all. The delta is real and
// the user wants it. Only its destination was ever wrong.
//
// This is provenance, not a timeout: the daemon already knows where the record
// is going before the preview goes out, so no preview it cannot retire is ever
// created.
package sessioncontroller

import "sync"

// foldedTypingAnnounceEvery is how often a running scoping re-announces itself.
// A long skill emits thousands of deltas and a line each would drown the log;
// silence would hide a fold diverting the whole session's typing into a bubble.
// The first is always announced and every Nth after it.
const foldedTypingAnnounceEvery = 200

// Reasons a relay verdict took the branch it took. Typed rather than composed at
// the log site so the branch a line reports is the branch the code took.
const (
	typingRelayKeptNoWindow      = "no_open_window"
	typingRelayKeptWindowOwnCard = "outermost_window_own_card_stays_on_feed"
	typingRelaySuppressedFold    = "previewed_emission_folds_into_window"
)

// typingRelayVerdict is where ONE ephemeral delta's previewed record is bound
// for, and therefore WHERE its preview must be opened.
type typingRelayVerdict struct {
	// Suppress reports that the record this delta previews will be folded into
	// a bubble, so its preview belongs there and NOT on the top-level feed.
	// The name is kept from when the verdict meant "drop it": what it has
	// always reported is that the top-level feed is the wrong surface.
	Suppress bool
	// BubbleID is the window bubble the record folds into, and therefore the
	// bubble the preview opens inside. Set only when Suppress is set — it is
	// the destination that makes a top-level preview unretirable.
	BubbleID string
	// Reason names the branch, one of the typingRelay* constants.
	Reason string
}

// typingRelayVerdict decides one ephemeral delta's fate against the window stack
// as it stands right now.
//
// toolUseId is the delta's stable tool identity, empty for a text or thinking
// delta. Only an input_json delta can name the outermost window's own opening
// call, which is the single kept case: an assistant's prose emitted while a
// window is open belongs to the window's work and folds with it.
func (s *asyncBubbleStore) typingRelayVerdict(toolUseID string) typingRelayVerdict {
	targets := s.windowFoldTargets()
	if len(targets) == 0 {
		return typingRelayVerdict{Reason: typingRelayKeptNoWindow}
	}
	if toolUseID != "" {
		for _, t := range targets {
			// parentBubbleID empty means this window is the OUTERMOST one, and
			// its own card stays on the top-level feed — so the preview of its
			// call's input IS retired, and relaying it is honest.
			if t.origin == toolUseID && t.parentBubbleID == "" {
				return typingRelayVerdict{Reason: typingRelayKeptWindowOwnCard}
			}
		}
	}
	return typingRelayVerdict{
		Suppress: true,
		BubbleID: targets[len(targets)-1].bubbleID,
		Reason:   typingRelaySuppressedFold,
	}
}

// foldedTypingLedger counts the deltas each window bubble's fold has diverted
// into it, so the diversion is loud on its first occurrence and stays audible
// without becoming the log.
//
// Zero value is ready to use.
type foldedTypingLedger struct {
	mu sync.Mutex
	n  map[string]uint64
}

// note records one scoped delta against a bubble and reports the running count
// plus whether this one is to be announced.
func (l *foldedTypingLedger) note(bubbleID string) (uint64, bool) {
	l.mu.Lock()
	defer l.mu.Unlock()
	if l.n == nil {
		l.n = map[string]uint64{}
	}
	l.n[bubbleID]++
	count := l.n[bubbleID]
	return count, count == 1 || count%foldedTypingAnnounceEvery == 0
}

// suppressed reports the running count for a bubble, for tests and diagnostics.
func (l *foldedTypingLedger) suppressed(bubbleID string) uint64 {
	l.mu.Lock()
	defer l.mu.Unlock()
	return l.n[bubbleID]
}
