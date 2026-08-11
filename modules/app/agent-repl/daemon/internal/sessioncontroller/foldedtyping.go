// THE LIVE TYPING RELAY IS A PROMISE THE TOP-LEVEL FEED HAS TO BE ABLE TO KEEP.
//
// A ContentDelta is EPHEMERAL: it opens a preview on the frontend's top-level
// feed — a streaming text block, or a tool card badged "streaming input…" — and
// that preview is retired by the AUTHORITATIVE record of the same block landing
// on the same feed. Nothing else retires it. The frontend has no timer and is
// given none: guessing when a stream died is exactly the thing this daemon
// exists to know instead.
//
// A WINDOW BREAKS THAT PROMISE. While an async window is open, foldWindows
// diverts the session's conversation items OFF the top-level feed and into the
// window's bubble (asyncwindows.go). The authoritative record of a block
// previewed by a delta relayed during that window therefore never arrives on the
// feed — so the preview it opened stands forever, spinning, with no body and no
// way to ever get one. Six of them, consecutive, is what the user sees.
//
// So the relay is decided by the SAME rule the fold is decided by, in one place:
// a delta is relayed only when the item it previews will be KEPT on the feed.
// The one such item while a window is open is the OUTERMOST window's own opening
// call — its card is where the bubble hangs, so it stays (see windowFoldTarget).
// Everything else folds, and its preview is refused rather than opened.
//
// This is provenance, not a timeout: the daemon already knows where the record
// is going before the preview goes out, so no preview it cannot retire is ever
// created.
package sessioncontroller

import "sync"

// foldedTypingAnnounceEvery is how often a running suppression re-announces
// itself. A long skill emits thousands of deltas and a line each would drown the
// log; silence would hide a fold diverting the whole session's typing. The
// first is always announced and every Nth after it.
const foldedTypingAnnounceEvery = 200

// Reasons a relay verdict took the branch it took. Typed rather than composed at
// the log site so the branch a line reports is the branch the code took.
const (
	typingRelayKeptNoWindow      = "no_open_window"
	typingRelayKeptWindowOwnCard = "outermost_window_own_card_stays_on_feed"
	typingRelaySuppressedFold    = "previewed_emission_folds_into_window"
)

// typingRelayVerdict is where ONE ephemeral delta's previewed record is bound
// for, and therefore whether the preview may be opened on the top-level feed.
type typingRelayVerdict struct {
	// Suppress reports that the record this delta previews will be folded into
	// a bubble, so a top-level preview of it could never be retired.
	Suppress bool
	// BubbleID is the window bubble the record folds into. Set only when
	// Suppress is set — it is the destination that makes the preview orphaned.
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

// foldedTypingLedger counts the deltas each window bubble's fold has cost the
// live relay, so the refusal is loud on its first occurrence and stays audible
// without becoming the log.
//
// Zero value is ready to use.
type foldedTypingLedger struct {
	mu sync.Mutex
	n  map[string]uint64
}

// note records one suppression against a bubble and reports the running count
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
