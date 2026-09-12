//go:build realtest

package realtest

import (
	"context"
	"fmt"
	"strings"
	"time"
)

// A KEY IS NOT DELIVERED BECAUSE THE POST RETURNED. It is delivered because
// EMACS SAYS SO.
//
// Every silent loss this harness has suffered came from the same shape: the
// window server accepted a CGEvent, the helper exited 0, and the run went on to
// assert an effect that could never arrive — so a harness hole was filed
// against the editor, twice as "a real `C-g` did not dismiss the prompt" and
// once as "`SPC TAB o` did not reach its command". minibuffer.go already reads
// the editor's own marks to attribute ONE key, the `C-g` that dismisses a
// standing minibuffer, after the fact. This file makes that reading part of
// pressing ANY key, before the run is allowed to believe it.
//
// THE MARKS ARE EMACS'S, AND THERE ARE TWO. On this macOS (NS) build a key that
// enters Emacs's input is obliged to leave one of them — minibuffer.go carries
// the `keyboard.c` reasoning and the sweep evidence, and this file does not
// re-derive it:
//
//   - `(recent-keys)` grows, because `read_char` `record_char`s what it
//     returns; or
//   - `quit-flag` is still armed, for a quit character nothing has taken yet.
//
// Neither mark, and the key never entered Emacs's input at all.
//
// AND THE READING HAS ONE AMBIGUITY, WHICH IS NAMED RATHER THAN IGNORED.
// `recent-keys` is a ring of the last `lossage-size` keys, and a run adopts a
// standing Emacs whose ring is normally full, so "it grew" is read off the
// rendered description changing. Appending one key to a full ring drops the
// oldest, and the description is unchanged only when every key in the ring is
// the same key as the one just pressed — k1..kn equals k2..kn+1 exactly when
// all of them are equal. That case is DETECTED (`recentKeysUniform`) and
// reported as undetermined rather than counted as an absent mark, because
// blaming the harness on a reading that cannot distinguish is the same sin as
// blaming the editor on one.

const (
	// keyDeliveryConfirmCeiling is how long Emacs may take to account for one
	// posted key before the press is called undelivered.
	//
	// The key is already queued on the process when this starts; the only
	// wait is for Emacs to turn its run loop once and for one emacsclient
	// round trip to read the mark back. Both are milliseconds when Emacs is
	// idle. Two seconds is a small multiple that also covers an Emacs busy in
	// a command — which is the state that lost keys in the first place — and
	// the target is held key for the whole of it, so a slow consumption is
	// waited out rather than dropped.
	keyDeliveryConfirmCeiling = 2 * time.Second

	// keyDeliveryPollInterval is how often the marks are re-read while
	// waiting. Tight because the target's focus is held for exactly as long as
	// this loop runs, and the owner's desktop is what pays for a slow poll.
	keyDeliveryPollInterval = 50 * time.Millisecond

	// keyDeliveryHoldCeiling is the helper's own bound on holding the target
	// key, passed as `--hold=`. Strictly larger than the confirm ceiling so
	// the hold outlives the reading it exists to protect, and bounded anyway
	// so a caller that dies mid-press cannot leave the owner's focus parked on
	// Emacs.
	keyDeliveryHoldCeiling = 5 * time.Second

	// keyDeliveryPostedLine is what the helper prints once the event is posted
	// and the target is being held key.
	keyDeliveryPostedLine = "keydriver-posted"

	// keyDeliveryAttempts is how many times a REPEATABLE chord is posted
	// before the press is reported undelivered. A chord that is not repeatable
	// is posted exactly once — see Chord.Repeatable for why a retry is not a
	// free action.
	keyDeliveryAttempts = 2
)

// InputMark is Emacs's own account of its input at one instant.
//
// Read as a unit and in one round trip: two probes taken a round trip apart
// could straddle the key being consumed, which would read as a mark appearing
// and disappearing.
type InputMark struct {
	// Keys is `(key-description (recent-keys))`.
	Keys string
	// QuitArmed is whether `quit-flag` was up.
	QuitArmed bool
	// ProbeFailure names the probe that would not answer, empty when it did.
	// A probe that failed is NOT an absent mark.
	ProbeFailure string
}

// inputMarkForm reads both marks in one form, so they describe one instant.
//
// The two are joined with a byte that cannot appear in a key description
// rather than parsed out of Lisp: the transport already carries strings, and a
// separator that could occur in the payload is a parser waiting to be wrong.
func inputMarkForm() string {
	return `(concat (if quit-flag "armed" "down") "\n" (key-description (recent-keys)))`
}

// ReadInputMark takes one reading of the editor's account of its input.
//
// It never answers an error: an editor that would not answer has said nothing
// about any key, and that has to travel WITH the reading so the verdict below
// can refuse to blame anybody on the strength of it.
func ReadInputMark(ctx context.Context, client *Client) InputMark {
	raw, err := client.ReadString(ctx, inputMarkForm())
	if err != nil {
		return InputMark{ProbeFailure: fmt.Sprintf("(recent-keys) and quit-flag: %v", err)}
	}
	return parseInputMark(raw)
}

// parseInputMark reads what inputMarkForm wrote.
func parseInputMark(raw string) InputMark {
	flag, keys, found := strings.Cut(raw, "\n")
	if !found {
		return InputMark{ProbeFailure: fmt.Sprintf("the input-mark probe answered %q, which carries no separator", raw)}
	}
	return InputMark{Keys: keys, QuitArmed: flag == "armed"}
}

// DeliveryVerdict is what Emacs's marks say about one posted key.
type DeliveryVerdict int

const (
	// DeliveryArrived: a mark appeared, so the key entered Emacs's input.
	DeliveryArrived DeliveryVerdict = iota
	// DeliveryAbsent: neither mark appeared, and on this build an arriving key
	// is obliged to leave one. The key was dropped between the window server
	// and Emacs's keymap, which is a HARNESS defect.
	DeliveryAbsent
	// DeliveryUndetermined: the editor did not answer, or the reading cannot
	// distinguish arrival from absence. Never blamed on anybody.
	DeliveryUndetermined
)

// judgeDelivery reads two marks taken around one press.
//
// The order of the questions is the order of their strength, and the
// undetermined answers come FIRST: a reading that cannot distinguish must never
// be overruled by one of the definite branches below it.
func judgeDelivery(before, after InputMark) (DeliveryVerdict, string) {
	if before.ProbeFailure != "" {
		return DeliveryUndetermined, "the editor would not answer before the press (" + before.ProbeFailure + ")"
	}
	if after.ProbeFailure != "" {
		return DeliveryUndetermined, "the editor would not answer after the press (" + after.ProbeFailure + ")"
	}
	if after.QuitArmed && !before.QuitArmed {
		return DeliveryArrived, "quit-flag came up, which only an arriving quit character does"
	}
	if after.Keys != before.Keys {
		return DeliveryArrived, "(recent-keys) changed, which is Emacs recording the key it read"
	}
	if recentKeysUniform(before.Keys) {
		return DeliveryUndetermined, "(recent-keys) is one key repeated for the whole ring, so appending that " +
			"same key again would render identically and the reading cannot tell arrival from absence"
	}
	if after.QuitArmed {
		return DeliveryArrived, "quit-flag was already armed before the press and still is, so a quit is owed " +
			"and this press cannot be shown to have been dropped"
	}
	return DeliveryAbsent, "(recent-keys) did not change and quit-flag is down, and on this build an arriving " +
		"key is obliged to leave one of those two marks"
}

// recentKeysUniform says whether a rendered `recent-keys` is the same key over
// and over, which is the one shape in which appending to a full ring renders
// identically.
//
// Two or more keys are required: a single key cannot have been shifted out of
// anything, so a one-key reading that did not change is a real absence.
func recentKeysUniform(keys string) bool {
	fields := strings.Fields(keys)
	if len(fields) < 2 {
		return false
	}
	for _, field := range fields[1:] {
		if field != fields[0] {
			return false
		}
	}
	return true
}

// DeliveryReceipt is what one press is known to have done.
//
// It carries the helper's own account alongside the editor's because the two
// answer different questions — the helper says whether the target held a key
// window while the event was in flight, the editor says whether the key
// arrived — and a finding needs both to be actionable.
type DeliveryReceipt struct {
	// Chord is the key that was pressed.
	Chord Chord
	// Verdict is what Emacs's marks said.
	Verdict DeliveryVerdict
	// Reason is why the verdict reads the way it does.
	Reason string
	// Attempts is how many times the chord was posted.
	Attempts int
	// Helper is the last `keydriver-receipt:` line, which says what
	// accessibility answered about the target's key window.
	Helper string
	// Before and After are the readings the verdict was taken from.
	Before InputMark
	After  InputMark
	// Elapsed is how long the confirmation took, focus held throughout.
	Elapsed time.Duration
	// Focus is what the EDITOR said about its own desktop focus while the
	// helper held the target key.
	//
	// It answers a question the key marks cannot: a pid-addressed CGEvent
	// reaches the process whether or not the activation that preceded it was
	// granted, so a confirmed key proves delivery and says nothing at all
	// about whether a focus edge happened. focus.go carries the reasoning and
	// the sweep that made it necessary.
	Focus FocusReading
}

// Confirmed says the press needs no report: the key arrived on the first post.
func (r DeliveryReceipt) Confirmed() bool {
	return r.Verdict == DeliveryArrived && r.Attempts <= 1
}

// Note renders the receipt in the words a manifest carries.
//
// A pure function of the receipt, for the same reason minibuffer.go's notes
// are: the wording of a finding must be testable without a running editor.
func (r DeliveryReceipt) Note() string {
	switch r.Verdict {
	case DeliveryArrived:
		if r.Attempts <= 1 {
			return fmt.Sprintf("key delivery confirmed: %s reached Emacs's input in %s — %s",
				r.Chord.Emacs, r.Elapsed.Round(time.Millisecond), r.Reason)
		}
		return fmt.Sprintf("HARNESS KEY DELIVERY WAS RETRIED: %s was posted %d times before Emacs's own "+
			"account showed it arriving (%s). The first post was dropped between the window server and the "+
			"keymap; the chord is repeatable (%s) so re-posting it is harmless, and the run continues. "+
			"The helper said: %s",
			r.Chord.Emacs, r.Attempts, r.Reason, r.Chord.repeatWhy(), r.Helper)
	case DeliveryUndetermined:
		return fmt.Sprintf("KEY DELIVERY UNCONFIRMED, AND NOT BLAMED ON ANYBODY: %s was posted and Emacs's "+
			"own account cannot say whether it arrived — %s. It is read as HAVING arrived, which is the "+
			"conservative direction: a real product defect must never be filed against this harness on the "+
			"strength of a reading nobody got. The helper said: %s",
			r.Chord.Emacs, r.Reason, r.Helper)
	default:
		note := fmt.Sprintf("HARNESS KEY DELIVERY FAILED, AND IT IS NOT A PRODUCT FINDING: %s was posted %d "+
			"time(s) to pid-addressed CGEvents while the target was held key, and Emacs's own account says "+
			"it never entered its input — %s. (recent-keys) went from %q to %q. The defect is in this "+
			"harness's key driver, not in the editor's handling of that key. The helper said: %s",
			r.Chord.Emacs, r.Attempts, r.Reason, tail(r.Before.Keys, 60), tail(r.After.Keys, 60), r.Helper)
		if !r.Chord.Repeatable {
			note += ". " + notRetriedNote(r.Chord)
		}
		return note
	}
}

// deliveryError is the error a press answers with when the key never arrived.
//
// It is an error rather than a note because every caller of Press already
// treats an error as "this chord did not happen", which is exactly what an
// undelivered key means; returning nil and a note would leave each of them to
// remember to look.
func (r DeliveryReceipt) deliveryError() error {
	return fmt.Errorf("%s", r.Note())
}

// notRetriedNote says why a dropped key was posted only once.
//
// A retry is not free: a key that was dropped at dispatch may still be sitting
// in the target's queue, so re-posting one that ADVANCES state — a workspace
// switch, a leader sequence, a letter that completes a command — risks the act
// happening twice, which is a worse lie than the act not happening. Only keys
// that return the editor to a resting state are re-posted.
func notRetriedNote(chord Chord) string {
	return fmt.Sprintf("it was posted once and not retried: %s is not repeatable (%s), and a dropped post may "+
		"still be queued on the target, so a second one could make the act happen twice",
		chord.Emacs, chord.repeatWhy())
}
