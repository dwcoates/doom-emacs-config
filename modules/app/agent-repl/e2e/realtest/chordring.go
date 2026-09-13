//go:build realtest

package realtest

import (
	"fmt"
	"strings"
)

// WHAT EMACS'S RING SAYS ABOUT A CHORD THAT DID NOT RAISE ITS PROMPT.
//
// "CHORD DID NOT REACH ITS COMMAND" IS AN ACCUSATION, and until this file it
// was the only sentence the prover had for every way a chord can fail. The
// 2026-09-13 11:10 sweep spent it on the wrong system. Realtest 8 pressed
// `SPC j m p`, the minibuffer held nothing, and the run reported the binding —
// while Emacs's own ring said, in full:
//
//	<escape> s <escape> ' SPC j m p
//
// Every key the run pressed is there, in order, at the tail. What is also
// there is a `'` sitting between the run's own `<escape>` and its `SPC`, and a
// `'` in evil normal state is `evil-goto-mark`, which READS ITS NEXT KEY as the
// mark's name. Emacs's own messages record the consequence, in this order:
// `evil-goto-mark: Marker ' ' is not set in this buffer` — the mark named by
// the `SPC` it swallowed — and then `evil-line-move: End of buffer` for the
// `j`, leaving `m p` to set a marker. The leader never got to be a leader. The
// binding is fine and was never asked.
//
// THE RUN KNOWS WHAT IT PRESSED, SO IT CAN SAY THIS ITSELF. Every chord
// sequence is preceded by a real `<escape>` (inputstate.go says why), so the
// ring of an UNCONTAMINATED press ends with that escape followed by the
// sequence. A ring that ends with the sequence but carries something else
// immediately before it is carrying input this run did not send, arriving
// between two of its own posts — and whatever that key began ate the leader.
//
// THE STRAY KEYS ARE NOT THIS HARNESS'S POSTS, and the ring is how that is
// known too: no chord in the table uses keycode 1 (`s`) or 39 (`'`), and every
// chord the run did post appears in the ring in the order it was posted. The
// key driver holds Emacs frontmost for up to the confirm ceiling around each
// press, which is the window in which a keystroke from anywhere else on the
// desktop lands in the editor. So this file does not fix the arrival. It stops
// the run from filing it against the binding, and prints the evidence that says
// which key did the swallowing.

// wsActRingReading is what the ring says about a sequence that did not work.
type wsActRingReading int

const (
	// wsActRingAsPressed: the ring ends with the run's own escape followed by
	// the sequence, so every key arrived, in order, with nothing in between.
	// Whatever went wrong is downstream of the keymap.
	wsActRingAsPressed wsActRingReading = iota
	// wsActRingForeignPreface: the ring ends with the sequence, and the key
	// immediately before it is not the escape the run pressed. Something the
	// run did not send arrived between the two, and the leader was read as
	// that something's argument.
	wsActRingForeignPreface
	// wsActRingSequenceMissing: the ring does not end with the sequence at
	// all, so at least one of the keys never arrived.
	wsActRingSequenceMissing
)

// readChordRing classifies a rendered `(recent-keys)` against what was pressed.
//
// `preface` is the chord pressed immediately before the sequence — always the
// `<escape>` that clears pending input — and `recorded` is the sequence in
// Emacs's own spelling (`SpellRecorded`). It answers the reading and, for a
// foreign preface, the key that was found in the escape's place.
//
// A pure function of two strings, so the wording of a finding is testable
// without a running editor, exactly as minibuffer.go's notes are.
func readChordRing(keys, preface, recorded string) (wsActRingReading, string) {
	pressed := strings.Fields(recorded)
	ring := strings.Fields(keys)
	if len(pressed) == 0 || len(ring) < len(pressed) {
		return wsActRingSequenceMissing, ""
	}
	tail := ring[len(ring)-len(pressed):]
	for i, key := range pressed {
		if tail[i] != key {
			return wsActRingSequenceMissing, ""
		}
	}
	before := ring[:len(ring)-len(pressed)]
	if len(before) == 0 {
		// Nothing precedes the sequence at all, so the escape this run pressed
		// has been shifted out of the ring rather than replaced by something.
		// That is not evidence of foreign input.
		return wsActRingAsPressed, ""
	}
	if last := before[len(before)-1]; last != preface {
		return wsActRingForeignPreface, last
	}
	return wsActRingAsPressed, ""
}

// wsActRingNote is the sentence a failed chord adds to its finding.
//
// It never repeats the ring — the caller's own note already carries it — and it
// says which system the reading points at, because a finding that names two is
// a finding that names neither (minibuffer.go says why that matters here).
func wsActRingNote(reading wsActRingReading, foreign, spelled, preface string) string {
	switch reading {
	case wsActRingForeignPreface:
		return fmt.Sprintf("AND IT IS NAMED AGAINST NEITHER THE BINDING NOR THE KEY DRIVER: every key of "+
			"`%s` is in Emacs's ring, in order, at its very end — but the key immediately before them is "+
			"`%s`, and this run pressed `%s` there. Input it did not send arrived between its own two posts, "+
			"and whatever that key began read the leader as its argument. A `'` is `evil-goto-mark`, which "+
			"reads the next key as a mark name; the editor's own messages name the key it swallowed. The "+
			"binding was never asked, so it is not the finding: the finding is that a keystroke from outside "+
			"this run reached the editor while the key driver held it frontmost",
			spelled, foreign, preface)
	case wsActRingSequenceMissing:
		return fmt.Sprintf("AND THE RING DOES NOT END WITH `%s` AT ALL, so at least one of its keys never "+
			"arrived and the keymap was never given the whole sequence to resolve. That is a key-delivery "+
			"finding against this harness before it is anything else", spelled)
	default:
		return fmt.Sprintf("AND THE RING ENDS WITH `%s` PRECEDED BY THIS RUN'S OWN `%s`, so every key arrived, "+
			"in order, with nothing in between. The sequence reached the keymap and the command behind it did "+
			"not raise its prompt, which is a finding about the binding", spelled, preface)
	}
}
