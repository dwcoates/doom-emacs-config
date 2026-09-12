//go:build realtest

package realtest

import (
	"fmt"
	"time"
)

// DISMISSING A STANDING MINIBUFFER, AND WHY IT HAS TWO CHANNELS.
//
// The 2026-09-12 workspace runs found a real `C-g` failing to close a prompt
// twice in realtest 6 — once after "Add project directory: " and once after
// "Open workspace: " — each time waiting the whole 30s chord ceiling out and
// then continuing with the prompt standing, so every act after it ran against
// a minibuffer. Two defects produced that, and both are fixed:
//
//  1. A KEY THAT NEVER ARRIVED READ AS A KEY THAT DID NOTHING. keydriver.swift
//     posted its event even when `NSRunningApplication.activate()` had not
//     taken, and exited 0. AppKit dispatches a key event only to a key window,
//     so a post to a process without one is dropped with no error: the harness
//     could not tell "the C-g never reached Emacs" from "the C-g reached Emacs
//     and the read did not abort". Every press is now CONFIRMED against Emacs's
//     own marks while the helper holds the target key (delivery.go), so the
//     chord channel reports its own failure instead of being waited out. The
//     confirmation is what settles it: the helper's own key-window reading is
//     carried in the receipt as evidence and refuses the post only on a blind
//     press, where nothing would read the editor back.
//
//  2. THERE WAS NO SECOND CHANNEL. A chord is the only way in, so a chord that
//     does not land leaves the run with no way to put the editor back in a
//     usable state. There is one now, and it is deliberately narrow: an
//     emacsclient eval that aborts the read, used ONLY after the real chord has
//     been given its chance and failed, and always reported as the deviation it
//     is.
//
// THE DEVIATION IS LEGITIMATE AND BOUNDED (lead's brief, 2026-09-12). The
// substrate already answers a command's minibuffer reads from this side
// (realtest_workspace_acts_test.go says why), so aborting a read this side
// opened, through the same channel, adds no new class of unreality. What it
// must never do is hide that the chord failed: `wsActDismissByEval` is a
// reported finding, not a silent fallback, and the run says so in its manifest.
//
// AND THE FINDING MUST NAME THE RIGHT SYSTEM. The 2026-09-12 sweep reported
// four failed dismissals as "a defect in key delivery or in the binding", which
// is two accusations in one breath and neither of them evidence. They are not
// the same defect and they do not live in the same system:
//
//   - THE CHORD REACHED EMACS AND THE READ DID NOT ABORT. That is the product:
//     a user pressing `C-g` at that prompt has no eval channel and would be
//     stuck exactly as the run was.
//
//   - THE CHORD NEVER REACHED EMACS AT ALL. That is this harness. The editor's
//     quit handling was never asked to do anything, so reporting it as a
//     product defect accuses an innocent system and buries a real hole in the
//     key driver.
//
// EMACS ITSELF TELLS THE TWO APART, and `wsActQuitEvidence` is how the run
// asks. On the macOS (NS) build `handle_interrupt` never throws into the read
// — `keyboard.c` guards `quit_throw_to_read_char` with `#ifndef HAVE_NS` — so a
// `C-g` that arrives ALWAYS lands as `quit-flag` first, and the flag is taken
// by `kbd_buffer_get_event`'s own wait loop, which makes `read_char` return the
// quit character as an event. An arriving `C-g` therefore leaves one of two
// marks and cannot leave neither:
//
//   - `(recent-keys)` grows by the quit character, because `read_char`
//     `record_char`s what it returns; or
//   - `quit-flag` is still armed, because nothing has taken it yet.
//
// Neither mark means the key never entered Emacs's input at all. That is not an
// inference from silence: it is the absence of both marks Emacs is obliged to
// leave.

const (
	// wsActChordDismissCeiling is how long a real `C-g` may take to close a
	// standing minibuffer before the eval channel is used instead.
	//
	// A minibuffer abort is Emacs's own command loop unwinding one recursive
	// edit — microseconds of work behind one key event that has already been
	// posted and acknowledged by the window server. The only latency it has to
	// tolerate is the poll interval below plus one emacsclient round trip, both
	// of which are sized here. Three seconds is a small multiple of that, and
	// it exists so a chord that DID land is never reported as a failure; a
	// chord that has not landed in three seconds is not going to.
	wsActChordDismissCeiling = 3 * time.Second

	// wsActEvalDismissCeiling is how long ONE eval-channel abort may take.
	//
	// The eval schedules a zero-delay timer, and Emacs runs its timers from
	// the same `read_char` that is waiting on the minibuffer, so the abort
	// fires on the next input wait. Two seconds is a bound on that wait, not on
	// the abort.
	wsActEvalDismissCeiling = 2 * time.Second

	// wsActEvalDismissAttempts is how many eval aborts are sent before the run
	// gives up. More than one because a prompt can be nested — a completion
	// read inside a read — and each abort unwinds one level.
	wsActEvalDismissAttempts = 3

	// wsActDismissPollInterval is how often the minibuffer is re-read while
	// waiting for it to close. Deliberately far tighter than the startup
	// polls: this is a local unwind, not a startup, and the whole point of the
	// change is that a failure to dismiss is reported in seconds rather than
	// in half a minute.
	wsActDismissPollInterval = 100 * time.Millisecond
)

// wsActDismissStage says how a standing minibuffer was dealt with.
type wsActDismissStage int

const (
	// wsActDismissNothingStanding: there was no minibuffer to dismiss.
	wsActDismissNothingStanding wsActDismissStage = iota
	// wsActDismissByChord: a real `C-g` closed it, which is the path that
	// proves the chord as well as clearing the editor.
	wsActDismissByChord
	// wsActDismissByEval: the real `C-g` did not close it and the eval channel
	// did. This is a FINDING, never a silent success.
	wsActDismissByEval
	// wsActDismissFailed: neither channel closed it.
	wsActDismissFailed
	// wsActDismissChordNeverArrived: the eval channel closed it and Emacs's
	// own account says the `C-g` never entered its input, so the editor's
	// quit handling was never asked to do anything. A HARNESS finding.
	wsActDismissChordNeverArrived
)

// wsActQuitEvidence is the editor's own account of one `C-g` press.
//
// Gathered around the press rather than after the fact: `KeysBefore` is read
// before the key is posted, because "(recent-keys) grew" is only an answer when
// there is a before to compare against.
type wsActQuitEvidence struct {
	// KeysBefore is `(key-description (recent-keys))` before the press.
	KeysBefore string
	// KeysAfter is the same reading after the chord had its chance.
	KeysAfter string
	// QuitFlagArmed is whether `quit-flag' was still up afterwards.
	QuitFlagArmed bool
	// ProbeFailure names the probe that would not answer, empty when both
	// answered. A probe that failed is NOT read as an absent mark.
	ProbeFailure string
}

// Arrived says whether the quit character entered Emacs's input.
//
// EITHER MARK IS ENOUGH AND A FAILED PROBE IS NOT A MARK'S ABSENCE. An editor
// that would not answer has said nothing about the key, so the press is treated
// as arrived and the run reports the product wording plus the probe failure —
// the conservative direction, because it never lets a real product defect be
// filed against the harness.
func (e wsActQuitEvidence) Arrived() bool {
	if e.ProbeFailure != "" {
		return true
	}
	return e.QuitFlagArmed || e.KeysAfter != e.KeysBefore
}

// wsActQuitFlagForm reads whether a quit is still owed.
//
// Rendered as a word rather than as a Lisp boolean so a reader of the manifest
// sees what the editor said, and so an empty answer cannot pass for `nil`.
func wsActQuitFlagForm() string {
	return `(if quit-flag "armed" "down")`
}

// wsActAbortMinibufferForm is the elisp the eval channel sends.
//
// IT SCHEDULES THE ABORT RATHER THAN PERFORMING IT. `abort-minibuffers` (and
// `abort-recursive-edit` on an Emacs too old to have it) unwinds by throwing to
// the recursive edit's own tag, and throwing out of `server-process-filter` —
// which is where an emacsclient `--eval` runs — would unwind the server's call
// rather than answer it, so the probe transport would see a broken connection
// instead of a result. A zero-delay timer runs from the same `read_char` the
// minibuffer is waiting in, so the throw happens in the right place and the
// eval itself returns cleanly.
//
// `abort-minibuffers` is preferred where it exists (Emacs 28 and later): it
// unwinds EVERY minibuffer level up to the selected one and handles a
// minibuffer that is not the innermost, which `abort-recursive-edit` does not.
func wsActAbortMinibufferForm() string {
	return `(progn
  (run-at-time 0 nil
    (lambda ()
      (if (fboundp 'abort-minibuffers)
          (abort-minibuffers)
        (abort-recursive-edit))))
  t)`
}

// wsActDismissNote renders what happened, in the words the manifest carries.
//
// It is a pure function of the outcome so the same sentence appears in the
// test log and in the manifest, and so the wording of a finding is testable
// without a running editor.
func wsActDismissNote(stage wsActDismissStage, prompt string, evalAttempts int) string {
	switch stage {
	case wsActDismissNothingStanding:
		return "no minibuffer was standing, so nothing had to be dismissed"
	case wsActDismissByChord:
		return fmt.Sprintf("a real `C-g` dismissed the standing minibuffer %q within %s",
			prompt, wsActChordDismissCeiling)
	case wsActDismissByEval:
		return fmt.Sprintf("DEVIATION, AND A PRODUCT FINDING: a real `C-g` REACHED Emacs and did NOT dismiss "+
			"the standing minibuffer %q within %s, so it was aborted through the read channel instead — an "+
			"emacsclient eval scheduling `abort-minibuffers`, which took %d attempt(s). The editor is clean "+
			"for the acts that follow, but the chord did not do it, and a user at that prompt has no eval "+
			"channel", prompt, wsActChordDismissCeiling, evalAttempts)
	case wsActDismissChordNeverArrived:
		return fmt.Sprintf("HARNESS KEY DELIVERY FAILED, AND IT IS NOT A PRODUCT FINDING: a real `C-g` was "+
			"posted while %q stood and Emacs's own account says it never entered its input — neither did "+
			"(recent-keys) grow nor was `quit-flag` left armed, and on this build an arriving quit character "+
			"is obliged to leave one of those two marks. The editor's quit handling was never asked to do "+
			"anything. The prompt was aborted through the read channel instead, which took %d attempt(s); "+
			"the defect is in this harness's key driver", prompt, evalAttempts)
	case wsActDismissFailed:
		return fmt.Sprintf("MINIBUFFER COULD NOT BE DISMISSED: %q is still standing after a real `C-g` (%s) and "+
			"%d eval abort(s) (%s each). Every act after this one would run against a standing minibuffer, so "+
			"the run stops here rather than reporting on acts that never happened",
			prompt, wsActChordDismissCeiling, evalAttempts, wsActEvalDismissCeiling)
	default:
		return fmt.Sprintf("unknown minibuffer dismissal outcome %d for prompt %q", int(stage), prompt)
	}
}

// wsActQuitEvidenceNote renders the editor's own account of the press, so a
// reader of the manifest can check the verdict rather than take it.
//
// It is a pure function of the evidence for the same reason the dismissal note
// is: the wording of a finding must be testable without a running editor.
func wsActQuitEvidenceNote(e wsActQuitEvidence) string {
	flag := "down"
	if e.QuitFlagArmed {
		flag = "armed"
	}
	note := fmt.Sprintf("what the editor said about that `C-g`: (recent-keys) went from %q to %q and "+
		"quit-flag was %s afterwards", tail(e.KeysBefore, 60), tail(e.KeysAfter, 60), flag)
	if e.ProbeFailure != "" {
		note += fmt.Sprintf("; a probe would not answer (%s), so the press is read as having ARRIVED rather "+
			"than blamed on the harness on the strength of a reading nobody got", e.ProbeFailure)
	}
	return note
}

// tail renders the last n bytes of text, marking that it was cut.
//
// Lives beside the evidence note rather than in a test file: the notes that
// carry a finding's evidence are production harness code, and a helper only the
// tests can see would have to be written twice.
func tail(text string, n int) string {
	if len(text) <= n {
		return text
	}
	return "..." + text[len(text)-n:]
}
