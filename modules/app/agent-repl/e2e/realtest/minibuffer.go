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
//   - THE CHORD NEVER LEFT THIS SIDE. That is this harness. The editor's quit
//     handling was never asked to do anything, so reporting it as a product
//     defect accuses an innocent system and buries a real hole in the key
//     driver.
//
// AND THERE IS A THIRD, WHICH IS THE ONE THE 2026-09-13 SWEEP ACTUALLY HIT.
// This file used to claim Emacs itself tells the first two apart, on the rule
// that an arriving `C-g` must leave `(recent-keys)` grown or `quit-flag`
// armed. THE RULE IS FALSE FOR THE QUIT CHARACTER, in both halves, and
// delivery.go now carries the `keyboard.c` derivation: the quit character is
// intercepted before `record_char` ever sees it, and the flag it arms is taken
// by the very read that is standing, microseconds later. A `C-g` that arrives
// and works leaves neither mark. So the marks cannot judge it at all, and the
// harness that judged it by them printed both findings at once for the same
// press — "HARNESS KEY DELIVERY FAILED, AND IT IS NOT A PRODUCT FINDING"
// immediately followed by "DEVIATION, AND A PRODUCT FINDING" — six times in
// one sweep.
//
// SO THE PRESS IS JUDGED BY ITS EFFECT, AND THE JUDGEMENT IS MADE ONCE. The
// `C-g` is pressed through `KeyDriver.PressWithEffect` with the effect it is
// pressed for — this prompt closing — polled inside the helper's hold. That
// one observation settles both questions, and the stage it produces is the
// run's ONLY verdict on the press:
//
//   - the prompt closed: `wsActDismissByChord`, and the chord is proven;
//   - the helper could not post at all: `wsActDismissNotPosted`, a HARNESS
//     finding, and no word about the product;
//   - Emacs's marks DID show the key arriving and the prompt still stood:
//     `wsActDismissByEval`, a PRODUCT finding;
//   - the key was posted, the prompt stood, and the quit character leaves no
//     mark to confirm it by: `wsActDismissUndetermined`, which names neither
//     system and carries the helper's own receipt for the owner to rule on.

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
	// wsActDismissByEval: the real `C-g` arrived — Emacs's own marks say so —
	// and did not close it, and the eval channel did. This is a PRODUCT
	// finding, never a silent success.
	wsActDismissByEval
	// wsActDismissFailed: neither channel closed it.
	wsActDismissFailed
	// wsActDismissNotPosted: the press itself failed, so the key never left
	// this side and the editor's quit handling was never asked to do
	// anything. A HARNESS finding, and the press's own error says what went
	// wrong.
	wsActDismissNotPosted
	// wsActDismissUndetermined: the key was posted, the prompt stood, and the
	// quit character leaves no mark that could confirm it arrived. Neither
	// system is named; the owner rules on it.
	wsActDismissUndetermined
)

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
	case wsActDismissNotPosted:
		return fmt.Sprintf("HARNESS KEY DELIVERY FAILED, AND IT IS NOT A PRODUCT FINDING: the `C-g` that "+
			"should have dismissed %q was never posted — the press reported its own failure above, and the "+
			"editor's quit handling was therefore never asked to do anything. The prompt was aborted through "+
			"the read channel instead, which took %d attempt(s); the defect is in this harness's key driver",
			prompt, evalAttempts)
	case wsActDismissUndetermined:
		return fmt.Sprintf("UNDETERMINED, AND NAMED AGAINST NEITHER SYSTEM: a real `C-g` was posted while %q "+
			"stood, the prompt did not close within %s, and the quit character leaves NO mark this side can "+
			"read it by — it is intercepted before `record_char` and the `quit-flag` it arms is taken by the "+
			"standing read itself (delivery.go carries the derivation). So this run cannot say whether the "+
			"key reached Emacs and the read refused to abort, or the key was dropped after the post. The "+
			"prompt was aborted through the read channel instead, which took %d attempt(s); the helper's own "+
			"receipt is beside this note and the owner rules on it",
			prompt, wsActChordDismissCeiling, evalAttempts)
	case wsActDismissFailed:
		return fmt.Sprintf("MINIBUFFER COULD NOT BE DISMISSED: %q is still standing after a real `C-g` (%s) and "+
			"%d eval abort(s) (%s each). Every act after this one would run against a standing minibuffer, so "+
			"the run stops here rather than reporting on acts that never happened",
			prompt, wsActChordDismissCeiling, evalAttempts, wsActEvalDismissCeiling)
	default:
		return fmt.Sprintf("unknown minibuffer dismissal outcome %d for prompt %q", int(stage), prompt)
	}
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
