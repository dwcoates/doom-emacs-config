//go:build playtest

package e2e

import (
	"testing"
)

// OWNER 5 of PLAYTEST-PLAN.md's partition: B14-B16 -- failed, hibernated,
// and merging/parked.
//
// B.14 is here. B.15 and B.16 are unwritten.

// TestPlaytestTabArmFailed is plan B.14: a turn that fails, and the tab that
// says so.
//
// `!fail-execution` ends the turn on the vendor's own execution error, which
// is a PURPLE fault by the module's color rule — the vendor's work — and not
// the blue of a broken local environment. That distinction is the picture's
// whole subject.
func TestPlaytestTabArmFailed(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "05-arm-failed",
		"Plan B.14. A turn that fails at the vendor, and the arm the tab paints for it.")
	p := s.Book

	repository := s.repoAt(t, "repo")
	name := s.register(t, repository.Dir)
	s.openPanel(t)
	p.note("one repository registered with its panel open",
		"the panel's webview is live and the webapp drew its footer")

	s.submit(t, "!fail-execution")
	// `:vendor-blocked` IS THE ANSWER, and it is asserted by name rather than
	// as "any settled arm". The module's color rule puts it in PURPLE -- the
	// vendor's own work went wrong -- and the whole value of this picture is
	// that the tab does NOT paint the blue of a broken local environment for
	// a failure that is not the local environment's.
	s.awaitArm(t, name, "the tab's arm to reach the vendor-blocked arm", playtestVendorBlockedArm)
	s.captureArm(t, "arm-after-failure", name,
		"the fake SDK's `!fail-execution` scenario ended the turn on an execution error",
		playtestVendorBlockedArm,
		"PURPLE is the whole subject: the vendor's own work went wrong, so the tab must NOT paint "+
			"the BLUE that means something on this machine broke, and must not still paint the RED "+
			"of a turn that is running.")
}

// TestPlaytestTabArmAttentionOnPermission is plan B.12's first half: a
// permission ask raised against a workspace the user is NOT looking at, and
// the attention marker the tab then paints.
//
// EMACS ANSWERS NOTHING. There is no permission-answering command in
// `lisp/`; the notification policy is Emacs's whole reaction to an ask, and
// the card in the webapp is the answering surface. B.12's second half —
// the marker CLEARING when the ask is answered from that card — is not here,
// because answering means clicking a feed row and the root feed's live tail
// is broken (see PLAYTEST-SPEC.md, "What is blocked").
//
// Two arrangements are forced, and both are the Emacs layer's own:
//   - `agent-repl--emacs-focused-p` is overridden, because it is an
//     environment probe and a container has no desktop to answer it
//     truthfully.
//   - the workspace under the ask is NOT the selected one, which is the case
//     `host.el` routes to `agent-repl-status-blink-tab`.
