//go:build playtest

package e2e

import (
	"testing"
	"time"
)

// THE SUBSTRATE'S PROOF THAT AN IDLE FRAME SETTLES, in the real webview
// against a real daemon, and NOT one of PLAYTEST-PLAN.md's owners'
// playbooks.
//
// WHAT WENT WRONG. Every capture waits for the framebuffer to hold still for
// a whole `playtestSettleWindow` before it fires, and against the webapp's
// resting animations -- the prompt bubble's `bubble-wave` above all, which
// runs forever on every `.bubble.user` in the scrollback -- that wait had no
// answer. MEASURED, one run of the D29-D32 playbook before the fix: 10 of 10
// captures ran the whole 2s `playtestSettleBound` out, took 62-70 redisplay
// rounds each, and were labelled "may be torn" in the manifest. Roughly 20s
// of a 33s section run spent waiting for a screen that was never going to
// stop, and a torn-frame note on every picture, which makes the note say
// nothing.
//
// The cure is `playtestMotionPaused`: the capture holds the page's resting
// animations still while it fires and releases them after. This is what says
// so about the RUNNING product rather than about the constants -- the
// stylesheet rule, the two scripts, the xwidget probe and the settle window
// only work if all four agree, and no host-only test can see that.

// playtestIdleSettleBound is how long a capture of an IDLE screen may take
// to settle before this proof fails.
//
// It is not the patience budget: `playtestSettleBound` is 2s and a capture
// that took anything like that long would have proved the defect rather than
// the fix. The floor is arithmetic -- the window is 50ms and the reads land
// on a 20ms grid, so the earliest a window can close is the 60ms
// `settledAt(0)` names, plus the time the reads themselves take.
//
// MEASURED, over the ten captures of one full D29-D32 run with the fix in:
// every one settled, at 58, 61, 61, 62, 63, 63, 64, 64, 65 and 67ms, each
// over exactly 3 redisplay rounds. 250ms is ~3.7x the worst of those, which
// is this module's standing rule for a wait bound, and is still eight times
// under the budget a torn capture would spend.
const playtestIdleSettleBound = 250 * time.Millisecond

// TestPlaytestAnIdleFrameSettlesWellInsideTheBudget is that proof.
func TestPlaytestAnIdleFrameSettlesWellInsideTheBudget(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "00-idle-settles",
		"The substrate's own proof that a capture of a QUIESCENT screen settles: the page's resting "+
			"animations are held still while the picture is taken, so the framebuffer really does stop "+
			"changing and the manifest's torn-frame note stays rare and true.")
	p := s.Book

	repository := s.repoAt(t, "repo")
	s.register(t, repository.Dir)
	s.openPanel(t)

	// A PROMPT BUBBLE IS THE POINT. An empty feed has no `.bubble.user` in
	// it and would settle even with the defect in place, so this proof needs
	// the very surface that never stopped moving -- and a SETTLED turn, so
	// that nothing in the world is legitimately still working.
	s.submit(t, "draw one plain prose answer for the substrate's settle proof")
	s.awaitInPage(t, "the user's own prompt bubble, whose fill carries the resting wave, to be drawn",
		`document.querySelector('[data-feed-row][data-row-kind="userPrompt"]')`)
	s.awaitInPage(t, "the assistant's response to settle, so nothing in the world is still working",
		`document.querySelector('[data-feed-row][data-row-kind="activity"][data-unit="response"][data-state="success"]')`)
	s.awaitArm(t, s.Name, "the turn to settle", emGHISettledArms...)

	p.capture("idle-settles", "the turn settled and the screen left alone",
		"the feed carries a `[data-row-kind=\"userPrompt\"]` bubble and a settled `[data-unit=\"response\"]`",
		"An idle editor with a prompt bubble and its answer. Every glyph is sharp: a picture taken "+
			"off a moving frame carries half of one draw and half of the next, and this one must not.")

	if !p.lastSettled {
		t.Errorf("a capture of an idle frame ran the whole %s patience budget out without the "+
			"framebuffer ever holding still for %s. The page's resting animations were supposed to be "+
			"held for the capture (see playtestMotionPaused); run again with %s=1 and the settle "+
			"diagnostic will name the pixels that moved",
			playtestSettleBound, playtestSettleWindow, playtestSettleDiagEnv)
	}
	if p.lastSettle > playtestIdleSettleBound {
		t.Errorf("a capture of an idle frame took %s to settle, over the measured %s: the screen is "+
			"still being disturbed between the reads, and every capture in every playbook pays it",
			p.lastSettle.Round(time.Millisecond), playtestIdleSettleBound)
	}
}
