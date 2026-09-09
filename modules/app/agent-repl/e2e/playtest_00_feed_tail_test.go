//go:build playtest

package e2e

import (
	"testing"
)

// THE SUBSTRATE'S OWN PROOF, and NOT one of PLAYTEST-PLAN.md's twenty
// owners' playbooks.
//
// Sixteen of the twenty owners photograph things the webapp draws in its
// FEED, and every one of them is worthless if the feed's live tail is not
// served: a picture of an empty feed says nothing about the product, and a
// table-driven loop would take two hundred of them. So this playbook proves
// the one precondition they all rest on, in the real `xwidget-webkit`
// webview against a real daemon, before any of them is written.
//
// WHAT WENT WRONG, and why this is a permanent playbook rather than a
// throwaway check. A browser caps a host at about six HTTP/1.1 connections
// and a server-streaming Connect call pins one for its whole life. The page
// opened six standing streams before the feed's, so `WatchFeed` was the
// seventh: queued in the browser forever, never on the wire, and therefore
// never a stream that ENDED -- so nothing filed a `daemon_unreachable` card
// and the page simply sat there with an empty feed. It was measured from
// inside the live page: streams 1-6 reached the daemon within 9ms and
// streams 7, 8 and 9 produced no daemon record at all.
//
// The count was UNBOUNDED, not merely six: every expanded subagent bubble
// opens another feed tail. That is why the last step here opens one, and why
// a fixed budget would never have been the fix.
//
// Everything the page subscribes to now rides ONE stream (`WatchPage`), so
// the failure is unrepresentable rather than unlikely. This playbook is what
// says so about the running product rather than about the code.

// TestPlaytestPageStreamsAndFeedTail is that proof.
func TestPlaytestPageStreamsAndFeedTail(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "00-feed-tail",
		"The substrate's own precondition, in the real webview: every standing view the page "+
			"subscribes to is served, the root feed tails LIVE rows without a reload, and an expanded "+
			"subagent bubble's own nested tail is served alongside it.")
	p := s.Book

	repository := s.repoAt(t, "repo")
	s.register(t, repository.Dir)
	s.openPanel(t)

	// THE ROOT FEED'S LIVE TAIL. Nothing is reloaded and nothing is reopened:
	// the rows must arrive on the standing tail, which is the exact thing
	// that was queued behind the connection cap. A reload would paint the
	// same rows from `OpenFeed`'s page and prove nothing.
	const prompt = "draw one plain prose answer for the playtest"
	s.submit(t, prompt)
	s.awaitInPage(t, "the user's own prompt bubble to arrive on the standing tail",
		`document.querySelector('[data-feed-row][data-row-kind="userPrompt"]')`)
	s.awaitInPage(t, "the assistant's response bubble to settle on the standing tail",
		`document.querySelector('[data-feed-row][data-row-kind="activity"][data-unit="response"][data-state="success"]')`)
	s.awaitArm(t, s.Name, "the turn to settle", emGHISettledArms...)
	p.capture("feed-tailed-live", "one plain-prose prompt submitted with composer RET, and nothing reloaded",
		"both rows arrived on the STANDING tail -- `[data-row-kind=\"userPrompt\"]` and a settled "+
			"`[data-unit=\"response\"]` -- with no reload and no second `OpenFeed`",
		"The feed carries TWO bubbles in order: the user's own prompt bubble with the text that was "+
			"typed, and beneath it the assistant's response bubble with prose in it. THE FEED MUST NOT "+
			"BE EMPTY: an empty feed here is the whole defect this playbook exists to catch.")

	// EVERY STANDING VIEW DREW, which is what "its subscription is served"
	// looks like from the page. Each of these is a different subscription on
	// the page's one stream, and before the mux each was a connection of its
	// own -- so a page that draws all of them at once is a page that is not
	// spending its whole budget on them.
	s.awaitInPage(t, "the workspace sidebar to draw its roster",
		`document.querySelector('[data-component="sidebar"]').textContent.trim() !== ""`)
	s.awaitInPage(t, "the topbar to draw",
		`document.querySelector('[data-component="topbar"]').textContent.trim() !== ""`)
	s.awaitInPage(t, "the hold tray to draw",
		`document.querySelector('[data-component="hold-tray"]').textContent.trim() !== ""`)
	s.awaitInPage(t, "the progress footer to draw its status word",
		`document.querySelector(".footer-status").textContent.trim() !== ""`)
	p.note("one repository registered and its panel opened",
		"the sidebar, the topbar, the hold tray and the progress footer have all drawn, so each of "+
			"their subscriptions is being served on the page's one stream")

	// AND THE UNBOUNDED ONE. Each expanded subagent bubble opens a feed tail
	// of its own, which is why no fixed connection budget could ever have
	// held: this one is the page's second, and on the old shape it could not
	// have been opened at all.
	const subagentRow = `[data-feed-row][data-row-kind="activity"][data-unit="subagent"]`
	s.submit(t, "!subagent")
	s.awaitInPage(t, "the subagent bubble to be drawn", `document.querySelector('`+subagentRow+`')`)
	s.clickInPage(t, "the subagent bubble's caret", subagentRow+` [data-expand]`)
	// THE CLAIM IS A SECOND TAIL, NOT A CSS STATE. What must be true is that
	// a feed container other than the root is carrying rows, because that
	// container has a stream of its own and it is the one no fixed connection
	// budget could ever have held. Asserting `data-expanded` instead would be
	// asserting how the bubble spells "open", which is the webapp suite's own
	// business and not this precondition's.
	s.awaitInPage(t, "the nested sub-feed to carry rows of its own",
		`document.querySelectorAll('[data-feed]:not([data-feed="root"]) [data-feed-row]').length > 0`)
	p.capture("subfeed-tailed-live", "the `!subagent` scenario run, and its bubble's caret clicked",
		"a feed container other than the root holds rows, so the bubble's OWN tail is served alongside "+
			"the root's",
		"The feed carries a SUBAGENT BUBBLE whose head names the commission, beneath the `!subagent` "+
			"prompt bubble, and the two bubbles from the previous step are still above it. "+
			"WHAT THIS PICTURE DOES NOT SETTLE: whether the caret visibly OPENS the bubble. The "+
			"assertion above proves a non-root feed container is carrying rows — the second tail is "+
			"served, which is this playbook's whole claim — but the bubble has been observed still "+
			"drawn collapsed at this point, and `data-expanded` never became true. That is the "+
			"webapp's own bubble toggle and is recorded as an open question, not asserted here.")
}
