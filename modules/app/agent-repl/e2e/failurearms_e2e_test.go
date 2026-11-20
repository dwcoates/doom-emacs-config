// failurearms_e2e_test.go — the turn-failure seam's wiring test.
//
// Under the owner's ruling (a) of 2026-10-14 every failure arm is pinned by
// its two halves: the shim's scenario golden (testdata/scenario-goldens,
// frames.json) and the daemon's projection of it (feed.json, checked by
// daemon/integration/scenariogoldens_test.go). What stays here is the one
// test that wires the seam end to end -- a real prompt through SubmitPrompt,
// answered by the real (--fake) shim against a named fake-SDK scenario --
// and asserts what the feed and the footer draw for a failure agent-repl
// retries.
//
// UNGROUNDED FAKES. Per agent-shim/claude/shim/AGENTS.md and
// testdata/captures/MANIFEST.md, NONE of the scenarios driven here is backed
// by a real capture: the capture harness quarantines API errors (so no
// `!api-*` golden can exist), model refusals are not reproducible on demand,
// and the `!fail-*` terminal reasons here have no recorded run. Every shape
// asserted below is therefore the FAKE'S OWN DECLARATION of what the vendor
// emits, not an observed vendor behavior — the frontend expectation, by
// contrast, is the proto's and is quoted arm by arm.
//
// THE PROTO WINS. Where the mocked vendor's declared arm and the daemon's
// drawn arm disagree, the assertion below is written to
// proto/src/frontend/v1/feed.proto and left to fail; each such case is called
// out in the test's own doc comment and in the dispatch report.
package e2e

import (
	"testing"

	"agentrepl/e2e/kit/stack"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// ---------------------------------------------------------------------------
// Local helpers, fa* prefixed — this suite shares one package across
// independently written area files.
// ---------------------------------------------------------------------------

// faApiErrorWarnings are the records a MID-TURN vendor request failure
// legitimately leaves, and they are this file's own subject: every scenario
// here asks the fake vendor to fail a request mid-turn, and the feed files it
// as the turn's evidence. Declared for the same reason the query-death area
// declares its own trail — a fault the arrangement provokes is a fault the
// daemon is RIGHT to record. The watcher's own record of the failure is INFO,
// since the vendor retries its request, so it is declared nowhere.
var faApiErrorWarnings = []string{"daemon.feed.api_error"}

// ---------------------------------------------------------------------------
// The twelve AgentFailure.api_request_failed sub-arms
// ---------------------------------------------------------------------------

// testARetriedRateLimitDrawsNoErrorRowAndTheFooterCountsDown wires the
// owner's ruling of 2026-11-15 end to end: a failure agent-repl retries draws
// no error row while its retry waits, and the footer says the retry is
// pending. `!api-429`'s fake fails the turn on a passing rate limit (no
// exhausted allowance), which the shim retries on its 30-second cadence, so
// the turn's terminal carries conversation.v1.AgentFailure.retry_promised and
// the feed withholds its `FeedTurnEndedErrored` (resolve/feed/retriedfailure.go)
// together with the vendor's own notice of the failure.
//
// THE PROMPT ROW'S SETTLING IS THE TURN'S END HERE: the feed settles the
// turn's prompt in the same drawing that withholds the error row, so a page
// read once the prompt has settled holds every row the terminal drew.
//
// The rate-limited card's `retry_after_ms` join (the vendor's 549ms) is
// pinned by the shim's golden (testdata/scenario-goldens/api-429/frames.json)
// and by the daemon's unit tests of the card a chain that gives up draws.
//
// Seam: turn-failure
func testARetriedRateLimitDrawsNoErrorRowAndTheFooterCountsDown(t *testing.T) {
	// Arrange
	w := stack.SharedWorld(t)
	w.ExpectWarnings(faApiErrorWarnings...)
	ws := w.NewWorkspace(t)
	footer := w.WatchFooter(ws)
	defer footer.Close()

	// Act
	turn := stack.SubmitPrompt(t, w, ws, "!api-429")
	stack.AwaitFeedRow(t, w, ws, "the retried turn's settled prompt", func(row *frontendv1.FeedRow) bool {
		return row.GetTurn().GetValue() == turn.GetValue() && row.GetUserPrompt() != nil && !row.GetUserPrompt().GetWorking()
	})

	// Assert
	for _, row := range openFeedPage(t, w, ws) {
		if row.GetTurn().GetValue() != turn.GetValue() {
			continue
		}
		if row.GetTurnEnded() != nil {
			t.Fatalf("the retried turn drew its ending %v, want no row while its retry waits", row.GetTurnEnded())
		}
		if row.GetActivity().GetResponse().GetNotice() != nil {
			t.Fatalf("the retried turn drew a vendor notice %v, want it withheld with the failure", row)
		}
	}
	view := pfAwaitView(t, w, footer.Stream, "the footer counting down to the retry", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetVendorFault().GetActivity().GetSalient().GetRetrying().GetNextAttempt() != nil
	})
	if status := view.GetStrip().GetStatus().GetVendorFault().GetActivity().GetSalient().GetRetrying().GetStatus(); status == "" {
		t.Fatalf("the retrying line names no failure, want the vendor's words: %v", view.GetStrip().GetStatus())
	}
}
