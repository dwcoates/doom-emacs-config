// failurearms_e2e_test.go — the turn-failure seam's wiring test.
//
// Under the owner's ruling (a) of 2026-10-14 every failure arm is pinned by
// its two halves: the shim's scenario golden (testdata/scenario-goldens,
// frames.json) and the daemon's projection of it (feed.json, checked by
// daemon/integration/scenariogoldens_test.go). What stays here is the one
// test that wires the seam end to end -- a real prompt through SubmitPrompt,
// answered by the real (--fake) shim against a named fake-SDK scenario --
// and asserts a SPECIFIC typed arm of `frontend.v1 FeedTurnEndedErrored.error`.
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

	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/integration/harness"
)

// ---------------------------------------------------------------------------
// Local helpers, fa* prefixed — this suite shares one package across
// independently written area files.
// ---------------------------------------------------------------------------

// faNewWorkspace mints a fresh fake-git-backed repository and registers it as
// a workspace on w's daemon. No real git anywhere: NewWorld never sets
// Opts.SkipFakeGit, so the daemon gets the daemon harness's scripted fake.
func faNewWorkspace(t *testing.T, w *World) *workspacev1.WorkspaceRef {
	t.Helper()
	repo := harness.NewRepo(t)
	return harness.Register(t, w.Daemon, repo.Dir)
}

// faApiErrorWarnings are the two records a MID-TURN vendor request failure
// legitimately leaves, and they are this file's own subject: every scenario
// here asks the fake vendor to fail a request mid-turn, the watcher routes
// that failure, and the feed files it as the turn's evidence. Declared for the
// same reason the query-death area declares its own trail — a fault the
// arrangement provokes is a fault the daemon is RIGHT to record.
var faApiErrorWarnings = []string{"daemon.sessionwatcher.api_error", "daemon.feed.api_error"}

// faDriveErrored drives one named failure scenario to completion and answers
// the turn's FeedTurnEndedErrored, failing loudly if the turn ended on any
// other outcome. The headline is checked here for every arm because Landing 8
// (PROTO-CHANGES.md) makes it unconditional: `FeedTurnEndedErrored.headline`
// is the client's whole account of what the arm means.
func faDriveErrored(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, scenario string) *frontendv1.FeedTurnEndedErrored {
	t.Helper()
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, scenario)
	row := AwaitTurnEnded(t, w, ws, turn)
	ended := row.GetTurnEnded()
	if ended == nil {
		t.Fatalf("row for turn %s has no TurnEnded: %v", turn.GetValue(), row)
	}
	errored := ended.GetErrored()
	if errored == nil {
		t.Fatalf("FeedTurnEnded.Outcome = %v, want Errored for !%s", ended, scenario)
	}
	if errored.GetHeadline().GetText() == "" {
		t.Fatalf("FeedTurnEndedErrored.Headline.Text is empty for !%s, want a daemon-composed sentence: %v", scenario, errored)
	}
	return errored
}

// ---------------------------------------------------------------------------
// The twelve AgentFailure.api_request_failed sub-arms
// ---------------------------------------------------------------------------

// TestApiRateLimitedCarriesTheVendorWait pins the ONE api arm with a field of
// its own: feed.proto:878-880 gives `FeedTurnErrorRateLimited` a
// `retry_after_ms` that "the client counts down from", UNSET meaning the
// vendor "said nothing, which is different from 'retry now'". `!api-429`'s
// fake states a wait — `rateLimits: { retryAfterSeconds: 30 }` on the
// `system:api_error` record and `retry_delay_ms: 549` on the `api_retry`
// message — so the drawn arm must carry a wait rather than leaving the field
// unset.
//
// THE JOIN IS MADE. convert/terminals.ts now states the opposite of what it
// once did — "The retry delay travels the same way: the vendor states it on
// `api_retry` (`retry_delay_ms`), and `ApiRateLimited.retry_after_ms` is the
// field the client counts down from, so the join IS made rather than
// dropped" — so the exact millisecond count the fake stated is asserted, not
// merely its presence. This shape is the fake's declaration, not a capture.
//
// Seam: turn-failure
func TestApiRateLimitedCarriesTheVendorWait(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	w.ExpectWarnings(faApiErrorWarnings...)
	ws := faNewWorkspace(t, w)

	// Act
	errored := faDriveErrored(t, w, ws, "api-429")

	// Assert
	limited := errored.GetRateLimited()
	if limited == nil {
		t.Fatalf("FeedTurnEndedErrored.Error = %T, want RateLimited", errored.GetError())
	}
	if limited.RetryAfterMs == nil {
		t.Fatalf("FeedTurnErrorRateLimited.RetryAfterMs is UNSET, want the vendor's stated wait: %v", limited)
	}
	// The fake states `retry_delay_ms: 549` on its `api_retry` message
	// (fake/scenarios/failures.ts apiErrorScenario), and fold.ts rounds that
	// number through unchanged, so 549 is the whole of the wait the client
	// counts down from.
	const wantMs = 549
	if got := limited.GetRetryAfterMs(); got != wantMs {
		t.Fatalf("FeedTurnErrorRateLimited.RetryAfterMs = %d, want the vendor's stated %d", got, wantMs)
	}
}
