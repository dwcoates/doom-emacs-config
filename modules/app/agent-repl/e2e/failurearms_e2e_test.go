// failurearms_e2e_test.go — the failure family's UNCOVERED arms.
//
// turnlifecycle_e2e_test.go drives five members of the failure family
// (`!fail-execution`, `!fail-max-turns`, `!fail-budget`, `!fail-stop-hook`,
// `!fail-structured-output`) and stops short of the rest. This file drives
// the remaining twenty-six: every `!api-*` class, both model-refusal
// scenarios, and the twelve `terminal_reason` stops with no sibling test.
// The idiom is turnlifecycle's exactly — one real prompt through SubmitPrompt,
// answered by the real (--fake) shim against a NAMED fake-SDK scenario, with
// durability proven by driveScenarioToCompletion's cursor-advance wait — and
// every assertion names a SPECIFIC typed arm of
// `frontend.v1 FeedTurnEndedErrored.error`, never merely "the turn ended".
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

	"claude-repld/integration/harness"
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

// faAssertMessage pins `FeedTurnEndedErrored.message` — feed.proto: "The
// vendor's own sentence, when one was recorded" — to the exact wording the
// fake declared, so a rewording anywhere along the seam is caught rather than
// absorbed by a non-empty check.
func faAssertMessage(t *testing.T, errored *frontendv1.FeedTurnEndedErrored, want string) {
	t.Helper()
	if got := errored.GetMessage().GetText(); got != want {
		t.Fatalf("FeedTurnEndedErrored.Message.Text = %q, want %q", got, want)
	}
}

// faAssertHeadline pins `FeedTurnEndedErrored.headline` — the sentence
// feed.proto has the client draw verbatim — to the daemon's exact per-arm
// wording. Asserted by NAME of the arm's own spelling rather than by a
// non-empty check, because twelve api classes with one collapsed sentence is
// precisely the regression this family exists to catch.
func faAssertHeadline(t *testing.T, errored *frontendv1.FeedTurnEndedErrored, want string) {
	t.Helper()
	if got := errored.GetHeadline().GetText(); got != want {
		t.Fatalf("FeedTurnEndedErrored.Headline.Text = %q, want the arm's own sentence %q", got, want)
	}
}

// ---------------------------------------------------------------------------
// The twelve AgentFailure.api_request_failed sub-arms
// ---------------------------------------------------------------------------

// TestApiRequestFailedArms drives all twelve `!api-*` scenarios
// (agent-shim/claude/shim/src/fake/scenarios/failures.ts, apiErrorScenario)
// and asserts each reaches ITS OWN arm of `FeedTurnEndedErrored.error`.
// Each fake emits the whole arc its doc comment describes — a
// `system:api_error` transcript record, an `api_retry` message for the
// retriable classes, then an `error_during_execution` result with
// `terminal_reason: "api_error"` and the status — and every one of those
// shapes is the FAKE'S DECLARATION, not a capture: the capture harness
// quarantines failed runs, so no `!api-*` golden exists or can exist without
// an explicit exception to that rule.
//
// THE HEADLINE IS THE USER-VISIBLE SURFACE, and it is asserted EXACTLY, arm
// by arm. feed.proto:846-848 makes `FeedTurnEndedErrored.headline` the
// sentence "the client draws verbatim", and
// daemon/internal/resolve/feed/turnended.go's apiErrorArm states outright
// that "THE ARM IS THE CAUSE and the sentence is ours: the client holds no
// per-arm table" — so the twelve arms have twelve DISTINCT spellings and a
// collapsed sentence would be a real regression. Equality is also a SPECIFIC
// NEGATIVE: turnended.go appends the turn's evidence to the headline when the
// resolver saw a mid-turn `OnApiError`, and none of these scenarios leaves
// one — the fake writes its `system:api_error` line to the TRANSCRIPT only
// (failures.ts apiErrorScenario), so the vendor's failure reaches the daemon
// once, as the terminal — and the asserted headline carries no evidence
// clause accordingly.
//
// ALL THREE FORMERLY-UNREACHABLE ARMS NOW LAND. `billing_error`,
// `oauth_org_not_allowed` and `max_output_tokens` were once unreachable
// because the result seam carried only an HTTP status, which cannot separate
// a 402 billing failure from anything else charged, a 403 org refusal from an
// ordinary permission denial, or a status-less max-output failure from
// nothing at all. The vendor's own error CLASS now rides an
// `SDKAssistantMessageError` on the failed assistant message, the fold holds
// it for the turn (convert/fold.ts rememberVendorApiError) and
// convert/terminals.ts apiFailureKind reads the class FIRST for exactly those
// three, so each reaches its own proto arm.
func TestApiRequestFailedArms(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name     string
		scenario string
		message  string
		headline string
		arm      func(*frontendv1.FeedTurnEndedErrored) bool
		want     string
	}{
		{
			name:     "RateLimited",
			scenario: "api-429",
			message:  "Rate limited; retry after 30 seconds.",
			headline: "rate limited by the vendor",
			arm:      func(e *frontendv1.FeedTurnEndedErrored) bool { return e.GetRateLimited() != nil },
			want:     "RateLimited (feed.proto:879-880, 429)",
		},
		{
			name:     "Overloaded",
			scenario: "api-529",
			message:  "The API is overloaded.",
			headline: "the vendor API is overloaded",
			arm:      func(e *frontendv1.FeedTurnEndedErrored) bool { return e.GetOverloaded() != nil },
			want:     "Overloaded (feed.proto:881-882, 529)",
		},
		{
			name:     "AuthenticationFailed",
			scenario: "api-401",
			message:  "Authentication failed.",
			headline: "the credential was rejected — sign in again",
			arm:      func(e *frontendv1.FeedTurnEndedErrored) bool { return e.GetAuthenticationFailed() != nil },
			want:     "AuthenticationFailed (feed.proto:883-884, 401)",
		},
		{
			name:     "PermissionDenied",
			scenario: "api-403",
			message:  "Permission denied for this request.",
			headline: "the credential lacks permission for this request",
			arm:      func(e *frontendv1.FeedTurnEndedErrored) bool { return e.GetPermissionDenied() != nil },
			want:     "PermissionDenied (feed.proto:885-886, 403)",
		},
		{
			name:     "InvalidRequest",
			scenario: "api-400",
			message:  "The request was invalid.",
			headline: "the vendor refused the request as malformed",
			arm:      func(e *frontendv1.FeedTurnEndedErrored) bool { return e.GetInvalidRequest() != nil },
			want:     "InvalidRequest (feed.proto:887-888, 400)",
		},
		{
			name:     "RequestTooLarge",
			scenario: "api-413",
			message:  "The request was too large.",
			headline: "the request exceeded the vendor's size limit",
			arm:      func(e *frontendv1.FeedTurnEndedErrored) bool { return e.GetRequestTooLarge() != nil },
			want:     "RequestTooLarge (feed.proto:889-890, 413)",
		},
		{
			name:     "NotFound",
			scenario: "api-404",
			message:  "The requested model was not found.",
			headline: "the model or resource does not exist",
			arm:      func(e *frontendv1.FeedTurnEndedErrored) bool { return e.GetNotFound() != nil },
			want:     "NotFound (feed.proto:891-892, 404)",
		},
		{
			name:     "Internal",
			scenario: "api-500",
			message:  "The service raised.",
			headline: "the vendor API hit its own internal error",
			arm:      func(e *frontendv1.FeedTurnEndedErrored) bool { return e.GetInternal() != nil },
			want:     "Internal (feed.proto:893-894, 500)",
		},
		{
			name:     "BillingError",
			scenario: "api-billing",
			message:  "The account has a billing problem.",
			headline: "the account could not be charged — check your billing",
			arm:      func(e *frontendv1.FeedTurnEndedErrored) bool { return e.GetBillingError() != nil },
			want:     "BillingError (feed.proto:904-905, 402)",
		},
		{
			name:     "OauthOrgNotAllowed",
			scenario: "api-oauth-org",
			message:  "This organization is not permitted to use OAuth here.",
			headline: "your organization does not allow this OAuth access",
			arm:      func(e *frontendv1.FeedTurnEndedErrored) bool { return e.GetOauthOrgNotAllowed() != nil },
			want:     "OauthOrgNotAllowed (feed.proto:909-910)",
		},
		{
			name:     "MaxOutputTokens",
			scenario: "api-max-output",
			message:  "The response hit the max output tokens.",
			headline: "the request asked for more output than the model will produce",
			arm:      func(e *frontendv1.FeedTurnEndedErrored) bool { return e.GetMaxOutputTokens() != nil },
			want:     "MaxOutputTokens (feed.proto:911-913)",
		},
		{
			name:     "VendorUnmodeled",
			scenario: "api-unmodeled",
			message:  "An error class this build does not model.",
			headline: "the vendor reported an error class we do not model yet",
			arm:      func(e *frontendv1.FeedTurnEndedErrored) bool { return e.GetVendorUnmodeled() != nil },
			want:     "VendorUnmodeled (feed.proto:895-896, an unmodeled class kept BY NAME)",
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			t.Parallel()
			// Arrange
			w := NewWorld(t, WorldOpts{})
			w.ExpectWarnings(faApiErrorWarnings...)
			ws := faNewWorkspace(t, w)

			// Act
			errored := faDriveErrored(t, w, ws, test.scenario)

			// Assert
			if !test.arm(errored) {
				t.Fatalf("!%s: FeedTurnEndedErrored.Error = %T, want %s", test.scenario, errored.GetError(), test.want)
			}
			faAssertHeadline(t, errored, test.headline)
			faAssertMessage(t, errored, test.message)
		})
	}
}

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

// TestApiUnmodeledKeepsTheVendorClassByName pins the one api arm that carries
// a name: feed.proto:895-896 confines `vendor_unmodeled` to "an API error
// class this schema does not model", and convert/terminals.ts apiFailureKind
// says a class the vendor added later "is kept BY NAME as `unmodeled` rather
// than being silently mishandled". `!api-unmodeled` states the class
// `"unknown"` on a status (418) no arm claims, so the drawn arm must repeat
// that class verbatim — an empty `type` would be the collapse the arm exists
// to prevent. Ungrounded: the class and the status are the fake's own
// declaration, since no `!api-*` capture exists.
func TestApiUnmodeledKeepsTheVendorClassByName(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	w.ExpectWarnings(faApiErrorWarnings...)
	ws := faNewWorkspace(t, w)

	// Act
	errored := faDriveErrored(t, w, ws, "api-unmodeled")

	// Assert
	unmodeled := errored.GetVendorUnmodeled()
	if unmodeled == nil {
		t.Fatalf("FeedTurnEndedErrored.Error = %T, want VendorUnmodeled (feed.proto:895-896)", errored.GetError())
	}
	const wantClass = "unknown"
	if got := unmodeled.GetType(); got != wantClass {
		t.Fatalf("FeedTurnErrorVendorUnmodeled.Type = %q, want the vendor's own class %q kept by name", got, wantClass)
	}
}

// ---------------------------------------------------------------------------
// The terminal_reason stops with no named arm of their own
// ---------------------------------------------------------------------------

// TestTerminalReasonTurnFailedArms drives the twelve `terminal_reason` stops
// the covered siblings leave out. Each fake (failures.ts stopScenario) emits
// thinking plus a partial answer, then an error `result` pairing a declared
// subtype with the terminal reason — the fake's own declaration, since no
// capture reaches any of these terminals.
//
// THE PROTO SAYS WHERE THEY LAND. feed.proto:925-927:
// "FailureVendorTurnFailed turn_failed = 22" carries
// "AgentFailure.structured_output_retry_exhausted AND EVERY OTHER
// UNCLASSIFIED ABNORMAL END; `stop_reason` names the vendor's own word", and
// feed.proto:895-896 confines `vendor_unmodeled` to "An API error class this
// schema does not model". So a producer terminal with no arm of its own is
// `turn_failed` carrying its reason verbatim, never `vendor_unmodeled`.
//
// EXPECTED TO FAIL for every row: daemon/internal/resolve/feed/turnended.go
// (producerErrorArm) routes each of these to `unmodeledArm(<reason>)`
// instead, which puts a producer terminal into the API-class arm and leaves
// `turn_failed`'s "every other unclassified abnormal end" contract
// unimplemented. `prompt_too_long` is a second, distinct violation: the
// daemon draws it as `request_too_large`, whose proto comment (feed.proto:889-890)
// says "413 — the request exceeded the size limit", an API status this
// producer terminal never carried.
func TestTerminalReasonTurnFailedArms(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name     string
		scenario string
		reason   string
		message  string
	}{
		{
			name:     "BlockingLimit",
			scenario: "fail-blocking-limit",
			reason:   "blocking_limit",
			message:  "the account's blocking limit was reached",
		},
		{
			name:     "RapidRefillBreaker",
			scenario: "fail-rapid-refill",
			reason:   "rapid_refill_breaker",
			message:  "the rapid-refill breaker tripped",
		},
		{
			name:     "PromptTooLong",
			scenario: "fail-prompt-too-long",
			reason:   "prompt_too_long",
			message:  "the prompt exceeded the context window",
		},
		{
			name:     "ImageError",
			scenario: "fail-image",
			reason:   "image_error",
			message:  "an attached image was rejected",
		},
		{
			name:     "ModelError",
			scenario: "fail-model",
			reason:   "model_error",
			message:  "the requested model is unavailable",
		},
		{
			name:     "MalformedToolUseExhausted",
			scenario: "fail-malformed-tool-use",
			reason:   "malformed_tool_use_exhausted",
			message:  "the model produced malformed tool input on every retry",
		},
		{
			name:     "ToolDeferred",
			scenario: "fail-tool-deferred",
			reason:   "tool_deferred",
			message:  "the turn deferred a tool call to the host",
		},
		{
			name:     "ToolDeferredUnavailable",
			scenario: "fail-tool-deferred-unavailable",
			reason:   "tool_deferred_unavailable",
			message:  "the deferred tool is not available to this host",
		},
		{
			name:     "TurnSetupFailed",
			scenario: "fail-turn-setup",
			reason:   "turn_setup_failed",
			message:  "the turn could not be set up",
		},
		{
			name:     "HookStopped",
			scenario: "fail-hook-stopped",
			reason:   "hook_stopped",
			message:  "a hook stopped the turn",
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			t.Parallel()
			// Arrange
			w := NewWorld(t, WorldOpts{})
			ws := faNewWorkspace(t, w)

			// Act
			errored := faDriveErrored(t, w, ws, test.scenario)

			// Assert
			failed := errored.GetTurnFailed()
			if failed == nil {
				t.Fatalf("!%s: FeedTurnEndedErrored.Error = %T, want TurnFailed (feed.proto:925-927, every other unclassified abnormal end)", test.scenario, errored.GetError())
			}
			if got := failed.GetStopReason(); got != test.reason {
				t.Fatalf("!%s: FailureVendorTurnFailed.StopReason = %q, want the vendor's own word %q", test.scenario, got, test.reason)
			}
			faAssertMessage(t, errored, test.message)
		})
	}
}

// TestTurnStopContinuationPrevented drives `!fail-continuation-prevented`
// (failures.ts FAIL_CONTINUATION_PREVENTED), whose fake emits BOTH declared
// prevent-continuation signals — an `informational` message with
// `prevent_continuation: true` and a `system:stop_hook_summary` record whose
// `preventedContinuation` is true — beside a `stop_hook_prevented` terminal.
// The fake's own doc names that pairing as the arm it ACTUALLY reaches, so
// the drawn arm is feed.proto:928-930's
// "FeedTurnErrorStopHookPrevented stop_hook_prevented = 23 —
// AgentFailure.stop_hook_prevented ... deliberate, not a fault, but not
// completion". Ungrounded: the whole pairing is the fake's declaration; no
// capture reaches this terminal.
//
// This is the sibling of turnlifecycle's TestTurnStopHookStop, which drives
// `!fail-stop-hook`: same arm, reached from the informational-message signal
// rather than from the hook summary alone.
func TestTurnStopContinuationPrevented(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	ws := faNewWorkspace(t, w)

	// Act
	errored := faDriveErrored(t, w, ws, "fail-continuation-prevented")

	// Assert
	if errored.GetStopHookPrevented() == nil {
		t.Fatalf("FeedTurnEndedErrored.Error = %T, want StopHookPrevented (feed.proto:928-930)", errored.GetError())
	}
	faAssertMessage(t, errored, "continuation was prevented")
}

// TestTurnAbortedWhileToolsRunning drives `!fail-aborted-tools`, the one
// member of the stop family whose declared arm is NOT a failure:
// failures.ts states "AgentInterrupted.by_user, reached through the tools
// rather than the stream", and convert/terminals.ts folds both
// `aborted_streaming` and `aborted_tools` to `success.interrupted.by_user`.
// So the drawn terminal is feed.proto:853-854's
// "FeedTurnEndedInterrupted interrupted = 4 — The user stopped it", and
// specifically NOT an Errored row: an abort through the tools is the same
// user stop as an abort through the stream. Ungrounded — the emitted shape is
// the fake's declaration, not a capture.
func TestTurnAbortedWhileToolsRunning(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	ws := faNewWorkspace(t, w)

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "fail-aborted-tools")
	row := AwaitTurnEnded(t, w, ws, turn)

	// Assert
	ended := row.GetTurnEnded()
	if ended == nil {
		t.Fatalf("row for turn %s has no TurnEnded: %v", turn.GetValue(), row)
	}
	if ended.GetInterrupted() == nil {
		t.Fatalf("FeedTurnEnded.Outcome = %v, want Interrupted (feed.proto:853-854) for a tools-phase abort", ended)
	}
}

// ---------------------------------------------------------------------------
// Model refusal, with and without a configured fallback
// ---------------------------------------------------------------------------

// TestModelRefusalWithFallback drives `!refusal-fallback`, whose fake emits a
// `fallback` content block naming both models, a `model_refusal_fallback`
// message carrying the refusal category and the RETRACTED uuids, and then the
// answer from the fallback leg with a `success` result. The recovery is the
// whole fact under test: a refusal that was recovered from is a CONCLUDED
// turn whose answer is the fallback model's, so the assertion is the exact
// answering prose, not merely that the turn ended well. Ungrounded: model
// refusals are not reproducible on demand and no capture backs this shape.
//
// No frontend arm is asserted for the model swap itself: no message in
// frontend/v1 declares a fallback notice, so the proto requires no row here
// and inventing one would be a test of nothing.
func TestModelRefusalWithFallback(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	ws := faNewWorkspace(t, w)

	// Act
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "refusal-fallback")
	row := AwaitTurnEnded(t, w, ws, turn)

	// Assert
	ended := row.GetTurnEnded()
	if ended == nil {
		t.Fatalf("row for turn %s has no TurnEnded: %v", turn.GetValue(), row)
	}
	concluded := ended.GetConcluded()
	if concluded == nil {
		t.Fatalf("FeedTurnEnded.Outcome = %v, want Concluded — the fallback leg answered", ended)
	}
	const want = "Here is the answer from the fallback model."
	if got := tlResponseMarkdown(t, tlOpenRows(t, w, ws), concluded.GetAnswer()); got != want {
		t.Fatalf("answering response markdown = %q, want the fallback model's answer %q", got, want)
	}
}

// TestModelRefusalWithoutFallback drives `!refusal-no-fallback`, whose fake
// emits a `model_refusal_no_fallback` message with an EMPTY `content` and an
// explanation pointing the integrator at the fallback docs, then an error
// terminal. The fake's declared arm is "AgentResponseFailure.reason=refused
// with no recovery", and the proto's home for exactly that is
// feed.proto:900-901: "The vendor refused to continue; there is no answer
// prose. FeedTurnErrorRefusal refusal = 12". Ungrounded — declared, not
// captured.
//
// WRITTEN TO THE PROTO, EXPECTED TO FAIL: `refusal` has NO producer anywhere
// in the daemon (nothing in resolve/feed constructs
// FeedTurnEndedErrored_Refusal), so the proto's refusal arm is unreachable
// and the fake's `model_error` terminal reason is drawn as an unmodeled
// producer arm instead, which tells the reader nothing about a refusal.
func TestModelRefusalWithoutFallback(t *testing.T) {
	t.Parallel()
	// Arrange
	w := NewWorld(t, WorldOpts{})
	ws := faNewWorkspace(t, w)

	// Act
	errored := faDriveErrored(t, w, ws, "refusal-no-fallback")

	// Assert
	if errored.GetRefusal() == nil {
		t.Fatalf("FeedTurnEndedErrored.Error = %T, want Refusal (feed.proto:900-901)", errored.GetError())
	}
	faAssertMessage(t, errored, "the model refused and no fallback is configured")
}
