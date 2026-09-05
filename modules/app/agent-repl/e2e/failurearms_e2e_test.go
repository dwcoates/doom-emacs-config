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
// THREE SUBTESTS ARE WRITTEN TO THE PROTO AND ARE EXPECTED TO FAIL, because
// the arm the proto declares is unreachable from what the seam carries:
//
//   - billing: feed.proto:904-905 "The account cannot be charged; the user
//     must act on their billing. FeedTurnErrorBillingError billing_error =
//     14" — the fake declares ApiBillingError on status 402, but the shim's
//     result-seam classifier (convert/terminals.ts apiFailureKind) reaches
//     `billing_error` only from a vendor error STRING, and the result record
//     carries only the status, which 402 does not match.
//   - oauth-org: feed.proto:909-910 "The account's organization does not
//     allow this OAuth access. FeedTurnErrorOauthOrgNotAllowed
//     oauth_org_not_allowed = 16" — same cause; status 403 alone is
//     indistinguishable from an ordinary permission denial.
//   - max-output: feed.proto:911-913 "The request asked for more output
//     tokens than the model will produce ... FeedTurnErrorMaxOutputTokens
//     max_output_tokens = 17" — the fake states NO status at all for this
//     class, so the classifier has nothing to key on.
func TestApiRequestFailedArms(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name     string
		scenario string
		message  string
		arm      func(*frontendv1.FeedTurnEndedErrored) bool
		want     string
	}{
		{
			name:     "RateLimited",
			scenario: "api-429",
			message:  "Rate limited; retry after 30 seconds.",
			arm:      func(e *frontendv1.FeedTurnEndedErrored) bool { return e.GetRateLimited() != nil },
			want:     "RateLimited (feed.proto:879-880, 429)",
		},
		{
			name:     "Overloaded",
			scenario: "api-529",
			message:  "The API is overloaded.",
			arm:      func(e *frontendv1.FeedTurnEndedErrored) bool { return e.GetOverloaded() != nil },
			want:     "Overloaded (feed.proto:881-882, 529)",
		},
		{
			name:     "AuthenticationFailed",
			scenario: "api-401",
			message:  "Authentication failed.",
			arm:      func(e *frontendv1.FeedTurnEndedErrored) bool { return e.GetAuthenticationFailed() != nil },
			want:     "AuthenticationFailed (feed.proto:883-884, 401)",
		},
		{
			name:     "PermissionDenied",
			scenario: "api-403",
			message:  "Permission denied for this request.",
			arm:      func(e *frontendv1.FeedTurnEndedErrored) bool { return e.GetPermissionDenied() != nil },
			want:     "PermissionDenied (feed.proto:885-886, 403)",
		},
		{
			name:     "InvalidRequest",
			scenario: "api-400",
			message:  "The request was invalid.",
			arm:      func(e *frontendv1.FeedTurnEndedErrored) bool { return e.GetInvalidRequest() != nil },
			want:     "InvalidRequest (feed.proto:887-888, 400)",
		},
		{
			name:     "RequestTooLarge",
			scenario: "api-413",
			message:  "The request was too large.",
			arm:      func(e *frontendv1.FeedTurnEndedErrored) bool { return e.GetRequestTooLarge() != nil },
			want:     "RequestTooLarge (feed.proto:889-890, 413)",
		},
		{
			name:     "NotFound",
			scenario: "api-404",
			message:  "The requested model was not found.",
			arm:      func(e *frontendv1.FeedTurnEndedErrored) bool { return e.GetNotFound() != nil },
			want:     "NotFound (feed.proto:891-892, 404)",
		},
		{
			name:     "Internal",
			scenario: "api-500",
			message:  "The service raised.",
			arm:      func(e *frontendv1.FeedTurnEndedErrored) bool { return e.GetInternal() != nil },
			want:     "Internal (feed.proto:893-894, 500)",
		},
		{
			name:     "BillingError",
			scenario: "api-billing",
			message:  "The account has a billing problem.",
			arm:      func(e *frontendv1.FeedTurnEndedErrored) bool { return e.GetBillingError() != nil },
			want:     "BillingError (feed.proto:904-905, 402)",
		},
		{
			name:     "OauthOrgNotAllowed",
			scenario: "api-oauth-org",
			message:  "This organization is not permitted to use OAuth here.",
			arm:      func(e *frontendv1.FeedTurnEndedErrored) bool { return e.GetOauthOrgNotAllowed() != nil },
			want:     "OauthOrgNotAllowed (feed.proto:909-910)",
		},
		{
			name:     "MaxOutputTokens",
			scenario: "api-max-output",
			message:  "The response hit the max output tokens.",
			arm:      func(e *frontendv1.FeedTurnEndedErrored) bool { return e.GetMaxOutputTokens() != nil },
			want:     "MaxOutputTokens (feed.proto:911-913)",
		},
		{
			name:     "VendorUnmodeled",
			scenario: "api-unmodeled",
			message:  "An error class this build does not model.",
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
// WRITTEN TO THE PROTO, EXPECTED TO FAIL: convert/terminals.ts states
// outright that "`retry_after_ms` is UNSET from a result record ... carrying
// one across would be a join the fold does not make", so the seam drops the
// only wait the vendor stated and the client has nothing to count down from.
// This shape is the fake's declaration, not a capture.
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
