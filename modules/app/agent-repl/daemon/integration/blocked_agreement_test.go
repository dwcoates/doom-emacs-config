//go:build integration

package integration

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/integration/harness"
)

// TestRosterAndFooterAgreeOnBlocked is the executable form of the owner's
// 2026-09-14 ruling: blocked is blue, and the roster agrees with the footer.
//
// The footer's `blocked` arm and the roster's `vendor_blocked` dot decide the
// same failure through ONE predicate (footer.FailureBlocks), so they cannot
// drift. This drives each failure in the footer's blocked partition through a
// real turn on the same workspace and asserts BOTH surfaces classify it as
// blocked — the strip as `blocked`, the dot as `vendor_blocked` (which
// render-colors paints blue on every surface). The regression it guards is the
// one the ruling named: an authentication_failed failure the footer painted
// `blocked` used to fall through the roster's private allowlist to a green arm.
func TestRosterAndFooterAgreeOnBlocked(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name    string
		failure *conversationv1.AgentFailure
	}{
		{"authentication_failed", &conversationv1.AgentFailure{
			Failure: &conversationv1.AgentFailure_ApiRequestFailed{ApiRequestFailed: &conversationv1.ApiRequestFailed{
				Kind: &conversationv1.ApiRequestFailed_AuthenticationFailed{
					AuthenticationFailed: &conversationv1.ApiAuthenticationFailed{}}}}}},
		{"oauth_org_not_allowed", &conversationv1.AgentFailure{
			Failure: &conversationv1.AgentFailure_ApiRequestFailed{ApiRequestFailed: &conversationv1.ApiRequestFailed{
				Kind: &conversationv1.ApiRequestFailed_OauthOrgNotAllowed{
					OauthOrgNotAllowed: &conversationv1.ApiOauthOrgNotAllowed{}}}}}},
		{"billing_error", &conversationv1.AgentFailure{
			Failure: &conversationv1.AgentFailure_ApiRequestFailed{ApiRequestFailed: &conversationv1.ApiRequestFailed{
				Kind: &conversationv1.ApiRequestFailed_BillingError{
					BillingError: &conversationv1.ApiBillingError{}}}}}},
		{"rate_limited", &conversationv1.AgentFailure{
			Failure: &conversationv1.AgentFailure_ApiRequestFailed{ApiRequestFailed: &conversationv1.ApiRequestFailed{
				Kind: &conversationv1.ApiRequestFailed_RateLimited{
					RateLimited: &conversationv1.ApiRateLimited{}}}}}},
		{"blocking_limit", &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_BlockingLimit{
			BlockingLimit: &conversationv1.AgentStoppedAtBlockingLimit{}}}},
		{"rapid_refill_breaker", &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_RapidRefillBreaker{
			RapidRefillBreaker: &conversationv1.AgentStoppedByRapidRefillBreaker{}}}},
		{"budget_exhausted", &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_BudgetExhausted{
			BudgetExhausted: &conversationv1.AgentBudgetExhausted{}}}},
		{"max_turns", &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_MaxTurns{
			MaxTurns: &conversationv1.AgentMaxTurnsReached{}}}},
		{"execution_error", &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ExecutionError{
			ExecutionError: &conversationv1.AgentExecutionError{}}}},
		{"structured_output_retry_exhausted", &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_StructuredOutputRetryExhausted{
			StructuredOutputRetryExhausted: &conversationv1.AgentStructuredOutputRetriesExhausted{}}}},
		{"model_error", &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ModelError{
			ModelError: &conversationv1.AgentModelError{}}}},
		{"stop_hook_prevented", &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_StopHookPrevented{
			StopHookPrevented: &conversationv1.AgentStoppedByStopHook{}}}},
		{"prompt_too_long", &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_PromptTooLong{
			PromptTooLong: &conversationv1.AgentPromptTooLong{}}}},
		{"query_died", &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_QueryDied{
			QueryDied: &conversationv1.SessionQueryDied{}}}},
	}

	for _, tc := range tests {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			// Arrange: one opened workspace, watched on both surfaces.
			f := newOpened(t, harness.Opts{})
			footer := f.d.WatchFooter(f.ws)
			roster := f.d.WatchRoster()

			// Act: run a turn and end it with this failure.
			f.submit("go", "k-"+tc.name, conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
			f.shim.PushAgentFrame(mainAgent, failureFrame(mainAgent, tc.failure))

			// Assert: the footer paints blocked and the roster paints
			// vendor_blocked for the SAME failure.
			awaitFooter(t, f, footer, "the footer paints blocked", func(v *frontendv1.FooterView) bool {
				return v.GetStrip().GetStatus().GetBlocked() != nil
			})
			awaitRoster(t, f.d, roster, "the roster paints vendor_blocked", func(r *frontendv1.WorkspaceRoster) bool {
				return rosterRow(r, f.ws.GetId()).GetVendorBlocked() != nil
			})
		})
	}
}
