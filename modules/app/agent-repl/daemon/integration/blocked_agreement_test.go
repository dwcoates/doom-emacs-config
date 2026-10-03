//go:build integration

package integration

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/integration/harness"
)

// TestRosterAndFooterAgreeOnBlocked is the executable form of the owner's
// rulings that the roster agrees with the footer (2026-09-14) and that
// vendor_blocked is ONLY for the vendor or the account (2026-09-28).
//
// The footer's `blocked` arm and the roster's `vendor_blocked` and
// `turn_failed` arms decide the same failure through ONE classifier
// (ladder.ClassifyFailure), so they cannot drift. This drives failures from
// each class through a real turn on the same workspace and asserts BOTH
// surfaces draw the class: a vendor or account refusal as `blocked` and
// `vendor_blocked`; the turn's own failure as `idle · turn_failed` and
// `turn_failed`; an expected stop as `idle · done` and `done`. The regression
// it first guarded is the one the 2026-09-14 ruling named: an
// authentication_failed failure the footer painted `blocked` used to fall
// through the roster's private allowlist to a green arm.
func TestRosterAndFooterAgreeOnBlocked(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name    string
		failure *conversationv1.AgentFailure
		class   string
	}{
		{"authentication_failed", &conversationv1.AgentFailure{
			Failure: &conversationv1.AgentFailure_ApiRequestFailed{ApiRequestFailed: &conversationv1.ApiRequestFailed{
				Kind: &conversationv1.ApiRequestFailed_AuthenticationFailed{
					AuthenticationFailed: &conversationv1.ApiAuthenticationFailed{}}}}}, "vendor"},
		{"oauth_org_not_allowed", &conversationv1.AgentFailure{
			Failure: &conversationv1.AgentFailure_ApiRequestFailed{ApiRequestFailed: &conversationv1.ApiRequestFailed{
				Kind: &conversationv1.ApiRequestFailed_OauthOrgNotAllowed{
					OauthOrgNotAllowed: &conversationv1.ApiOauthOrgNotAllowed{}}}}}, "vendor"},
		{"billing_error", &conversationv1.AgentFailure{
			Failure: &conversationv1.AgentFailure_ApiRequestFailed{ApiRequestFailed: &conversationv1.ApiRequestFailed{
				Kind: &conversationv1.ApiRequestFailed_BillingError{
					BillingError: &conversationv1.ApiBillingError{}}}}}, "vendor"},
		{"rate_limited", &conversationv1.AgentFailure{
			Failure: &conversationv1.AgentFailure_ApiRequestFailed{ApiRequestFailed: &conversationv1.ApiRequestFailed{
				Kind: &conversationv1.ApiRequestFailed_RateLimited{
					RateLimited: &conversationv1.ApiRateLimited{}}}}}, "vendor"},
		{"blocking_limit", &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_BlockingLimit{
			BlockingLimit: &conversationv1.AgentStoppedAtBlockingLimit{}}}, "vendor"},
		{"rapid_refill_breaker", &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_RapidRefillBreaker{
			RapidRefillBreaker: &conversationv1.AgentStoppedByRapidRefillBreaker{}}}, "vendor"},
		{"budget_exhausted", &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_BudgetExhausted{
			BudgetExhausted: &conversationv1.AgentBudgetExhausted{}}}, "turn"},
		{"max_turns", &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_MaxTurns{
			MaxTurns: &conversationv1.AgentMaxTurnsReached{}}}, "turn"},
		{"execution_error", &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ExecutionError{
			ExecutionError: &conversationv1.AgentExecutionError{}}}, "turn"},
		{"structured_output_retry_exhausted", &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_StructuredOutputRetryExhausted{
			StructuredOutputRetryExhausted: &conversationv1.AgentStructuredOutputRetriesExhausted{}}}, "turn"},
		{"model_error", &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ModelError{
			ModelError: &conversationv1.AgentModelError{}}}, "turn"},
		{"api_overloaded", &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ApiRequestFailed{
			ApiRequestFailed: &conversationv1.ApiRequestFailed{
				Kind: &conversationv1.ApiRequestFailed_Overloaded{
					Overloaded: &conversationv1.ApiOverloaded{}}}}}, "turn"},
		{"stop_hook_prevented", &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_StopHookPrevented{
			StopHookPrevented: &conversationv1.AgentStoppedByStopHook{}}}, "expected"},
		{"prompt_too_long", &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_PromptTooLong{
			PromptTooLong: &conversationv1.AgentPromptTooLong{}}}, "turn"},
		{"query_died", &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_QueryDied{
			QueryDied: &conversationv1.SessionQueryDied{}}}, "turn"},
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

			// Assert: both surfaces draw the SAME class for the same failure.
			switch tc.class {
			case "vendor":
				awaitFooter(t, f, footer, "the footer paints blocked", func(v *frontendv1.FooterView) bool {
					return v.GetStrip().GetStatus().GetVendorFault() != nil
				})
				awaitRoster(t, f.d, roster, "the roster paints vendor_blocked", func(r *frontendv1.WorkspaceRoster) bool {
					return rosterRow(r, f.ws.GetId()).GetVendorBlocked() != nil
				})
			case "turn":
				awaitFooter(t, f, footer, "the footer draws turn_failed", func(v *frontendv1.FooterView) bool {
					return v.GetStrip().GetStatus().GetTurnFailed() != nil
				})
				awaitRoster(t, f.d, roster, "the roster paints turn_failed", func(r *frontendv1.WorkspaceRoster) bool {
					return rosterRow(r, f.ws.GetId()).GetTurnFailed() != nil
				})
			case "expected":
				awaitFooter(t, f, footer, "the footer draws idle · done", func(v *frontendv1.FooterView) bool {
					return v.GetStrip().GetStatus().GetIdle().GetDone() != nil
				})
				awaitRoster(t, f.d, roster, "the roster paints done", func(r *frontendv1.WorkspaceRoster) bool {
					return rosterRow(r, f.ws.GetId()).GetDone() != nil
				})
			default:
				t.Fatalf("case %q names no class %q", tc.name, tc.class)
			}
		})
	}
}
