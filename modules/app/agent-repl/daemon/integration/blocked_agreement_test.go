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
// The footer's `vendor_fault` and `agent_repl_fault · turn_died` and the
// roster's `vendor_blocked` and `turn_died` decide the same failure through
// ONE classifier (ladder.ClassifyFailure, ladder.ResolveTurnFault), so they
// cannot drift. This drives failures from each class through a real turn on
// the same workspace and asserts BOTH surfaces draw the class: a vendor or
// account refusal as `vendor_fault` and `vendor_blocked`; every other cause
// the vendor ended (owner ruling, 2026-10-06) as `vendor_fault · vendor_error`
// and `vendor_blocked`; the query dying as `agent_repl_fault · turn_died` and
// `turn_died`; an expected stop as `idle · done` and `done`. The regression
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
			BudgetExhausted: &conversationv1.AgentBudgetExhausted{}}}, "vendor_turn"},
		{"max_turns", &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_MaxTurns{
			MaxTurns: &conversationv1.AgentMaxTurnsReached{}}}, "vendor_turn"},
		{"execution_error", &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ExecutionError{
			ExecutionError: &conversationv1.AgentExecutionError{}}}, "vendor_turn"},
		{"structured_output_retry_exhausted", &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_StructuredOutputRetryExhausted{
			StructuredOutputRetryExhausted: &conversationv1.AgentStructuredOutputRetriesExhausted{}}}, "vendor_turn"},
		{"model_error", &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ModelError{
			ModelError: &conversationv1.AgentModelError{}}}, "vendor_turn"},
		{"api_overloaded", &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ApiRequestFailed{
			ApiRequestFailed: &conversationv1.ApiRequestFailed{
				Kind: &conversationv1.ApiRequestFailed_Overloaded{
					Overloaded: &conversationv1.ApiOverloaded{}}}}}, "vendor_turn"},
		{"stop_hook_prevented", &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_StopHookPrevented{
			StopHookPrevented: &conversationv1.AgentStoppedByStopHook{}}}, "expected"},
		{"prompt_too_long", &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_PromptTooLong{
			PromptTooLong: &conversationv1.AgentPromptTooLong{}}}, "vendor_turn"},
		{"query_died", &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_QueryDied{
			QueryDied: &conversationv1.SessionQueryDied{}}}, "agent_repl"},
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
			case "vendor_turn":
				// A TURN THE VENDOR ENDED IS A VENDOR FAULT (owner ruling,
				// 2026-10-06): vendor_fault · vendor_error and vendor_blocked.
				awaitFooter(t, f, footer, "the footer draws vendor_fault · vendor_error", func(v *frontendv1.FooterView) bool {
					return v.GetStrip().GetStatus().GetVendorFault().GetVendorError() != nil
				})
				awaitRoster(t, f.d, roster, "the roster paints vendor_blocked", func(r *frontendv1.WorkspaceRoster) bool {
					return rosterRow(r, f.ws.GetId()).GetVendorBlocked() != nil
				})
			case "agent_repl":
				// A TURN THE QUERY'S DEATH ENDED IS AN AGENT-REPL FAULT (owner
				// ruling, 2026-10-06): agent_repl_fault · turn_died and turn_died.
				awaitFooter(t, f, footer, "the footer draws agent_repl_fault · turn_died", func(v *frontendv1.FooterView) bool {
					return v.GetStrip().GetStatus().GetAgentReplFault().GetTurnDied() != nil
				})
				awaitRoster(t, f.d, roster, "the roster paints turn_died", func(r *frontendv1.WorkspaceRoster) bool {
					return rosterRow(r, f.ws.GetId()).GetTurnDied() != nil
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

// TestRosterLeavesApiRetryingWithTheFooterWhenTheVendorAnswers holds the
// recovery edge on both surfaces: a turn whose call the vendor retried (the
// connection dropped) stands `api_retrying` on the roster and `vendor_fault ·
// api_retrying` on the footer, and the vendor answering the retried call (the
// connection back) returns BOTH to the running turn, with the turn still in
// flight. The roster was never handed the answer before, and stood teal after
// every reconnect until the turn ended.
func TestRosterLeavesApiRetryingWithTheFooterWhenTheVendorAnswers(t *testing.T) {
	t.Parallel()
	// Arrange: a running turn whose call the vendor is retrying.
	f := newOpened(t, harness.Opts{})
	// The sweep covers every test; the declared record is the vendor failure the test feeds, stated once by its owner.
	f.d.ExpectWarnings("daemon.sessionwatcher.api_error")
	footer := f.d.WatchFooter(f.ws)
	roster := f.d.WatchRoster()
	f.submit("go", "k-api-retry-recovers", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_ApiError{ApiError: &conversationv1.ApiRequestFailed{
			Message: "Connection lost while your computer was asleep",
		}},
	}))
	awaitFooter(t, f, footer, "the footer draws vendor_fault · api_retrying", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetVendorFault().GetApiRetrying() != nil
	})
	awaitRoster(t, f.d, roster, "the roster paints api_retrying", func(r *frontendv1.WorkspaceRoster) bool {
		return rosterRow(r, f.ws.GetId()).GetApiRetrying() != nil
	})

	// Act: the retried call answers.
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("think-reconnected"),
		Item:       &conversationv1.AgentActivity_Thinking{Thinking: &conversationv1.AgentThinking{Result: &conversationv1.AgentThinking_Start{Start: &conversationv1.AgentThinkingStart{}}}},
	}))

	// Assert: both surfaces are back on the running turn.
	awaitFooter(t, f, footer, "the footer is working again", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetWorking() != nil
	})
	awaitRoster(t, f.d, roster, "the roster is thinking again", func(r *frontendv1.WorkspaceRoster) bool {
		return rosterRow(r, f.ws.GetId()).GetThinking() != nil
	})
}
