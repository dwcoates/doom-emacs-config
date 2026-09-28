package ladder

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

func TestClassifyFailurePlacesEveryAgentFailureArm(t *testing.T) {
	cases := []struct {
		name    string
		failure *conversationv1.AgentFailure
		want    FailureClass
	}{
		{name: "no failure", failure: nil, want: NoFailure},
		{name: "api_request_failed of an unstated kind", failure: apiFailed(&conversationv1.ApiRequestFailed{}), want: TurnFailed},
		{name: "api_request_failed: rate limited", failure: apiFailed(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_RateLimited{RateLimited: &conversationv1.ApiRateLimited{}}}), want: VendorBlocked},
		{name: "api_request_failed: authentication failed", failure: apiFailed(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_AuthenticationFailed{AuthenticationFailed: &conversationv1.ApiAuthenticationFailed{}}}), want: VendorBlocked},
		{name: "api_request_failed: permission denied", failure: apiFailed(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_PermissionDenied{PermissionDenied: &conversationv1.ApiPermissionDenied{}}}), want: VendorBlocked},
		{name: "api_request_failed: billing", failure: apiFailed(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_BillingError{BillingError: &conversationv1.ApiBillingError{}}}), want: VendorBlocked},
		{name: "api_request_failed: organization not allowed", failure: apiFailed(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_OauthOrgNotAllowed{OauthOrgNotAllowed: &conversationv1.ApiOauthOrgNotAllowed{}}}), want: VendorBlocked},
		{name: "api_request_failed: overloaded", failure: apiFailed(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_Overloaded{Overloaded: &conversationv1.ApiOverloaded{}}}), want: TurnFailed},
		{name: "api_request_failed: invalid request", failure: apiFailed(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_InvalidRequest{InvalidRequest: &conversationv1.ApiInvalidRequest{}}}), want: TurnFailed},
		{name: "api_request_failed: request too large", failure: apiFailed(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_RequestTooLarge{RequestTooLarge: &conversationv1.ApiRequestTooLarge{}}}), want: TurnFailed},
		{name: "api_request_failed: not found", failure: apiFailed(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_NotFound{NotFound: &conversationv1.ApiNotFound{}}}), want: TurnFailed},
		{name: "api_request_failed: internal", failure: apiFailed(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_Internal{Internal: &conversationv1.ApiInternal{}}}), want: TurnFailed},
		{name: "api_request_failed: max output tokens", failure: apiFailed(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_MaxOutputTokens{MaxOutputTokens: &conversationv1.ApiMaxOutputTokens{}}}), want: TurnFailed},
		{name: "api_request_failed: unmodeled", failure: apiFailed(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_Unmodeled{Unmodeled: &conversationv1.ApiUnmodeledError{}}}), want: TurnFailed},
		{name: "blocking_limit", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_BlockingLimit{BlockingLimit: &conversationv1.AgentStoppedAtBlockingLimit{}}}, want: VendorBlocked},
		{name: "rapid_refill_breaker", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_RapidRefillBreaker{RapidRefillBreaker: &conversationv1.AgentStoppedByRapidRefillBreaker{}}}, want: VendorBlocked},
		{name: "model_error", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ModelError{ModelError: &conversationv1.AgentModelError{}}}, want: TurnFailed},
		{name: "prompt_too_long", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_PromptTooLong{PromptTooLong: &conversationv1.AgentPromptTooLong{}}}, want: TurnFailed},
		{name: "image_error", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ImageError{ImageError: &conversationv1.AgentImageRejected{}}}, want: TurnFailed},
		{name: "malformed_tool_use_exhausted", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_MalformedToolUseExhausted{MalformedToolUseExhausted: &conversationv1.AgentMalformedToolUseExhausted{}}}, want: TurnFailed},
		{name: "stop_hook_prevented", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_StopHookPrevented{StopHookPrevented: &conversationv1.AgentStoppedByStopHook{}}}, want: ExpectedStop},
		{name: "hook_stopped", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_HookStopped{HookStopped: &conversationv1.AgentStoppedByHook{}}}, want: TurnFailed},
		{name: "tool_deferred", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ToolDeferred{ToolDeferred: &conversationv1.AgentToolDeferred{}}}, want: ExpectedStop},
		{name: "tool_deferred_unavailable", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ToolDeferredUnavailable{ToolDeferredUnavailable: &conversationv1.AgentToolDeferredUnavailable{}}}, want: TurnFailed},
		{name: "max_turns", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_MaxTurns{MaxTurns: &conversationv1.AgentMaxTurnsReached{}}}, want: TurnFailed},
		{name: "budget_exhausted", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_BudgetExhausted{BudgetExhausted: &conversationv1.AgentBudgetExhausted{}}}, want: TurnFailed},
		{name: "structured_output_retry_exhausted", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_StructuredOutputRetryExhausted{StructuredOutputRetryExhausted: &conversationv1.AgentStructuredOutputRetriesExhausted{}}}, want: TurnFailed},
		{name: "turn_setup_failed", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_TurnSetupFailed{TurnSetupFailed: &conversationv1.AgentTurnSetupFailed{}}}, want: TurnFailed},
		{name: "execution_error", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ExecutionError{ExecutionError: &conversationv1.AgentExecutionError{}}}, want: TurnFailed},
		{name: "continuation_prevented", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ContinuationPrevented{ContinuationPrevented: &conversationv1.AgentContinuationPrevented{}}}, want: TurnFailed},
		{name: "lost", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_Lost{Lost: &conversationv1.DetachedLost{}}}, want: TurnFailed},
		{name: "query_died", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_QueryDied{QueryDied: &conversationv1.SessionQueryDied{}}}, want: TurnFailed},
		{name: "an arm this build does not know", failure: &conversationv1.AgentFailure{}, want: TurnFailed},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := ClassifyFailure(tc.failure)

			// Assert
			if got != tc.want {
				t.Fatalf("ClassifyFailure = %s, want %s", got, tc.want)
			}
		})
	}
}

func TestEveryAgentFailureArmIsClassifiedByTheTable(t *testing.T) {
	// Arrange: the arms the generated oneof declares, so an arm landing in the
	// proto is walked here the day it lands; it must classify as SOMETHING
	// other than no-failure.
	oneof := (&conversationv1.AgentFailure{}).ProtoReflect().Descriptor().Oneofs().ByName("failure")
	fields := oneof.Fields()
	for i := 0; i < fields.Len(); i++ {
		field := fields.Get(i)
		t.Run(string(field.Name()), func(t *testing.T) {
			failure := &conversationv1.AgentFailure{}
			m := failure.ProtoReflect()
			m.Set(field, m.NewField(field))

			// Act
			got := ClassifyFailure(failure)

			// Assert
			if got == NoFailure {
				t.Fatalf("ClassifyFailure(%s) = no failure, want a class", field.Name())
			}
		})
	}
}

func TestRateLimitBlocks(t *testing.T) {
	cases := []struct {
		name   string
		status *conversationv1.SessionRateLimitStatus
		want   bool
	}{
		{name: "a rejected verdict blocks", status: &conversationv1.SessionRateLimitStatus{
			Status: &conversationv1.SessionRateLimitStatus_Rejected{Rejected: &conversationv1.SessionRateLimitRejected{}}}, want: true},
		{name: "an allowed verdict does not", status: &conversationv1.SessionRateLimitStatus{
			Status: &conversationv1.SessionRateLimitStatus_Allowed{Allowed: &conversationv1.SessionRateLimitAllowed{}}}},
		{name: "no verdict does not", status: &conversationv1.SessionRateLimitStatus{}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := RateLimitBlocks(tc.status)

			// Assert
			if got != tc.want {
				t.Fatalf("RateLimitBlocks = %v, want %v", got, tc.want)
			}
		})
	}
}

// apiFailed wraps a failed api request as the turn-ending failure it reports.
func apiFailed(failed *conversationv1.ApiRequestFailed) *conversationv1.AgentFailure {
	return &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ApiRequestFailed{ApiRequestFailed: failed}}
}
