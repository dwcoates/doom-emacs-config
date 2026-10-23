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
		{name: "api_request_failed of an unstated kind", failure: apiFailed(&conversationv1.ApiRequestFailed{}), want: VendorFailed},
		{name: "api_request_failed: rate limited", failure: apiFailed(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_RateLimited{RateLimited: &conversationv1.ApiRateLimited{}}}), want: VendorBlocked},
		{name: "api_request_failed: authentication failed", failure: apiFailed(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_AuthenticationFailed{AuthenticationFailed: &conversationv1.ApiAuthenticationFailed{}}}), want: VendorBlocked},
		{name: "api_request_failed: permission denied", failure: apiFailed(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_PermissionDenied{PermissionDenied: &conversationv1.ApiPermissionDenied{}}}), want: VendorBlocked},
		{name: "api_request_failed: billing", failure: apiFailed(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_BillingError{BillingError: &conversationv1.ApiBillingError{}}}), want: VendorBlocked},
		{name: "api_request_failed: organization not allowed", failure: apiFailed(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_OauthOrgNotAllowed{OauthOrgNotAllowed: &conversationv1.ApiOauthOrgNotAllowed{}}}), want: VendorBlocked},
		{name: "api_request_failed: overloaded", failure: apiFailed(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_Overloaded{Overloaded: &conversationv1.ApiOverloaded{}}}), want: VendorFailed},
		{name: "api_request_failed: invalid request", failure: apiFailed(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_InvalidRequest{InvalidRequest: &conversationv1.ApiInvalidRequest{}}}), want: VendorFailed},
		{name: "api_request_failed: request too large", failure: apiFailed(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_RequestTooLarge{RequestTooLarge: &conversationv1.ApiRequestTooLarge{}}}), want: VendorFailed},
		{name: "api_request_failed: not found", failure: apiFailed(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_NotFound{NotFound: &conversationv1.ApiNotFound{}}}), want: VendorFailed},
		{name: "api_request_failed: internal", failure: apiFailed(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_Internal{Internal: &conversationv1.ApiInternal{}}}), want: VendorFailed},
		{name: "api_request_failed: max output tokens", failure: apiFailed(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_MaxOutputTokens{MaxOutputTokens: &conversationv1.ApiMaxOutputTokens{}}}), want: VendorFailed},
		{name: "api_request_failed: unreachable", failure: apiFailed(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_Unreachable{Unreachable: &conversationv1.ApiUnreachable{Code: "ECONNRESET"}}}), want: NetworkFailed},
		{name: "api_request_failed: unmodeled", failure: apiFailed(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_Unmodeled{Unmodeled: &conversationv1.ApiUnmodeledError{}}}), want: VendorFailed},
		{name: "blocking_limit", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_BlockingLimit{BlockingLimit: &conversationv1.AgentStoppedAtBlockingLimit{}}}, want: VendorBlocked},
		{name: "rapid_refill_breaker", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_RapidRefillBreaker{RapidRefillBreaker: &conversationv1.AgentStoppedByRapidRefillBreaker{}}}, want: VendorBlocked},
		{name: "model_error", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ModelError{ModelError: &conversationv1.AgentModelError{}}}, want: VendorFailed},
		{name: "prompt_too_long", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_PromptTooLong{PromptTooLong: &conversationv1.AgentPromptTooLong{}}}, want: VendorFailed},
		{name: "image_error", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ImageError{ImageError: &conversationv1.AgentImageRejected{}}}, want: VendorFailed},
		{name: "malformed_tool_use_exhausted", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_MalformedToolUseExhausted{MalformedToolUseExhausted: &conversationv1.AgentMalformedToolUseExhausted{}}}, want: VendorFailed},
		{name: "stop_hook_prevented", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_StopHookPrevented{StopHookPrevented: &conversationv1.AgentStoppedByStopHook{}}}, want: ExpectedStop},
		{name: "hook_stopped", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_HookStopped{HookStopped: &conversationv1.AgentStoppedByHook{}}}, want: VendorFailed},
		{name: "tool_deferred", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ToolDeferred{ToolDeferred: &conversationv1.AgentToolDeferred{}}}, want: ExpectedStop},
		{name: "tool_deferred_unavailable", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ToolDeferredUnavailable{ToolDeferredUnavailable: &conversationv1.AgentToolDeferredUnavailable{}}}, want: VendorFailed},
		{name: "max_turns", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_MaxTurns{MaxTurns: &conversationv1.AgentMaxTurnsReached{}}}, want: VendorFailed},
		{name: "budget_exhausted", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_BudgetExhausted{BudgetExhausted: &conversationv1.AgentBudgetExhausted{}}}, want: VendorFailed},
		{name: "structured_output_retry_exhausted", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_StructuredOutputRetryExhausted{StructuredOutputRetryExhausted: &conversationv1.AgentStructuredOutputRetriesExhausted{}}}, want: VendorFailed},
		{name: "turn_setup_failed", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_TurnSetupFailed{TurnSetupFailed: &conversationv1.AgentTurnSetupFailed{}}}, want: VendorFailed},
		{name: "execution_error", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ExecutionError{ExecutionError: &conversationv1.AgentExecutionError{}}}, want: VendorFailed},
		{name: "continuation_prevented", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ContinuationPrevented{ContinuationPrevented: &conversationv1.AgentContinuationPrevented{}}}, want: VendorFailed},
		{name: "lost", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_Lost{Lost: &conversationv1.DetachedLost{}}}, want: VendorFailed},
		{name: "query_died", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_QueryDied{QueryDied: &conversationv1.SessionQueryDied{}}}, want: AgentReplFailed},
		{name: "an arm this build does not know", failure: &conversationv1.AgentFailure{}, want: VendorFailed},
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

// rateLimitEvent is a rate-limit event about window with the given status
// arm (nil for a statusless event).
func rateLimitEvent(window *conversationv1.SessionRateLimitType, status any) *conversationv1.SessionRateLimitStatus {
	event := &conversationv1.SessionRateLimitStatus{RateLimitType: window}
	switch arm := status.(type) {
	case *conversationv1.SessionRateLimitStatus_Rejected:
		event.Status = arm
	case *conversationv1.SessionRateLimitStatus_Allowed:
		event.Status = arm
	case *conversationv1.SessionRateLimitStatus_AllowedWarning:
		event.Status = arm
	}
	return event
}

var (
	rejected = &conversationv1.SessionRateLimitStatus_Rejected{Rejected: &conversationv1.SessionRateLimitRejected{}}
	allowed  = &conversationv1.SessionRateLimitStatus_Allowed{Allowed: &conversationv1.SessionRateLimitAllowed{}}
	warning  = &conversationv1.SessionRateLimitStatus_AllowedWarning{AllowedWarning: &conversationv1.SessionRateLimitAllowedWarning{}}

	fiveHour = &conversationv1.SessionRateLimitType{Window: &conversationv1.SessionRateLimitType_FiveHour{
		FiveHour: &conversationv1.SessionRateLimitWindowFiveHour{}}}
	sevenDay = &conversationv1.SessionRateLimitType{Window: &conversationv1.SessionRateLimitType_SevenDay{
		SevenDay: &conversationv1.SessionRateLimitWindowSevenDay{}}}
	sevenDayOpus = &conversationv1.SessionRateLimitType{Window: &conversationv1.SessionRateLimitType_SevenDayOpus{
		SevenDayOpus: &conversationv1.SessionRateLimitWindowSevenDayOpus{}}}
)

func TestRateLimitRejections(t *testing.T) {
	cases := []struct {
		name   string
		events []*conversationv1.SessionRateLimitStatus
		want   bool
	}{
		{name: "an empty set does not block", want: false},
		{name: "a rejected verdict blocks",
			events: []*conversationv1.SessionRateLimitStatus{rateLimitEvent(sevenDay, rejected)}, want: true},
		{name: "an allowed verdict does not block",
			events: []*conversationv1.SessionRateLimitStatus{rateLimitEvent(sevenDay, allowed)}, want: false},
		{name: "a warning verdict does not block",
			events: []*conversationv1.SessionRateLimitStatus{rateLimitEvent(fiveHour, warning)}, want: false},
		{name: "no verdict does not block",
			events: []*conversationv1.SessionRateLimitStatus{rateLimitEvent(sevenDay, nil)}, want: false},
		{name: "an allowed verdict on the same window lifts the rejection",
			events: []*conversationv1.SessionRateLimitStatus{rateLimitEvent(sevenDay, rejected), rateLimitEvent(sevenDay, allowed)}, want: false},
		{name: "an allowed verdict on another window leaves the rejection standing",
			events: []*conversationv1.SessionRateLimitStatus{rateLimitEvent(sevenDay, rejected), rateLimitEvent(fiveHour, allowed)}, want: true},
		{name: "the opus window is a window of its own",
			events: []*conversationv1.SessionRateLimitStatus{rateLimitEvent(sevenDayOpus, rejected), rateLimitEvent(sevenDay, allowed)}, want: true},
		{name: "a windowless rejection is lifted only by a windowless verdict",
			events: []*conversationv1.SessionRateLimitStatus{rateLimitEvent(nil, rejected), rateLimitEvent(fiveHour, allowed)}, want: true},
		{name: "two rejected windows block until both are lifted",
			events: []*conversationv1.SessionRateLimitStatus{
				rateLimitEvent(sevenDay, rejected), rateLimitEvent(fiveHour, rejected), rateLimitEvent(sevenDay, allowed)}, want: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			var set RateLimitRejections
			got := set.Blocked()

			// Act
			for _, event := range tc.events {
				got = set.Observe(event)
			}

			// Assert
			if got != tc.want || set.Blocked() != tc.want {
				t.Fatalf("Observe = %v, Blocked = %v, want %v", got, set.Blocked(), tc.want)
			}
		})
	}
}

func TestRateLimitRejectionsClearLiftsEveryWindow(t *testing.T) {
	// Arrange
	var set RateLimitRejections
	set.Observe(rateLimitEvent(sevenDay, rejected))
	set.Observe(rateLimitEvent(fiveHour, rejected))

	// Act
	set.Clear()

	// Assert
	if set.Blocked() {
		t.Fatal("Blocked after Clear = true, want false")
	}
}

func TestRateLimitWindow(t *testing.T) {
	cases := []struct {
		name   string
		window *conversationv1.SessionRateLimitType
		want   string
	}{
		{name: "five hour", window: fiveHour, want: "five_hour"},
		{name: "seven day", window: sevenDay, want: "seven_day"},
		{name: "seven day opus", window: sevenDayOpus, want: "seven_day_opus"},
		{name: "seven day sonnet", window: &conversationv1.SessionRateLimitType{Window: &conversationv1.SessionRateLimitType_SevenDaySonnet{
			SevenDaySonnet: &conversationv1.SessionRateLimitWindowSevenDaySonnet{}}}, want: "seven_day_sonnet"},
		{name: "seven day overage included", window: &conversationv1.SessionRateLimitType{Window: &conversationv1.SessionRateLimitType_SevenDayOverageIncluded{
			SevenDayOverageIncluded: &conversationv1.SessionRateLimitWindowSevenDayOverageIncluded{}}}, want: "seven_day_overage_included"},
		{name: "overage", window: &conversationv1.SessionRateLimitType{Window: &conversationv1.SessionRateLimitType_Overage{
			Overage: &conversationv1.SessionRateLimitWindowOverage{}}}, want: "overage"},
		{name: "no window named", window: nil, want: "unset"},
		{name: "a type with no window set", window: &conversationv1.SessionRateLimitType{}, want: "unset"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := RateLimitWindow(&conversationv1.SessionRateLimitStatus{RateLimitType: tc.window})

			// Assert
			if got != tc.want {
				t.Fatalf("RateLimitWindow = %q, want %q", got, tc.want)
			}
		})
	}
}

// apiFailed wraps a failed api request as the turn-ending failure it reports.
func apiFailed(failed *conversationv1.ApiRequestFailed) *conversationv1.AgentFailure {
	return &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ApiRequestFailed{ApiRequestFailed: failed}}
}

func TestFailureClassNamesEveryClass(t *testing.T) {
	cases := []struct {
		class FailureClass
		want  string
	}{
		{class: NoFailure, want: "none"},
		{class: VendorBlocked, want: "vendor_blocked"},
		{class: VendorFailed, want: "vendor_failed"},
		{class: AgentReplFailed, want: "agent_repl_failed"},
		{class: NetworkFailed, want: "network_failed"},
		{class: ExpectedStop, want: "expected_stop"},
		{class: FailureClass(99), want: "unknown"},
	}
	for _, tc := range cases {
		t.Run(tc.want, func(t *testing.T) {
			// Act
			got := tc.class.String()

			// Assert
			if got != tc.want {
				t.Fatalf("String() = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestFailureCuredBySignIn(t *testing.T) {
	cases := []struct {
		name    string
		failure *conversationv1.AgentFailure
		want    bool
	}{
		{name: "no failure", failure: nil, want: false},
		{name: "authentication failed", failure: apiFailed(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_AuthenticationFailed{AuthenticationFailed: &conversationv1.ApiAuthenticationFailed{}}}), want: true},
		{name: "organization not allowed", failure: apiFailed(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_OauthOrgNotAllowed{OauthOrgNotAllowed: &conversationv1.ApiOauthOrgNotAllowed{}}}), want: true},
		{name: "permission denied blocks but is not an account cause", failure: apiFailed(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_PermissionDenied{PermissionDenied: &conversationv1.ApiPermissionDenied{}}}), want: false},
		{name: "rate limited", failure: apiFailed(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_RateLimited{RateLimited: &conversationv1.ApiRateLimited{}}}), want: false},
		{name: "api failure of an unstated kind", failure: apiFailed(&conversationv1.ApiRequestFailed{}), want: false},
		{name: "a non-api failure", failure: &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ModelError{ModelError: &conversationv1.AgentModelError{}}}, want: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := FailureCuredBySignIn(tc.failure)

			// Assert
			if got != tc.want {
				t.Fatalf("FailureCuredBySignIn = %v, want %v", got, tc.want)
			}
		})
	}
}
