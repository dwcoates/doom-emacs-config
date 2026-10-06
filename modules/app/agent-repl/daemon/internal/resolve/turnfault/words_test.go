package turnfault

import (
	"strings"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/wsm"
)

func failureOf(arm any) *conversationv1.AgentFailure {
	f := &conversationv1.AgentFailure{}
	switch a := arm.(type) {
	case *conversationv1.AgentPromptTooLong:
		f.Failure = &conversationv1.AgentFailure_PromptTooLong{PromptTooLong: a}
	case *conversationv1.AgentStoppedAtBlockingLimit:
		f.Failure = &conversationv1.AgentFailure_BlockingLimit{BlockingLimit: a}
	case *conversationv1.AgentStoppedByRapidRefillBreaker:
		f.Failure = &conversationv1.AgentFailure_RapidRefillBreaker{RapidRefillBreaker: a}
	case *conversationv1.AgentImageRejected:
		f.Failure = &conversationv1.AgentFailure_ImageError{ImageError: a}
	case *conversationv1.AgentModelError:
		f.Failure = &conversationv1.AgentFailure_ModelError{ModelError: a}
	case *conversationv1.AgentMalformedToolUseExhausted:
		f.Failure = &conversationv1.AgentFailure_MalformedToolUseExhausted{MalformedToolUseExhausted: a}
	case *conversationv1.AgentStoppedByStopHook:
		f.Failure = &conversationv1.AgentFailure_StopHookPrevented{StopHookPrevented: a}
	case *conversationv1.AgentStoppedByHook:
		f.Failure = &conversationv1.AgentFailure_HookStopped{HookStopped: a}
	case *conversationv1.AgentToolDeferred:
		f.Failure = &conversationv1.AgentFailure_ToolDeferred{ToolDeferred: a}
	case *conversationv1.AgentToolDeferredUnavailable:
		f.Failure = &conversationv1.AgentFailure_ToolDeferredUnavailable{ToolDeferredUnavailable: a}
	case *conversationv1.AgentMaxTurnsReached:
		f.Failure = &conversationv1.AgentFailure_MaxTurns{MaxTurns: a}
	case *conversationv1.AgentBudgetExhausted:
		f.Failure = &conversationv1.AgentFailure_BudgetExhausted{BudgetExhausted: a}
	case *conversationv1.AgentStructuredOutputRetriesExhausted:
		f.Failure = &conversationv1.AgentFailure_StructuredOutputRetryExhausted{StructuredOutputRetryExhausted: a}
	case *conversationv1.AgentTurnSetupFailed:
		f.Failure = &conversationv1.AgentFailure_TurnSetupFailed{TurnSetupFailed: a}
	case *conversationv1.AgentExecutionError:
		f.Failure = &conversationv1.AgentFailure_ExecutionError{ExecutionError: a}
	case *conversationv1.AgentContinuationPrevented:
		f.Failure = &conversationv1.AgentFailure_ContinuationPrevented{ContinuationPrevented: a}
	case *conversationv1.ApiRequestFailed:
		f.Failure = &conversationv1.AgentFailure_ApiRequestFailed{ApiRequestFailed: a}
	case *conversationv1.SessionQueryDied:
		f.Failure = &conversationv1.AgentFailure_QueryDied{QueryDied: a}
	case *conversationv1.DetachedLost:
		f.Failure = &conversationv1.AgentFailure_Lost{Lost: a}
	}
	return f
}

func TestOfAgentFailureWordsEveryProducerArm(t *testing.T) {
	cases := []struct {
		name    string
		failure *conversationv1.AgentFailure
		refused bool
		want    Words
	}{
		{name: "prompt too long", failure: failureOf(&conversationv1.AgentPromptTooLong{}),
			want: Words{"the prompt was too long to send — the context must be cut first", "prompt_too_long", "prompt too long"}},
		{name: "blocking limit", failure: failureOf(&conversationv1.AgentStoppedAtBlockingLimit{}),
			want: Words{"an account-level block stopped the run", "blocking_limit", "blocking limit"}},
		{name: "rapid refill breaker", failure: failureOf(&conversationv1.AgentStoppedByRapidRefillBreaker{}),
			want: Words{"the account's refill-rate breaker tripped — this is a wait, not a fault", "rapid_refill_breaker", "rapid refill breaker"}},
		{name: "image error", failure: failureOf(&conversationv1.AgentImageRejected{}),
			want: Words{"an image in the request could not be processed", "image_error", "image error"}},
		{name: "model error", failure: failureOf(&conversationv1.AgentModelError{}),
			want: Words{"the model errored in a way the API did not classify", "model_error", "model error"}},
		{name: "a model error after a refused response is the refusal", failure: failureOf(&conversationv1.AgentModelError{}), refused: true,
			want: Words{"the model refused to continue — there is no answer", "refusal", "refusal"}},
		{name: "malformed tool use exhausted", failure: failureOf(&conversationv1.AgentMalformedToolUseExhausted{}),
			want: Words{"the model's tool calls could not be parsed and the attempts ran out", "malformed_tool_use_exhausted", "malformed tool use exhausted"}},
		{name: "stop hook prevented", failure: failureOf(&conversationv1.AgentStoppedByStopHook{}),
			want: Words{"a Stop hook ended the run", "stop_hook_prevented", "Stop hook ended the run"}},
		{name: "hook stopped", failure: failureOf(&conversationv1.AgentStoppedByHook{}),
			want: Words{"a hook ended the run", "hook_stopped", "hook stopped"}},
		{name: "tool deferred", failure: failureOf(&conversationv1.AgentToolDeferred{}),
			want: Words{"the run ended waiting on a deferred tool call", "tool_deferred", "tool deferred"}},
		{name: "tool deferred unavailable", failure: failureOf(&conversationv1.AgentToolDeferredUnavailable{}),
			want: Words{"the run ended on a tool call deferred to something unavailable", "tool_deferred_unavailable", "tool deferred unavailable"}},
		{name: "max turns", failure: failureOf(&conversationv1.AgentMaxTurnsReached{}),
			want: Words{"stopped at the turn limit", "max_turns", "max turns"}},
		{name: "budget exhausted", failure: failureOf(&conversationv1.AgentBudgetExhausted{}),
			want: Words{"stopped at the budget", "max_budget", "max budget"}},
		{name: "structured output retry exhausted", failure: failureOf(&conversationv1.AgentStructuredOutputRetriesExhausted{}),
			want: Words{"the run ended: structured_output_retry_exhausted", "structured_output_retry_exhausted", "structured output retry exhausted"}},
		{name: "turn setup failed", failure: failureOf(&conversationv1.AgentTurnSetupFailed{}),
			want: Words{"the run could not be set up and never reached the model", "turn_setup_failed", "turn setup failed"}},
		{name: "execution error", failure: failureOf(&conversationv1.AgentExecutionError{}),
			want: Words{"the run broke while executing", "execution_error", "execution error"}},
		{name: "continuation prevented", failure: failureOf(&conversationv1.AgentContinuationPrevented{}),
			want: Words{"a producer notice ended the run", "continuation_prevented", "continuation prevented"}},
		{name: "an arm this build does not know", failure: &conversationv1.AgentFailure{},
			want: Words{"the run ended on a failure with no stated cause", "unset", "unset"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := OfAgentFailure(tc.failure, tc.refused)

			// Assert
			if got != tc.want {
				t.Fatalf("OfAgentFailure = %+v, want %+v", got, tc.want)
			}
		})
	}
}

func TestOfAgentFailureWordsAnApiFailureByItsClass(t *testing.T) {
	// Arrange
	failure := failureOf(&conversationv1.ApiRequestFailed{
		Kind: &conversationv1.ApiRequestFailed_Overloaded{Overloaded: &conversationv1.ApiOverloaded{}},
	})

	// Act
	got := OfAgentFailure(failure, false)

	// Assert
	if got != (Words{"the vendor API is overloaded", "overloaded", "overloaded"}) {
		t.Fatalf("OfAgentFailure = %+v, want the overloaded words", got)
	}
}

func TestOfAgentFailureWordsAQueryDeathAsTheDeath(t *testing.T) {
	// Arrange
	failure := failureOf(&conversationv1.SessionQueryDied{
		Cause: &conversationv1.SessionQueryDied_UnexpectedEof{UnexpectedEof: &conversationv1.SessionQueryUnexpectedEof{}},
	})

	// Act
	got := OfAgentFailure(failure, false)

	// Assert
	if got.Cause != "query_died" || got.Detail != "query died" {
		t.Fatalf("OfAgentFailure = %+v, want the query death's words", got)
	}
}

func TestOfApiFailureWordsEveryClass(t *testing.T) {
	cases := []struct {
		name string
		kind any
		want Words
	}{
		{name: "rate limited", kind: &conversationv1.ApiRequestFailed_RateLimited{RateLimited: &conversationv1.ApiRateLimited{}},
			want: Words{"rate limited by the vendor", "rate_limited", "rate limited"}},
		{name: "overloaded", kind: &conversationv1.ApiRequestFailed_Overloaded{Overloaded: &conversationv1.ApiOverloaded{}},
			want: Words{"the vendor API is overloaded", "overloaded", "overloaded"}},
		{name: "authentication failed", kind: &conversationv1.ApiRequestFailed_AuthenticationFailed{AuthenticationFailed: &conversationv1.ApiAuthenticationFailed{}},
			want: Words{"the credential was rejected — sign in again", "authentication_failed", "authentication failed"}},
		{name: "permission denied", kind: &conversationv1.ApiRequestFailed_PermissionDenied{PermissionDenied: &conversationv1.ApiPermissionDenied{}},
			want: Words{"the credential lacks permission for this request", "permission_denied", "permission denied"}},
		{name: "invalid request", kind: &conversationv1.ApiRequestFailed_InvalidRequest{InvalidRequest: &conversationv1.ApiInvalidRequest{}},
			want: Words{"the vendor refused the request as malformed", "invalid_request", "invalid request"}},
		{name: "request too large", kind: &conversationv1.ApiRequestFailed_RequestTooLarge{RequestTooLarge: &conversationv1.ApiRequestTooLarge{}},
			want: Words{"the request exceeded the vendor's size limit", "request_too_large", "request too large"}},
		{name: "not found", kind: &conversationv1.ApiRequestFailed_NotFound{NotFound: &conversationv1.ApiNotFound{}},
			want: Words{"the model or resource does not exist", "not_found", "not found"}},
		{name: "internal", kind: &conversationv1.ApiRequestFailed_Internal{Internal: &conversationv1.ApiInternal{}},
			want: Words{"the vendor API hit its own internal error", "internal", "internal"}},
		{name: "billing", kind: &conversationv1.ApiRequestFailed_BillingError{BillingError: &conversationv1.ApiBillingError{}},
			want: Words{"the account could not be charged — check your billing", "billing_error", "billing error"}},
		{name: "organization not allowed", kind: &conversationv1.ApiRequestFailed_OauthOrgNotAllowed{OauthOrgNotAllowed: &conversationv1.ApiOauthOrgNotAllowed{}},
			want: Words{"your organization does not allow this OAuth access", "oauth_org_not_allowed", "oauth org not allowed"}},
		{name: "max output tokens", kind: &conversationv1.ApiRequestFailed_MaxOutputTokens{MaxOutputTokens: &conversationv1.ApiMaxOutputTokens{}},
			want: Words{"the request asked for more output than the model will produce", "max_output_tokens", "max output tokens"}},
		{name: "unmodeled keeps the vendor's class verbatim", kind: &conversationv1.ApiRequestFailed_Unmodeled{Unmodeled: &conversationv1.ApiUnmodeledError{Type: "quota_exceeded_v2"}},
			want: Words{"the vendor reported an error class we do not model yet", "quota_exceeded_v2", "quota_exceeded_v2"}},
		{name: "an unstated class", kind: nil,
			want: Words{"the vendor reported a failure with no stated class", "unset", "unset"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			failed := &conversationv1.ApiRequestFailed{}
			if tc.kind != nil {
				setKind(failed, tc.kind)
			}

			// Act
			got := OfApiFailure(failed)

			// Assert
			if got != tc.want {
				t.Fatalf("OfApiFailure = %+v, want %+v", got, tc.want)
			}
		})
	}
}

// setKind sets an ApiRequestFailed's kind arm from its generated wrapper.
func setKind(failed *conversationv1.ApiRequestFailed, kind any) {
	switch k := kind.(type) {
	case *conversationv1.ApiRequestFailed_RateLimited:
		failed.Kind = k
	case *conversationv1.ApiRequestFailed_Overloaded:
		failed.Kind = k
	case *conversationv1.ApiRequestFailed_AuthenticationFailed:
		failed.Kind = k
	case *conversationv1.ApiRequestFailed_PermissionDenied:
		failed.Kind = k
	case *conversationv1.ApiRequestFailed_InvalidRequest:
		failed.Kind = k
	case *conversationv1.ApiRequestFailed_RequestTooLarge:
		failed.Kind = k
	case *conversationv1.ApiRequestFailed_NotFound:
		failed.Kind = k
	case *conversationv1.ApiRequestFailed_Internal:
		failed.Kind = k
	case *conversationv1.ApiRequestFailed_BillingError:
		failed.Kind = k
	case *conversationv1.ApiRequestFailed_OauthOrgNotAllowed:
		failed.Kind = k
	case *conversationv1.ApiRequestFailed_MaxOutputTokens:
		failed.Kind = k
	case *conversationv1.ApiRequestFailed_Unmodeled:
		failed.Kind = k
	}
}

func TestOfQueryDeathWordsEachCause(t *testing.T) {
	cases := []struct {
		name string
		died *conversationv1.SessionQueryDied
		want string
	}{
		{name: "unexpected eof", died: &conversationv1.SessionQueryDied{Cause: &conversationv1.SessionQueryDied_UnexpectedEof{UnexpectedEof: &conversationv1.SessionQueryUnexpectedEof{}}},
			want: "the query died — the agent binary's stream ended without closing"},
		{name: "iterator failure", died: &conversationv1.SessionQueryDied{Cause: &conversationv1.SessionQueryDied_IteratorFailure{IteratorFailure: &conversationv1.SessionQueryIteratorFailure{Cause: "boom"}}},
			want: "the query died — the SDK's iterator threw"},
		{name: "no stated cause", died: &conversationv1.SessionQueryDied{},
			want: "the query died out from under the turn"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := OfQueryDeath(tc.died)

			// Assert
			if got != (Words{tc.want, "query_died", "query died"}) {
				t.Fatalf("OfQueryDeath = %+v, want sentence %q", got, tc.want)
			}
		})
	}
}

func TestQueryDeathThrownIsTheIteratorsCauseAlone(t *testing.T) {
	cases := []struct {
		name string
		died *conversationv1.SessionQueryDied
		want string
	}{
		{name: "an iterator failure answers what it threw", died: &conversationv1.SessionQueryDied{Cause: &conversationv1.SessionQueryDied_IteratorFailure{IteratorFailure: &conversationv1.SessionQueryIteratorFailure{Cause: "socket hang up"}}}, want: "socket hang up"},
		{name: "an eof threw nothing", died: &conversationv1.SessionQueryDied{Cause: &conversationv1.SessionQueryDied_UnexpectedEof{UnexpectedEof: &conversationv1.SessionQueryUnexpectedEof{}}}, want: ""},
		{name: "no stated cause threw nothing", died: &conversationv1.SessionQueryDied{}, want: ""},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := QueryDeathThrown(tc.died)

			// Assert
			if got != tc.want {
				t.Fatalf("QueryDeathThrown = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestALostRunSaysWeStoppedSeeingItRatherThanThatItFailed(t *testing.T) {
	cases := []struct {
		name      string
		lost      *conversationv1.DetachedLost
		wantCause string
	}{
		{name: "file vanished", lost: &conversationv1.DetachedLost{How: &conversationv1.DetachedLost_FileVanished{FileVanished: &conversationv1.DetachedLostFileVanished{}}}, wantCause: "lost:file_vanished"},
		{name: "went silent", lost: &conversationv1.DetachedLost{How: &conversationv1.DetachedLost_WentSilent{WentSilent: &conversationv1.DetachedLostWentSilent{}}}, wantCause: "lost:went_silent"},
		{name: "swept up", lost: &conversationv1.DetachedLost{How: &conversationv1.DetachedLost_SweptUp{SweptUp: &conversationv1.DetachedLostSweptUp{}}}, wantCause: "lost:swept_up"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := OfAgentFailure(failureOf(tc.lost), false)

			// Assert
			if !strings.Contains(got.Sentence, "we lost sight of this work") || strings.Contains(got.Sentence, "failed") {
				t.Fatalf("sentence = %q, want the lost wording and no claim of failure", got.Sentence)
			}
			if got.Cause != tc.wantCause {
				t.Fatalf("cause = %q, want %q", got.Cause, tc.wantCause)
			}
		})
	}
}

func TestALostArmNamingNoHowIsAnUnstatedFailure(t *testing.T) {
	// Act
	got := OfAgentFailure(failureOf(&conversationv1.DetachedLost{}), false)

	// Assert
	if got != (Words{"the run ended on a failure with no stated cause", "unset", "unset"}) {
		t.Fatalf("OfAgentFailure = %+v, want the unstated-cause words", got)
	}
}

func TestOfCloseWordsEveryFailingClose(t *testing.T) {
	cases := []struct {
		name string
		how  wsm.TurnClose
		want Words
	}{
		{name: "the agent process died", how: wsm.CloseAgentDied,
			want: Words{"the agent process died, and the turn it was running ended with it", "agent_process_died", "process died"}},
		{name: "an orphaned turn", how: wsm.CloseOrphaned,
			want: Words{"the turn was dropped: nothing saw it end", "closed:orphaned", "closed:orphaned"}},
		{name: "a failed close no terminal explained", how: wsm.CloseFailed,
			want: Words{"the turn failed with an error, and no account of the failure was recorded", "closed:failed", "closed:failed"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, ok := OfClose(tc.how)

			// Assert
			if !ok {
				t.Fatalf("OfClose(%s) reported no failure", tc.how)
			}
			if got != tc.want {
				t.Fatalf("OfClose(%s) = %+v, want %+v", tc.how, got, tc.want)
			}
		})
	}
}

func TestOfCloseWordsNothingForACloseThatIsNoFailure(t *testing.T) {
	cases := []struct {
		name string
		how  wsm.TurnClose
	}{
		{name: "a completion", how: wsm.CloseCompleted},
		{name: "an interrupt", how: wsm.CloseKilled},
		{name: "a folded prompt", how: wsm.CloseFolded},
		{name: "a close this build does not know", how: wsm.TurnClose(99)},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			_, ok := OfClose(tc.how)

			// Assert
			if ok {
				t.Fatalf("OfClose(%d) worded a close that is no failure", int(tc.how))
			}
		})
	}
}

func TestRetryAfterIsTheVendorsStatedWaitAlone(t *testing.T) {
	ms := int64(42_000)
	cases := []struct {
		name    string
		failure *conversationv1.AgentFailure
		want    time.Duration
		wantOK  bool
	}{
		{name: "a rate limit with a wait", failure: failureOf(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_RateLimited{RateLimited: &conversationv1.ApiRateLimited{RetryAfterMs: &ms}}}), want: 42 * time.Second, wantOK: true},
		{name: "an overload with a wait", failure: failureOf(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_Overloaded{Overloaded: &conversationv1.ApiOverloaded{RetryAfterMs: &ms}}}), want: 42 * time.Second, wantOK: true},
		{name: "a rate limit that stated no wait", failure: failureOf(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_RateLimited{RateLimited: &conversationv1.ApiRateLimited{}}}), wantOK: false},
		{name: "an overload that stated no wait", failure: failureOf(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_Overloaded{Overloaded: &conversationv1.ApiOverloaded{}}}), wantOK: false},
		{name: "another api class", failure: failureOf(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_Internal{Internal: &conversationv1.ApiInternal{}}}), wantOK: false},
		{name: "a producer failure", failure: failureOf(&conversationv1.AgentMaxTurnsReached{}), wantOK: false},
		{name: "no failure", failure: nil, wantOK: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, ok := RetryAfter(tc.failure)

			// Assert
			if ok != tc.wantOK || got != tc.want {
				t.Fatalf("RetryAfter = (%s, %v), want (%s, %v)", got, ok, tc.want, tc.wantOK)
			}
		})
	}
}

func TestRefusedResponseIsTheRefusalArmAlone(t *testing.T) {
	failed := func(reason *conversationv1.AgentResponseFailureReason) *conversationv1.AgentResponse {
		return &conversationv1.AgentResponse{Result: &conversationv1.AgentResponse_Failure{Failure: &conversationv1.AgentResponseFailure{Reason: reason}}}
	}
	cases := []struct {
		name     string
		response *conversationv1.AgentResponse
		want     bool
	}{
		{name: "a refused response", response: failed(&conversationv1.AgentResponseFailureReason{Reason: &conversationv1.AgentResponseFailureReason_Refused{Refused: &conversationv1.AgentResponseRefused{}}}), want: true},
		{name: "a response cut at the token ceiling", response: failed(&conversationv1.AgentResponseFailureReason{Reason: &conversationv1.AgentResponseFailureReason_MaxTokens{MaxTokens: &conversationv1.AgentResponseStoppedAtMaxTokens{}}}), want: false},
		{name: "a failure stating no reason", response: failed(nil), want: false},
		{name: "a response that succeeded", response: &conversationv1.AgentResponse{Result: &conversationv1.AgentResponse_Success{Success: &conversationv1.AgentResponseSuccess{}}}, want: false},
		{name: "no response", response: nil, want: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := RefusedResponse(tc.response)

			// Assert
			if got != tc.want {
				t.Fatalf("RefusedResponse = %v, want %v", got, tc.want)
			}
		})
	}
}
