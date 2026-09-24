package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
)

// THE TERMINAL ROW is the liveness anchor: its ABSENCE for the current turn is
// what "the turn is live" means. Every arm carries a daemon-composed headline,
// because the client holds no per-arm sentence table.

// terminal sends one agent terminal for a turn.
func (h *harness) terminal(turn string, success *conversationv1.AgentSuccess, failure *conversationv1.AgentFailure) {
	h.t.Helper()
	id := ids.TurnID(turn)
	h.resolver.OnAgentTerminal(testWorkspace, mainAgent(), &id, success, failure, noAddress())
}

// terminalRow finds the terminal row for a turn.
// queryDied hands the resolver one session-level query death.
func (h *harness) queryDied(died *conversationv1.SessionQueryDied) {
	h.t.Helper()
	h.resolver.OnSessionUpdate(testWorkspace, &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_QueryDied{QueryDied: died},
	})
}

func (h *harness) terminalRow(turn string) *frontendv1.FeedTurnEnded {
	h.t.Helper()
	want := testEncode(feedid.Ref{
		WS: testWorkspace, Feed: rootFeed(),
		Row: feedid.RowKey{Kind: feedid.KindTurnEnded, ID: turn},
	}).GetValue()
	for _, row := range h.rows(rootFeed()) {
		if row.GetId().GetValue() == want {
			return row.GetTurnEnded()
		}
	}
	h.t.Fatalf("no terminal row for turn %q", turn)
	return nil
}

// interruptedByUser is the success terminal a stop — the footer's stop button,
// or a /clear cutting the turn — produces on the wire.
func interruptedByUser() *conversationv1.AgentSuccess {
	return &conversationv1.AgentSuccess{
		Outcome: &conversationv1.AgentSuccess_Interrupted{Interrupted: &conversationv1.AgentInterrupted{
			Cause: &conversationv1.AgentInterrupted_ByUser{ByUser: &conversationv1.AgentInterruptedByUser{}},
		}},
	}
}

// hasTerminalRow reports whether a terminal row was drawn for a turn.
func (h *harness) hasTerminalRow(turn string) bool {
	h.t.Helper()
	want := testEncode(feedid.Ref{
		WS: testWorkspace, Feed: rootFeed(),
		Row: feedid.RowKey{Kind: feedid.KindTurnEnded, ID: turn},
	}).GetValue()
	for _, row := range h.rows(rootFeed()) {
		if row.GetId().GetValue() == want {
			return true
		}
	}
	return false
}

// A /clear's interrupt is the cut, not a bubble. Once the clear is confirmed by
// its ContextCut, the turn's interrupted terminal draws NO "response cut short"
// card below the divider.
func TestAConfirmedClearTurnDrawsNoTerminalBubble(t *testing.T) {
	// Arrange: a confirmed clear.
	h := newHarness(t)
	h.resolver.OnClearReceived(testWorkspace, ids.TurnID("turn-2"))
	h.resolver.OnTurnOpened(testWorkspace, ids.TurnID("turn-2"))
	h.resolver.OnContextCut(testWorkspace, mainAgent(),
		&conversationv1.ContextCut{Cut: &conversationv1.ContextCut_Cleared{Cleared: &conversationv1.ContextCleared{}}},
		&conversationv1.HistoryPointer{Value: "entry-clear"}, nil, noAddress())

	// Act: the interrupted terminal the clear left arrives.
	h.terminal("turn-2", interruptedByUser(), nil)

	// Assert: no terminal bubble below the divider.
	if h.hasTerminalRow("turn-2") {
		t.Fatal("a confirmed /clear drew a terminal bubble; the divider is its only outcome")
	}
}

// A USER STOP IS NOT A CLEAR. A turn the user stopped with no context cut still
// draws its "interrupted" terminal — the suppression is for clears alone.
func TestAUserStopWithoutAClearStillDrawsInterrupted(t *testing.T) {
	// Arrange: an ordinary running turn — no clear.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")

	// Act: the user stops it.
	h.terminal("turn-1", interruptedByUser(), nil)

	// Assert: the stop is drawn as the interrupted terminal it is.
	if h.terminalRow("turn-1").GetInterrupted() == nil {
		t.Fatalf("outcome = %T, want interrupted", h.terminalRow("turn-1").GetOutcome())
	}
}

// A CLEAR THAT FAILED SURFACES ITS FAILURE. When a clear turn ends with a
// failure and no cut ever confirmed it, the optimistic bar is retired and the
// errored terminal is drawn — the failure is never swallowed.
func TestAFailedClearRetiresItsBarAndDrawsTheError(t *testing.T) {
	// Arrange: the optimistic bar is up and the clear turn is running.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.resolver.OnClearReceived(testWorkspace, ids.TurnID("turn-2"))
	h.resolver.OnTurnOpened(testWorkspace, ids.TurnID("turn-2"))

	// Act: the turn ends on a failure, with no cut ever delivered.
	h.terminal("turn-2", nil, &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_ExecutionError{ExecutionError: &conversationv1.AgentExecutionError{}},
	})

	// Assert: the phantom bar is gone and the failure is drawn.
	if got := len(h.separationRows()); got != 0 {
		t.Fatalf("separation rows = %d, want the phantom bar retired on a failed clear", got)
	}
	if h.terminalRow("turn-2").GetErrored() == nil {
		t.Fatalf("outcome = %T, want the failure surfaced", h.terminalRow("turn-2").GetOutcome())
	}
}

func TestAConcludedTurnNamesItsAnsweringRow(t *testing.T) {
	// Arrange: a turn whose prose the producer named as the answer.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "what is it")
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseSuccessActivity("unit-9", "it is this"), nil, noAddress())

	// Act.
	h.terminal("turn-1", &conversationv1.AgentSuccess{
		Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{
			Answer: &conversationv1.AgentActivityId{Value: "unit-9"},
		}},
	}, nil)

	// Assert: finality is named by the producer, never derived from position.
	concluded := h.terminalRow("turn-1").GetConcluded()
	if concluded == nil {
		t.Fatalf("outcome = %T, want concluded", h.terminalRow("turn-1").GetOutcome())
	}
	wantAnswer := testEncode(feedid.Ref{
		WS: testWorkspace, Feed: rootFeed(),
		Row: feedid.RowKey{Kind: feedid.KindActivity, ID: "unit-9"},
	}).GetValue()
	if concluded.GetAnswer().GetValue() != wantAnswer {
		t.Fatalf("answer = %q, want the response row", concluded.GetAnswer().GetValue())
	}
}

func TestATurnThatSaidNothingConcludesWithNoAnswer(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.terminal("turn-1", &conversationv1.AgentSuccess{
		Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{}},
	}, nil)

	// Assert: absence draws no final-answer border anywhere.
	if h.terminalRow("turn-1").GetConcluded().GetAnswer() != nil {
		t.Fatal("a turn with no prose named an answering row")
	}
}

func TestAnAcknowledgedStopIsInterruptedAndNotAFailure(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.terminal("turn-1", &conversationv1.AgentSuccess{
		Outcome: &conversationv1.AgentSuccess_Interrupted{Interrupted: &conversationv1.AgentInterrupted{
			Cause: &conversationv1.AgentInterrupted_ByUser{ByUser: &conversationv1.AgentInterruptedByUser{}},
		}},
	}, nil)

	// Assert.
	if h.terminalRow("turn-1").GetInterrupted() == nil {
		t.Fatalf("outcome = %T, want interrupted", h.terminalRow("turn-1").GetOutcome())
	}
}

func TestABackgroundedStreamStillConcludedTheTurn(t *testing.T) {
	// Arrange, Act: the stream ended while the work did not — what was asked
	// for, so a success.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.terminal("turn-1", &conversationv1.AgentSuccess{
		Outcome: &conversationv1.AgentSuccess_Backgrounded{Backgrounded: &conversationv1.AgentBackgrounded{}},
	}, nil)

	// Assert.
	if h.terminalRow("turn-1").GetConcluded() == nil {
		t.Fatalf("outcome = %T, want concluded", h.terminalRow("turn-1").GetOutcome())
	}
}

func TestTheVendorsErrorTaxonomyIsRespelledArmForArm(t *testing.T) {
	tests := []struct {
		name string
		kind any
		want string
	}{
		{name: "429", kind: &conversationv1.ApiRateLimited{}, want: "rate_limited"},
		{name: "529", kind: &conversationv1.ApiOverloaded{}, want: "overloaded"},
		{name: "401", kind: &conversationv1.ApiAuthenticationFailed{}, want: "authentication_failed"},
		{name: "403", kind: &conversationv1.ApiPermissionDenied{}, want: "permission_denied"},
		{name: "400", kind: &conversationv1.ApiInvalidRequest{}, want: "invalid_request"},
		{name: "413", kind: &conversationv1.ApiRequestTooLarge{}, want: "request_too_large"},
		{name: "404", kind: &conversationv1.ApiNotFound{}, want: "not_found"},
		{name: "500", kind: &conversationv1.ApiInternal{}, want: "internal"},
		{name: "billing", kind: &conversationv1.ApiBillingError{}, want: "billing_error"},
		{name: "oauth org", kind: &conversationv1.ApiOauthOrgNotAllowed{}, want: "oauth_org_not_allowed"},
		{name: "max output tokens", kind: &conversationv1.ApiMaxOutputTokens{}, want: "max_output_tokens"},
		{name: "a class we do not model is kept by name", kind: &conversationv1.ApiUnmodeledError{Type: "teapot_error"}, want: "vendor_unmodeled"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.deliverPrompt("turn-1", "hello")
			failed := &conversationv1.ApiRequestFailed{Message: "the vendor said so"}
			switch k := tc.kind.(type) {
			case *conversationv1.ApiRateLimited:
				failed.Kind = &conversationv1.ApiRequestFailed_RateLimited{RateLimited: k}
			case *conversationv1.ApiOverloaded:
				failed.Kind = &conversationv1.ApiRequestFailed_Overloaded{Overloaded: k}
			case *conversationv1.ApiAuthenticationFailed:
				failed.Kind = &conversationv1.ApiRequestFailed_AuthenticationFailed{AuthenticationFailed: k}
			case *conversationv1.ApiPermissionDenied:
				failed.Kind = &conversationv1.ApiRequestFailed_PermissionDenied{PermissionDenied: k}
			case *conversationv1.ApiInvalidRequest:
				failed.Kind = &conversationv1.ApiRequestFailed_InvalidRequest{InvalidRequest: k}
			case *conversationv1.ApiRequestTooLarge:
				failed.Kind = &conversationv1.ApiRequestFailed_RequestTooLarge{RequestTooLarge: k}
			case *conversationv1.ApiNotFound:
				failed.Kind = &conversationv1.ApiRequestFailed_NotFound{NotFound: k}
			case *conversationv1.ApiInternal:
				failed.Kind = &conversationv1.ApiRequestFailed_Internal{Internal: k}
			case *conversationv1.ApiBillingError:
				failed.Kind = &conversationv1.ApiRequestFailed_BillingError{BillingError: k}
			case *conversationv1.ApiOauthOrgNotAllowed:
				failed.Kind = &conversationv1.ApiRequestFailed_OauthOrgNotAllowed{OauthOrgNotAllowed: k}
			case *conversationv1.ApiMaxOutputTokens:
				failed.Kind = &conversationv1.ApiRequestFailed_MaxOutputTokens{MaxOutputTokens: k}
			case *conversationv1.ApiUnmodeledError:
				failed.Kind = &conversationv1.ApiRequestFailed_Unmodeled{Unmodeled: k}
			}

			// Act.
			h.terminal("turn-1", nil, &conversationv1.AgentFailure{
				Failure: &conversationv1.AgentFailure_ApiRequestFailed{ApiRequestFailed: failed},
			})

			// Assert.
			errored := h.terminalRow("turn-1").GetErrored()
			if got := erroredArmWord(errored); got != tc.want {
				t.Fatalf("arm = %q, want %q", got, tc.want)
			}
		})
	}
}

// erroredArmWord names an errored terminal's cause arm.
func erroredArmWord(errored *frontendv1.FeedTurnEndedErrored) string {
	switch errored.GetError().(type) {
	case *frontendv1.FeedTurnEndedErrored_RateLimited:
		return "rate_limited"
	case *frontendv1.FeedTurnEndedErrored_Overloaded:
		return "overloaded"
	case *frontendv1.FeedTurnEndedErrored_AuthenticationFailed:
		return "authentication_failed"
	case *frontendv1.FeedTurnEndedErrored_PermissionDenied:
		return "permission_denied"
	case *frontendv1.FeedTurnEndedErrored_InvalidRequest:
		return "invalid_request"
	case *frontendv1.FeedTurnEndedErrored_RequestTooLarge:
		return "request_too_large"
	case *frontendv1.FeedTurnEndedErrored_NotFound:
		return "not_found"
	case *frontendv1.FeedTurnEndedErrored_Internal:
		return "internal"
	case *frontendv1.FeedTurnEndedErrored_BillingError:
		return "billing_error"
	case *frontendv1.FeedTurnEndedErrored_OauthOrgNotAllowed:
		return "oauth_org_not_allowed"
	case *frontendv1.FeedTurnEndedErrored_MaxOutputTokens:
		return "max_output_tokens"
	case *frontendv1.FeedTurnEndedErrored_VendorUnmodeled:
		return "vendor_unmodeled"
	case *frontendv1.FeedTurnEndedErrored_MaxTokens:
		return "max_tokens"
	case *frontendv1.FeedTurnEndedErrored_Refusal:
		return "refusal"
	case *frontendv1.FeedTurnEndedErrored_QueryDied:
		return "query_died"
	case *frontendv1.FeedTurnEndedErrored_ModelNotFound:
		return "model_not_found"
	case *frontendv1.FeedTurnEndedErrored_MaxTurns:
		return "max_turns"
	case *frontendv1.FeedTurnEndedErrored_MaxBudget:
		return "max_budget"
	case *frontendv1.FeedTurnEndedErrored_ExecutionError:
		return "execution_error"
	case *frontendv1.FeedTurnEndedErrored_TurnFailed:
		return "turn_failed"
	case *frontendv1.FeedTurnEndedErrored_StopHookPrevented:
		return "stop_hook_prevented"
	case *frontendv1.FeedTurnEndedErrored_AgentProcessDied:
		return "agent_process_died"
	}
	return "unset"
}

func TestEveryErroredArmCarriesADaemonComposedHeadline(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.terminal("turn-1", nil, &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_ApiRequestFailed{
			ApiRequestFailed: &conversationv1.ApiRequestFailed{
				Message: "429 Too Many Requests",
				Kind: &conversationv1.ApiRequestFailed_RateLimited{
					RateLimited: &conversationv1.ApiRateLimited{},
				},
			},
		},
	})

	// Assert: the headline is OURS, and the vendor's sentence stays beside it.
	errored := h.terminalRow("turn-1").GetErrored()
	if got := errored.GetHeadline().GetText(); got != "rate limited by the vendor" {
		t.Fatalf("headline = %q", got)
	}
	if got := errored.GetMessage().GetText(); got != "429 Too Many Requests" {
		t.Fatalf("vendor message = %q, want it kept beside the headline", got)
	}
}

func TestARateLimitCarriesTheVendorsWaitWhenItGaveOne(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	wait := int64(30_000)

	// Act.
	h.terminal("turn-1", nil, &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_ApiRequestFailed{
			ApiRequestFailed: &conversationv1.ApiRequestFailed{
				Kind: &conversationv1.ApiRequestFailed_RateLimited{
					RateLimited: &conversationv1.ApiRateLimited{RetryAfterMs: &wait},
				},
			},
		},
	})

	// Assert: only the countdown is client-ticked.
	got := h.terminalRow("turn-1").GetErrored().GetRateLimited()
	if got.RetryAfterMs == nil || got.GetRetryAfterMs() != 30_000 {
		t.Fatalf("retry_after_ms = %+v, want the vendor's wait", got.RetryAfterMs)
	}
}

func TestARateLimitWithNoStatedWaitCarriesNone(t *testing.T) {
	// Arrange, Act: saying nothing is different from "retry now".
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.terminal("turn-1", nil, &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_ApiRequestFailed{
			ApiRequestFailed: &conversationv1.ApiRequestFailed{
				Kind: &conversationv1.ApiRequestFailed_RateLimited{
					RateLimited: &conversationv1.ApiRateLimited{},
				},
			},
		},
	})

	// Assert.
	if h.terminalRow("turn-1").GetErrored().GetRateLimited().RetryAfterMs != nil {
		t.Fatal("a rate limit with no stated wait carried one")
	}
}

// A PRODUCER TERMINAL WITH NO DEDICATED ARM lands under turn_failed carrying
// the vendor's own word — never under vendor_unmodeled, which feed.proto
// confines to "an API error class this schema does not model".
func TestEveryUnclassifiedProducerTerminalDrawsTurnFailedWithItsStopReason(t *testing.T) {
	tests := []struct {
		name     string
		failure  *conversationv1.AgentFailure
		reason   string
		headline string
	}{
		{
			name:     "blocking limit",
			failure:  &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_BlockingLimit{BlockingLimit: &conversationv1.AgentStoppedAtBlockingLimit{}}},
			reason:   "blocking_limit",
			headline: "an account-level block stopped the run",
		},
		{
			name:     "rapid refill breaker",
			failure:  &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_RapidRefillBreaker{RapidRefillBreaker: &conversationv1.AgentStoppedByRapidRefillBreaker{}}},
			reason:   "rapid_refill_breaker",
			headline: "the account's refill-rate breaker tripped — this is a wait, not a fault",
		},
		{
			name:     "prompt too long",
			failure:  &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_PromptTooLong{PromptTooLong: &conversationv1.AgentPromptTooLong{}}},
			reason:   "prompt_too_long",
			headline: "the prompt was too long to send — the context must be cut first",
		},
		{
			name:     "image error",
			failure:  &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ImageError{ImageError: &conversationv1.AgentImageRejected{}}},
			reason:   "image_error",
			headline: "an image in the request could not be processed",
		},
		{
			name:     "model error",
			failure:  &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ModelError{ModelError: &conversationv1.AgentModelError{}}},
			reason:   "model_error",
			headline: "the model errored in a way the API did not classify",
		},
		{
			name:     "malformed tool use exhausted",
			failure:  &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_MalformedToolUseExhausted{MalformedToolUseExhausted: &conversationv1.AgentMalformedToolUseExhausted{}}},
			reason:   "malformed_tool_use_exhausted",
			headline: "the model's tool calls could not be parsed and the attempts ran out",
		},
		{
			name:     "hook stopped",
			failure:  &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_HookStopped{HookStopped: &conversationv1.AgentStoppedByHook{}}},
			reason:   "hook_stopped",
			headline: "a hook ended the run",
		},
		{
			name:     "tool deferred",
			failure:  &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ToolDeferred{ToolDeferred: &conversationv1.AgentToolDeferred{}}},
			reason:   "tool_deferred",
			headline: "the run ended waiting on a deferred tool call",
		},
		{
			name:     "tool deferred unavailable",
			failure:  &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ToolDeferredUnavailable{ToolDeferredUnavailable: &conversationv1.AgentToolDeferredUnavailable{}}},
			reason:   "tool_deferred_unavailable",
			headline: "the run ended on a tool call deferred to something unavailable",
		},
		{
			name:     "turn setup failed",
			failure:  &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_TurnSetupFailed{TurnSetupFailed: &conversationv1.AgentTurnSetupFailed{}}},
			reason:   "turn_setup_failed",
			headline: "the run could not be set up and never reached the model",
		},
		{
			name:     "continuation prevented",
			failure:  &conversationv1.AgentFailure{Failure: &conversationv1.AgentFailure_ContinuationPrevented{ContinuationPrevented: &conversationv1.AgentContinuationPrevented{}}},
			reason:   "continuation_prevented",
			headline: "a producer notice ended the run",
		},
		{
			name:     "no stated cause",
			failure:  &conversationv1.AgentFailure{},
			reason:   "unset",
			headline: "the run ended on a failure with no stated cause",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.deliverPrompt("turn-1", "hello")

			// Act.
			h.terminal("turn-1", nil, tc.failure)

			// Assert.
			errored := h.terminalRow("turn-1").GetErrored()
			if got := erroredArmWord(errored); got != "turn_failed" {
				t.Fatalf("arm = %q, want turn_failed", got)
			}
			if got := errored.GetTurnFailed().GetStopReason(); got != tc.reason {
				t.Fatalf("stop_reason = %q, want the vendor's own word %q", got, tc.reason)
			}
			if got := errored.GetHeadline().GetText(); got != tc.headline {
				t.Fatalf("headline = %q, want %q", got, tc.headline)
			}
		})
	}
}

// PROMPT-TOO-LONG IS NOT A 413. request_too_large is the vendor's own API
// status, which this producer terminal never carried.
func TestPromptTooLongIsNotDrawnAsTheApiRequestTooLarge(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")

	// Act.
	h.terminal("turn-1", nil, &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_PromptTooLong{PromptTooLong: &conversationv1.AgentPromptTooLong{}},
	})

	// Assert.
	if h.terminalRow("turn-1").GetErrored().GetRequestTooLarge() != nil {
		t.Fatal("a producer prompt_too_long was drawn as the API's request_too_large")
	}
}

// THE REFUSAL ARM. AgentModelError is an empty message, so the refusal is
// drawn from the response frame that stated it.
func TestAModelErrorAfterARefusedResponseDrawsTheRefusalArm(t *testing.T) {
	// Arrange: a response the vendor refused, then the run's model_error end.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseFailure{
			Prose: &conversationv1.AgentResponseProse{},
			Reason: &conversationv1.AgentResponseFailureReason{
				Reason: &conversationv1.AgentResponseFailureReason_Refused{
					Refused: &conversationv1.AgentResponseRefused{},
				},
			},
		}, nil), nil, noAddress())

	// Act.
	h.terminal("turn-1", nil, &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_ModelError{ModelError: &conversationv1.AgentModelError{}},
	})

	// Assert.
	errored := h.terminalRow("turn-1").GetErrored()
	if errored.GetRefusal() == nil {
		t.Fatalf("arm = %q, want refusal", erroredArmWord(errored))
	}
	if got := errored.GetHeadline().GetText(); got != "the model refused to continue — there is no answer" {
		t.Fatalf("headline = %q", got)
	}
}

// A NON-REFUSAL response failure leaves the terminal alone: only `refused`
// carries the refusal fact.
func TestAModelErrorAfterAMaxTokensResponseIsNotARefusal(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseFailure{
			Prose: &conversationv1.AgentResponseProse{Markdown: "half an ans"},
			Reason: &conversationv1.AgentResponseFailureReason{
				Reason: &conversationv1.AgentResponseFailureReason_MaxTokens{
					MaxTokens: &conversationv1.AgentResponseStoppedAtMaxTokens{},
				},
			},
		}, nil), nil, noAddress())

	// Act.
	h.terminal("turn-1", nil, &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_ModelError{ModelError: &conversationv1.AgentModelError{}},
	})

	// Assert.
	errored := h.terminalRow("turn-1").GetErrored()
	if errored.GetRefusal() != nil {
		t.Fatal("a max-tokens stop was drawn as a refusal")
	}
	if got := errored.GetTurnFailed().GetStopReason(); got != "model_error" {
		t.Fatalf("stop_reason = %q, want model_error", got)
	}
}

// THE REFUSAL DOES NOT OUTLIVE ITS TURN: the next turn's model_error is its
// own unclassified end.
func TestARefusalDoesNotColorTheNextTurnsModelError(t *testing.T) {
	// Arrange: turn-1 refuses and ends.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "do the thing")
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseFrame("unit-1", &conversationv1.AgentResponseFailure{
			Prose: &conversationv1.AgentResponseProse{},
			Reason: &conversationv1.AgentResponseFailureReason{
				Reason: &conversationv1.AgentResponseFailureReason_Refused{
					Refused: &conversationv1.AgentResponseRefused{},
				},
			},
		}, nil), nil, noAddress())
	h.terminal("turn-1", nil, &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_ModelError{ModelError: &conversationv1.AgentModelError{}},
	})

	// Act: a second turn ends the same way, having refused nothing.
	h.deliverPrompt("turn-2", "again")
	h.terminal("turn-2", nil, &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_ModelError{ModelError: &conversationv1.AgentModelError{}},
	})

	// Assert.
	errored := h.terminalRow("turn-2").GetErrored()
	if errored.GetRefusal() != nil {
		t.Fatal("turn-1's refusal colored turn-2's terminal")
	}
	if got := errored.GetTurnFailed().GetStopReason(); got != "model_error" {
		t.Fatalf("stop_reason = %q, want model_error", got)
	}
}

// A STOP HOOK gets its own arm, not turn_failed.
func TestAStopHookPreventedDrawsItsOwnArm(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.terminal("turn-1", nil, &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_StopHookPrevented{StopHookPrevented: &conversationv1.AgentStoppedByStopHook{}},
	})

	// Assert.
	errored := h.terminalRow("turn-1").GetErrored()
	if errored.GetStopHookPrevented() == nil {
		t.Fatalf("arm = %q, want stop_hook_prevented", erroredArmWord(errored))
	}
}

// AN ABORT IS THE USER'S STOP, whichever phase it reached the producer in:
// the turn ended INTERRUPTED, never errored.
func TestAnAbortEndsTheTurnInterruptedRatherThanErrored(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.terminal("turn-1", &conversationv1.AgentSuccess{
		Outcome: &conversationv1.AgentSuccess_Interrupted{
			Interrupted: &conversationv1.AgentInterrupted{},
		},
	}, nil)

	// Assert.
	ended := h.terminalRow("turn-1")
	if ended.GetInterrupted() == nil {
		t.Fatalf("outcome = %T, want interrupted", ended.GetOutcome())
	}
}

// THE RUN'S OWN TERMINALS (landing 8): five arms that had no wire path to any
// frontend stream before, each with its own drawn arm and composed headline.
func TestEachRunTerminalDrawsItsOwnArm(t *testing.T) {
	tests := []struct {
		name    string
		failure *conversationv1.AgentFailure
		wantArm string
	}{
		{"max_turns", maxTurnsFailure(), "max_turns"},
		{"budget_exhausted", budgetExhaustedFailure(), "max_budget"},
		{"execution_error", executionErrorFailure(), "execution_error"},
		{"structured_output_retry_exhausted", structuredOutputFailure(), "turn_failed"},
		{"stop_hook_prevented", stopHookFailure(), "stop_hook_prevented"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			h := newHarness(t)
			h.deliverPrompt("turn-1", "hello")
			h.terminal("turn-1", nil, tc.failure)

			// Assert.
			if got := erroredArmWord(h.terminalRow("turn-1").GetErrored()); got != tc.wantArm {
				t.Fatalf("arm = %q, want %q", got, tc.wantArm)
			}
		})
	}
}

func TestEachRunTerminalComposesItsOwnHeadline(t *testing.T) {
	tests := []struct {
		name    string
		failure *conversationv1.AgentFailure
		want    string
	}{
		{"max_turns", maxTurnsFailure(), "stopped at the turn limit"},
		{"budget_exhausted", budgetExhaustedFailure(), "stopped at the budget"},
		{"execution_error", executionErrorFailure(), "the run broke while executing"},
		{"structured_output_retry_exhausted", structuredOutputFailure(), "the run ended: structured_output_retry_exhausted"},
		{"stop_hook_prevented", stopHookFailure(), "a Stop hook ended the run"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			h := newHarness(t)
			h.deliverPrompt("turn-1", "hello")
			h.terminal("turn-1", nil, tc.failure)

			// Assert.
			if got := h.terminalRow("turn-1").GetErrored().GetHeadline().GetText(); got != tc.want {
				t.Fatalf("headline = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestEachVendorRunTerminalCarriesItsVendorContext(t *testing.T) {
	tests := []struct {
		name   string
		vendor func(*frontendv1.FeedTurnEndedErrored) *frontendv1.VendorFailureContext
		fail   *conversationv1.AgentFailure
	}{
		{"max_turns", func(e *frontendv1.FeedTurnEndedErrored) *frontendv1.VendorFailureContext {
			return e.GetMaxTurns().GetVendor()
		}, maxTurnsFailure()},
		{"max_budget", func(e *frontendv1.FeedTurnEndedErrored) *frontendv1.VendorFailureContext {
			return e.GetMaxBudget().GetVendor()
		}, budgetExhaustedFailure()},
		{"execution_error", func(e *frontendv1.FeedTurnEndedErrored) *frontendv1.VendorFailureContext {
			return e.GetExecutionError().GetVendor()
		}, executionErrorFailure()},
		{"turn_failed", func(e *frontendv1.FeedTurnEndedErrored) *frontendv1.VendorFailureContext {
			return e.GetTurnFailed().GetVendor()
		}, structuredOutputFailure()},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			h := newHarness(t)
			h.deliverPrompt("turn-1", "hello")
			h.terminal("turn-1", nil, tc.fail)

			// Assert: present, so the arm is whole on the wire.
			if tc.vendor(h.terminalRow("turn-1").GetErrored()) == nil {
				t.Fatal("the arm carried no VendorFailureContext")
			}
		})
	}
}

func TestATurnFailedNamesTheVendorsOwnStopReason(t *testing.T) {
	// Arrange, Act: the arm exists exactly where the vendor named no kind.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.terminal("turn-1", nil, structuredOutputFailure())

	// Assert.
	got := h.terminalRow("turn-1").GetErrored().GetTurnFailed().GetStopReason()
	if got != "structured_output_retry_exhausted" {
		t.Fatalf("stop_reason = %q", got)
	}
}

func TestARunTerminalCarriesTheVendorsWordingWhenRecorded(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	failure := maxTurnsFailure()
	failure.Errors = []string{"reached 12 turns"}
	h.terminal("turn-1", nil, failure)

	// Assert.
	if got := h.terminalRow("turn-1").GetErrored().GetMessage().GetText(); got != "reached 12 turns" {
		t.Fatalf("message = %q, want the vendor's wording", got)
	}
}

func TestARunTerminalCarriesNoMessageWhenTheVendorRecordedNone(t *testing.T) {
	// Arrange, Act: an empty message would be a sentinel for a state.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.terminal("turn-1", nil, maxTurnsFailure())

	// Assert.
	if got := h.terminalRow("turn-1").GetErrored().Message; got != nil {
		t.Fatalf("message = %+v, want UNSET", got)
	}
}

func maxTurnsFailure() *conversationv1.AgentFailure {
	return &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_MaxTurns{MaxTurns: &conversationv1.AgentMaxTurnsReached{}},
	}
}

func budgetExhaustedFailure() *conversationv1.AgentFailure {
	return &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_BudgetExhausted{
			BudgetExhausted: &conversationv1.AgentBudgetExhausted{}},
	}
}

func executionErrorFailure() *conversationv1.AgentFailure {
	return &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_ExecutionError{
			ExecutionError: &conversationv1.AgentExecutionError{}},
	}
}

func structuredOutputFailure() *conversationv1.AgentFailure {
	return &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_StructuredOutputRetryExhausted{
			StructuredOutputRetryExhausted: &conversationv1.AgentStructuredOutputRetriesExhausted{}},
	}
}

func stopHookFailure() *conversationv1.AgentFailure {
	return &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_StopHookPrevented{
			StopHookPrevented: &conversationv1.AgentStoppedByStopHook{}},
	}
}

// A LOST AGENT TERMINAL IS `turn_failed`, never `vendor_unmodeled`: the latter
// is confined to API error classes, while the producer's own "lost" vocabulary
// is an unclassified abnormal end whose stop_reason names the cause.
func TestALostAgentSaysWeStoppedSeeingItRatherThanThatItFailed(t *testing.T) {
	tests := []struct {
		name     string
		lost     *conversationv1.DetachedLost
		reason   string
		headline string
	}{
		{
			name:     "the transcript vanished from disk",
			lost:     &conversationv1.DetachedLost{How: &conversationv1.DetachedLost_FileVanished{FileVanished: &conversationv1.DetachedLostFileVanished{}}},
			reason:   "lost:file_vanished",
			headline: "its transcript disappeared from disk",
		},
		{
			name:     "the run went silent",
			lost:     &conversationv1.DetachedLost{How: &conversationv1.DetachedLost_WentSilent{WentSilent: &conversationv1.DetachedLostWentSilent{}}},
			reason:   "lost:went_silent",
			headline: "it went silent past the reader's ruling",
		},
		{
			name:     "a boot sweep found it open",
			lost:     &conversationv1.DetachedLost{How: &conversationv1.DetachedLost_SweptUp{SweptUp: &conversationv1.DetachedLostSweptUp{}}},
			reason:   "lost:swept_up",
			headline: "a boot sweep found it open with no living producer",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.deliverPrompt("turn-1", "hello")

			// Act
			h.terminal("turn-1", nil, &conversationv1.AgentFailure{
				Failure: &conversationv1.AgentFailure_Lost{Lost: tc.lost},
			})

			// Assert
			errored := h.terminalRow("turn-1").GetErrored()
			if got := erroredArmWord(errored); got != "turn_failed" {
				t.Fatalf("arm = %q, want turn_failed: lost is not an unmodeled API error class", got)
			}
			if got := errored.GetTurnFailed().GetStopReason(); got != tc.reason {
				t.Fatalf("stop reason = %q, want %q", got, tc.reason)
			}
			if !contains(errored.GetHeadline().GetText(), tc.headline) {
				t.Fatalf("headline = %q, want the lost wording %q", errored.GetHeadline().GetText(), tc.headline)
			}
		})
	}
}

func TestAMidTurnApiErrorRidesTheTerminalsHeadlineAsEvidence(t *testing.T) {
	// Arrange: a request that failed mid-turn and the turn went on.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.resolver.OnApiError(testWorkspace, mainAgent(), &conversationv1.ApiRequestFailed{
		Message: "connection reset",
	}, nil, noAddress())

	// Act: the turn then dies of something else.
	h.terminal("turn-1", nil, &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_ExecutionError{
			ExecutionError: &conversationv1.AgentExecutionError{},
		},
	})

	// Assert: EVIDENCE, never a terminal of its own.
	headline := h.terminalRow("turn-1").GetErrored().GetHeadline().GetText()
	if !contains(headline, "connection reset") {
		t.Fatalf("headline = %q, want the mid-turn evidence folded in", headline)
	}
	if len(h.rows(rootFeed())) != 2 {
		t.Fatalf("rows = %d, want the prompt and the terminal alone", len(h.rows(rootFeed())))
	}
	if !h.hasRecord("warn", "daemon.feed.api_error") {
		t.Fatalf("records = %+v, want a WARN daemon.feed.api_error", h.records())
	}
}

func TestAQueryDeathDrawsTheInFlightTurnsTerminal(t *testing.T) {
	// Arrange: a turn in flight.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")

	// Act.
	h.resolver.OnSessionUpdate(testWorkspace, &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_QueryDied{QueryDied: &conversationv1.SessionQueryDied{
			Cause: &conversationv1.SessionQueryDied_IteratorFailure{
				IteratorFailure: &conversationv1.SessionQueryIteratorFailure{Cause: "iterator threw"},
			},
		}},
	})

	// Assert: a consumer with no stream open still gets the terminal.
	errored := h.terminalRow("turn-1").GetErrored()
	if errored.GetQueryDied() == nil {
		t.Fatalf("arm = %q, want query_died", erroredArmWord(errored))
	}
	if !contains(errored.GetHeadline().GetText(), "the SDK's iterator threw") {
		t.Fatalf("headline = %q", errored.GetHeadline().GetText())
	}
	if errored.GetMessage().GetText() != "iterator threw" {
		t.Fatalf("message = %q, want the thrown cause", errored.GetMessage().GetText())
	}
}

// TestTheTurnOpenEdgeIsWhatAQueryDeathTerminates covers the turn a turn the
// DAEMON opened: StartTurn answers with the prompt instead of echoing it on
// the agent's stream, so the queue's hand-over is the feed's only source for
// which turn is running.
func TestTheTurnOpenEdgeIsWhatAQueryDeathTerminates(t *testing.T) {
	// Arrange: the turn-open edge alone -- no streamed prompt at all.
	h := newHarness(t)
	h.resolver.OnTurnOpened(testWorkspace, ids.TurnID("turn-7"))

	// Act.
	h.resolver.OnSessionUpdate(testWorkspace, &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_QueryDied{QueryDied: &conversationv1.SessionQueryDied{
			Cause: &conversationv1.SessionQueryDied_UnexpectedEof{
				UnexpectedEof: &conversationv1.SessionQueryUnexpectedEof{},
			},
		}},
	})

	// Assert.
	if h.terminalRow("turn-7").GetErrored().GetQueryDied() == nil {
		t.Fatalf("rows = %+v, want the opened turn's query_died terminal", h.rows(rootFeed()))
	}
}

// Landing 10 gave FeedTurnErrorQueryDied a `cause` oneof mirroring
// SessionQueryDied's, so a reader can tell the agent binary vanishing from
// the SDK throwing. One test per cause.

func TestAnUnexpectedEofDrawsTheUnexpectedEofCause(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")

	// Act.
	h.queryDied(&conversationv1.SessionQueryDied{
		Cause: &conversationv1.SessionQueryDied_UnexpectedEof{
			UnexpectedEof: &conversationv1.SessionQueryUnexpectedEof{},
		},
	})

	// Assert.
	if h.terminalRow("turn-1").GetErrored().GetQueryDied().GetUnexpectedEof() == nil {
		t.Fatalf("cause = %v, want unexpected_eof", h.terminalRow("turn-1").GetErrored().GetQueryDied())
	}
}

func TestAnIteratorFailureDrawsTheIteratorFailureCause(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")

	// Act.
	h.queryDied(&conversationv1.SessionQueryDied{
		Cause: &conversationv1.SessionQueryDied_IteratorFailure{
			IteratorFailure: &conversationv1.SessionQueryIteratorFailure{Cause: "iterator threw"},
		},
	})

	// Assert.
	if h.terminalRow("turn-1").GetErrored().GetQueryDied().GetIteratorFailure() == nil {
		t.Fatalf("cause = %v, want iterator_failure", h.terminalRow("turn-1").GetErrored().GetQueryDied())
	}
}

// TestADeathStatingNoCauseLeavesTheCauseUnset: inventing one would claim the
// producer said something it did not.
func TestADeathStatingNoCauseLeavesTheCauseUnset(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")

	// Act.
	h.queryDied(&conversationv1.SessionQueryDied{})

	// Assert.
	if got := h.terminalRow("turn-1").GetErrored().GetQueryDied().GetCause(); got != nil {
		t.Fatalf("cause = %v, want unset", got)
	}
}

// queryDiedTerminal is the terminal the shim owes the open turn when its query
// dies under it: AgentFailure's own query_died arm, carrying the death.
func queryDiedTerminal(died *conversationv1.SessionQueryDied) *conversationv1.AgentFailure {
	return &conversationv1.AgentFailure{
		Errors:  []string{"fake vendor query died mid-turn"},
		Failure: &conversationv1.AgentFailure_QueryDied{QueryDied: died},
	}
}

// iteratorDeath is a query death whose SDK iterator threw.
func iteratorDeath() *conversationv1.SessionQueryDied {
	return &conversationv1.SessionQueryDied{
		Cause: &conversationv1.SessionQueryDied_IteratorFailure{
			IteratorFailure: &conversationv1.SessionQueryIteratorFailure{Cause: "fake vendor query died mid-turn"},
		},
	}
}

// TestAQueryDeathDrawsQueryDiedWhicheverStatementArrivesFirst covers the two
// statements of one death: the session's query_died push and the turn's own
// query_died terminal, served from the store. They travel by independent
// channels with no ordering between them, so the drawn terminal must be the
// death, with its cause, in either arrival order.
func TestAQueryDeathDrawsQueryDiedWhicheverStatementArrivesFirst(t *testing.T) {
	cases := []struct {
		name string
		act  func(h *harness)
	}{
		{name: "the terminal first, then the push", act: func(h *harness) {
			h.terminal("turn-1", nil, queryDiedTerminal(iteratorDeath()))
			h.queryDied(iteratorDeath())
		}},
		{name: "the push first, then the terminal", act: func(h *harness) {
			h.queryDied(iteratorDeath())
			h.terminal("turn-1", nil, queryDiedTerminal(iteratorDeath()))
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a turn in flight.
			h := newHarness(t)
			h.deliverPrompt("turn-1", "!query-fail")

			// Act.
			tc.act(h)

			// Assert.
			errored := h.terminalRow("turn-1").GetErrored()
			if errored.GetQueryDied().GetIteratorFailure() == nil {
				t.Fatalf("arm = %q (%v), want query_died.iterator_failure", erroredArmWord(errored), errored)
			}
			if got := errored.GetMessage().GetText(); got != "fake vendor query died mid-turn" {
				t.Fatalf("message = %q, want the thrown cause", got)
			}
		})
	}
}

// TestAnOrdinaryExecutionErrorIsStillDrawnAsOne: only the query_died arm is a
// death, so a producer's execution_error keeps its own arm.
func TestAnOrdinaryExecutionErrorIsStillDrawnAsOne(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.terminal("turn-1", nil, &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_ExecutionError{
			ExecutionError: &conversationv1.AgentExecutionError{},
		},
	})

	// Assert.
	if h.terminalRow("turn-1").GetErrored().GetExecutionError() == nil {
		t.Fatalf("arm = %q, want execution_error", erroredArmWord(h.terminalRow("turn-1").GetErrored()))
	}
}

func TestAQueryDeathWithNoTurnInFlightOwesNoTerminal(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.resolver.OnSessionUpdate(testWorkspace, &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_QueryDied{QueryDied: &conversationv1.SessionQueryDied{
			Cause: &conversationv1.SessionQueryDied_UnexpectedEof{
				UnexpectedEof: &conversationv1.SessionQueryUnexpectedEof{},
			},
		}},
	})

	// Assert.
	if rows := h.rows(rootFeed()); len(rows) != 0 {
		t.Fatalf("rows = %d, want none", len(rows))
	}
	if !h.hasRecord("debug", "daemon.feed.query_died_without_turn") {
		t.Fatalf("records = %+v, want the no-turn branch recorded", h.records())
	}
}

func TestASubagentStreamEndingDrawsNoTurnTerminal(t *testing.T) {
	// Arrange, Act: a terminal with no turn belongs to a bubble.
	h := newHarness(t)
	h.resolver.OnAgentTerminal(testWorkspace, mainAgent(), nil, &conversationv1.AgentSuccess{
		Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{}},
	}, nil, noAddress())

	// Assert.
	if rows := h.rows(rootFeed()); len(rows) != 0 {
		t.Fatalf("rows = %d, want none", len(rows))
	}
	if !h.hasRecord("debug", "daemon.feed.agent_terminal_without_turn") {
		t.Fatalf("records = %+v, want the no-turn branch recorded", h.records())
	}
}

func TestATerminalWithNeitherSuccessNorFailureIsAnInvariantViolation(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.terminal("turn-1", nil, nil)

	// Assert.
	if !h.hasRecord("error", "daemon.feed.terminal_without_outcome") {
		t.Fatalf("records = %+v, want an ERROR daemon.feed.terminal_without_outcome", h.records())
	}
}

func TestTheTerminalRowIsStampedWithItsTurn(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.terminal("turn-1", &conversationv1.AgentSuccess{
		Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{}},
	}, nil)

	// Assert.
	for _, row := range h.rows(rootFeed()) {
		if row.GetTurnEnded() != nil && row.GetTurn().GetValue() != "turn-1" {
			t.Fatalf("turn stamp = %q, want turn-1", row.GetTurn().GetValue())
		}
	}
}

// TestATerminalDoesNotRestateTheApiFailureItEndedOn pins the fix for the race
// a headless run of the real editor found in section H: the SAME vendor
// failure reaches this resolver twice — once as the sidecar's
// transcript-tailed mid-turn `system:api_error`, once as the shim's own stream
// terminal — and whichever arrives first decided whether the headline gained
// an evidence clause. Measured in one run of the twelve `!api-*` arms,
// `api-429` lost that race by 17ms and `api-401` won it by 3ms, so one run
// drew two headlines for one shape of failure.
//
// The evidence line's own words settle it: it says the turn WENT ON, which is
// false of the failure that ended it.
func TestATerminalDoesNotRestateTheApiFailureItEndedOn(t *testing.T) {
	// Arrange: the vendor's own sentence, as both producers state it.
	const message = "Authentication failed."
	h := newHarness(t)
	h.deliverPrompt("turn-1", "!api-401")
	h.resolver.OnApiError(testWorkspace, mainAgent(), &conversationv1.ApiRequestFailed{
		Message: message,
	}, nil, noAddress())

	// Act: the turn then ends on THAT failure.
	h.terminal("turn-1", nil, &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_ApiRequestFailed{
			ApiRequestFailed: &conversationv1.ApiRequestFailed{
				Message: message,
				Kind: &conversationv1.ApiRequestFailed_AuthenticationFailed{
					AuthenticationFailed: &conversationv1.ApiAuthenticationFailed{},
				},
			},
		},
	})

	// Assert: the arm's sentence, and nothing appended to it.
	headline := h.terminalRow("turn-1").GetErrored().GetHeadline().GetText()
	if headline != "the credential was rejected — sign in again" {
		t.Fatalf("headline = %q, want the arm's sentence with no evidence clause", headline)
	}
}

// TestATerminalRestatesNothingWhateverOrderTheTwoProducersArriveIn is the same
// failure with the two producers REVERSED: the terminal first, the sidecar's
// mid-turn record after. This is the ordering that was already green by
// accident — evidence for a closed turn is dropped — and it is pinned so the
// two orderings are held to ONE headline rather than to one code path.
func TestATerminalRestatesNothingWhateverOrderTheTwoProducersArriveIn(t *testing.T) {
	// Arrange
	const message = "Authentication failed."
	h := newHarness(t)
	h.deliverPrompt("turn-1", "!api-401")

	// Act: the terminal lands first, then the transcript's record.
	h.terminal("turn-1", nil, &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_ApiRequestFailed{
			ApiRequestFailed: &conversationv1.ApiRequestFailed{
				Message: message,
				Kind: &conversationv1.ApiRequestFailed_AuthenticationFailed{
					AuthenticationFailed: &conversationv1.ApiAuthenticationFailed{},
				},
			},
		},
	})
	h.resolver.OnApiError(testWorkspace, mainAgent(), &conversationv1.ApiRequestFailed{
		Message: message,
	}, nil, noAddress())

	// Assert: the same sentence the other ordering drew.
	headline := h.terminalRow("turn-1").GetErrored().GetHeadline().GetText()
	if headline != "the credential was rejected — sign in again" {
		t.Fatalf("headline = %q, want the arm's sentence with no evidence clause", headline)
	}
}

// TestATerminalStatesAMidTurnApiFailureItDidNotDieOf is the specific negative
// of the two above: dropping evidence is about the failure the turn ENDED ON,
// never about api evidence as a class. A 429 the turn survived and a 500 it
// then died of are two facts, and the reader wants both.
func TestATerminalStatesAMidTurnApiFailureItDidNotDieOf(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.resolver.OnApiError(testWorkspace, mainAgent(), &conversationv1.ApiRequestFailed{
		Message: "Rate limited; retry after 30 seconds.",
	}, nil, noAddress())

	// Act
	h.terminal("turn-1", nil, &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_ApiRequestFailed{
			ApiRequestFailed: &conversationv1.ApiRequestFailed{
				Message: "The service raised.",
				Kind: &conversationv1.ApiRequestFailed_Internal{
					Internal: &conversationv1.ApiInternal{},
				},
			},
		},
	})

	// Assert
	headline := h.terminalRow("turn-1").GetErrored().GetHeadline().GetText()
	if !contains(headline, "Rate limited; retry after 30 seconds.") {
		t.Fatalf("headline = %q, want the surviving mid-turn failure folded in as evidence", headline)
	}
}

// THE DISCRIMINATOR IS THE CUT, NOT THE INTERRUPTED ARM. A /clear and a footer
// stop both arrive as AgentSuccess.interrupted.by_user; the ONLY difference is
// whether a ContextCut accompanied the turn's end. The user-stop must draw its
// interrupted terminal (visible feedback that the stop took effect), and the
// clear must draw none.
func TestInterruptedTerminalDrawsForAUserStopButNotAClear(t *testing.T) {
	// Arrange: a clear turn (with a confirming cut) and, separately, an ordinary
	// user-stopped turn — both ending on the same interrupted-by-user arm.
	h := newHarness(t)

	// A /clear: registered, its cut confirmed.
	h.resolver.OnClearReceived(testWorkspace, ids.TurnID("clear-turn"))
	h.resolver.OnTurnOpened(testWorkspace, ids.TurnID("clear-turn"))
	h.resolver.OnContextCut(testWorkspace, mainAgent(),
		&conversationv1.ContextCut{Cut: &conversationv1.ContextCut_Cleared{Cleared: &conversationv1.ContextCleared{}}},
		&conversationv1.HistoryPointer{Value: "entry-clear"}, nil, noAddress())

	// An ordinary turn the user stops — no cut.
	h.deliverPrompt("stop-turn", "do the thing")

	// Act: both end on interrupted-by-user.
	h.terminal("clear-turn", interruptedByUser(), nil)
	h.terminal("stop-turn", interruptedByUser(), nil)

	// Assert: the clear drew no terminal; the user-stop drew its interrupted one.
	if h.hasTerminalRow("clear-turn") {
		t.Fatal("the /clear drew a terminal bubble; only its divider is its outcome")
	}
	if h.terminalRow("stop-turn").GetInterrupted() == nil {
		t.Fatalf("user-stop outcome = %T, want a visible interrupted terminal",
			h.terminalRow("stop-turn").GetOutcome())
	}
}

// TestALeftoverDetachmentIsReportedOnlyWhenItArrivedLive pins the line between
// a failure and replay: a LIVE detachment whose call never drew is
// unplaceable (ERROR, topbar); one REPLAYED from history names a call the
// replay simply did not reach, and is recorded at DEBUG.
func TestALeftoverDetachmentIsReportedOnlyWhenItArrivedLive(t *testing.T) {
	for _, tc := range []struct {
		name       string
		arrange    func(h *harness)
		wantError  bool
		wantRecord string
	}{
		{
			name:       "a live detachment is reported unplaceable",
			arrange:    func(h *harness) { h.detachWork("work-1", "never-seen") },
			wantError:  true,
			wantRecord: "daemon.feed.detached_unplaceable",
		},
		{
			name: "a replayed detachment is recorded at debug",
			arrange: func(h *harness) {
				h.replay(historyPage(&conversationv1.HistoryMore{}, frameEntry(mainAgent(), &conversationv1.AgentDetachedWork{
					Work: &conversationv1.DetachedWorkId{Value: "work-1"},
					Origin: &conversationv1.AgentDetachedWork_Detached{Detached: &conversationv1.DetachedWorkDetached{
						DetachedFromId: &conversationv1.AgentActivityId{Value: "never-seen"},
						Cause:          &conversationv1.DetachedWorkDetached_Requested{Requested: &conversationv1.DetachedCauseRequested{}},
					}},
				})))
			},
			wantError:  false,
			wantRecord: "daemon.feed.replayed_detachment_unclaimed",
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			tc.arrange(h)

			// Act.
			h.terminal("turn-1", &conversationv1.AgentSuccess{
				Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{}},
			}, nil)

			// Assert.
			if got := h.hasRecord("error", "daemon.feed.detached_unplaceable"); got != tc.wantError {
				t.Fatalf("ERROR recorded = %v, want %v; records = %+v", got, tc.wantError, h.records())
			}
			if got := len(h.warnings.keys()) > 0; got != tc.wantError {
				t.Fatalf("raised = %v, want raised %v", h.warnings.keys(), tc.wantError)
			}
			level := "debug"
			if tc.wantError {
				level = "error"
			}
			if !h.hasRecord(level, tc.wantRecord) {
				t.Fatalf("records = %+v, want %s %s", h.records(), level, tc.wantRecord)
			}
		})
	}
}
