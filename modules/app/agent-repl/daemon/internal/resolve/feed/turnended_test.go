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

func TestAConcludedTurnNamesItsAnsweringRow(t *testing.T) {
	// Arrange: a turn whose prose the producer named as the answer.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "what is it")
	h.resolver.OnActivity(testWorkspace, mainAgent(),
		responseSuccessActivity("unit-9", "it is this"), noAddress())

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

func TestAProducerArmWithNoDrawnCounterpartIsKeptByName(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.terminal("turn-1", nil, &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_MaxTurns{MaxTurns: &conversationv1.AgentMaxTurnsReached{}},
	})

	// Assert: never flattened into a generic failure.
	errored := h.terminalRow("turn-1").GetErrored()
	if got := errored.GetVendorUnmodeled().GetType(); got != "max_turns" {
		t.Fatalf("type = %q, want the arm's own name", got)
	}
	if got := errored.GetHeadline().GetText(); got != "the run reached its ceiling on model round-trips" {
		t.Fatalf("headline = %q", got)
	}
}

func TestAPromptTooLongIsDrawnAsARequestTooLarge(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.terminal("turn-1", nil, &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_PromptTooLong{PromptTooLong: &conversationv1.AgentPromptTooLong{}},
	})

	// Assert.
	if h.terminalRow("turn-1").GetErrored().GetRequestTooLarge() == nil {
		t.Fatalf("arm = %q, want request_too_large", erroredArmWord(h.terminalRow("turn-1").GetErrored()))
	}
}

func TestALostAgentSaysWeStoppedSeeingItRatherThanThatItFailed(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.terminal("turn-1", nil, &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_Lost{Lost: &conversationv1.DetachedLost{
			How: &conversationv1.DetachedLost_SweptUp{SweptUp: &conversationv1.DetachedLostSweptUp{}},
		}},
	})

	// Assert.
	errored := h.terminalRow("turn-1").GetErrored()
	if got := errored.GetVendorUnmodeled().GetType(); got != "lost:swept_up" {
		t.Fatalf("type = %q, want the lost cause named", got)
	}
	if !contains(errored.GetHeadline().GetText(), "we lost sight of this work") {
		t.Fatalf("headline = %q, want the lost wording", errored.GetHeadline().GetText())
	}
}

func TestAMidTurnApiErrorRidesTheTerminalsHeadlineAsEvidence(t *testing.T) {
	// Arrange: a request that failed mid-turn and the turn went on.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.resolver.OnApiError(testWorkspace, mainAgent(), &conversationv1.ApiRequestFailed{
		Message: "connection reset",
	}, noAddress())

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
