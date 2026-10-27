package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/ids"
)

// A VENDOR THAT SERVES AGAIN ANSWERS ITS OUTAGE (vendorrecovery.go): the
// vendor-error cards of turns a waiting cause ended leave the feed once a
// later turn shows the vendor serving.

// rateLimited is a passing 429.
func rateLimited() *conversationv1.AgentFailure {
	return apiFailure(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_RateLimited{RateLimited: &conversationv1.ApiRateLimited{}}})
}

// unmodeledFailure is a failure the vendor named by a class we do not model.
func unmodeledFailure() *conversationv1.AgentFailure {
	return apiFailure(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_Unmodeled{Unmodeled: &conversationv1.ApiUnmodeledError{Type: "connection_error"}}})
}

// unreachableFailure is a request that never reached the API.
func unreachableFailure() *conversationv1.AgentFailure {
	return apiFailure(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_Unreachable{Unreachable: &conversationv1.ApiUnreachable{Code: "ENOTFOUND"}}})
}

// invalidRequest is the vendor refusing the request itself.
func invalidRequest() *conversationv1.AgentFailure {
	return apiFailure(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_InvalidRequest{InvalidRequest: &conversationv1.ApiInvalidRequest{}}})
}

// outageTurn ends TURN on FAILURE with nothing drawn before it, as an
// agent-repl retry of a turn the vendor failed does.
func (h *harness) outageTurn(turn string, failure *conversationv1.AgentFailure) {
	h.t.Helper()
	h.terminal(turn, nil, failure)
}

// endingDrawn reports whether TURN's ending is on the root feed.
func (h *harness) endingDrawn(turn string) bool {
	h.t.Helper()
	_, ok := h.findRow(rootFeed(), h.turnEndedRowID(turn))
	return ok
}

func TestAVendorServingALaterTurnRetiresTheOutageCardsBeforeIt(t *testing.T) {
	cases := []struct {
		name  string
		serve func(h *harness)
	}{
		{name: "a response", serve: func(h *harness) { h.liveResponse("u-t3", "back again", "t3") }},
		{name: "a turn that concluded", serve: func(h *harness) { h.terminal("t3", completed(""), nil) }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.deliverPrompt("t1", "question")
			h.outageTurn("t1", overloaded())
			h.outageTurn("t2", internalFailure("API Error: 500"))

			// Act
			tc.serve(h)

			// Assert
			if h.endingDrawn("t1") || h.endingDrawn("t2") {
				t.Fatalf("t1 drawn = %v, t2 drawn = %v, want both outage cards retired", h.endingDrawn("t1"), h.endingDrawn("t2"))
			}
			if h.rowByID(rootFeed(), promptRowID("t1").GetValue()) == nil {
				t.Fatal("the prompt the outage failed was retired too")
			}
			if !h.hasRecord("info", opVendorRecovered) {
				t.Fatalf("records = %+v, want the recovery recorded at INFO", h.records())
			}
		})
	}
}

func TestAnOutageCardStandsWhileNoLaterTurnIsServed(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("t1", "question")

	// Act
	h.outageTurn("t1", overloaded())
	h.outageTurn("t2", overloaded())

	// Assert
	if !h.endingDrawn("t1") || !h.endingDrawn("t2") {
		t.Fatalf("t1 drawn = %v, t2 drawn = %v, want both outage cards standing", h.endingDrawn("t1"), h.endingDrawn("t2"))
	}
}

func TestOnlyAVendorErrorWaitingCuresIsRetiredByService(t *testing.T) {
	cases := []struct {
		name    string
		failure *conversationv1.AgentFailure
		retired bool
	}{
		{name: "an overload", failure: overloaded(), retired: true},
		{name: "the API's internal error", failure: internalFailure("API Error: 500"), retired: true},
		{name: "a rate limit", failure: rateLimited(), retired: true},
		{name: "an unmodeled class", failure: unmodeledFailure(), retired: true},
		{name: "a request that never reached the API", failure: unreachableFailure(), retired: true},
		{name: "a rejected credential", failure: authFailed(), retired: false},
		{name: "a malformed request", failure: invalidRequest(), retired: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.deliverPrompt("t1", "question")
			h.outageTurn("t1", tc.failure)

			// Act
			h.liveResponse("u-t2", "served", "t2")

			// Assert
			if got := !h.endingDrawn("t1"); got != tc.retired {
				t.Fatalf("t1 retired = %v, want %v", got, tc.retired)
			}
		})
	}
}

func TestServiceOfTheFailedTurnItselfDoesNotRetireItsCard(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("t1", "question")
	h.liveResponse("u-t1", "partial answer", "t1")

	// Act
	h.terminal("t1", nil, overloaded())

	// Assert
	if !h.endingDrawn("t1") {
		t.Fatal("the turn's own earlier response retired the card it then ended on")
	}
}

func TestRowsTheVendorDidNotServeAreNoEvidence(t *testing.T) {
	cases := []struct {
		name string
		draw func(h *harness)
	}{
		{name: "a vendor notice", draw: func(h *harness) { h.notice("u-t2", "API Error: 529", "t2") }},
		{name: "a broken response bubble", draw: func(h *harness) { h.brokenResponse("u-t2", "cut short", "t2") }},
		{name: "a user prompt", draw: func(h *harness) { h.deliverPrompt("t2", "anyone there?") }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.deliverPrompt("t1", "question")
			h.outageTurn("t1", overloaded())

			// Act
			tc.draw(h)

			// Assert
			if !h.endingDrawn("t1") {
				t.Fatal("a row the vendor did not serve retired the outage card")
			}
		})
	}
}

func TestAnOutageAfterTheRecoveryStandsUntilTheNextService(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("t1", "question")
	h.outageTurn("t1", overloaded())
	h.terminal("t2", completed(""), nil)

	// Act
	h.outageTurn("t3", overloaded())

	// Assert
	if h.endingDrawn("t1") {
		t.Fatal("the first outage's card survived the recovery")
	}
	if !h.endingDrawn("t3") {
		t.Fatal("a new outage after the recovery was retired by the earlier service")
	}
}

func TestAnOutageCardThatSortsBeforeTheNewestServiceIsRetiredAsItIsDrawn(t *testing.T) {
	// Arrange: the service of t2 sits later in the conversation than t1's
	// ending, which arrives after it, as a page loaded on scroll-back does.
	h := newHarness(t)
	h.deliverPromptAt("t1", "question", 1_000)
	h.resolver.OnActivity(testWorkspace, mainAgent(), responseSuccessActivity("u-t2", "served"),
		&conversationv1.TurnId{Value: "t2"}, placeAt(3_000))
	turn := ids.TurnID("t1")

	// Act
	h.resolver.OnAgentTerminal(testWorkspace, mainAgent(), &turn, nil, overloaded(), placeAt(2_000))

	// Assert
	if h.endingDrawn("t1") {
		t.Fatal("an outage card older than the feed's newest service was drawn")
	}
	if !h.hasRecordWith("info", opVendorRecovered, "evidence_turn", "t2") {
		t.Fatalf("records = %+v, want the retirement recorded naming the service's turn", h.records())
	}
}
