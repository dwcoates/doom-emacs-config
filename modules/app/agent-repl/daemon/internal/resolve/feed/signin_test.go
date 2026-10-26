package feed

import (
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/ids"
)

// A SIGN-IN ANSWERS AN ACCOUNT FAILURE (signin.go): the broken bubble of a
// turn that ended on a sign-in-curable cause offers the action, and a completed
// sign-in retires that bubble and the turn's errored ending for good.

// authFailed is the vendor rejecting the credential.
func authFailed() *conversationv1.AgentFailure {
	return apiFailure(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_AuthenticationFailed{AuthenticationFailed: &conversationv1.ApiAuthenticationFailed{}}})
}

// overloaded is a vendor failure no sign-in cures.
func overloaded() *conversationv1.AgentFailure {
	return apiFailure(&conversationv1.ApiRequestFailed{Kind: &conversationv1.ApiRequestFailed_Overloaded{Overloaded: &conversationv1.ApiOverloaded{}}})
}

// brokenResponse draws one response the turn's failure cut short.
func (h *harness) brokenResponse(unit, text, turn string) {
	h.t.Helper()
	stamp := &conversationv1.TurnId{Value: turn}
	failure := &conversationv1.AgentResponseFailure{Prose: &conversationv1.AgentResponseProse{Markdown: text}}
	h.resolver.OnActivity(testWorkspace, mainAgent(), responseFrame(unit, failure, nil), stamp, nil)
}

// failedTurn draws a prompt, its broken bubble and its failed ending.
func (h *harness) failedTurn(turn string, failure *conversationv1.AgentFailure) {
	h.t.Helper()
	h.deliverPrompt(turn, "question "+turn)
	h.brokenResponse("u-"+turn, "Failed to authenticate.", turn)
	h.terminal(turn, nil, failure)
}

// turnRows answers the root feed's rows of TURN.
func (h *harness) turnRows(turn string) []*frontendv1.FeedRow {
	h.t.Helper()
	var out []*frontendv1.FeedRow
	for _, row := range h.rows(rootFeed()) {
		if row.GetTurn().GetValue() == turn {
			out = append(out, row)
		}
	}
	return out
}

// brokenBubbleOf answers TURN's broken bubble, nil when none is drawn.
func (h *harness) brokenBubbleOf(turn string) *frontendv1.FeedResponseError {
	h.t.Helper()
	for _, row := range h.turnRows(turn) {
		if broken := row.GetActivity().GetResponse().GetError(); broken != nil {
			return broken
		}
	}
	return nil
}

// failureRowsOf counts TURN's failure rows: broken bubbles and errored endings.
func (h *harness) failureRowsOf(turn string) int {
	h.t.Helper()
	n := 0
	for _, row := range h.turnRows(turn) {
		if isFailureRow(row) {
			n++
		}
	}
	return n
}

func TestTheBrokenBubbleOfASignInCurableTurnOffersTheSignInAction(t *testing.T) {
	cases := []struct {
		name    string
		failure *conversationv1.AgentFailure
		want    bool
	}{
		{name: "a rejected credential", failure: authFailed(), want: true},
		{name: "an overload", failure: overloaded(), want: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act: the bubble is drawn first, the ending names the cause after.
			h.failedTurn("turn-1", tc.failure)

			// Assert
			if got := h.brokenBubbleOf("turn-1").GetSignIn() != nil; got != tc.want {
				t.Fatalf("broken bubble sign_in = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestABrokenBubbleRedeliveredAfterTheEndingKeepsTheSignInAction(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.failedTurn("turn-1", authFailed())

	// Act: the other store plane delivers the same bubble again.
	h.brokenResponse("u-turn-1", "Failed to authenticate.", "turn-1")

	// Assert
	if h.brokenBubbleOf("turn-1").GetSignIn() == nil {
		t.Fatal("the redelivered broken bubble lost the sign-in action")
	}
}

func TestASignInRetiresTheAnsweredFailureAndKeepsTheRest(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.failedTurn("t1", overloaded())
	h.failedTurn("t2", authFailed())

	// Act
	err := h.resolver.OnSignedIn(testWorkspace)

	// Assert
	if err != nil {
		t.Fatalf("OnSignedIn: %v", err)
	}
	if n := h.failureRowsOf("t2"); n != 0 {
		t.Fatalf("t2 still draws %d failure rows, want none", n)
	}
	if h.rowByID(rootFeed(), promptRowID("t2").GetValue()) == nil {
		t.Fatal("the answered turn's prompt was retired too")
	}
	if n := h.failureRowsOf("t1"); n != 2 {
		t.Fatalf("t1 draws %d failure rows, want its bubble and ending kept", n)
	}
	if !h.hasRecord("info", opSignedIn) {
		t.Fatalf("records = %+v, want the sign-in recorded at INFO", h.records())
	}
}

func TestASignInWithNothingToAnswerRetiresNothing(t *testing.T) {
	// Arrange
	store := &fakeTurnSet{}
	h := newDurableHarness(t, durableStores{signedIn: store})
	h.failedTurn("t1", overloaded())

	// Act
	err := h.resolver.OnSignedIn(testWorkspace)

	// Assert
	if err != nil {
		t.Fatalf("OnSignedIn: %v", err)
	}
	if n := h.failureRowsOf("t1"); n != 2 {
		t.Fatalf("t1 draws %d failure rows, want 2", n)
	}
	if len(store.turns) != 0 {
		t.Fatalf("recorded %v, want nothing", store.turns)
	}
}

func TestAFailureRowOfAnAnsweredTurnArrivingLaterIsNotDrawn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.failedTurn("t1", authFailed())
	if err := h.resolver.OnSignedIn(testWorkspace); err != nil {
		t.Fatalf("OnSignedIn: %v", err)
	}

	// Act: the other store plane re-delivers the bubble and the ending.
	h.brokenResponse("u-t1", "Failed to authenticate.", "t1")
	h.terminal("t1", nil, authFailed())

	// Assert
	if n := h.failureRowsOf("t1"); n != 0 {
		t.Fatalf("t1 draws %d failure rows after the sign-in, want none", n)
	}
}

func TestAnAnsweredFailureStaysRetiredAfterARestart(t *testing.T) {
	// Arrange
	store := &fakeTurnSet{}
	before := newDurableHarness(t, durableStores{signedIn: store})
	before.failedTurn("t1", authFailed())
	if err := before.resolver.OnSignedIn(testWorkspace); err != nil {
		t.Fatalf("OnSignedIn: %v", err)
	}

	// Act: the successor replays the conversation.
	after := newDurableHarness(t, durableStores{signedIn: store})
	after.failedTurn("t1", authFailed())

	// Assert
	if n := after.failureRowsOf("t1"); n != 0 {
		t.Fatalf("t1 draws %d failure rows after a restart, want none", n)
	}
	if !after.hasRecord("info", opSignedInRestore) {
		t.Fatalf("records = %+v, want the load recorded at INFO", after.records())
	}
}

func TestAFailedSignInRecordStillRetiresTheRowsAndIsRaised(t *testing.T) {
	// Arrange
	store := &fakeTurnSet{putErr: errors.New("disk full")}
	h := newDurableHarness(t, durableStores{signedIn: store})
	h.failedTurn("t1", authFailed())

	// Act
	err := h.resolver.OnSignedIn(testWorkspace)

	// Assert
	if err == nil {
		t.Fatal("OnSignedIn hid a failed record")
	}
	if n := h.failureRowsOf("t1"); n != 0 {
		t.Fatalf("t1 draws %d failure rows, want them retired anyway", n)
	}
	assertRecordedFault(t, h, opSignedIn, "disk full")
}

func TestAFailedSignInLoadIsLoggedAndRaised(t *testing.T) {
	// Arrange
	store := &fakeTurnSet{loadErr: errors.New("corrupt page")}

	// Act
	h := newDurableHarness(t, durableStores{signedIn: store})

	// Assert
	assertRecordedFault(t, h, opSignedInRestore, "corrupt page")
}

func TestASignInRecordsTheAnsweredTurns(t *testing.T) {
	// Arrange
	store := &fakeTurnSet{}
	h := newDurableHarness(t, durableStores{signedIn: store})
	h.failedTurn("t1", authFailed())

	// Act
	if err := h.resolver.OnSignedIn(testWorkspace); err != nil {
		t.Fatalf("OnSignedIn: %v", err)
	}

	// Assert
	if len(store.turns) != 1 || store.turns[0] != ids.TurnID("t1") {
		t.Fatalf("recorded %v, want [t1]", store.turns)
	}
}
