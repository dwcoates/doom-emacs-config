package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
)

// A FAILURE AGENT-REPL RETRIES DRAWS NO ROW (retriedfailure.go): a terminal
// promised agent-repl's retry withholds its errored row, and an abandonment of
// that retry draws it.

// promised marks FAILURE as one agent-repl retries, the ATTEMPT-th retry
// following it.
func promised(failure *conversationv1.AgentFailure, attempt uint32) *conversationv1.AgentFailure {
	failure.RetryPromised = &conversationv1.AgentFailureRetryPromised{Attempt: attempt}
	return failure
}

// abandonRetry states that the retry promised to TURN was abandoned.
func (h *harness) abandonRetry(turn string) {
	h.t.Helper()
	h.resolver.OnSessionUpdate(testWorkspace, &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_VendorErrorRetry{
		VendorErrorRetry: &conversationv1.SessionVendorErrorRetry{
			Stated: &conversationv1.SessionVendorErrorRetry_Abandoned{Abandoned: &conversationv1.SessionVendorErrorRetryAbandoned{
				FailedTurn: &conversationv1.TurnId{Value: turn}, Cause: "the prompt queue is closed"}},
		},
	}})
}

func TestARetriedFailureDrawsNoRow(t *testing.T) {
	cases := []struct {
		name    string
		failure func() *conversationv1.AgentFailure
	}{
		{name: "a request that never reached the API", failure: unreachableFailure},
		{name: "an overloaded API", failure: overloaded},
		{name: "a passing rate limit", failure: rateLimited},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.deliverPrompt("t1", "question")

			// Act
			h.terminal("t1", nil, promised(tc.failure(), 1))

			// Assert
			if h.endingDrawn("t1") {
				t.Fatal("the retried failure's errored row was drawn")
			}
		})
	}
}

func TestAFailureNothingRetriesDrawsItsRow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("t1", "question")

	// Act: the chain gave up here, so the terminal carries no promise.
	h.terminal("t1", nil, unreachableFailure())

	// Assert
	if !h.endingDrawn("t1") {
		t.Fatal("a failure nothing retries drew no errored row")
	}
}

func TestARetriedFailureLeavesThePromptItFailed(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("t1", "question")

	// Act
	h.terminal("t1", nil, promised(unreachableFailure(), 1))

	// Assert
	if h.rowByID(rootFeed(), promptRowID("t1").GetValue()) == nil {
		t.Fatal("the prompt of the retried turn left the feed")
	}
}

func TestARetriedFailureFilesNoLiveEnding(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("t1", "question")

	// Act
	h.terminal("t1", nil, promised(unreachableFailure(), 1))

	// Assert: no ending, so no failure banner.
	if _, ok := h.resolver.TakeTurnEnding(testWorkspace, "t1"); ok {
		t.Fatal("the retried failure filed a live ending for the desktop banner")
	}
}

func TestARetriedFailureIsRecordedAtInfo(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("t1", "question")

	// Act
	h.terminal("t1", nil, promised(unreachableFailure(), 2))

	// Assert
	record, ok := dlog.FindRecord(h.records(), dlog.RecordQuery{Level: "info", Operation: opRetriedFailureWithheld})
	if !ok || record.Context["turn"] != "t1" || record.Context["attempt"] != uint32(2) {
		t.Fatalf("records = %+v, want the withheld failure recorded at INFO with its turn and attempt", h.records())
	}
}

func TestAnAbandonedRetryDrawsTheWithheldFailure(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("t1", "question")
	h.terminal("t1", nil, promised(unreachableFailure(), 1))

	// Act
	h.abandonRetry("t1")

	// Assert
	if !h.endingDrawn("t1") {
		t.Fatal("the abandoned retry's failure was not drawn")
	}
	if !h.hasRecord("info", opRetryAbandoned) {
		t.Fatalf("records = %+v, want the abandonment recorded at INFO", h.records())
	}
}

func TestAnAbandonmentStatedBeforeItsTerminalDrawsTheTerminalsFailure(t *testing.T) {
	// Arrange: the abandonment is restated on a watch opened before the
	// terminal's page replays.
	h := newHarness(t)
	h.deliverPrompt("t1", "question")
	h.abandonRetry("t1")

	// Act
	h.terminal("t1", nil, promised(unreachableFailure(), 1))

	// Assert
	if !h.endingDrawn("t1") {
		t.Fatal("a terminal whose retry was already abandoned drew no errored row")
	}
}

func TestAnAbandonmentDrawsOnlyTheTurnItNames(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("t1", "question")
	h.terminal("t1", nil, promised(unreachableFailure(), 1))
	h.terminal("t2", nil, promised(unreachableFailure(), 2))

	// Act
	h.abandonRetry("t2")

	// Assert
	if h.endingDrawn("t1") || !h.endingDrawn("t2") {
		t.Fatalf("t1 drawn = %v, t2 drawn = %v, want only t2's failure drawn", h.endingDrawn("t1"), h.endingDrawn("t2"))
	}
}

func TestAnAbandonmentNamingNoTurnIsRefusedAtError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("t1", "question")
	h.terminal("t1", nil, promised(unreachableFailure(), 1))

	// Act
	h.abandonRetry("")

	// Assert
	if h.endingDrawn("t1") {
		t.Fatal("an abandonment naming no turn drew a withheld failure")
	}
	if !h.hasRecord("error", opRetryAbandoned) {
		t.Fatalf("records = %+v, want the refusal recorded at ERROR", h.records())
	}
}

func TestAScheduleThatAbandonsNothingDrawsNoRow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("t1", "question")
	h.terminal("t1", nil, promised(unreachableFailure(), 1))

	// Act
	h.resolver.OnSessionUpdate(testWorkspace, &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_VendorErrorRetry{
		VendorErrorRetry: &conversationv1.SessionVendorErrorRetry{},
	}})

	// Assert
	if h.endingDrawn("t1") {
		t.Fatal("a statement that abandons nothing drew the withheld failure")
	}
}

func TestANoticeDrawnBeforeARetriedFailureLeavesTheFeedWithIt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("t1", "question")
	h.notice("unit-1", "Rate limited; retry after 30 seconds.", "t1")

	// Act
	h.terminal("t1", nil, promised(rateLimited(), 1))

	// Assert
	if h.noticeRowDrawn("unit-1") {
		t.Fatal("the notice of a retried failure is still drawn")
	}
}

func TestANoticeArrivingAfterARetriedFailureIsNeverDrawn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("t1", "question")
	h.terminal("t1", nil, promised(rateLimited(), 1))

	// Act
	h.notice("unit-1", "Rate limited; retry after 30 seconds.", "t1")

	// Assert
	if h.noticeRowDrawn("unit-1") {
		t.Fatal("a late notice of a retried failure was drawn")
	}
	if !h.hasRecord("info", opNoticeFoldedIntoHeld) {
		t.Fatalf("records = %+v, want the fold into the withheld failure recorded at INFO", h.records())
	}
}

func TestAnAbandonedRetryDrawsTheNoticesItsFailureWithheld(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(h *harness)
	}{
		{name: "a notice before the terminal", arrange: func(h *harness) {
			h.notice("unit-1", "Rate limited; retry after 30 seconds.", "t1")
			h.terminal("t1", nil, promised(rateLimited(), 1))
		}},
		{name: "a notice after the terminal", arrange: func(h *harness) {
			h.terminal("t1", nil, promised(rateLimited(), 1))
			h.notice("unit-1", "Rate limited; retry after 30 seconds.", "t1")
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.deliverPrompt("t1", "question")
			tc.arrange(h)

			// Act
			h.abandonRetry("t1")

			// Assert
			if got := noticeTexts(h.markerOf("t1")); len(got) != 1 || got[0] != "Rate limited; retry after 30 seconds." {
				t.Fatalf("notices = %q, want the withheld notice's words on the drawn card", got)
			}
			if h.noticeRowDrawn("unit-1") {
				t.Fatal("the notice was drawn as its own row beside the card")
			}
		})
	}
}
