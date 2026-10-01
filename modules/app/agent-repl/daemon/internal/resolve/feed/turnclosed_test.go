package feed

import (
	"errors"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// THE DOOR'S FEED HALF: a turn the prompt queue's door closed has exactly one
// ending row, live and on replay, whatever closed it.

// closedAt is the instant every recorded close in this file carries.
var closedAt = time.UnixMilli(1_700_000_123_000)

// closeTurn tells the resolver the door closed a turn.
func (h *harness) closeTurn(turn string, how wsm.TurnClose) {
	h.t.Helper()
	h.resolver.OnTurnClosed(testWorkspace, ids.TurnID(turn), wsm.RecordedClose{How: how, At: closedAt})
}

// endingRows counts one turn's turn_ended rows across every feed.
func (h *harness) endingRows(turn string) int {
	h.t.Helper()
	h.resolver.mu.Lock()
	defer h.resolver.mu.Unlock()
	n := 0
	for _, f := range h.resolver.state(testWorkspace).feeds {
		for _, id := range f.order {
			row := f.rows[id]
			if row.GetTurnEnded() != nil && row.GetTurn().GetValue() == turn {
				n++
			}
		}
	}
	return n
}

// endingArm names a turn_ended row's outcome, down to the errored arm and its
// stop reason, for one assertion per case.
func endingArm(ended *frontendv1.FeedTurnEnded) string {
	switch ended.GetOutcome().(type) {
	case *frontendv1.FeedTurnEnded_Concluded:
		return "concluded"
	case *frontendv1.FeedTurnEnded_Interrupted:
		return "interrupted"
	case *frontendv1.FeedTurnEnded_Errored:
		errored := ended.GetErrored()
		if failed := errored.GetTurnFailed(); failed != nil {
			return "turn_failed:" + failed.GetStopReason()
		}
		return erroredArmWord(errored)
	}
	return "unset"
}

func TestATurnClosedWithNoTerminalIsEndedFromItsClose(t *testing.T) {
	tests := []struct {
		name     string
		how      wsm.TurnClose
		wantArm  string
		wantText string
	}{
		{name: "completed", how: wsm.CloseCompleted, wantArm: "concluded"},
		{name: "killed", how: wsm.CloseKilled, wantArm: "interrupted"},
		{name: "agent died", how: wsm.CloseAgentDied, wantArm: "agent_process_died", wantText: "the agent process died"},
		{name: "orphaned", how: wsm.CloseOrphaned, wantArm: "turn_failed:closed:orphaned", wantText: "the turn was dropped"},
		{name: "failed", how: wsm.CloseFailed, wantArm: "turn_failed:closed:failed", wantText: "the turn failed with an error"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.deliverPrompt("turn-1", "hello")

			// Act
			h.closeTurn("turn-1", tc.how)

			// Assert
			ended := h.terminalRow("turn-1")
			if got := endingArm(ended); got != tc.wantArm {
				t.Fatalf("arm = %q, want %q", got, tc.wantArm)
			}
			if !contains(ended.GetErrored().GetHeadline().GetText(), tc.wantText) {
				t.Fatalf("headline = %q, want it to say %q", ended.GetErrored().GetHeadline().GetText(), tc.wantText)
			}
			if n := h.endingRows("turn-1"); n != 1 {
				t.Fatalf("ending rows = %d, want exactly 1", n)
			}
		})
	}
}

// TestAKilledCloseWithNoTerminalDrawsAnUnsetCommand covers the daemon-built
// close: a recorded close carries no cause, so its interrupted row states no
// command, which the client draws as a direct stop.
func TestAKilledCloseWithNoTerminalDrawsAnUnsetCommand(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")

	// Act
	h.closeTurn("turn-1", wsm.CloseKilled)

	// Assert
	if got := interruptedCommandWord(h.terminalRow("turn-1")); got != "unset" {
		t.Fatalf("command = %q, want unset", got)
	}
}

// TestAKilledCloseAfterAnInterjectionKeepsTheInterjection covers the two paths
// agreeing live: the terminal drew the interjection first, and the door's
// close that follows it does not redraw the row as a bubble.
func TestAKilledCloseAfterAnInterjectionKeepsTheInterjection(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.terminal("turn-1", userStop(byUserInterjection()), nil)

	// Act
	h.closeTurn("turn-1", wsm.CloseKilled)

	// Assert
	if got := interruptedCommandWord(h.terminalRow("turn-1")); got != "interjection" {
		t.Fatalf("command = %q, want the terminal's interjection", got)
	}
}

func TestATurnClosedEndsAtItsRecordedInstant(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")

	// Act
	h.closeTurn("turn-1", wsm.CloseOrphaned)

	// Assert
	if got := h.terminalRow("turn-1").GetEndedAtMs(); got != closedAt.UnixMilli() {
		t.Fatalf("ended_at_ms = %d, want the recorded close %d", got, closedAt.UnixMilli())
	}
}

func TestATurnClosedAfterItsTerminalKeepsTheTerminalsEnding(t *testing.T) {
	// Arrange: the vendor's own terminal drew the ending first.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.terminal("turn-1", &conversationv1.AgentSuccess{
		Outcome: &conversationv1.AgentSuccess_Interrupted{Interrupted: &conversationv1.AgentInterrupted{}},
	}, nil)

	// Act: the door closes the row after it.
	h.closeTurn("turn-1", wsm.CloseOrphaned)

	// Assert
	if got := endingArm(h.terminalRow("turn-1")); got != "interrupted" {
		t.Fatalf("arm = %q, want the terminal's own interrupted", got)
	}
	if n := h.endingRows("turn-1"); n != 1 {
		t.Fatalf("ending rows = %d, want exactly 1", n)
	}
}

func TestATurnClosedAfterAQueryDeathKeepsTheDeathsEnding(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.queryDied(&conversationv1.SessionQueryDied{Cause: &conversationv1.SessionQueryDied_UnexpectedEof{
		UnexpectedEof: &conversationv1.SessionQueryUnexpectedEof{},
	}})

	// Act
	h.closeTurn("turn-1", wsm.CloseFailed)

	// Assert
	if got := endingArm(h.terminalRow("turn-1")); got != "query_died" {
		t.Fatalf("arm = %q, want the death's own query_died", got)
	}
	if n := h.endingRows("turn-1"); n != 1 {
		t.Fatalf("ending rows = %d, want exactly 1", n)
	}
}

func TestATurnClosedTwiceIsEndedOnce(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.closeTurn("turn-1", wsm.CloseAgentDied)

	// Act
	h.closeTurn("turn-1", wsm.CloseOrphaned)

	// Assert
	if got := endingArm(h.terminalRow("turn-1")); got != "agent_process_died" {
		t.Fatalf("arm = %q, want the first close's ending kept", got)
	}
	if n := h.endingRows("turn-1"); n != 1 {
		t.Fatalf("ending rows = %d, want exactly 1", n)
	}
}

func TestATurnClosedThatTheFeedNeverSawOpenedDrawsNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.closeTurn("turn-unseen", wsm.CloseOrphaned)

	// Assert
	if n := h.endingRows("turn-unseen"); n != 0 {
		t.Fatalf("ending rows = %d, want none for a turn the feed never saw", n)
	}
	if !h.hasRecord(dlog.LevelDebug, "daemon.feed.turn_closed_unseen") {
		t.Fatalf("records = %+v, want the unseen close recorded", h.records())
	}
}

func TestATurnOpenedOnlyByTheDaemonIsEndedFromItsClose(t *testing.T) {
	// Arrange: the turn-open edge alone, as a shim that died before its first
	// frame leaves it.
	h := newHarness(t)
	h.resolver.OnTurnOpened(testWorkspace, ids.TurnID("turn-7"))

	// Act
	h.closeTurn("turn-7", wsm.CloseAgentDied)

	// Assert
	if got := endingArm(h.terminalRow("turn-7")); got != "agent_process_died" {
		t.Fatalf("arm = %q, want agent_process_died", got)
	}
}

func TestATurnClosedStopsItsPromptWorking(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")

	// Act
	h.closeTurn("turn-1", wsm.CloseAgentDied)

	// Assert
	for _, row := range h.rows(rootFeed()) {
		if row.GetUserPrompt() != nil && row.GetUserPrompt().GetWorking() {
			t.Fatalf("the closed turn's prompt is still working: %+v", row)
		}
	}
}

func TestATurnClosedIsNoLongerTheTurnInFlight(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.closeTurn("turn-1", wsm.CloseAgentDied)

	// Act: a query death with no turn left in flight owes no terminal.
	h.queryDied(&conversationv1.SessionQueryDied{})

	// Assert
	if got := endingArm(h.terminalRow("turn-1")); got != "agent_process_died" {
		t.Fatalf("arm = %q, want the close's ending untouched", got)
	}
}

func TestAnUndeclaredCloseIsDrawnAsAnUnexplainedEndAndRecordedAtError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")

	// Act
	h.closeTurn("turn-1", wsm.TurnClose(99))

	// Assert
	if got := endingArm(h.terminalRow("turn-1")); got != "turn_failed:closed:99" {
		t.Fatalf("arm = %q, want the unexplained end", got)
	}
	if !h.hasRecord(dlog.LevelError, "daemon.feed.turn_closed_undeclared") {
		t.Fatalf("records = %+v, want the undeclared close at ERROR", h.records())
	}
}

// THE REPLAY HALF: a replayed turn whose page carries no terminal for it is
// ended from its recorded close, under the turn it ends.

func TestAReplayedTurnWithNoTerminalIsEndedFromItsRecordedClose(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.closes = map[ids.TurnID]wsm.RecordedClose{"turn-1": {How: wsm.CloseAgentDied, At: closedAt}}

	// Act
	h.replay(historyPage(&conversationv1.HistoryFloor{}, promptEntry("turn-1", "hello")))

	// Assert
	if got := endingArm(h.terminalRow("turn-1")); got != "agent_process_died" {
		t.Fatalf("arm = %q, want the recorded close's agent_process_died", got)
	}
	if n := h.endingRows("turn-1"); n != 1 {
		t.Fatalf("ending rows = %d, want exactly 1", n)
	}
}

func TestAReplayedTurnsRecordedEndingLandsUnderItsOwnTurn(t *testing.T) {
	// Arrange: an older turn with no terminal on the page, then a newer one.
	h := newHarness(t)
	h.closes = map[ids.TurnID]wsm.RecordedClose{"turn-1": {How: wsm.CloseOrphaned, At: closedAt}}

	// Act
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		promptEntry("turn-2", "second"),
		promptEntry("turn-1", "first"),
	))

	// Assert
	ending := testEncode(feedid.Ref{WS: testWorkspace, Feed: rootFeed(), Row: feedid.RowKey{Kind: feedid.KindTurnEnded, ID: "turn-1"}}).GetValue()
	got := rowIDs(h.rows(rootFeed()))
	want := []string{h.promptRowID("turn-1"), ending, h.promptRowID("turn-2")}
	if len(got) != len(want) {
		t.Fatalf("rows = %v, want %v", got, want)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("rows = %v, want %v", got, want)
		}
	}
}

func TestAReplayedTurnWhoseTerminalIsOnThePageKeepsThatTerminal(t *testing.T) {
	// Arrange: the record says orphaned, but the page carries the turn's own
	// terminal.
	h := newHarness(t)
	h.closes = map[ids.TurnID]wsm.RecordedClose{"turn-1": {How: wsm.CloseOrphaned, At: closedAt}}

	// Act
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		frameEntry(mainAgent(), &conversationv1.AgentSuccess{
			Outcome: &conversationv1.AgentSuccess_Interrupted{Interrupted: &conversationv1.AgentInterrupted{}},
		}),
		promptEntry("turn-1", "hello"),
	))

	// Assert
	if got := endingArm(h.terminalRow("turn-1")); got != "interrupted" {
		t.Fatalf("arm = %q, want the page's own interrupted terminal", got)
	}
	if n := h.endingRows("turn-1"); n != 1 {
		t.Fatalf("ending rows = %d, want exactly 1", n)
	}
}

func TestAReplayedTurnWithNoRecordedCloseStaysRunning(t *testing.T) {
	// Arrange: the durable row is open, so the turn is still running.
	h := newHarness(t)

	// Act
	h.replay(historyPage(&conversationv1.HistoryFloor{}, promptEntry("turn-1", "hello")))

	// Assert
	if n := h.endingRows("turn-1"); n != 0 {
		t.Fatalf("ending rows = %d, want none for a turn still running", n)
	}
	if !h.only(rootFeed()).GetUserPrompt().GetWorking() {
		t.Fatalf("the running turn's prompt is not working")
	}
}

func TestAReplayedTurnAlreadyEndedLiveIsNotEndedAgain(t *testing.T) {
	// Arrange: the live feed ended the turn; a reopen replays it again.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "hello")
	h.closeTurn("turn-1", wsm.CloseAgentDied)
	h.closes = map[ids.TurnID]wsm.RecordedClose{"turn-1": {How: wsm.CloseAgentDied, At: closedAt}}

	// Act
	h.replay(historyPage(&conversationv1.HistoryFloor{}, promptEntry("turn-1", "hello")))

	// Assert
	if n := h.endingRows("turn-1"); n != 1 {
		t.Fatalf("ending rows = %d, want exactly 1", n)
	}
}

func TestAnUnreadableRecordedCloseIsAnErrorAndThePageStillDraws(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.closesErr = errors.New("database is closed")

	// Act
	h.replay(historyPage(&conversationv1.HistoryFloor{}, promptEntry("turn-1", "hello")))

	// Assert
	if !h.hasRecord(dlog.LevelError, "daemon.feed.turn_closes_unreadable") {
		t.Fatalf("records = %+v, want the failed read at ERROR", h.records())
	}
	if h.only(rootFeed()).GetUserPrompt() == nil {
		t.Fatalf("the page's prompt was not drawn")
	}
}

func TestAFoldedCloseDrawsNoEndingOfItsOwn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "port the footer")
	h.deliverFoldedPrompt("turn-2", "turn-1", "also cover the edge case")

	// Act
	h.closeTurn("turn-2", wsm.CloseFolded)

	// Assert
	if n := h.endingRows("turn-2") + h.endingRows("turn-1"); n != 0 {
		t.Fatalf("ending rows = %d, want none: the joined turn is still running", n)
	}
	if !h.hasRecord(dlog.LevelDebug, "daemon.feed.turn_closed_folded") {
		t.Fatalf("records = %+v, want the folded close recorded", h.records())
	}
}

func TestAReplayedTurnsRecordedEndingIsDrawnAtItsRecordedAddress(t *testing.T) {
	// Arrange: a repair turn recorded at a tab, closed with no
	// terminal on the page, then an ordinary turn.
	h := newHarness(t)
	lease := ids.LeaseID("lease-7")
	h.addresses = map[ids.TurnID]*wsm.OutputAddress{"turn-1": recordedTabAddress(lease), "turn-2": nil}
	h.closes = map[ids.TurnID]wsm.RecordedClose{"turn-1": {How: wsm.CloseOrphaned, At: closedAt}}

	// Act
	h.replay(historyPage(&conversationv1.HistoryFloor{},
		promptEntry("turn-2", "second"),
		promptEntry("turn-1", "resolve the conflict"),
	))

	// Assert: the ending is in the tab, not drawn as one of the ordinary turn's.
	ending := testEncode(feedid.Ref{WS: testWorkspace, Feed: feedid.Feed{Merge: &lease}, Row: feedid.RowKey{Kind: feedid.KindTurnEnded, ID: "turn-1"}}).GetValue()
	if _, found := h.findRow(feedid.Feed{Merge: &lease}, ending); !found {
		t.Fatalf("tab rows = %v, want the recorded ending %q among them", rowIDs(h.rows(feedid.Feed{Merge: &lease})), ending)
	}
}
