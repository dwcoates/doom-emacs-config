package footer

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
)

// replayedPage is a resumed session's opening catch-up page carrying one prior
// prompt — the only statement a relaunched daemon gets that a turn ever ran.
func replayedPage() *conversationv1.HistoryPage {
	return &conversationv1.HistoryPage{
		Entries: []*conversationv1.HistoryEntryAt{{
			At: &conversationv1.HistoryPointer{Value: "ptr-1"},
			Entry: &conversationv1.HistoryEntry{
				Entry: &conversationv1.HistoryEntry_UserPrompt{UserPrompt: &conversationv1.AgentPrompt{
					Id:    &conversationv1.TurnId{Value: string(testTurnID)},
					Agent: mainAgent,
				}},
			},
		}},
		Boundary: &conversationv1.HistoryPage_Floor{Floor: &conversationv1.HistoryFloor{}},
	}
}

// TestIdleIsDoneAfterAReplayedPageSaysATurnRan is the RELAUNCH's footer. The
// prior turn's open edge and terminal belonged to a daemon process that is
// gone, so without the page the strip would report a rehydrated conversation
// as one that has never run.
func TestIdleIsDoneAfterAReplayedPageSaysATurnRan(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnHistoryPage(testWS, mainAgent, replayedPage())

	// Assert
	idle := h.view(t).GetStrip().GetStatus().GetIdle()
	if idle.GetDone() == nil {
		t.Fatalf("substatus = %+v, want done", idle.GetSubstatus())
	}
}

// pageEntry is one replayed activity frame, in the turn it names ("" names
// none).
type pageEntry struct {
	turn  string
	agent *conversationv1.AgentId
	unit  string
	usage *conversationv1.TokenUsage
}

// usagePage is a replayed page of settled activity frames, newest first as a
// page is served.
func usagePage(entries ...pageEntry) *conversationv1.HistoryPage {
	page := &conversationv1.HistoryPage{
		Boundary: &conversationv1.HistoryPage_Floor{Floor: &conversationv1.HistoryFloor{}},
	}
	for _, e := range entries {
		at := &conversationv1.HistoryEntryAt{
			At: &conversationv1.HistoryPointer{Value: "ptr-" + e.unit},
			Entry: &conversationv1.HistoryEntry{Entry: &conversationv1.HistoryEntry_AgentFrame{AgentFrame: &conversationv1.AgentFrame{
				AgentId: e.agent,
				Result: &conversationv1.AgentFrame_Update{Update: &conversationv1.AgentUpdate{
					Update: &conversationv1.AgentUpdate_Activity{Activity: responseFrame(e.unit, "success", e.usage)},
				}},
			}}},
		}
		if e.turn != "" {
			at.Turn = &conversationv1.TurnId{Value: e.turn}
		}
		page.Entries = append(page.Entries, at)
	}
	return page
}

// restoringPage is a page whose newest turn's main agent spent 18.2k fresh.
func restoringPage() *conversationv1.HistoryPage {
	return usagePage(pageEntry{turn: "turn-2", agent: mainAgent, unit: "u-2", usage: usage(0, 18_200, 0, 0, 0)})
}

// TestARelaunchedDaemonStandsTheLastTurnsFigure: the cell's figure outlives
// its turn until the next submission, and a relaunch is not a submission, so
// the main agent's opening page restates it.
func TestARelaunchedDaemonStandsTheLastTurnsFigure(t *testing.T) {
	turn := testTurnID
	tests := []struct {
		name string
		act  func(h *harness)
		want string
	}{
		{
			name: "the newest turn's main usage is summed",
			act: func(h *harness) {
				h.r.OnHistoryPage(testWS, mainAgent, usagePage(
					pageEntry{turn: "turn-2", agent: mainAgent, unit: "u-3", usage: usage(0, 8_200, 0, 0, 0)},
					pageEntry{turn: "turn-2", agent: mainAgent, unit: "u-2", usage: usage(90_000, 10_000, 0, 500, 0)},
				))
			},
			want: "18.2k in",
		},
		{
			name: "an older turn on the page is not counted",
			act: func(h *harness) {
				h.r.OnHistoryPage(testWS, mainAgent, usagePage(
					pageEntry{turn: "turn-2", agent: mainAgent, unit: "u-2", usage: usage(0, 18_200, 0, 0, 0)},
					pageEntry{turn: "turn-1", agent: mainAgent, unit: "u-1", usage: usage(0, 40_000, 0, 0, 0)},
				))
			},
			want: "18.2k in",
		},
		{
			name: "another agent's frame on the page is left out",
			act: func(h *harness) {
				h.r.OnHistoryPage(testWS, mainAgent, usagePage(
					pageEntry{turn: "turn-2", agent: detachedAgent, unit: "s-1", usage: usage(0, 50_000, 0, 0, 0)},
					pageEntry{turn: "turn-2", agent: mainAgent, unit: "u-2", usage: usage(0, 18_200, 0, 0, 0)},
				))
			},
			want: "18.2k in",
		},
		{
			name: "an entry naming no turn is skipped",
			act: func(h *harness) {
				h.r.OnHistoryPage(testWS, mainAgent, usagePage(
					pageEntry{agent: mainAgent, unit: "x-1", usage: usage(0, 50_000, 0, 0, 0)},
					pageEntry{turn: "turn-2", agent: mainAgent, unit: "u-2", usage: usage(0, 18_200, 0, 0, 0)},
				))
			},
			want: "18.2k in",
		},
		{
			name: "a newest turn that stated no usage leaves the cell idle",
			act: func(h *harness) {
				h.r.OnHistoryPage(testWS, mainAgent, usagePage(
					pageEntry{turn: "turn-2", agent: mainAgent, unit: "u-2"},
					pageEntry{turn: "turn-1", agent: mainAgent, unit: "u-1", usage: usage(0, 40_000, 0, 0, 0)},
				))
			},
			want: "--",
		},
		{
			name: "a later page of older turns is not read",
			act: func(h *harness) {
				h.r.OnHistoryPage(testWS, mainAgent, usagePage(pageEntry{turn: "turn-2", agent: mainAgent, unit: "u-2"}))
				h.r.OnHistoryPage(testWS, mainAgent, usagePage(
					pageEntry{turn: "turn-1", agent: mainAgent, unit: "u-1", usage: usage(0, 40_000, 0, 0, 0)},
				))
			},
			want: "--",
		},
		{
			name: "a subagent's page is not the main agent's",
			act: func(h *harness) {
				h.r.OnHistoryPage(testWS, detachedAgent, usagePage(
					pageEntry{turn: "turn-2", agent: detachedAgent, unit: "s-1", usage: usage(0, 50_000, 0, 0, 0)},
				))
			},
			want: "--",
		},
		{
			name: "a turn this daemon watched is not replaced",
			act: func(h *harness) {
				h.r.SetTurn(testWS, &TurnStarted{At: instant})
				h.r.OnActivity(testWS, mainAgent, responseFrame("w-1", "success", usage(0, 1_000, 0, 0, 0)))
				h.r.OnAgentTerminal(testWS, mainAgent, &turn, completed(), nil)
				h.r.OnHistoryPage(testWS, mainAgent, restoringPage())
			},
			want: "1k in",
		},
		{
			name: "the next submission clears the restored figure",
			act: func(h *harness) {
				h.r.OnHistoryPage(testWS, mainAgent, restoringPage())
				h.r.SetTurn(testWS, &TurnStarted{At: instant})
			},
			want: "0 in",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)

			// Act
			tt.act(h)

			// Assert
			if got := cellText(t, h); got != tt.want {
				t.Fatalf("cell = %q, want %q", got, tt.want)
			}
		})
	}
}

// TestARestoredTurnCarriesNoVerdict: the reconciliation's denominator belonged
// to the process that watched the turn, so no verdict is claimed for it.
func TestARestoredTurnCarriesNoVerdict(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnHistoryPage(testWS, mainAgent, restoringPage())

	// Assert
	if v := h.view(t).GetStrip().GetTokens().GetVerdict(); v != nil {
		t.Fatalf("verdict = %v, want unset on a restored turn", v)
	}
}

// TestARestoredTurnStandsItsAlarm: the alarm is the turn's, kept until the
// next turn, so a relaunch restates it with the figure.
func TestARestoredTurnStandsItsAlarm(t *testing.T) {
	// Arrange
	h := newHarness(t, WithTokenAlarmThreshold(10_000))
	connected(h)

	// Act
	h.r.OnHistoryPage(testWS, mainAgent, restoringPage())

	// Assert
	if h.view(t).GetStrip().GetTokens().GetAlarm() == nil {
		t.Fatalf("the restored turn's alarm is missing")
	}
}

// TestRestoringTheLastTurnIsRecorded: the restoration, and a main page that
// restores nothing, each leave a record.
func TestRestoringTheLastTurnIsRecorded(t *testing.T) {
	tests := []struct {
		name      string
		page      *conversationv1.HistoryPage
		level     string
		operation string
	}{
		{
			name:      "a restored figure is recorded at INFO",
			page:      restoringPage(),
			level:     dlog.LevelInfo,
			operation: "daemon.footer.tokens_restored",
		},
		{
			name:      "a page with no usage for its newest turn is recorded at DEBUG",
			page:      usagePage(pageEntry{turn: "turn-2", agent: mainAgent, unit: "u-2"}),
			level:     dlog.LevelDebug,
			operation: "daemon.footer.tokens_not_restored",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)

			// Act
			h.r.OnHistoryPage(testWS, mainAgent, tt.page)

			// Assert
			if !hasLevel(h.log.Records(), tt.level, tt.operation) {
				t.Fatalf("records = %+v, want %s %s", h.log.Records(), tt.level, tt.operation)
			}
		})
	}
}

// TestIdleStaysReadyAfterAnEmptyReplayedPage is the FRESH conversation's
// footer: a floor page with no entries states that nothing has ever run, and
// reading it as a turn would tell every new workspace its conversation had
// already happened.
func TestIdleStaysReadyAfterAnEmptyReplayedPage(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnHistoryPage(testWS, mainAgent, &conversationv1.HistoryPage{
		Boundary: &conversationv1.HistoryPage_Floor{Floor: &conversationv1.HistoryFloor{}},
	})

	// Assert
	idle := h.view(t).GetStrip().GetStatus().GetIdle()
	if idle.GetReady() == nil {
		t.Fatalf("substatus = %+v, want ready", idle.GetSubstatus())
	}
}
