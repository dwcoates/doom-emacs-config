package footer

import (
	"fmt"
	"testing"
	"time"

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

func TestNewestTurnNamesTheFirstEntryCarryingATurn(t *testing.T) {
	tests := []struct {
		name string
		page *conversationv1.HistoryPage
		want string
	}{
		{
			name: "the first entry's turn is the newest",
			page: usagePage(
				pageEntry{turn: "turn-2", agent: mainAgent, unit: "u-2"},
				pageEntry{turn: "turn-1", agent: mainAgent, unit: "u-1"},
			),
			want: "turn-2",
		},
		{
			name: "an entry naming no turn is skipped",
			page: usagePage(
				pageEntry{agent: mainAgent, unit: "x-1"},
				pageEntry{turn: "turn-1", agent: mainAgent, unit: "u-1"},
			),
			want: "turn-1",
		},
		{
			name: "a page naming no turn names none",
			page: usagePage(pageEntry{agent: mainAgent, unit: "x-1"}),
			want: "",
		},
		{
			name: "an empty page names none",
			page: usagePage(),
			want: "",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the page above.

			// Act
			got := newestTurn(tt.page)

			// Assert
			if got != tt.want {
				t.Fatalf("newestTurn = %q, want %q", got, tt.want)
			}
		})
	}
}

// spanEntry is one replayed entry a clock is restated from: a user prompt, a
// main-agent terminal ("success" or "failure"), or an activity frame, placed at
// atMs. received places it by the store's stand-in rather than a recorded place.
type spanEntry struct {
	turn     string
	kind     string
	agent    *conversationv1.AgentId
	atMs     int64
	received bool
}

// spanPage is a replayed page of the entries above, newest first.
func spanPage(entries ...spanEntry) *conversationv1.HistoryPage {
	page := &conversationv1.HistoryPage{
		Boundary: &conversationv1.HistoryPage_Floor{Floor: &conversationv1.HistoryFloor{}},
	}
	for i, e := range entries {
		entry := &conversationv1.HistoryEntry{}
		frame := &conversationv1.AgentFrame{AgentId: e.agent}
		switch e.kind {
		case "prompt":
			entry.Entry = &conversationv1.HistoryEntry_UserPrompt{UserPrompt: &conversationv1.AgentPrompt{
				Id: &conversationv1.TurnId{Value: e.turn}, Agent: e.agent,
			}}
		case "success":
			frame.Result = &conversationv1.AgentFrame_Success{Success: completed()}
			entry.Entry = &conversationv1.HistoryEntry_AgentFrame{AgentFrame: frame}
		case "failure":
			frame.Result = &conversationv1.AgentFrame_Failure{Failure: &conversationv1.AgentFailure{
				Failure: &conversationv1.AgentFailure_MaxTurns{MaxTurns: &conversationv1.AgentMaxTurnsReached{}},
			}}
			entry.Entry = &conversationv1.HistoryEntry_AgentFrame{AgentFrame: frame}
		default:
			frame.Result = &conversationv1.AgentFrame_Update{Update: &conversationv1.AgentUpdate{
				Update: &conversationv1.AgentUpdate_Activity{Activity: responseFrame(fmt.Sprintf("u-%d", i), "success", nil)},
			}}
			entry.Entry = &conversationv1.HistoryEntry_AgentFrame{AgentFrame: frame}
		}
		at := &conversationv1.HistoryEntryAt{
			At:    &conversationv1.HistoryPointer{Value: fmt.Sprintf("ptr-%d", i)},
			Entry: entry,
			Turn:  &conversationv1.TurnId{Value: e.turn},
		}
		place := &conversationv1.ConversationPlace{AtMs: e.atMs}
		if e.received {
			at.Place = &conversationv1.HistoryEntryAt_ReceivedPlace{ReceivedPlace: place}
		} else {
			at.Place = &conversationv1.HistoryEntryAt_RecordedPlace{RecordedPlace: place}
		}
		page.Entries = append(page.Entries, at)
	}
	return page
}

// restoringClockPage is a page whose newest turn ran from 1000 to 43000.
func restoringClockPage() *conversationv1.HistoryPage {
	return spanPage(
		spanEntry{turn: "turn-2", kind: "success", agent: mainAgent, atMs: 43_000},
		spanEntry{turn: "turn-2", kind: "activity", agent: mainAgent, atMs: 20_000},
		spanEntry{turn: "turn-2", kind: "prompt", agent: mainAgent, atMs: 1_000},
	)
}

func TestLastTurnSpanReadsThePromptAndTheTerminal(t *testing.T) {
	tests := []struct {
		name        string
		page        *conversationv1.HistoryPage
		wantSpan    *turnSpan
		wantMissing string
	}{
		{
			name:     "a completed turn spans its prompt to its terminal",
			page:     restoringClockPage(),
			wantSpan: &turnSpan{startedAt: time.UnixMilli(1_000), endedAt: time.UnixMilli(43_000)},
		},
		{
			name: "a failed turn's terminal ends it too",
			page: spanPage(
				spanEntry{turn: "turn-2", kind: "failure", agent: mainAgent, atMs: 9_000},
				spanEntry{turn: "turn-2", kind: "prompt", agent: mainAgent, atMs: 1_000},
			),
			wantSpan: &turnSpan{startedAt: time.UnixMilli(1_000), endedAt: time.UnixMilli(9_000)},
		},
		{
			name: "a store-received place stands in for a recorded one",
			page: spanPage(
				spanEntry{turn: "turn-2", kind: "success", agent: mainAgent, atMs: 9_000, received: true},
				spanEntry{turn: "turn-2", kind: "prompt", agent: mainAgent, atMs: 1_000, received: true},
			),
			wantSpan: &turnSpan{startedAt: time.UnixMilli(1_000), endedAt: time.UnixMilli(9_000)},
		},
		{
			name: "a turn whose prompt is past the page's boundary is not restated",
			page: spanPage(
				spanEntry{turn: "turn-2", kind: "success", agent: mainAgent, atMs: 9_000},
				spanEntry{turn: "turn-2", kind: "activity", agent: mainAgent, atMs: 5_000},
			),
			wantMissing: "prompt",
		},
		{
			name: "a turn with no terminal is not restated",
			page: spanPage(
				spanEntry{turn: "turn-2", kind: "activity", agent: mainAgent, atMs: 5_000},
				spanEntry{turn: "turn-2", kind: "prompt", agent: mainAgent, atMs: 1_000},
			),
			wantMissing: "terminal",
		},
		{
			name: "another agent's terminal does not end the main agent's turn",
			page: spanPage(
				spanEntry{turn: "turn-2", kind: "success", agent: detachedAgent, atMs: 9_000},
				spanEntry{turn: "turn-2", kind: "prompt", agent: mainAgent, atMs: 1_000},
			),
			wantMissing: "terminal",
		},
		{
			name: "an older turn's entries never stand in for the newest turn's",
			page: spanPage(
				spanEntry{turn: "turn-2", kind: "activity", agent: mainAgent, atMs: 50_000},
				spanEntry{turn: "turn-1", kind: "success", agent: mainAgent, atMs: 9_000},
				spanEntry{turn: "turn-1", kind: "prompt", agent: mainAgent, atMs: 1_000},
			),
			wantMissing: "prompt",
		},
		{
			name:        "a page naming no turn restates nothing",
			page:        usagePage(pageEntry{agent: mainAgent, unit: "x-1"}),
			wantMissing: "turn",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the page above.

			// Act
			_, span, missing := lastTurnSpan(tt.page, mainAgent.GetValue())

			// Assert
			switch {
			case tt.wantSpan == nil && span != nil:
				t.Fatalf("span = %+v, want none", span)
			case tt.wantSpan != nil && (span == nil || !span.startedAt.Equal(tt.wantSpan.startedAt) || !span.endedAt.Equal(tt.wantSpan.endedAt)):
				t.Fatalf("span = %+v, want %+v", span, tt.wantSpan)
			case missing != tt.wantMissing:
				t.Fatalf("missing = %q, want %q", missing, tt.wantMissing)
			}
		})
	}
}

// TestARelaunchedDaemonStandsTheLastTurnsClock: the clock outlives its turn
// until the next submission, as the tokens figure does, and a relaunch is not
// a submission, so the main agent's opening page restates it.
func TestARelaunchedDaemonStandsTheLastTurnsClock(t *testing.T) {
	turn := testTurnID
	tests := []struct {
		name        string
		act         func(h *harness)
		wantStarted *int64
		wantEnded   *int64
	}{
		{
			name:        "the newest turn's span is restated",
			act:         func(h *harness) { h.r.OnHistoryPage(testWS, mainAgent, restoringClockPage()) },
			wantStarted: ptr(int64(1_000)),
			wantEnded:   ptr(int64(43_000)),
		},
		{
			name: "a page lacking the newest turn's terminal leaves the clock idle",
			act: func(h *harness) {
				h.r.OnHistoryPage(testWS, mainAgent, spanPage(
					spanEntry{turn: "turn-2", kind: "prompt", agent: mainAgent, atMs: 1_000},
				))
			},
		},
		{
			name: "a later page of older turns is not read",
			act: func(h *harness) {
				h.r.OnHistoryPage(testWS, mainAgent, spanPage(
					spanEntry{turn: "turn-2", kind: "prompt", agent: mainAgent, atMs: 50_000},
				))
				h.r.OnHistoryPage(testWS, mainAgent, restoringClockPage())
			},
		},
		{
			name: "a subagent's page is not the main agent's",
			act: func(h *harness) {
				h.r.OnHistoryPage(testWS, detachedAgent, spanPage(
					spanEntry{turn: "turn-2", kind: "success", agent: detachedAgent, atMs: 43_000},
					spanEntry{turn: "turn-2", kind: "prompt", agent: detachedAgent, atMs: 1_000},
				))
			},
		},
		{
			name: "a turn this daemon watched is not replaced",
			act: func(h *harness) {
				h.r.SetTurn(testWS, &TurnStarted{At: instant})
				h.clock.Advance(7 * time.Second)
				h.r.OnAgentTerminal(testWS, mainAgent, &turn, completed(), nil)
				h.r.OnHistoryPage(testWS, mainAgent, restoringClockPage())
			},
			wantStarted: ptr(instant.UnixMilli()),
			wantEnded:   ptr(instant.Add(7 * time.Second).UnixMilli()),
		},
		{
			name: "the next submission clears the restored clock",
			act: func(h *harness) {
				h.r.OnHistoryPage(testWS, mainAgent, restoringClockPage())
				h.r.SetTurn(testWS, &TurnStarted{At: instant})
			},
			wantStarted: ptr(instant.UnixMilli()),
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
			started, ended := clockSpan(t, h)
			if !sameMs(started, tt.wantStarted) || !sameMs(ended, tt.wantEnded) {
				t.Fatalf("clock = (%v, %v), want (%v, %v)", deref(started), deref(ended), deref(tt.wantStarted), deref(tt.wantEnded))
			}
		})
	}
}

// TestRestoringTheLastTurnsClockIsRecorded: the restoration, and a main page
// that restores no clock, each leave a record.
func TestRestoringTheLastTurnsClockIsRecorded(t *testing.T) {
	tests := []struct {
		name      string
		page      *conversationv1.HistoryPage
		level     string
		operation string
	}{
		{
			name:      "a restored clock is recorded at INFO",
			page:      restoringClockPage(),
			level:     dlog.LevelInfo,
			operation: "daemon.footer.clock_restored",
		},
		{
			name:      "a page lacking the newest turn's span is recorded at DEBUG",
			page:      spanPage(spanEntry{turn: "turn-2", kind: "prompt", agent: mainAgent, atMs: 1_000}),
			level:     dlog.LevelDebug,
			operation: "daemon.footer.clock_not_restored",
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

// sameMs reports whether two optional instants are both unset or equal.
func sameMs(got, want *int64) bool {
	if got == nil || want == nil {
		return got == want
	}
	return *got == *want
}

// deref prints an optional instant, "unset" when nil.
func deref(v *int64) string {
	if v == nil {
		return "unset"
	}
	return fmt.Sprint(*v)
}
