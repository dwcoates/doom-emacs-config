package footer

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
)

// usage builds one canonical token record.
func usage(read, written, unwritten, output, thinking uint64) *conversationv1.TokenUsage {
	return &conversationv1.TokenUsage{
		InputHits:            &conversationv1.TokenCacheHits{Read: read},
		InputMisses:          &conversationv1.TokenCacheMisses{Written: written, Unwritten: unwritten},
		OutputTokens:         output,
		OutputThinkingTokens: thinking,
	}
}

// responseFrame is one frame of a response unit, optionally carrying the
// response's usage.
func responseFrame(unit, stage string, u *conversationv1.TokenUsage) *conversationv1.AgentActivity {
	resp := &conversationv1.AgentResponse{}
	switch stage {
	case "start":
		resp.Result = &conversationv1.AgentResponse_Start{Start: &conversationv1.AgentResponseStart{}}
	case "update":
		resp.Result = &conversationv1.AgentResponse_Update{Update: &conversationv1.AgentResponseUpdate{}}
	default:
		resp.Result = &conversationv1.AgentResponse_Success{Success: &conversationv1.AgentResponseSuccess{}}
	}
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: unit},
		Usage:      u,
		Item:       &conversationv1.AgentActivity_Response{Response: resp},
	}
}

// thinkingFrame is one settled thinking unit, optionally carrying the API
// response's usage — which is where usage rides whenever the response's first
// content block is a reasoning block.
func thinkingFrame(unit string, u *conversationv1.TokenUsage) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: unit},
		Usage:      u,
		Item: &conversationv1.AgentActivity_Thinking{Thinking: &conversationv1.AgentThinking{
			Result: &conversationv1.AgentThinking_Success{Success: &conversationv1.AgentThinkingSuccess{}},
		}},
	}
}

// panelInput is the panel's summed uncached-input line — the spend across
// every agent, which is what the strip's cell used to show before it became
// the main agent's context growth.
func panelInput(t *testing.T, h *harness) string {
	t.Helper()
	return h.view(t).GetExpanded().GetTokens().GetInput().GetValue()
}

func TestThePanelSumsTheUncachedInputFigure(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	h.r.OnActivity(testWS, mainAgent, responseFrame("unit-1", "success", usage(90_000, 18_000, 200, 500, 100)))

	// Assert
	got := panelInput(t, h)
	if got != "18.2k" {
		t.Fatalf("panel input = %q, want the input_misses total (written + unwritten)", got)
	}
}

func TestUsageIsSummedAcrossTheTurnsUnits(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	h.r.OnActivity(testWS, mainAgent, responseFrame("unit-1", "success", usage(0, 1_000, 0, 0, 0)))
	h.r.OnActivity(testWS, mainAgent, responseFrame("unit-2", "success", usage(0, 2_000, 0, 0, 0)))

	// Assert
	got := panelInput(t, h)
	if got != "3k" {
		t.Fatalf("panel input = %q, want 3k across two carrying units", got)
	}
}

func TestARepeatedFrameDoesNotDoubleCountItsUsage(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	frame := responseFrame("unit-1", "update", usage(0, 5_000, 0, 0, 0))

	// Act: the same unit upserts, re-reporting the same usage each time.
	h.r.OnActivity(testWS, mainAgent, frame)
	h.r.OnActivity(testWS, mainAgent, frame)
	h.r.OnActivity(testWS, mainAgent, frame)

	// Assert
	got := panelInput(t, h)
	if got != "5k" {
		t.Fatalf("panel input = %q, want 5k: usage is keyed by unit and REPLACED, never added", got)
	}
}

func TestNoVerdictExistsWhileTheTurnRuns(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	h.r.OnActivity(testWS, mainAgent, responseFrame("unit-1", "success", usage(0, 100, 0, 0, 0)))

	// Assert
	if h.view(t).GetStrip().GetTokens().GetVerdict() != nil {
		t.Fatalf("a running turn carries a verdict; UNSET is the running state")
	}
}

func TestASettledTurnWithEveryUsageObservedReconcilesComplete(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	turn := testTurnID
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnActivity(testWS, mainAgent, responseFrame("unit-1", "success", usage(0, 100, 0, 0, 0)))

	// Act
	h.r.OnAgentTerminal(testWS, mainAgent, &turn, completed(), nil)

	// Assert
	if h.view(t).GetStrip().GetTokens().GetVerdict().GetComplete() == nil {
		t.Fatalf("verdict = %+v, want complete", h.view(t).GetStrip().GetTokens().GetVerdict())
	}
}

func TestAResponseWithNoUsageReconcilesIncomplete(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	turn := testTurnID
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	// Two responses settle before any usage is ever seen — nothing accounts for
	// the API response they arrived in — and a later one carries its own.
	h.r.OnActivity(testWS, mainAgent, responseFrame("unit-1", "success", nil))
	h.r.OnActivity(testWS, mainAgent, responseFrame("unit-2", "success", nil))
	h.r.OnActivity(testWS, mainAgent, responseFrame("unit-3", "success", usage(0, 100, 0, 0, 0)))

	// Act
	h.r.OnAgentTerminal(testWS, mainAgent, &turn, completed(), nil)

	// Assert
	line := h.view(t).GetExpanded().GetTokens().GetVerdict().GetIncomplete()
	if line.GetText() != "2 responses missing usage" {
		t.Fatalf("evidence = %q, want the count of responses with no usage", line.GetText())
	}
}

func TestAResponseWhoseUsageRodeTheThinkingUnitThatOpenedItReconcilesComplete(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	turn := testTurnID
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	// The ordinary prose turn: usage rides block 0 — the reasoning block — and
	// the response unit that follows carries none of its own.
	h.r.OnActivity(testWS, mainAgent, thinkingFrame("unit-0", usage(0, 100, 0, 0, 0)))
	h.r.OnActivity(testWS, mainAgent, responseFrame("unit-1", "success", nil))

	// Act
	h.r.OnAgentTerminal(testWS, mainAgent, &turn, completed(), nil)

	// Assert
	if h.view(t).GetStrip().GetTokens().GetVerdict().GetComplete() == nil {
		t.Fatalf("verdict = %+v, want complete: absent usage means \"not the carrying unit\", never \"free\"",
			h.view(t).GetStrip().GetTokens().GetVerdict())
	}
}

func TestTwoResponseUnitsOfOneApiResponseShareItsSingleUsageStamp(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	turn := testTurnID
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	// One API response written [text, tool_use, text]: two response units, one
	// usage stamp, which accounts for both.
	h.r.OnActivity(testWS, mainAgent, responseFrame("unit-0", "success", usage(0, 100, 0, 0, 0)))
	h.r.OnActivity(testWS, mainAgent, responseFrame("unit-2", "success", nil))

	// Act
	h.r.OnAgentTerminal(testWS, mainAgent, &turn, completed(), nil)

	// Assert
	if h.view(t).GetStrip().GetTokens().GetVerdict().GetComplete() == nil {
		t.Fatalf("verdict = %+v, want complete: one API response's usage accounts for every unit it produced",
			h.view(t).GetStrip().GetTokens().GetVerdict())
	}
}

func TestUsageArrivingAfterTheTurnSettledCorrectsTheVerdict(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	turn := testTurnID
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnActivity(testWS, mainAgent, responseFrame("unit-1", "success", nil))
	h.r.OnAgentTerminal(testWS, mainAgent, &turn, completed(), nil)
	if h.view(t).GetStrip().GetTokens().GetVerdict().GetIncomplete() == nil {
		t.Fatalf("a settled turn with no usage at all reconciles incomplete until some lands")
	}

	// Act: the response's usage upserts onto the unit it was already filed under.
	h.r.OnActivity(testWS, mainAgent, responseFrame("unit-1", "success", usage(0, 100, 0, 0, 0)))

	// Assert
	if h.view(t).GetStrip().GetTokens().GetVerdict().GetComplete() == nil {
		t.Fatalf("verdict = %+v, want complete once the late usage lands",
			h.view(t).GetStrip().GetTokens().GetVerdict())
	}
}

func TestOneMissingResponseIsCountedInTheSingular(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	turn := testTurnID
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnActivity(testWS, mainAgent, responseFrame("unit-1", "success", nil))

	// Act
	h.r.OnAgentTerminal(testWS, mainAgent, &turn, completed(), nil)

	// Assert
	line := h.view(t).GetExpanded().GetTokens().GetVerdict().GetIncomplete()
	if line.GetText() != "1 response missing usage" {
		t.Fatalf("evidence = %q, want the singular", line.GetText())
	}
}

func TestAUnitReportingTwoDifferentUsagesReconcilesInvalid(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	turn := testTurnID
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnActivity(testWS, mainAgent, responseFrame("unit-1", "update", usage(0, 100, 0, 0, 0)))

	// Act
	h.r.OnActivity(testWS, mainAgent, responseFrame("unit-1", "success", usage(0, 900, 0, 0, 0)))
	h.r.OnAgentTerminal(testWS, mainAgent, &turn, completed(), nil)

	// Assert
	line := h.view(t).GetExpanded().GetTokens().GetVerdict().GetInvalid()
	if line == nil || line.GetText() == "" {
		t.Fatalf("verdict = %+v, want invalid with the problem named", line)
	}
}

func TestThinkingAboveOutputReconcilesInvalid(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	turn := testTurnID
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	h.r.OnActivity(testWS, mainAgent, responseFrame("unit-1", "success", usage(0, 10, 0, 5, 9)))
	h.r.OnAgentTerminal(testWS, mainAgent, &turn, completed(), nil)

	// Assert
	if h.view(t).GetStrip().GetTokens().GetVerdict().GetInvalid() == nil {
		t.Fatalf("thinking tokens above output tokens must reconcile invalid")
	}
}

func TestThinkingIsNeverAddedToOutput(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	h.r.OnActivity(testWS, mainAgent, responseFrame("unit-1", "success", usage(0, 0, 0, 900, 400)))

	// Assert
	panel := h.view(t).GetExpanded().GetTokens()
	if panel.GetOutput().GetValue() != "900" {
		t.Fatalf("output = %q, want the vendor's total unchanged", panel.GetOutput().GetValue())
	}
	if panel.GetThinking().GetValue() != "400" {
		t.Fatalf("thinking = %q, want the partition drawn on its own line", panel.GetThinking().GetValue())
	}
}

func TestTheAlarmTripsPastTheThreshold(t *testing.T) {
	// Arrange
	h := newHarness(t, WithTokenAlarmThreshold(20_000))
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	h.r.OnActivity(testWS, mainAgent, responseFrame("unit-1", "success", usage(0, 61_000, 0, 0, 0)))

	// Assert
	if h.view(t).GetStrip().GetTokens().GetAlarm() == nil {
		t.Fatalf("the alarm glyph is absent past the threshold")
	}
	got := h.view(t).GetExpanded().GetTokens().GetAlarm().GetText()
	if got != "expensive turn — 41k over 20k" {
		t.Fatalf("alarm sentence = %q, want the composed over-threshold sentence", got)
	}
}

func TestTheAlarmDoesNotTripAtTheThreshold(t *testing.T) {
	// Arrange
	h := newHarness(t, WithTokenAlarmThreshold(20_000))
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	h.r.OnActivity(testWS, mainAgent, responseFrame("unit-1", "success", usage(0, 20_000, 0, 0, 0)))

	// Assert
	if h.view(t).GetStrip().GetTokens().GetAlarm() != nil {
		t.Fatalf("the alarm tripped AT the threshold; it trips only past it")
	}
}

func TestANewTurnResetsTheCellAndTheAlarm(t *testing.T) {
	// Arrange
	h := newHarness(t, WithTokenAlarmThreshold(1_000))
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnActivity(testWS, mainAgent, responseFrame("unit-1", "success", usage(0, 50_000, 0, 0, 0)))

	// Act
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Assert
	cell := h.view(t).GetStrip().GetTokens()
	if cell.GetAlarm() != nil {
		t.Fatalf("the alarm survived into the next turn")
	}
	if cell.GetInput().GetText() != "0 in" {
		t.Fatalf("cell = %q, want a reset figure", cell.GetInput().GetText())
	}
}

func TestTheAlarmSurvivesTheTurnsEnd(t *testing.T) {
	// Arrange
	h := newHarness(t, WithTokenAlarmThreshold(1_000))
	connected(h)
	turn := testTurnID
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnActivity(testWS, mainAgent, responseFrame("unit-1", "success", usage(0, 50_000, 0, 0, 0)))

	// Act
	h.r.OnAgentTerminal(testWS, mainAgent, &turn, completed(), nil)

	// Assert
	if h.view(t).GetStrip().GetTokens().GetAlarm() == nil {
		t.Fatalf("the alarm was cleared at the turn's end; it is kept until the NEXT turn")
	}
}

func TestFirstTokenLatencyIsMeasuredFromTheResponseStart(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnActivity(testWS, mainAgent, responseFrame("unit-1", "start", nil))

	// Act
	h.clock.Advance(412 * 1000 * 1000)
	h.r.OnActivity(testWS, mainAgent, responseFrame("unit-1", "update", nil))

	// Assert
	got := h.view(t).GetExpanded().GetTokens().GetFirstToken().GetValue()
	if got != "412ms" {
		t.Fatalf("first token = %q, want the latency to the first update", got)
	}
}

func TestFirstTokenLatencyIsUnsetBeforeTheFirstUpdate(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	h.r.OnActivity(testWS, mainAgent, responseFrame("unit-1", "start", nil))

	// Assert
	if h.view(t).GetExpanded().GetTokens().GetFirstToken().Value != nil {
		t.Fatalf("a latency was drawn before the first token landed")
	}
}

func TestPanelFiguresAreUnsetUntilAnyUsageIsKnown(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Assert
	panel := h.view(t).GetExpanded().GetTokens()
	if panel.GetInput().Value != nil {
		t.Fatalf("input = %v, want an empty slot until a figure is known", *panel.GetInput().Value)
	}
}

// detachedAgent is the created-agent id a detached run's own-book frames arrive
// under, distinct from the main agent's.
var detachedAgent = &conversationv1.AgentId{Value: "agent-2"}

func TestALiveDetachedAgentsUsageSurvivesTheTurnReset(t *testing.T) {
	// Arrange: a detached subagent is live and has reported real usage on its
	// own book.
	h := newHarness(t)
	connected(h)
	h.r.OnDetachedWork(testWS, mainAgent, detachedSubagentWork("work-1", detachedAgent.GetValue(), "Explore"))
	h.r.OnActivity(testWS, detachedAgent, responseFrame("det-1", "success", usage(0, 5_000, 0, 0, 0)))

	// Act: a new main turn opens, which resets the turn's accounting.
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Assert: the detached agent is still burning that input, so its spend
	// stands in the panel rather than falling back to the new turn's nothing.
	got := panelInput(t, h)
	if got != "5k" {
		t.Fatalf("panel input = %q, want the live detached agent's usage carried across the reset", got)
	}
}

func TestTheTurnResetKeepsTheDetachedAgentButDropsTheMainTurn(t *testing.T) {
	// Arrange: both the main turn and a live detached agent have spent input.
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnActivity(testWS, mainAgent, responseFrame("main-1", "success", usage(0, 3_000, 0, 0, 0)))
	h.r.OnDetachedWork(testWS, mainAgent, detachedSubagentWork("work-1", detachedAgent.GetValue(), "Explore"))
	h.r.OnActivity(testWS, detachedAgent, responseFrame("det-1", "success", usage(0, 5_000, 0, 0, 0)))

	// Act: the next turn opens.
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Assert: the reset is SELECTIVE — the concluded turn's own units go, the
	// live detached agent's stay, so the spend is the detached 5k alone.
	got := panelInput(t, h)
	if got != "5k" {
		t.Fatalf("panel input = %q, want the detached 5k with the main turn's 3k wiped", got)
	}
}

func TestARetiredDetachedAgentsUsageStopsCounting(t *testing.T) {
	// Arrange: a detached subagent has spent input.
	h := newHarness(t)
	connected(h)
	h.r.OnDetachedWork(testWS, mainAgent, detachedSubagentWork("work-1", detachedAgent.GetValue(), "Explore"))
	h.r.OnActivity(testWS, detachedAgent, responseFrame("det-1", "success", usage(0, 5_000, 0, 0, 0)))

	// Act: the run reaches its own terminal.
	h.r.OnSubagent(testWS, workID("work-1"), subagentSettled(false))

	// Assert: a settled run charges nothing more, so its background units leave
	// the panel rather than standing in it for the rest of the session.
	if got := panelInput(t, h); got != "" {
		t.Fatalf("panel input = %q, want the retired detached agent's usage dropped", got)
	}
}

func TestADetachedAgentsUsageIsNotDoubleCountedAcrossAReset(t *testing.T) {
	// Arrange: a detached agent's usage is carried across a turn reset.
	h := newHarness(t)
	connected(h)
	h.r.OnDetachedWork(testWS, mainAgent, detachedSubagentWork("work-1", detachedAgent.GetValue(), "Explore"))
	frame := responseFrame("det-1", "update", usage(0, 5_000, 0, 0, 0))
	h.r.OnActivity(testWS, detachedAgent, frame)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act: the same unit re-reports the same usage after the reset.
	h.r.OnActivity(testWS, detachedAgent, frame)

	// Assert: the carried unit keeps its key, so the re-report UPSERTS rather
	// than adding — 5k, never 10k.
	got := panelInput(t, h)
	if got != "5k" {
		t.Fatalf("panel input = %q, want 5k: a carried unit is replaced, never added", got)
	}
}

func TestAnInTurnSubagentsUsageDoesNotSurviveTheTurnReset(t *testing.T) {
	// Arrange: an IN-TURN subagent (no detached handle) has spent input on its
	// own book — it is the turn's own progress, not background work.
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnActivity(testWS, mainAgent, subagentStart("spawn-1", detachedAgent.GetValue(), "Explore", ""))
	h.r.OnActivity(testWS, detachedAgent, responseFrame("sub-1", "success", usage(0, 5_000, 0, 0, 0)))

	// Act: the next turn opens.
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Assert: only DETACHED agents are carried; an in-turn subagent resets with
	// the turn that owned it.
	if got := panelInput(t, h); got != "" {
		t.Fatalf("panel input = %q, want the in-turn subagent's usage wiped with the turn", got)
	}
}

func TestTheCacheSplitIsDrawnOnItsOwnLines(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	h.r.OnActivity(testWS, mainAgent, responseFrame("unit-1", "success", usage(7_000, 3_000, 500, 0, 0)))

	// Assert
	panel := h.view(t).GetExpanded().GetTokens()
	if panel.GetCacheRead().GetValue() != "7k" {
		t.Fatalf("cache read = %q, want the cheap bucket alone", panel.GetCacheRead().GetValue())
	}
	if panel.GetCacheWrite().GetValue() != "3k" {
		t.Fatalf("cache write = %q, want the written bucket alone", panel.GetCacheWrite().GetValue())
	}
	if panel.GetInput().GetValue() != "3.5k" {
		t.Fatalf("input = %q, want written + unwritten", panel.GetInput().GetValue())
	}
}

// ---- the cell: the main agent's context growth -----------------------------

// contextReading is one context_usage push stating the context held.
func contextReading(total int64) *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_ContextUsage{
		ContextUsage: &conversationv1.SessionContextUsage{TotalTokens: total, MaxTokens: 200_000},
	}}
}

// readContext pushes each reading in order.
func readContext(h *harness, totals ...int64) {
	for _, total := range totals {
		h.r.OnSessionUpdate(testWS, contextReading(total))
	}
}

// cellText is the strip's tokens cell figure.
func cellText(t *testing.T, h *harness) string {
	t.Helper()
	return h.view(t).GetStrip().GetTokens().GetInput().GetText()
}

// panelGrowth is the panel's context-growth line: the main agent's context
// growth, which the cell no longer shows.
func panelGrowth(t *testing.T, h *harness) string {
	t.Helper()
	return h.view(t).GetExpanded().GetTokens().GetContextGrowth().GetValue()
}

// cellHeat is the strip's tokens cell heat, nil while the cell is uncolored.
func cellHeat(t *testing.T, h *harness) *frontendv1.FooterTokensCellInputHeat {
	t.Helper()
	return h.view(t).GetStrip().GetTokens().GetInput().GetHeat()
}

func TestTheCellIsTheMainAgentsFreshInput(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		want    string
	}{
		{
			name: "cache writes and uncached input both count",
			arrange: func(h *harness) {
				h.r.SetTurn(testWS, &TurnStarted{At: instant})
				h.r.OnActivity(testWS, mainAgent, responseFrame("main-1", "success", usage(0, 18_000, 200, 0, 0)))
			},
			want: "18.2k in",
		},
		{
			name: "cache reads and output never count",
			arrange: func(h *harness) {
				h.r.SetTurn(testWS, &TurnStarted{At: instant})
				h.r.OnActivity(testWS, mainAgent, responseFrame("main-1", "success", usage(90_000, 1_000, 0, 5_000, 0)))
			},
			want: "1k in",
		},
		{
			name: "every main API response of the turn is summed",
			arrange: func(h *harness) {
				h.r.SetTurn(testWS, &TurnStarted{At: instant})
				h.r.OnActivity(testWS, mainAgent, responseFrame("main-1", "success", usage(0, 10_000, 0, 0, 0)))
				h.r.OnActivity(testWS, mainAgent, responseFrame("main-2", "success", usage(0, 8_200, 0, 0, 0)))
			},
			want: "18.2k in",
		},
		{
			name: "the context window's growth is not the figure",
			arrange: func(h *harness) {
				readContext(h, 100_000)
				h.r.SetTurn(testWS, &TurnStarted{At: instant})
				readContext(h, 140_000)
				h.r.OnActivity(testWS, mainAgent, responseFrame("main-1", "success", usage(0, 18_200, 0, 0, 0)))
			},
			want: "18.2k in",
		},
		{
			name: "an in-turn subagent's spend is excluded",
			arrange: func(h *harness) {
				h.r.SetTurn(testWS, &TurnStarted{At: instant})
				h.r.OnActivity(testWS, mainAgent, subagentStart("spawn-1", detachedAgent.GetValue(), "Explore", ""))
				h.r.OnActivity(testWS, detachedAgent, responseFrame("sub-1", "success", usage(0, 50_000, 0, 0, 0)))
				h.r.OnActivity(testWS, mainAgent, responseFrame("main-1", "success", usage(0, 18_200, 0, 0, 0)))
			},
			want: "18.2k in",
		},
		{
			name: "a detached agent's spend is excluded",
			arrange: func(h *harness) {
				h.r.OnDetachedWork(testWS, mainAgent, detachedSubagentWork("work-1", detachedAgent.GetValue(), "Explore"))
				h.r.OnActivity(testWS, detachedAgent, responseFrame("det-1", "success", usage(0, 50_000, 0, 0, 0)))
				h.r.SetTurn(testWS, &TurnStarted{At: instant})
				h.r.OnActivity(testWS, detachedAgent, responseFrame("det-2", "success", usage(0, 70_000, 0, 0, 0)))
				h.r.OnActivity(testWS, mainAgent, responseFrame("main-1", "success", usage(0, 18_200, 0, 0, 0)))
			},
			want: "18.2k in",
		},
		{
			name: "a turn whose main agent stated no usage yet reads zero",
			arrange: func(h *harness) {
				h.r.SetTurn(testWS, &TurnStarted{At: instant})
			},
			want: "0 in",
		},
		{
			name: "the previous turn's main spend does not carry over",
			arrange: func(h *harness) {
				h.r.SetTurn(testWS, &TurnStarted{At: instant})
				h.r.OnActivity(testWS, mainAgent, responseFrame("main-1", "success", usage(0, 40_000, 0, 0, 0)))
				h.r.SetTurn(testWS, &TurnStarted{At: instant})
				h.r.OnActivity(testWS, mainAgent, responseFrame("main-2", "success", usage(0, 1_000, 0, 0, 0)))
			},
			want: "1k in",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)

			// Act
			tt.arrange(h)

			// Assert
			if got := cellText(t, h); got != tt.want {
				t.Fatalf("cell = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestTheCellsHeatFollowsTheGradientStops(t *testing.T) {
	tests := []struct {
		name  string
		fresh uint64
		want  float64
	}{
		{name: "zero is green", fresh: 0, want: 0},
		{name: "halfway to the yellow stop", fresh: 15_000, want: 1.0 / 6},
		{name: "the yellow stop", fresh: 30_000, want: 1.0 / 3},
		{name: "halfway from yellow to orange", fresh: 40_000, want: 0.5},
		{name: "the orange stop", fresh: 50_000, want: 2.0 / 3},
		{name: "halfway from orange to red", fresh: 75_000, want: 5.0 / 6},
		{name: "the red stop", fresh: 100_000, want: 1},
		{name: "past the red stop holds at red", fresh: 400_000, want: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			h.r.SetTurn(testWS, &TurnStarted{At: instant})

			// Act
			h.r.OnActivity(testWS, mainAgent, responseFrame("main-1", "success", usage(0, tt.fresh, 0, 0, 0)))

			// Assert
			heat := cellHeat(t, h)
			if heat == nil {
				t.Fatal("heat = unset, want a position while the turn runs")
			}
			if diff := heat.GetPosition() - tt.want; diff > 1e-9 || diff < -1e-9 {
				t.Fatalf("heat = %v, want %v", heat.GetPosition(), tt.want)
			}
		})
	}
}

func TestThePanelsContextGrowthMovesWithEachMidTurnReading(t *testing.T) {
	tests := []struct {
		name     string
		readings []int64
		want     string
	}{
		{name: "the first reading after one response", readings: []int64{110_000}, want: "10k"},
		{name: "a later reading replaces it", readings: []int64{110_000, 125_000}, want: "25k"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			readContext(h, 100_000)
			h.r.SetTurn(testWS, &TurnStarted{At: instant})

			// Act
			readContext(h, tt.readings...)

			// Assert
			if got := panelGrowth(t, h); got != tt.want {
				t.Fatalf("panel growth = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestAContextCutRebasesTheGrowth(t *testing.T) {
	tests := []struct {
		name         string
		act          func(h *harness)
		wantGrowth   string
		wantSinceCut bool
	}{
		{
			name:         "a reading below the baseline is a cut, and the growth restarts from it",
			act:          func(h *harness) { readContext(h, 150_000, 30_000) },
			wantGrowth:   "0",
			wantSinceCut: true,
		},
		{
			name:         "growth after the cut is measured from the post-cut size",
			act:          func(h *harness) { readContext(h, 150_000, 30_000, 35_000) },
			wantGrowth:   "5k",
			wantSinceCut: true,
		},
		{
			name:         "a reading at the baseline is no cut",
			act:          func(h *harness) { readContext(h, 100_000) },
			wantGrowth:   "0",
			wantSinceCut: false,
		},
		{
			name: "the next turn opens without the since-cut marker",
			act: func(h *harness) {
				readContext(h, 150_000, 30_000)
				h.r.SetTurn(testWS, &TurnStarted{At: instant})
			},
			wantGrowth:   "0",
			wantSinceCut: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			readContext(h, 100_000)
			h.r.SetTurn(testWS, &TurnStarted{At: instant})

			// Act
			tt.act(h)

			// Assert
			if got := panelGrowth(t, h); got != tt.wantGrowth {
				t.Fatalf("panel growth = %q, want %q", got, tt.wantGrowth)
			}
			sinceCut := h.view(t).GetExpanded().GetTokens().GetContextGrowth().GetSinceCut() != nil
			if sinceCut != tt.wantSinceCut {
				t.Fatalf("since-cut marker = %v, want %v", sinceCut, tt.wantSinceCut)
			}
		})
	}
}

func TestTheIdleCellIsTheStatedDash(t *testing.T) {
	turn := testTurnID
	tests := []struct {
		name    string
		arrange func(h *harness)
	}{
		{
			name:    "no turn has ever run",
			arrange: func(h *harness) { readContext(h, 100_000) },
		},
		{
			name: "the turn ended",
			arrange: func(h *harness) {
				readContext(h, 100_000)
				h.r.SetTurn(testWS, &TurnStarted{At: instant})
				readContext(h, 118_200)
				h.r.OnAgentTerminal(testWS, mainAgent, &turn, completed(), nil)
			},
		},
		{
			name: "a context cut ended the turn",
			arrange: func(h *harness) {
				h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActCompact})
				h.r.OnContextCut(testWS, mainAgent, compactedCut())
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)

			// Act
			tt.arrange(h)

			// Assert
			if got := cellText(t, h); got != "--" {
				t.Fatalf("idle cell = %q, want the daemon's stated \"--\"", got)
			}
			if heat := cellHeat(t, h); heat != nil {
				t.Fatalf("idle heat = %v, want unset so the dash draws uncolored", heat)
			}
		})
	}
}

func TestTheIdleCellStillOpensAPopulatedPanel(t *testing.T) {
	turn := testTurnID
	tests := []struct {
		name  string
		check func(t *testing.T, view *frontendv1.FooterView)
	}{
		{
			name: "the panel still arrives",
			check: func(t *testing.T, view *frontendv1.FooterView) {
				if view.GetExpanded().GetTokens() == nil {
					t.Fatalf("the idle footer shipped no tokens panel; the cell must still open one")
				}
			},
		},
		{
			name: "the panel holds the most recent turn's growth",
			check: func(t *testing.T, view *frontendv1.FooterView) {
				if got := view.GetExpanded().GetTokens().GetContextGrowth().GetValue(); got != "18.2k" {
					t.Fatalf("context growth = %q, want the ended turn's 18.2k", got)
				}
			},
		},
		{
			name: "the panel holds the most recent turn's spend",
			check: func(t *testing.T, view *frontendv1.FooterView) {
				if got := view.GetExpanded().GetTokens().GetInput().GetValue(); got != "61k" {
					t.Fatalf("panel input = %q, want the ended turn's 61k", got)
				}
			},
		},
		{
			name: "the idle cell keeps the turn's alarm glyph",
			check: func(t *testing.T, view *frontendv1.FooterView) {
				if view.GetStrip().GetTokens().GetAlarm() == nil {
					t.Fatalf("the idle cell dropped the alarm glyph; it stands until the next turn")
				}
			},
		},
		{
			name: "the idle cell carries the settled turn's verdict",
			check: func(t *testing.T, view *frontendv1.FooterView) {
				if view.GetStrip().GetTokens().GetVerdict().GetComplete() == nil {
					t.Fatalf("verdict = %v, want complete on the idle cell", view.GetStrip().GetTokens().GetVerdict())
				}
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t, WithTokenAlarmThreshold(20_000))
			connected(h)
			readContext(h, 100_000)
			h.r.SetTurn(testWS, &TurnStarted{At: instant})
			h.r.OnActivity(testWS, mainAgent, responseFrame("unit-1", "success", usage(0, 61_000, 0, 0, 0)))
			readContext(h, 118_200)

			// Act
			h.r.OnAgentTerminal(testWS, mainAgent, &turn, completed(), nil)

			// Assert
			tt.check(t, h.view(t))
		})
	}
}

func TestThePanelsContextGrowthIsUnsetBeforeAnyTurn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	readContext(h, 100_000)

	// Assert
	if v := h.view(t).GetExpanded().GetTokens().GetContextGrowth().Value; v != nil {
		t.Fatalf("context growth = %q before any turn opened, want the empty slot", *v)
	}
}

// ---- the panel: per-agent spend and its sum --------------------------------

// arrangeThreeAgents spends on the main agent, an in-turn subagent and a
// detached agent inside one turn. The subagent's usage arrives FIRST, so the
// main entry's place is the ordering's own rule rather than arrival order.
func arrangeThreeAgents(h *harness) {
	sub := &conversationv1.AgentId{Value: "agent-sub"}
	h.r.OnDetachedWork(testWS, mainAgent, detachedSubagentWork("work-1", detachedAgent.GetValue(), "general-purpose"))
	h.r.SetTurn(testWS, &TurnStarted{At: instant})
	h.r.OnActivity(testWS, mainAgent, subagentStart("spawn-1", sub.GetValue(), "Explore", "find the footer"))
	h.r.OnActivity(testWS, sub, responseFrame("sub-1", "success", usage(4_000, 2_000, 500, 700, 0)))
	h.r.OnActivity(testWS, mainAgent, responseFrame("main-1", "success", usage(90_000, 18_000, 200, 1_500, 300)))
	h.r.OnActivity(testWS, detachedAgent, responseFrame("det-1", "success", usage(1_000, 3_000, 0, 200, 0)))
}

func TestThePanelListsEachAgentsSpend(t *testing.T) {
	type entry struct{ label, input, cacheRead, cacheWrite, output string }
	tests := []struct {
		name  string
		index int
		want  entry
	}{
		{name: "the main agent is listed first", index: 0, want: entry{"main", "18.2k", "90k", "18k", "1.5k"}},
		{name: "an in-turn subagent is named by type and description", index: 1, want: entry{"Explore · find the footer", "2.5k", "4k", "2k", "700"}},
		{name: "a detached agent is named by type", index: 2, want: entry{"general-purpose", "3k", "1k", "3k", "200"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)

			// Act
			arrangeThreeAgents(h)

			// Assert
			agents := h.view(t).GetExpanded().GetTokens().GetAgents()
			if len(agents) != 3 {
				t.Fatalf("agents = %d entries, want 3", len(agents))
			}
			a := agents[tt.index]
			got := entry{a.GetLabel(), a.GetInput().GetValue(), a.GetCacheRead().GetValue(), a.GetCacheWrite().GetValue(), a.GetOutput().GetValue()}
			if got != tt.want {
				t.Fatalf("agents[%d] = %+v, want %+v", tt.index, got, tt.want)
			}
		})
	}
}

func TestThePanelSumIsAcrossEveryAgent(t *testing.T) {
	tests := []struct {
		name string
		line func(p *frontendv1.FooterExpandedTokens) string
		want string
	}{
		{name: "uncached input", line: func(p *frontendv1.FooterExpandedTokens) string { return p.GetInput().GetValue() }, want: "23.7k"},
		{name: "cache read", line: func(p *frontendv1.FooterExpandedTokens) string { return p.GetCacheRead().GetValue() }, want: "95k"},
		{name: "cache write", line: func(p *frontendv1.FooterExpandedTokens) string { return p.GetCacheWrite().GetValue() }, want: "23k"},
		{name: "output", line: func(p *frontendv1.FooterExpandedTokens) string { return p.GetOutput().GetValue() }, want: "2.4k"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)

			// Act
			arrangeThreeAgents(h)

			// Assert
			if got := tt.line(h.view(t).GetExpanded().GetTokens()); got != tt.want {
				t.Fatalf("%s sum = %q, want %q", tt.name, got, tt.want)
			}
		})
	}
}

func TestThePanelListsNoAgentBeforeAnyUsage(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Assert
	if got := h.view(t).GetExpanded().GetTokens().GetAgents(); len(got) != 0 {
		t.Fatalf("agents = %v, want none until some usage is known", got)
	}
}

// agentLabels is the panel's per-agent entry names, in order.
func agentLabels(t *testing.T, h *harness) []string {
	t.Helper()
	var out []string
	for _, a := range h.view(t).GetExpanded().GetTokens().GetAgents() {
		out = append(out, a.GetLabel())
	}
	return out
}

func TestDetachedSpendStaysInThePanelAcrossTurns(t *testing.T) {
	turn := testTurnID
	sub := &conversationv1.AgentId{Value: "agent-sub"}
	tests := []struct {
		name    string
		arrange func(h *harness)
		want    []string
	}{
		{
			name: "a live detached agent's entry is carried into the next turn",
			arrange: func(h *harness) {
				h.r.OnDetachedWork(testWS, mainAgent, detachedSubagentWork("work-1", detachedAgent.GetValue(), "general-purpose"))
				h.r.SetTurn(testWS, &TurnStarted{At: instant})
				h.r.OnActivity(testWS, detachedAgent, responseFrame("det-1", "success", usage(0, 3_000, 0, 0, 0)))
				h.r.OnAgentTerminal(testWS, mainAgent, &turn, completed(), nil)
				h.r.SetTurn(testWS, &TurnStarted{At: instant})
			},
			want: []string{"general-purpose"},
		},
		{
			name: "a live detached agent's idle spend is in the idle panel",
			arrange: func(h *harness) {
				h.r.OnDetachedWork(testWS, mainAgent, detachedSubagentWork("work-1", detachedAgent.GetValue(), "general-purpose"))
				h.r.OnActivity(testWS, detachedAgent, responseFrame("det-1", "success", usage(0, 3_000, 0, 0, 0)))
			},
			want: []string{"general-purpose"},
		},
		{
			name: "a retired detached agent's carried spend leaves the panel",
			arrange: func(h *harness) {
				h.r.OnDetachedWork(testWS, mainAgent, detachedSubagentWork("work-1", detachedAgent.GetValue(), "general-purpose"))
				h.r.OnActivity(testWS, detachedAgent, responseFrame("det-1", "success", usage(0, 3_000, 0, 0, 0)))
				h.r.SetTurn(testWS, &TurnStarted{At: instant})
				h.r.OnSubagent(testWS, workID("work-1"), subagentSettled(false))
			},
			want: nil,
		},
		{
			name: "an in-turn subagent's entry stays after it retires, until the next turn",
			arrange: func(h *harness) {
				h.r.SetTurn(testWS, &TurnStarted{At: instant})
				h.r.OnActivity(testWS, mainAgent, subagentStart("spawn-1", sub.GetValue(), "Explore", ""))
				h.r.OnActivity(testWS, sub, responseFrame("sub-1", "success", usage(0, 2_000, 0, 0, 0)))
				h.r.OnAgentTerminal(testWS, sub, nil, completed(), nil)
			},
			want: []string{"Explore"},
		},
		{
			name: "the next turn drops an ended in-turn subagent's entry",
			arrange: func(h *harness) {
				h.r.SetTurn(testWS, &TurnStarted{At: instant})
				h.r.OnActivity(testWS, mainAgent, subagentStart("spawn-1", sub.GetValue(), "Explore", ""))
				h.r.OnActivity(testWS, sub, responseFrame("sub-1", "success", usage(0, 2_000, 0, 0, 0)))
				h.r.OnAgentTerminal(testWS, sub, nil, completed(), nil)
				h.r.SetTurn(testWS, &TurnStarted{At: instant})
			},
			want: nil,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)

			// Act
			tt.arrange(h)

			// Assert
			got := agentLabels(t, h)
			if len(got) != len(tt.want) {
				t.Fatalf("agents = %v, want %v", got, tt.want)
			}
			for i := range got {
				if got[i] != tt.want[i] {
					t.Fatalf("agents = %v, want %v", got, tt.want)
				}
			}
		})
	}
}

// ---- the reading's records, and the readings the contract cannot hold ------

func TestContextReadingsAreRecorded(t *testing.T) {
	tests := []struct {
		name       string
		act        func(h *harness)
		level      string
		operation  string
		wantGrowth string
	}{
		{
			name: "a reading with no payload is refused at WARN and the growth stands",
			act: func(h *harness) {
				h.r.OnSessionUpdate(testWS, &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_ContextUsage{}})
			},
			level:      dlog.LevelWarn,
			operation:  "daemon.footer.context_usage_unreadable",
			wantGrowth: "10k",
		},
		{
			name:       "a negative reading is refused at WARN and the growth stands",
			act:        func(h *harness) { readContext(h, -5) },
			level:      dlog.LevelWarn,
			operation:  "daemon.footer.context_usage_unreadable",
			wantGrowth: "10k",
		},
		{
			name:       "a cut is recorded at INFO",
			act:        func(h *harness) { readContext(h, 20_000) },
			level:      dlog.LevelInfo,
			operation:  "daemon.footer.context_cut_rebased",
			wantGrowth: "0",
		},
		{
			name:       "an ordinary reading is recorded at DEBUG",
			act:        func(h *harness) { readContext(h, 112_000) },
			level:      dlog.LevelDebug,
			operation:  "daemon.footer.context_held",
			wantGrowth: "12k",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			readContext(h, 100_000)
			h.r.SetTurn(testWS, &TurnStarted{At: instant})
			readContext(h, 110_000)

			// Act
			tt.act(h)

			// Assert
			if !hasLevel(h.log.Records(), tt.level, tt.operation) {
				t.Fatalf("records = %+v, want %s %s", h.log.Records(), tt.level, tt.operation)
			}
			if got := panelGrowth(t, h); got != tt.wantGrowth {
				t.Fatalf("panel growth = %q, want %q", got, tt.wantGrowth)
			}
		})
	}
}

func TestTheFirstReadingOfATurnWithNoBaselineIsRecorded(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	readContext(h, 100_000)

	// Assert
	if !hasLevel(h.log.Records(), dlog.LevelInfo, "daemon.footer.context_baseline_taken") {
		t.Fatalf("records = %+v, want INFO daemon.footer.context_baseline_taken", h.log.Records())
	}
}

func TestAnUndrawableContextWindowIsRecordedAndTheLastOneStands(t *testing.T) {
	tests := []struct {
		name  string
		total int64
		max   int64
	}{
		{name: "no usable window", total: 10, max: 0},
		{name: "more held than the window allows", total: 300_000, max: 200_000},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			h.r.OnSessionUpdate(testWS, contextUsage(50_000, 200_000))

			// Act
			h.r.OnSessionUpdate(testWS, contextUsage(tt.total, tt.max))

			// Assert
			if got := enduringOf(t, h).GetContextWindow().GetUsedTokens(); got != 50_000 {
				t.Fatalf("used = %d, want the last readable report standing", got)
			}
			if !hasLevel(h.log.Records(), "warn", "daemon.footer.context_window_unreadable") {
				t.Fatalf("no WARN daemon.footer.context_window_unreadable was recorded")
			}
		})
	}
}
