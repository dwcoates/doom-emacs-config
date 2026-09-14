package footer

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
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

func TestTheCellShowsTheUncachedInputFigure(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant})

	// Act
	h.r.OnActivity(testWS, mainAgent, responseFrame("unit-1", "success", usage(90_000, 18_000, 200, 500, 100)))

	// Assert
	got := h.view(t).GetStrip().GetTokens().GetInput().GetText()
	if got != "18.2k in" {
		t.Fatalf("cell = %q, want the input_misses total (written + unwritten)", got)
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
	got := h.view(t).GetStrip().GetTokens().GetInput().GetText()
	if got != "3k in" {
		t.Fatalf("cell = %q, want 3k across two carrying units", got)
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
	got := h.view(t).GetStrip().GetTokens().GetInput().GetText()
	if got != "5k in" {
		t.Fatalf("cell = %q, want 5k: usage is keyed by unit and REPLACED, never added", got)
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

	// Assert: the detached agent is still burning that input, so its figure
	// stands rather than falling back to the new turn's zero.
	got := h.view(t).GetStrip().GetTokens().GetInput().GetText()
	if got != "5k in" {
		t.Fatalf("cell = %q, want the live detached agent's usage carried across the reset", got)
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
	// live detached agent's stay, so the figure is the detached 5k alone.
	got := h.view(t).GetStrip().GetTokens().GetInput().GetText()
	if got != "5k in" {
		t.Fatalf("cell = %q, want the detached 5k with the main turn's 3k wiped", got)
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

	// Assert: a settled run charges nothing more, so its units leave the figure
	// rather than standing in it for the rest of the session.
	got := h.view(t).GetStrip().GetTokens().GetInput().GetText()
	if got != "0 in" {
		t.Fatalf("cell = %q, want the retired detached agent's usage dropped", got)
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
	got := h.view(t).GetStrip().GetTokens().GetInput().GetText()
	if got != "5k in" {
		t.Fatalf("cell = %q, want 5k: a carried unit is replaced, never added", got)
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
	got := h.view(t).GetStrip().GetTokens().GetInput().GetText()
	if got != "0 in" {
		t.Fatalf("cell = %q, want the in-turn subagent's usage wiped with the turn", got)
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
