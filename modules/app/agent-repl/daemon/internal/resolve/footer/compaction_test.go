package footer

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// progress is one compaction phase frame.
func progress(
	p conversationv1.SessionCompactionPhase,
	before, after uint64,
	why string,
) *conversationv1.SessionCompactionProgress {
	return &conversationv1.SessionCompactionProgress{
		Phase: p, TokensBefore: before, TokensAfter: after, Error: why,
	}
}

// sessionCompactionProgress wraps a phase as the session frame that carries it.
func sessionCompactionProgress(p *conversationv1.SessionCompactionProgress) *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_CompactionProgress{CompactionProgress: p},
	}
}

func TestCompactionLineNamesTheSizeWhileSummarizing(t *testing.T) {
	// Arrange.
	p := progress(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_SUMMARIZING, 101_600, 0, "")

	// Act.
	line := CompactionLine(p)

	// Assert.
	if line != "summarizing the conversation (101.6k)…" {
		t.Fatalf("line = %q, want the size named", line)
	}
}

func TestCompactionLineOmitsAnUnknownSizeWhileSummarizing(t *testing.T) {
	// Arrange.
	p := progress(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_SUMMARIZING, 0, 0, "")

	// Act.
	line := CompactionLine(p)

	// Assert.
	if line != "summarizing the conversation…" {
		t.Fatalf("line = %q, want no invented figure", line)
	}
}

func TestCompactionLineStatesBothFiguresOnCompletion(t *testing.T) {
	// Arrange.
	p := progress(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_STARTED, 101_600, 12_400, "")

	// Act.
	line := CompactionLine(p)

	// Assert.
	if line != "compacted and resumed (101.6k → 12.4k)" {
		t.Fatalf("line = %q, want both figures", line)
	}
}

func TestCompactionLineCarriesTheFailuresOwnWords(t *testing.T) {
	// Arrange.
	p := progress(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_FAILED, 0, 0, "the summarizing session ended as error_max_turns")

	// Act.
	line := CompactionLine(p)

	// Assert.
	if line != "compaction failed — the summarizing session ended as error_max_turns" {
		t.Fatalf("line = %q, want the producer's account", line)
	}
}

func TestCompactionRequestLineNamesTheRemediation(t *testing.T) {
	// Arrange / Act.
	line := CompactionRequestLine(ChoiceCompact, "the conversation is cold at 101600 context tokens")

	// Assert.
	if line != "compaction requested (the conversation is cold at 101600 context tokens)" {
		t.Fatalf("line = %q, want the request naming the gate", line)
	}
}

func TestTheVendorsAutoCompactionDrawsTheCompactionLine(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	connected(h)
	// The vendor's auto-compaction runs inside a turn; the step only exists
	// while one is in flight.
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActCompact})

	// Act. The vendor's own start signal, which carries no phase and no figure.
	h.r.OnSessionUpdate(testWS, &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_Compacting{Compacting: &conversationv1.SessionCompacting{}},
	})

	// Assert.
	got := h.view(t).GetStrip().GetStatus().GetThinking().GetActivity().GetCompaction().GetText()
	if got != "compacting the context…" {
		t.Fatalf("activity = %q, want the vendor compaction line", got)
	}
}

func TestARelayedPhaseBecomesTheThinkingActivity(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActCompact})

	// Act.
	h.r.OnSessionUpdate(testWS, sessionCompactionProgress(
		progress(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_RESUMING, 101_600, 12_400, "")))

	// Assert.
	got := h.view(t).GetStrip().GetStatus().GetThinking().GetActivity().GetCompaction().GetText()
	if got != "resuming the session from the summary…" {
		t.Fatalf("activity = %q, want the relayed phase's line", got)
	}
}

func TestARelayedPhaseTakesTheCompactingStep(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActCompact})

	// Act.
	h.r.OnSessionUpdate(testWS, sessionCompactionProgress(
		progress(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_SUMMARIZING, 101_600, 0, "")))

	// Assert. The SAME step the vendor's auto-compaction takes.
	thinking := h.view(t).GetStrip().GetStatus().GetThinking()
	if thinking.GetCompacting() == nil {
		t.Fatalf("substatus = %+v, want compacting", thinking.GetSubstatus())
	}
}

func TestAFailedPhaseEndsTheCompaction(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	connected(h)
	// An ORDINARY turn the vendor decided to compact inside of: the turn's own
	// act is not `compact`, so the compaction flag is the only thing holding
	// the step.
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})
	h.r.OnSessionUpdate(testWS, &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_Compacting{Compacting: &conversationv1.SessionCompacting{}},
	})

	// Act.
	h.r.OnSessionUpdate(testWS, sessionCompactionProgress(
		progress(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_FAILED, 0, 0, "it died")))

	// Assert. The line still stands, but the session is no longer compacting.
	status := h.view(t).GetStrip().GetStatus()
	if status.GetThinking().GetCompacting() != nil {
		t.Fatalf("status = %+v, want the compaction over", status)
	}
}

func TestAnAnsweredColdGateThinksRatherThanWaits(t *testing.T) {
	// Arrange. The gate is still standing: it is not lifted until the re-open
	// lands, and the answer must outrank it.
	h := newHarness(t)
	connected(h)
	h.r.SetColdGate(testWS, ColdGate{Standing: true, Detail: "the conversation is cold"})

	// Act.
	h.r.SetColdGateAnswer(testWS, &ColdGateAnswer{Choice: ChoiceCompact, Text: "compaction requested"})

	// Assert.
	status := h.view(t).GetStrip().GetStatus()
	if status.GetThinking().GetCompacting() == nil {
		t.Fatalf("status = %+v, want thinking·compacting over the standing gate", status)
	}
}

func TestAnAnsweredColdGateDrawsTheDaemonsOwnLine(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	connected(h)
	h.r.SetColdGate(testWS, ColdGate{Standing: true, Detail: "the conversation is cold"})

	// Act.
	h.r.SetColdGateAnswer(testWS, &ColdGateAnswer{Choice: ChoiceCompact, Text: "compaction requested"})

	// Assert.
	got := h.view(t).GetStrip().GetStatus().GetThinking().GetActivity().GetCompaction().GetText()
	if got != "compaction requested" {
		t.Fatalf("activity = %q, want the answer's own line", got)
	}
}

func TestAClearedGateAnswerTakesTheClearingStep(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	connected(h)
	h.r.SetColdGate(testWS, ColdGate{Standing: true, Detail: "the conversation is cold"})

	// Act.
	h.r.SetColdGateAnswer(testWS, &ColdGateAnswer{Choice: ChoiceClear, Text: "clearing the context"})

	// Assert.
	thinking := h.view(t).GetStrip().GetStatus().GetThinking()
	if thinking.GetClearing() == nil {
		t.Fatalf("substatus = %+v, want clearing", thinking.GetSubstatus())
	}
}

func TestAPaidGateAnswerTakesTheSubmittingStep(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	connected(h)
	h.r.SetColdGate(testWS, ColdGate{Standing: true, Detail: "the conversation is cold"})

	// Act.
	h.r.SetColdGateAnswer(testWS, &ColdGateAnswer{Choice: ChoicePay, Text: "resuming and paying"})

	// Assert.
	thinking := h.view(t).GetStrip().GetStatus().GetThinking()
	if thinking.GetSubmitting() == nil {
		t.Fatalf("substatus = %+v, want submitting", thinking.GetSubstatus())
	}
}

func TestClearingTheAnswerGivesTheStandingGateBackTheStrip(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	connected(h)
	h.r.SetColdGate(testWS, ColdGate{Standing: true, Detail: "the conversation is cold"})
	h.r.SetColdGateAnswer(testWS, &ColdGateAnswer{Choice: ChoiceCompact, Text: "compaction requested"})

	// Act. The re-open failed, so the gate is still the question.
	h.r.SetColdGateAnswer(testWS, nil)

	// Assert.
	waiting := h.view(t).GetStrip().GetStatus().GetWaiting()
	if waiting.GetColdGate() == nil {
		t.Fatalf("status = %+v, want the standing gate back", h.view(t).GetStrip().GetStatus())
	}
}

func TestClearingTheAnswerRetiresItsProgressLine(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	connected(h)
	h.r.SetColdGateAnswer(testWS, &ColdGateAnswer{Choice: ChoiceCompact, Text: "compaction requested"})

	// Act.
	h.r.SetColdGateAnswer(testWS, nil)

	// Assert. No stale progress sentence survives the act it narrated.
	var empty *frontendv1.FooterStatusActivityCompaction
	if got := h.view(t).GetStrip().GetStatus().GetThinking().GetActivity().GetCompaction(); got != empty {
		t.Fatalf("activity = %+v, want no compaction line", got)
	}
}
