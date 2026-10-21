package footer

import (
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
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
	line := compactionSentence(p)

	// Assert.
	if line != "summarizing the conversation (101.6k)…" {
		t.Fatalf("line = %q, want the size named", line)
	}
}

func TestCompactionLineOmitsAnUnknownSizeWhileSummarizing(t *testing.T) {
	// Arrange.
	p := progress(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_SUMMARIZING, 0, 0, "")

	// Act.
	line := compactionSentence(p)

	// Assert.
	if line != "summarizing the conversation…" {
		t.Fatalf("line = %q, want no invented figure", line)
	}
}

func TestCompactionLineStatesBothFiguresOnCompletion(t *testing.T) {
	// Arrange.
	p := progress(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_STARTED, 101_600, 12_400, "")

	// Act.
	line := compactionSentence(p)

	// Assert.
	if line != "compacted and resumed (101.6k → 12.4k)" {
		t.Fatalf("line = %q, want both figures", line)
	}
}

func TestCompactionLineCarriesTheFailuresOwnWords(t *testing.T) {
	// Arrange.
	p := progress(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_FAILED, 0, 0, "the summarizing session ended as error_max_turns")

	// Act.
	line := compactionSentence(p)

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
	got := h.view(t).GetStrip().GetStatus().GetWorking().GetActivity().GetSalient().GetCompaction().GetText()
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
	got := h.view(t).GetStrip().GetStatus().GetWorking().GetActivity().GetSalient().GetCompaction().GetText()
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
	thinking := h.view(t).GetStrip().GetStatus().GetWorking()
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
	if status.GetWorking().GetCompacting() != nil {
		t.Fatalf("status = %+v, want the compaction over", status)
	}
}

func TestAnAnsweredColdGateThinksRatherThanWaits(t *testing.T) {
	// Arrange. The gate is still standing: it is not lifted until the re-open
	// lands, and the answer must outrank it.
	h := newHarness(t)
	connected(h)
	h.r.SetColdGate(testWS, ColdGate{Standing: true, Cost: FiguredLine{Lead: "the conversation is cold"}})

	// Act.
	h.r.SetColdGateAnswer(testWS, &ColdGateAnswer{Choice: ChoiceCompact, Request: "compaction requested"})

	// Assert.
	status := h.view(t).GetStrip().GetStatus()
	if status.GetWorking().GetCompacting() == nil {
		t.Fatalf("status = %+v, want working·compacting over the standing gate", status)
	}
}

func TestAnAnsweredColdGateDrawsTheDaemonsOwnLine(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	connected(h)
	h.r.SetColdGate(testWS, ColdGate{Standing: true, Cost: FiguredLine{Lead: "the conversation is cold"}})

	// Act.
	h.r.SetColdGateAnswer(testWS, &ColdGateAnswer{Choice: ChoiceCompact, Request: "compaction requested"})

	// Assert.
	got := h.view(t).GetStrip().GetStatus().GetWorking().GetActivity().GetSalient().GetCompaction().GetText()
	if got != "compaction requested" {
		t.Fatalf("activity = %q, want the answer's own line", got)
	}
}

func TestACompactAndResumeGateAnswerTakesTheCompactingStep(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	connected(h)
	h.r.SetColdGate(testWS, ColdGate{Standing: true, Cost: FiguredLine{Lead: "the conversation is cold"}})

	// Act.
	h.r.SetColdGateAnswer(testWS, &ColdGateAnswer{Choice: ChoiceCompactAndResume, Request: "compaction requested"})

	// Assert.
	if got := h.view(t).GetStrip().GetStatus().GetWorking().GetCompacting(); got == nil {
		t.Fatalf("status = %+v, want working·compacting, as for compact", h.view(t).GetStrip().GetStatus())
	}
}

func TestCompactionRequestLineWordsCompactAndResumeAsACompaction(t *testing.T) {
	// Arrange / Act.
	line := CompactionRequestLine(ChoiceCompactAndResume, "the conversation is cold at 101600 context tokens")

	// Assert.
	if line != "compaction requested (the conversation is cold at 101600 context tokens)" {
		t.Fatalf("line = %q, want the compaction's request line", line)
	}
}

func TestAClearedGateAnswerTakesTheClearingStep(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	connected(h)
	h.r.SetColdGate(testWS, ColdGate{Standing: true, Cost: FiguredLine{Lead: "the conversation is cold"}})

	// Act.
	h.r.SetColdGateAnswer(testWS, &ColdGateAnswer{Choice: ChoiceClear, Request: "clearing the context"})

	// Assert.
	thinking := h.view(t).GetStrip().GetStatus().GetWorking()
	if thinking.GetClearing() == nil {
		t.Fatalf("substatus = %+v, want clearing", thinking.GetSubstatus())
	}
}

func TestAPaidGateAnswerTakesTheSubmittingStep(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	connected(h)
	h.r.SetColdGate(testWS, ColdGate{Standing: true, Cost: FiguredLine{Lead: "the conversation is cold"}})

	// Act.
	h.r.SetColdGateAnswer(testWS, &ColdGateAnswer{Choice: ChoicePay, Request: "resuming and paying"})

	// Assert.
	thinking := h.view(t).GetStrip().GetStatus().GetWorking()
	if thinking.GetSubmitting() == nil {
		t.Fatalf("substatus = %+v, want submitting", thinking.GetSubstatus())
	}
}

func TestClearingTheAnswerGivesTheStandingGateBackTheStrip(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	connected(h)
	h.r.SetColdGate(testWS, ColdGate{Standing: true, Cost: FiguredLine{Lead: "the conversation is cold"}})
	h.r.SetColdGateAnswer(testWS, &ColdGateAnswer{Choice: ChoiceCompact, Request: "compaction requested"})

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
	h.r.SetColdGateAnswer(testWS, &ColdGateAnswer{Choice: ChoiceCompact, Request: "compaction requested"})

	// Act.
	h.r.SetColdGateAnswer(testWS, nil)

	// Assert. No stale progress sentence survives the act it narrated.
	var empty *frontendv1.FooterStatusActivityCompaction
	if got := h.view(t).GetStrip().GetStatus().GetWorking().GetActivity().GetSalient().GetCompaction(); got != empty {
		t.Fatalf("activity = %+v, want no compaction line", got)
	}
}

// ---- the line's lifetime ----------------------------------------------------

// outlivedTurn is the operation of the invariant-violation record.
const outlivedTurn = "daemon.footer.compaction_line_outlived_turn"

// drawnCompaction is the compaction line the published view draws, empty when
// none stands.
func drawnCompaction(t *testing.T, h *harness) string {
	t.Helper()
	return h.view(t).GetStrip().GetStatus().GetWorking().GetActivity().GetSalient().GetCompaction().GetText()
}

// endTurn delivers the main thread's ordinary terminal for turn.
func endTurn(h *harness, turn ids.TurnID) {
	h.r.OnAgentTerminal(testWS, mainAgent, &turn, completed(), nil)
}

// compactedCut is the context cut that ends a successful compaction.
func compactedCut() *conversationv1.ContextCut {
	return &conversationv1.ContextCut{
		Cut: &conversationv1.ContextCut_Compacted{Compacted: &conversationv1.ContextCompacted{}},
	}
}

// clearedCut is the context cut a /clear leaves.
func clearedCut() *conversationv1.ContextCut {
	return &conversationv1.ContextCut{
		Cut: &conversationv1.ContextCut_Cleared{Cleared: &conversationv1.ContextCleared{}},
	}
}

// recordsOf answers every record of one operation.
func recordsOf(records []dlog.Record, operation string) []dlog.Record {
	return dlog.FilterRecords(records, dlog.RecordQuery{Operation: operation})
}

func TestTheTurnsTerminalEndsACompactionLineItsCutNeverEnded(t *testing.T) {
	// Arrange: the vendor compacts inside an ordinary turn and the cut is lost.
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})
	h.r.OnSessionUpdate(testWS, vendorCompacting())
	h.clock.Advance(90 * time.Second)

	// Act
	endTurn(h, "turn-1")

	// Assert: the next turn draws no compaction line.
	h.r.SetTurn(testWS, &TurnStarted{At: h.clock.Now(), Act: ActPrompt})
	if got := drawnCompaction(t, h); got != "" {
		t.Fatalf("activity = %q, want no compaction line on the next turn", got)
	}
}

func TestATerminalEndingACompactionLineRecordsTheViolation(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})
	h.r.OnSessionUpdate(testWS, vendorCompacting())
	h.clock.Advance(90 * time.Second)

	// Act
	endTurn(h, "turn-1")

	// Assert
	errs := recordsOf(h.log.Records(), outlivedTurn)
	if len(errs) != 1 || errs[0].Level != dlog.LevelError {
		t.Fatalf("records = %+v, want exactly one ERROR", errs)
	}
	want := map[string]any{
		"workspace_id": string(testWS),
		"turn_id":      "turn-1",
		"text":         "compacting the context…",
		"age_ms":       int64(90_000),
	}
	for key, value := range want {
		if got := errs[0].Context[key]; got != value {
			t.Fatalf("%s = %v (%T), want %v (record %+v)", key, got, got, value, errs[0].Context)
		}
	}
}

func TestTheCutEndsTheCompactionLineWithNoViolation(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})
	h.r.OnSessionUpdate(testWS, vendorCompacting())

	// Act
	h.r.OnContextCut(testWS, mainAgent, compactedCut())
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})
	endTurn(h, "turn-1")

	// Assert
	if got := drawnCompaction(t, h); got != "" {
		t.Fatalf("activity = %q, want the line gone with the cut", got)
	}
	if errs := recordsOf(h.log.Records(), outlivedTurn); len(errs) != 0 {
		t.Fatalf("records = %+v, want no violation: the cut ended the act", errs)
	}
}

func TestACompactTurnsTerminalEndsItsLine(t *testing.T) {
	// Arrange: a `/compact` is its own turn.
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActCompact})
	h.r.OnSessionUpdate(testWS, vendorCompacting())

	// Act
	endTurn(h, "turn-compact")

	// Assert
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})
	if got := drawnCompaction(t, h); got != "" {
		t.Fatalf("activity = %q, want the compact turn's line ended with it", got)
	}
}

func TestATurnOpeningWithACompactionLineStandingIsNoViolation(t *testing.T) {
	// Arrange: the vendor's start signal lands before its turn's open edge.
	h := newHarness(t)
	connected(h)
	h.r.OnSessionUpdate(testWS, vendorCompacting())

	// Act
	h.r.OnTurnOpened(testWS, "turn-1")

	// Assert
	if got := drawnCompaction(t, h); got != vendorCompactionLine {
		t.Fatalf("activity = %q, want the turn's own compaction line", got)
	}
	if errs := recordsOf(h.log.Records(), outlivedTurn); len(errs) != 0 {
		t.Fatalf("records = %+v, want no violation at an opening", errs)
	}
}

func TestAConcludedPhaseEndsTheCompactingStep(t *testing.T) {
	tests := []struct {
		name  string
		phase conversationv1.SessionCompactionPhase
	}{
		{"started", conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_STARTED},
		{"failed", conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_FAILED},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: an ordinary turn, so only the flag can hold the step.
			h := newHarness(t)
			connected(h)
			h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})

			// Act
			h.r.OnSessionUpdate(testWS, sessionCompactionProgress(progress(tt.phase, 101_600, 12_400, "")))

			// Assert: the step is over at once.
			if h.view(t).GetStrip().GetStatus().GetWorking().GetCompacting() != nil {
				t.Fatalf("substatus = compacting, want the compaction over")
			}
		})
	}
}

func TestATerminalAfterAConcludedPhaseIsNoViolation(t *testing.T) {
	// Arrange: the act concluded; its dwell has not elapsed yet.
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})
	h.r.OnSessionUpdate(testWS, sessionCompactionProgress(
		progress(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_STARTED, 0, 0, "")))

	// Act
	endTurn(h, "turn-1")

	// Assert
	if errs := recordsOf(h.log.Records(), outlivedTurn); len(errs) != 0 {
		t.Fatalf("records = %+v, want no violation: the phase ended the act", errs)
	}
}

func TestTheRepeatedVendorSignal(t *testing.T) {
	tests := []struct {
		name     string
		arrange  func(h *harness)
		wantText string
		wantAt   time.Time
	}{
		{
			name: "a repeat while standing keeps the instant the line began",
			arrange: func(h *harness) {
				h.r.OnSessionUpdate(testWS, vendorCompacting())
				h.clock.Advance(30 * time.Second)
			},
			wantText: vendorCompactionLine,
			wantAt:   instant,
		},
		{
			name: "a repeat after the line ended stands it anew",
			arrange: func(h *harness) {
				h.r.OnSessionUpdate(testWS, vendorCompacting())
				h.r.OnContextCut(testWS, mainAgent, compactedCut())
				h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})
				h.clock.Advance(30 * time.Second)
			},
			wantText: vendorCompactionLine,
			wantAt:   instant.Add(30 * time.Second),
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})
			tt.arrange(h)

			// Act
			h.r.OnSessionUpdate(testWS, vendorCompacting())

			// Assert
			activity := h.view(t).GetStrip().GetStatus().GetWorking().GetActivity()
			if got := activity.GetSalient().GetCompaction().GetText(); got != tt.wantText {
				t.Fatalf("activity = %q, want %q", got, tt.wantText)
			}
			if got := activity.GetSalient().GetAt().GetAtMs(); got != epochMs(tt.wantAt) {
				t.Fatalf("at = %d, want %d", got, epochMs(tt.wantAt))
			}
		})
	}
}

func TestADeadQueryEndsTheCompaction(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})
	h.r.OnSessionUpdate(testWS, vendorCompacting())

	// Act
	h.r.OnSessionUpdate(testWS, &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_QueryDied{QueryDied: &conversationv1.SessionQueryDied{}},
	})

	// Assert: the restarting turn draws neither the step nor the line.
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})
	thinking := h.view(t).GetStrip().GetStatus().GetWorking()
	if thinking.GetCompacting() != nil || thinking.GetActivity().GetSalient().GetCompaction() != nil {
		t.Fatalf("thinking = %+v, want no compaction on the restarting turn", thinking)
	}
}

func TestTheCompactionLinesEndIsRecordedWithItsCause(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})
	h.r.OnSessionUpdate(testWS, vendorCompacting())

	// Act
	h.r.OnContextCut(testWS, mainAgent, compactedCut())

	// Assert
	ended := recordsOf(h.log.Records(), "daemon.footer.compaction_line_ended")
	if len(ended) != 1 || ended[0].Context["cause"] != "daemon.footer.on_context_cut" {
		t.Fatalf("records = %+v, want one end caused by the cut", ended)
	}
}

func TestTheColdGateAnswersLine(t *testing.T) {
	tests := []struct {
		name string
		act  func(h *harness)
		want string
	}{
		{
			name: "a turn's terminal leaves the answer's line to the answer",
			act: func(h *harness) {
				h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})
				endTurn(h, "turn-1")
			},
			want: "compacted and resumed",
		},
		{
			name: "a relayed concluded phase leaves the answer's line to the answer",
			act: func(h *harness) {
				h.r.OnSessionUpdate(testWS, sessionCompactionProgress(
					progress(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_STARTED, 0, 0, "")))
			},
			want: "compacted and resumed",
		},
		{
			name: "clearing the answer ends the line",
			act:  func(h *harness) { h.r.SetColdGateAnswer(testWS, nil) },
			want: "",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the gate's verb relayed its concluded phase.
			h := newHarness(t)
			connected(h)
			h.r.SetColdGate(testWS, ColdGate{Standing: true, Cost: FiguredLine{Lead: "the conversation is cold"}})
			h.r.SetColdGateAnswer(testWS, &ColdGateAnswer{Choice: ChoiceCompact, Request: "compacted and resumed"})

			// Act
			tt.act(h)

			// Assert
			if got := drawnCompaction(t, h); got != tt.want {
				t.Fatalf("activity = %q, want %q", got, tt.want)
			}
			if errs := recordsOf(h.log.Records(), outlivedTurn); len(errs) != 0 {
				t.Fatalf("records = %+v, want no violation on the cold path", errs)
			}
		})
	}
}

// concludedPhases are the two phases that end a compaction.
var concludedPhases = []struct {
	name  string
	phase conversationv1.SessionCompactionPhase
}{
	{"started", conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_STARTED},
	{"failed", conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_FAILED},
}

func TestAConcludedPhaseEndsTheSalientLineAtOnce(t *testing.T) {
	for _, tt := range concludedPhases {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})
			h.r.OnSessionUpdate(testWS, vendorCompacting())

			// Act
			h.r.OnSessionUpdate(testWS, sessionCompactionProgress(progress(tt.phase, 101_600, 12_400, "")))

			// Assert
			if got := drawnCompaction(t, h); got != "" {
				t.Fatalf("salient compaction = %q, want none once the compaction concluded", got)
			}
		})
	}
}

func TestACompletedCompactionIsAnnouncedAsCompactionConcluded(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})

	// Act
	h.r.OnSessionUpdate(testWS, sessionCompactionProgress(
		progress(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_STARTED, 101_600, 12_400, "")))

	// Assert
	if got := transientOf(t, h).GetCompactionConcluded().GetText(); got != "compacted and resumed (101.6k → 12.4k)" {
		t.Fatalf("compaction_concluded = %q, want the composed outcome line", got)
	}
}

func TestAFailedCompactionPhaseRaisesNoSalientLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})

	// Act
	h.r.OnSessionUpdate(testWS, sessionCompactionProgress(
		progress(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_FAILED, 0, 0, "the summary was empty")))

	// Assert
	if got := activityLineOf(h.view(t).GetStrip().GetStatus()); got.tier == "salient" {
		t.Fatalf("activity = %+v, want no salient line: a failed compaction is the feed's marker alone", got)
	}
}

func TestAConcludedCompactionArmsNoTimer(t *testing.T) {
	for _, tt := range concludedPhases {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})
			h.r.OnSessionUpdate(testWS, vendorCompacting())
			// The turn's own vendor-silence timer stands from its opening; it
			// ends no line, and is not the conclusion's.
			before := len(h.clock.pending)

			// Act
			h.r.OnSessionUpdate(testWS, sessionCompactionProgress(progress(tt.phase, 101_600, 12_400, "")))

			// Assert: no timer may end a salient line, and a transient's expiry is the client's.
			if n := len(h.clock.pending) - before; n != 0 {
				t.Fatalf("%d timers armed by the conclusion, want none", n)
			}
		})
	}
}

func TestAConclusionUnderAColdGateAnswerStillRaisesItsTransient(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetColdGate(testWS, ColdGate{Standing: true, Cost: FiguredLine{Lead: "the conversation is cold"}})
	h.r.SetColdGateAnswer(testWS, &ColdGateAnswer{Choice: ChoiceCompact, Request: "resuming the session from the summary…"})
	h.r.OnSessionUpdate(testWS, sessionCompactionProgress(
		progress(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_STARTED, 101_600, 12_400, "")))

	// Act: the answer's verb is done.
	h.r.SetColdGateAnswer(testWS, nil)
	h.r.SetColdGate(testWS, ColdGate{Standing: false})

	// Assert
	if transientOf(t, h).GetCompactionConcluded() == nil {
		t.Fatalf("transient = %+v, want the conclusion beneath once the answer cleared", transientOf(t, h))
	}
}

// namedCompacting is the vendor's compacting start signal for compaction ID.
func namedCompacting(id string) *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_Compacting{
		Compacting: &conversationv1.SessionCompacting{Compaction: &conversationv1.CompactionId{Value: id}},
	}}
}

// namedCompactedCut is the cut that ends compaction ID.
func namedCompactedCut(id string) *conversationv1.ContextCut {
	return &conversationv1.ContextCut{Cut: &conversationv1.ContextCut_Compacted{
		Compacted: &conversationv1.ContextCompacted{Compaction: &conversationv1.CompactionId{Value: id}},
	}}
}

func TestCompactingNamedByItsOwnConcludedCut(t *testing.T) {
	tests := []struct {
		name      string
		cut       *conversationv1.ContextCut
		start     *conversationv1.SessionUpdate
		wantLine  string
		wantStale int
	}{
		{
			name:      "a start signal outrun by its own cut stands nothing",
			cut:       namedCompactedCut("c-1"),
			start:     namedCompacting("c-1"),
			wantLine:  "",
			wantStale: 1,
		},
		{
			name: "a failed cut concludes its compaction too",
			cut: &conversationv1.ContextCut{Cut: &conversationv1.ContextCut_CompactionFailed{
				CompactionFailed: &conversationv1.ContextCompactionFailed{Error: "x", Compaction: &conversationv1.CompactionId{Value: "c-1"}},
			}},
			start:     namedCompacting("c-1"),
			wantLine:  "",
			wantStale: 1,
		},
		{
			name:      "a new compaction after a cut stands its line",
			cut:       namedCompactedCut("c-1"),
			start:     namedCompacting("c-2"),
			wantLine:  "compacting the context…",
			wantStale: 0,
		},
		{
			name:      "a cut naming no compaction concludes nothing",
			cut:       compactedCut(),
			start:     namedCompacting("c-1"),
			wantLine:  "compacting the context…",
			wantStale: 0,
		},
		{
			name:      "a start signal naming no compaction (an older shim) stands its line",
			cut:       namedCompactedCut("c-1"),
			start:     vendorCompacting(),
			wantLine:  "compacting the context…",
			wantStale: 0,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})
			h.r.OnContextCut(testWS, mainAgent, tt.cut)
			h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})

			// Act
			h.r.OnSessionUpdate(testWS, tt.start)

			// Assert
			if got := drawnCompaction(t, h); got != tt.wantLine {
				t.Fatalf("activity = %q, want %q", got, tt.wantLine)
			}
			if got := len(recordsOf(h.log.Records(), "daemon.footer.compacting_after_its_cut")); got != tt.wantStale {
				t.Fatalf("stale records = %d, want %d", got, tt.wantStale)
			}
		})
	}
}

func TestAStaleStartSignalLeavesNoViolationAtTheTerminal(t *testing.T) {
	// Arrange: the cut lands before its own start signal, on another stream.
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})
	h.r.OnContextCut(testWS, mainAgent, namedCompactedCut("c-1"))
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})
	h.r.OnSessionUpdate(testWS, namedCompacting("c-1"))

	// Act
	endTurn(h, "turn-1")

	// Assert
	if errs := recordsOf(h.log.Records(), outlivedTurn); len(errs) != 0 {
		t.Fatalf("records = %+v, want no violation: the compaction had ended", errs)
	}
}

func TestACompactionTurnsCloseEndsTheCompactingStep(t *testing.T) {
	// Arrange: a /compact turn, whose act alone holds the step.
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActCompact})

	// Act: the prompt queue's door closes the turn, with no vendor clear after.
	h.r.SetTurnEnded(testWS, wsm.CloseCompleted)

	// Assert.
	if got := h.status(t); got != "idle" {
		t.Fatalf("status = %q, want idle: the compaction ended with its turn", got)
	}
}

// TestARestatedCutNamesItsCompaction covers OnContextCutRestated: the same cut
// row is written by the shim (naming its compaction) and by the sidecar
// (which cannot), and whichever was served first was the one routed. The
// identity a re-served cut brings ends the compaction whichever side of its
// start signal it lands on.
func TestARestatedCutNamesItsCompaction(t *testing.T) {
	tests := []struct {
		name string
		// act drives the footer after an unnamed cut ended the turn.
		act       func(h *harness)
		wantLine  string
		wantStale int
	}{
		{
			name: "a restated cut ends the line its start signal stood for",
			act: func(h *harness) {
				h.r.OnSessionUpdate(testWS, namedCompacting("c-1"))
				h.r.OnContextCutRestated(testWS, namedCompactedCut("c-1"))
			},
			wantLine: "",
		},
		{
			name: "a restated cut ahead of its start signal leaves that signal standing nothing",
			act: func(h *harness) {
				h.r.OnContextCutRestated(testWS, namedCompactedCut("c-1"))
				h.r.OnSessionUpdate(testWS, namedCompacting("c-1"))
			},
			wantLine:  "",
			wantStale: 1,
		},
		{
			name: "a restated cut naming another compaction leaves the line standing",
			act: func(h *harness) {
				h.r.OnSessionUpdate(testWS, namedCompacting("c-2"))
				h.r.OnContextCutRestated(testWS, namedCompactedCut("c-1"))
			},
			wantLine: "compacting the context…",
		},
		{
			name: "a restated cut naming no compaction changes nothing",
			act: func(h *harness) {
				h.r.OnSessionUpdate(testWS, namedCompacting("c-1"))
				h.r.OnContextCutRestated(testWS, compactedCut())
			},
			wantLine: "compacting the context…",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the sidecar's version of the cut, naming nothing, came
			// first and ended the turn.
			h := newHarness(t)
			connected(h)
			h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})
			h.r.OnContextCut(testWS, mainAgent, compactedCut())
			h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})

			// Act
			tt.act(h)

			// Assert
			if got := drawnCompaction(t, h); got != tt.wantLine {
				t.Fatalf("activity = %q, want %q", got, tt.wantLine)
			}
			if got := len(recordsOf(h.log.Records(), "daemon.footer.compacting_after_its_cut")); got != tt.wantStale {
				t.Fatalf("stale records = %d, want %d", got, tt.wantStale)
			}
		})
	}
}

func TestARestatedCutLeavesNoViolationAtTheTerminal(t *testing.T) {
	// Arrange: the unnamed cut first, then the start signal, then the shim's
	// version of the cut naming the compaction.
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})
	h.r.OnContextCut(testWS, mainAgent, compactedCut())
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})
	h.r.OnSessionUpdate(testWS, namedCompacting("c-1"))
	h.r.OnContextCutRestated(testWS, namedCompactedCut("c-1"))

	// Act
	endTurn(h, "turn-1")

	// Assert
	if errs := recordsOf(h.log.Records(), outlivedTurn); len(errs) != 0 {
		t.Fatalf("records = %+v, want no violation: the restated cut ended the compaction", errs)
	}
}

// ---- what the gate's compaction spent --------------------------------------

// spentWindow is the window the spend tests' gate measured its figure against.
const spentWindow = 1_000_000

// spent is a phase stating what its summarizer spent.
func spent(phase conversationv1.SessionCompactionPhase, model string, tokens uint64) *conversationv1.SessionCompactionProgress {
	p := progress(phase, 101_600, 12_400, "")
	p.Spend = &conversationv1.SessionCompactionSpend{
		Model: &conversationv1.AgentModel{Name: model}, Tokens: tokens,
	}
	return p
}

// gateStanding stands a cold gate measured against spentWindow.
func gateStanding(h *harness) {
	h.r.SetColdGate(testWS, ColdGate{Standing: true, Cost: FiguredLine{Lead: "the conversation is cold"}, Window: spentWindow})
}

// partsOf flattens a wire line's parts for comparison.
func partsOf(p *frontendv1.FooterColdGateFiguredLine) []any {
	return []any{p.GetLead(), p.GetFigure().GetText(), p.GetFigure().GetWindowFill(), p.GetTail()}
}

func TestCompactionSpendLine(t *testing.T) {
	tests := []struct {
		name   string
		tokens uint64
		window int64
		want   FiguredLine
	}{
		{
			name: "a spend in thousands is scaled as the line's other figures are", tokens: 104_300, window: 1_000_000,
			want: FiguredLine{Lead: "compacted · claude-sonnet-5 spent ", Figure: "104.3k", Tail: " tokens", WindowFill: 0.1043},
		},
		{
			name: "a spend under a thousand is drawn whole", tokens: 950, window: 1_000_000,
			want: FiguredLine{Lead: "compacted · claude-sonnet-5 spent ", Figure: "950", Tail: " tokens", WindowFill: 0.00095},
		},
		{
			name: "a spend beyond the window fills it and no further", tokens: 300_000, window: 200_000,
			want: FiguredLine{Lead: "compacted · claude-sonnet-5 spent ", Figure: "300k", Tail: " tokens", WindowFill: 1},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			spend := &conversationv1.SessionCompactionSpend{Model: &conversationv1.AgentModel{Name: "claude-sonnet-5"}, Tokens: tt.tokens}

			// Act
			got := compactionSpendLine("compacted", spend, tt.window)

			// Assert
			if got != tt.want {
				t.Fatalf("compactionSpendLine = %+v, want %+v", got, tt.want)
			}
		})
	}
}

func TestARunningPhaseWithItsSpendNamesTheSummarizerAndItsTokens(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	gateStanding(h)

	// Act
	h.r.SetColdGateAnswer(testWS, &ColdGateAnswer{Choice: ChoiceCompact,
		Progress: spent(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_SUMMARIZED, "claude-sonnet-5", 104_300)})

	// Assert
	if got := drawnCompaction(t, h); got != "the summary is written; the context is cut · claude-sonnet-5 spent 104.3k tokens" {
		t.Fatalf("activity = %q, want the summarizer and its spend named", got)
	}
}

func TestARunningPhaseWithItsSpendCarriesTheSpendAsTheGatesFigure(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	gateStanding(h)

	// Act
	h.r.SetColdGateAnswer(testWS, &ColdGateAnswer{Choice: ChoiceCompact,
		Progress: spent(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_RESUMING, "claude-sonnet-5", 104_300)})

	// Assert
	line := h.view(t).GetStrip().GetStatus().GetWorking().GetActivity().GetSalient().GetCompaction()
	got := partsOf(line.GetParts())
	want := []any{"resuming the session from the summary… · claude-sonnet-5 spent ", "104.3k", 0.1043, " tokens"}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("parts = %v, want %v", got, want)
		}
	}
}

func TestARunningPhaseWithItsSpendIsExactlyItsParts(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	gateStanding(h)

	// Act
	h.r.SetColdGateAnswer(testWS, &ColdGateAnswer{Choice: ChoiceCompact,
		Progress: spent(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_SUMMARIZED, "claude-sonnet-5", 104_300)})

	// Assert
	line := h.view(t).GetStrip().GetStatus().GetWorking().GetActivity().GetSalient().GetCompaction()
	parts := line.GetParts()
	if whole := parts.GetLead() + parts.GetFigure().GetText() + parts.GetTail(); whole != line.GetText() {
		t.Fatalf("text = %q, want lead+figure+tail %q", line.GetText(), whole)
	}
}

func TestARunningPhaseWithoutASpendCarriesNoParts(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	gateStanding(h)

	// Act
	h.r.SetColdGateAnswer(testWS, &ColdGateAnswer{Choice: ChoiceCompact,
		Progress: progress(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_SUMMARIZING, 101_600, 0, "")})

	// Assert
	line := h.view(t).GetStrip().GetStatus().GetWorking().GetActivity().GetSalient().GetCompaction()
	if line.GetParts() != nil || line.GetText() != "summarizing the conversation (101.6k)…" {
		t.Fatalf("line = %+v, want the plain sentence and no parts", line)
	}
}

func TestTheConcludedCompactionNamesTheSummarizersSpendInParts(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	gateStanding(h)

	// Act
	h.r.SetColdGateAnswer(testWS, &ColdGateAnswer{Choice: ChoiceCompact,
		Progress: spent(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_STARTED, "claude-sonnet-5", 104_300)})
	// The re-open landed: the gate retires before the answer does, as the
	// verb that settles it orders the two.
	h.r.SetColdGate(testWS, ColdGate{Standing: false})
	h.r.SetColdGateAnswer(testWS, nil)

	// Assert
	concluded := transientOf(t, h).GetCompactionConcluded()
	got := append([]any{concluded.GetText()}, partsOf(concluded.GetParts())...)
	want := []any{
		"compacted and resumed (101.6k → 12.4k) · claude-sonnet-5 spent 104.3k tokens",
		"compacted and resumed (101.6k → 12.4k) · claude-sonnet-5 spent ", "104.3k", 0.1043, " tokens",
	}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("compaction_concluded = %v, want %v", got, want)
		}
	}
}

func TestAPhaseOnTheSessionStreamNamesItsSpendAsTheAnswersDoes(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	gateStanding(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActCompact})

	// Act
	h.r.OnSessionUpdate(testWS, sessionCompactionProgress(
		spent(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_RESUMING, "claude-haiku-4-5", 2_000)))
	// The standing gate outranks the working line; with it lifted, the line
	// the phase stood is the one drawn.
	h.r.SetColdGate(testWS, ColdGate{Standing: false})

	// Assert
	line := h.view(t).GetStrip().GetStatus().GetWorking().GetActivity().GetSalient().GetCompaction()
	got := append([]any{line.GetText()}, partsOf(line.GetParts())...)
	want := []any{
		"resuming the session from the summary… · claude-haiku-4-5 spent 2k tokens",
		"resuming the session from the summary… · claude-haiku-4-5 spent ", "2k", 0.002, " tokens",
	}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("line = %v, want %v", got, want)
		}
	}
}

func TestASpendWithNoGateStandingIsRecordedAndFailsHard(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("a spend with no gate standing did not fail hard")
		}
		errs := recordsOf(h.log.Records(), "daemon.footer.compaction_spend_without_gate")
		if len(errs) != 1 || errs[0].Level != dlog.LevelError ||
			errs[0].Context["summarizer_model"] != "claude-sonnet-5" || errs[0].Context["gate_standing"] != false {
			t.Fatalf("records = %+v, want one ERROR naming the summarizer and the absent gate", errs)
		}
	}()

	// Act
	h.r.SetColdGateAnswer(testWS, &ColdGateAnswer{Choice: ChoiceCompact,
		Progress: spent(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_SUMMARIZED, "claude-sonnet-5", 104_300)})
}

func TestAColdGateAnswerSettingNeitherLineIsRecordedAndFailsHard(t *testing.T) {
	tests := []struct {
		name   string
		answer *ColdGateAnswer
	}{
		{name: "neither", answer: &ColdGateAnswer{Choice: ChoiceCompact}},
		{name: "both", answer: &ColdGateAnswer{Choice: ChoiceCompact, Request: "compaction requested",
			Progress: progress(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_SUMMARIZING, 0, 0, "")}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			defer func() {
				// Assert
				if recover() == nil {
					t.Fatal("a malformed answer did not fail hard")
				}
				errs := recordsOf(h.log.Records(), "daemon.footer.cold_gate_answer_malformed")
				if len(errs) != 1 || errs[0].Level != dlog.LevelError || errs[0].Context["choice"] != ChoiceCompact {
					t.Fatalf("records = %+v, want one ERROR naming the answer", errs)
				}
			}()

			// Act
			h.r.SetColdGateAnswer(testWS, tt.answer)
		})
	}
}

func TestLineOfNamesWhatTheAnswerSays(t *testing.T) {
	tests := []struct {
		name   string
		answer *ColdGateAnswer
		want   string
	}{
		{name: "no answer", answer: nil, want: ""},
		{name: "a request", answer: &ColdGateAnswer{Request: "compaction requested"}, want: "compaction requested"},
		{name: "a relayed phase", answer: &ColdGateAnswer{
			Progress: progress(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_STARTED, 0, 0, "")},
			want: "SESSION_COMPACTION_PHASE_STARTED"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := lineOf(tt.answer)

			// Assert
			if got != tt.want {
				t.Fatalf("lineOf = %q, want %q", got, tt.want)
			}
		})
	}
}
