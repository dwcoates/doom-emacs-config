package footer

import (
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
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
	h.r.SetColdGate(testWS, ColdGate{Standing: true, Detail: "the conversation is cold"})

	// Act.
	h.r.SetColdGateAnswer(testWS, &ColdGateAnswer{Choice: ChoiceCompact, Text: "compaction requested"})

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
	h.r.SetColdGate(testWS, ColdGate{Standing: true, Detail: "the conversation is cold"})

	// Act.
	h.r.SetColdGateAnswer(testWS, &ColdGateAnswer{Choice: ChoiceCompact, Text: "compaction requested"})

	// Assert.
	got := h.view(t).GetStrip().GetStatus().GetWorking().GetActivity().GetSalient().GetCompaction().GetText()
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
	thinking := h.view(t).GetStrip().GetStatus().GetWorking()
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
	thinking := h.view(t).GetStrip().GetStatus().GetWorking()
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
	if got := h.view(t).GetStrip().GetStatus().GetWorking().GetActivity().GetSalient().GetCompaction(); got != empty {
		t.Fatalf("activity = %+v, want no compaction line", got)
	}
}

// ---- the line's lifetime ----------------------------------------------------

// outlivedTurn is the operation of the invariant-violation record.
const outlivedTurn = "daemon.footer.compaction_line_outlived_turn"

// compactionText is the compaction line the published view draws, empty when
// none stands.
func compactionText(t *testing.T, h *harness) string {
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

// recordsOf answers every record of one operation.
func recordsOf(records []dlog.Record, operation string) []dlog.Record {
	var out []dlog.Record
	for _, rec := range records {
		if rec.Operation == operation {
			out = append(out, rec)
		}
	}
	return out
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
	if got := compactionText(t, h); got != "" {
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
	if got := compactionText(t, h); got != "" {
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
	if got := compactionText(t, h); got != "" {
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
	if got := compactionText(t, h); got != vendorCompactionLine {
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
			h.r.SetColdGate(testWS, ColdGate{Standing: true, Detail: "the conversation is cold"})
			h.r.SetColdGateAnswer(testWS, &ColdGateAnswer{Choice: ChoiceCompact, Text: "compacted and resumed"})

			// Act
			tt.act(h)

			// Assert
			if got := compactionText(t, h); got != tt.want {
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
			if got := compactionText(t, h); got != "" {
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

func TestAFailedCompactionIsAnnouncedAsTheBudgetLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})

	// Act
	h.r.OnSessionUpdate(testWS, sessionCompactionProgress(
		progress(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_FAILED, 0, 0, "the summary was empty")))

	// Assert
	transient := transientOf(t, h)
	if transient.GetContextBudget().GetText() != "compaction failed — the summary was empty" || transient.GetCompactionConcluded() != nil {
		t.Fatalf("transient = %+v, want the failure as the context_budget line", transient)
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

			// Act
			h.r.OnSessionUpdate(testWS, sessionCompactionProgress(progress(tt.phase, 101_600, 12_400, "")))

			// Assert: no timer may end a salient line, and a transient's expiry is the client's.
			if n := len(h.clock.pending); n != 0 {
				t.Fatalf("%d timers pending after the conclusion, want none", n)
			}
		})
	}
}

func TestAConclusionUnderAColdGateAnswerStillRaisesItsTransient(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetColdGate(testWS, ColdGate{Standing: true, Detail: "the conversation is cold"})
	h.r.SetColdGateAnswer(testWS, &ColdGateAnswer{Choice: ChoiceCompact, Text: "resuming the session from the summary…"})
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
