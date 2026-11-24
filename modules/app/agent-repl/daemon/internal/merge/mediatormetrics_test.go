package merge

import (
	"context"
	"errors"
	"slices"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/sourcescan"
	"agent-repld/internal/wsm"
)

// aTurnSpend is a mediator turn's spend, each kind its own figure.
var aTurnSpend = footer.TurnSpent{Turn: "turn-spent", StartedAt: deployStarted, FreshInput: 1200, CacheRead: 45000, Output: 800}

// mediatorTokens is what the meter recorded of the mediator's turns.
func mediatorTokens(h *harness) []string {
	var out []string
	for _, m := range h.meter.all() {
		if strings.HasPrefix(m, "mediator_tokens:") {
			out = append(out, m)
		}
	}
	return out
}

// spentIn is the meter's record of aTurnSpend ended in PHASE.
func spentIn(phase string) []string {
	return []string{
		"mediator_tokens:" + phase + ":" + MediatorTokensFreshInput + ":1200",
		"mediator_tokens:" + phase + ":" + MediatorTokensCacheRead + ":45000",
		"mediator_tokens:" + phase + ":" + MediatorTokensOutput + ":800",
	}
}

// beginFinish records the mediator's finish under way at its gate, serving
// an escalation when ESCALATION is set.
func beginFinish(t *testing.T, h *harness, escalation bool) {
	t.Helper()
	if _, err := h.db.BeginRemediationFinish(context.Background(), wsm.RemediationFinish{Workspace: "ws-fixer", Repo: mediationKey(h), StartedAt: deployStarted, ServesEscalation: escalation}); err != nil {
		t.Fatalf("BeginRemediationFinish: %v", err)
	}
}

func TestAMediatorsTurnIsMeteredByThePhaseItEndedIn(t *testing.T) {
	later := deployStarted.Add(time.Hour)
	tests := []struct {
		name    string
		arrange func(t *testing.T, h *harness)
		started time.Time
		want    string
	}{
		{name: "a check its mediator is not told of yet is rolling out", want: MediatorPhaseRollout,
			arrange: func(t *testing.T, h *harness) { owe(t, h) }},
		{name: "a check its mediator was told of is assessed", want: MediatorPhaseAssessing,
			arrange: func(t *testing.T, h *harness) { owe(t, h); told(t, h, deployStarted, "turn-told") }},
		{name: "a check dispatched is fixed", want: MediatorPhaseFixing,
			arrange: func(t *testing.T, h *harness) {
				owe(t, h)
				if err := dispatched(h, Decision{}); err != nil {
					t.Fatal(err)
				}
			}},
		{name: "a check escalated is the escalation's", want: MediatorPhaseEscalation,
			arrange: func(t *testing.T, h *harness) {
				owe(t, h)
				if err := decideTold(h, Decision{Kind: MediationEscalate, Summary: "yours", Problems: aProblem}); err != nil {
					t.Fatal(err)
				}
			}},
		{name: "a finish serving an escalation is the escalation's", want: MediatorPhaseEscalation,
			arrange: func(t *testing.T, h *harness) { beginFinish(t, h, true) }},
		{name: "a finish of a dispatched fix is the fix's", want: MediatorPhaseFixing,
			arrange: func(t *testing.T, h *harness) { beginFinish(t, h, false) }},
		{name: "a turn that approved its check ended the assessment", want: MediatorPhaseAssessing,
			arrange: func(t *testing.T, h *harness) {
				owe(t, h)
				if err := decideTold(h, Decision{Kind: MediationApprove, Summary: "clean"}); err != nil {
					t.Fatal(err)
				}
			}},
		{name: "a turn that approved an escalated check ended the escalation", want: MediatorPhaseEscalation,
			arrange: func(t *testing.T, h *harness) {
				owe(t, h)
				if err := decideTold(h, Decision{Kind: MediationEscalate, Summary: "yours", Problems: aProblem}); err != nil {
					t.Fatal(err)
				}
				if err := decideTold(h, Decision{Kind: MediationApprove, Summary: "clean after all"}); err != nil {
					t.Fatal(err)
				}
			}},
		{name: "a turn that started after the run ended is idle", want: MediatorPhaseIdle, started: later,
			arrange: func(t *testing.T, h *harness) {
				owe(t, h)
				if err := decideTold(h, Decision{Kind: MediationApprove, Summary: "clean"}); err != nil {
					t.Fatal(err)
				}
			}},
		{name: "a turn with no run is idle", want: MediatorPhaseIdle, arrange: func(*testing.T, *harness) {}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			withMediator(t, h)
			tt.arrange(t, h)
			spend := aTurnSpend
			if !tt.started.IsZero() {
				spend.StartedAt = tt.started
			}

			// Act
			h.o.MediatorTurnSpent(context.Background(), "ws-fixer", spend)

			// Assert
			if got := mediatorTokens(h); !slices.Equal(got, spentIn(tt.want)) {
				t.Fatalf("metered %v, want %v", got, spentIn(tt.want))
			}
			if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "info", Operation: opMediation, Message: "metered the deploy mediator's turn"}); !ok {
				t.Fatalf("records = %v, want the metered turn recorded", h.logs.Records())
			}
		})
	}
}

func TestATurnOfAnyOtherWorkspaceIsNotMetered(t *testing.T) {
	// Arrange
	h := checked(t)
	withMediator(t, h)
	owe(t, h)

	// Act
	h.o.MediatorTurnSpent(context.Background(), theWorkspace, aTurnSpend)

	// Assert
	if got := mediatorTokens(h); len(got) != 0 {
		t.Fatalf("metered %v, want nothing", got)
	}
}

func TestAMediatorsTurnWhoseReadsFailIsRecordedAndUnmetered(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		message string
	}{
		{name: "whether it is a mediator cannot be read",
			arrange: func(h *harness) { h.db.mediatorRepoErr = errors.New("database is locked") },
			message: "could not read whether the workspace whose turn ended is a deploy mediator; its turn's tokens are not metered"},
		{name: "its check cannot be read",
			arrange: func(h *harness) { h.db.mediationErr = errors.New("database is locked") },
			message: "could not read the phase the deploy mediator's turn ended in; its tokens are not metered"},
		{name: "its finishes cannot be read",
			arrange: func(h *harness) { h.db.remediationFinishesErr = errors.New("database is locked") },
			message: "could not read the phase the deploy mediator's turn ended in; its tokens are not metered"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			withMediator(t, h)
			owe(t, h)
			h.db.mu.Lock()
			tt.arrange(h)
			h.db.mu.Unlock()

			// Act
			h.o.MediatorTurnSpent(context.Background(), "ws-fixer", aTurnSpend)

			// Assert
			if got := mediatorTokens(h); len(got) != 0 {
				t.Fatalf("metered %v, want nothing", got)
			}
			if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation, Message: tt.message}); !ok {
				t.Fatalf("records = %v, want %q at ERROR", h.logs.Records(), tt.message)
			}
		})
	}
}

// mediatorRuns is what the meter counted of the mediator's runs.
func mediatorRuns(h *harness) []string {
	var out []string
	for _, m := range h.meter.all() {
		if strings.HasPrefix(m, "mediator_run:") {
			out = append(out, m)
		}
	}
	return out
}

func TestAnApprovalCountsTheMediatorsRunOnce(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, h *harness)
		want    []string
	}{
		{name: "an approval that released its check", arrange: func(*testing.T, *harness) {},
			want: []string{"mediator_run:" + MediatorRunApproved}},
		{name: "an approval a newer deploy raced releases nothing and counts nothing",
			arrange: func(t *testing.T, h *harness) {
				// The check is replaced under the decision: its release
				// finds a newer deploy owed.
				h.db.mu.Lock()
				h.db.onRelease = func() {
					h.db.mu.Lock()
					h.db.onRelease = nil
					h.db.mu.Unlock()
					oweAt(t, h, deployStarted.Add(time.Minute))
				}
				h.db.mu.Unlock()
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			withMediator(t, h)
			owe(t, h)
			tt.arrange(t, h)

			// Act
			err := decideTold(h, Decision{Kind: MediationApprove, Summary: "clean"})

			// Assert
			if got := mediatorRuns(h); err != nil || !slices.Equal(got, tt.want) {
				t.Fatalf("DecideMediation = %v, counted %v; want %v", err, got, tt.want)
			}
		})
	}
}

func TestALandedFinishCountsTheMediatorsRunByWhatItServed(t *testing.T) {
	tests := []struct {
		name       string
		escalation bool
		want       string
	}{
		{name: "a dispatched fix", want: MediatorRunFinishedRemediation},
		{name: "an escalation", escalation: true, want: MediatorRunFinishedEscalation},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := finishing(t)
			// A finish no dispatched check holds serves an escalation the
			// user settles with the mediator.
			if tt.escalation {
				if released, err := h.db.ReleaseMediation(context.Background(), mediationKey(h), deployStarted); err != nil || !released.Released {
					t.Fatalf("ReleaseMediation = (%v, %v)", released, err)
				}
			}

			// Act
			err := h.o.FinishRemediation(context.Background(), "ws-fixer", aRemediationPatch)

			// Assert
			if got := mediatorRuns(h); err != nil || !slices.Equal(got, []string{"mediator_run:" + tt.want}) {
				t.Fatalf("FinishRemediation = %v, counted %v; want one %s", err, got, tt.want)
			}
		})
	}
}

func TestEachCompactionOfTheMediatorIsTimedByHowItEnded(t *testing.T) {
	tests := []struct {
		name       string
		compactErr error
		want       string
	}{
		{name: "a compaction that compacts", want: MediatorCompactionCompacted},
		{name: "a compaction that fails", compactErr: errors.New("summarizer refused"), want: MediatorCompactionFailed},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			withMediator(t, h)
			h.checks.compactErr = tt.compactErr

			// Act
			h.o.compactMediator(context.Background(), mediationKey(h), "a test compacts it", dlog.Context{})

			// Assert
			var got []string
			for _, m := range h.meter.all() {
				if strings.HasPrefix(m, "mediator_compaction:") {
					got = append(got, m)
				}
			}
			if !slices.Equal(got, []string{"mediator_compaction:" + tt.want}) || len(h.meter.compactionTook) != 1 || h.meter.compactionTook[0] <= 0 {
				t.Fatalf("timed %v over %v, want one %s with its duration", got, h.meter.compactionTook, tt.want)
			}
		})
	}
}

func TestEachCompactionOwedAfterARemediationIsTimedByHowItEnded(t *testing.T) {
	tests := []struct {
		name    string
		notOwed bool
		oweErr  error
		want    string
	}{
		{name: "a compaction owed to the next start", want: MediatorCompactionOwed},
		{name: "a mediator with nothing to compact", notOwed: true, want: MediatorCompactionSkipped},
		{name: "a compaction that cannot be owed", oweErr: errors.New("database is locked"), want: MediatorCompactionFailed},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			withMediator(t, h)
			h.checks.notOwed, h.checks.oweErr = tt.notOwed, tt.oweErr

			// Act
			h.o.compactMediatorAfterRemediation(context.Background(), mediationKey(h), dlog.Context{})

			// Assert
			var got []string
			for _, m := range h.meter.all() {
				if strings.HasPrefix(m, "mediator_compaction:") {
					got = append(got, m)
				}
			}
			if !slices.Equal(got, []string{"mediator_compaction:" + tt.want}) || len(h.meter.compactionTook) != 1 {
				t.Fatalf("timed %v over %v, want one %s with its duration", got, h.meter.compactionTook, tt.want)
			}
			if len(h.checks.compacted) != 0 {
				t.Fatalf("compacted %v, want no compaction run", h.checks.compacted)
			}
		})
	}
}

func TestMediatorOwedCompactionOutcome(t *testing.T) {
	tests := []struct {
		name string
		owed bool
		err  error
		want string
	}{
		{name: "owed", owed: true, want: MediatorCompactionOwed},
		{name: "nothing to compact", want: MediatorCompactionSkipped},
		{name: "a failure", err: errors.New("boom"), want: MediatorCompactionFailed},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := mediatorOwedCompactionOutcome(tt.owed, tt.err)

			// Assert
			if got != tt.want {
				t.Fatalf("mediatorOwedCompactionOutcome = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestMediatorCompactionOutcome(t *testing.T) {
	tests := []struct {
		name      string
		compacted bool
		err       error
		want      string
	}{
		{name: "compacted", compacted: true, want: MediatorCompactionCompacted},
		{name: "nothing to compact", want: MediatorCompactionSkipped},
		{name: "a failure", err: errors.New("boom"), want: MediatorCompactionFailed},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := mediatorCompactionOutcome(tt.compacted, tt.err)

			// Assert
			if got != tt.want {
				t.Fatalf("mediatorCompactionOutcome = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestATurnSpendAnswersEveryKindAndPanicsOnAnyOther(t *testing.T) {
	// Act / Assert
	for _, kind := range MediatorTokenKinds {
		if spentOf(aTurnSpend, kind) == 0 {
			t.Fatalf("tokens(%q) = 0, want the kind's figure", kind)
		}
	}
	defer func() {
		if recover() == nil {
			t.Fatal("tokens of an unknown kind did not panic")
		}
	}()
	spentOf(aTurnSpend, "thinking")
}

func TestAFinishsPhaseAndRunEndFollowWhatItServes(t *testing.T) {
	tests := []struct {
		name      string
		f         wsm.RemediationFinish
		wantPhase string
		wantEnd   string
	}{
		{name: "a dispatched fix", wantPhase: MediatorPhaseFixing, wantEnd: MediatorRunFinishedRemediation},
		{name: "an escalation", f: wsm.RemediationFinish{ServesEscalation: true}, wantPhase: MediatorPhaseEscalation, wantEnd: MediatorRunFinishedEscalation},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			phase, end := finishPhase(tt.f), finishRunEnd(tt.f)

			// Assert
			if phase != tt.wantPhase || end != tt.wantEnd {
				t.Fatalf("finishPhase, finishRunEnd = %q, %q; want %q, %q", phase, end, tt.wantPhase, tt.wantEnd)
			}
		})
	}
}

// TestEveryFinishPhaseGoesThroughFinishPhase keeps the turn meter and the run
// counter labeling a finish alike: a site spelling a finish's phase by hand
// fails here.
func TestEveryFinishPhaseGoesThroughFinishPhase(t *testing.T) {
	if sites := sourcescan.Sites(t, "f.ServesEscalation, MediatorPhaseFixing"); !slices.Equal(sites, []string{"mediatormetrics.go"}) {
		t.Fatalf("a finish's phase is spelled in %v, want only finishPhase in mediatormetrics.go", sites)
	}
}
