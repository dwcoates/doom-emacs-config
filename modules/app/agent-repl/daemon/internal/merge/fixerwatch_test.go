package merge

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// fixerTurnEnded records TURN of ws-fixer as ended.
func fixerTurnEnded(h *harness, turn ids.TurnID) {
	closed, at := wsm.CloseCompleted, deployStarted.Add(2*time.Minute)
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	h.db.turns["ws-fixer"] = append(h.db.turns["ws-fixer"], wsm.Turn{ID: turn, Workspace: "ws-fixer", Close: &closed, ClosedAt: &at})
}

// watch settles the harness's dispatched check as M records it.
func watch(t *testing.T, h *harness, m wsm.Mediation) {
	t.Helper()
	ctx := context.Background()
	q, ok, err := h.o.checkedQueueOf(ctx, mediationKey(h))
	if err != nil || !ok {
		t.Fatalf("checkedQueueOf = (%v, %v)", ok, err)
	}
	h.o.watchFixer(ctx, q, m, dlog.Context{})
}

// stillDispatched fails unless the check is still dispatched to ws-fixer and
// not escalated.
func stillDispatched(t *testing.T, h *harness) {
	t.Helper()
	if m := owedCheck(t, h); m == nil || !m.Dispatched || m.Escalated() {
		t.Fatalf("check %+v, want it still dispatched to the fixer", m)
	}
}

func TestADispatchedFixerWhoseTurnEndedWithoutAFixIsEscalated(t *testing.T) {
	// Arrange
	h := finishing(t)
	fixerTurnEnded(h, aFixerTurn.ID)

	// Act
	watch(t, h, *owedCheck(t, h))

	// Assert
	if heldEscalation(t, h) != "ws-fixer" || !strings.Contains(owedCheck(t, h).EscalationBrief, fixerStoppedSummary) {
		t.Fatalf("check %+v, want it escalated to the fixer with why", owedCheck(t, h))
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opRemediationFinish,
		Message: "the deploy remediation cannot land its fix; the chain is escalated to the user and the queue is held for them"}); !ok {
		t.Fatalf("records = %v, want the escalation recorded", h.logs.Records())
	}
}

func TestADispatchedFixerAtWorkHoldsTheQueue(t *testing.T) {
	// Arrange: the fixer's brief reached it and runs.
	h := finishing(t)

	// Act
	watch(t, h, *owedCheck(t, h))

	// Assert
	stillDispatched(t, h)
	if len(h.awaitedTurns) != 0 {
		t.Fatalf("awaited %v, want nothing waited on", h.awaitedTurns)
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "info", Operation: opMediation, Message: "the fixer's turn is under way; its end runs the check's next round"}); !ok {
		t.Fatalf("records = %v, want the turn under way recorded", h.logs.Records())
	}
}

func TestAFixerWhoseTurnEndedWithItsFinishUnderWayIsNotEscalated(t *testing.T) {
	// Arrange
	h := finishing(t)
	fixerTurnEnded(h, aFixerTurn.ID)
	if _, err := h.db.BeginRemediationFinish(context.Background(), wsm.RemediationFinish{Workspace: "ws-fixer", Repo: mediationKey(h), StartedAt: deployStarted}); err != nil {
		t.Fatal(err)
	}

	// Act
	watch(t, h, *owedCheck(t, h))

	// Assert
	stillDispatched(t, h)
}

func TestAFixersBriefThatNeverReachedTheQueueIsSubmittedAsItWas(t *testing.T) {
	// Arrange: the brief was recorded, and the daemon stopped before it was
	// submitted.
	h := finishing(t)
	brief := wsm.FixerTurn{ID: "turn-unsent", Brief: "the gate failed; fix it", Origin: int32(conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR)}
	if ok, err := h.db.RecordFixerTurn(context.Background(), mediationKey(h), deployStarted, "ws-fixer", brief); err != nil || !ok {
		t.Fatalf("RecordFixerTurn = (%v, %v)", ok, err)
	}

	// Act
	watch(t, h, *owedCheck(t, h))

	// Assert
	stillDispatched(t, h)
	subs := h.queue.submissions
	if len(subs) != 1 || subs[0].WS != "ws-fixer" || subs[0].Turn != "turn-unsent" || subs[0].Origin != conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR || submittedText(subs[0]) != brief.Brief {
		t.Fatalf("submitted %+v, want the recorded brief submitted under its own turn and origin", subs)
	}
}

func TestADispatchAnEarlierBuildRecordedStandsOnTheFixersOpenTurns(t *testing.T) {
	tests := []struct {
		name          string
		open          bool
		wantEscalated bool
	}{
		{name: "an open turn holds the queue for the fixer", open: true},
		{name: "no open turn is a fixer that stopped", wantEscalated: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := finishing(t)
			if tt.open {
				h.db.mu.Lock()
				h.db.turns["ws-fixer"] = append(h.db.turns["ws-fixer"], wsm.Turn{ID: "turn-user", Workspace: "ws-fixer"})
				h.db.mu.Unlock()
			}
			m := *owedCheck(t, h)
			m.FixerTurn = wsm.FixerTurn{}

			// Act
			watch(t, h, m)

			// Assert
			if got := owedCheck(t, h).Escalated(); got != tt.wantEscalated {
				t.Fatalf("escalated %v, want %v", got, tt.wantEscalated)
			}
		})
	}
}

func TestAFixerThatStoppedButCannotBeEscalatedHoldsTheQueueLoudly(t *testing.T) {
	// Arrange
	h := finishing(t)
	fixerTurnEnded(h, aFixerTurn.ID)
	m := *owedCheck(t, h)
	h.db.mu.Lock()
	h.db.mediationErr = errors.New("database is locked")
	h.db.mu.Unlock()

	// Act
	watch(t, h, m)

	// Assert
	rec, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation,
		Message: "the fixer stopped without a fix but its check could not be escalated; the queue stays held"})
	if !ok || rec.Context["error"] != "database is locked" || rec.Context["remediation"] != "ws-fixer" {
		t.Fatalf("records = %v, want the failed escalation recorded with its cause", h.logs.Records())
	}
}

func TestFixerTurnEndedSettlesOnlyADispatchedFixersCheck(t *testing.T) {
	tests := []struct {
		name          string
		ws            ids.WorkspaceID
		wantEscalated bool
	}{
		{name: "the dispatched fixer's turn end settles its check", ws: "ws-fixer", wantEscalated: true},
		{name: "another workspace's turn end is no check's", ws: "ws-one"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := finishing(t)
			fixerTurnEnded(h, aFixerTurn.ID)

			// Act
			h.o.FixerTurnEnded(context.Background(), tt.ws)

			// Assert
			if got := owedCheck(t, h).Escalated(); got != tt.wantEscalated {
				t.Fatalf("escalated %v, want %v", got, tt.wantEscalated)
			}
		})
	}
}

func TestBriefFixer(t *testing.T) {
	tests := []struct {
		name       string
		ws         ids.WorkspaceID
		wantRecord bool
	}{
		{name: "the dispatched fixer's brief is recorded on its check before it is submitted", ws: "ws-fixer", wantRecord: true},
		{name: "a fixer no dispatch is out to is briefed and not watched", ws: "ws-other"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := finishing(t)

			// Act
			err := h.o.briefFixer(context.Background(), mediationKey(h), tt.ws, "the gate failed; fix it", conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR, dlog.Context{})

			// Assert
			subs := h.queue.submissions
			if err != nil || len(subs) == 0 || subs[0].WS != tt.ws || submittedText(subs[0]) != "the gate failed; fix it" {
				t.Fatalf("briefFixer = %v, submitted %+v; want the brief submitted", err, subs)
			}
			recorded := owedCheck(t, h).FixerTurn
			if (recorded.ID == subs[0].Turn && recorded.Brief == "the gate failed; fix it") != tt.wantRecord {
				t.Fatalf("check's fixer turn %+v, submitted turn %s; want recorded %v", recorded, subs[0].Turn, tt.wantRecord)
			}
		})
	}
}

func TestBriefFixerThatCannotReadItsCheckSubmitsNothing(t *testing.T) {
	// Arrange
	h := finishing(t)
	h.db.mu.Lock()
	h.db.mediationErr = errors.New("database is locked")
	h.db.mu.Unlock()

	// Act
	err := h.o.briefFixer(context.Background(), mediationKey(h), "ws-fixer", "fix it", conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR, dlog.Context{})

	// Assert
	if err == nil || len(h.queue.submissions) != 0 {
		t.Fatalf("briefFixer = %v, submitted %d; want the failure answered and nothing submitted", err, len(h.queue.submissions))
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation, Message: "could not read the check the fixer's brief is for; it is not briefed"}); !ok {
		t.Fatalf("records = %v, want the failure recorded", h.logs.Records())
	}
}
