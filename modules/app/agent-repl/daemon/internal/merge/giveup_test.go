package merge

import (
	"context"
	"errors"
	"slices"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// openTurn records TURN of the harness's workspace open, as ORIGIN.
func openTurn(h *harness, turn ids.TurnID, origin conversationv1.PromptOrigin) {
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	h.db.turns[theWorkspace] = append(h.db.turns[theWorkspace], wsm.Turn{ID: turn, Workspace: theWorkspace, Origin: origin.String()})
}

// offQueueCauseOf reads why the harness's workspace's merge left its queue,
// "" while it has not.
func offQueueCauseOf(t *testing.T, h *harness) string {
	t.Helper()
	records, err := h.db.OffQueueMerges(context.Background())
	if err != nil {
		t.Fatal(err)
	}
	for _, r := range records {
		if r.Workspace == theWorkspace {
			return r.Cause
		}
	}
	return ""
}

func TestGiveUpRefusesAWorkspaceRunningNoRepairTurn(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
	}{
		{name: "no turn runs"},
		{name: "only the user's own turn runs", arrange: func(h *harness) {
			openTurn(h, "turn-user", conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT)
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			enqueue(t, h)
			if tt.arrange != nil {
				tt.arrange(h)
			}

			// Act
			err := h.o.GiveUp(context.Background(), theWorkspace, "the conflict needs a redesign")

			// Assert
			var refusal *RefusalError
			if !errors.As(err, &refusal) || refusal.Arm != ArmNoRepairTurn {
				t.Fatalf("GiveUp = %v, want it refused %s", err, ArmNoRepairTurn)
			}
			if cause := offQueueCauseOf(t, h); cause != "" {
				t.Fatalf("the merge left its queue for %q, want it still in line", cause)
			}
		})
	}
}

func TestGiveUpFromAFixingTurnTakesTheMergeOffItsQueueWithoutAnotherGate(t *testing.T) {
	// Arrange: the head's suite fails, and its fixing turn runs.
	c := newChain(t)
	c.runner.script(c.dirs[theWorkspace], failingSuiteRun)
	c.h.mu.Lock()
	c.h.awaitGate, c.h.awaitStarted = make(chan struct{}), make(chan ids.TurnID, 4)
	c.h.mu.Unlock()
	defer close(c.h.awaitGate)
	enqueue(t, c.h)
	done := admitAsync(c.h, context.Background())
	<-c.h.awaitStarted
	openTurn(c.h, "turn-fix", conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR)

	// Act: the agent gives up from inside that turn, which still runs.
	err := c.h.o.GiveUp(context.Background(), theWorkspace, "the failures need a redesign of the store")
	<-done

	// Assert
	if err != nil || offQueueCauseOf(t, c.h) != string(CauseAgentGaveUp) {
		t.Fatalf("GiveUp = %v, off queue for %q; want the merge given up", err, offQueueCauseOf(t, c.h))
	}
	c.runner.mu.Lock()
	n := c.runner.calls[c.dirs[theWorkspace]]
	c.runner.mu.Unlock()
	if n != 1 {
		t.Fatalf("gate runs = %d, want only the one that failed", n)
	}
	if _, ok := dlog.FindRecord(c.h.logs.Records(), dlog.RecordQuery{Level: "info", Operation: "daemon.merge.give_up", Message: "the merge's own agent gave it up from its repair turn; it leaves the queue"}); !ok {
		t.Fatalf("records = %v, want the give-up recorded with its reason", c.h.logs.Records())
	}
}

func TestGiveUpThatCannotReadTheTurnsGivesNothingUp(t *testing.T) {
	// Arrange
	h := newHarness(t)
	enqueue(t, h)
	h.db.mu.Lock()
	h.db.openTurnsErr = errors.New("database is locked")
	h.db.mu.Unlock()

	// Act
	err := h.o.GiveUp(context.Background(), theWorkspace, "the conflict needs a redesign")

	// Assert
	if err == nil || offQueueCauseOf(t, h) != "" {
		t.Fatalf("GiveUp = %v, off queue for %q; want the failure answered and the merge in line", err, offQueueCauseOf(t, h))
	}
	rec, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.merge.give_up",
		Message: "could not read whether the workspace runs a repair turn; its merge is not given up"})
	if !ok || rec.Context["error"] != "database is locked" || rec.Context["reason"] != "the conflict needs a redesign" {
		t.Fatalf("records = %v, want the failure recorded with its cause and the reason", h.logs.Records())
	}
}

func TestRepairOriginsAreEveryRepairsOrigin(t *testing.T) {
	// Act
	got := repairOrigins

	// Assert: a fixing attempt's and a conflict resolution's, and nothing else.
	want := []string{
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR.String(),
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR.String(),
	}
	if !slices.Equal(got, want) {
		t.Fatalf("repairOrigins = %v, want %v", got, want)
	}
}
