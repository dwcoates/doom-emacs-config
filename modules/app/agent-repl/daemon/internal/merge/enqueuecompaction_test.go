package merge

import (
	"context"
	"errors"
	"slices"
	"testing"

	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// requestAndWait files the harness workspace's own-branch finish and runs its
// wait to the end: the finish stage, the enqueue compaction, and its place in
// line.
func requestAndWait(t *testing.T, h *harness) {
	t.Helper()
	if err := h.request(t, ownBranch, RequestedByAgent); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	h.runWait(t)
}

func TestAFinishEntersTheQueueCompactedOnSonnet(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	requestAndWait(t, h)

	// Assert
	if !slices.Equal(h.queueCompactions, []ids.WorkspaceID{theWorkspace}) {
		t.Fatalf("enqueue compactions = %v, want the finishing workspace's", h.queueCompactions)
	}
	if want := []modelSwitch{{ws: theWorkspace, model: EnqueueModel}}; !slices.Equal(h.modelSwitches, want) {
		t.Fatalf("model switches = %v, want %v", h.modelSwitches, want)
	}
	if state, _ := h.entryState(); state != wsm.MergeQueued {
		t.Fatalf("queue entry state = %v, want queued once compacted", state)
	}
	if got := h.db.restoreModels[theWorkspace]; got != testSessionModel {
		t.Fatalf("restore model = %q, want the session's model before the switch %q", got, testSessionModel)
	}
	if !slices.Contains(h.briefNames, BriefEnqueueCompaction) {
		t.Fatalf("briefs = %v, want the compaction composed from %s", h.briefNames, BriefEnqueueCompaction)
	}
}

func TestTheMergeIsNotInLineWhileItsEnqueueCompactionRuns(t *testing.T) {
	// Arrange
	h := newHarness(t)
	var stateWhileCompacting wsm.MergeQueueState
	h.duringQueueCompaction = func() { stateWhileCompacting, _ = h.entryState() }

	// Act
	requestAndWait(t, h)

	// Assert
	if stateWhileCompacting != wsm.MergeRequested {
		t.Fatalf("queue entry state while compacting = %v, want only requested", stateWhileCompacting)
	}
}

func TestTheEnqueueCompactionRunsAfterTheFinishStage(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.finishHooks(anyTarget(promptHookOf("open the pull request")))
	var finishPromptsWhileCompacting []string
	h.duringQueueCompaction = func() { finishPromptsWhileCompacting = h.finishPrompts() }

	// Act
	requestAndWait(t, h)

	// Assert
	if !slices.Equal(finishPromptsWhileCompacting, []string{"open the pull request"}) {
		t.Fatalf("finish prompts when the compaction ran = %v, want the finish stage already run", finishPromptsWhileCompacting)
	}
}

func TestAFailedFinishStageIsNeverCompacted(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.finishHooks(anyTarget(promptHookOf("open the pull request")))
	h.turnCloses = []wsm.TurnClose{wsm.CloseFailed}

	// Act
	requestAndWait(t, h)

	// Assert
	if len(h.queueCompactions) != 0 || len(h.modelSwitches) != 0 {
		t.Fatalf("compactions = %v, switches = %v; want none for a withdrawn finish", h.queueCompactions, h.modelSwitches)
	}
}

func TestAnEnqueueCompactionThatFailsKeepsTheMergeOutOfLine(t *testing.T) {
	tests := []struct {
		name        string
		arrange     func(h *harness)
		message     string
		wantSwitch  bool
		wantCompact bool
	}{
		{
			name:    "the session is missing",
			arrange: func(h *harness) { delete(h.db.sessions, theWorkspace) },
			message: "the workspace whose merge enters the queue has no session; it is not compacted",
		},
		{
			name:    "the session names no model",
			arrange: func(h *harness) { h.db.sessions[theWorkspace] = wsm.Session{Workspace: theWorkspace} },
			message: "could not record the model the workspace returns to; it is not compacted",
		},
		{
			name:    "the brief is missing",
			arrange: func(h *harness) { delete(h.briefs, BriefEnqueueCompaction) },
			message: "could not compose the compaction's instructions; it is not compacted",
		},
		{
			name:        "the compaction does not complete",
			arrange:     func(h *harness) { h.queueCompactionErr = errors.New("the compaction ended failed") },
			message:     "the compaction of the workspace whose merge enters the queue did not complete; the merge is not put in line",
			wantCompact: true,
		},
		{
			name:        "the model switch is refused",
			arrange:     func(h *harness) { h.modelSwitchErr = errors.New("not_in_catalog") },
			message:     "could not switch the compacted workspace's model; the merge is not put in line",
			wantCompact: true,
			wantSwitch:  true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			tt.arrange(h)

			// Act
			requestAndWait(t, h)

			// Assert
			if state, _ := h.entryState(); state != wsm.MergeRequested {
				t.Fatalf("queue entry state = %v, want still only requested", state)
			}
			if got := len(h.queueCompactions) == 1; got != tt.wantCompact {
				t.Fatalf("compactions = %v, want compacted %v", h.queueCompactions, tt.wantCompact)
			}
			if got := len(h.modelSwitches) == 1; got != tt.wantSwitch {
				t.Fatalf("switches = %v, want switched %v", h.modelSwitches, tt.wantSwitch)
			}
			recs := h.recordsFor("error", opEnqueueCompaction, tt.message)
			if len(recs) != 1 || recs[0].Context["workspace"] != string(theWorkspace) {
				t.Fatalf("records = %v, want one error naming the workspace: %q", h.logs.Records(), tt.message)
			}
			if len(h.recordsFor("error", "daemon.merge.request", "the enqueue compaction failed; the request stays recorded for the next boot")) != 1 {
				t.Fatalf("the request's failure was not recorded: %v", h.logs.Records())
			}
		})
	}
}

// snapshotHandBacks records each handback with the model switches that stood
// when it was handed.
type snapshotHandBacks struct {
	h        *harness
	switches [][]modelSwitch
}

func (s *snapshotHandBacks) HandBack(_ context.Context, _ ids.WorkspaceID, _ wsm.Workspace, _ Ending) error {
	s.h.mu.Lock()
	defer s.h.mu.Unlock()
	s.switches = append(s.switches, slices.Clone(s.h.modelSwitches))
	return nil
}

func TestAMergeThatDidNotLandRestoresTheModelBeforeTheHandback(t *testing.T) {
	// Arrange
	h := newHarness(t)
	if err := h.db.HoldMergeRestoreModel(context.Background(), theWorkspace, "opus"); err != nil {
		t.Fatalf("HoldMergeRestoreModel: %v", err)
	}
	handBacks := &snapshotHandBacks{h: h}
	h.o.deps.HandBacks = handBacks

	// Act
	h.o.handBackInPlace(context.Background(), theWorkspace, Ending{Kind: EndingFailed, Summary: "the gate failed"})

	// Assert
	want := []modelSwitch{{ws: theWorkspace, model: "opus"}}
	if len(handBacks.switches) != 1 || !slices.Equal(handBacks.switches[0], want) {
		t.Fatalf("switches at the handback = %v, want %v already made", handBacks.switches, want)
	}
	if _, held, _ := h.db.TakeMergeRestoreModel(context.Background(), theWorkspace); held {
		t.Fatal("the restore model stands after it was restored")
	}
	if len(h.queueCompactions) != 0 {
		t.Fatalf("compactions = %v, want none on a handback", h.queueCompactions)
	}
}

func TestAHandbackRestoresNoModelItDidNotSwitch(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.o.handBackInPlace(context.Background(), theWorkspace, Ending{Kind: EndingAbandoned})

	// Assert
	if len(h.modelSwitches) != 0 {
		t.Fatalf("switches = %v, want none with no model held", h.modelSwitches)
	}
	if len(h.handBacks.sent()) != 1 {
		t.Fatalf("handbacks = %v, want the merge handed back", h.handBacks.sent())
	}
}

func TestALandedMergeRestoresNoModel(t *testing.T) {
	// Arrange
	h := newHarness(t)
	if err := h.db.HoldMergeRestoreModel(context.Background(), theWorkspace, "opus"); err != nil {
		t.Fatalf("HoldMergeRestoreModel: %v", err)
	}

	// Act
	h.o.handBackInPlace(context.Background(), theWorkspace, Ending{Kind: EndingLanded})

	// Assert
	if len(h.modelSwitches) != 0 {
		t.Fatalf("switches = %v, want none for a landed merge, whose workspace closes", h.modelSwitches)
	}
}

func TestAFailedModelRestoreIsRecordedAndTheHandbackGoesOn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	if err := h.db.HoldMergeRestoreModel(context.Background(), theWorkspace, "opus"); err != nil {
		t.Fatalf("HoldMergeRestoreModel: %v", err)
	}
	h.modelSwitchErr = errors.New("not_in_catalog")

	// Act
	h.o.handBackInPlace(context.Background(), theWorkspace, Ending{Kind: EndingFailed})

	// Assert
	recs := h.recordsFor("error", opEnqueueCompaction, "could not switch the handed-back workspace back to its model")
	if len(recs) != 1 || recs[0].Context["model"] != "opus" {
		t.Fatalf("records = %v, want one error naming the model", h.logs.Records())
	}
	if len(h.handBacks.sent()) != 1 {
		t.Fatalf("handbacks = %v, want the merge handed back anyway", h.handBacks.sent())
	}
}
