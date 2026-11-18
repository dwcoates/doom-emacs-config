package merge

import (
	"context"
	"errors"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/wsm"
)

// lastFooterFacts answers the last merge facts the footer was told for WS,
// and whether it was told any.
func lastFooterFacts(h *harness, ws ids.WorkspaceID) (footer.MergeFacts, bool) {
	h.footer.mu.Lock()
	defer h.footer.mu.Unlock()
	for i := len(h.footer.told) - 1; i >= 0; i-- {
		if h.footer.told[i] == ws {
			return h.footer.facts[i], true
		}
	}
	return footer.MergeFacts{}, false
}

func TestAMediatorsFinishIsDrawnAtItsStep(t *testing.T) {
	tests := []struct {
		name      string
		path      []wsm.RemediationStep
		wantState string
		wantStep  footer.MergeStep
	}{
		{name: "a gating finish is testing fix", wantState: StateMerging, wantStep: footer.StepTestingFix},
		{name: "a landing finish is testing fix", path: []wsm.RemediationStep{wsm.RemediationLanding}, wantState: StateMerging, wantStep: footer.StepTestingFix},
		{name: "a deploying finish is deploying fix", path: []wsm.RemediationStep{wsm.RemediationLanding, wsm.RemediationDeploying}, wantState: StateMerging, wantStep: footer.StepDeployingFix},
		{name: "a finish sent back to fixing is drawn as its session stands", path: []wsm.RemediationStep{wsm.RemediationFixing}, wantState: "none"},
		{name: "a deployed finish is drawn as its session stands", path: []wsm.RemediationStep{wsm.RemediationLanding, wsm.RemediationDeploying, wsm.RemediationDeployed}, wantState: "none"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the finish is drawn at its gate first.
			h := finishing(t)
			ctx := context.Background()
			if _, err := h.db.BeginRemediationFinish(ctx, wsm.RemediationFinish{Workspace: "ws-fixer", Repo: mediationKey(h), StartedAt: deployStarted}); err != nil {
				t.Fatal(err)
			}
			h.o.surfaceFinish(ctx, mediationKey(h))
			from := wsm.RemediationGating
			for _, to := range tt.path {
				if _, err := h.db.AdvanceRemediationFinish(ctx, "ws-fixer", from, to, deployStarted); err != nil {
					t.Fatal(err)
				}
				from = to
			}

			// Act
			h.o.surfaceFinish(ctx, mediationKey(h))

			// Assert
			got, told := lastFooterFacts(h, "ws-fixer")
			if !told || got.State != tt.wantState || got.Step != tt.wantStep {
				t.Fatalf("footer told (%+v, %v), want state %q step %q", got, told, tt.wantState, tt.wantStep)
			}
		})
	}
}

func TestAFinishOfAnotherQueueLeavesTheDrawnFinish(t *testing.T) {
	// Arrange: the harness queue's finish is drawn testing its fix.
	h := finishing(t)
	ctx := context.Background()
	if _, err := h.db.BeginRemediationFinish(ctx, wsm.RemediationFinish{Workspace: "ws-fixer", Repo: mediationKey(h), StartedAt: deployStarted}); err != nil {
		t.Fatal(err)
	}
	h.o.surfaceFinish(ctx, mediationKey(h))

	// Act: another queue is restated.
	h.o.surfaceFinish(ctx, wsm.RepoKey(t.TempDir()))

	// Assert
	if got := h.o.surfaceFacts("ws-fixer"); got.State != StateMerging || got.Step != footer.StepTestingFix {
		t.Fatalf("surfaceFacts = %+v, want the finish still drawn testing its fix", got)
	}
}

func TestFinishesThatCannotBeReadKeepWhatTheSurfacesDrew(t *testing.T) {
	// Arrange: the finish is drawn testing its fix, then the store fails.
	h := finishing(t)
	ctx := context.Background()
	if _, err := h.db.BeginRemediationFinish(ctx, wsm.RemediationFinish{Workspace: "ws-fixer", Repo: mediationKey(h), StartedAt: deployStarted}); err != nil {
		t.Fatal(err)
	}
	h.o.surfaceFinish(ctx, mediationKey(h))
	h.db.mu.Lock()
	h.db.remediationFinishesErr = errors.New("database is locked")
	h.db.mu.Unlock()

	// Act
	h.o.surfaceFinish(ctx, mediationKey(h))

	// Assert
	if got := h.o.surfaceFacts("ws-fixer"); got.Step != footer.StepTestingFix {
		t.Fatalf("surfaceFacts = %+v, want the drawn step kept", got)
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation,
		Message: "could not read the remediation finishes to draw them; the surfaces keep what they drew"}); !ok {
		t.Fatalf("records = %v, want the failed read at ERROR", h.logs.Records())
	}
}
