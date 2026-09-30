package rollout

import (
	"context"
	"testing"

	"claude-repld/internal/deployprogress"
	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// successorFaults answers the standing daemon-scoped successor faults.
func successorFaults(t *testing.T, h *harness) []wsm.Fault {
	t.Helper()
	open, err := h.db.OpenFaults(context.Background(), wsm.FaultScope{Kind: health.KindSuccessorSpawnFailed})
	if err != nil {
		t.Fatalf("OpenFaults: %v", err)
	}
	return open
}

func TestASuccessorThatTookOverFromADeploySaysUpdated(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)
	h.c.recordDeployTakeover(true)

	// Act
	takeOver(h)

	// Assert
	stated := h.progress.statements()
	if len(stated) != 1 || stated[0] == nil || stated[0].Phase != deployprogress.Updated {
		t.Fatalf("statements = %+v, want one updated", stated)
	}
}

func TestTheUpdatedLineNotesAStaleShimRegisteredBehindItsWork(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := staleReported(t, h)
	h.freeness.SetFree(ws, false)
	h.c.recordDeployTakeover(true)

	// Act
	takeOver(h)

	// Assert
	stated := h.progress.statements()
	if len(stated) != 1 {
		t.Fatalf("statements = %+v, want one updated", stated)
	}
	notes := stated[0].Notes[ws]
	if len(notes) != 1 || notes[0] != deployprogress.ShimWhenIdle {
		t.Fatalf("notes = %v, want shim_when_idle on the busy stale workspace", stated[0].Notes)
	}
}

func TestATakeoverThatWasNoDeploysSaysNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)

	// Act
	takeOver(h)

	// Assert
	if stated := h.progress.statements(); len(stated) != 0 {
		t.Fatalf("statements = %+v, want none: no deploy wrote the manifest", stated)
	}
}

func TestTheDeployStoryIsFinishedOnce(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)
	h.c.recordDeployTakeover(true)
	takeOver(h)

	// Act
	takeOver(h)

	// Assert
	if stated := h.progress.statements(); len(stated) != 1 {
		t.Fatalf("statements = %d, want exactly one updated", len(stated))
	}
}

func TestTheManifestAHandoverWritesNamesTheDeploy(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)

	// Act
	if _, err := h.c.HandOver(context.Background(), false); err != nil {
		t.Fatalf("HandOver: %v", err)
	}

	// Assert
	m, found, err := ReadManifest(h.c.deps.IntentManifest)
	if err != nil || !found || !m.Deploy {
		t.Fatalf("manifest = (%+v, found %v, %v), want one naming the deploy", m, found, err)
	}
}

func TestASuccessorThatWillNotStartIsAFault(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
	}{
		{"its spawn fails", func(h *harness) { h.spawner.err = errFake }},
		{"it never proves it is serving", func(h *harness) { h.spawner.readyErr = errFake }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.workspace(t)
			tc.arrange(h)

			// Act
			_, err := h.c.HandOver(context.Background(), false)

			// Assert
			if err == nil {
				t.Fatalf("HandOver succeeded through the failure")
			}
			open := successorFaults(t, h)
			if len(open) != 1 || open[0].Workspace != nil {
				t.Fatalf("faults = %+v, want one daemon-scoped successor_spawn_failed", open)
			}
		})
	}
}

func TestASuccessorThatServesClosesTheStandingFault(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)
	h.spawner.readyErr = errFake
	if _, err := h.c.HandOver(context.Background(), false); err == nil {
		t.Fatalf("the first HandOver succeeded")
	}
	h.spawner.readyErr = nil

	// Act
	if _, err := h.c.HandOver(context.Background(), false); err != nil {
		t.Fatalf("HandOver: %v", err)
	}

	// Assert
	if open := successorFaults(t, h); len(open) != 0 {
		t.Fatalf("faults = %+v, want the successor fault closed", open)
	}
}

// A LATER FAILED HANDOVER SUPERSEDES THE STANDING FAULT: failures never pile
// up, and the one that stands is the latest.
func TestALaterFailedHandoverSupersedesTheStandingFault(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)
	h.spawner.readyErr = errFake
	if _, err := h.c.HandOver(context.Background(), false); err == nil {
		t.Fatalf("the first HandOver succeeded")
	}
	first := successorFaults(t, h)
	if len(first) != 1 {
		t.Fatalf("faults after the first failure = %+v, want one", first)
	}

	// Act
	if _, err := h.c.HandOver(context.Background(), false); err == nil {
		t.Fatalf("the second HandOver succeeded")
	}

	// Assert
	open := successorFaults(t, h)
	if len(open) != 1 || open[0].ID == first[0].ID {
		t.Fatalf("faults = %+v, want only the second failure's fault standing", open)
	}
}

// A SERVING SUCCESSOR IS THE RECOVERY EDGE of every daemon-scoped fault whose
// lifetime ends there (health/lifetime.go), and of no workspace's.
func TestAServingSuccessorClosesTheDaemonFaultsWhoseLifetimeEndsThere(t *testing.T) {
	tests := []struct {
		name      string
		kind      string
		workspace bool
		closes    bool
	}{
		{"an unclaimed handover", health.KindAdoptionWindowExpired, false, true},
		{"a workspace the incumbent took back waits for its own attach", health.KindAdoptionWindowExpired, true, false},
		{"a missing prompts directory waits for a served brief", health.KindPromptsDirMissing, false, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ws, _ := h.workspace(t)
			fault := wsm.Fault{Kind: tt.kind}
			if tt.workspace {
				fault.Workspace = &ws
			}
			id, err := h.db.OpenFault(context.Background(), fault)
			if err != nil {
				t.Fatalf("OpenFault: %v", err)
			}

			// Act
			h.c.closeSuccessorFaults(context.Background(), nil)
			open, err := h.db.OpenFaults(context.Background(), wsm.FaultScope{Kind: tt.kind})

			// Assert
			if err != nil {
				t.Fatalf("OpenFaults: %v", err)
			}
			stands := false
			for _, f := range open {
				stands = stands || f.ID == id
			}
			if closed := !stands; closed != tt.closes {
				t.Fatalf("%s closed = %v, want %v", tt.kind, closed, tt.closes)
			}
		})
	}
}

func TestAHandoverThatCannotFinishTakesTheLineDown(t *testing.T) {
	// Arrange: the quiesce fails, so the transfer does.
	h := newHarness(t, func(d *Deps) {
		d.Quiesce = func(context.Context, ids.WorkspaceID) (ids.LeaseID, error) { return "", errFake }
	})
	h.workspace(t)

	// Act
	if _, err := h.c.HandOver(context.Background(), false); err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	h.registry.wait()
	h.c.handoverDone.Wait()

	// Assert
	stated := h.progress.statements()
	if len(stated) != 1 || stated[0] != nil {
		t.Fatalf("statements = %+v, want the line taken down", stated)
	}
}

func TestARestartThatCannotFinishTakesTheLineDown(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)
	h.spawner.replacementErr = errFake

	// Act
	runRestart(t, h)

	// Assert
	stated := h.progress.statements()
	if len(stated) != 1 || stated[0] != nil {
		t.Fatalf("statements = %+v, want the line taken down", stated)
	}
}

func TestNewRefusesAMissingProgressSink(t *testing.T) {
	// Arrange
	h := newHarness(t)
	deps := h.c.deps
	deps.Progress = nil

	// Act
	_, err := New(deps)

	// Assert
	if err == nil {
		t.Fatalf("New accepted a controller with no progress sink")
	}
}
