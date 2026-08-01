package sessioncontroller

import (
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/ssm"
)

func TestTeardownMergedHibernatesTheLiveSession(t *testing.T) {
	// Arrange.
	m, applier := hibernateGuardRig(t)
	applier.setCurrent("ws", &frontendv1.WorkspaceState{
		State: frontendv1.RenderState_RENDER_STATE_MERGED,
	})

	// Act.
	if err := m.TeardownMerged("ws"); err != nil {
		t.Fatalf("TeardownMerged: %v", err)
	}

	// Assert.
	if got := lastWiring(t, applier, "ws"); got.wiring != ssm.WiringHibernated {
		t.Fatalf("wiring = %s, want hibernated — the merged workspace's session was not stood down", got.wiring)
	}
}

func TestTeardownMergedWithNoLiveSessionIsSatisfied(t *testing.T) {
	// Arrange. A workspace the idle sweeper already reaped has nothing left to
	// stand down, which is the post-merge condition already met.
	var lines []string
	m := &Manager{logf: func(format string, args ...any) { lines = append(lines, format) }}

	// Act.
	err := m.TeardownMerged("ws")

	// Assert.
	if err != nil {
		t.Fatalf("TeardownMerged with no live session = %v, want nil", err)
	}
	if len(lines) != 1 || !strings.Contains(lines[0], "decision=none") {
		t.Fatalf("logged %v, want the absent-session note recorded exactly once", lines)
	}
}

func TestTeardownMergedRefusesAnEmptyWorkspace(t *testing.T) {
	// Arrange.
	m := &Manager{logf: func(string, ...any) {}}

	// Act.
	err := m.TeardownMerged("")

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "empty workspace") {
		t.Fatalf("TeardownMerged(\"\") = %v, want an empty-workspace refusal", err)
	}
}

// A TEARDOWN THE HIBERNATION REFUSES IS A LOUD FAILURE, never a quiet one. The
// merge has landed either way, so the honest outcome is a surfaced error naming
// the session still running rather than a silent success over a live shim.
func TestTeardownMergedSurfacesAHibernationRefusal(t *testing.T) {
	// Arrange — the workspace reads as still working, which the shared teardown
	// refuses.
	m, applier := hibernateGuardRig(t)
	applier.setCurrent("ws", &frontendv1.WorkspaceState{
		State:      frontendv1.RenderState_RENDER_STATE_THINKING,
		TurnActive: true,
	})

	// Act.
	err := m.TeardownMerged("ws")

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "merged teardown for workspace") {
		t.Fatalf("TeardownMerged over a refused hibernation = %v, want the failure surfaced", err)
	}
}

func TestTeardownMergedLogsTheHibernationRefusal(t *testing.T) {
	// Arrange.
	m, applier := hibernateGuardRig(t)
	applier.setCurrent("ws", &frontendv1.WorkspaceState{
		State:      frontendv1.RenderState_RENDER_STATE_THINKING,
		TurnActive: true,
	})
	var lines []string
	m.logf = func(format string, args ...any) { lines = append(lines, format) }

	// Act.
	if err := m.TeardownMerged("ws"); err == nil {
		t.Fatal("TeardownMerged succeeded over a refused hibernation")
	}

	// Assert.
	found := false
	for _, l := range lines {
		if strings.Contains(l, "merged teardown FAILED") {
			found = true
		}
	}
	if !found {
		t.Fatalf("no canonical log line for the failed merged teardown; lines=%v", lines)
	}
}
