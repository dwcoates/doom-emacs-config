package ssm

import (
	"errors"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// hibernate parks a workspace exactly where a successor daemon's boot finds a
// session that outlived its predecessor: `hibernated`, still naming the retired
// generation, with the anonymous restart cause every survivor's row carries.
func hibernate(t *testing.T, m *Manager, workspace, sessionID, generationID string) {
	t.Helper()
	connectOperational(t, m, workspace, sessionID, generationID)
	if err := m.ApplySessionConnectivity(
		workspace, sessionID, generationID, SessionConnectivityHibernated, "daemon_restart",
	); err != nil {
		t.Fatalf("ApplySessionConnectivity(hibernated): %v", err)
	}
}

func snapshotFor(t *testing.T, m *Manager, workspace string) *frontendv1.WorkspaceState {
	t.Helper()
	states, err := m.Snapshot()
	if err != nil {
		t.Fatalf("Snapshot: %v", err)
	}
	for _, state := range states {
		if state.GetWorkspace() == workspace {
			return state
		}
	}
	t.Fatalf("Snapshot has no state for %q", workspace)
	return nil
}

// TestBootSweepVerdictReachesThePushedState is the per-verdict routing case:
// each of the sweep's four conclusions lands as its own fault window and the
// pushed frame names THAT conclusion instead of the anonymous `daemon_restart`
// cause the hibernation wrote.
func TestBootSweepVerdictReachesThePushedState(t *testing.T) {
	tests := []struct {
		name    string
		verdict string
	}{
		{name: "the shim is genuinely gone", verdict: "boot_sweep_no_live_shim"},
		{name: "a live holder never dialled in", verdict: "boot_sweep_lock_held_without_connection"},
		{name: "the connection probe failed twice", verdict: "boot_sweep_probe_failed"},
		{name: "the lock probe could not tell", verdict: "boot_sweep_lock_probe_failed"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			m, _, _ := openUnwiredTest(t, fakeResolver{"session-1": "ws"})
			hibernate(t, m, "ws", "session-1", "generation-1")

			// Act
			if err := m.ApplyBootSweepVerdict("ws", "session-1", tc.verdict); err != nil {
				t.Fatalf("ApplyBootSweepVerdict(%s): %v", tc.verdict, err)
			}

			// Assert
			composite := mustComposite(t, m, "ws")
			if composite.Connectivity != SessionConnectivityHibernated {
				t.Fatalf("connectivity = %q, want hibernated: the sweep made no bring-up claim", composite.Connectivity)
			}
			if len(composite.ActiveFaults) != 1 {
				t.Fatalf("active faults = %#v, want exactly the verdict", composite.ActiveFaults)
			}
			fault := composite.ActiveFaults[0]
			if fault.Component != BootSweepFaultComponent || fault.FaultType != tc.verdict ||
				fault.Impact != FaultImpactConnectivity || fault.CauseKind != tc.verdict {
				t.Fatalf("fault = %#v, want component %q fault_type %q impact connectivity cause %q",
					fault, BootSweepFaultComponent, tc.verdict, tc.verdict)
			}
			state := snapshotFor(t, m, "ws")
			if state.GetConnectivity() != frontendv1.SessionConnectivity_SESSION_CONNECTIVITY_HIBERNATED {
				t.Fatalf("pushed connectivity = %v, want hibernated", state.GetConnectivity())
			}
			if state.GetCauseKind() != tc.verdict {
				t.Fatalf("pushed cause_kind = %q, want the verdict %q rather than the anonymous restart cause",
					state.GetCauseKind(), tc.verdict)
			}
		})
	}
}

// TestBootSweepVerdictKeepsTheGenerationOffTheFrame guards the half of the
// carve-out a reader would otherwise be lied to about: hibernated means NO
// generation is current, so the retired one the fault is filed under must not
// reach the frame.
func TestBootSweepVerdictKeepsTheGenerationOffTheFrame(t *testing.T) {
	// Arrange
	m, _, _ := openUnwiredTest(t, fakeResolver{"session-1": "ws"})
	hibernate(t, m, "ws", "session-1", "generation-1")

	// Act
	if err := m.ApplyBootSweepVerdict("ws", "session-1", "boot_sweep_no_live_shim"); err != nil {
		t.Fatalf("ApplyBootSweepVerdict: %v", err)
	}

	// Assert
	if got := snapshotFor(t, m, "ws").GetControllerGenerationId(); got != "" {
		t.Fatalf("pushed controller_generation_id = %q, want empty: no generation is current", got)
	}
}

// TestBootSweepVerdictIsIdempotentForAStandingVerdict is what makes "exactly
// once per boot" survive a daemon restarted twice over an unchanged situation.
func TestBootSweepVerdictIsIdempotentForAStandingVerdict(t *testing.T) {
	// Arrange
	m, logs, _ := openUnwiredTest(t, fakeResolver{"session-1": "ws"})
	hibernate(t, m, "ws", "session-1", "generation-1")
	if err := m.ApplyBootSweepVerdict("ws", "session-1", "boot_sweep_no_live_shim"); err != nil {
		t.Fatalf("first ApplyBootSweepVerdict: %v", err)
	}

	// Act
	if err := m.ApplyBootSweepVerdict("ws", "session-1", "boot_sweep_no_live_shim"); err != nil {
		t.Fatalf("second ApplyBootSweepVerdict: %v", err)
	}

	// Assert
	if faults := mustComposite(t, m, "ws").ActiveFaults; len(faults) != 1 {
		t.Fatalf("active faults = %#v, want one window for one standing fact", faults)
	}
	if !logs.contains("branch=already-standing") {
		t.Fatalf("the repeat attach was not accounted for: %v", logs.lines)
	}
}

// TestBootSweepVerdictRefusals covers the boundaries of the carve-out. Each
// case is a door the boot-scoped entry point must keep shut.
func TestBootSweepVerdictRefusals(t *testing.T) {
	tests := []struct {
		name string
		// arrange parks the workspace in the state the refusal is about.
		arrange func(t *testing.T, m *Manager)
		// the verdict's arguments.
		workspace, sessionID, verdict string
		// want, when non-nil, is the sentinel the refusal must wrap.
		want error
	}{
		{
			name:      "an empty workspace",
			arrange:   func(*testing.T, *Manager) {},
			workspace: "", sessionID: "session-1", verdict: "boot_sweep_no_live_shim",
		},
		{
			name:      "an empty session",
			arrange:   func(*testing.T, *Manager) {},
			workspace: "ws", sessionID: "", verdict: "boot_sweep_no_live_shim",
		},
		{
			name:      "an empty verdict",
			arrange:   func(*testing.T, *Manager) {},
			workspace: "ws", sessionID: "session-1", verdict: "",
		},
		{
			name:      "a workspace the SSM has no lifecycle row for",
			arrange:   func(*testing.T, *Manager) {},
			workspace: "ws", sessionID: "session-1", verdict: "boot_sweep_no_live_shim",
			want: ErrBootSweepVerdictNoLifecycle,
		},
		{
			name: "a workspace whose generation is live",
			arrange: func(t *testing.T, m *Manager) {
				connectOperational(t, m, "ws", "session-1", "generation-1")
			},
			workspace: "ws", sessionID: "session-1", verdict: "boot_sweep_no_live_shim",
			want: ErrBootSweepVerdictLiveGeneration,
		},
		{
			name: "a workspace still coming up",
			arrange: func(t *testing.T, m *Manager) {
				if err := m.ApplySessionConnectivity("ws", "session-1", "generation-1", SessionConnectivityConnecting, "bring_up"); err != nil {
					t.Fatalf("ApplySessionConnectivity(connecting): %v", err)
				}
			},
			workspace: "ws", sessionID: "session-1", verdict: "boot_sweep_no_live_shim",
			want: ErrBootSweepVerdictLiveGeneration,
		},
		{
			name: "a verdict about a session the workspace's row does not name",
			arrange: func(t *testing.T, m *Manager) {
				hibernate(t, m, "ws", "session-1", "generation-1")
			},
			workspace: "ws", sessionID: "session-2", verdict: "boot_sweep_no_live_shim",
			want: ErrStaleControllerGeneration,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			m, _, _ := openUnwiredTest(t, fakeResolver{"session-1": "ws"})
			tc.arrange(t, m)

			// Act
			err := m.ApplyBootSweepVerdict(tc.workspace, tc.sessionID, tc.verdict)

			// Assert
			if err == nil {
				t.Fatalf("ApplyBootSweepVerdict = nil, want a refusal")
			}
			if tc.want != nil && !errors.Is(err, tc.want) {
				t.Fatalf("ApplyBootSweepVerdict = %v, want one wrapping %v", err, tc.want)
			}
		})
	}
}

// TestOrdinaryFaultStillRefusedWithoutAGeneration is the boundary of the
// carve-out stated from the other side: NOTHING but a boot-sweep verdict
// reaches a workspace no generation owns. ApplyRuntimeFault is untouched.
func TestOrdinaryFaultStillRefusedWithoutAGeneration(t *testing.T) {
	// Arrange
	m, _, _ := openUnwiredTest(t, fakeResolver{"session-1": "ws"})
	hibernate(t, m, "ws", "session-1", "generation-1")

	// Act
	err := m.ApplyRuntimeFault(
		"ws", "session-1", "generation-1", "shim-store-client", "disconnected",
		FaultImpactConnectivity, true, "store_gone",
	)

	// Assert
	if !errors.Is(err, ErrStaleControllerGeneration) {
		t.Fatalf("ApplyRuntimeFault on a hibernated workspace = %v, want a stale-generation refusal", err)
	}
}

// TestHibernationDoesNotResurrectADeadGenerationsFault is why the hibernated
// read is narrowed to the boot-sweep component: a generation that died with an
// ordinary window open stays retired, verdict or no verdict.
func TestHibernationDoesNotResurrectADeadGenerationsFault(t *testing.T) {
	// Arrange
	m, _, _ := openUnwiredTest(t, fakeResolver{"session-1": "ws"})
	connectOperational(t, m, "ws", "session-1", "generation-1")
	if err := m.ApplyRuntimeFault(
		"ws", "session-1", "generation-1", "shim-store-client", "disconnected",
		FaultImpactConnectivity, true, "store_gone",
	); err != nil {
		t.Fatalf("ApplyRuntimeFault(open): %v", err)
	}
	if err := m.ApplySessionConnectivity(
		"ws", "session-1", "generation-1", SessionConnectivityHibernated, "daemon_restart",
	); err != nil {
		t.Fatalf("ApplySessionConnectivity(hibernated): %v", err)
	}

	// Act
	if err := m.ApplyBootSweepVerdict("ws", "session-1", "boot_sweep_no_live_shim"); err != nil {
		t.Fatalf("ApplyBootSweepVerdict: %v", err)
	}

	// Assert
	faults := mustComposite(t, m, "ws").ActiveFaults
	if len(faults) != 1 || faults[0].Component != BootSweepFaultComponent {
		t.Fatalf("active faults = %#v, want only the boot-sweep verdict", faults)
	}
}

// TestBootSweepVerdictSelfRetiresOnTheNextBringUp is the lifetime the carve-out
// promised: nothing has to close the verdict, because the generation it is
// filed under stops being the one the composite reads.
func TestBootSweepVerdictSelfRetiresOnTheNextBringUp(t *testing.T) {
	// Arrange
	m, _, _ := openUnwiredTest(t, fakeResolver{"session-1": "ws"})
	hibernate(t, m, "ws", "session-1", "generation-1")
	if err := m.ApplyBootSweepVerdict("ws", "session-1", "boot_sweep_no_live_shim"); err != nil {
		t.Fatalf("ApplyBootSweepVerdict: %v", err)
	}

	// Act
	if err := m.ApplySessionConnectivity(
		"ws", "session-1", "generation-2", SessionConnectivityConnecting, "bring_up",
	); err != nil {
		t.Fatalf("ApplySessionConnectivity(connecting): %v", err)
	}

	// Assert
	if faults := mustComposite(t, m, "ws").ActiveFaults; len(faults) != 0 {
		t.Fatalf("active faults after the next bring-up = %#v, want none: the verdict describes a situation that has ended", faults)
	}
}

// TestHibernationWithoutAVerdictKeepsItsOwnCause guards the fallback: the
// boot-sweep branch must not have stolen the cause from every OTHER hibernated
// workspace.
func TestHibernationWithoutAVerdictKeepsItsOwnCause(t *testing.T) {
	// Arrange
	m, _, _ := openUnwiredTest(t, fakeResolver{"session-1": "ws"})

	// Act
	hibernate(t, m, "ws", "session-1", "generation-1")

	// Assert
	if got := snapshotFor(t, m, "ws").GetCauseKind(); got != "daemon_restart" {
		t.Fatalf("pushed cause_kind = %q, want the hibernation's own cause", got)
	}
}
