package ssm

import (
	"database/sql"
	"errors"
	"path/filepath"
	"strings"
	"testing"
)

func connectOperational(t *testing.T, m *Manager, workspace, sessionID, generationID string) {
	t.Helper()
	if err := m.ApplySessionConnectivity(
		workspace,
		sessionID,
		generationID,
		SessionConnectivityConnecting,
		"bring_up",
	); err != nil {
		t.Fatalf("ApplySessionConnectivity(connecting): %v", err)
	}
	if err := m.ApplySessionConnectivity(
		workspace,
		sessionID,
		generationID,
		SessionConnectivityOperational,
		"shim_ready",
	); err != nil {
		t.Fatalf("ApplySessionConnectivity(operational): %v", err)
	}
}

func mustComposite(t *testing.T, m *Manager, workspace string) CompositeState {
	t.Helper()
	got, found, err := m.Composite(workspace)
	if err != nil {
		t.Fatalf("Composite(%q): %v", workspace, err)
	}
	if !found {
		t.Fatalf("Composite(%q): not found", workspace)
	}
	return got
}

func TestCompositeHealthyReadyAndThinking(t *testing.T) {
	m, logs, _ := openUnwiredTest(t, fakeResolver{"session-1": "ws"})
	connectOperational(t, m, "ws", "session-1", "generation-1")

	if err := m.Apply(evSessionStarted("session-1", 1)); err != nil {
		t.Fatalf("Apply(SessionStarted): %v", err)
	}
	ready := mustComposite(t, m, "ws")
	if ready.Connectivity != SessionConnectivityOperational || ready.Status != SessionStatusReady {
		t.Fatalf("ready composite = connectivity %q status %q, want operational + ready",
			ready.Connectivity, ready.Status)
	}
	if ready.AgentReplSessionID != "session-1" || ready.ControllerGenerationID != "generation-1" {
		t.Fatalf("ready identity = session %q generation %q, want session-1 + generation-1",
			ready.AgentReplSessionID, ready.ControllerGenerationID)
	}

	if err := m.Apply(evTurnStarted("session-1", 2)); err != nil {
		t.Fatalf("Apply(TurnStarted): %v", err)
	}
	thinking := mustComposite(t, m, "ws")
	if thinking.Connectivity != SessionConnectivityOperational || thinking.Status != SessionStatusThinking {
		t.Fatalf("thinking composite = connectivity %q status %q, want operational + thinking",
			thinking.Connectivity, thinking.Status)
	}
	for _, identity := range []string{
		`ws="ws"`,
		`session="session-1"`,
		`generation="generation-1"`,
		`lifecycle_top="operational"`,
		`connectivity="operational"`,
		`status="thinking"`,
	} {
		if !logs.contains(identity) {
			t.Fatalf("composite log missing %s: %v", identity, logs.lines)
		}
	}
}

func TestConnectivityFaultDegradesWithoutDestroyingSessionStatus(t *testing.T) {
	m, _, _ := openUnwiredTest(t, fakeResolver{"session-1": "ws"})
	connectOperational(t, m, "ws", "session-1", "generation-1")
	if err := m.Apply(evTurnStarted("session-1", 1)); err != nil {
		t.Fatalf("Apply(TurnStarted): %v", err)
	}

	if err := m.ApplyRuntimeFault(
		"ws", "session-1", "generation-1",
		"shim-store", "subscription-idle",
		FaultImpactConnectivity, true, "store_subscription_idle",
	); err != nil {
		t.Fatalf("open store fault: %v", err)
	}
	degraded := mustComposite(t, m, "ws")
	if degraded.Connectivity != SessionConnectivityDegraded {
		t.Fatalf("connectivity = %q, want degraded", degraded.Connectivity)
	}
	if degraded.Status != SessionStatusThinking {
		t.Fatalf("status = %q, want thinking preserved", degraded.Status)
	}
	if len(degraded.ActiveFaults) != 1 {
		t.Fatalf("active faults = %d, want 1", len(degraded.ActiveFaults))
	}

	if err := m.ApplyRuntimeFault(
		"ws", "session-1", "generation-1",
		"shim-store", "subscription-idle",
		FaultImpactConnectivity, false, "store_subscription_recovered",
	); err != nil {
		t.Fatalf("close store fault: %v", err)
	}
	recovered := mustComposite(t, m, "ws")
	if recovered.Connectivity != SessionConnectivityOperational {
		t.Fatalf("connectivity after close = %q, want operational", recovered.Connectivity)
	}
	if recovered.Status != SessionStatusThinking {
		t.Fatalf("status after close = %q, want thinking preserved", recovered.Status)
	}
	if len(recovered.ActiveFaults) != 0 {
		t.Fatalf("active faults after close = %d, want 0", len(recovered.ActiveFaults))
	}
}

func TestOverlappingFaultWindowsCloseIndependently(t *testing.T) {
	m, _, _ := openUnwiredTest(t, fakeResolver{})
	connectOperational(t, m, "ws", "session-1", "generation-1")
	faults := []struct {
		component string
		faultType string
	}{
		{"shim-store", "subscription-idle"},
		{"shim-client", "heartbeat-missed"},
	}
	for _, fault := range faults {
		if err := m.ApplyRuntimeFault(
			"ws", "session-1", "generation-1",
			fault.component, fault.faultType,
			FaultImpactConnectivity, true, "fault_open",
		); err != nil {
			t.Fatalf("open %s/%s: %v", fault.component, fault.faultType, err)
		}
	}
	if got := mustComposite(t, m, "ws"); got.Connectivity != SessionConnectivityDegraded || len(got.ActiveFaults) != 2 {
		t.Fatalf("two-fault composite = connectivity %q faults %d, want degraded + 2",
			got.Connectivity, len(got.ActiveFaults))
	}

	if err := m.ApplyRuntimeFault(
		"ws", "session-1", "generation-1",
		faults[0].component, faults[0].faultType,
		FaultImpactConnectivity, false, "one_recovered",
	); err != nil {
		t.Fatalf("close first fault: %v", err)
	}
	oneOpen := mustComposite(t, m, "ws")
	if oneOpen.Connectivity != SessionConnectivityDegraded || len(oneOpen.ActiveFaults) != 1 {
		t.Fatalf("one-fault composite = connectivity %q faults %d, want degraded + 1",
			oneOpen.Connectivity, len(oneOpen.ActiveFaults))
	}
	if oneOpen.ActiveFaults[0].Component != faults[1].component {
		t.Fatalf("remaining component = %q, want %q", oneOpen.ActiveFaults[0].Component, faults[1].component)
	}
}

func TestNonConnectivityFaultsRemainDiagnosticOnly(t *testing.T) {
	tests := []struct {
		name   string
		impact FaultImpact
	}{
		{"feature", FaultImpactFeature},
		{"command", FaultImpactCommand},
		{"turn terminal", FaultImpactTurnTerminal},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			m, _, _ := openUnwiredTest(t, fakeResolver{})
			connectOperational(t, m, "ws", "session-1", "generation-1")
			if err := m.ApplyRuntimeFault(
				"ws", "session-1", "generation-1",
				"component", "typed-failure",
				test.impact, true, "fault_open",
			); err != nil {
				t.Fatalf("open %s fault: %v", test.impact, err)
			}
			got := mustComposite(t, m, "ws")
			if got.Connectivity != SessionConnectivityOperational {
				t.Fatalf("connectivity = %q, want operational for impact %q", got.Connectivity, test.impact)
			}
			if len(got.ActiveFaults) != 1 || got.ActiveFaults[0].Impact != test.impact {
				t.Fatalf("active faults = %#v, want one %q fault", got.ActiveFaults, test.impact)
			}
		})
	}
}

func TestControllerReplacementScopesOutRetiredFaultsAndEdges(t *testing.T) {
	m, logs, _ := openUnwiredTest(t, fakeResolver{})
	connectOperational(t, m, "ws", "session-old", "generation-old")
	if err := m.ApplyRuntimeFault(
		"ws", "session-old", "generation-old",
		"shim-store", "subscription-idle",
		FaultImpactConnectivity, true, "old_fault",
	); err != nil {
		t.Fatalf("open old fault: %v", err)
	}

	connectOperational(t, m, "ws", "session-new", "generation-new")
	current := mustComposite(t, m, "ws")
	if current.Connectivity != SessionConnectivityOperational ||
		current.AgentReplSessionID != "session-new" ||
		current.ControllerGenerationID != "generation-new" ||
		len(current.ActiveFaults) != 0 {
		t.Fatalf("replacement composite = %#v, want clean new generation", current)
	}

	before := faultRowCount(t, m.db, "ws")
	err := m.ApplyRuntimeFault(
		"ws", "session-old", "generation-old",
		"shim-store", "subscription-idle",
		FaultImpactConnectivity, false, "late_old_recovery",
	)
	if !errors.Is(err, ErrStaleControllerGeneration) {
		t.Fatalf("late old recovery error = %v, want ErrStaleControllerGeneration", err)
	}
	if after := faultRowCount(t, m.db, "ws"); after != before {
		t.Fatalf("fault rows after rejected stale close = %d, want unchanged %d", after, before)
	}
	if !logs.contains(`branch=stale-controller`) ||
		!logs.contains(`current_generation="generation-new"`) ||
		!logs.contains(`generation="generation-old"`) {
		t.Fatalf("stale rejection log missing full identity: %v", logs.lines)
	}
}

func TestSameGenerationCannotMoveToAnotherSession(t *testing.T) {
	m, _, _ := openUnwiredTest(t, fakeResolver{})
	if err := m.ApplySessionConnectivity(
		"ws", "session-old", "generation-1",
		SessionConnectivityConnecting, "bring_up",
	); err != nil {
		t.Fatalf("first connecting: %v", err)
	}
	err := m.ApplySessionConnectivity(
		"ws", "session-new", "generation-1",
		SessionConnectivityConnecting, "replacement",
	)
	if !errors.Is(err, ErrStaleControllerGeneration) {
		t.Fatalf("same-generation replacement error = %v, want ErrStaleControllerGeneration", err)
	}
}

func TestReplacementDoesNotInheritRetiredSessionStatus(t *testing.T) {
	m, _, _ := openUnwiredTest(t, fakeResolver{"session-old": "ws", "session-new": "ws"})
	connectOperational(t, m, "ws", "session-old", "generation-old")
	if err := m.Apply(evTurnStarted("session-old", 1)); err != nil {
		t.Fatalf("Apply old TurnStarted: %v", err)
	}
	connectOperational(t, m, "ws", "session-new", "generation-new")

	replaced := mustComposite(t, m, "ws")
	if replaced.Status != "" {
		t.Fatalf("replacement status = %q, want unspecified until the current session reports status", replaced.Status)
	}
	if err := m.Apply(evSessionStarted("session-new", 1)); err != nil {
		t.Fatalf("Apply new SessionStarted: %v", err)
	}
	if current := mustComposite(t, m, "ws"); current.Status != SessionStatusReady {
		t.Fatalf("current session status = %q, want ready", current.Status)
	}
}

func TestUnavailableAndHibernatedConnectivity(t *testing.T) {
	t.Run("bring-up failure", func(t *testing.T) {
		m, _, _ := openUnwiredTest(t, fakeResolver{})
		if err := m.ApplySessionConnectivity(
			"ws", "session-1", "generation-1",
			SessionConnectivityConnecting, "bring_up",
		); err != nil {
			t.Fatalf("connecting: %v", err)
		}
		if err := m.ApplySessionConnectivity(
			"ws", "session-1", "generation-1",
			SessionConnectivityUnavailable, "shim_ready_failed",
		); err != nil {
			t.Fatalf("unavailable: %v", err)
		}
		got := mustComposite(t, m, "ws")
		if got.Connectivity != SessionConnectivityUnavailable {
			t.Fatalf("connectivity = %q, want unavailable", got.Connectivity)
		}
	})

	t.Run("intentional hibernation", func(t *testing.T) {
		m, _, _ := openUnwiredTest(t, fakeResolver{})
		connectOperational(t, m, "ws", "session-1", "generation-1")
		if err := m.ApplySessionConnectivity(
			"ws", "session-1", "generation-1",
			SessionConnectivityHibernated, "idle_sweep",
		); err != nil {
			t.Fatalf("hibernated: %v", err)
		}
		got := mustComposite(t, m, "ws")
		if got.Connectivity != SessionConnectivityHibernated {
			t.Fatalf("connectivity = %q, want hibernated", got.Connectivity)
		}
		if got.ControllerGenerationID != "" {
			t.Fatalf("current generation = %q, want empty for hibernated workspace", got.ControllerGenerationID)
		}
		if len(got.ActiveFaults) != 0 {
			t.Fatalf("active faults = %d, want none for hibernated workspace", len(got.ActiveFaults))
		}
		err := m.ApplySessionConnectivity(
			"ws", "session-1", "generation-1",
			SessionConnectivityConnecting, "illegal_generation_reuse",
		)
		if !errors.Is(err, ErrStaleControllerGeneration) {
			t.Fatalf("hibernated generation reuse error = %v, want ErrStaleControllerGeneration", err)
		}
	})
}

func TestDaemonRestartInvalidatesPersistedControllerGeneration(t *testing.T) {
	path := filepath.Join(t.TempDir(), "state.db")
	firstLogs := &capLog{}
	first, err := Open(Options{DBPath: path, Logf: firstLogs.logf})
	if err != nil {
		t.Fatalf("Open first: %v", err)
	}
	connectOperational(t, first, "ws", "session-1", "generation-1")
	if err := first.Close(); err != nil {
		t.Fatalf("Close first: %v", err)
	}

	secondLogs := &capLog{}
	second, err := Open(Options{DBPath: path, Logf: secondLogs.logf})
	if err != nil {
		t.Fatalf("Open second: %v", err)
	}
	t.Cleanup(func() { second.Close() })
	got := mustComposite(t, second, "ws")
	if got.Connectivity != SessionConnectivityHibernated {
		t.Fatalf("connectivity after restart = %q, want hibernated", got.Connectivity)
	}
	if got.ControllerGenerationID != "" {
		t.Fatalf("generation after restart = %q, want empty", got.ControllerGenerationID)
	}
	if !secondLogs.contains(`cause="daemon_restart"`) ||
		!secondLogs.contains(`prior="operational"`) ||
		!secondLogs.contains(`next="hibernated"`) {
		t.Fatalf("restart log missing full transition: %v", secondLogs.lines)
	}

	before := connectivityRowCount(t, second.db, "ws")
	if err := second.Close(); err != nil {
		t.Fatalf("Close second: %v", err)
	}
	third, err := Open(Options{DBPath: path, Logf: func(string, ...any) {}})
	if err != nil {
		t.Fatalf("Open third: %v", err)
	}
	t.Cleanup(func() { third.Close() })
	if after := connectivityRowCount(t, third.db, "ws"); after != before {
		t.Fatalf("connectivity rows after reopening hibernated workspace = %d, want unchanged %d", after, before)
	}
}

func TestObservedStaleWorkspaceHistoriesResolveNewGenerationOperational(t *testing.T) {
	tests := []struct {
		name       string
		legacyRows []struct {
			sessionID string
			state     string
			cause     string
		}
	}{
		{
			name: "obsolete unmatched handshake before replacement",
			legacyRows: []struct {
				sessionID string
				state     string
				cause     string
			}{
				{"session-old", sigDegraded, "connection_degraded:turn handshake has no matching live session controller"},
			},
		},
		{
			name: "historical degradation has no recovery edge",
			legacyRows: []struct {
				sessionID string
				state     string
				cause     string
			}{
				{"session-old", sigWired, "wired:shim_ready"},
				{"session-old", sigDegraded, "connection_degraded:store subscription idle"},
			},
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			m, _, _ := openUnwiredTest(t, fakeResolver{})
			for i, row := range test.legacyRows {
				if err := appendRow(
					m.db,
					"ws",
					row.sessionID,
					row.state,
					row.cause,
					sql.NullInt64{},
					int64(i+1),
					"",
				); err != nil {
					t.Fatalf("seed legacy row %d: %v", i, err)
				}
			}
			connectOperational(t, m, "ws", "session-new", "generation-new")
			got := mustComposite(t, m, "ws")
			if got.Connectivity != SessionConnectivityOperational {
				t.Fatalf("connectivity = %q, want operational despite legacy history", got.Connectivity)
			}
			if len(got.ActiveFaults) != 0 {
				t.Fatalf("active faults = %d, want 0", len(got.ActiveFaults))
			}
		})
	}
}

func TestFaultWindowErrorsLogIdentityAndDoNotMutate(t *testing.T) {
	m, logs, _ := openUnwiredTest(t, fakeResolver{})
	connectOperational(t, m, "ws", "session-1", "generation-1")

	before := faultRowCount(t, m.db, "ws")
	err := m.ApplyRuntimeFault(
		"ws", "session-1", "generation-1",
		"shim-store", "subscription-idle",
		FaultImpactConnectivity, false, "unexpected_recovery",
	)
	if !errors.Is(err, ErrFaultWindowNotOpen) {
		t.Fatalf("close missing fault error = %v, want ErrFaultWindowNotOpen", err)
	}
	if after := faultRowCount(t, m.db, "ws"); after != before {
		t.Fatalf("fault rows after rejected close = %d, want unchanged %d", after, before)
	}
	for _, identity := range []string{
		`ws="ws"`,
		`session="session-1"`,
		`generation="generation-1"`,
		`component="shim-store"`,
		`fault_type="subscription-idle"`,
		`impact="connectivity"`,
		`branch=not-open`,
	} {
		if !logs.contains(identity) {
			t.Fatalf("error log missing %s: %v", identity, logs.lines)
		}
	}

	err = m.ApplyRuntimeFault(
		"ws", "session-1", "generation-1",
		"shim-store", "subscription-idle",
		FaultImpact("unknown"), true, "invalid_impact",
	)
	if err == nil || !strings.Contains(err.Error(), "invalid impact") {
		t.Fatalf("invalid impact error = %v, want explicit validation error", err)
	}
	if after := faultRowCount(t, m.db, "ws"); after != before {
		t.Fatalf("fault rows after invalid impact = %d, want unchanged %d", after, before)
	}

	if err := m.ApplyRuntimeFault(
		"ws", "session-1", "generation-1",
		"shim-store", "subscription-idle",
		FaultImpactConnectivity, true, "fault_open",
	); err != nil {
		t.Fatalf("open fault: %v", err)
	}
	afterOpen := faultRowCount(t, m.db, "ws")
	err = m.ApplyRuntimeFault(
		"ws", "session-1", "generation-1",
		"shim-store", "subscription-idle",
		FaultImpactConnectivity, true, "duplicate_open",
	)
	if !errors.Is(err, ErrFaultWindowAlreadyOpen) {
		t.Fatalf("duplicate open error = %v, want ErrFaultWindowAlreadyOpen", err)
	}
	if after := faultRowCount(t, m.db, "ws"); after != afterOpen {
		t.Fatalf("fault rows after duplicate open = %d, want unchanged %d", after, afterOpen)
	}
	err = m.ApplyRuntimeFault(
		"ws", "session-1", "generation-1",
		"shim-store", "subscription-idle",
		FaultImpactFeature, false, "wrong_impact_close",
	)
	if err == nil || !strings.Contains(err.Error(), "impact mismatch") {
		t.Fatalf("wrong-impact close error = %v, want impact mismatch", err)
	}
	if after := faultRowCount(t, m.db, "ws"); after != afterOpen {
		t.Fatalf("fault rows after wrong-impact close = %d, want unchanged %d", after, afterOpen)
	}
}

func TestClosedVocabularyValidation(t *testing.T) {
	connectivityCases := []struct {
		name                                    string
		workspace, sessionID, generation, cause string
		state                                   SessionConnectivity
		want                                    string
	}{
		{"workspace", "", "session", "generation", "cause", SessionConnectivityConnecting, "empty workspace"},
		{"session", "ws", "", "generation", "cause", SessionConnectivityConnecting, "empty agent-repl session id"},
		{"generation", "ws", "session", "", "cause", SessionConnectivityConnecting, "empty controller generation id"},
		{"cause", "ws", "session", "generation", "", SessionConnectivityConnecting, "empty cause kind"},
		{"state", "ws", "session", "generation", "cause", SessionConnectivityDegraded, "invalid lifecycle state"},
	}
	for _, test := range connectivityCases {
		t.Run("connectivity "+test.name, func(t *testing.T) {
			err := validateConnectivityIdentity(
				test.workspace,
				test.sessionID,
				test.generation,
				test.state,
				test.cause,
			)
			if err == nil || !strings.Contains(err.Error(), test.want) {
				t.Fatalf("error = %v, want containing %q", err, test.want)
			}
		})
	}

	faultCases := []struct {
		name                                                          string
		workspace, sessionID, generation, component, faultType, cause string
		impact                                                        FaultImpact
		want                                                          string
	}{
		{"workspace", "", "session", "generation", "component", "fault", "cause", FaultImpactConnectivity, "empty workspace"},
		{"session", "ws", "", "generation", "component", "fault", "cause", FaultImpactConnectivity, "empty agent-repl session id"},
		{"generation", "ws", "session", "", "component", "fault", "cause", FaultImpactConnectivity, "empty controller generation id"},
		{"component", "ws", "session", "generation", "", "fault", "cause", FaultImpactConnectivity, "empty component"},
		{"fault type", "ws", "session", "generation", "component", "", "cause", FaultImpactConnectivity, "empty fault type"},
		{"impact", "ws", "session", "generation", "component", "fault", "cause", FaultImpact("bad"), "invalid impact"},
		{"cause", "ws", "session", "generation", "component", "fault", "", FaultImpactConnectivity, "empty cause kind"},
	}
	for _, test := range faultCases {
		t.Run("fault "+test.name, func(t *testing.T) {
			err := validateFaultIdentity(
				test.workspace,
				test.sessionID,
				test.generation,
				test.component,
				test.faultType,
				test.impact,
				test.cause,
			)
			if err == nil || !strings.Contains(err.Error(), test.want) {
				t.Fatalf("error = %v, want containing %q", err, test.want)
			}
		})
	}
}

func TestSessionStatusVocabularyAndMonitoringProjection(t *testing.T) {
	tests := []struct {
		token     string
		liveTasks int64
		want      SessionStatus
	}{
		{sigThinking, 1, SessionStatusThinking},
		{sigPermission, 1, SessionStatusPermission},
		{sigDone, 0, SessionStatusDone},
		{sigDone, 1, SessionStatusMonitoring},
		{sigInterrupted, 0, SessionStatusInterrupted},
		{sigInterrupted, 1, SessionStatusMonitoring},
		{sigVendorBlocked, 1, SessionStatusVendorBlocked},
		{sigReady, 0, SessionStatusReady},
		{sigReady, 1, SessionStatusMonitoring},
		{sigIdle, 0, SessionStatusReady},
		{sigIdle, 1, SessionStatusMonitoring},
	}
	for _, test := range tests {
		t.Run(test.token, func(t *testing.T) {
			got, err := sessionStatusOf(test.token, test.liveTasks)
			if err != nil {
				t.Fatalf("sessionStatusOf(%q, %d): %v", test.token, test.liveTasks, err)
			}
			if got != test.want {
				t.Fatalf("sessionStatusOf(%q, %d) = %q, want %q",
					test.token, test.liveTasks, got, test.want)
			}
		})
	}
	if _, err := sessionStatusOf("unknown", 0); err == nil {
		t.Fatal("sessionStatusOf unknown token must fail")
	}
}

func TestConnectivityTransitionValidation(t *testing.T) {
	m, _, _ := openUnwiredTest(t, fakeResolver{})
	err := m.ApplySessionConnectivity(
		"ws", "session-1", "generation-1",
		SessionConnectivityOperational, "ready_without_connecting",
	)
	if !errors.Is(err, ErrConnectivityTransition) {
		t.Fatalf("first operational error = %v, want ErrConnectivityTransition", err)
	}
	if got := connectivityRowCount(t, m.db, "ws"); got != 0 {
		t.Fatalf("rows after invalid first operational = %d, want 0", got)
	}
	if err := m.ApplySessionConnectivity(
		"ws", "session-1", "generation-1",
		SessionConnectivityConnecting, "bring_up",
	); err != nil {
		t.Fatalf("connecting: %v", err)
	}
	before := connectivityRowCount(t, m.db, "ws")
	err = m.ApplySessionConnectivity(
		"ws", "session-1", "generation-1",
		SessionConnectivityConnecting, "duplicate_connecting",
	)
	if !errors.Is(err, ErrConnectivityTransition) {
		t.Fatalf("duplicate connecting error = %v, want ErrConnectivityTransition", err)
	}
	if after := connectivityRowCount(t, m.db, "ws"); after != before {
		t.Fatalf("rows after duplicate connecting = %d, want unchanged %d", after, before)
	}
	err = m.ApplySessionConnectivity(
		"ws", "session-2", "generation-2",
		SessionConnectivityUnavailable, "replacement_skipped_connecting",
	)
	if !errors.Is(err, ErrStaleControllerGeneration) {
		t.Fatalf("replacement unavailable error = %v, want ErrStaleControllerGeneration", err)
	}
}

func TestCompositeMissAndPersistedVocabularyErrors(t *testing.T) {
	t.Run("unknown workspace", func(t *testing.T) {
		m, logs, _ := openUnwiredTest(t, fakeResolver{})
		if _, found, err := m.Composite("unknown"); err != nil || found {
			t.Fatalf("Composite unknown = found %t error %v, want false + nil", found, err)
		}
		if !logs.contains(`branch=not-found`) {
			t.Fatalf("missing not-found log: %v", logs.lines)
		}
		if _, _, err := m.Composite(""); err == nil {
			t.Fatal("Composite empty workspace must fail")
		}
		if !logs.contains(`branch=validation`) {
			t.Fatalf("missing validation log: %v", logs.lines)
		}
	})

	t.Run("invalid connectivity", func(t *testing.T) {
		m, logs, _ := openUnwiredTest(t, fakeResolver{})
		if _, err := m.db.Exec(
			`INSERT INTO session_connectivity(
				workspace, agent_repl_session_id, controller_generation_id, state, cause_kind, at
			) VALUES (?,?,?,?,?,?)`,
			"ws", "session-1", "generation-1", "invalid", "fixture", 1,
		); err != nil {
			t.Fatalf("seed invalid connectivity: %v", err)
		}
		if _, _, err := m.Composite("ws"); err == nil || !strings.Contains(err.Error(), "invalid persisted session connectivity") {
			t.Fatalf("Composite invalid connectivity error = %v", err)
		}
		if !logs.contains(`branch=query`) {
			t.Fatalf("missing query error log: %v", logs.lines)
		}
	})

	t.Run("invalid fault impact", func(t *testing.T) {
		m, logs, _ := openUnwiredTest(t, fakeResolver{})
		connectOperational(t, m, "ws", "session-1", "generation-1")
		if _, err := m.db.Exec(
			`INSERT INTO session_fault(
				workspace, agent_repl_session_id, controller_generation_id,
				component, fault_type, impact, open, cause_kind, at
			) VALUES (?,?,?,?,?,?,?,?,?)`,
			"ws", "session-1", "generation-1", "component", "fault", "invalid", 1, "fixture", m.nextAt(),
		); err != nil {
			t.Fatalf("seed invalid fault: %v", err)
		}
		if _, _, err := m.Composite("ws"); err == nil || !strings.Contains(err.Error(), "invalid persisted runtime-fault impact") {
			t.Fatalf("Composite invalid fault impact error = %v", err)
		}
		if !logs.contains(`branch=query`) {
			t.Fatalf("missing fault query error log: %v", logs.lines)
		}
	})
}

func TestSessionConnectivitySchemaMigration(t *testing.T) {
	db := newTestDB(t)
	tables := []string{"session_connectivity", "session_fault"}
	for _, table := range tables {
		var found string
		if err := db.QueryRow(
			`SELECT name FROM sqlite_master WHERE type = 'table' AND name = ?`,
			table,
		).Scan(&found); err != nil {
			t.Fatalf("find table %s: %v", table, err)
		}
		if found != table {
			t.Fatalf("table = %q, want %q", found, table)
		}
	}
	var version int
	if err := db.QueryRow(`SELECT version FROM schema_meta LIMIT 1`).Scan(&version); err != nil {
		t.Fatalf("read schema version: %v", err)
	}
	if version != schemaVersion {
		t.Fatalf("schema version = %d, want %d", version, schemaVersion)
	}
}

func faultRowCount(t *testing.T, db *sql.DB, workspace string) int {
	t.Helper()
	var count int
	if err := db.QueryRow(`SELECT COUNT(*) FROM session_fault WHERE workspace = ?`, workspace).Scan(&count); err != nil {
		t.Fatalf("count session_fault rows: %v", err)
	}
	return count
}

func connectivityRowCount(t *testing.T, db *sql.DB, workspace string) int {
	t.Helper()
	var count int
	if err := db.QueryRow(`SELECT COUNT(*) FROM session_connectivity WHERE workspace = ?`, workspace).Scan(&count); err != nil {
		t.Fatalf("count session_connectivity rows: %v", err)
	}
	return count
}
