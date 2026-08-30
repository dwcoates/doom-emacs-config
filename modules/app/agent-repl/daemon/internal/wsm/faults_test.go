package wsm

import (
	"context"
	"errors"
	"testing"
	"time"
)

func TestOpenFaultMintsAnIdentity(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	id, err := s.OpenFault(context.Background(), Fault{Kind: "store_unreachable", Detail: "dial refused", OpenedAt: instant})

	// Assert
	if err != nil {
		t.Fatalf("OpenFault: %v", err)
	}
	if len(id) != IDLength {
		t.Fatalf("fault id = %q, want %d characters", id, IDLength)
	}
}

func TestOpenFaultRefusesAnEmptyKind(t *testing.T) {
	// Arrange
	s, log := testStore(t)

	// Act
	_, err := s.OpenFault(context.Background(), Fault{Detail: "no kind", OpenedAt: instant})

	// Assert
	if err == nil {
		t.Fatalf("OpenFault with no kind succeeded")
	}
	if !loggedOperation(log, "daemon.wsm.open_fault", "error") {
		t.Fatalf("the refusal was not logged at error: %v", log.Records())
	}
}

func TestOpenFaultsScopesByWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if _, err := s.OpenFault(context.Background(), Fault{Workspace: &ws.ID, Kind: "keepalive_failed", OpenedAt: instant}); err != nil {
		t.Fatalf("OpenFault: %v", err)
	}
	if _, err := s.OpenFault(context.Background(), Fault{Kind: "keepalive_failed", OpenedAt: instant}); err != nil {
		t.Fatalf("OpenFault: %v", err)
	}

	// Act
	got, err := s.OpenFaults(context.Background(), FaultScope{Workspace: &ws.ID})

	// Assert
	if err != nil {
		t.Fatalf("OpenFaults: %v", err)
	}
	if len(got) != 1 || got[0].Workspace == nil || *got[0].Workspace != ws.ID {
		t.Fatalf("faults = %+v, want only the workspace's own", got)
	}
}

func TestOpenFaultsScopesByKind(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	if _, err := s.OpenFault(context.Background(), Fault{Kind: "converter_defect", OpenedAt: instant}); err != nil {
		t.Fatalf("OpenFault: %v", err)
	}
	if _, err := s.OpenFault(context.Background(), Fault{Kind: "log_sink_poisoned", OpenedAt: instant}); err != nil {
		t.Fatalf("OpenFault: %v", err)
	}

	// Act
	got, err := s.OpenFaults(context.Background(), FaultScope{Kind: "converter_defect"})

	// Assert
	if err != nil {
		t.Fatalf("OpenFaults: %v", err)
	}
	if len(got) != 1 || got[0].Kind != "converter_defect" {
		t.Fatalf("faults = %+v, want only the converter defect", got)
	}
}

func TestOpenFaultsExcludesAResolvedFault(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	id, err := s.OpenFault(context.Background(), Fault{Kind: "vendor_query_failed", OpenedAt: instant})
	if err != nil {
		t.Fatalf("OpenFault: %v", err)
	}
	if err := s.CloseFault(context.Background(), id, instant.Add(time.Minute)); err != nil {
		t.Fatalf("CloseFault: %v", err)
	}

	// Act
	got, err := s.OpenFaults(context.Background(), FaultScope{})

	// Assert
	if err != nil {
		t.Fatalf("OpenFaults: %v", err)
	}
	if len(got) != 0 {
		t.Fatalf("faults = %+v, want none open", got)
	}
}

func TestCloseFaultPersistsTheResolvedInstant(t *testing.T) {
	// Arrange — a card that reopens unresolved on every boot is what the
	// persisted instant makes unrepresentable.
	s, _ := testStore(t)
	id, err := s.OpenFault(context.Background(), Fault{Kind: "store_unreachable", OpenedAt: instant})
	if err != nil {
		t.Fatalf("OpenFault: %v", err)
	}
	resolved := instant.Add(time.Hour)

	// Act
	if err := s.CloseFault(context.Background(), id, resolved); err != nil {
		t.Fatalf("CloseFault: %v", err)
	}

	// Assert
	got, err := s.Fault(context.Background(), id)
	if err != nil {
		t.Fatalf("Fault: %v", err)
	}
	if got.ResolvedAt == nil || !got.ResolvedAt.Equal(resolved) {
		t.Fatalf("resolved at = %v, want %v", got.ResolvedAt, resolved)
	}
}

func TestCloseFaultRefusesAnAlreadyResolvedFault(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	id, err := s.OpenFault(context.Background(), Fault{Kind: "store_unreachable", OpenedAt: instant})
	if err != nil {
		t.Fatalf("OpenFault: %v", err)
	}
	if err := s.CloseFault(context.Background(), id, instant.Add(time.Minute)); err != nil {
		t.Fatalf("CloseFault: %v", err)
	}

	// Act
	err = s.CloseFault(context.Background(), id, instant.Add(time.Hour))

	// Assert
	if err == nil {
		t.Fatalf("CloseFault on a resolved fault succeeded")
	}
	if !loggedOperation(log, "daemon.wsm.close_fault", "error") {
		t.Fatalf("the refusal was not logged at error: %v", log.Records())
	}
}

func TestCloseFaultRefusesAnUnknownFault(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.CloseFault(context.Background(), FaultID("absent"), instant)

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("CloseFault = %v, want ErrNotFound", err)
	}
}

func TestFaultRefusesAnUnknownFault(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	_, err := s.Fault(context.Background(), FaultID("absent"))

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("Fault = %v, want ErrNotFound", err)
	}
}

func TestForgetDeletesAWorkspacesFaults(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if _, err := s.OpenFault(context.Background(), Fault{Workspace: &ws.ID, Kind: "keepalive_failed", OpenedAt: instant}); err != nil {
		t.Fatalf("OpenFault: %v", err)
	}

	// Act
	if err := s.Forget(context.Background(), ws.ID); err != nil {
		t.Fatalf("Forget: %v", err)
	}

	// Assert
	if n := scalar[int](t, s, `SELECT count(*) FROM faults WHERE workspace_id = ?`, ws.ID); n != 0 {
		t.Fatalf("%d faults survived the nuke, want none", n)
	}
}

func TestOpenFaultRoundTripsTheTypedArmEvidence(t *testing.T) {
	// Arrange: the typed arm's own fields travel on the record, so the reporter
	// fills the arm rather than parsing them back out of the prose detail.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act.
	id, err := s.OpenFault(context.Background(), Fault{
		Workspace: &ws.ID,
		Kind:      "shim_start_failed",
		Detail:    "the shim exited during bring-up",
		Evidence:  map[string]string{"exit_code": "127", "stderr_tail": "node: not found"},
	})
	if err != nil {
		t.Fatalf("OpenFault: %v", err)
	}
	got, err := s.Fault(context.Background(), id)

	// Assert.
	if err != nil {
		t.Fatalf("Fault: %v", err)
	}
	if got.Evidence["exit_code"] != "127" || got.Evidence["stderr_tail"] != "node: not found" {
		t.Fatalf("evidence = %v, want the recorded exit code and stderr tail", got.Evidence)
	}
}

func TestOpenFaultStoresAbsentEvidenceAsAnEmptyObject(t *testing.T) {
	// Arrange: an absent map and an empty one read back the same, so a decode
	// never has to guess.
	s, _ := testStore(t)

	// Act.
	id, err := s.OpenFault(context.Background(), Fault{Kind: "wsm_read_only"})
	if err != nil {
		t.Fatalf("OpenFault: %v", err)
	}
	got, err := s.Fault(context.Background(), id)

	// Assert.
	if err != nil {
		t.Fatalf("Fault: %v", err)
	}
	if len(got.Evidence) != 0 {
		t.Fatalf("evidence = %v, want it empty", got.Evidence)
	}
}

func TestOpenFaultsCarriesTheEvidence(t *testing.T) {
	// Arrange.
	s, _ := testStore(t)
	if _, err := s.OpenFault(context.Background(), Fault{
		Kind: "log_sink_poisoned", Evidence: map[string]string{"sink": "/w/.claude/emacs/daemon.log"},
	}); err != nil {
		t.Fatalf("OpenFault: %v", err)
	}

	// Act.
	got, err := s.OpenFaults(context.Background(), FaultScope{})

	// Assert.
	if err != nil {
		t.Fatalf("OpenFaults: %v", err)
	}
	if len(got) != 1 || got[0].Evidence["sink"] != "/w/.claude/emacs/daemon.log" {
		t.Fatalf("faults = %+v, want the evidence carried", got)
	}
}

func TestOpenFaultsRefusesTheWholeReadOnCorruptEvidence(t *testing.T) {
	// Arrange: a fault whose typed arm cannot be filled is never reported as
	// one that can, so the corrupt row fails the read rather than degrading.
	s, _ := testStore(t)
	if _, err := s.OpenFault(context.Background(), Fault{Kind: "link_severed"}); err != nil {
		t.Fatalf("OpenFault: %v", err)
	}
	corrupt(t, s, `UPDATE faults SET evidence = 'not json'`)

	// Act.
	_, err := s.OpenFaults(context.Background(), FaultScope{})

	// Assert.
	if err == nil {
		t.Fatal("OpenFaults() = nil error, want the corrupt row to fail the read")
	}
}
