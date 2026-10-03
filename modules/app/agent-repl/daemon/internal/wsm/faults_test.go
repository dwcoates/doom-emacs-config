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
	if _, err := s.Forget(context.Background(), ws.ID); err != nil {
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

// TestOpenFaultRefusesAForgottenWorkspaceAsNotFound is the shim-death
// cascade's root: the link watcher, the health reporter and the lifecycle sink
// all outlive a registry row, so a shim that dies after its workspace has been
// forgotten opens a fault about a workspace that is gone. The foreign key
// always refused it; what it refused WITH was `FOREIGN KEY constraint failed
// (787)` at ERROR, in three layers at once.
func TestOpenFaultRefusesAForgottenWorkspaceAsNotFound(t *testing.T) {
	tests := []struct {
		name        string
		registered  bool
		wantErr     error
		wantLevel   string
		wantSuccess bool
	}{
		{
			name:        "a workspace the registry still holds",
			registered:  true,
			wantLevel:   "debug",
			wantSuccess: true,
		},
		{
			name:       "a workspace forgotten while its shim was dying",
			registered: false,
			wantErr:    ErrNotFound,
			wantLevel:  "debug",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			s, log := testStore(t)
			id := WorkspaceID("0000000000000000")
			if tt.registered {
				id = testWorkspace(t, s).ID
			}

			// Act.
			_, err := s.OpenFault(context.Background(), Fault{
				Workspace: &id, Kind: "shim_died", Detail: "the shim process is gone", OpenedAt: instant,
			})

			// Assert.
			if tt.wantSuccess {
				if err != nil {
					t.Fatalf("OpenFault: %v", err)
				}
			} else if !errors.Is(err, tt.wantErr) {
				t.Fatalf("OpenFault error = %v, want %v", err, tt.wantErr)
			}
			if loggedOperation(log, "daemon.wsm.open_fault", "error") {
				t.Fatalf("the write was reported at error: %v", log.Records())
			}
		})
	}
}

func TestFaultRecordedMatchesByWhatTheFaultWasRaisedAbout(t *testing.T) {
	recordedEvidence := map[string]string{"turn": "adopted-1", "unit": "msg-1:0", "why": "answer_row_unresolved"}
	tests := []struct {
		name     string
		resolved bool
		kind     string
		evidence map[string]string
		want     bool
	}{
		{name: "an open fault with the same evidence", kind: "final_answer_unresolved", evidence: recordedEvidence, want: true},
		{name: "a resolved fault with the same evidence", resolved: true, kind: "final_answer_unresolved", evidence: recordedEvidence, want: true},
		{name: "a subset of the recorded evidence", kind: "final_answer_unresolved", evidence: map[string]string{"turn": "adopted-1"}, want: true},
		{name: "another turn", kind: "final_answer_unresolved", evidence: map[string]string{"turn": "adopted-2", "unit": "msg-1:0", "why": "answer_row_unresolved"}, want: false},
		{name: "another kind", kind: "keepalive_failed", evidence: recordedEvidence, want: false},
		{name: "an evidence key the record does not carry", kind: "final_answer_unresolved", evidence: map[string]string{"turn": "adopted-1", "agent": "main"}, want: false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			id, err := s.OpenFault(context.Background(), Fault{
				Workspace: &ws.ID, Kind: "final_answer_unresolved", Detail: "no drawn row", Evidence: recordedEvidence, OpenedAt: instant,
			})
			if err != nil {
				t.Fatalf("OpenFault: %v", err)
			}
			if tt.resolved {
				if err := s.CloseFault(context.Background(), id, instant.Add(time.Minute)); err != nil {
					t.Fatalf("CloseFault: %v", err)
				}
			}

			// Act.
			got, err := s.FaultRecorded(context.Background(), FaultMatch{Workspace: ws.ID, Kind: tt.kind, Evidence: tt.evidence})

			// Assert.
			if err != nil {
				t.Fatalf("FaultRecorded: %v", err)
			}
			if got != tt.want {
				t.Fatalf("FaultRecorded = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestFaultRecordedIsScopedToItsWorkspace(t *testing.T) {
	// Arrange.
	s, _ := testStore(t)
	raised := testWorkspaceNamed(t, s, "raised")
	other := testWorkspaceNamed(t, s, "other")
	evidence := map[string]string{"turn": "adopted-1"}
	if _, err := s.OpenFault(context.Background(), Fault{
		Workspace: &raised.ID, Kind: "final_answer_unresolved", Evidence: evidence, OpenedAt: instant,
	}); err != nil {
		t.Fatalf("OpenFault: %v", err)
	}

	// Act.
	got, err := s.FaultRecorded(context.Background(), FaultMatch{Workspace: other.ID, Kind: "final_answer_unresolved", Evidence: evidence})

	// Assert.
	if err != nil {
		t.Fatalf("FaultRecorded: %v", err)
	}
	if got {
		t.Fatalf("FaultRecorded = true for a workspace that raised nothing")
	}
}

func TestFaultRecordedRefusesAnIllFormedMatch(t *testing.T) {
	tests := []struct {
		name  string
		match func(ws WorkspaceID) FaultMatch
	}{
		{name: "no kind", match: func(ws WorkspaceID) FaultMatch { return FaultMatch{Workspace: ws} }},
		{name: "no workspace", match: func(WorkspaceID) FaultMatch { return FaultMatch{Kind: "final_answer_unresolved"} }},
		{name: "an evidence key that is no field name", match: func(ws WorkspaceID) FaultMatch {
			return FaultMatch{Workspace: ws, Kind: "final_answer_unresolved", Evidence: map[string]string{"turn') OR 1=1 --": "x"}}
		}},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			s, log := testStore(t)
			ws := testWorkspace(t, s)

			// Act.
			_, err := s.FaultRecorded(context.Background(), tt.match(ws.ID))

			// Assert.
			if err == nil {
				t.Fatalf("FaultRecorded accepted an ill-formed match")
			}
			if !loggedOperation(log, "daemon.wsm.fault_recorded", "error") {
				t.Fatalf("the refusal was not logged at error: %v", log.Records())
			}
		})
	}
}
