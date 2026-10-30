package workspace

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"testing"

	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// retargetChange is a change from the main checkout to a parent workspace.
func retargetChange(ws ids.WorkspaceID) wsm.ParentChange {
	parent := ids.WorkspaceID("p1")
	return wsm.ParentChange{
		Workspace: ws,
		Before:    wsm.MergeTarget{Dir: "/repo", DefaultBranch: "master"},
		After:     wsm.MergeTarget{Parent: &parent, ParentName: "parent", Dir: "/repo-p1", DefaultBranch: "master"},
	}
}

func TestSetParentRecordsTheRetargetAndRepublishesTheRoster(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.setParentChange = retargetChange("w1")
	parent := ids.WorkspaceID("p1")

	// Act
	change, err := f.verbs.SetParent(context.Background(), "w1", &parent)

	// Assert
	if err != nil {
		t.Fatalf("SetParent: %v", err)
	}
	if change.After.Dir != "/repo-p1" {
		t.Fatalf("change = %+v, want the store's change", change)
	}
	if len(f.db.parentWrites) != 1 || f.db.parentWrites[0] == nil || *f.db.parentWrites[0] != parent {
		t.Fatalf("parent writes = %v, want one naming p1", f.db.parentWrites)
	}
	if len(f.sidebar.registries) != 1 {
		t.Fatalf("roster republications = %d, want exactly one", len(f.sidebar.registries))
	}
	if !hasLogRecord(f, "info", opSetParent) {
		t.Fatalf("records = %+v, want the retarget recorded at info", f.log.logger.Records())
	}
}

func TestSetParentRefuses(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f *fixture)
		want    string
	}{
		{name: "an unknown workspace", arrange: func(f *fixture) {}, want: ArmUnknownWorkspace},
		{name: "a workspace handed to a successor", arrange: func(f *fixture) {
			f.workspace("w1", "/w1")
			f.owner.standing = StandingTransferringAway
		}, want: ArmTransferringAway},
		{name: "a workspace not yet adopted", arrange: func(f *fixture) {
			f.workspace("w1", "/w1")
			f.owner.standing = StandingNotYetAdopted
		}, want: ArmNotYetAdopted},
		{name: "a workspace forgotten between the read and the write", arrange: func(f *fixture) {
			f.workspace("w1", "/w1")
			f.db.setParentErr = fmt.Errorf("wsm: workspace w1: %w", wsm.ErrNotFound)
		}, want: ArmUnknownWorkspace},
		{name: "an unknown parent", arrange: func(f *fixture) {
			f.workspace("w1", "/w1")
			f.db.setParentErr = &wsm.ParentRefusedError{Refusal: wsm.ParentUnknown, Workspace: "w1"}
		}, want: ArmUnknownParent},
		{name: "a cycle", arrange: func(f *fixture) {
			f.workspace("w1", "/w1")
			f.db.setParentErr = &wsm.ParentRefusedError{Refusal: wsm.ParentCycle, Workspace: "w1"}
		}, want: ArmCycle},
		{name: "no merge layout", arrange: func(f *fixture) {
			f.workspace("w1", "/w1")
			f.db.setParentErr = &wsm.ParentRefusedError{Refusal: wsm.ParentNoMergeLayout, Workspace: "w1"}
		}, want: ArmNoMergeLayout},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			tt.arrange(f)

			// Act
			_, err := f.verbs.SetParent(context.Background(), "w1", nil)

			// Assert
			refusal := asRefusal(t, err, tt.want)
			if refusal.Rpc != rpcSetParent {
				t.Fatalf("rpc = %q, want %q", refusal.Rpc, rpcSetParent)
			}
			if len(f.sidebar.registries) != 0 {
				t.Fatalf("roster republications = %d, want none for a refusal", len(f.sidebar.registries))
			}
			if !hasLogRecord(f, "info", opRefusal) {
				t.Fatalf("records = %+v, want the refusal recorded at info", f.log.logger.Records())
			}
		})
	}
}

func TestSetParentSurfacesAFailedWriteAtError(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", "/w1")
	f.db.setParentErr = errors.New("disk I/O error")

	// Act
	_, err := f.verbs.SetParent(context.Background(), "w1", nil)

	// Assert
	if err == nil {
		t.Fatal("a failed retarget write was answered as success")
	}
	if _, refused := AsRefusal(err); refused {
		t.Fatalf("a failed write was answered as a refusal: %v", err)
	}
	if !loggedAtError(f, opSetParent) {
		t.Fatal("the failed write was not recorded at error")
	}
	if len(f.sidebar.registries) != 0 {
		t.Fatalf("roster republications = %d, want none for a failed write", len(f.sidebar.registries))
	}
}

func TestRestoreParent(t *testing.T) {
	tests := []struct {
		name          string
		stale         bool
		wantLevel     string
		wantRepublish int
	}{
		{name: "undoes the retarget", wantLevel: "info", wantRepublish: 1},
		{name: "leaves a superseding retarget", stale: true, wantLevel: "warn"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.workspace("w1", "/w1")
			f.db.restoreStale = tt.stale
			change := retargetChange("w1")

			// Act
			err := f.verbs.RestoreParent(context.Background(), change)

			// Assert
			if err != nil {
				t.Fatalf("RestoreParent: %v", err)
			}
			if len(f.db.restores) != 1 || f.db.restores[0].After.Dir != change.After.Dir {
				t.Fatalf("restores = %+v, want the change", f.db.restores)
			}
			if !hasLogRecord(f, tt.wantLevel, opSetParent) {
				t.Fatalf("records = %+v, want %s under %s", f.log.logger.Records(), tt.wantLevel, opSetParent)
			}
			if len(f.sidebar.registries) != tt.wantRepublish {
				t.Fatalf("roster republications = %d, want %d", len(f.sidebar.registries), tt.wantRepublish)
			}
		})
	}
}

func TestRestoreParentSurfacesAFailedWriteAtError(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", "/w1")
	f.db.restoreErr = errors.New("disk I/O error")

	// Act
	err := f.verbs.RestoreParent(context.Background(), retargetChange("w1"))

	// Assert
	if err == nil {
		t.Fatal("a failed undo was answered as success")
	}
	if !loggedAtError(f, opSetParent) {
		t.Fatal("the failed undo was not recorded at error")
	}
}

func TestUnknownParentRefusalIsTheUnknownParentArmAtInfo(t *testing.T) {
	// Arrange
	f := newFixture(t)

	// Act
	refusal := UnknownParentRefusal(f.log.logger, "/nowhere", wsm.ErrNotFound)

	// Assert
	if refusal.Arm != ArmUnknownParent || refusal.Rpc != rpcSetParent || !refusal.NotFound {
		t.Fatalf("refusal = %+v, want SetWorkspaceParent's unknown_parent, not found", refusal)
	}
	if !strings.Contains(refusal.Reason, "/nowhere") {
		t.Fatalf("reason = %q, want the directory named", refusal.Reason)
	}
	if !hasLogRecord(f, "info", opRefusal) {
		t.Fatalf("records = %+v, want the refusal recorded at info", f.log.logger.Records())
	}
}

func TestSetParentAnswersAParentThatIsNotOpen(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.setParentErr = errParentNotOpen
	parent := ids.WorkspaceID("parent")

	// Act
	_, err := f.verbs.SetParent(context.Background(), "w1", &parent)

	// Assert
	requireFamilyAnswer(t, f, err, errParentNotOpen, opSetParent)
	if len(f.sidebar.registries) != 0 {
		t.Fatalf("roster republications = %d, want none for a refused retarget", len(f.sidebar.registries))
	}
}
