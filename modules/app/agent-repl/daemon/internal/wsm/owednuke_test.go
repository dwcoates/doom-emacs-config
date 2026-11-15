package wsm

import (
	"context"
	"testing"
	"time"
)

func TestOweNukeMarksTheWorkspaceOwingANuke(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	at := time.Unix(1_700_000_000, 0)

	// Act
	err := s.OweNuke(context.Background(), ws.ID, at)

	// Assert
	if err != nil {
		t.Fatalf("OweNuke: %v", err)
	}
	listed, err := s.ListWorkspaces(context.Background())
	if err != nil || len(listed) != 1 {
		t.Fatalf("ListWorkspaces = (%+v, %v), want the one workspace", listed, err)
	}
	if got := listed[0].NukeOwedAt; got == nil || !got.Equal(at) {
		t.Fatalf("NukeOwedAt = %v, want %v", got, at)
	}
}

func TestAWorkspaceOwesNoNukeUntilOneBegins(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	testWorkspace(t, s)

	// Act
	listed, err := s.ListWorkspaces(context.Background())

	// Assert
	if err != nil || len(listed) != 1 || listed[0].NukeOwedAt != nil {
		t.Fatalf("ListWorkspaces = (%+v, %v), want one workspace owing no nuke", listed, err)
	}
}

func TestOweNukeRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.OweNuke(context.Background(), "no-such-workspace", time.Unix(1, 0))

	// Assert
	if err == nil {
		t.Fatal("OweNuke accepted a workspace the registry does not hold")
	}
}

func TestForgetRetiresTheOwedNukeWithTheRow(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.OweNuke(context.Background(), ws.ID, time.Unix(1, 0)); err != nil {
		t.Fatalf("OweNuke: %v", err)
	}

	// Act
	_, err := s.Forget(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("Forget: %v", err)
	}
	if n := scalar[int](t, s, `SELECT count(*) FROM workspaces WHERE nuke_owed_at IS NOT NULL`); n != 0 {
		t.Fatalf("rows owing a nuke after the forget = %d, want 0", n)
	}
}

// TestTheOwedNukeStepAddsAnUnsetMark pins the layout-141 step: every row
// already registered owes no nuke.
func TestTheOwedNukeStepAddsAnUnsetMark(t *testing.T) {
	// Arrange
	path := fixtureAt(t, stepNamed(t, "owed_nukes")-1)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	defer handle.Close()
	listed, err := handle.ListWorkspaces(context.Background())

	// Assert
	if err != nil || len(listed) == 0 {
		t.Fatalf("ListWorkspaces = (%+v, %v), want the fixture's workspaces", listed, err)
	}
	for _, ws := range listed {
		if ws.NukeOwedAt != nil {
			t.Fatalf("workspace %s owes a nuke after the step, want none", ws.ID)
		}
	}
}
