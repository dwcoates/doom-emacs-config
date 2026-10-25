package wsm

import (
	"context"
	"errors"
	"testing"
)

func TestSetNameRecordsTheNameAndLeavesBranchAndDir(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.SetName(context.Background(), ws.ID, "renamed-workspace")

	// Assert
	if err != nil {
		t.Fatalf("SetName: %v", err)
	}
	got, err := s.Workspace(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if got.Name != "renamed-workspace" {
		t.Errorf("name = %q, want %q", got.Name, "renamed-workspace")
	}
	if got.Branch != ws.Branch || got.Dir != ws.Dir {
		t.Errorf("branch, dir = %q, %q; want the unchanged %q, %q", got.Branch, got.Dir, ws.Branch, ws.Dir)
	}
}

func TestSetNameRefusesAnEmptyNameAndWritesNothing(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.SetName(context.Background(), ws.ID, "")

	// Assert
	if !errors.Is(err, errEmptyName) {
		t.Fatalf("SetName(\"\") = %v, want errEmptyName", err)
	}
	got, _ := s.Workspace(context.Background(), ws.ID)
	if got.Name != ws.Name {
		t.Errorf("name = %q, want the standing %q untouched", got.Name, ws.Name)
	}
	found := false
	for _, r := range log.Records() {
		if r.Operation == "daemon.wsm.set_name" && r.Level == "error" && r.Context["workspace"] == string(ws.ID) {
			found = true
		}
	}
	if !found {
		t.Errorf("no error record of daemon.wsm.set_name names the workspace: %v", log.Records())
	}
}

func TestSetNameOfAnUnknownWorkspaceIsNotFound(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.SetName(context.Background(), WorkspaceID("absent"), "anything")

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("SetName = %v, want ErrNotFound", err)
	}
}
