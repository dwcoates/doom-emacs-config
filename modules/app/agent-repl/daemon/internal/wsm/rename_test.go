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

func TestSetNameOwesTheNameToTheAgentOnce(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	ctx := context.Background()
	if err := s.SetName(ctx, ws.ID, "first-name"); err != nil {
		t.Fatalf("SetName: %v", err)
	}

	// Act
	if err := s.SetName(ctx, ws.ID, "second-name"); err != nil {
		t.Fatalf("SetName: %v", err)
	}
	first, firstFound, firstErr := s.TakeOwedRename(ctx, ws.ID)
	_, secondFound, secondErr := s.TakeOwedRename(ctx, ws.ID)

	// Assert
	if firstErr != nil || !firstFound || first != "second-name" {
		t.Fatalf("first take = (%q, %v, %v), want the latest name", first, firstFound, firstErr)
	}
	if secondErr != nil || secondFound {
		t.Fatalf("second take = (%v, %v), want nothing left", secondFound, secondErr)
	}
}

func TestRestoringATakenNameKeepsANewerOne(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	ctx := context.Background()
	if err := s.SetName(ctx, ws.ID, "older-name"); err != nil {
		t.Fatalf("SetName: %v", err)
	}
	taken, _, _ := s.TakeOwedRename(ctx, ws.ID)
	if err := s.SetName(ctx, ws.ID, "newer-name"); err != nil {
		t.Fatalf("SetName: %v", err)
	}

	// Act
	err := s.RestoreOwedRename(ctx, ws.ID, taken)

	// Assert
	got, found, _ := s.TakeOwedRename(ctx, ws.ID)
	if err != nil || !found || got != "newer-name" {
		t.Fatalf("RestoreOwedRename = %v, owed = (%q, %v); want the newer name kept", err, got, found)
	}
}

func TestRestoringAnEmptyNameIsRefusedAndRecorded(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.RestoreOwedRename(context.Background(), ws.ID, "")

	// Assert
	if !errors.Is(err, errEmptyName) {
		t.Fatalf("RestoreOwedRename(\"\") = %v, want errEmptyName", err)
	}
	found := false
	for _, r := range log.Records() {
		if r.Operation == "daemon.wsm.restore_owed_rename" && r.Level == "error" {
			found = true
		}
	}
	if !found {
		t.Errorf("no error record of daemon.wsm.restore_owed_rename: %v", log.Records())
	}
}

func TestABindForgetsTheNameTheOldConversationOwed(t *testing.T) {
	// Arrange
	s, _, ws := ownForkStore(t)
	ctx := context.Background()
	if err := s.SetName(ctx, ws.ID, "renamed"); err != nil {
		t.Fatalf("SetName: %v", err)
	}

	// Act
	err := s.BindSession(ctx, boundSession(t, s, ws), nil)

	// Assert
	if _, found, _ := s.TakeOwedRename(ctx, ws.ID); err != nil || found {
		t.Fatalf("BindSession = %v, owed found = %v; want the name forgotten", err, found)
	}
}
