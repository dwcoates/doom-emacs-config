package wsm

import (
	"context"
	"errors"
	"os"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
)

func TestSetWorkspaceColumnsAssignsEveryColumnTogether(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.setWorkspaceColumns(context.Background(), "daemon.wsm.test", ws.ID, dlog.Context{},
		workspaceColumn{name: "attention", value: true},
		workspaceColumn{name: "commit_count", value: int64(9)})

	// Assert
	if err != nil {
		t.Fatalf("setWorkspaceColumns: %v", err)
	}
	got, err := s.Workspace(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if !got.Attention || got.CommitCount == nil || *got.CommitCount != 9 {
		t.Fatalf("attention = %v, commit count = %v; want both assigned", got.Attention, got.CommitCount)
	}
}

func TestSetWorkspaceColumnsRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	s, log := testStore(t)

	// Act
	err := s.setWorkspaceColumns(context.Background(), "daemon.wsm.test", WorkspaceID("absent"), dlog.Context{},
		workspaceColumn{name: "attention", value: true})

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("setWorkspaceColumns = %v, want ErrNotFound", err)
	}
	found := false
	for _, r := range log.Records() {
		if r.Operation == "daemon.wsm.test" && r.Context["workspace"] == "absent" {
			found = true
		}
	}
	if !found {
		t.Fatalf("no daemon.wsm.test record names the workspace: %v", log.Records())
	}
}

// TestPlainWorkspaceUpdatesGoThroughSetWorkspaceColumns pins the shared shape:
// workspaces.go spells its plain update-by-id once, in setWorkspaceColumns.
func TestPlainWorkspaceUpdatesGoThroughSetWorkspaceColumns(t *testing.T) {
	// Arrange
	source, err := os.ReadFile("workspaces.go")
	if err != nil {
		t.Fatalf("read workspaces.go: %v", err)
	}

	// Act
	spelled := strings.Count(string(source), "UPDATE workspaces SET ")

	// Assert: setWorkspaceColumns, MarkClosed's, SetCurrent's three (the
	// departing workspace's dwell, the clear and the set), and SetPriority's,
	// which share a transaction with another statement (the priority's
	// re-landing of the workspace's place) or assign NULLs and literals.
	if spelled != 6 {
		t.Fatalf("workspaces.go spells a workspace update %d times, want 6 (setWorkspaceColumns, MarkClosed, SetCurrent three times, SetPriority)", spelled)
	}
}
