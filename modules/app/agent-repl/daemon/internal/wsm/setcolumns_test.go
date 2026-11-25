package wsm

import (
	"context"
	"errors"
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
