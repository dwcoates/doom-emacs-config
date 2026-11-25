package server

import (
	"testing"

	"agent-repld/internal/ids"
)

func TestPerWorkspaceMintsAnEntryOnFirstUse(t *testing.T) {
	// Arrange.
	s := &server{}
	m := map[ids.WorkspaceID]*int{}

	// Act.
	got := perWorkspace(s, m, "w1")

	// Assert.
	if got == nil || m["w1"] != got {
		t.Fatalf("perWorkspace = %v, map = %v, want a minted entry stored under w1", got, m)
	}
}

func TestPerWorkspaceAnswersTheSameEntryAgain(t *testing.T) {
	// Arrange.
	s := &server{}
	m := map[ids.WorkspaceID]*int{}
	first := perWorkspace(s, m, "w1")

	// Act.
	second := perWorkspace(s, m, "w1")

	// Assert.
	if first != second {
		t.Fatal("a second lookup minted a new entry, want the first one")
	}
}

func TestPerWorkspaceKeepsWorkspacesApart(t *testing.T) {
	// Arrange.
	s := &server{}
	m := map[ids.WorkspaceID]*int{}
	first := perWorkspace(s, m, "w1")

	// Act.
	other := perWorkspace(s, m, "w2")

	// Assert.
	if first == other || len(m) != 2 {
		t.Fatalf("w1 and w2 share an entry (map %v), want one each", m)
	}
}
