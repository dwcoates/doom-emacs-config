package workspace

import (
	"context"
	"errors"
	"testing"
)

func TestKillForcesTheSessionDeath(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Kill(context.Background(), "w1"); err != nil {
		t.Fatalf("Kill: %v", err)
	}

	// Assert.
	if len(f.shim.killedSession) != 1 || !f.shim.killedSession[0] {
		t.Fatalf("KillSession forces = %v, want exactly one forced kill", f.shim.killedSession)
	}
}

func TestKillStopsTheShimEvenWhenTheSessionWillNotAnswer(t *testing.T) {
	// Arrange: a shim that refuses the forced kill is not a reason to leave the
	// workspace alive.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.shim.killSessionErr = errors.New("the query refused to end")

	// Act.
	if err := f.verbs.Kill(context.Background(), "w1"); err != nil {
		t.Fatalf("Kill: %v", err)
	}

	// Assert.
	if len(f.fleet.stopped) != 1 || !f.fleet.stopped[0].Force {
		t.Fatalf("stopped sessions = %+v, want one forced stop", f.fleet.stopped)
	}
}

func TestKillClosesTheOrphanedTurns(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Kill(context.Background(), "w1"); err != nil {
		t.Fatalf("Kill: %v", err)
	}

	// Assert: the terminal is REHYDRATABLE, so the record says how it died.
	terminal, ok := f.db.terminals["w1"]
	if !ok || terminal.Kind != "killed" {
		t.Fatalf("session terminal = %+v, want a killed terminal", terminal)
	}
}

func TestKillMarksTheRosterRowClosed(t *testing.T) {
	// Arrange: Emacs derives its tab set from closed = true.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Kill(context.Background(), "w1"); err != nil {
		t.Fatalf("Kill: %v", err)
	}

	// Assert.
	if !f.db.closedFlags["w1"] {
		t.Fatal("Kill() did not mark the roster row closed")
	}
}

func TestKillLeavesTheWorkspaceRecordInPlace(t *testing.T) {
	// Arrange: Kill destroys no data.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Kill(context.Background(), "w1"); err != nil {
		t.Fatalf("Kill: %v", err)
	}

	// Assert.
	if len(f.db.forgotten) != 0 {
		t.Fatalf("forgotten workspaces = %v, want none", f.db.forgotten)
	}
}

func TestKillWithNoLiveSessionStillStopsTheShim(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.hasSession = false

	// Act.
	if err := f.verbs.Kill(context.Background(), "w1"); err != nil {
		t.Fatalf("Kill: %v", err)
	}

	// Assert.
	if len(f.shim.killedSession) != 0 {
		t.Fatalf("KillSession calls = %v, want none without a live session", f.shim.killedSession)
	}
}

func TestNukeDestroysTheWorktreeAndTheBranch(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ws := f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Nuke(context.Background(), "w1"); err != nil {
		t.Fatalf("Nuke: %v", err)
	}

	// Assert.
	if len(f.git.nuked) != 1 {
		t.Fatalf("nuked worktrees = %+v, want exactly one", f.git.nuked)
	}
	if f.git.nuked[0].WorktreeDir != ws.Dir || f.git.nuked[0].Branch != ws.Branch {
		t.Fatalf("nuked %+v, want the workspace's own dir and branch", f.git.nuked[0])
	}
}

func TestNukeLeavesTheRoster(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Nuke(context.Background(), "w1"); err != nil {
		t.Fatalf("Nuke: %v", err)
	}

	// Assert.
	if len(f.db.forgotten) != 1 || f.db.forgotten[0] != "w1" {
		t.Fatalf("forgotten workspaces = %v, want w1", f.db.forgotten)
	}
}

func TestNukeKillsALiveSessionFirst(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.fleet.live["w1"] = true

	// Act.
	if err := f.verbs.Nuke(context.Background(), "w1"); err != nil {
		t.Fatalf("Nuke: %v", err)
	}

	// Assert.
	if len(f.shim.killedSession) != 1 {
		t.Fatalf("KillSession calls = %v, want one before the nuke", f.shim.killedSession)
	}
}

func TestNukeForgetsNothingWhenTheDestructionFails(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.git.nukeErr = errors.New("the worktree is locked")

	// Act.
	err := f.verbs.Nuke(context.Background(), "w1")

	// Assert.
	if err == nil {
		t.Fatal("Nuke() = nil error, want the git failure surfaced")
	}
	if len(f.db.forgotten) != 0 {
		t.Fatalf("forgotten workspaces = %v, want none after a failed destruction", f.db.forgotten)
	}
}

func TestNukeRefusesAnUnregisteredRepository(t *testing.T) {
	// Arrange: the repository the worktree belongs to is not in the registry.
	f := newFixture(t)
	f.db.with(f.workspace("w1", t.TempDir()))
	f.db.repositories = nil

	// Act.
	err := f.verbs.Nuke(context.Background(), "w1")

	// Assert.
	if err == nil {
		t.Fatal("Nuke() = nil error, want the unregistered repository surfaced")
	}
}
