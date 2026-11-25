package gitclient

import (
	"context"
	"errors"
	"path/filepath"
	"strings"
	"testing"
)

// branchRef is the show-ref probe of BRANCH.
func branchRef(branch string) []string {
	return []string{"show-ref", "--verify", "--quiet", "refs/heads/" + branch}
}

func TestMoveWorktreeMovesTheTreeAndItsLogSinks(t *testing.T) {
	// Arrange
	git, surfaces := newTestClient(t)
	from := existingDir(t, "old")
	to := filepath.Join(t.TempDir(), "new")
	fake := newFakeGit(t, gitFixture{Match: []string{"worktree", "move"}, MoveFrom: from, MoveTo: to})

	// Act
	err := git.MoveWorktree(context.Background(), "/repo", from, to)

	// Assert
	if err != nil {
		t.Fatalf("MoveWorktree: %v", err)
	}
	fake.assertSubject(0, "worktree", "move", from, to)
	if got := strings.Join(surfaces.dirEvents, ","); got != "detach "+from+",attach "+to {
		t.Fatalf("log sink directory events = %q, want the old path detached and the new one attached", got)
	}
}

func TestMoveWorktreeAlreadyMadeOnlyAttachesTheDestination(t *testing.T) {
	// Arrange
	git, surfaces := newTestClient(t)
	from := filepath.Join(t.TempDir(), "old")
	to := existingDir(t, "new")
	fake := newFakeGit(t)

	// Act
	err := git.MoveWorktree(context.Background(), "/repo", from, to)

	// Assert
	if err != nil {
		t.Fatalf("MoveWorktree: %v", err)
	}
	fake.assertNever("worktree", "move")
	if got := strings.Join(surfaces.dirEvents, ","); got != "attach "+to {
		t.Fatalf("log sink directory events = %q, want only the destination attached", got)
	}
}

func TestMoveWorktreeRefusals(t *testing.T) {
	tests := []struct {
		name     string
		fromOn   bool
		toOn     bool
		wantMsg  string
		wantGone bool
	}{
		{name: "neither the tree nor its destination exists", wantMsg: "neither the worktree nor its destination exists"},
		{name: "the destination already exists", fromOn: true, toOn: true, wantMsg: "the destination already exists"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			git, surfaces := newTestClient(t)
			from, to := filepath.Join(t.TempDir(), "old"), filepath.Join(t.TempDir(), "new")
			if tc.fromOn {
				from = existingDir(t, "old")
			}
			if tc.toOn {
				to = existingDir(t, "new")
			}
			fake := newFakeGit(t)

			// Act
			err := git.MoveWorktree(context.Background(), "/repo", from, to)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tc.wantMsg) {
				t.Fatalf("MoveWorktree = %v, want %q", err, tc.wantMsg)
			}
			fake.assertNever("worktree", "move")
			if len(surfaces.dirEvents) != 0 {
				t.Fatalf("log sink directory events = %v, want none", surfaces.dirEvents)
			}
			if _, found := recordFor(surfaces.records(), "error", "daemon.gitclient.move_worktree"); !found {
				t.Fatal("the refusal was not recorded at ERROR")
			}
		})
	}
}

func TestMoveWorktreeGitRefusesKeepsTheTreeAttached(t *testing.T) {
	// Arrange
	git, surfaces := newTestClient(t)
	from := existingDir(t, "old")
	to := filepath.Join(t.TempDir(), "new")
	newFakeGit(t, fails(128, "fatal: locked", "worktree", "move"))

	// Act
	err := git.MoveWorktree(context.Background(), "/repo", from, to)

	// Assert
	if err == nil {
		t.Fatal("MoveWorktree = nil, want git's refusal")
	}
	if got := strings.Join(surfaces.dirEvents, ","); got != "detach "+from+",attach "+from {
		t.Fatalf("log sink directory events = %q, want the unmoved tree attached again", got)
	}
	if _, found := recordFor(surfaces.records(), "error", "daemon.gitclient.move_worktree"); !found {
		t.Fatal("git's refusal was not recorded at ERROR")
	}
}

func TestMoveWorktreeDetachFailureMovesNothing(t *testing.T) {
	// Arrange
	git, surfaces := newTestClient(t)
	surfaces.dirFailure = errors.New("detach refused")
	from := existingDir(t, "old")
	fake := newFakeGit(t)

	// Act
	err := git.MoveWorktree(context.Background(), "/repo", from, filepath.Join(t.TempDir(), "new"))

	// Assert
	if err == nil || !strings.Contains(err.Error(), "detach refused") {
		t.Fatalf("MoveWorktree = %v, want the detach failure", err)
	}
	fake.assertNever("worktree", "move")
}

func TestRenameBranchRenamesIt(t *testing.T) {
	// Arrange
	git, _ := newTestClient(t)
	fake := newFakeGit(t, ok("", branchRef("old")...), fails(1, "", branchRef("new")...), ok("", "branch", "-m"))

	// Act
	err := git.RenameBranch(context.Background(), "/repo", "old", "new")

	// Assert
	if err != nil {
		t.Fatalf("RenameBranch: %v", err)
	}
	fake.assertSubject(2, "branch", "-m", "old", "new")
}

func TestRenameBranchAlreadyMadeRunsNoRename(t *testing.T) {
	// Arrange
	git, surfaces := newTestClient(t)
	fake := newFakeGit(t, fails(1, "", branchRef("old")...), ok("", branchRef("new")...))

	// Act
	err := git.RenameBranch(context.Background(), "/repo", "old", "new")

	// Assert
	if err != nil {
		t.Fatalf("RenameBranch: %v", err)
	}
	fake.assertNever("branch", "-m")
	if _, found := recordFor(surfaces.records(), "error", "daemon.gitclient.rename_branch"); found {
		t.Fatal("a rename already made was recorded at ERROR")
	}
}

func TestRenameBranchRefusals(t *testing.T) {
	tests := []struct {
		name     string
		fixtures []gitFixture
		wantMsg  string
	}{
		{name: "neither branch exists", fixtures: []gitFixture{fails(1, "", branchRef("old")...), fails(1, "", branchRef("new")...)}, wantMsg: "neither branch exists"},
		{name: "the new name is already a branch", fixtures: []gitFixture{ok("", branchRef("old")...), ok("", branchRef("new")...)}, wantMsg: "already a branch"},
		{name: "git refuses the rename", fixtures: []gitFixture{ok("", branchRef("old")...), fails(1, "", branchRef("new")...), fails(128, "fatal: no", "branch", "-m")}, wantMsg: "fatal: no"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			git, surfaces := newTestClient(t)
			newFakeGit(t, tc.fixtures...)

			// Act
			err := git.RenameBranch(context.Background(), "/repo", "old", "new")

			// Assert
			if err == nil || !strings.Contains(err.Error(), tc.wantMsg) {
				t.Fatalf("RenameBranch = %v, want %q", err, tc.wantMsg)
			}
			if _, found := recordFor(surfaces.records(), "error", "daemon.gitclient.rename_branch"); !found {
				t.Fatal("the refusal was not recorded at ERROR")
			}
		})
	}
}
