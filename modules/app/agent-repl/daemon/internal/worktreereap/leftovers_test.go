package worktreereap

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"slices"
	"testing"

	"agent-repld/internal/wsm"
)

// leftover makes a directory NAME beside the worktrees holding a link per
// LINKS entry, each a path relative to the directory, and answers it. Every
// link names a real file under the world's targets.
func (w *world) leftover(name string, links ...string) string {
	w.t.Helper()
	dir := w.mkdir(filepath.Join("trees", name))
	for _, rel := range links {
		target := filepath.Join(w.mkdir("targets"), name+"-"+filepath.Base(rel))
		if err := os.WriteFile(target, []byte("log"), 0o644); err != nil {
			w.t.Fatalf("write %s: %v", target, err)
		}
		link := filepath.Join(dir, rel)
		if err := os.MkdirAll(filepath.Dir(link), 0o755); err != nil {
			w.t.Fatalf("mkdir %s: %v", filepath.Dir(link), err)
		}
		if err := os.Symlink(target, link); err != nil {
			w.t.Fatalf("symlink %s: %v", link, err)
		}
	}
	return dir
}

func TestALeftoverHoldingOnlyLogLinksIsRemoved(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	w.addRepo("repo")
	dir := w.leftover("gone", ".claude/emacs/emacs.log", ".claude/emacs/daemon.log")

	// Act.
	report := w.sweep()

	// Assert.
	if !slices.Equal(report.Leftovers, []string{dir}) {
		t.Fatalf("leftovers = %v, want [%s]", report.Leftovers, dir)
	}
	if _, err := os.Lstat(dir); !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("the leftover still stands: %v", err)
	}
	if len(w.records("info", opLeftover)) != 1 {
		t.Fatalf("leftover INFO records = %v, want one", w.records("info", opLeftover))
	}
}

func TestRemovingALeftoverKeepsWhatItsLinksName(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	w.addRepo("repo")
	w.leftover("gone", ".claude/emacs/emacs.log")

	// Act.
	w.sweep()

	// Assert.
	if _, err := os.Stat(filepath.Join(w.root, "targets", "gone-emacs.log")); err != nil {
		t.Fatalf("the link's target went with it: %v", err)
	}
}

func TestADirectoryHoldingAnythingButLogLinksIsKept(t *testing.T) {
	tests := []struct {
		name  string
		stage func(w *world) string
	}{
		{name: "a regular file beside the links", stage: func(w *world) string {
			dir := w.leftover("x", ".claude/emacs/emacs.log")
			w.writeAged(filepath.Join(dir, "notes.txt"), longAgo)
			return dir
		}},
		{name: "a link not named .log", stage: func(w *world) string { return w.leftover("x", ".claude/emacs/emacs.txt") }},
		{name: "a log link outside .claude/emacs", stage: func(w *world) string { return w.leftover("x", "emacs.log") }},
		{name: "an empty directory", stage: func(w *world) string { return w.mkdir(filepath.Join("trees", "x")) }},
		{name: "a .git file", stage: func(w *world) string {
			dir := w.leftover("x", ".claude/emacs/emacs.log")
			w.writeAged(filepath.Join(dir, ".git"), longAgo)
			return dir
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			w := newWorld(t)
			w.addRepo("repo")
			dir := tt.stage(w)

			// Act.
			report := w.sweep()

			// Assert.
			if len(report.Leftovers) != 0 || report.Kept[keepNotLeftover] != 1 {
				t.Fatalf("report %+v, want %s kept as %s", report, dir, keepNotLeftover)
			}
			if _, err := os.Lstat(dir); err != nil {
				t.Fatalf("the directory was removed: %v", err)
			}
		})
	}
}

func TestALeftoverGitStillListsIsAWorktreeNotALeftover(t *testing.T) {
	// Arrange: a listed worktree whose directory happens to hold only links.
	w := newWorld(t)
	repo := w.addRepo("repo")
	dir := w.leftover("listed", ".claude/emacs/emacs.log")
	w.addTree(repo, tree{name: "listed", outcome: unlanded})

	// Act.
	report := w.sweep()

	// Assert.
	if len(report.Leftovers) != 0 {
		t.Fatalf("leftovers = %v, want none: %s is a listed worktree", report.Leftovers, dir)
	}
}

func TestALeftoverAnOpenWorkspaceNamesIsKept(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	repo := w.addRepo("repo")
	dir := w.leftover("open", ".claude/emacs/emacs.log")
	w.register(repo, dir, wsm.Workspace{})

	// Act.
	report := w.sweep()

	// Assert.
	if len(report.Leftovers) != 0 || report.Kept[keepOpenWorkspace] != 1 {
		t.Fatalf("report %+v, want the open workspace's directory kept", report)
	}
}

func TestADryRunListsALeftoverAndKeepsIt(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	w.addRepo("repo")
	dir := w.leftover("gone", ".claude/emacs/emacs.log")

	// Act.
	report := w.sweepWith(Options{DryRun: true})

	// Assert.
	if !slices.Equal(report.Leftovers, []string{dir}) {
		t.Fatalf("leftovers = %v, want [%s]", report.Leftovers, dir)
	}
	if _, err := os.Lstat(dir); err != nil {
		t.Fatalf("a dry run removed the leftover: %v", err)
	}
}

func TestARepositoryWithNoWorktreesDirectoryHasNoLeftovers(t *testing.T) {
	// Arrange: no trees directory exists at all.
	w := newWorld(t)
	w.addRepo("repo")

	// Act.
	report := w.sweep()

	// Assert.
	if len(report.Leftovers) != 0 || report.Failures != 0 {
		t.Fatalf("report %+v, want nothing and no failure", report)
	}
}

func TestAWorktreesDirectoryThatCannotBeResolvedIsAnError(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	w.addRepo("repo")
	deps := w.deps()
	deps.WorktreeParent = func(string) (string, error) { return "", errors.New("no parent") }
	r, err := New(deps)
	if err != nil {
		t.Fatalf("New: %v", err)
	}

	// Act.
	report, err := r.Sweep(context.Background())

	// Assert.
	if err != nil || report.Failures != 1 || len(w.records("error", opLeftover)) != 1 {
		t.Fatalf("report %+v err %v, want one failure recorded at ERROR", report, err)
	}
}

func TestNewRefusesAMissingWorktreeParent(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	deps := w.deps()
	deps.WorktreeParent = nil

	// Act.
	_, err := New(deps)

	// Assert.
	if err == nil {
		t.Fatal("New accepted a reaper with no WorktreeParent")
	}
}
