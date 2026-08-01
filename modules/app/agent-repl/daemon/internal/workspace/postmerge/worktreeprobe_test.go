package postmerge

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"claude-repld/internal/gitexec"
)

// --- git fixtures -------------------------------------------------------

func gitRun(t *testing.T, dir string, args ...string) {
	t.Helper()
	// The same env-stripped builder the probe uses: without that boundary a
	// scratch `git init` or `git commit` can operate on the CALLER's real
	// repository instead of the temporary one.
	full := append([]string{"-c", "core.hooksPath=/dev/null"}, args...)
	cmd := gitexec.Command(context.Background(), dir, full...)
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("git %v in %s: %v\n%s", args, dir, err, out)
	}
}

// initRepo creates a temp repository with one commit, in a temp dir only.
func initRepo(t *testing.T) string {
	t.Helper()
	dir := t.TempDir()
	gitRun(t, dir, "init", "-q", "-b", "main")
	gitRun(t, dir, "config", "user.email", "test@example.com")
	gitRun(t, dir, "config", "user.name", "Test")
	gitRun(t, dir, "config", "commit.gpgsign", "false")
	gitRun(t, dir, "config", "core.hooksPath", "/dev/null")
	if err := os.WriteFile(filepath.Join(dir, "base.txt"), []byte("base\n"), 0o644); err != nil {
		t.Fatalf("write fixture file: %v", err)
	}
	gitRun(t, dir, "add", ".")
	gitRun(t, dir, "commit", "-q", "-m", "A")
	return dir
}

// addLinkedWorktree adds a linked worktree on a new branch, modeling a child
// workspace's checkout.
func addLinkedWorktree(t *testing.T, repo string) string {
	t.Helper()
	dir := filepath.Join(t.TempDir(), "wt")
	gitRun(t, repo, "worktree", "add", "-q", "-b", "feature", dir, "main")
	return dir
}

func newTestProbe(t *testing.T) (*GitWorktreeProbe, *[]string) {
	t.Helper()
	var logs []string
	p, err := NewGitWorktreeProbe(func(format string, args ...any) {
		logs = append(logs, format)
		t.Logf(format, args...)
	})
	if err != nil {
		t.Fatalf("NewGitWorktreeProbe: %v", err)
	}
	return p, &logs
}

// --- construction -------------------------------------------------------

func TestNewGitWorktreeProbeRequiresLogf(t *testing.T) {
	tests := []struct {
		name    string
		logf    func(string, ...any)
		wantErr bool
	}{
		{name: "with logger", logf: func(string, ...any) {}, wantErr: false},
		{name: "nil logger", logf: nil, wantErr: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			p, err := NewGitWorktreeProbe(tc.logf)

			// Assert.
			if tc.wantErr {
				if err == nil {
					t.Fatalf("NewGitWorktreeProbe() error = nil, want error")
				}
				return
			}
			if err != nil || p == nil {
				t.Fatalf("NewGitWorktreeProbe() = %v, %v, want a probe", p, err)
			}
		})
	}
}

// --- probing ------------------------------------------------------------

func TestIsLinkedWorktreeIsFalseForTheMainWorktree(t *testing.T) {
	// Arrange — the repository's own checkout.
	repo := initRepo(t)
	p, _ := newTestProbe(t)

	// Act.
	linked, err := p.IsLinkedWorktree(context.Background(), repo)

	// Assert.
	if err != nil {
		t.Fatalf("IsLinkedWorktree() error = %v", err)
	}
	if linked {
		t.Fatalf("IsLinkedWorktree(main) = true, want false")
	}
}

func TestIsLinkedWorktreeIsTrueForALinkedWorktree(t *testing.T) {
	// Arrange — a sibling worktree of the same repository.
	repo := initRepo(t)
	feature := addLinkedWorktree(t, repo)
	p, _ := newTestProbe(t)

	// Act.
	linked, err := p.IsLinkedWorktree(context.Background(), feature)

	// Assert.
	if err != nil {
		t.Fatalf("IsLinkedWorktree() error = %v", err)
	}
	if !linked {
		t.Fatalf("IsLinkedWorktree(linked) = false, want true")
	}
}

func TestIsLinkedWorktreeRefusesAnEmptyDirectory(t *testing.T) {
	// Arrange.
	p, logs := newTestProbe(t)

	// Act.
	linked, err := p.IsLinkedWorktree(context.Background(), "")

	// Assert — refused, logged, and no answer invented.
	if err == nil {
		t.Fatalf("IsLinkedWorktree(\"\") error = nil, want error")
	}
	if linked {
		t.Fatalf("IsLinkedWorktree(\"\") = true, want false alongside the error")
	}
	if len(*logs) != 1 {
		t.Fatalf("logs = %v, want exactly one canonical record", *logs)
	}
}

func TestIsLinkedWorktreeSurfacesANonRepositoryDirectory(t *testing.T) {
	// Arrange — a directory git knows nothing about.
	dir := t.TempDir()
	p, logs := newTestProbe(t)

	// Act.
	linked, err := p.IsLinkedWorktree(context.Background(), dir)

	// Assert — an error, never a guessed "not linked" that would silently
	// swallow the parent handoff.
	if err == nil {
		t.Fatalf("IsLinkedWorktree(non-repo) error = nil, want error")
	}
	if linked {
		t.Fatalf("IsLinkedWorktree(non-repo) = true, want false alongside the error")
	}
	found := false
	for _, line := range *logs {
		if strings.Contains(line, "FAILED") {
			found = true
		}
	}
	if !found {
		t.Fatalf("logs = %v, want the git failure recorded", *logs)
	}
}

func TestIsLinkedWorktreeSurfacesAMissingDirectory(t *testing.T) {
	// Arrange — the worktree was removed between the merge and the handoff.
	p, _ := newTestProbe(t)

	// Act.
	_, err := p.IsLinkedWorktree(context.Background(), filepath.Join(t.TempDir(), "gone"))

	// Assert.
	if err == nil {
		t.Fatalf("IsLinkedWorktree(missing) error = nil, want error")
	}
}
