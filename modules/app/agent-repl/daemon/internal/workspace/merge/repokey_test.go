package merge

import (
	"context"
	"path/filepath"
	"testing"
)

func TestNewGitRepoKeyerRequiresLogf(t *testing.T) {
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
			k, err := NewGitRepoKeyer(tc.logf)

			// Assert.
			if tc.wantErr {
				if err == nil {
					t.Fatalf("NewGitRepoKeyer() error = nil, want error")
				}
				return
			}
			if err != nil || k == nil {
				t.Fatalf("NewGitRepoKeyer() = %v, %v, want a keyer", k, err)
			}
		})
	}
}

func TestRepoKeyRefusesEmptyWorktreeDir(t *testing.T) {
	// Arrange.
	var logs []string
	k, err := NewGitRepoKeyer(func(format string, args ...any) { logs = append(logs, format) })
	if err != nil {
		t.Fatalf("NewGitRepoKeyer: %v", err)
	}

	// Act.
	key, err := k.RepoKey(context.Background(), "")

	// Assert — refused, logged, and no key invented.
	if err == nil {
		t.Fatalf("RepoKey(\"\") error = nil, want error")
	}
	if key != "" {
		t.Fatalf("RepoKey(\"\") key = %q, want empty", key)
	}
	if len(logs) != 1 {
		t.Fatalf("logs = %v, want exactly one canonical record", logs)
	}
}

func TestRepoKeyResolvesTheMainWorktreeToItsGitDir(t *testing.T) {
	// Arrange.
	target := initTarget(t)
	k := newTestKeyer(t)

	// Act.
	key, err := k.RepoKey(context.Background(), target)
	if err != nil {
		t.Fatalf("RepoKey: %v", err)
	}

	// Assert — the absolute, canonical .git of the repo.
	want, err := filepath.EvalSymlinks(filepath.Join(target, ".git"))
	if err != nil {
		t.Fatalf("EvalSymlinks: %v", err)
	}
	if key != want {
		t.Fatalf("RepoKey = %q, want %q", key, want)
	}
	if !filepath.IsAbs(key) {
		t.Fatalf("RepoKey = %q, want an absolute path", key)
	}
}

func TestRepoKeyCollapsesSiblingWorktreesToOneKey(t *testing.T) {
	// Arrange — the real system's shape: a target worktree and a linked sibling.
	target := initTarget(t)
	feature := addFeatureWorktree(t, target)
	k := newTestKeyer(t)

	// Act.
	targetKey, err := k.RepoKey(context.Background(), target)
	if err != nil {
		t.Fatalf("RepoKey(target): %v", err)
	}
	featureKey, err := k.RepoKey(context.Background(), feature)
	if err != nil {
		t.Fatalf("RepoKey(feature): %v", err)
	}

	// Assert — one repository, one queue key.
	if targetKey != featureKey {
		t.Fatalf("sibling worktrees keyed differently: %q vs %q", targetKey, featureKey)
	}
}

func TestRepoKeyKeepsUnrelatedRepositoriesApart(t *testing.T) {
	// Arrange.
	one := initTarget(t)
	two := initTarget(t)
	k := newTestKeyer(t)

	// Act.
	keyOne, err := k.RepoKey(context.Background(), one)
	if err != nil {
		t.Fatalf("RepoKey(one): %v", err)
	}
	keyTwo, err := k.RepoKey(context.Background(), two)
	if err != nil {
		t.Fatalf("RepoKey(two): %v", err)
	}

	// Assert.
	if keyOne == keyTwo {
		t.Fatalf("unrelated repos share key %q", keyOne)
	}
}

func TestRepoKeySurfacesGitFailure(t *testing.T) {
	// Arrange — a directory that is not a git worktree at all.
	var logs []string
	k, err := NewGitRepoKeyer(func(format string, args ...any) { logs = append(logs, format) })
	if err != nil {
		t.Fatalf("NewGitRepoKeyer: %v", err)
	}

	// Act.
	key, err := k.RepoKey(context.Background(), t.TempDir())

	// Assert — an error, a canonical log record, and no fabricated key.
	if err == nil {
		t.Fatalf("RepoKey(non-repo) error = nil, want error")
	}
	if key != "" {
		t.Fatalf("RepoKey(non-repo) key = %q, want empty", key)
	}
	if len(logs) != 1 {
		t.Fatalf("logs = %v, want exactly one canonical record", logs)
	}
}

func newTestKeyer(t *testing.T) *GitRepoKeyer {
	t.Helper()
	k, err := NewGitRepoKeyer(t.Logf)
	if err != nil {
		t.Fatalf("NewGitRepoKeyer: %v", err)
	}
	return k
}
