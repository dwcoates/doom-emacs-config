package session

import (
	"path/filepath"
	"testing"
)

// ---------------------------------------------------------------------------
// The account a workspace runs under is a function of its path, and this is the
// daemon's single evaluation of that function. It must answer exactly what
// Emacs' agent-repl--compute-config-dir answers, because a disagreement is a
// workspace whose transcripts nothing else can find.
// ---------------------------------------------------------------------------

func TestAccountConfigDirFor(t *testing.T) {
	home := t.TempDir()
	multiRoot := filepath.Join(home, "workspace", "ChessCom")
	multiConfig := filepath.Join(home, ".claude-chesscom")

	tests := []struct {
		name      string
		root      string
		path      string
		want      string
		wantEmpty bool
	}{
		{
			name: "a worktree under the multi-repo root takes the multi-repo account",
			root: multiRoot,
			path: filepath.Join(multiRoot, "explanation-engine-worktrees", "slack-thread-pr-link"),
			want: multiConfig,
		},
		{
			name: "the multi-repo root itself is under itself",
			root: multiRoot,
			path: multiRoot,
			want: multiConfig,
		},
		{
			name:      "a path outside the root takes the CLI default, spelled empty",
			root:      multiRoot,
			path:      filepath.Join(home, ".config", "doom-worktrees", "some-branch"),
			wantEmpty: true,
		},
		{
			name:      "a sibling whose name merely prefixes the root is outside it",
			root:      multiRoot,
			path:      multiRoot + "-other",
			wantEmpty: true,
		},
		{
			name:      "no root configured routes everything to the default",
			root:      "",
			path:      filepath.Join(multiRoot, "explanation-engine"),
			wantEmpty: true,
		},
		{
			name:      "an empty path has no account to resolve",
			root:      multiRoot,
			path:      "",
			wantEmpty: true,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			t.Setenv(MultiRepoRootEnv, tc.root)
			t.Setenv(MultiRepoConfigDirEnv, multiConfig)

			// Act
			got, err := AccountConfigDirFor(tc.path)

			// Assert
			if err != nil {
				t.Fatalf("AccountConfigDirFor(%q): %v", tc.path, err)
			}
			want := tc.want
			if tc.wantEmpty {
				want = ""
			}
			if got != want {
				t.Fatalf("AccountConfigDirFor(%q) = %q, want %q", tc.path, got, want)
			}
		})
	}
}

func TestMultiRepoConfigDirDefaultsToTheChessComAccount(t *testing.T) {
	// Arrange — nothing overrides it, so the default expands against home.
	t.Setenv(MultiRepoConfigDirEnv, "")

	// Act
	got, err := MultiRepoConfigDir()

	// Assert
	if err != nil {
		t.Fatalf("MultiRepoConfigDir: %v", err)
	}
	want, err := ExpandHome(DefaultMultiRepoConfigDir)
	if err != nil {
		t.Fatalf("ExpandHome: %v", err)
	}
	if got != want {
		t.Fatalf("MultiRepoConfigDir = %q, want %q", got, want)
	}
}

func TestUnderDirIgnoresPathSpelling(t *testing.T) {
	// Arrange / Act / Assert — "/a/b" and "/a//b/." are one answer.
	if !UnderDir("/a/b", "/a//b/./c") {
		t.Fatal("UnderDir did not clean its inputs")
	}
}
