package workspace

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestSlugAppliesTheNamingRule(t *testing.T) {
	tests := []struct {
		name string
		text string
		want string
	}{
		{name: "lowercases and hyphenates", text: "Fix The Login", want: "fix-the-login"},
		{name: "keeps at most three words", text: "fix the login bug today", want: "fix-the-login"},
		{name: "collapses punctuation runs", text: "fix:: the -- login", want: "fix-the-login"},
		{name: "keeps digits", text: "bump v2 deps", want: "bump-v2-deps"},
		{name: "ignores leading separators", text: "  ...fix login", want: "fix-login"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange in the table. Act.
			got, err := Slug(tt.text)
			// Assert.
			if err != nil {
				t.Fatalf("Slug(%q): %v", tt.text, err)
			}
			if got != tt.want {
				t.Fatalf("Slug(%q) = %q, want %q", tt.text, got, tt.want)
			}
		})
	}
}

func TestSlugBoundsTheLength(t *testing.T) {
	// Arrange: three words that together exceed the bound.
	text := strings.Repeat("a", 20) + " " + strings.Repeat("b", 20) + " " + strings.Repeat("c", 20)

	// Act.
	got, err := Slug(text)

	// Assert.
	if err != nil {
		t.Fatalf("Slug: %v", err)
	}
	if len(got) > SlugMaxLen {
		t.Fatalf("Slug() = %q (%d chars), want at most %d", got, len(got), SlugMaxLen)
	}
}

func TestSlugNeverEndsOnATruncatedHyphen(t *testing.T) {
	// Arrange: the bound falls exactly on the separator between two words.
	text := strings.Repeat("a", SlugMaxLen) + " tail"

	// Act.
	got, err := Slug(text)

	// Assert.
	if err != nil {
		t.Fatalf("Slug: %v", err)
	}
	if strings.HasSuffix(got, "-") {
		t.Fatalf("Slug() = %q, want no trailing hyphen", got)
	}
}

func TestSlugRefusesTextWithNoWords(t *testing.T) {
	// Arrange. Act.
	_, err := Slug("!!! ??? ...")

	// Assert.
	if err == nil {
		t.Fatal("Slug(punctuation only) = nil error, want a refusal")
	}
}

func TestNamePrefixesTheSlug(t *testing.T) {
	// Arrange. Act.
	got := Name("DWC", "fix-login")

	// Assert.
	if got != "DWC/fix-login" {
		t.Fatalf("Name() = %q, want DWC/fix-login", got)
	}
}

func TestNameWithoutAPrefixIsTheBareSlug(t *testing.T) {
	// Arrange. Act.
	got := Name("", "fix-login")

	// Assert.
	if got != "fix-login" {
		t.Fatalf("Name() = %q, want fix-login", got)
	}
}

func TestBareNameStripsThePrefix(t *testing.T) {
	// Arrange. Act.
	got := BareName("DWC/fix-login")

	// Assert.
	if got != "fix-login" {
		t.Fatalf("BareName() = %q, want fix-login", got)
	}
}

func TestPrefixPrefersTheCurrentEnvironmentVariable(t *testing.T) {
	// Arrange.
	t.Setenv(PrefixEnv, "NEW")
	t.Setenv(LegacyPrefixEnv, "OLD")

	// Act.
	got := Prefix()

	// Assert.
	if got != "NEW" {
		t.Fatalf("Prefix() = %q, want NEW", got)
	}
}

func TestPrefixFallsBackToTheLegacyEnvironmentVariable(t *testing.T) {
	// Arrange.
	t.Setenv(PrefixEnv, "")
	t.Setenv(LegacyPrefixEnv, "OLD")

	// Act.
	got := Prefix()

	// Assert.
	if got != "OLD" {
		t.Fatalf("Prefix() = %q, want OLD", got)
	}
}

func TestWorktreeDirOfAMainWorktreeIsTheSiblingWorktreesDirectory(t *testing.T) {
	// Arrange: a main worktree, whose .git is a DIRECTORY.
	parent := t.TempDir()
	repo := filepath.Join(parent, "doom")
	if err := os.MkdirAll(filepath.Join(repo, ".git"), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}

	// Act.
	got, err := WorktreeDir(repo, "DWC/fix-login")

	// Assert.
	want := filepath.Join(parent, "doom-worktrees", "fix-login")
	if err != nil || got != want {
		t.Fatalf("WorktreeDir() = (%q, %v), want %q", got, err, want)
	}
}

func TestWorktreeDirOfALinkedWorktreeStaysASibling(t *testing.T) {
	// Arrange: a linked worktree, whose .git is a regular FILE.
	parent := t.TempDir()
	worktree := filepath.Join(parent, "existing")
	if err := os.MkdirAll(worktree, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	if err := os.WriteFile(filepath.Join(worktree, ".git"), []byte("gitdir: elsewhere\n"), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}

	// Act.
	got, err := WorktreeDir(worktree, "DWC/fix-login")

	// Assert.
	want := filepath.Join(parent, "fix-login")
	if err != nil || got != want {
		t.Fatalf("WorktreeDir() = (%q, %v), want %q", got, err, want)
	}
}

func TestWorktreeDirRefusesABranchWithNoSafeComponent(t *testing.T) {
	// Arrange.
	repo := t.TempDir()
	if err := os.MkdirAll(filepath.Join(repo, ".git"), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}

	// Act.
	_, err := WorktreeDir(repo, "/")

	// Assert.
	if err == nil {
		t.Fatal("WorktreeDir(branch \"/\") = nil error, want a refusal")
	}
}

func TestIsWorktreeRejectsAPlainDirectory(t *testing.T) {
	// Arrange. Act. Assert.
	if IsWorktree(t.TempDir()) {
		t.Fatal("IsWorktree(plain directory) reported a worktree")
	}
}

func TestIsWorktreeAcceptsAGitDirectory(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	if err := os.MkdirAll(filepath.Join(dir, ".git"), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}

	// Act. Assert.
	if !IsWorktree(dir) {
		t.Fatal("IsWorktree(git directory) reported no worktree")
	}
}

func TestWorktreeDirOfACommonDirReadsBackToTheMainWorktree(t *testing.T) {
	// Arrange: the repository as the registry spells it — its COMMON DIR.
	parent := t.TempDir()
	repo := filepath.Join(parent, "doom")
	if err := os.MkdirAll(filepath.Join(repo, ".git"), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}

	// Act.
	got, err := WorktreeDir(filepath.Join(repo, ".git"), "DWC/fix-login")

	// Assert.
	want := filepath.Join(parent, "doom-worktrees", "fix-login")
	if err != nil || got != want {
		t.Fatalf("WorktreeDir() = (%q, %v), want %q", got, err, want)
	}
}

func TestMainWorktreeDirLeavesAWorktreeAlone(t *testing.T) {
	// Arrange.
	dir := filepath.Join("/tmp", "repos", "doom")

	// Act.
	got := MainWorktreeDir(dir)

	// Assert.
	if got != dir {
		t.Fatalf("MainWorktreeDir(%q) = %q, want it unchanged", dir, got)
	}
}
