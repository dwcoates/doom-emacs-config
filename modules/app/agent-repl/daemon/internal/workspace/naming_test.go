package workspace

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestValidateSlugAcceptsTheNamingRule(t *testing.T) {
	tests := []struct {
		name string
		slug string
	}{
		{name: "one word", slug: "login"},
		{name: "two words", slug: "flaky-login"},
		{name: "three words", slug: "flaky-login-test"},
		{name: "digits are words", slug: "bump-v2-deps"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange in the table. Act.
			err := ValidateSlug(tt.slug)
			// Assert.
			if err != nil {
				t.Fatalf("ValidateSlug(%q) = %v, want nil", tt.slug, err)
			}
		})
	}
}

func TestValidateSlugRefusesWhatTheRuleForbids(t *testing.T) {
	tests := []struct {
		name string
		slug string
	}{
		{name: "empty", slug: ""},
		{name: "four words", slug: "fix-the-login-bug"},
		{name: "uppercase", slug: "Fix-Login"},
		{name: "spaces", slug: "fix login"},
		{name: "a sentence around the name", slug: "the name is fix-login"},
		{name: "leading hyphen", slug: "-fix-login"},
		{name: "trailing hyphen", slug: "fix-login-"},
		{name: "a slash", slug: "DWC/fix-login"},
		{name: "a path component", slug: "../escape"},
		{name: "punctuation", slug: "fix_login"},
		{name: "over the length bound", slug: strings.Repeat("a", SlugMaxLen+1)},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange in the table. Act.
			err := ValidateSlug(tt.slug)
			// Assert.
			if err == nil {
				t.Fatalf("ValidateSlug(%q) = nil, want a refusal: there is no repair path", tt.slug)
			}
		})
	}
}

func TestSlugWordCountCountsAlphanumericRuns(t *testing.T) {
	tests := []struct {
		name   string
		answer string
		want   int
	}{
		{name: "a well-shaped slug", answer: "agent-repl-input-shorter", want: 4},
		{name: "one word", answer: "login", want: 1},
		{name: "a sentence", answer: "The name is flaky-login-test.", want: 6},
		{name: "empty", answer: "", want: 0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange in the table. Act.
			got := SlugWordCount(tt.answer)
			// Assert.
			if got != tt.want {
				t.Fatalf("SlugWordCount(%q) = %d, want %d", tt.answer, got, tt.want)
			}
		})
	}
}

// TestValidateSlugNamesAnOverLongAnswersWordCount pins that a well-shaped
// answer with too many words is refused with its count and the limit, which is
// what the naming call's correction carries back to the model.
func TestValidateSlugNamesAnOverLongAnswersWordCount(t *testing.T) {
	// Arrange.
	answer := "agent-repl-input-shorter"

	// Act.
	err := ValidateSlug(answer)

	// Assert.
	want := fmt.Sprintf("the answer is 4 words, over the %d-word limit", SlugWordLimit)
	if err == nil || err.Error() != want {
		t.Fatalf("ValidateSlug(%q) = %v, want %q", answer, err, want)
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
