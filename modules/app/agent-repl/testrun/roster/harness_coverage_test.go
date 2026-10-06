package roster

import (
	"io/fs"
	"os"
	"path/filepath"
	"runtime"
	"slices"
	"strings"
	"testing"
)

// harnessScripts answers every test-named shell harness in the places the
// repository keeps them, spelled as Suite.Path spells it: module-relative,
// or "/"-prefixed and repository-relative outside the module.
func harnessScripts(t *testing.T, module, repo string) []string {
	t.Helper()
	var found []string
	for _, glob := range []string{"bin/test-*.sh", "scripts/test-*.sh"} {
		matches, err := filepath.Glob(filepath.Join(module, glob))
		if err != nil {
			t.Fatal(err)
		}
		for _, m := range matches {
			rel, _ := filepath.Rel(module, m)
			found = append(found, rel)
		}
	}
	for _, glob := range []string{".githooks/test-*.sh", "bin/test-*.sh"} {
		matches, err := filepath.Glob(filepath.Join(repo, glob))
		if err != nil {
			t.Fatal(err)
		}
		for _, m := range matches {
			rel, _ := filepath.Rel(repo, m)
			found = append(found, "/"+rel)
		}
	}
	claude, err := claudeHarnesses(repo)
	if err != nil {
		t.Fatal(err)
	}
	return append(found, claude...)
}

// claudeHarnesses answers every test-named shell script under repo/.claude,
// "/"-prefixed and repository-relative.
//
// IT NEVER DESCENDS INTO ANOTHER CHECKOUT. Claude Code keeps whole copies of
// the repository under .claude/worktrees for its agent sessions, and a walk
// that entered them flagged each copy's harnesses as unrostered here (full
// run on master 4472a9f23). So .claude/worktrees is skipped, and so is any
// directory holding a .git of its own: the root of a worktree or a nested
// repository, which is never this repository's.
func claudeHarnesses(repo string) ([]string, error) {
	var found []string
	root := filepath.Join(repo, ".claude")
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			if path == filepath.Join(root, "worktrees") {
				return filepath.SkipDir
			}
			if _, statErr := os.Lstat(filepath.Join(path, ".git")); statErr == nil {
				return filepath.SkipDir
			}
			return nil
		}
		if strings.HasPrefix(d.Name(), "test") && strings.HasSuffix(d.Name(), ".sh") {
			rel, _ := filepath.Rel(repo, path)
			found = append(found, "/"+rel)
		}
		return nil
	})
	return found, err
}

func TestEveryHarnessScriptIsRosteredOrExcluded(t *testing.T) {
	// Arrange
	_, self, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatal("runtime.Caller could not locate this file")
	}
	module := filepath.Clean(filepath.Join(filepath.Dir(self), "..", ".."))
	repo := filepath.Clean(filepath.Join(module, "..", "..", ".."))
	scripts := harnessScripts(t, module, repo)
	if len(scripts) == 0 {
		t.Fatal("found no harness scripts: the layout this test assumes moved")
	}
	rostered := map[string]bool{}
	for _, s := range Suites {
		rostered[s.Path] = true
	}

	// Act
	var forgotten []string
	for _, path := range scripts {
		if _, excluded := NotSuites[path]; !rostered[path] && !excluded {
			forgotten = append(forgotten, path)
		}
	}

	// Assert
	if len(forgotten) > 0 {
		slices.Sort(forgotten)
		t.Fatalf("these harnesses run in no suite; add each to roster.Suites, or to roster.NotSuites with why:\n%s", strings.Join(forgotten, "\n"))
	}
}

func TestEveryExclusionNamesAScriptThatExists(t *testing.T) {
	// Arrange
	_, self, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatal("runtime.Caller could not locate this file")
	}
	module := filepath.Clean(filepath.Join(filepath.Dir(self), "..", ".."))
	repo := filepath.Clean(filepath.Join(module, "..", "..", ".."))
	scripts := harnessScripts(t, module, repo)

	// Act / Assert
	for path, why := range NotSuites {
		if why == "" {
			t.Errorf("%s is excluded without a reason", path)
		}
		if !slices.Contains(scripts, path) {
			t.Errorf("%s is excluded but is no harness script any more; drop the stale entry", path)
		}
	}
}

func TestClaudeHarnessesNeverEntersANestedCheckout(t *testing.T) {
	tests := []struct {
		name  string
		files []string
		want  []string
	}{
		{name: "a harness of this repository is found", files: []string{".claude/skills/q/test-run.sh"}, want: []string{"/.claude/skills/q/test-run.sh"}},
		{name: "a copy under .claude/worktrees is skipped", files: []string{".claude/worktrees/agent-1/.claude/test-install.sh"}, want: nil},
		{name: "a directory with its own .git is skipped", files: []string{".claude/vendor/other/.git", ".claude/vendor/other/test-x.sh"}, want: nil},
		{name: "a script not named test is ignored", files: []string{".claude/install.sh"}, want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			repo := t.TempDir()
			for _, f := range tt.files {
				path := filepath.Join(repo, f)
				if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
					t.Fatal(err)
				}
				if err := os.WriteFile(path, nil, 0o644); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			got, err := claudeHarnesses(repo)

			// Assert
			if err != nil {
				t.Fatal(err)
			}
			if !slices.Equal(got, tt.want) {
				t.Fatalf("found %q, want %q", got, tt.want)
			}
		})
	}
}
