package sandboxscripts

import (
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"slices"
	"strings"
	"testing"
)

// The sandbox stages an ENUMERATED set of the module's top-level entries
// (STAGE_ENTRIES in sandbox/bin/e2e-sandbox.sh), and every Go module inside
// it must build there. A `replace` pointing at a local module outside that
// set builds on the host and fails only inside the container, as
// "replacement directory ... does not exist" -- which is how the test
// runner's testrun module, imported by the e2e and daemon harnesses, was
// first left out.

var stageEntryLine = regexp.MustCompile(`^\s+([A-Za-z0-9._-]+)\s*$`)

// stageEntries reads STAGE_ENTRIES' names out of the entrypoint script.
func stageEntries(t *testing.T, script string) map[string]bool {
	t.Helper()
	data, err := os.ReadFile(script)
	if err != nil {
		t.Fatalf("read %s: %v", script, err)
	}
	entries := map[string]bool{}
	in := false
	for line := range strings.SplitSeq(string(data), "\n") {
		switch {
		case strings.HasPrefix(line, "STAGE_ENTRIES=("):
			in = true
		case in && strings.TrimSpace(line) == ")":
			in = false
		case in:
			if m := stageEntryLine.FindStringSubmatch(line); m != nil {
				entries[m[1]] = true
			}
		}
	}
	if len(entries) == 0 {
		t.Fatalf("%s declares no STAGE_ENTRIES", script)
	}
	return entries
}

var localReplace = regexp.MustCompile(`=>\s+(\.\.?/\S*)`)

func TestEveryLocalGoReplaceIsStagedInTheSandbox(t *testing.T) {
	t.Parallel()
	// Arrange
	wd, err := os.Getwd()
	if err != nil {
		t.Fatalf("getwd: %v", err)
	}
	module := filepath.Dir(filepath.Dir(wd))
	staged := stageEntries(t, filepath.Join(wd, "..", "sandbox", "bin", "e2e-sandbox.sh"))

	// Act: resolve every local replace target of every go.mod in the module.
	var unstaged []string
	err = filepath.WalkDir(module, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() && d.Name() == "node_modules" {
			return filepath.SkipDir
		}
		if d.IsDir() || d.Name() != "go.mod" {
			return nil
		}
		data, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		for _, m := range localReplace.FindAllStringSubmatch(string(data), -1) {
			target := filepath.Join(filepath.Dir(path), m[1])
			rel, err := filepath.Rel(module, target)
			if err != nil || strings.HasPrefix(rel, "..") {
				unstaged = append(unstaged, path+" => "+m[1]+" (outside the module)")
				continue
			}
			if top := strings.Split(rel, string(filepath.Separator))[0]; !staged[top] {
				unstaged = append(unstaged, path+" => "+m[1]+" (top-level "+top+" is not staged)")
			}
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walk %s: %v", module, err)
	}
	if len(unstaged) > 0 {
		t.Fatalf("local Go replace targets the sandbox does not stage:\n  %s", strings.Join(unstaged, "\n  "))
	}
}

func TestStageEntriesReadsTheEnumeratedNames(t *testing.T) {
	t.Parallel()
	// Arrange
	script := filepath.Join(t.TempDir(), "entrypoint.sh")
	body := "before\nSTAGE_ENTRIES=(\n  # a comment\n  daemon\n  config.el\n\n)\nafter_entry\n"
	if err := os.WriteFile(script, []byte(body), 0o644); err != nil {
		t.Fatal(err)
	}

	// Act
	got := stageEntries(t, script)

	// Assert
	if len(got) != 2 || !got["daemon"] || !got["config.el"] {
		t.Fatalf("stageEntries = %v, want daemon and config.el", got)
	}
}

func TestStageFindExcludesAnExcludedDirectoryWhetherRealOrLinked(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, module, store string)
		want    bool
	}{
		{name: "a real node_modules directory is not staged", arrange: func(t *testing.T, module, _ string) {
			mustMkdir(t, filepath.Join(module, "webapp", "node_modules", "dep"))
		}},
		{name: "a node_modules linked into the host's node store is not staged", arrange: func(t *testing.T, module, store string) {
			if err := os.Symlink(store, filepath.Join(module, "webapp", "node_modules")); err != nil {
				t.Fatal(err)
			}
		}},
		{name: "a dist linked outside the webapp's own is not staged", arrange: func(t *testing.T, module, store string) {
			if err := os.Symlink(store, filepath.Join(module, "webapp", "src", "dist")); err != nil {
				t.Fatal(err)
			}
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			// Arrange
			module, store := t.TempDir(), t.TempDir()
			mustMkdir(t, filepath.Join(module, "webapp", "src"))
			if err := os.WriteFile(filepath.Join(module, "webapp", "src", "main.ts"), nil, 0o644); err != nil {
				t.Fatal(err)
			}
			tt.arrange(t, module, store)
			wd, err := os.Getwd()
			if err != nil {
				t.Fatal(err)
			}
			script := filepath.Join(wd, "..", "sandbox", "bin", "e2e-sandbox.sh")
			cmd := exec.Command("bash", "-c", `set -euo pipefail; . "$1"; STAGE_ENTRIES=(webapp); stage_find "$2" | tr '\0' '\n'`, "bash", script, module)

			// Act
			out, err := cmd.CombinedOutput()

			// Assert
			if err != nil {
				t.Fatalf("stage_find = %v\n%s", err, out)
			}
			listed := strings.Split(strings.TrimSpace(string(out)), "\n")
			if !slices.Contains(listed, "webapp/src/main.ts") {
				t.Fatalf("stage_find listed %q, want webapp/src/main.ts staged", listed)
			}
			for _, e := range listed {
				if strings.Contains(e, "node_modules") || strings.HasSuffix(e, "/dist") || strings.Contains(e, "/dist/") {
					t.Fatalf("stage_find listed %q, which staging excludes", e)
				}
			}
		})
	}
}

func mustMkdir(t *testing.T, dir string) {
	t.Helper()
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
}
