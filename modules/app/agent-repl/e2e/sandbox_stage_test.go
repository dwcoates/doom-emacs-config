package e2e

import (
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
)

// The sandbox stages an ENUMERATED set of the module's top-level entries
// (STAGE_ENTRIES in sandbox/bin/entrypoint.sh), and every Go module inside
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
	module := filepath.Dir(wd)
	staged := stageEntries(t, filepath.Join(wd, "sandbox", "bin", "entrypoint.sh"))

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
