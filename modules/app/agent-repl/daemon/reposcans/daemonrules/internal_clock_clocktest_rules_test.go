package daemonrules

import (
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalClockClocktestDir is the package these rules hold, under the daemon module's root.
const internalClockClocktestDir = "internal/clock/clocktest"

// TestTheMigratedSuitesShareTheScriptedClock pins the suites that drive a
// periodic schedule to this one clock: a hand-rolled After beside them fails
// here rather than drifting from Scripted.
func TestTheMigratedSuitesShareTheScriptedClock(t *testing.T) {
	sourcescan.InPackage(t, internalClockClocktestDir)
	for _, dir := range []string{"..", "../../worktreereap", "../../newsdigest", "../../repodivergence"} {
		files, err := filepath.Glob(filepath.Join(dir, "*_test.go"))
		if err != nil || len(files) == 0 {
			t.Fatalf("globbing %s's tests: %v (%d files)", dir, err, len(files))
		}
		for _, file := range files {
			// Arrange.
			source, err := os.ReadFile(file)
			if err != nil {
				t.Fatalf("reading %s: %v", file, err)
			}

			// Act.
			handRolled := strings.Contains(string(source), ") After(d time.Duration) <-chan time.Time")

			// Assert.
			if handRolled {
				t.Errorf("%s declares its own After; use clocktest.Scripted", file)
			}
		}
	}
}

// TestOptionalInstantComparisonsHaveOneHome fails when a test file of the
// daemon hand-rolls its own comparison of two optional instants instead of
// calling SameInstant.
func TestOptionalInstantComparisonsHaveOneHome(t *testing.T) {
	sourcescan.InPackage(t, internalClockClocktestDir)
	// Arrange.
	root := filepath.Join("..", "..", "..")
	shape := regexp.MustCompile(`func \w+\(\w+, \w+ \*time\.Time\) bool`)
	var sites []string

	// Act.
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil || d.IsDir() || !strings.HasSuffix(path, ".go") {
			return err
		}
		source, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		for range shape.FindAll(source, -1) {
			sites = append(sites, strings.TrimPrefix(path, root+string(filepath.Separator)))
		}
		return nil
	})

	// Assert.
	if err != nil {
		t.Fatal(err)
	}
	if want := []string{filepath.Join("internal", "clock", "clocktest", "clocktest.go")}; !slices.Equal(sites, want) {
		t.Fatalf("optional instants are compared at %v, want only SameInstant in %v", sites, want)
	}
}
