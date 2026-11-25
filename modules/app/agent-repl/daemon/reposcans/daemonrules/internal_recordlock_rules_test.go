package daemonrules

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalRecordlockDir is the package these rules hold, under the daemon module's root.
const internalRecordlockDir = "internal/recordlock"

// Every lock file this package opens goes through openLockFile, the one open
// OpenFile and OpenExisting share: a hand-rolled open elsewhere is a drifted
// copy.
func TestEveryLockFileOpenGoesThroughOpenFile(t *testing.T) {
	sourcescan.InPackage(t, internalRecordlockDir)
	body, err := os.ReadFile("recordlock.go")
	if err != nil {
		t.Fatalf("read recordlock.go: %v", err)
	}
	lines := strings.Split(string(body), "\n")
	for i, line := range lines {
		if strings.Contains(line, "os.OpenFile(") && !strings.Contains(lines[max(i-1, 0)], "func openLockFile") {
			t.Errorf("recordlock.go:%d opens a file by hand; use OpenFile or OpenExisting: %s", i+1, strings.TrimSpace(line))
		}
	}
}

// TestEveryDaemonKernelLockGoesThroughThisPackage pins the call sites to the
// one primitive: a production file of the daemon that takes a kernel lock of
// its own fails here. The one exception is sessionlock's PROBE of the flock a
// shim's holder process takes (shim-lock), which is that process's contract
// and is only ever probed here, never held.
func TestEveryDaemonKernelLockGoesThroughThisPackage(t *testing.T) {
	sourcescan.InPackage(t, internalRecordlockDir)
	// Arrange.
	allowed := map[string]bool{
		filepath.Join("internal", "recordlock", "recordlock.go"): true,
		filepath.Join("internal", "sessionlock", "api.go"):       true,
	}
	root := filepath.Join("..", "..")
	var offenders []string

	// Act.
	err := filepath.WalkDir(root, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() && (d.Name() == "integration" || d.Name() == "testdata") {
			return filepath.SkipDir
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		body, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		rel, _ := filepath.Rel(root, path)
		s := string(body)
		if (strings.Contains(s, "Flock(") || strings.Contains(s, "F_SETLK")) && !allowed[rel] {
			offenders = append(offenders, rel)
		}
		return nil
	})

	// Assert.
	if err != nil {
		t.Fatalf("walking the daemon's sources: %v", err)
	}
	if len(offenders) != 0 {
		t.Fatalf("kernel locks taken outside recordlock in %v; take recordlock.Hold or recordlock.TryExclusive", offenders)
	}
}
