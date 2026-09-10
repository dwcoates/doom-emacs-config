package harness

import (
	"path/filepath"
	"strings"
	"testing"

	"claude-repld/internal/stateroot"
)

// TestShortTempDirYieldsASocketPathBudgetEvenUnderARidiculouslyLongTestNameThatWouldOtherwiseOverflowTheUnixDomainSocketLimit
// is the harness's own guard, and ITS NAME IS THE FIXTURE. t.TempDir() spells
// the whole test name into the directory it hands back, so a state root taken
// from it under macOS's per-user /var/folders temp root overflows the 103-byte
// sun_path budget and the daemon refuses to boot — which is why four tests in
// this suite used to pass only under a TMPDIR=/tmp override. ShortTempDir's
// length must be fixed and independent of the name, so this name is made long
// enough that any name-derived root would fail, and the assertion is the same
// budget check StartDaemon runs.
func TestShortTempDirYieldsASocketPathBudgetEvenUnderARidiculouslyLongTestNameThatWouldOtherwiseOverflowTheUnixDomainSocketLimit(t *testing.T) {
	t.Parallel()
	// Arrange
	dir := ShortTempDir(t)

	// Act
	layout, err := stateroot.Root(filepath.Join(dir, "state"), "")
	if err != nil {
		t.Fatalf("stateroot.Root(%q) = %v, want a resolved layout", dir, err)
	}

	// Assert
	if err := layout.CheckSocketPathBudget(); err != nil {
		t.Fatalf("CheckSocketPathBudget on a state root under ShortTempDir = %v, want a state root that fits a shim socket whatever the test is called", err)
	}
	if strings.Contains(dir, "ShortTempDir") {
		t.Fatalf("ShortTempDir = %q, want a path that never encodes the test's name", dir)
	}
}
