package daemonrules

import (
	"os"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalWorkspacesubsDir is the package these rules hold, under the daemon module's root.
const internalWorkspacesubsDir = "internal/workspacesubs"

func TestBothNotifiersShareOneSupersession(t *testing.T) {
	sourcescan.InPackage(t, internalWorkspacesubsDir)
	// Arrange: the supersede-and-carry decision lives in internal/untoldnotice
	// alone; a notifier that hand-rolled its own would call Supersede itself.
	var calls int
	for _, path := range []string{"workspacesubs.go", "../mergenotice/mergenotice.go"} {
		src, err := os.ReadFile(path)
		if err != nil {
			t.Fatalf("read %s: %v", path, err)
		}

		// Act
		if strings.Contains(string(src), ".Supersede(") {
			t.Errorf("%s calls Supersede itself; take untoldnotice.Carried", path)
		}
		calls += strings.Count(string(src), "untoldnotice.Carried(")
	}

	// Assert
	if calls != 2 {
		t.Fatalf("untoldnotice.Carried calls = %d, want one per notifier", calls)
	}
}
