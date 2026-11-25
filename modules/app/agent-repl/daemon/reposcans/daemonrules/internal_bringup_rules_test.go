package daemonrules

import (
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalBringupDir is the package these rules hold, under the daemon module's root.
const internalBringupDir = "internal/bringup"

func TestTheStandDownClassificationHasOneHome(t *testing.T) {
	sourcescan.InPackage(t, internalBringupDir)
	// Assert: the three stand-down errors are tested together only inside
	// IsStandDown, so no start site in this package classifies its own way.
	if got := sourcescan.Sites(t, internalBringupDir, "errors.Is(err, ErrNotServed)"); !slices.Equal(got, []string{"bringup.go"}) {
		t.Fatalf("ErrNotServed classified at %v, want only IsStandDown in bringup.go", got)
	}
	if got := sourcescan.Count(t, internalBringupDir, "IsStandDown(err)"); got < 1 {
		t.Fatalf("IsStandDown(err) called %d times in bringup, want the bring-up's own start to use it", got)
	}
}

// TestEveryBringUpMarkerRaiseGoesThroughRaiseMarkers pins that no boot or
// takeover raises the bring-up marker by hand: each raise goes through
// RaiseMarkers, so a start that runs no shim is never marked.
func TestEveryBringUpMarkerRaiseGoesThroughRaiseMarkers(t *testing.T) {
	sourcescan.InPackage(t, internalBringupDir)
	// Arrange
	handRaise := regexp.MustCompile(`BringingUp\([^)]*,\s*true\)`)
	var offenders []string
	for _, dir := range []string{"../boot", "../rollout"} {
		files, err := filepath.Glob(filepath.Join(dir, "*.go"))
		if err != nil {
			t.Fatalf("Glob(%s): %v", dir, err)
		}
		if len(files) == 0 {
			t.Fatalf("no Go files under %s: the scan read nothing", dir)
		}
		for _, file := range files {
			if strings.HasSuffix(file, "_test.go") {
				continue
			}
			src, err := os.ReadFile(file)
			if err != nil {
				t.Fatalf("ReadFile(%s): %v", file, err)
			}
			// Act
			if handRaise.Match(src) {
				offenders = append(offenders, file)
			}
		}
	}

	// Assert
	if len(offenders) != 0 {
		t.Fatalf("files raising the bring-up marker by hand = %v, want every raise through bringup.RaiseMarkers", offenders)
	}
}
