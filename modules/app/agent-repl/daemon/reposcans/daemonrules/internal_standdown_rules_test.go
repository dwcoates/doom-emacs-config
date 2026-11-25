package daemonrules

import (
	"os"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalStanddownDir is the package these rules hold, under the daemon module's root.
const internalStanddownDir = "internal/standdown"

// TestStandDownSitesShareCutShort fails a site that hand-rolls the stand-down
// test instead of asking CutShort: CutShort's body is the one place the
// lifetime's end is read for these callers.
func TestStandDownSitesShareCutShort(t *testing.T) {
	sourcescan.InPackage(t, internalStanddownDir)
	cases := []string{
		"../desktopnotify/notifier.go",
		"../desktopnotify/summary.go",
		"../mergeworkspace/mergeworkspace.go",
	}
	for _, file := range cases {
		t.Run(file, func(t *testing.T) {
			// Arrange
			src, err := os.ReadFile(file)
			if err != nil {
				t.Fatalf("read %s: %v", file, err)
			}

			// Act
			asks := strings.Count(string(src), "standdown.CutShort(")
			reads := strings.Count(string(src), "ctx.Err() != nil")

			// Assert
			if asks == 0 || reads != 0 {
				t.Fatalf("%s: standdown.CutShort( x%d, ctx.Err() != nil x%d (want 0)", file, asks, reads)
			}
		})
	}
}
