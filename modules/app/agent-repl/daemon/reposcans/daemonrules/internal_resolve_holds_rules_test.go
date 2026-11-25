package daemonrules

import (
	"regexp"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalResolveHoldsDir is the package these rules hold, under the daemon module's root.
const internalResolveHoldsDir = "internal/resolve/holds"

// THE TRAY ASKS holdfold.Seen whether a hold has a card, the reading the
// queue's fold and classifier share: no file of the tray reads a hold's
// origin as a daemon notice by hand.
func TestTrayAsksHoldfoldWhetherAHoldIsSeen(t *testing.T) {
	sourcescan.InPackage(t, internalResolveHoldsDir)
	// Arrange
	handRolled := regexp.MustCompile(`\bDaemonNotice(Named)?\([A-Za-z_.\[\]]*\.Origin\)`)

	// Act
	var found []string
	for _, file := range sourcescan.Production(t, internalResolveHoldsDir) {
		if handRolled.Match(file.Source) {
			found = append(found, file.Name)
		}
	}

	// Assert
	if len(found) > 0 {
		t.Fatalf("these files read a hold as a daemon notice by hand instead of holdfold.Seen: %v", found)
	}
}
