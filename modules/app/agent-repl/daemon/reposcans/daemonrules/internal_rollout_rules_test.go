package daemonrules

import (
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalRolloutDir is the package these rules hold, under the daemon module's root.
const internalRolloutDir = "internal/rollout"

// TestControllerLockIsReleasedOnlyThroughUnlock pins that no production file
// releases the controller's lock around unlock, which is the one place a
// change of ServesIntake's answer is signaled.
func TestControllerLockIsReleasedOnlyThroughUnlock(t *testing.T) {
	sourcescan.InPackage(t, internalRolloutDir)
	// Arrange
	files := sourcescan.Production(t, internalRolloutDir)

	for _, f := range files {
		t.Run(f.Name, func(t *testing.T) {
			// Act
			got := strings.Count(string(f.Source), "c.mu.Unlock()")

			// Assert: unlock itself holds the one direct release.
			want := 0
			if f.Name == "standing.go" {
				want = 1
			}
			if got != want {
				t.Fatalf("%s releases c.mu directly %d times, want %d; release it through c.unlock()", f.Name, got, want)
			}
		})
	}
}
