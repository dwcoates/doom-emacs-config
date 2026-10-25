package e2e

import (
	"os"
	"strings"
	"testing"
)

// TestTheEmacsWorldIsolatesItsDaemonFromTheOwnersZettel pins the Emacs world's
// launcher (emacs_e2e_test.go) to the harness's two zettel statements: no
// world runs a real zettel, so its daemon dials an unserved socket of the
// world's own (harness.UnservedZettelEnv), and the staged placeholder's build
// is stated so a deploy restarts no zettel (DeployBuilder.StatePlaceholderBuilds).
func TestTheEmacsWorldIsolatesItsDaemonFromTheOwnersZettel(t *testing.T) {
	// Arrange
	raw, err := os.ReadFile("emacs_e2e_test.go")
	if err != nil {
		t.Fatalf("read emacs_e2e_test.go: %v", err)
	}
	source := string(raw)

	for _, call := range []string{"harness.UnservedZettelEnv(", ".StatePlaceholderBuilds("} {
		t.Run(call, func(t *testing.T) {
			// Assert
			if !strings.Contains(source, call) {
				t.Fatalf("emacs_e2e_test.go makes no %s call", call)
			}
		})
	}
}
