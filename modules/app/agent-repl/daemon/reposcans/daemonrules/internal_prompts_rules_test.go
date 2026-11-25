package daemonrules

import (
	"os"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalPromptsDir is the package these rules hold, under the daemon module's root.
const internalPromptsDir = "internal/prompts"

func TestEveryBriefWordingPinAssertsThroughUnsaid(t *testing.T) {
	sourcescan.InPackage(t, internalPromptsDir)
	// Arrange: a pin that hand-rolls its own phrase check drifts from the
	// rest, so none may.
	src, err := os.ReadFile("api_test.go")
	if err != nil {
		t.Fatalf("reading api_test.go: %v", err)
	}
	handRolled := "!strings.Contains(got" + ".Body"

	// Act.
	n := strings.Count(string(src), handRolled)

	// Assert.
	if n != 0 {
		t.Fatalf("api_test.go hand-rolls %d phrase checks on a brief's body; assert through unsaid", n)
	}
}
