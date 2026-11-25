package daemonrules

import (
	"os"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalSlackwatchDir is the package these rules hold, under the daemon module's root.
const internalSlackwatchDir = "internal/slackwatch"

// TestTheDigestSubmitsThroughTheComposer holds the digest to the notifiers'
// one shape (briefprompt.TestEveryNotifierSubmitsThroughTheComposer): it
// submits through briefprompt.Composer, and never calls the prompt handler
// or mints a key itself. It reads its own package, which its unit may.
func TestTheDigestSubmitsThroughTheComposer(t *testing.T) {
	sourcescan.InPackage(t, internalSlackwatchDir)
	// Arrange
	raw, err := os.ReadFile("digest.go")
	if err != nil {
		t.Fatal(err)
	}
	source := string(raw)

	// Assert
	if !strings.Contains(source, "briefprompt.Composer{") {
		t.Error("digest.go does not submit through briefprompt.Composer")
	}
	if strings.Contains(source, "prompts.Submit(") || strings.Contains(source, ".MintKey(\"") {
		t.Error("digest.go composes or submits a brief prompt by hand")
	}
}
