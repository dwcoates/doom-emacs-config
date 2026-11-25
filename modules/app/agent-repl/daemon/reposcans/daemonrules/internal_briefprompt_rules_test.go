package daemonrules

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalBriefpromptDir is the package these rules hold, under the daemon module's root.
const internalBriefpromptDir = "internal/briefprompt"

// TestEveryNotifierSubmitsThroughTheComposer holds the notifiers to the one
// shape: each submits its brief prompts through Composer, and none calls the
// prompt handler itself, so a hand-rolled site fails here. Slack's digest is
// held to the same shape by slackwatch's own TestTheDigestSubmitsThroughTheComposer,
// since this unit may read only the files the test runner's roster grants it.
func TestEveryNotifierSubmitsThroughTheComposer(t *testing.T) {
	sourcescan.InPackage(t, internalBriefpromptDir)
	for _, file := range []string{
		"../mergenotice/mergenotice.go",
		"../workspacesubs/workspacesubs.go",
		"../workspacenotice/workspacenotice.go",
		"../prwatch/poller.go",
		"../mainsync/mainsync.go",
		"../gather/gather.go",
	} {
		t.Run(filepath.Base(file), func(t *testing.T) {
			// Arrange
			raw, err := os.ReadFile(file)
			if err != nil {
				t.Fatal(err)
			}
			source := string(raw)

			// Assert
			if !strings.Contains(source, "briefprompt.Composer{") {
				t.Errorf("%s does not submit through briefprompt.Composer", file)
			}
			if strings.Contains(source, "prompts.Submit(") || strings.Contains(source, ".MintKey(\"") {
				t.Errorf("%s composes or submits a brief prompt by hand", file)
			}
		})
	}
}
