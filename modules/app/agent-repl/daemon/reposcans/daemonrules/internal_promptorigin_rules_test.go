package daemonrules

import (
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalPromptoriginDir is the package these rules hold, under the daemon module's root.
const internalPromptoriginDir = "internal/promptorigin"

// THE SENDER RULE IS WRITTEN ONCE: every layer carrying a prompt's sender asks
// CheckSender, so a hand-rolled comparison of an origin against
// PROMPT_ORIGIN_AGENT_SENT beside a sender fails here.
func TestEverySenderCheckIsCheckSender(t *testing.T) {
	sourcescan.InPackage(t, internalPromptoriginDir)
	// Arrange
	handRolled := regexp.MustCompile(`PROMPT_ORIGIN_AGENT_SENT\)\s*!=\s*\(`)
	var found []string

	// Act
	err := filepath.WalkDir("../..", func(path string, d fs.DirEntry, err error) error {
		if err != nil || d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return err
		}
		raw, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if handRolled.Match(raw) && !strings.HasSuffix(path, "promptorigin.go") {
			found = append(found, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walk the daemon's source: %v", err)
	}
	if len(found) > 0 {
		t.Fatalf("these files compare an origin to PROMPT_ORIGIN_AGENT_SENT by hand instead of asking CheckSender: %v", found)
	}
}

// A STORED ORIGIN IS READ IN ONE PLACE: every layer reading the spelling a
// hold or a turn stores asks Named, so a hand-rolled lookup in
// conversation.v1.PromptOrigin_value outside this package fails here.
func TestEveryStoredOriginIsReadByNamed(t *testing.T) {
	sourcescan.InPackage(t, internalPromptoriginDir)
	// Arrange
	handRolled := regexp.MustCompile(`PromptOrigin\(conversationv1\.PromptOrigin_value\[`)
	var found []string

	// Act
	err := filepath.WalkDir("../..", func(path string, d fs.DirEntry, err error) error {
		if err != nil || d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return err
		}
		raw, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if handRolled.Match(raw) && !strings.HasSuffix(path, "promptorigin.go") {
			found = append(found, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walk the daemon's source: %v", err)
	}
	if len(found) > 0 {
		t.Fatalf("these files read a stored origin by hand instead of asking Named: %v", found)
	}
}
