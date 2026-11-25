package daemonrules

import (
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalHeadlessDir is the package these rules hold, under the daemon module's root.
const internalHeadlessDir = "internal/headless"

// TestNoHandRolledValidatedAskLoop fails on a daemon file that loops over
// Headless.Run attempts itself instead of calling AskValidated. The workspace
// naming call is the one exception: it rewrites its question between
// attempts (the shorten brief), which AskValidated's one Request cannot.
func TestNoHandRolledValidatedAskLoop(t *testing.T) {
	sourcescan.InPackage(t, internalHeadlessDir)
	root := filepath.Join("..", "..")
	loop := regexp.MustCompile(`for attempt := 1; attempt <=`)
	allowed := map[string]bool{filepath.Join("internal", "workspace", "namecall.go"): true, filepath.Join("internal", "headless", "ask.go"): true}
	err := filepath.WalkDir(root, func(path string, d os.DirEntry, err error) error {
		if err != nil || d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return err
		}
		rel, _ := filepath.Rel(root, path)
		body, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if loop.Match(body) && strings.Contains(string(body), ".Run(ctx, headless.Request") && !allowed[rel] {
			t.Errorf("%s loops over headless attempts by hand; call headless.AskValidated", rel)
		}
		return nil
	})
	if err != nil {
		t.Fatal(err)
	}
}

// TestShortAnswersValidateThroughWords fails on a short-answer validator that
// trims and counts words by hand instead of through Words.
func TestShortAnswersValidateThroughWords(t *testing.T) {
	sourcescan.InPackage(t, internalHeadlessDir)
	for _, file := range []string{"../chat/abbreviate.go"} {
		t.Run(file, func(t *testing.T) {
			body, err := os.ReadFile(file)
			if err != nil {
				t.Fatal(err)
			}
			if !strings.Contains(string(body), "headless.Words{") || strings.Contains(string(body), "strings.Fields(a)") {
				t.Fatalf("%s validates its short answer by hand; use headless.Words", file)
			}
		})
	}
}
