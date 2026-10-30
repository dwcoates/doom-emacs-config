package workspace

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/internal/prompts"
)

// shippedPromptsDir is the module's own brief corpus, as seen from this
// package's directory.
var shippedPromptsDir = filepath.Join("..", "..", "..", "prompts")

// loadShippedBrief loads the brief named name from the shipped corpus, failing
// the test when it cannot be read.
func loadShippedBrief(t *testing.T, name string) prompts.Prompt {
	t.Helper()
	brief, err := prompts.Load(shippedPromptsDir, name)
	if err != nil {
		t.Fatalf("Load %s: %v", name, err)
	}
	return brief
}

func TestLoadShippedBriefReadsTheShippedCorpus(t *testing.T) {
	// Arrange.
	want, err := os.ReadFile(filepath.Join(shippedPromptsDir, BriefCreateFailed+".md"))
	if err != nil {
		t.Fatalf("ReadFile: %v", err)
	}

	// Act.
	brief := loadShippedBrief(t, BriefCreateFailed)

	// Assert.
	if brief.Name != BriefCreateFailed {
		t.Fatalf("Name = %q, want %q", brief.Name, BriefCreateFailed)
	}
	if !strings.Contains(string(want), brief.Body) {
		t.Fatalf("Body = %q, want a slice of the shipped file", brief.Body)
	}
}

// TestEveryShippedBriefLoadGoesThroughLoadShippedBrief keeps the package's
// tests from hand-rolling the shipped corpus's path again.
func TestEveryShippedBriefLoadGoesThroughLoadShippedBrief(t *testing.T) {
	// Arrange.
	files, err := filepath.Glob("*_test.go")
	if err != nil {
		t.Fatalf("Glob: %v", err)
	}
	handRolled := `filepath.Join("..", "..", "..", "prompts")`

	for _, file := range files {
		t.Run(file, func(t *testing.T) {
			// Act.
			text, err := os.ReadFile(file)
			if err != nil {
				t.Fatalf("ReadFile: %v", err)
			}

			// Assert.
			count := strings.Count(string(text), handRolled)
			if file == "shippedbrief_test.go" {
				count -= 2 // the helper's own definition and this check
			}
			if count != 0 {
				t.Fatalf("%s spells the shipped corpus's path %d times, want loadShippedBrief or shippedPromptsDir", file, count)
			}
		})
	}
}
