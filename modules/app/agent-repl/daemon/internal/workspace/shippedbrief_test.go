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

// handedFailuresSection is the metaprompt section a brief handing over a
// failure points at for how the fix is dispatched.
const handedFailuresSection = "Handed failures"

// assertBriefHandsTheFixToAChild fails unless body points at the metaprompt's
// handed-failures rule and never asks the workspace to propose a fix and wait.
func assertBriefHandsTheFixToAChild(t *testing.T, body string) {
	t.Helper()
	if !strings.Contains(body, `"`+handedFailuresSection+`"`) {
		t.Fatalf("brief = %q, want it to point at the metaprompt's %q section", body, handedFailuresSection)
	}
	if !strings.Contains(body, "one-shot child workspace") {
		t.Fatalf("brief = %q, want it to dispatch the fix as a one-shot child workspace", body)
	}
	for _, waiting := range []string{"until I answer", "wait for my answer", "propose a fix", "propose a change", "Offer to make"} {
		if strings.Contains(body, waiting) {
			t.Fatalf("brief = %q, want no ask-and-wait wording, found %q", body, waiting)
		}
	}
}
