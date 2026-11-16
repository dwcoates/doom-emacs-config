package merge

import (
	"strings"
	"testing"

	"agent-repld/internal/sourcescan"
)

// THE ROUND'S HOLD HAS ONE WRITER (holdRound): a second caller of the
// store's hold could mark a round held that no resume record waits on.
func TestOnlyHoldRoundWritesARoundsHold(t *testing.T) {
	// Act
	var callers []string
	for _, file := range sourcescan.Production(t) {
		if strings.Contains(string(file.Source), ".HoldMergeRound(") && !strings.HasSuffix(file.Name, "held.go") {
			callers = append(callers, file.Name)
		}
	}

	// Assert
	if len(callers) != 0 {
		t.Fatalf("files calling HoldMergeRound besides held.go: %v", callers)
	}
}
