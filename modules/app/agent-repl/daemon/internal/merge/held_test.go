package merge

import (
	"context"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
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

// THE HOLD'S SET AND CLEAR ARE INFO RECORDS, so the shared log says when a
// merge was suspended at its round and when it ran on (2026-11-17: no record
// showed sidebar-roster-streams' fixes round held).
func TestAHoldRecordsItsSetAndItsClearAtInfo(t *testing.T) {
	tests := []struct {
		name string
		held bool
	}{
		{name: "the hold set", held: true},
		{name: "the hold cleared", held: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			h.o.holdRound(context.Background(), h.repoKey(), theWorkspace, "lease-1", tt.held)

			// Assert
			rec, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "info", Operation: opHold, Message: "recorded whether the merge's live round is held"})
			if !ok || rec.Context["held"] != tt.held || rec.Context["lease"] != "lease-1" || rec.Context["workspace"] != string(theWorkspace) {
				t.Fatalf("records = %+v, want the hold's write at INFO naming its workspace, lease and hold", h.logs.Records())
			}
		})
	}
}
