package cachelapse

import (
	"context"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

func TestScheduleBySmallContext(t *testing.T) {
	tests := []struct {
		name   string
		tokens int64
		queued bool
		armed  bool
	}{
		{name: "a context one token above the line is kept warm", tokens: SmallContextTokens + 1, armed: true},
		{name: "a context at the line owes no keep-alive", tokens: SmallContextTokens},
		{name: "a context well below the line owes no keep-alive", tokens: 1000},
		{name: "a context the vendor has not reported is kept warm", tokens: 0, armed: true},
		{name: "a small context owes none even with a merge queued", tokens: SmallContextTokens, queued: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t, running(0))
			h.contextTokens = tt.tokens
			if tt.queued {
				h.db.queued = []ids.WorkspaceID{theWorkspace}
			}

			// Act
			h.keeper.schedule(context.Background(), theWorkspace, "test")

			// Assert
			if _, armed := h.keeper.beats[theWorkspace]; armed != tt.armed {
				t.Fatalf("beat armed = %v, want %v", armed, tt.armed)
			}
		})
	}
}

func TestASmallContextStatesWhyItOwesNoKeepaliveAndMarksNothingCold(t *testing.T) {
	// Arrange
	h := newHarness(t, running(0))
	h.contextTokens = 50_000

	// Act
	h.keeper.schedule(context.Background(), theWorkspace, "test")

	// Assert
	record, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "info", Operation: opDrive, Message: "the workspace owes no keep-alive"})
	why, _ := record.Context["why"].(string)
	if !ok || !strings.Contains(why, "50000 tokens") || record.Context["workspace"] != string(theWorkspace) {
		t.Fatalf("records = %+v, want the small context's reason for the workspace", h.log.Records())
	}
	if h.db.coldMarks != 0 {
		t.Fatalf("cold marks = %d, want none: a small context is left to lapse, not marked cold", h.db.coldMarks)
	}
}

func TestNewRefusesNoContextSizes(t *testing.T) {
	// Arrange
	h := newHarness(t, running(0))
	deps := h.keeper.deps
	deps.ContextTokens = nil

	// Act
	_, err := New(deps)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "context sizes") {
		t.Fatalf("err = %v, want the missing context sizes refused", err)
	}
}
