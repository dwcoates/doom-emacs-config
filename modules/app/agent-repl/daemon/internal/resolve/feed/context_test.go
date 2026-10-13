package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/ids"
)

// HOW FULL THE CONTEXT WAS BEFORE A PROMPT (context.go): the newest main-agent
// reading placed before the prompt's row and after the newest context cut.

// usageAt sends one main-agent unit stating USAGE, its entry placed at AT_MS.
func (h *harness) usageAt(unit string, usage *conversationv1.TokenUsage, atMs int64) {
	h.t.Helper()
	h.sendAt(thinkingFrame(unit, usage), atMs)
}

// contextOf answers the context figure RollbackChoices states for TURN's
// prompt, and whether one stands.
func (h *harness) contextOf(turn string) (uint64, bool) {
	h.t.Helper()
	for _, choice := range h.resolver.RollbackChoices(testWorkspace) {
		if choice.Turn == ids.TurnID(turn) {
			if choice.ContextTokens == nil {
				return 0, false
			}
			return *choice.ContextTokens, true
		}
	}
	h.t.Fatalf("RollbackChoices() holds no prompt of turn %s", turn)
	return 0, false
}

func TestContextTokensOfCountsEveryInputBucketAndNoOutput(t *testing.T) {
	tests := []struct {
		name  string
		usage *conversationv1.TokenUsage
		want  uint64
	}{
		{name: "a nil usage is zero", usage: nil, want: 0},
		{name: "cache reads count", usage: turnUsage(0, 0, 90_000, 0), want: 90_000},
		{name: "cache writes count", usage: turnUsage(0, 4_000, 0, 0), want: 4_000},
		{name: "uncached input counts", usage: turnUsage(7, 0, 0, 0), want: 7},
		{name: "output never counts", usage: turnUsage(0, 0, 0, 1_200), want: 0},
		{name: "every input bucket together", usage: turnUsage(7, 4_000, 90_000, 1_200), want: 94_007},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := contextTokensOf(tc.usage)

			// Assert
			if got != tc.want {
				t.Fatalf("contextTokensOf() = %d, want %d", got, tc.want)
			}
		})
	}
}

func TestAPromptsContextIsTheNewestReadingPlacedBeforeIt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPromptAt("t1", "first", 1_000)
	h.usageAt("u1", turnUsage(5, 10, 100, 7), 1_010)
	h.usageAt("u2", turnUsage(5, 20, 200, 7), 1_020)
	h.deliverPromptAt("t2", "second", 1_100)

	// Act
	got, ok := h.contextOf("t2")

	// Assert
	if !ok || got != 225 {
		t.Fatalf("context before t2 = %d (stated %v), want 225", got, ok)
	}
}

func TestTheFirstPromptHasNoContextFigure(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPromptAt("t1", "first", 1_000)
	h.usageAt("u1", turnUsage(5, 10, 100, 7), 1_010)

	// Act
	_, ok := h.contextOf("t1")

	// Assert
	if ok {
		t.Fatal("the first prompt states a context figure, want none")
	}
}

func TestAReadingArrivingLateIsOrderedByItsPlace(t *testing.T) {
	// Arrange: the earlier response's reading arrives after the later one, as
	// a history page loaded newest first delivers it.
	h := newHarness(t)
	h.deliverPromptAt("t1", "first", 1_000)
	h.usageAt("u-late", turnUsage(0, 0, 500, 0), 1_050)
	h.usageAt("u-early", turnUsage(0, 0, 300, 0), 1_010)
	h.deliverPromptAt("t2", "second", 1_100)

	// Act
	got, _ := h.contextOf("t2")

	// Assert
	if got != 500 {
		t.Fatalf("context before t2 = %d, want the later-placed 500", got)
	}
}

func TestARestatedUnitReplacesItsReading(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPromptAt("t1", "first", 1_000)
	h.usageAt("u1", turnUsage(0, 0, 100, 0), 1_010)
	h.usageAt("u1", turnUsage(0, 0, 150, 0), 1_010)
	h.deliverPromptAt("t2", "second", 1_100)

	// Act
	got, _ := h.contextOf("t2")

	// Assert
	if got != 150 {
		t.Fatalf("context before t2 = %d, want the restated 150", got)
	}
}

func TestAReadingBeforeAContextCutIsNotTaken(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPromptAt("t1", "first", 1_000)
	h.usageAt("u1", turnUsage(0, 0, 100, 0), 1_010)
	h.cutPlaced("cut-1", compactedCut("summary"), 1_050)
	h.deliverPromptAt("t2", "second", 1_100)

	// Act
	_, ok := h.contextOf("t2")

	// Assert
	if ok {
		t.Fatal("the prompt after a compaction states the pre-cut figure, want none")
	}
}

func TestARolledBackTurnsReadingIsNotTaken(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPromptAt("t1", "first", 1_000)
	h.usageAt("u1", turnUsage(0, 0, 100, 0), 1_010)
	h.deliverPromptAt("t2", "second", 1_100)
	h.usageAt("u2", turnUsage(0, 0, 900, 0), 1_110)
	if err := h.resolver.RollBackTurns(testWorkspace, []ids.TurnID{"t2"}); err != nil {
		t.Fatalf("RollBackTurns: %v", err)
	}
	h.deliverPromptAt("t3", "third", 1_200)

	// Act
	got, _ := h.contextOf("t3")

	// Assert
	if got != 100 {
		t.Fatalf("context before t3 = %d, want t1's 100, not the rolled-back 900", got)
	}
}

func TestASubagentsUsageFilesNoReading(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPromptAt("t1", "first", 1_000)
	h.spawnSubagent("spawn-1", subAgent(), "Explore", "look")
	h.resolver.OnActivity(testWorkspace, subAgent(), thinkingFrame("u-sub", turnUsage(0, 0, 999, 0)), nil, placeAt(1_010))

	// Act
	h.resolver.mu.Lock()
	_, filed := h.resolver.state(testWorkspace).contextReadings["u-sub"]
	h.resolver.mu.Unlock()

	// Assert
	if filed {
		t.Fatal("a subagent's usage filed a context reading, want none")
	}
}

func TestAnUnplacedUsageFilesNoReadingAndSaysSo(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPromptAt("t1", "first", 1_000)

	// Act
	h.sendAs(mainAgent(), thinkingFrame("u-unplaced", turnUsage(0, 0, 100, 0)))

	// Assert
	h.resolver.mu.Lock()
	_, filed := h.resolver.state(testWorkspace).contextReadings["u-unplaced"]
	h.resolver.mu.Unlock()
	if filed {
		t.Fatal("an unplaced usage filed a context reading, want none")
	}
	if !h.hasRecord("debug", "daemon.feed.context_reading_unplaced") {
		t.Fatalf("records = %+v, want the unplaced reading recorded", h.records())
	}
}
