package worktreereap

import (
	"testing"

	"claude-repld/internal/sourcescan"
)

// TestTheScheduleIsClockRepeat pins this package's periodic loop to the one
// shared schedule: a hand-rolled wait-and-run loop beside it fails here.
func TestTheScheduleIsClockRepeat(t *testing.T) {
	// Act.
	repeats := sourcescan.Count(t, "clock.Repeat(")
	waits := sourcescan.Count(t, "Clock.After(") + sourcescan.Count(t, "clock.After(")

	// Assert.
	if repeats != 1 || waits != 0 {
		t.Fatalf("clock.Repeat( appears %d times and a hand-rolled .After( wait %d times; want one Repeat and no own wait", repeats, waits)
	}
}
