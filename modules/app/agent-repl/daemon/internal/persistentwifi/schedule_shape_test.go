package persistentwifi

import (
	"testing"

	"agent-repld/internal/sourcescan"
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

// TestTheNoHotspotFaultIsBuiltOnce pins the no-hotspot configuration fault to
// errNoHotspot: a step that spells its own fault beside it fails here.
func TestTheNoHotspotFaultIsBuiltOnce(t *testing.T) {
	// Act.
	built := sourcescan.Count(t, "usersetup.Errorf(")

	// Assert.
	if built != 1 {
		t.Fatalf("usersetup.Errorf( appears %d times; want only errNoHotspot's", built)
	}
}
