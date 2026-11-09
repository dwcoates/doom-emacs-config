package persistentwifi

import (
	"testing"

	"agent-repld/internal/sourcescan"
)

// TestTheStandingIsHeardNeverRepeated pins the re-read to the host's own
// change signals: a clock.Repeat beside them, or a second hand-rolled wait
// beside the failover's one retry timer, fails here.
func TestTheStandingIsHeardNeverRepeated(t *testing.T) {
	// Act.
	repeats := sourcescan.Count(t, "clock.Repeat(")
	waits := sourcescan.Count(t, "Clock.After(") + sourcescan.Count(t, "clock.After(")

	// Assert.
	if repeats != 0 || waits != 1 {
		t.Fatalf("clock.Repeat( appears %d times and a clock .After( wait %d times; want no Repeat and only the failover retry's wait", repeats, waits)
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
