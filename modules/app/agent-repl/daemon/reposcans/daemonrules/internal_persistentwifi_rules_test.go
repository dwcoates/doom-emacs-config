package daemonrules

import (
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalPersistentwifiDir is the package these rules hold, under the daemon module's root.
const internalPersistentwifiDir = "internal/persistentwifi"

// TestTheStandingIsHeardNeverRepeated pins the re-read to the host's own
// change signals: a clock.Repeat beside them, or a second hand-rolled wait
// beside the failover's one retry timer, fails here.
func TestTheStandingIsHeardNeverRepeated(t *testing.T) {
	sourcescan.InPackage(t, internalPersistentwifiDir)
	// Act.
	repeats := sourcescan.Count(t, internalPersistentwifiDir, "clock.Repeat(")
	waits := sourcescan.Count(t, internalPersistentwifiDir, "Clock.After(") + sourcescan.Count(t, internalPersistentwifiDir, "clock.After(")

	// Assert.
	if repeats != 0 || waits != 1 {
		t.Fatalf("clock.Repeat( appears %d times and a clock .After( wait %d times; want no Repeat and only the failover retry's wait", repeats, waits)
	}
}

// TestTheNoHotspotFaultIsBuiltOnce pins the no-hotspot configuration fault to
// errNoHotspot: a step that spells its own fault beside it fails here.
func TestTheNoHotspotFaultIsBuiltOnce(t *testing.T) {
	sourcescan.InPackage(t, internalPersistentwifiDir)
	// Act.
	built := sourcescan.Count(t, internalPersistentwifiDir, "usersetup.Errorf(")

	// Assert.
	if built != 1 {
		t.Fatalf("usersetup.Errorf( appears %d times; want only errNoHotspot's", built)
	}
}
