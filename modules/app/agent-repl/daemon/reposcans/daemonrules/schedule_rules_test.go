package daemonrules

import (
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// TestTheScheduleIsClockRepeat pins each periodic package's loop to the one
// shared schedule: a hand-rolled wait-and-run loop beside it fails here.
func TestTheScheduleIsClockRepeat(t *testing.T) {
	for _, dir := range []string{internalColdbacklogDir, internalRepodivergenceDir, internalWorktreereapDir} {
		t.Run(dir, func(t *testing.T) {
			// Act.
			repeats := sourcescan.Count(t, dir, "clock.Repeat(")
			waits := sourcescan.Count(t, dir, "Clock.After(") + sourcescan.Count(t, dir, "clock.After(")

			// Assert.
			if repeats != 1 || waits != 0 {
				t.Fatalf("clock.Repeat( appears %d times and a hand-rolled .After( wait %d times; want one Repeat and no own wait", repeats, waits)
			}
		})
	}
}
