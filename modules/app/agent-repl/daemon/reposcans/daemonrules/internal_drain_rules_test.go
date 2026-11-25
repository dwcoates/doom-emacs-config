package daemonrules

import (
	"fmt"
	"slices"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalDrainDir is the package these rules hold, under the daemon module's root.
const internalDrainDir = "internal/drain"

// TestEveryDrainReadIsLevelledThroughReadFailed keeps the cut-or-failed split
// in one place: no drain source but readFailed asks ctxend for it.
func TestEveryDrainReadIsLevelledThroughReadFailed(t *testing.T) {
	sourcescan.InPackage(t, internalDrainDir)
	// Act
	var sites []string
	for _, f := range sourcescan.Production(t, internalDrainDir) {
		for i, line := range strings.Split(string(f.Source), "\n") {
			if strings.Contains(line, "ctxend.") && !strings.Contains(line, "\"agent-repld/internal/ctxend\"") {
				sites = append(sites, fmt.Sprintf("%s:%d", f.Name, i+1))
			}
		}
	}

	// Assert
	if len(sites) != 1 || !strings.HasPrefix(sites[0], "controller.go:") {
		t.Fatalf("ctxend is asked at %v, want only inside readFailed", sites)
	}
}

// EVERY STAND-DOWN PATH GOES THROUGH THE ONE GATE: the shim is stood down,
// and the mode read, only in park. Each reason park is handed names its
// trigger and remote-control policy
// (drain.TestEveryStandDownReasonNamesItsTriggerAndPolicy), built from the
// eight standDownReason literals pinned here.
func TestEveryStandDownPathGoesThroughTheRemoteControlGate(t *testing.T) {
	// Act
	kills := sourcescan.Sites(t, internalDrainDir, "Stand.KillSession(")
	hibernates := sourcescan.Sites(t, internalDrainDir, "Stand.Hibernate(")
	modeReads := sourcescan.Sites(t, internalDrainDir, "DB.RemoteControl(")
	literals := sourcescan.Sites(t, internalDrainDir, "standDownReason{")
	parks := sourcescan.Sites(t, internalDrainDir, "c.park(")

	// Assert
	// controller.go's is the daemon's own immediate shutdown, which forces
	// every session down for its exit and is no trigger of the park.
	if !slices.Equal(kills, []string{"controller.go", "sweep.go"}) || !slices.Equal(hibernates, []string{"sweep.go"}) {
		t.Fatalf("KillSession sites = %v, Hibernate sites = %v; want the shutdown's and park's alone", kills, hibernates)
	}
	if !slices.Equal(modeReads, []string{"sweep.go"}) {
		t.Fatalf("remote-control reads = %v, want park's alone", modeReads)
	}
	if !slices.Equal(parks, []string{"cycle.go", "sweep.go", "withheld.go"}) {
		t.Fatalf("park sites = %v, want the cycle's, the sweep's and the withheld path's", parks)
	}
	if len(literals) != 8 {
		t.Fatalf("standDownReason literals = %v, want the eight allReasons builds from", literals)
	}
}
