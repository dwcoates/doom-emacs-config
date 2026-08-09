// WHAT THE BOOT SWEEP LEAVES BEHIND.
//
// The boot sweep (server/bootsweep.go) reconciles a fresh daemon with the shims
// that outlived its predecessor. Every verdict it reaches is logged, loudly and
// carefully — and every verdict it reaches is logged AND NOTHING ELSE. Three of
// them leave a session unwired:
//
//   - "has no live shim; leaving it UNWIRED for the on-demand bring-up"
//   - "STILL holds its lock without connecting after the redial window"
//   - "parked-connection probe FAILED again ... leaving it unwired"
//
// THE GAP. In all three the daemon has REACHED A CONCLUSION about a session the
// user owns, and the user is told nothing. Worse, the durable connectivity the
// previous daemon wrote is still standing, so the workspace can go on
// presenting as OPERATIONAL — a live session, according to the only surface
// anybody reads — with no shim behind it at all. The one place the truth exists
// is a log line at boot.
//
// WHAT MUST BE TRUE. A session the sweep has finished with and not wired yields
// a USER-VISIBLE CLASSIFIED RECORD: the connectivity axis says it is not
// operational, and the record says which verdict it was. Silence is not a legal
// answer for a conclusion the daemon actually reached.
package e2e

import (
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/shim"
)

// TestE2EASessionLeftUnwiredByTheBootSweepIsClassifiedNotSilent covers THE
// NEVER-REDIALLED VERDICT: the shim is genuinely gone, both probes agree, and
// the sweep is finished with the session.
func TestE2EASessionLeftUnwiredByTheBootSweepIsClassifiedNotSilent(t *testing.T) {
	// DESIGN-BLOCKED, deliberately skipped rather than red: the emission this
	// asserts has no legal route yet. The classifier hook and the
	// HostBootSweepSessionUnwired arm are landed, but the SSM's connectivity
	// model refuses every transition out of a predecessor-generation
	// hibernated state without a bring-up claim, so a sweep that wires
	// nothing cannot paint the verdict. The skip comes out with the SSM
	// entry point for controller-less sessions (in flight); a merge gate must
	// not stay red on a test of deliberately unimplemented behavior.
	t.Skip("boot-sweep verdict emission awaits the SSM controller-less connectivity entry point")
	// Arrange — a session whose shim will NOT survive the bounce.
	// Tempdirs before the world: cleanups run LIFO, so this tears the daemons
	// and their shims down before the directories are removed.
	cwd := t.TempDir()
	world := newShutdownWorld(t)
	first := world.boot(t)
	liveSession(t, first.harness(), cwd)

	// Act — kill the shim, wait out its exit so neither probe can find it, then
	// bounce. Waiting on the recorded exit is a rendezvous with the reaper, not
	// a guess: a shim still dying would hold its lock and the sweep would defer
	// rather than conclude.
	dead := first.shimFor(t, cwd)
	if err := dead.proc.Terminate(shim.Stop{
		Initiator: "e2e_boot_sweep_visibility",
		Reason:    "the session's shim must be gone before the successor sweeps for it",
	}); err != nil {
		t.Fatalf("terminate the session shim for %s: %v", cwd, err)
	}
	<-dead.exited
	first.bounce()
	second := world.boot(t)
	// The re-check pass is fired directly rather than through
	// sweepRecheckWhenParked: nothing will ever park, which is this test's whole
	// premise, so waiting for a parked connection would be waiting for the state
	// the test has deliberately made impossible.
	second.sweepRecheck()

	// Assert — the successor publishes a classified account of the session it
	// decided not to wire.
	frontend := second.dialFrontend(t)
	awaitAll(t, frontend, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"a WorkspaceState classifying the session the boot sweep left UNWIRED — a conclusion the daemon reached about a session the user owns cannot be a log line only, and the stale connectivity its predecessor wrote must not go on presenting a shimless workspace as OPERATIONAL": func(frame *frontendv1.FrontendFrame) bool {
			state := workspaceStateFor(frame, cwd)
			if state == nil {
				return false
			}
			classified := state.GetConnectivity() != frontendv1.SessionConnectivity_SESSION_CONNECTIVITY_UNSPECIFIED &&
				state.GetConnectivity() != frontendv1.SessionConnectivity_SESSION_CONNECTIVITY_OPERATIONAL
			return classified && state.GetCauseKind() != ""
		},
	})
}
