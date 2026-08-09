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
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/server"
	"claude-repld/internal/shim"
	"claude-repld/internal/ssm"
)

// TestE2EASessionLeftUnwiredByTheBootSweepIsClassifiedNotSilent covers THE
// NEVER-REDIALLED VERDICT: the shim is genuinely gone, both probes agree, and
// the sweep is finished with the session.
func TestE2EASessionLeftUnwiredByTheBootSweepIsClassifiedNotSilent(t *testing.T) {
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
	//
	// TWO AMENDMENTS TO WHAT THIS USED TO ASSERT, both because the original
	// pair of conditions was not a witness of the behavior:
	//
	//  1. IT READS THE CONNECT SNAPSHOT, not only a pushed WorkspaceState. The
	//     sweep concludes during boot, before this test can dial, so the push
	//     it makes has no subscriber and the ONLY delivery is the snapshot the
	//     frontend sends on connect. Waiting for a per-workspace push here was
	//     waiting for a frame that had already gone out to nobody. The
	//     snapshot's `workspaces` field is the authoritative per-workspace
	//     ruling (frame.proto), so this is the same observable, delivered the
	//     way a client that connects after a boot actually receives it.
	//
	//  2. IT NAMES THE VERDICT. `connectivity != operational` and
	//     `cause_kind != ""` were BOTH already true before any of this existed:
	//     a bounced daemon hibernates every surviving session with the cause
	//     `daemon_restart`, which is what EVERY survivor's row says and
	//     therefore says nothing about this one. The contract in this file's
	//     header is that the record says WHICH VERDICT IT WAS, so that is what
	//     is asserted — the frame's cause is the verdict, and the classified
	//     fault row carrying it is on the frame.
	//
	// The connectivity assertion is kept and sharpened to `hibernated`: the
	// sweep made no bring-up claim, so hibernated is the true state, and what
	// had to stop being silent is the CAUSE rather than the axis.
	frontend := second.dialFrontend(t)
	awaitAll(t, frontend, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"a WorkspaceState naming the verdict the boot sweep reached about the session it left UNWIRED — a conclusion the daemon reached about a session the user owns cannot be a log line only, and the anonymous restart cause its predecessor wrote says nothing about THIS session": func(frame *frontendv1.FrontendFrame) bool {
			state := snapshotWorkspaceStateFor(frame, cwd)
			if state == nil {
				return false
			}
			if state.GetConnectivity() != frontendv1.SessionConnectivity_SESSION_CONNECTIVITY_HIBERNATED {
				return false
			}
			if state.GetCauseKind() != server.BootSweepUnwiredNoLiveShim {
				return false
			}
			for _, fault := range state.GetActiveFaults() {
				if fault.GetComponent() == ssm.BootSweepFaultComponent &&
					fault.GetFaultType() == server.BootSweepUnwiredNoLiveShim &&
					fault.GetImpact() == string(ssm.FaultImpactConnectivity) {
					return true
				}
			}
			return false
		},
	})

	// And the host is told in words. The pushed state is a classification; the
	// retained host action is the only surface that puts a sentence in front of
	// the person whose session did not come back.
	notes := second.hostVerdicts.bootSweepHostNotes()
	if len(notes) != 1 {
		t.Fatalf("host verdicts = %#v, want exactly one account of the unwired session", notes)
	}
	if notes[0].verdict != server.BootSweepUnwiredNoLiveShim || notes[0].sessionID == "" {
		t.Fatalf("host verdict = %#v, want the never-redialled verdict named against its session", notes[0])
	}
	if !strings.Contains(notes[0].reason, server.BootSweepUnwiredNoLiveShim) {
		t.Fatalf("host reason = %q, want the verdict token in the sentence the host renders", notes[0].reason)
	}
}

// snapshotWorkspaceStateFor returns the WorkspaceState a CONNECT SNAPSHOT
// carries for workspace, or nil when the frame is not a snapshot or does not
// mention it. It is the snapshot-arm counterpart of workspaceStateFor: a client
// that connects after an edge has already been pushed learns the fact here and
// nowhere else.
func snapshotWorkspaceStateFor(frame *frontendv1.FrontendFrame, workspace string) *frontendv1.WorkspaceState {
	snapshot, ok := frame.GetFrame().(*frontendv1.FrontendFrame_Snapshot)
	if !ok {
		return nil
	}
	for _, state := range snapshot.Snapshot.GetWorkspaces() {
		if state.GetWorkspace() == workspace {
			return state
		}
	}
	return nil
}
