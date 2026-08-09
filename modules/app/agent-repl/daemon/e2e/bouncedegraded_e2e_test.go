// A DEGRADATION THAT NOBODY WAS LISTENING TO.
//
// A shim reports a degradation as an EPHEMERAL synthetic event
// (uds-session.ts degradedEvent: Plane.SYNTHETIC, EventClass.EPHEMERAL), and
// `sendEvent` (uds/server.ts) says exactly what happens to one with no daemon
// attached: "EPHEMERAL event dropped permanently (never written to the store,
// never replayed)". The report has no second chance — it is not in the store,
// so no replay can carry it — and the user is never told the session degraded.
//
// The gap is therefore twofold and the fix is one mechanism:
//
//   - A degradation raised while the daemon is DOWN is lost outright.
//   - A degradation raised while a daemon was UP dies with THAT daemon: the
//     card lived in its memory, and the successor has never heard of it.
//
// WHAT MUST BE TRUE. A shim RE-REPORTS ITS CURRENT DEGRADED STATE ON RECONNECT.
// That single guarantee covers both: what the daemon missed and what the
// previous daemon knew are the same question — "what is degraded right now" —
// and only the shim can answer it.
//
// HOW A REAL DEGRADATION IS PROVOKED. A prompt carrying a NON-SWITCHABLE
// permission mode. `bypassPermissions` is a valid launch mode that a running
// session cannot switch into, and the shim's submit path says so by reporting a
// DegradedState on `claude-shim-permission-mode` rather than by logging: "the
// user picked a mode; if it did not take, the session is running under a
// DIFFERENT permission posture than the one they chose, and a log nobody reads
// is not an acceptable way to tell them that". It is the shim's own report,
// through the shim's own channel — nothing here injects a degradation on its
// behalf.
//
// AND ONE THING THAT IS NOT A GAP. ContentDeltas are ephemeral too, and the
// typing animation lost during a gap is lost BY DESIGN: the assistant's message
// and its result are persistent, so the conversation converges on the durable
// text without them. The last test pins that convergence, because "we
// deliberately drop these" is only acceptable while the thing they were
// animating still arrives.
package e2e

import (
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// degradedComponent is the component the provoked degradation is filed under.
const degradedComponent = "claude-shim-permission-mode"

// unswitchableMode is a real permission mode a RUNNING session cannot adopt.
const unswitchableMode = "bypassPermissions"

// TestE2EAStandingDegradationIsReReportedToTheDaemonThatReplacesItsListener
// covers THE RE-REPORT: an open degradation survives the daemon that heard it.
func TestE2EAStandingDegradationIsReReportedToTheDaemonThatReplacesItsListener(t *testing.T) {
	// Arrange — a real, still-open degradation the first daemon rendered.
	// Tempdirs before the world: cleanups run LIFO, so this tears the daemons
	// and their shims down before the directories are removed.
	cwd := t.TempDir()
	world := newShutdownWorld(t)
	first := world.boot(t)
	_, conn, _, _ := liveSession(t, first.harness(), cwd)
	submitPromptInMode(t, conn, "r-degrade", "a prompt in a mode this session cannot adopt", unswitchableMode)
	awaitAll(t, conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"the degradation the shim reported for " + degradedComponent: func(frame *frontendv1.FrontendFrame) bool {
			return degradedCardIn(frame, cwd, degradedComponent) != nil
		},
	})

	// Act — the daemon that was holding that card is destroyed.
	first.bounce()
	second := world.boot(t)

	// Assert — the successor learns the session is degraded, from the shim.
	_, snap := reattachedSnapshot(t, second, cwd)
	if !snapshotDegradedCard(snap, cwd, degradedComponent) {
		t.Fatalf("the successor's connect snapshot for %s reports no open fault naming %s: the degradation is EPHEMERAL, so it is in no store and no replay can carry it — only the shim can re-report what is degraded right now, and a session that stays quietly degraded across a bounce is one running under a posture the user was told about once and never again",
			cwd, degradedComponent)
	}
}

// TestE2EADegradationRaisedWhileUnattachedSurfacesAfterReattach covers THE GAP
// HALF: the report is raised with no daemon in existence at all, so the drop is
// not "the previous daemon knew and died" but "nobody ever heard it".
//
// The prompt is submitted at the very edge of the teardown, so the shim's
// report lands against a connection that is going or gone. Whichever side of
// that edge it falls on, the guarantee asserted is the same one — the successor
// is told — and it is absent today either way, so the test cannot pass by
// missing its window.
func TestE2EADegradationRaisedWhileUnattachedSurfacesAfterReattach(t *testing.T) {
	// Arrange
	cwd := t.TempDir()
	world := newShutdownWorld(t)
	first := world.boot(t)
	_, conn, _, _ := liveSession(t, first.harness(), cwd)

	// Act — provoke the degradation and take the listener away underneath it.
	//
	// THE ACK IS WAITED FOR, AND IT IS PART OF THE ARRANGEMENT, NOT OF WHAT IS
	// ASSERTED. Bouncing on the line after the write raced the prompt against
	// the daemon's own teardown, and the prompt lost every time: the command was
	// NACKED with `session-controller: manager closed`, so it never reached the
	// shim, no mode was ever applied, and nothing degraded. A test that provokes
	// nothing cannot observe the loss of it. Waiting for the ack pins the prompt
	// as ACCEPTED and handed to the shim, which is what puts the shim's report
	// on the intended edge — against a daemon connection that is going or gone.
	submitPromptInMode(t, conn, "r-degrade", "a prompt in a mode this session cannot adopt", unswitchableMode)
	awaitAck(t, conn, "r-degrade", "the prompt whose non-switchable mode is what degrades this session")
	first.bounce()
	second := world.boot(t)

	// Assert
	_, snap := reattachedSnapshot(t, second, cwd)
	if !snapshotDegradedCard(snap, cwd, degradedComponent) {
		t.Fatalf("no daemon ever learned that %s degraded on %s: the report was raised with nothing attached, was dropped permanently because it is EPHEMERAL, and no reconnect re-asked the shim what its current state is",
			degradedComponent, cwd)
	}
}

// TestE2EAConversationConvergesWithoutTheContentDeltasLostToTheGap covers THE
// BY-DESIGN LOSS: the typing animation for a turn that ran across the gap is
// gone, and the conversation is still correct and complete without it.
//
// This is the standing guard on that design decision. Dropping an ephemeral
// delta is only acceptable while the persistent message it was animating still
// arrives, so the assertion is on the assistant's FINAL TEXT for the prompt —
// reached through the durable record, on a daemon that never saw a single one
// of the deltas.
func TestE2EAConversationConvergesWithoutTheContentDeltasLostToTheGap(t *testing.T) {
	// Arrange
	cwd := t.TempDir()
	world := newShutdownWorld(t)
	first := world.boot(t)
	_, conn, _, _ := liveSession(t, first.harness(), cwd)

	// Act — a turn that runs across the daemon's death.
	const prompt = "a turn whose typing nobody watched"
	submitPrompt(t, conn, "r-gap-turn", prompt)
	first.bounce()
	second := world.boot(t)
	_, afterConn, _ := reattached(t, second, cwd)

	// Assert — the answer itself is there, whole.
	awaitEcho(t, afterConn, cwd, prompt,
		"the complete assistant reply to a turn whose ContentDeltas were dropped during the gap — the deltas are ephemeral by design, which is only defensible while the persistent message they animate still converges")
}
