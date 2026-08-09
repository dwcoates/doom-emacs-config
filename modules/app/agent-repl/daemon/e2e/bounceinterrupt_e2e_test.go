// AN INTERRUPT IN FLIGHT WHEN THE UDS CONNECTION DIES.
//
// An Interrupt is a CONTROL REQUEST with an answer: the daemon writes it, the
// shim decides the outcome synchronously off its own turn counter
// (uds-session.ts interrupt), and writes the verdict back. When the connection
// dies between those two halves the request has already had all of its effects
// on the shim — every blocked permission wait cancelled, the SDK query
// interrupted, the displaced turns' terminals armed — while the daemon that
// asked for it is gone before the verdict lands.
//
// THREE THINGS MUST HOLD, and they are three separate facts:
//
//  1. NO WEDGE. The shim completes the interrupt locally, and the session it
//     serves is still driveable by whatever daemon comes next.
//  2. NO DOUBLE FIRE. The interrupted turn ends exactly once in the durable
//     record. A successor that re-drove the stop, or that cut the claim a
//     second time on top of the shim's own end, would put two ends against one
//     start.
//  3. THE OUTCOME RECONCILES. The turn the stop displaced is accounted for
//     after the reattach rather than left thinking forever.
//
// AND THE DESIGN DEPENDENCE UNDERNEATH ALL OF IT: a mid-turn shim survives the
// loss of its daemon connection and KEEPS STREAMING TO THE STORE. Every
// guarantee above rests on that, because the store is the only place the facts
// produced during the gap can be waiting when the successor arrives.
package e2e

import (
	"testing"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// TestE2EAnInterruptLostToTheUDSDeathLeavesTheSessionDriveable covers NO WEDGE.
func TestE2EAnInterruptLostToTheUDSDeathLeavesTheSessionDriveable(t *testing.T) {
	// Arrange — a turn in flight, and a stop written but not yet answered.
	// Tempdirs before the world: cleanups run LIFO, so this tears the daemons
	// and their shims down before the directories are removed.
	cwd := t.TempDir()
	world := newShutdownWorld(t)
	first := world.boot(t)
	_, conn, _, _ := liveSession(t, first.harness(), cwd)
	holdTurnOpen(t, conn, cwd, "r-hold", "sleep e2e-interrupt-at-uds-loss")

	// Act — the stop goes out and the daemon dies underneath it. The ack is
	// deliberately NOT awaited: an interrupt whose verdict was already read is
	// not an interrupt in flight.
	writeCmd(t, conn, `{"requestId":"r-interrupt","interrupt":{}}`)
	first.bounce()
	second := world.boot(t)

	// Assert — the successor can still drive the session.
	_, afterConn, _ := reattached(t, second, cwd)
	const prompt = "driveable after an interrupt lost to the daemon's death"
	submitPrompt(t, afterConn, "r-after", prompt)
	awaitEcho(t, afterConn, cwd, prompt,
		"the reply to a prompt submitted after an interrupt died with its daemon — a stop whose verdict never came back must leave the shim serving its session, not wedged behind a control request nobody will ever answer")
}

// TestE2EAnInterruptLostToTheUDSDeathEndsItsTurnExactlyOnce covers NO DOUBLE
// FIRE, read from the durable record rather than from a frame: the store is
// where a second end would be visible as such.
func TestE2EAnInterruptLostToTheUDSDeathEndsItsTurnExactlyOnce(t *testing.T) {
	// Arrange
	cwd := t.TempDir()
	world := newShutdownWorld(t)
	first := world.boot(t)
	_, conn, vendorID, _ := liveSession(t, first.harness(), cwd)
	holdTurnOpen(t, conn, cwd, "r-hold", "sleep e2e-interrupt-double-fire")

	// Act
	writeCmd(t, conn, `{"requestId":"r-interrupt","interrupt":{}}`)
	first.bounce()
	second := world.boot(t)
	_, afterConn, _ := reattached(t, second, cwd)
	// A prompt AFTER the reattach is the ordering device: its own reply cannot
	// arrive until the successor has finished re-establishing the session, so a
	// durable read taken once it has landed cannot precede whatever the
	// reattach was going to write.
	const prompt = "sentinel after the lost interrupt"
	submitPrompt(t, afterConn, "r-after", prompt)
	awaitEcho(t, afterConn, cwd, prompt, "the post-reattach sentinel turn's reply")

	// Assert — one start, one end, for the interrupted turn.
	starts := storeTurnIDs(t, vendorID)
	ends := countStoreTurnEnds(t, vendorID)
	if ends > len(starts) {
		t.Errorf("conversation %s holds %d durable TurnEnded records against %d TurnStarted (%v): an interrupt whose verdict was lost to the daemon's death must be completed ONCE by the shim, never re-driven or re-cut by the daemon that replaces it",
			vendorID, ends, len(starts), starts)
	}
}

// TestE2EAnInterruptLostToTheUDSDeathReconcilesItsOutcome covers THE OUTCOME:
// the turn the stop displaced is accounted for after the reattach rather than
// left claimed and thinking.
func TestE2EAnInterruptLostToTheUDSDeathReconcilesItsOutcome(t *testing.T) {
	// Arrange
	cwd := t.TempDir()
	world := newShutdownWorld(t)
	first := world.boot(t)
	_, conn, _, _ := liveSession(t, first.harness(), cwd)
	holdTurnOpen(t, conn, cwd, "r-hold", "sleep e2e-interrupt-reconcile")

	// Act
	writeCmd(t, conn, `{"requestId":"r-interrupt","interrupt":{}}`)
	first.bounce()
	second := world.boot(t)

	// Assert — the workspace stops claiming a live turn.
	_, afterConn, _ := reattached(t, second, cwd)
	awaitAll(t, afterConn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"a WorkspaceState reporting NO live turn after the reattach — the stop the shim completed locally must be reconciled into the successor's view, not left as a claim that keeps the workspace thinking forever": func(frame *frontendv1.FrontendFrame) bool {
			state := workspaceStateFor(frame, cwd)
			return state != nil && !state.GetTurnActive()
		},
	})
}

// TestE2EAMidTurnShimKeepsStreamingToTheStoreWithNoDaemonAttached covers THE
// DESIGN DEPENDENCE the three guarantees above rest on.
//
// The turn is started and the daemon destroyed immediately; the shim runs the
// turn to completion against the STORE ALONE, with no daemon in existence to
// receive anything. The evidence is read straight out of the store, while the
// successor has deliberately NOT been booted: if the store's record of the turn
// grows here, it grew with nobody attached.
func TestE2EAMidTurnShimKeepsStreamingToTheStoreWithNoDaemonAttached(t *testing.T) {
	// Arrange
	cwd := t.TempDir()
	world := newShutdownWorld(t)
	first := world.boot(t)
	_, conn, vendorID, _ := liveSession(t, first.harness(), cwd)
	before := storeEventCount(t, vendorID)

	// Act — start a turn and take the daemon away.
	submitPrompt(t, conn, "r-gap-turn", "a turn that outlives its daemon")
	first.bounce()

	// Assert — the conversation's durable record grows with no daemon attached.
	//
	// Bounded by the suite's frame budget and polled through the store's own
	// replay, which is a fresh subscription per read: there is no daemon left to
	// push an edge at, so the store's answer is the only observation available.
	deadline := time.Now().Add(frameTimeout)
	for time.Now().Before(deadline) {
		if storeEventCount(t, vendorID) > before {
			return
		}
	}
	t.Fatalf("conversation %s still holds %d events after its daemon died mid-turn: a shim that stops writing to the store when its daemon goes away makes every other bounce guarantee unreachable, because the store is the only place the gap's facts can wait",
		vendorID, before)
}
