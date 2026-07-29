// THE CONNECTION-TRUTH LAW, end to end over the REAL processes.
//
// A workspace's color is CONNECTION TRUTH: blue means there is no live backend
// session for it, and every non-blue color is a GUARANTEE that the session
// substrate is fully wired — shim live, handshake complete, store link settled.
// The two tests here are that law's two directions on the real stack, because
// the wiring is exactly the thing no unit test can prove: the daemon's own
// bring-up gate has to close against a real shim for the axis to open at all.
//
// These share e2e_test.go's package and reuse its helpers READ-ONLY
// (newUDSHarness, createSession, dial, readFrame, frameTimeout, withIdleSweeper)
// and interrupt_e2e_test.go's workspaceStateFor.
package e2e

import (
	"testing"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"github.com/gorilla/websocket"
)

// observedStates reads frames until want is seen for the workspace, returning
// every render state observed along the way in arrival order.
//
// It waits on the FRAME, never on a duration: the daemon pushes a state the
// instant the resolver produces one, so the arrival is the event.
func observedStates(t *testing.T, conn *websocket.Conn, workspace string, want frontendv1.RenderState) []frontendv1.RenderState {
	t.Helper()
	var seen []frontendv1.RenderState
	deadline := time.Now().Add(frameTimeout)
	for time.Now().Before(deadline) {
		st := workspaceStateFor(readFrame(t, conn), workspace)
		if st == nil {
			continue
		}
		seen = append(seen, st.GetState())
		if st.GetState() == want {
			return seen
		}
	}
	t.Fatalf("%s never reached %s before the deadline; saw %v", workspace, want, seen)
	return nil
}

// A FRESHLY CREATED SESSION walks the law forwards: blue while the bring-up is
// in flight, and a real agent state only once the gate has closed.
//
// The green half is what could not be true before the axis existed. Nothing in
// the daemon previously PROVED that a non-blue tab had a wired session behind
// it; the color came off the agent axis alone, which a hibernated or restarted
// workspace could keep reporting with no substrate at all.
func TestE2EAFreshSessionGoesStartingThenWiredThenItsRealState(t *testing.T) {
	// Arrange.
	h := newUDSHarness(t)
	cwd := t.TempDir()
	id := h.createSession(t, cwd)
	conn := h.dial(t, id)
	if first := readFrame(t, conn); first.GetSnapshot() == nil {
		t.Fatalf("first frame = %T, want a StateSnapshot", first.GetFrame())
	}

	// Act — read until the session reports a state only a WIRED workspace can.
	seen := observedStates(t, conn, cwd, frontendv1.RenderState_RENDER_STATE_READY)

	// Assert — the green is the guarantee, and nothing dormant preceded it: a
	// bring-up actually in flight reports INIT, never the resting blue.
	for _, st := range seen {
		if st == frontendv1.RenderState_RENDER_STATE_SEVERED {
			t.Fatalf("a session coming up reported DORMANT; the bring-up window is INIT. saw %v", seen)
		}
	}
	if got := seen[len(seen)-1]; got != frontendv1.RenderState_RENDER_STATE_READY {
		t.Fatalf("final state = %s, want READY", got)
	}
}

// HIBERNATION walks it backwards: the substrate goes away, so the color must.
//
// It is driven through the daemon's own idle sweeper — the production
// hibernation trigger, which calls driver.Hibernate — on a clock this test
// supplies, so the edge is provoked by an event rather than waited out.
func TestE2EHibernationDropsTheWorkspaceToDormant(t *testing.T) {
	// Arrange — a wired, green session.
	h := newUDSHarness(t, withIdleSweeper())
	cwd := t.TempDir()
	id := h.createSession(t, cwd)
	conn := h.dial(t, id)
	if first := readFrame(t, conn); first.GetSnapshot() == nil {
		t.Fatalf("first frame = %T, want a StateSnapshot", first.GetFrame())
	}
	// Read up to the green, which is itself the proof that the substrate is
	// wired: nothing else can report it. Deliberately NOT waitAttachedSession —
	// that helper returns on the first workspace state and would swallow the
	// very frame this needs to see.
	observedStates(t, conn, cwd, frontendv1.RenderState_RENDER_STATE_READY)

	// Act — the idle sweeper hibernates it.
	h.sweepIdle <- time.Now()

	// Assert — the workspace reports the absence of its session, not the last
	// thing the agent happened to say before it went away.
	observedStates(t, conn, cwd, frontendv1.RenderState_RENDER_STATE_SEVERED)
}
