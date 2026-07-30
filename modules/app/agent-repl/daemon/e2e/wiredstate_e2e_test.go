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

	"github.com/gorilla/websocket"
	"google.golang.org/protobuf/encoding/protojson"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
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
// the daemon previously PROVED that a green tab had a wired session behind it;
// the color came off the agent axis alone, which a hibernated or restarted
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

	// Assert — the green is the guarantee, and neither closed half of the axis
	// preceded it: a bring-up actually in flight reports INIT, never a resting
	// state. SEVERED would accuse the substrate of breaking, and HIBERNATED would
	// claim we put a session to sleep that we were in fact starting.
	for _, st := range seen {
		if st == frontendv1.RenderState_RENDER_STATE_SEVERED || st == frontendv1.RenderState_RENDER_STATE_HIBERNATED {
			t.Fatalf("a session coming up reported %s; the bring-up window is INIT. saw %v", st, seen)
		}
	}
	if got := seen[len(seen)-1]; got != frontendv1.RenderState_RENDER_STATE_READY {
		t.Fatalf("final state = %s, want READY", got)
	}
}

// HIBERNATION walks it backwards: the substrate goes away, so the color must —
// but it goes TEAL, not blue, and this is the end-to-end proof of the split.
//
// It is driven through the daemon's own idle sweeper — the production
// hibernation trigger, which calls controller.Hibernate — on a clock this test
// supplies, so the edge is provoked by an event rather than waited out. That
// also makes this the only test that exercises the trap in situ: the sweeper's
// hibernation cancels the session controller ctx, so the session-controller-exit tail fires on this same
// workspace milliseconds later, and a tail that wrote `severed` unconditionally
// would repaint the tab blue right after this assertion's frame.
func TestE2EHibernationDropsTheWorkspaceToHibernated(t *testing.T) {
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
	// thing the agent happened to say before it went away, and it reports the
	// BENIGN absence: nothing broke here, we reclaimed the memory on purpose.
	observedStates(t, conn, cwd, frontendv1.RenderState_RENDER_STATE_HIBERNATED)
}

// AND THE TEAL STAYS TEAL. The frame above is only half the guarantee: the
// hibernation's own cancel ends client.Run, so the session-controller-exit tail runs on this
// workspace immediately afterwards, and an unconditional severance there
// repainted every single hibernation blue milliseconds after it went teal.
//
// This reads PAST the hibernation frame and fails on a severance arriving behind
// it, which is the only way to catch a repaint that happens after the state the
// previous test already accepted.
func TestE2EAHibernationIsNotRepaintedByItsOwnSessionControllerExit(t *testing.T) {
	// Arrange — a wired, green session under the idle sweeper.
	h := newUDSHarness(t, withIdleSweeper())
	cwd := t.TempDir()
	id := h.createSession(t, cwd)
	conn := h.dial(t, id)
	if first := readFrame(t, conn); first.GetSnapshot() == nil {
		t.Fatalf("first frame = %T, want a StateSnapshot", first.GetFrame())
	}
	observedStates(t, conn, cwd, frontendv1.RenderState_RENDER_STATE_READY)

	// Act — hibernate, then keep reading for the whole window the exit tail
	// could land in.
	h.sweepIdle <- time.Now()
	observedStates(t, conn, cwd, frontendv1.RenderState_RENDER_STATE_HIBERNATED)

	// Assert — no severance follows. The wait is INVERTED: silence is the pass,
	// so the read must not be the fatal helper every other assertion here uses.
	// Running out of frames is exactly what is being asserted.
	deadline := time.Now().Add(repaintWindow)
	for time.Now().Before(deadline) {
		frame, ok := tryReadFrame(t, conn, deadline)
		if !ok {
			break
		}
		st := workspaceStateFor(frame, cwd)
		if st == nil {
			continue
		}
		if st.GetState() == frontendv1.RenderState_RENDER_STATE_SEVERED {
			t.Fatalf("the session-controller-exit tail repainted a hibernation %s; a clean exit must write no wired row at all", st.GetState())
		}
	}
}

// repaintWindow is how long a severance following a hibernation is watched for.
// It is a FAILURE bound rather than a tuned delay: the tail runs milliseconds
// after the cancel, so anything this side of a second is generous, and the test
// spends the whole window only when it is passing.
const repaintWindow = 2 * time.Second

// tryReadFrame reads one frame, reporting ok=false when the socket goes quiet
// before DEADLINE rather than failing the test.
//
// It exists for the one assertion whose PASS condition is silence. readFrame
// treats a read timeout as a fatal error, which is right everywhere a frame is
// genuinely expected and wrong here: the absence of a further frame is the whole
// claim. A malformed frame is still fatal — that is a protocol fault, not
// silence.
func tryReadFrame(t *testing.T, conn *websocket.Conn, deadline time.Time) (*frontendv1.FrontendFrame, bool) {
	t.Helper()
	if err := conn.SetReadDeadline(deadline); err != nil {
		t.Fatalf("deadline: %v", err)
	}
	_, data, err := conn.ReadMessage()
	if err != nil {
		return nil, false
	}
	frame := &frontendv1.FrontendFrame{}
	if err := protojson.Unmarshal(data, frame); err != nil {
		t.Fatalf("protojson unmarshal %s: %v", data, err)
	}
	return frame, true
}
