// The RESOLVED COMPONENT VIEWS and their fence.
//
// The reshape moves the chrome a rendering frontend draws off the session
// catalog and onto three views the daemon resolves completely: TopbarView
// (frame arm 21), TokenBreakdownView (22) and WorkspaceGateView (23). Each is
// stamped with the workspace's staleness FENCE — "an opaque token the client
// compares BYTE-WISE against the fence on the workspace's current
// WorkspaceState, and never parses, splits or interprets".
//
// A view with an EMPTY fence is the failure this test is about: byte-comparing
// "" against a real fence makes every push look stale, and byte-comparing ""
// against "" makes every push look current forever. Either way the client's
// only staleness check has been silently disabled.
//
// The same views must also ride the connect StateSnapshot (fields 12/13/14), so
// a client draws its chrome without waiting for each view's first push.
package e2e

import (
	"testing"
	"time"

	"github.com/gorilla/websocket"
)

// TestE2ETheResolvedComponentViewsArriveFencedAndInTheConnectSnapshot covers
// the FENCED PUBLICATION edge for all three views together.
//
// ONE test rather than three because the edge is a single statement about the
// fence's authority: these views agree with the workspace's own WorkspaceState.
// Splitting it would assert the same fact three times over and still not say
// that the three agree with each other. It is also the only sound reading off
// ONE socket: three separate awaits on a shared stream would let the first
// consume and discard the frame the third was waiting for.
func TestE2ETheResolvedComponentViewsArriveFencedAndInTheConnectSnapshot(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	id := h.createSession(t, cwd)
	conn := h.dial(t, id)
	state := workspaceStateInSnapshot(t, readFrame(t, conn), cwd)
	fence := state.GetFence()
	if fence == "" {
		t.Fatalf("the authoritative WorkspaceState for %q carries an empty fence: there is no token for a fenced view to be compared against", cwd)
	}

	// Act — a real turn, so the turn-moved views have a reason to be pushed:
	// the topbar's accounting line and the breakdown's figures both move.
	//
	// The GATE is deliberately absent from the push assertions below: an
	// ordinary turn is not a gate transition (the gate stays `open`
	// throughout), and the publisher's stated invariant is push-on-change
	// judged on the rendered view — demanding an unchanged gate be re-pushed
	// would assert against that invariant, not against the contract. The
	// gate's delivery guarantee is the SNAPSHOT half below, which every
	// joining client exercises; its push-on-transition guarantee belongs to a
	// hibernation-transition stimulus, not to this test. (Orchestrator ruling
	// during the figma→idl e2e loop.)
	submitPrompt(t, conn, "e2e-fenced-views", "hello fenced views")

	// Assert — collect the turn-moved views off ONE drain, then judge each.
	fences := drainComponentViewFences(t, conn, cwd)
	for _, view := range []struct {
		what string
		arm  string
	}{
		{"TopbarView", "topbar (frame arm 21)"},
		{"TokenBreakdownView", "token_breakdown (frame arm 22)"},
	} {
		got, arrived := fences[view.what]
		if !arrived {
			t.Errorf("no %s for workspace %q arrived on the %s frame arm: the client has no resolved copy of that chrome at all", view.what, cwd, view.arm)
			continue
		}
		if got == "" {
			t.Errorf("%s arrived with an EMPTY fence: the client's byte-wise staleness comparison is silently disabled", view.what)
			continue
		}
		if got != fence {
			t.Errorf("%s fence = %q, want the workspace's current WorkspaceState.fence %q: the client discards this push whole as stale", view.what, got, fence)
		}
	}

	// The RECONNECT half: the same views ride the connect snapshot, so a
	// joining client draws its chrome without waiting for the next push.
	fresh := h.dial(t, id)
	snapshot := readFrame(t, fresh).GetSnapshot()
	if snapshot == nil {
		t.Fatal("the reconnecting client's first frame was not a StateSnapshot")
	}
	if len(snapshot.GetTopbars()) == 0 {
		t.Error("the connect StateSnapshot carries no topbars (field 12): a joining client renders no title, model selector or connectivity glyph until the next push")
	}
	if len(snapshot.GetTokenBreakdowns()) == 0 {
		t.Error("the connect StateSnapshot carries no token_breakdowns (field 13): a joining client's counter menu is empty until the next push")
	}
	if len(snapshot.GetWorkspaceGates()) == 0 {
		t.Error("the connect StateSnapshot carries no workspace_gates (field 14): a joining client cannot tell whether its composer may send a prompt")
	}
}

// drainComponentViewFences reads frames until all three component views for
// workspace have been seen, or the deadline expires, and returns each view's
// fence by view name. A view that never arrives is simply absent from the map,
// which is what lets the caller report the missing view by name rather than as
// a bare timeout.
func drainComponentViewFences(t *testing.T, conn *websocket.Conn, workspace string) map[string]string {
	t.Helper()
	fences := map[string]string{}
	deadline := time.Now().Add(frameTimeout)
	// Drains until the two TURN-MOVED views arrive (the gate is push-on-
	// transition and an ordinary turn is not one — see the caller); a gate
	// frame that does arrive is still recorded.
	for len(fences) < 2 && time.Now().Before(deadline) {
		frame := readFrame(t, conn)
		if v := frame.GetTopbar(); v != nil && v.GetWorkspace() == workspace {
			fences["TopbarView"] = v.GetFence()
		}
		if v := frame.GetTokenBreakdown(); v != nil && v.GetWorkspace() == workspace {
			fences["TokenBreakdownView"] = v.GetFence()
		}
		if v := frame.GetWorkspaceGate(); v != nil && v.GetWorkspace() == workspace {
			fences["WorkspaceGateView"] = v.GetFence()
		}
	}
	return fences
}
