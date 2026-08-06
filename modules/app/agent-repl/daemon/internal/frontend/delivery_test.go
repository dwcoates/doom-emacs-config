package frontend

import (
	"net/http"
	"net/http/httptest"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
	"claude-repld/internal/errclass"

	"github.com/gorilla/websocket"
	"google.golang.org/protobuf/encoding/protojson"
)

// ---------------------------------------------------------------------------
// DELIVERY IS UNSEQUENCED. Every connected frontend receives every emission
// immediately — same order, same content — with no frontend's render pace
// gating another's.
//
// The gate these replace withheld each state from the Emacs tab bar until the
// rendering webview acknowledged having drawn it, and it had a hole of its own:
// an observer's reconnect snapshot was filtered to states a painter had SETTLED,
// so a workspace whose first emission was still held was OMITTED from Emacs
// entirely. Both are asserted gone here.
// ---------------------------------------------------------------------------

// dialScoped opens a scoped WebSocket client of the given kind against s,
// reads its connect snapshot, and returns the connection plus that snapshot.
func dialScoped(t *testing.T, s *Server, scope Scope, kind ClientKind) (*websocket.Conn, *frontendv1.StateSnapshot) {
	t.Helper()
	httpSrv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		s.ServeWSScoped(w, r, scope, kind, nil)
	}))
	t.Cleanup(httpSrv.Close)
	conn, _, err := websocket.DefaultDialer.Dial("ws"+httpSrv.URL[len("http"):], nil)
	if err != nil {
		t.Fatalf("ws dial: %v", err)
	}
	t.Cleanup(func() { conn.Close() })
	return conn, readWSSnapshot(t, conn)
}

// readWSSnapshot reads the connect snapshot, failing if the first frame is
// anything else (the snapshot-then-deltas contract).
func readWSSnapshot(t *testing.T, conn *websocket.Conn) *frontendv1.StateSnapshot {
	t.Helper()
	frame := readWSFrame(t, conn)
	snap := frame.GetSnapshot()
	if snap == nil {
		t.Fatalf("first frame was not a snapshot: %v", frame)
	}
	return snap
}

// A state reaches BOTH connected frontends without either waiting on the other,
// and both are handed the identical resolution.
func TestEveryFrontendReceivesAPushImmediatelyAndIdentically(t *testing.T) {
	// Arrange — the two production shapes: the rendering webview and a second
	// scoped client standing in for the Emacs host's read of the same workspace.
	s, _ := newTestServer(t, 0)
	defer s.Close()
	stream, _ := dialScoped(t, s, Scope{Workspace: "w1", SessionID: "s1"}, ClientKindGUIStream)
	other, _ := dialScoped(t, s, Scope{Workspace: "w1", SessionID: "s1"}, ClientKindGUIObserver)

	// Act — one emission, nobody acknowledging anything.
	s.PushWorkspaceState(&frontendv1.WorkspaceState{
		Workspace: "w1", SessionId: "s1",
		State: frontendv1.RenderState_RENDER_STATE_THINKING,
	})

	// Assert — both got it, and got the same thing.
	got1 := readWSFrame(t, stream).GetWorkspaceState()
	got2 := readWSFrame(t, other).GetWorkspaceState()
	if got1.GetState() != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("stream state = %s, want THINKING", got1.GetState())
	}
	if got2.GetState() != got1.GetState() {
		t.Fatalf("the two frontends were told different states: %s vs %s", got1.GetState(), got2.GetState())
	}
}

// ORDER IS PRESERVED per connection: what the resolver produced is what each
// queue carries, in that sequence.
func TestPushesArriveInResolutionOrder(t *testing.T) {
	// Arrange.
	s, _ := newTestServer(t, 0)
	defer s.Close()
	conn, _ := dialScoped(t, s, Scope{Workspace: "w1", SessionID: "s1"}, ClientKindGUIObserver)

	want := []frontendv1.RenderState{
		frontendv1.RenderState_RENDER_STATE_THINKING,
		frontendv1.RenderState_RENDER_STATE_DONE,
		frontendv1.RenderState_RENDER_STATE_SEVERED,
	}

	// Act.
	for _, st := range want {
		s.PushWorkspaceState(&frontendv1.WorkspaceState{Workspace: "w1", SessionId: "s1", State: st})
	}

	// Assert.
	for i, expect := range want {
		if got := readWSFrame(t, conn).GetWorkspaceState().GetState(); got != expect {
			t.Fatalf("frame %d = %s, want %s", i, got, expect)
		}
	}
}

// A frontend that never sends anything back is never a reason to withhold a
// state. This is the wedge the gate could produce and this delivery cannot.
func TestASilentFrontendDoesNotWithholdFromAnother(t *testing.T) {
	// Arrange — the silent one connects first, so a sequencer would be holding
	// its acknowledgment when the second is served.
	s, _ := newTestServer(t, 0)
	defer s.Close()
	dialScoped(t, s, Scope{Workspace: "w1", SessionID: "s1"}, ClientKindGUIStream)
	other, _ := dialScoped(t, s, Scope{Workspace: "w1", SessionID: "s1"}, ClientKindGUIObserver)

	// Act.
	s.PushWorkspaceState(&frontendv1.WorkspaceState{
		Workspace: "w1", SessionId: "s1",
		State: frontendv1.RenderState_RENDER_STATE_READY,
	})

	// Assert.
	if got := readWSFrame(t, other).GetWorkspaceState().GetState(); got != frontendv1.RenderState_RENDER_STATE_READY {
		t.Fatalf("state = %s, want READY delivered with nobody acknowledging anything", got)
	}
}

// THE RECONNECT HOLE, closed. A connect snapshot carries every workspace the
// provider knows about; nothing filters it down to what some other frontend has
// settled, so no workspace can be OMITTED from a reconnecting client.
func TestTheConnectSnapshotOmitsNoWorkspace(t *testing.T) {
	// Arrange — a provider with two workspaces, neither ever emitted through
	// the push path (the exact condition that used to omit them).
	h := &mockHandler{}
	s := New(Config{
		Logf:        testLogf(t),
		LogVerbosef: testLogf(t),
		Handler:     h,
		State: staticState{snap: &frontendv1.StateSnapshot{
			Workspaces: []*frontendv1.WorkspaceState{
				{Workspace: "w1", SessionId: "s1", State: frontendv1.RenderState_RENDER_STATE_THINKING},
				{Workspace: "w2", SessionId: "s2", State: frontendv1.RenderState_RENDER_STATE_SEVERED},
			},
		}},
	})
	defer s.Close()

	// Act — a fresh unscoped connection, the Emacs host's shape.
	httpSrv := httptest.NewServer(http.HandlerFunc(s.ServeWS))
	defer httpSrv.Close()
	conn, _, err := websocket.DefaultDialer.Dial("ws"+httpSrv.URL[len("http"):], nil)
	if err != nil {
		t.Fatalf("ws dial: %v", err)
	}
	defer conn.Close()
	snap := readWSSnapshot(t, conn)

	// Assert.
	seen := map[string]frontendv1.RenderState{}
	for _, w := range snap.GetWorkspaces() {
		seen[w.GetWorkspace()] = w.GetState()
	}
	if len(seen) != 2 {
		t.Fatalf("snapshot carried %d workspace(s) (%v), want both", len(seen), seen)
	}
	if seen["w1"] != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("w1 = %s, want THINKING", seen["w1"])
	}
	if seen["w2"] != frontendv1.RenderState_RENDER_STATE_SEVERED {
		t.Fatalf("w2 = %s, want SEVERED", seen["w2"])
	}
}

// The same, for the RESYNC snapshot: a client that asks again is answered with
// everything, not with what has settled.
func TestTheResyncSnapshotOmitsNoWorkspace(t *testing.T) {
	// Arrange.
	h := &mockHandler{}
	s := New(Config{
		Logf:        testLogf(t),
		LogVerbosef: testLogf(t),
		Handler:     h,
		State: staticState{snap: &frontendv1.StateSnapshot{
			Workspaces: []*frontendv1.WorkspaceState{
				{Workspace: "w1", SessionId: "s1", State: frontendv1.RenderState_RENDER_STATE_READY},
				{Workspace: "w2", SessionId: "s2", State: frontendv1.RenderState_RENDER_STATE_SEVERED},
			},
		}},
	})
	defer s.Close()
	httpSrv := httptest.NewServer(http.HandlerFunc(s.ServeWS))
	defer httpSrv.Close()
	conn, _, err := websocket.DefaultDialer.Dial("ws"+httpSrv.URL[len("http"):], nil)
	if err != nil {
		t.Fatalf("ws dial: %v", err)
	}
	defer conn.Close()
	readWSSnapshot(t, conn)

	// Act.
	cmd := &frontendv1.FrontendCommand{
		RequestId: "r1", Workspace: "w1",
		Command: &frontendv1.FrontendCommand_Resync{Resync: &frontendv1.ResyncCmd{}},
	}
	data, err := protojson.Marshal(cmd)
	if err != nil {
		t.Fatalf("marshal resync: %v", err)
	}
	if err := conn.WriteMessage(websocket.TextMessage, data); err != nil {
		t.Fatalf("write resync: %v", err)
	}

	// Assert.
	snap := readWSSnapshot(t, conn)
	if len(snap.GetWorkspaces()) != 2 {
		t.Fatalf("resync snapshot carried %d workspace(s), want both", len(snap.GetWorkspaces()))
	}
}

func TestSupersededResyncReceivesSnapshotCapturedAfterClassification(t *testing.T) {
	h := &mockHandler{err: errclass.ErrSessionSuperseded}
	states := &sequenceState{snaps: []*frontendv1.StateSnapshot{
		{Workspaces: []*frontendv1.WorkspaceState{{Workspace: "w1", SessionId: "s1", ControllerGenerationId: "g-old"}}},
		{Workspaces: []*frontendv1.WorkspaceState{{Workspace: "w1", SessionId: "s1", ControllerGenerationId: "g-old"}}},
		{Workspaces: []*frontendv1.WorkspaceState{{Workspace: "w1", SessionId: "s2", ControllerGenerationId: "g-new"}}},
	}}
	s := New(Config{Logf: testLogf(t), LogVerbosef: testLogf(t), Handler: h, State: states})
	defer s.Close()
	httpSrv := httptest.NewServer(http.HandlerFunc(s.ServeWS))
	defer httpSrv.Close()
	conn, _, err := websocket.DefaultDialer.Dial("ws"+httpSrv.URL[len("http"):], nil)
	if err != nil {
		t.Fatalf("ws dial: %v", err)
	}
	defer conn.Close()
	readWSSnapshot(t, conn)

	cmd := &frontendv1.FrontendCommand{
		RequestId: "r-stale", Workspace: "w1",
		Command: &frontendv1.FrontendCommand_Resync{Resync: &frontendv1.ResyncCmd{
			SessionId: "s1", ControllerGenerationId: "g-old",
		}},
	}
	data, err := protojson.Marshal(cmd)
	if err != nil {
		t.Fatalf("marshal resync: %v", err)
	}
	if err := conn.WriteMessage(websocket.TextMessage, data); err != nil {
		t.Fatalf("write resync: %v", err)
	}

	if got := readWSSnapshot(t, conn).GetWorkspaces()[0].GetControllerGenerationId(); got != "g-old" {
		t.Fatalf("pre-dispatch generation = %q, want g-old", got)
	}
	post := readWSSnapshot(t, conn).GetWorkspaces()[0]
	if post.GetSessionId() != "s2" || post.GetControllerGenerationId() != "g-new" {
		t.Fatalf("post-supersession identity = (%q, %q), want (s2, g-new)", post.GetSessionId(), post.GetControllerGenerationId())
	}
	ack := readWSFrame(t, conn).GetCommandAck()
	if ack.GetFailure().GetErrorType() != string(errclass.TypeSessionReconnectSuperseded) {
		t.Fatalf("failure type = %q, want %q", ack.GetFailure().GetErrorType(), errclass.TypeSessionReconnectSuperseded)
	}
}
