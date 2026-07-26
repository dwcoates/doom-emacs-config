package frontend

import (
	"bufio"
	"net"
	"net/http"
	"net/http/httptest"
	"runtime"
	"strings"
	"sync"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"github.com/gorilla/websocket"
	"google.golang.org/protobuf/encoding/protojson"
)

// withdrawals records the paint-attestation retractions the server requests.
// The connect edge fires on the accept goroutine, so the recorder is guarded.
type withdrawals struct {
	mu   sync.Mutex
	seen []string
}

func (w *withdrawals) sink(workspace, reason string) {
	w.mu.Lock()
	defer w.mu.Unlock()
	w.seen = append(w.seen, workspace+":"+reason)
}

func (w *withdrawals) list() []string {
	w.mu.Lock()
	defer w.mu.Unlock()
	return append([]string(nil), w.seen...)
}

// await busy-polls until the recorder holds want entries (no sleep-based
// synchronization), then returns them.
func (w *withdrawals) await(t *testing.T, want int) []string {
	t.Helper()
	for i := 0; i < 100000; i++ {
		if got := w.list(); len(got) >= want {
			return got
		}
		runtime.Gosched()
	}
	t.Fatalf("withdrawals never reached %d (last=%v)", want, w.list())
	return nil
}

// readWorkspaceStateFrame reads frames until a workspace state arrives,
// skipping the command acks the connection also carries.
func readWorkspaceStateFrame(t *testing.T, r *bufio.Reader) *frontendv1.WorkspaceState {
	t.Helper()
	for i := 0; i < 16; i++ {
		if ws := readFrame(t, r).GetWorkspaceState(); ws != nil {
			return ws
		}
	}
	t.Fatal("no workspace-state frame arrived")
	return nil
}

// ---------------------------------------------------------------------------
// helpers
// ---------------------------------------------------------------------------

// register plants a client of the given role directly in the fan-out set.
// Registration is synchronous here, so every case below is deterministic
// without waiting on an accept goroutine.
func register(t *testing.T, s *Server, role Role, scope *Scope) *client {
	t.Helper()
	cl := &client{
		send:  make(chan []byte, s.bufSize),
		done:  make(chan struct{}),
		role:  role,
		scope: scope,
	}
	s.mu.Lock()
	s.clients[cl] = struct{}{}
	s.mu.Unlock()
	return cl
}

// drain reads every frame currently queued for a client. Delivery happens
// synchronously inside the push, so an empty result is proof nothing was
// delivered rather than proof nothing has arrived YET.
func drain(t *testing.T, cl *client) []*frontendv1.FrontendFrame {
	t.Helper()
	var out []*frontendv1.FrontendFrame
	for {
		select {
		case data := <-cl.send:
			f := &frontendv1.FrontendFrame{}
			if err := protojson.Unmarshal(data, f); err != nil {
				t.Fatalf("unmarshal queued frame: %v", err)
			}
			out = append(out, f)
		default:
			return out
		}
	}
}

// states extracts the workspace states from a list of frames.
func states(t *testing.T, frames []*frontendv1.FrontendFrame) []*frontendv1.WorkspaceState {
	t.Helper()
	out := make([]*frontendv1.WorkspaceState, 0, len(frames))
	for _, f := range frames {
		if ws := f.GetWorkspaceState(); ws != nil {
			out = append(out, ws)
		}
	}
	return out
}

func wsState(workspace string, state frontendv1.RenderState) *frontendv1.WorkspaceState {
	return &frontendv1.WorkspaceState{Workspace: workspace, SessionId: "s1", State: state}
}

func paintAck(workspace string, generation uint64, outcome frontendv1.PaintOutcome) *frontendv1.FrontendCommand {
	return &frontendv1.FrontendCommand{
		RequestId: "ack", Workspace: workspace,
		Command: &frontendv1.FrontendCommand_PaintAck{PaintAck: &frontendv1.PaintAckCmd{
			StateGeneration: generation,
			Outcome:         outcome,
		}},
	}
}

// ---------------------------------------------------------------------------
// The ordering guarantee: an observer never holds a state a painter has not
// answered for.
// ---------------------------------------------------------------------------

// THE CORE GUARANTEE. With a painter connected, a freshly resolved state
// reaches the painter and NOT the observer.
func TestPainterHoldsTheStateBackFromObservers(t *testing.T) {
	// Arrange.
	s, _ := newTestServer(t, 0)
	defer s.Close()
	painter := register(t, s, RolePainter, nil)
	observer := register(t, s, RoleObserver, nil)

	// Act.
	s.PushWorkspaceState(wsState("w1", frontendv1.RenderState_RENDER_STATE_THINKING))

	// Assert.
	if got := len(states(t, drain(t, painter))); got != 1 {
		t.Fatalf("painter received %d workspace states, want 1", got)
	}
	if got := len(states(t, drain(t, observer))); got != 0 {
		t.Fatalf("observer received %d workspace states, want 0 (held until acknowledged)", got)
	}
}

// The acknowledgment is what releases it, and what the observer then holds is
// the very state the painter answered for.
func TestAcknowledgedGenerationReachesObservers(t *testing.T) {
	// Arrange.
	s, _ := newTestServer(t, 0)
	defer s.Close()
	painter := register(t, s, RolePainter, nil)
	observer := register(t, s, RoleObserver, nil)
	s.PushWorkspaceState(wsState("w1", frontendv1.RenderState_RENDER_STATE_THINKING))
	painted := states(t, drain(t, painter))[0]

	// Act.
	s.settlePaint("w1", painted.GetGeneration())

	// Assert.
	got := states(t, drain(t, observer))
	if len(got) != 1 {
		t.Fatalf("observer received %d workspace states, want 1", len(got))
	}
	if got[0].GetGeneration() != painted.GetGeneration() {
		t.Errorf("observer got generation %d, want %d (the acknowledged one)",
			got[0].GetGeneration(), painted.GetGeneration())
	}
	if got[0].GetState() != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Errorf("observer got %s, want THINKING", got[0].GetState())
	}
}

// A STALE ACKNOWLEDGMENT settles nothing: it describes a render of an emission
// the resolver has already superseded.
func TestStaleAckLeavesTheNewerStateHeld(t *testing.T) {
	// Arrange: two emissions, so the second supersedes the first.
	s, _ := newTestServer(t, 0)
	defer s.Close()
	painter := register(t, s, RolePainter, nil)
	observer := register(t, s, RoleObserver, nil)
	s.PushWorkspaceState(wsState("w1", frontendv1.RenderState_RENDER_STATE_THINKING))
	first := states(t, drain(t, painter))[0]
	s.PushWorkspaceState(wsState("w1", frontendv1.RenderState_RENDER_STATE_READY))
	_ = drain(t, painter)

	// Act: acknowledge the OLD generation.
	s.settlePaint("w1", first.GetGeneration())

	// Assert.
	if got := len(states(t, drain(t, observer))); got != 0 {
		t.Fatalf("observer received %d workspace states on a stale ack, want 0", got)
	}
}

// The superseded emission is never delivered on its own: once a newer one has
// replaced it, acknowledging the newer one hands the observer the NEWER state
// and nothing else.
func TestSupersededEmissionIsNeverDeliveredToObservers(t *testing.T) {
	// Arrange.
	s, _ := newTestServer(t, 0)
	defer s.Close()
	painter := register(t, s, RolePainter, nil)
	observer := register(t, s, RoleObserver, nil)
	s.PushWorkspaceState(wsState("w1", frontendv1.RenderState_RENDER_STATE_THINKING))
	s.PushWorkspaceState(wsState("w1", frontendv1.RenderState_RENDER_STATE_READY))
	emitted := states(t, drain(t, painter))
	newest := emitted[len(emitted)-1]

	// Act.
	s.settlePaint("w1", newest.GetGeneration())

	// Assert.
	got := states(t, drain(t, observer))
	if len(got) != 1 {
		t.Fatalf("observer received %d workspace states, want exactly the newest", len(got))
	}
	if got[0].GetState() != frontendv1.RenderState_RENDER_STATE_READY {
		t.Errorf("observer got %s, want READY (the superseded THINKING must never land)", got[0].GetState())
	}
}

// An acknowledgment settles only the workspace it names.
func TestAckSettlesOnlyItsOwnWorkspace(t *testing.T) {
	// Arrange.
	s, _ := newTestServer(t, 0)
	defer s.Close()
	painter := register(t, s, RolePainter, nil)
	observer := register(t, s, RoleObserver, nil)
	s.PushWorkspaceState(wsState("w1", frontendv1.RenderState_RENDER_STATE_THINKING))
	s.PushWorkspaceState(wsState("w2", frontendv1.RenderState_RENDER_STATE_READY))
	emitted := states(t, drain(t, painter))

	// Act: acknowledge w1 only.
	s.settlePaint("w1", emitted[0].GetGeneration())

	// Assert.
	got := states(t, drain(t, observer))
	if len(got) != 1 {
		t.Fatalf("observer received %d workspace states, want 1", len(got))
	}
	if got[0].GetWorkspace() != "w1" {
		t.Errorf("observer got %q, want w1 (w2 is still held)", got[0].GetWorkspace())
	}
}

// ---------------------------------------------------------------------------
// Mode transitions: no painter, and a painter leaving mid-flight
// ---------------------------------------------------------------------------

// EDGE CASE A. With no painter connected there is nobody to wait for, so the
// state settles on emission and observers have it at once.
func TestNoPainterDeliversStraightToObservers(t *testing.T) {
	// Arrange.
	s, _ := newTestServer(t, 0)
	defer s.Close()
	observer := register(t, s, RoleObserver, nil)

	// Act.
	s.PushWorkspaceState(wsState("w1", frontendv1.RenderState_RENDER_STATE_THINKING))

	// Assert.
	got := states(t, drain(t, observer))
	if len(got) != 1 {
		t.Fatalf("observer received %d workspace states, want 1 (nothing to wait for)", len(got))
	}
	if got[0].GetGeneration() == 0 {
		t.Error("a directly delivered state still carries a generation, so a later ack can address it")
	}
}

// EDGE CASE A, the transition. A painter that disappears while a state is held
// cannot ever acknowledge it, so removing it settles the state instead of
// stranding the observer behind a connection that is gone.
func TestPainterDisconnectSettlesTheHeldState(t *testing.T) {
	// Arrange.
	s, _ := newTestServer(t, 0)
	defer s.Close()
	painter := register(t, s, RolePainter, nil)
	observer := register(t, s, RoleObserver, nil)
	s.PushWorkspaceState(wsState("w1", frontendv1.RenderState_RENDER_STATE_THINKING))
	if got := len(states(t, drain(t, observer))); got != 0 {
		t.Fatalf("observer received %d workspace states before the disconnect, want 0", got)
	}

	// Act.
	s.disconnect(painter)

	// Assert: the pending state is delivered, not dropped.
	got := states(t, drain(t, observer))
	if len(got) != 1 {
		t.Fatalf("observer received %d workspace states after the painter left, want 1", len(got))
	}
	if got[0].GetState() != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Errorf("observer got %s, want the held THINKING", got[0].GetState())
	}
}

// The fall back to direct delivery must not DOUBLE-deliver: a state settled by
// a departing painter is delivered exactly once, and a later acknowledgment
// from that connection settles nothing further.
func TestDisconnectSettledStateIsNotDeliveredTwice(t *testing.T) {
	// Arrange.
	s, _ := newTestServer(t, 0)
	defer s.Close()
	painter := register(t, s, RolePainter, nil)
	observer := register(t, s, RoleObserver, nil)
	s.PushWorkspaceState(wsState("w1", frontendv1.RenderState_RENDER_STATE_THINKING))
	emitted := states(t, drain(t, painter))[0]
	s.disconnect(painter)
	_ = drain(t, observer)

	// Act: the departed painter's acknowledgment arrives late.
	s.settlePaint("w1", emitted.GetGeneration())

	// Assert.
	if got := len(states(t, drain(t, observer))); got != 0 {
		t.Fatalf("observer received %d further workspace states, want 0 (already settled once)", got)
	}
}

// A SCOPED painter only answers for its own workspace, so another workspace's
// state is never held behind it.
func TestScopedPainterDoesNotHoldOtherWorkspaces(t *testing.T) {
	// Arrange.
	s, _ := newTestServer(t, 0)
	defer s.Close()
	register(t, s, RolePainter, &Scope{Workspace: "w1"})
	observer := register(t, s, RoleObserver, nil)

	// Act.
	s.PushWorkspaceState(wsState("w2", frontendv1.RenderState_RENDER_STATE_THINKING))

	// Assert.
	got := states(t, drain(t, observer))
	if len(got) != 1 || got[0].GetWorkspace() != "w2" {
		t.Fatalf("observer received %v, want w2 delivered directly (no painter covers it)", got)
	}
}

// ---------------------------------------------------------------------------
// Generations
// ---------------------------------------------------------------------------

func TestGenerationsStrictlyIncreasePerWorkspace(t *testing.T) {
	// Arrange.
	s, _ := newTestServer(t, 0)
	defer s.Close()
	painter := register(t, s, RolePainter, nil)

	// Act.
	for i := 0; i < 3; i++ {
		s.PushWorkspaceState(wsState("w1", frontendv1.RenderState_RENDER_STATE_THINKING))
	}
	got := states(t, drain(t, painter))

	// Assert.
	if len(got) != 3 {
		t.Fatalf("painter received %d states, want 3", len(got))
	}
	for i := 1; i < len(got); i++ {
		if got[i].GetGeneration() <= got[i-1].GetGeneration() {
			t.Fatalf("generation %d did not advance past %d",
				got[i].GetGeneration(), got[i-1].GetGeneration())
		}
	}
}

func TestGenerationsAreIndependentAcrossWorkspaces(t *testing.T) {
	// Arrange.
	s, _ := newTestServer(t, 0)
	defer s.Close()
	painter := register(t, s, RolePainter, nil)

	// Act.
	s.PushWorkspaceState(wsState("w1", frontendv1.RenderState_RENDER_STATE_THINKING))
	s.PushWorkspaceState(wsState("w2", frontendv1.RenderState_RENDER_STATE_THINKING))
	got := states(t, drain(t, painter))

	// Assert: each workspace's counter starts at 1, so an ack for one can never
	// be read as covering the other's emission.
	if got[0].GetGeneration() != 1 || got[1].GetGeneration() != 1 {
		t.Fatalf("generations = %d/%d, want 1/1 (per-workspace counters)",
			got[0].GetGeneration(), got[1].GetGeneration())
	}
}

// ---------------------------------------------------------------------------
// Connect snapshots
// ---------------------------------------------------------------------------

// An observer that connects while a workspace's FIRST emission is still held
// is told nothing about that workspace, rather than being handed a state no
// painter has answered for.
func TestObserverSnapshotOmitsAWorkspaceNeverSettled(t *testing.T) {
	// Arrange.
	s, _ := newTestServer(t, 0)
	defer s.Close()
	register(t, s, RolePainter, nil)
	s.PushWorkspaceState(wsState("w1", frontendv1.RenderState_RENDER_STATE_THINKING))

	// Act.
	s.mu.Lock()
	snap := s.gate.snapshotLocked(&frontendv1.StateSnapshot{
		Workspaces: []*frontendv1.WorkspaceState{wsState("w1", frontendv1.RenderState_RENDER_STATE_THINKING)},
	}, RoleObserver)
	s.mu.Unlock()

	// Assert.
	if got := len(snap.GetWorkspaces()); got != 0 {
		t.Fatalf("observer snapshot carried %d workspaces, want 0 (nothing has settled)", got)
	}
}

// Once a workspace has settled, an observer's snapshot carries the SETTLED
// state, never a newer one still awaiting acknowledgment.
func TestObserverSnapshotCarriesTheLastSettledState(t *testing.T) {
	// Arrange: settle THINKING, then hold READY.
	s, _ := newTestServer(t, 0)
	defer s.Close()
	painter := register(t, s, RolePainter, nil)
	s.PushWorkspaceState(wsState("w1", frontendv1.RenderState_RENDER_STATE_THINKING))
	settled := states(t, drain(t, painter))[0]
	s.settlePaint("w1", settled.GetGeneration())
	s.PushWorkspaceState(wsState("w1", frontendv1.RenderState_RENDER_STATE_READY))

	// Act.
	s.mu.Lock()
	snap := s.gate.snapshotLocked(&frontendv1.StateSnapshot{
		Workspaces: []*frontendv1.WorkspaceState{wsState("w1", frontendv1.RenderState_RENDER_STATE_READY)},
	}, RoleObserver)
	s.mu.Unlock()

	// Assert.
	got := snap.GetWorkspaces()
	if len(got) != 1 {
		t.Fatalf("observer snapshot carried %d workspaces, want 1", len(got))
	}
	if got[0].GetState() != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Errorf("observer snapshot state = %s, want THINKING (READY is still held)", got[0].GetState())
	}
}

// A painter's snapshot carries the HELD emission, so what it renders is
// exactly what its acknowledgment will settle.
func TestPainterSnapshotCarriesTheHeldEmission(t *testing.T) {
	// Arrange.
	s, _ := newTestServer(t, 0)
	defer s.Close()
	register(t, s, RolePainter, nil)
	s.PushWorkspaceState(wsState("w1", frontendv1.RenderState_RENDER_STATE_THINKING))

	// Act: the resolver has since moved on, but the sequencer is still holding
	// the emission the painter must answer for.
	s.mu.Lock()
	snap := s.gate.snapshotLocked(&frontendv1.StateSnapshot{
		Workspaces: []*frontendv1.WorkspaceState{wsState("w1", frontendv1.RenderState_RENDER_STATE_READY)},
	}, RolePainter)
	s.mu.Unlock()

	// Assert.
	got := snap.GetWorkspaces()
	if len(got) != 1 || got[0].GetState() != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("painter snapshot = %v, want the held THINKING emission", got)
	}
}

// A workspace the sequencer has never emitted for is adopted as already
// settled. Nothing is holding it and no painter was ever asked to draw it, so
// holding it would wedge the observer behind an acknowledgment nobody could
// send.
func TestUnseenWorkspaceIsAdoptedAsSettled(t *testing.T) {
	// Arrange: a painter is connected, so the "hold it" branch is the tempting
	// one and this case pins that it is not taken.
	s, _ := newTestServer(t, 0)
	defer s.Close()
	register(t, s, RolePainter, nil)

	// Act.
	s.mu.Lock()
	snap := s.gate.snapshotLocked(&frontendv1.StateSnapshot{
		Workspaces: []*frontendv1.WorkspaceState{wsState("w9", frontendv1.RenderState_RENDER_STATE_IDLE)},
	}, RoleObserver)
	s.mu.Unlock()

	// Assert.
	got := snap.GetWorkspaces()
	if len(got) != 1 || got[0].GetState() != frontendv1.RenderState_RENDER_STATE_IDLE {
		t.Fatalf("observer snapshot = %v, want the adopted IDLE baseline", got)
	}
	if got[0].GetGeneration() == 0 {
		t.Error("an adopted baseline still carries a generation")
	}
}

// ---------------------------------------------------------------------------
// Outcomes
// ---------------------------------------------------------------------------

// EDGE CASE B. A SUSPENDED acknowledgment settles delivery (so a hidden
// webview cannot wedge the tab bar) but attests no paint (so it cannot green a
// workspace it never drew).
func TestPaintOutcomeSemantics(t *testing.T) {
	tests := []struct {
		name          string
		outcome       frontendv1.PaintOutcome
		wantSettles   bool
		wantAttestsOk bool
	}{
		{"painted settles and attests", frontendv1.PaintOutcome_PAINT_OUTCOME_PAINTED, true, true},
		{"suspended settles but attests nothing", frontendv1.PaintOutcome_PAINT_OUTCOME_SUSPENDED, true, false},
		{"unspecified does neither", frontendv1.PaintOutcome_PAINT_OUTCOME_UNSPECIFIED, false, false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			cmd := &frontendv1.PaintAckCmd{Outcome: tc.outcome}
			// Act + Assert.
			if got := settlesDelivery(cmd); got != tc.wantSettles {
				t.Errorf("settlesDelivery = %v, want %v", got, tc.wantSettles)
			}
			if got := AttestsPaint(cmd); got != tc.wantAttestsOk {
				t.Errorf("AttestsPaint = %v, want %v", got, tc.wantAttestsOk)
			}
		})
	}
}

// ---------------------------------------------------------------------------
// Paint-attestation withdrawal (the edges ApplyPaintLost never had)
// ---------------------------------------------------------------------------

// A painter ATTACHING has attested nothing, so whatever the previous renderer
// claimed is withdrawn: this connection has to draw the history it was handed
// before its workspace can be green again.
func TestPainterConnectWithdrawsTheStaleAttestation(t *testing.T) {
	// Arrange.
	rec := &withdrawals{}
	s := New(Config{
		Logf: testLogf(t), State: staticState{snap: sampleSnapshot()}, Handler: &mockHandler{},
		WithdrawPaint: rec.sink,
	})
	defer s.Close()
	httpSrv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		s.ServeWSScoped(w, r, Scope{Workspace: "w1"}, RolePainter, nil)
	}))
	defer httpSrv.Close()

	// Act.
	conn, _, err := websocket.DefaultDialer.Dial("ws"+strings.TrimPrefix(httpSrv.URL, "http"), nil)
	if err != nil {
		t.Fatalf("ws dial: %v", err)
	}
	defer conn.Close()
	if snap := readWSFrame(t, conn); snap.GetSnapshot() == nil {
		t.Fatalf("first WS frame was not a snapshot: %v", snap)
	}

	// Assert.
	got := rec.await(t, 1)
	if len(got) != 1 || got[0] != "w1:painter_connect" {
		t.Fatalf("withdrawals = %v, want [w1:painter_connect]", got)
	}
}

// A painter LEAVING takes its claim with it.
func TestPainterDisconnectWithdrawsTheAttestation(t *testing.T) {
	// Arrange.
	rec := &withdrawals{}
	s := New(Config{
		Logf: testLogf(t), State: staticState{snap: sampleSnapshot()}, Handler: &mockHandler{},
		WithdrawPaint: rec.sink,
	})
	defer s.Close()
	painter := register(t, s, RolePainter, &Scope{Workspace: "w1"})
	s.PushWorkspaceState(wsState("w1", frontendv1.RenderState_RENDER_STATE_READY))

	// Act.
	s.disconnect(painter)

	// Assert.
	if got := rec.list(); len(got) != 1 || got[0] != "w1:painter_disconnect" {
		t.Fatalf("withdrawals = %v, want [w1:painter_disconnect]", got)
	}
}

// An OBSERVER leaving withdraws nothing: it never attested anything, so there
// is no claim of its to retract.
func TestObserverDisconnectWithdrawsNothing(t *testing.T) {
	// Arrange.
	rec := &withdrawals{}
	s := New(Config{
		Logf: testLogf(t), State: staticState{snap: sampleSnapshot()}, Handler: &mockHandler{},
		WithdrawPaint: rec.sink,
	})
	defer s.Close()
	observer := register(t, s, RoleObserver, nil)
	s.PushWorkspaceState(wsState("w1", frontendv1.RenderState_RENDER_STATE_READY))

	// Act.
	s.disconnect(observer)

	// Assert.
	if got := rec.list(); len(got) != 0 {
		t.Fatalf("withdrawals = %v, want none", got)
	}
}

// A workspace another painter still covers keeps its attestation: that
// painter's claim is still current.
func TestDisconnectKeepsAnAttestationAnotherPainterStillCovers(t *testing.T) {
	// Arrange: two painters on the same workspace.
	rec := &withdrawals{}
	s := New(Config{
		Logf: testLogf(t), State: staticState{snap: sampleSnapshot()}, Handler: &mockHandler{},
		WithdrawPaint: rec.sink,
	})
	defer s.Close()
	leaving := register(t, s, RolePainter, &Scope{Workspace: "w1"})
	register(t, s, RolePainter, &Scope{Workspace: "w1"})
	s.PushWorkspaceState(wsState("w1", frontendv1.RenderState_RENDER_STATE_READY))

	// Act.
	s.disconnect(leaving)

	// Assert.
	if got := rec.list(); len(got) != 0 {
		t.Fatalf("withdrawals = %v, want none (a painter still covers w1)", got)
	}
}

// ---------------------------------------------------------------------------
// End to end: a real Emacs socket and a real GUI socket through the daemon
// ---------------------------------------------------------------------------

// THE END-TO-END GUARANTEE, over the production transports: the UDS observer
// (Emacs) is never handed a state the scoped WebSocket painter (the GUI) has
// not answered for, and what it is finally handed is the generation the
// painter acknowledged.
//
// Two emissions make the proof deterministic rather than timing-based: the
// observer's FIRST workspace-state frame is the second emission, which is only
// possible if the first was withheld.
func TestEmacsNeverSeesAStateTheGuiHasNotAcknowledged(t *testing.T) {
	// Arrange: the daemon's two real endpoints.
	s, _ := newTestServer(t, 0)
	defer s.Close()
	l, err := net.Listen("unix", shortSock(t, "seq.sock"))
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	go func() { _ = s.Serve(l) }()
	httpSrv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		s.ServeWSScoped(w, r, Scope{Workspace: "w1", SessionID: "s1"}, RolePainter, nil)
	}))
	defer httpSrv.Close()

	gui, _, err := websocket.DefaultDialer.Dial("ws"+strings.TrimPrefix(httpSrv.URL, "http"), nil)
	if err != nil {
		t.Fatalf("ws dial: %v", err)
	}
	defer gui.Close()
	if snap := readWSFrame(t, gui); snap.GetSnapshot() == nil {
		t.Fatalf("GUI first frame was not a snapshot: %v", snap)
	}
	emacs := retryDialUnix(t, l.Addr().String())
	defer emacs.Close()
	emacsR := bufio.NewReader(emacs)
	if snap := readFrame(t, emacsR); snap.GetSnapshot() == nil {
		t.Fatalf("Emacs first frame was not a snapshot: %v", snap)
	}
	waitClientCount(t, s, 2)

	// Act: two emissions, then acknowledge only the second.
	s.PushWorkspaceState(wsState("w1", frontendv1.RenderState_RENDER_STATE_THINKING))
	s.PushWorkspaceState(wsState("w1", frontendv1.RenderState_RENDER_STATE_READY))
	firstSeen := readWSFrame(t, gui).GetWorkspaceState()
	secondSeen := readWSFrame(t, gui).GetWorkspaceState()
	if firstSeen.GetState() != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("GUI frame 1 = %s, want THINKING", firstSeen.GetState())
	}
	ackData, err := protojson.Marshal(paintAck("w1", secondSeen.GetGeneration(),
		frontendv1.PaintOutcome_PAINT_OUTCOME_PAINTED))
	if err != nil {
		t.Fatalf("marshal ack: %v", err)
	}
	if err := gui.WriteMessage(websocket.TextMessage, ackData); err != nil {
		t.Fatalf("ws write ack: %v", err)
	}

	// Assert: Emacs's first workspace-state frame is the acknowledged one.
	got := readWorkspaceStateFrame(t, emacsR)
	if got.GetGeneration() != secondSeen.GetGeneration() {
		t.Errorf("Emacs got generation %d, want %d (the acknowledged emission)",
			got.GetGeneration(), secondSeen.GetGeneration())
	}
	if got.GetState() != frontendv1.RenderState_RENDER_STATE_READY {
		t.Errorf("Emacs got %s, want READY (THINKING was never acknowledged)", got.GetState())
	}
}
