package server

import (
	"context"
	"fmt"
	"net"
	"net/http"
	"net/http/httptest"
	"sync"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/progress"
	"claude-repld/internal/registry"

	"github.com/gorilla/websocket"
	"google.golang.org/protobuf/encoding/protojson"
	"google.golang.org/protobuf/types/known/anypb"
)

const (
	raceWorkspace = "/pending/new"
	raceSession   = "s_pending"
	raceJob       = "job-pending"
)

// delayedMaterializationBridge is the host-creation seam with an explicit
// release latch.  The test, not elapsed time, controls when Emacs has finished
// materializing the workspace.
type delayedMaterializationBridge struct {
	mu             sync.Mutex
	decision       SessionPublicationDecision
	releases       chan SessionPublicationRelease
	available      chan *frontendv1.WorkspaceAvailable
	pauseLookup    bool
	lookupEntered  chan struct{}
	continueLookup chan struct{}
}

func newDelayedMaterializationBridge() *delayedMaterializationBridge {
	return &delayedMaterializationBridge{
		decision:  SessionPublicationDecision{JobID: raceJob, WorktreePath: raceWorkspace, SessionID: raceSession},
		releases:  make(chan SessionPublicationRelease, 8),
		available: make(chan *frontendv1.WorkspaceAvailable, 8),
	}
}

func (b *delayedMaterializationBridge) MarkWorkspaceMaterialized(_ context.Context, jobID string) error {
	if jobID != raceJob {
		return fmt.Errorf("unexpected materialization job %q", jobID)
	}
	return b.release()
}

func (*delayedMaterializationBridge) CompleteHostAction(context.Context, string, bool, string) error {
	return nil
}

func (*delayedMaterializationBridge) PostprocessingPrompt(string) (string, error) { return "", nil }

func (*delayedMaterializationBridge) BeforeWSMergePrompt(string) (string, error) { return "", nil }

func (b *delayedMaterializationBridge) SessionPublicationDecision(worktree, session string) (SessionPublicationDecision, error) {
	b.mu.Lock()
	if worktree != raceWorkspace {
		b.mu.Unlock()
		return SessionPublicationDecision{WorktreePath: worktree, SessionID: session, Materialized: true}, nil
	}
	decision := b.decision
	pause, entered, continueLookup := b.pauseLookup, b.lookupEntered, b.continueLookup
	b.pauseLookup = false
	b.mu.Unlock()
	if pause {
		close(entered)
		<-continueLookup
		b.mu.Lock()
		decision = b.decision
		b.mu.Unlock()
	}
	return decision, nil
}

func (b *delayedMaterializationBridge) SubscribeSessionPublicationReleases() (<-chan SessionPublicationRelease, func()) {
	return b.releases, func() {}
}

func (*delayedMaterializationBridge) SnapshotHostWork() WorkspaceHostWorkSnapshot {
	return WorkspaceHostWorkSnapshot{}
}

func (b *delayedMaterializationBridge) SubscribeWorkspaceAvailable() (<-chan *frontendv1.WorkspaceAvailable, func()) {
	return b.available, func() {}
}

func (*delayedMaterializationBridge) SubscribeHostActions() (<-chan *frontendv1.HostAction, func()) {
	ch := make(chan *frontendv1.HostAction)
	return ch, func() { close(ch) }
}

func (b *delayedMaterializationBridge) release() error {
	completion := make(chan error, 1)
	open := func() error {
		b.mu.Lock()
		defer b.mu.Unlock()
		if b.decision.Materialized {
			return fmt.Errorf("duplicate materialization release for %q", raceJob)
		}
		b.decision.Materialized = true
		return nil
	}
	b.releases <- SessionPublicationRelease{JobID: raceJob, WorktreePath: raceWorkspace, SessionID: raceSession, Open: open, Completion: completion}
	if err := <-completion; err != nil {
		return fmt.Errorf("authoritative publication: %w", err)
	}
	return nil
}

func (b *delayedMaterializationBridge) pauseNextLookup() (<-chan struct{}, chan<- struct{}) {
	b.mu.Lock()
	defer b.mu.Unlock()
	b.pauseLookup = true
	b.lookupEntered = make(chan struct{})
	b.continueLookup = make(chan struct{})
	return b.lookupEntered, b.continueLookup
}

func newPreMaterializationRaceHarness(t *testing.T) (*AgentShim, *progress.Manager, *delayedMaterializationBridge, *websocket.Conn) {
	t.Helper()
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: raceSession, CWD: raceWorkspace}); err != nil {
		t.Fatalf("record pending session: %v", err)
	}
	prog := progress.New(progress.Options{Logf: func(string, ...any) {}, CoalesceWindow: -1, UncachedAlertTokens: 1})
	t.Cleanup(func() { _ = prog.Close() })
	bridge := newDelayedMaterializationBridge()
	shim, err := WireAgentShim(AgentShimConfig{
		Resumes:           &fakeResumes{},
		SSM:               openTestSSM(t, reg),
		Progress:          prog,
		Prompts:           &fakePrompts{},
		Turns:             &fakePrompts{},
		Lifecycle:         &fakeLifecycle{},
		SessionDeaths:     stubSessionDeaths{},
		SessionCommands:   &SessionCommandBinding{},
		Sessions:          fakeSessions{views: []*frontendv1.SessionView{{Workspace: raceWorkspace, SessionId: raceSession}}},
		Inits:             fakeInits{inits: []*frontendv1.SessionInitView{{Workspace: raceWorkspace, Fence: raceSession}}},
		WorkspaceCreation: bridge,
		MergeLease:        stubMergeLease{},
		MergeQueue:        newTestMergeQueue(t),
		LogVerbosef:       t.Logf,
	})
	if err != nil {
		t.Fatalf("WireAgentShim: %v", err)
	}
	t.Cleanup(func() { _ = shim.Close() })
	httpServer := httptest.NewServer(http.HandlerFunc(shim.Server.ServeWS))
	t.Cleanup(httpServer.Close)
	conn, _, err := websocket.DefaultDialer.Dial("ws"+httpServer.URL[len("http"):], nil)
	if err != nil {
		t.Fatalf("dial frontend: %v", err)
	}
	t.Cleanup(func() { _ = conn.Close() })
	if frame := raceReadFrame(t, conn); frame.GetSnapshot() == nil {
		t.Fatalf("first frame = %T, want StateSnapshot", frame.GetFrame())
	}
	return shim, prog, bridge, conn
}

func raceReadFrame(t *testing.T, conn *websocket.Conn) *frontendv1.FrontendFrame {
	t.Helper()
	if err := conn.SetReadDeadline(time.Now().Add(2 * time.Second)); err != nil {
		t.Fatalf("set read deadline: %v", err)
	}
	_, bytes, err := conn.ReadMessage()
	if err != nil {
		t.Fatalf("read frame: %v", err)
	}
	frame := &frontendv1.FrontendFrame{}
	if err := protojson.Unmarshal(bytes, frame); err != nil {
		t.Fatalf("decode frame: %v", err)
	}
	return frame
}

func raceAssertNoSessionFrame(t *testing.T, shim *AgentShim) {
	t.Helper()
	// A gorilla connection that observes a read deadline is poisoned for later
	// reads.  The observer is deliberately disposable; the connection that
	// receives the post-ACK snapshot never takes an absence timeout.
	conn := newPreMaterializationRaceConn(t, shim)
	if frame := raceReadFrame(t, conn); frame.GetSnapshot() == nil {
		t.Fatalf("absence observer first frame = %T, want StateSnapshot", frame.GetFrame())
	}
	if err := conn.SetReadDeadline(time.Now().Add(100 * time.Millisecond)); err != nil {
		t.Fatalf("set absence deadline: %v", err)
	}
	_, _, err := conn.ReadMessage()
	if err == nil {
		t.Fatal("received a frontend frame before materialization released the session")
	}
	if netErr, ok := err.(net.Error); !ok || !netErr.Timeout() {
		t.Fatalf("read while publication gated: %v", err)
	}
}

func raceApplyCost(t *testing.T, prog *progress.Manager) {
	t.Helper()
	if err := prog.Apply(raceWorkspace, raceSession, &corev1.Event{
		SessionId: raceSession, ProducedAtMs: 1,
		Payload: &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{TurnId: "pre-materialization-turn"}},
	}); err != nil {
		t.Fatalf("start pending turn: %v", err)
	}
	any, err := anypb.New(&datav1.ClaudeStreamMessage{Msg: &datav1.ClaudeStreamMessage_Result{Result: &datav1.ResultMessage{Usage: &datav1.Usage{InputTokens: 9}}}})
	if err != nil {
		t.Fatalf("encode cost result: %v", err)
	}
	if err := prog.Apply(raceWorkspace, raceSession, &corev1.Event{SessionId: raceSession, ProducedAtMs: 2, Payload: &corev1.Event_Vendor{Vendor: any}}); err != nil {
		t.Fatalf("apply pending cost: %v", err)
	}
}

// TestPreMaterializationPublicationGateDrainsEveryOrdering is the acceptance
// harness for the creation gate.  It delays Emacs materialization while the
// shim-side state and cost facts arrive, then demands an authoritative first
// post-ACK render rather than replaying lossy pre-ACK frames.
func TestPreMaterializationPublicationGateDrainsEveryOrdering(t *testing.T) {
	shim, prog, bridge, conn := newPreMaterializationRaceHarness(t)

	// State/progress before WorkspaceAvailable remain completely outside the
	// frontend stream.  The host-only available descriptor is then delivered
	// while Emacs deliberately withholds its materialization acknowledgement.
	if err := shim.SSM.ApplyMergeTransition(raceWorkspace, "merging", "shim ready before host materialization"); err != nil {
		t.Fatalf("apply first state: %v", err)
	}
	raceApplyCost(t, prog)
	raceAssertNoSessionFrame(t, shim)
	bridge.available <- &frontendv1.WorkspaceAvailable{JobId: raceJob, FinalName: "new", WorktreePath: raceWorkspace, SessionId: raceSession}

	// More state and context-derived progress after WorkspaceAvailable but
	// before ACK still cannot cross the session publication boundary.
	if err := shim.SSM.ApplyMergeTransition(raceWorkspace, "merged", "more progress while host materialization is delayed"); err != nil {
		t.Fatalf("apply delayed state: %v", err)
	}
	prog.SetCounts(raceWorkspace, 3, 2)
	raceAssertNoSessionFrame(t, shim)

	// A reconnect during the gate is a snapshot, not a publication leak.
	second := newPreMaterializationRaceConn(t, shim)
	snap := raceReadFrame(t, second).GetSnapshot()
	if snap == nil {
		t.Fatal("reconnect did not begin with a StateSnapshot")
	}
	if len(snap.GetWorkspaces()) != 0 || len(snap.GetProgress()) != 0 {
		t.Fatalf("gated reconnect snapshot leaked session state workspaces=%v progress=%v", snap.GetWorkspaces(), snap.GetProgress())
	}

	// The gate's decision is paused while the ACK commits.  This is a concrete
	// interleaving rather than scheduler luck: an emission begins on the old
	// side of the latch and completes only after the release is durable.
	entered, continueLookup := bridge.pauseNextLookup()
	emitted := make(chan struct{})
	go func() {
		shim.Server.PushWorkspaceState(&frontendv1.WorkspaceState{
			Workspace: raceWorkspace,
			SessionId: raceSession,
			State:     frontendv1.RenderState_RENDER_STATE_MERGED,
		})
		close(emitted)
	}()
	<-entered
	released := make(chan error, 1)
	go func() { released <- bridge.release() }()
	select {
	case err := <-released:
		t.Fatalf("materialization release completed while the publication lookup held the reader lock: %v", err)
	default:
		// The lookup's confirmed entry proves it holds the frontend reader lock.
	}
	close(continueLookup)
	<-emitted
	if err := <-released; err != nil {
		t.Fatalf("release materialization: %v", err)
	}
	// A prompt-derived delta submitted by the creation worker immediately after
	// release must queue after the authoritative snapshot.  The synchronous
	// completion receipt makes the ordering a property of the handshake rather
	// than a race between the worker and the frontend goroutine.
	shim.Server.PushConversationDelta(&frontendv1.ConversationDelta{Workspace: raceWorkspace, Fence: raceSession})
	prog.SetCounts(raceWorkspace, 5, 4)

	raceAssertAuthoritativeRelease(t, conn)
}

func raceAssertAuthoritativeRelease(t *testing.T, conn *websocket.Conn) {
	t.Helper()
	for deadline := time.Now().Add(2 * time.Second); time.Now().Before(deadline); {
		frame := raceReadFrame(t, conn)
		if delta := frame.GetConversationDelta(); delta != nil && delta.GetWorkspace() == raceWorkspace {
			t.Fatal("prompt-derived conversation delta arrived before the authoritative post-ACK snapshot")
		}
		snapshot := frame.GetSnapshot()
		if snapshot == nil {
			continue
		}
		var state *frontendv1.WorkspaceState
		for _, candidate := range snapshot.GetWorkspaces() {
			if candidate.GetWorkspace() == raceWorkspace {
				state = candidate
			}
		}
		if state == nil || state.GetState() != frontendv1.RenderState_RENDER_STATE_MERGED {
			t.Fatalf("post-ACK snapshot state = %v, want final merged state", state)
		}
		var view *frontendv1.ProgressView
		for _, candidate := range snapshot.GetProgress() {
			if candidate.GetWorkspace() == raceWorkspace {
				view = candidate
			}
		}
		if view == nil || view.GetExpensiveTurn().GetTurnId() != "pre-materialization-turn" {
			t.Fatalf("post-ACK snapshot progress = %v, want preserved context-cost alert", view)
		}
		if len(snapshot.GetSessions()) != 1 || snapshot.GetSessions()[0].GetSessionId() != raceSession {
			t.Fatalf("post-ACK snapshot sessions = %v, want the released session", snapshot.GetSessions())
		}
		if len(snapshot.GetInits()) != 1 || snapshot.GetInits()[0].GetFence() != raceSession {
			t.Fatalf("post-ACK snapshot inits = %v, want the released session init", snapshot.GetInits())
		}
		return
	}
	t.Fatal("no authoritative post-ACK snapshot arrived")
}

func newPreMaterializationRaceConn(t *testing.T, shim *AgentShim) *websocket.Conn {
	t.Helper()
	httpServer := httptest.NewServer(http.HandlerFunc(shim.Server.ServeWS))
	t.Cleanup(httpServer.Close)
	conn, _, err := websocket.DefaultDialer.Dial("ws"+httpServer.URL[len("http"):], nil)
	if err != nil {
		t.Fatalf("dial reconnect: %v", err)
	}
	t.Cleanup(func() { _ = conn.Close() })
	return conn
}
