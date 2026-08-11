package frontend

import (
	"io"
	"sync"
	"sync/atomic"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"google.golang.org/protobuf/encoding/protojson"
)

// recordingConn hands every written frame to the test and blocks its read loop
// until the test closes it, so a connect is observed by RENDEZVOUS with the
// frames rather than by waiting out a clock.
type recordingConn struct {
	frames chan []byte
	block  chan struct{}
	once   sync.Once
}

func newRecordingConn() *recordingConn {
	return &recordingConn{frames: make(chan []byte, 64), block: make(chan struct{})}
}

func (c *recordingConn) writeFrame(b []byte, done func()) error {
	c.frames <- append([]byte(nil), b...)
	if done != nil {
		done()
	}
	return nil
}

func (c *recordingConn) readCommand() (*frontendv1.FrontendCommand, error) {
	<-c.block
	return nil, io.EOF
}

func (c *recordingConn) close(closeCause) error {
	c.once.Do(func() { close(c.block) })
	return nil
}

// nextFrame decodes the next frame the connection was given.
func (c *recordingConn) nextFrame(t *testing.T) *frontendv1.FrontendFrame {
	t.Helper()
	frame := &frontendv1.FrontendFrame{}
	if err := protojson.Unmarshal(<-c.frames, frame); err != nil {
		t.Fatalf("decode connect frame: %v", err)
	}
	return frame
}

// mutatingState is the deferred boot sweep in miniature: every composition of
// the snapshot is overtaken by a WorkspaceState publication that crosses the
// delivery lock, which is exactly the condition the old connect loop retried
// on — forever, because the sweep kept producing it for ~23s after boot.
type mutatingState struct {
	server *Server
	calls  atomic.Int64
}

func (m *mutatingState) Snapshot() *frontendv1.StateSnapshot {
	n := m.calls.Add(1)
	// The publication lands AFTER this composition's content, so the captured
	// snapshot is stale by construction on every attempt.
	m.server.PushWorkspaceState(&frontendv1.WorkspaceState{
		Workspace: "/w", SessionId: "s1", AtMs: 100 + n,
		State: frontendv1.RenderState_RENDER_STATE_THINKING,
	})
	return &frontendv1.StateSnapshot{Workspaces: []*frontendv1.WorkspaceState{
		{Workspace: "/w", SessionId: "s1", AtMs: n, State: frontendv1.RenderState_RENDER_STATE_IDLE},
	}}
}

func newMutatingServer(t *testing.T) (*Server, *mutatingState) {
	t.Helper()
	state := &mutatingState{}
	s := New(Config{
		Logf:        testLogf(t),
		LogVerbosef: testLogf(t),
		State:       state,
		Handler:     &mockHandler{},
	})
	state.server = s
	return s, state
}

// A host connect composes exactly ONE snapshot even though state moved under
// it. The retry loop composed a fresh full snapshot per attempt — seconds each
// at this user's roster — and could not terminate while the boot sweep mutated
// state, which is what made the host wait ~30s for the frame that gates every
// downstream recovery signal.
func TestHostConnectComposesOneSnapshotWhileStateMutates(t *testing.T) {
	// Arrange.
	s, state := newMutatingServer(t)
	c := newRecordingConn()

	// Act: the connect completes when its first frames reach the transport.
	go s.serveClient(c, nil, ClientKindHost)
	c.nextFrame(t)
	defer c.close(causeServerClosed)

	// Assert.
	if got := state.calls.Load(); got != 1 {
		t.Fatalf("snapshot compositions = %d, want exactly 1 regardless of concurrent state", got)
	}
}

// The snapshot the client is served is repaired, not raced: the state the
// transport published after the capture rides immediately behind it, so the
// client's per-workspace view ends at the delivered revision.
func TestHostConnectServesTheOvertakenStateAsACatchUpFrame(t *testing.T) {
	// Arrange.
	s, _ := newMutatingServer(t)
	c := newRecordingConn()

	// Act.
	go s.serveClient(c, nil, ClientKindHost)
	snapshot := c.nextFrame(t)
	catchUp := c.nextFrame(t)
	defer c.close(causeServerClosed)

	// Assert: snapshot first, then the newer per-workspace frame.
	if got := snapshot.GetSnapshot().GetWorkspaces()[0].GetAtMs(); got != 1 {
		t.Fatalf("connect snapshot at_ms = %d, want the captured revision 1", got)
	}
	state := catchUp.GetWorkspaceState()
	if state.GetWorkspace() != "/w" || state.GetAtMs() != 101 {
		t.Fatalf("catch-up frame = %v, want /w at the delivered revision 101", state)
	}
	if state.GetState() != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("catch-up state = %v, want the published THINKING content, not the snapshot's", state.GetState())
	}
}

// A connect completes while another goroutine mutates state CONTINUOUSLY, which
// is the shape of the deferred boot sweep. Completion is the assertion; the
// mutations stop only once the connect has been served.
func TestHostConnectCompletesUnderContinuousMutation(t *testing.T) {
	// Arrange.
	s, _ := newMutatingServer(t)
	c := newRecordingConn()
	stop := make(chan struct{})
	churn := make(chan struct{})
	go func() {
		defer close(churn)
		for at := int64(1); ; at++ {
			select {
			case <-stop:
				return
			default:
			}
			s.PushWorkspaceState(&frontendv1.WorkspaceState{Workspace: "/other", SessionId: "s2", AtMs: at})
		}
	}()

	// Act.
	go s.serveClient(c, nil, ClientKindHost)
	frame := c.nextFrame(t)
	close(stop)
	<-churn
	defer c.close(causeServerClosed)

	// Assert.
	if frame.GetSnapshot() == nil {
		t.Fatalf("first connect frame = %T, want the state snapshot", frame.GetFrame())
	}
}

// A scoped client is repaired only for ITS workspace: the catch-up pass must
// not leak another workspace's state to a session-scoped connection.
func TestScopedConnectCatchUpStaysInsideItsScope(t *testing.T) {
	// Arrange.
	s := New(Config{
		Logf: testLogf(t), LogVerbosef: testLogf(t),
		State: staticState{snap: &frontendv1.StateSnapshot{Workspaces: []*frontendv1.WorkspaceState{
			{Workspace: "/w", SessionId: "s1", AtMs: 1},
		}}},
		Handler: &mockHandler{},
	})
	s.PushWorkspaceState(&frontendv1.WorkspaceState{Workspace: "/other", SessionId: "s2", AtMs: 9})
	s.PushWorkspaceState(&frontendv1.WorkspaceState{Workspace: "/w", SessionId: "s1", AtMs: 7})

	// Act.
	s.mu.Lock()
	catchUp, err := s.snapshotCatchUpLocked(
		&frontendv1.StateSnapshot{Workspaces: []*frontendv1.WorkspaceState{{Workspace: "/w", SessionId: "s1", AtMs: 1}}},
		&Scope{Workspace: "/w", SessionID: "s1"})
	s.mu.Unlock()

	// Assert.
	if err != nil {
		t.Fatalf("snapshotCatchUpLocked: %v", err)
	}
	if len(catchUp) != 1 || catchUp[0].workspace != "/w" {
		t.Fatalf("catch-up = %v, want only the scoped workspace", catchUp)
	}
}

// A snapshot already carrying the delivered revision needs no repair at all.
func TestConnectCatchUpIsEmptyWhenTheSnapshotIsCurrent(t *testing.T) {
	// Arrange.
	s := New(Config{Logf: testLogf(t), LogVerbosef: testLogf(t), State: staticState{}, Handler: &mockHandler{}})
	s.PushWorkspaceState(&frontendv1.WorkspaceState{Workspace: "/w", SessionId: "s1", AtMs: 5})

	// Act.
	s.mu.Lock()
	catchUp, err := s.snapshotCatchUpLocked(
		&frontendv1.StateSnapshot{Workspaces: []*frontendv1.WorkspaceState{{Workspace: "/w", SessionId: "s1", AtMs: 5}}}, nil)
	s.mu.Unlock()

	// Assert.
	if err != nil {
		t.Fatalf("snapshotCatchUpLocked: %v", err)
	}
	if len(catchUp) != 0 {
		t.Fatalf("catch-up = %v, want none for a current snapshot", catchUp)
	}
}
