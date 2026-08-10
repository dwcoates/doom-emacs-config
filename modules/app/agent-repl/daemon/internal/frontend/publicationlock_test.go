package frontend

import (
	"errors"
	"sync"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"google.golang.org/protobuf/encoding/protojson"
)

// publicationlock_test.go covers the LOCK-ORDER half of the materialization
// release: the release must never hold a server lock across the injected open
// and snapshot functions, because both reach into subsystems that push back
// into this server.
//
// The production failure this guards was a genuine ABBA cycle. The release held
// publicationMu's writer side, called the snapshot provider, and the provider
// blocked on the workspace-view publisher's mutex — which was itself held by a
// goroutine blocked pushing a topbar back through Broadcast, behind the
// release's pending writer. Every session controller and the merge queue drain
// wedged behind it, twice.

// releaseDeadline is how long a test waits for a release before declaring the
// cycle back. It is a FAILURE deadline, never a synchronization device: the
// passing path never reaches it.
const releaseDeadline = 30 * time.Second

// awaitRelease reports the release's outcome, or fails the test if the release
// never returns — which is exactly what the deadlock looked like.
func awaitRelease(t *testing.T, done <-chan error) error {
	t.Helper()
	select {
	case err := <-done:
		return err
	case <-time.After(releaseDeadline):
		t.Fatal("ReleaseSessionPublication never returned — the release is holding a lock its snapshot provider needs")
		return nil
	}
}

// publicationLatch is the durable materialization decision as the frontend sees
// it: closed until the release's open function opens it.
type publicationLatch struct {
	mu     sync.Mutex
	open   bool
	failed error
}

func (l *publicationLatch) allowed(string, string) (bool, error) {
	l.mu.Lock()
	defer l.mu.Unlock()
	return l.open, nil
}

func (l *publicationLatch) openFn() error {
	l.mu.Lock()
	defer l.mu.Unlock()
	if l.failed != nil {
		return l.failed
	}
	l.open = true
	return nil
}

// newLatchedServer builds a server whose publication gate is the latch, with
// one unscoped host client registered to observe delivery.
func newLatchedServer(t *testing.T, latch *publicationLatch) (*Server, *client) {
	t.Helper()
	s := New(Config{
		Logf:                      testLogf(t),
		LogVerbosef:               testLogf(t),
		State:                     staticState{snap: sampleSnapshot()},
		Handler:                   &mockHandler{},
		SessionPublicationAllowed: latch.allowed,
	})
	cl := &client{id: 1, out: newOutbox(8), done: make(chan struct{}), kind: ClientKindHost}
	s.clients[cl] = struct{}{}
	return s, cl
}

func topbarFrameFor(workspace string) *frontendv1.FrontendFrame {
	return TopbarViewFrame(&frontendv1.TopbarView{Workspace: workspace, Fence: "s1|g1"})
}

// THE REGRESSION. A snapshot provider that pushes back into the server — which
// is what the real one does, transitively, through the workspace-view publisher
// — must not be able to wedge the release that called it.
func TestReleaseSessionPublicationSurvivesASnapshotProviderThatPushesBack(t *testing.T) {
	// Arrange — the snapshot provider broadcasts a session-scoped frame from
	// another goroutine and waits for that broadcast to complete before it
	// answers, the shape the SSM/workspace-view path has in production.
	latch := &publicationLatch{}
	s, _ := newLatchedServer(t, latch)
	defer s.Close()
	var pushed sync.WaitGroup
	snapshot := func() *frontendv1.StateSnapshot {
		returned := make(chan int, 1)
		pushed.Add(1)
		go func() {
			defer pushed.Done()
			returned <- s.Broadcast(topbarFrameFor("/w"))
		}()
		<-returned
		return sampleSnapshot()
	}

	// Act.
	done := make(chan error, 1)
	go func() { done <- s.ReleaseSessionPublication(latch.openFn, snapshot) }()
	err := awaitRelease(t, done)
	pushed.Wait()

	// Assert.
	if err != nil {
		t.Fatalf("ReleaseSessionPublication: %v", err)
	}
}

// A frame that arrives inside the release window is PARKED, not dropped, and it
// reaches the client after the authoritative snapshot.
func TestAFramePushedDuringTheReleaseWindowIsDeliveredAfterTheSnapshot(t *testing.T) {
	// Arrange.
	latch := &publicationLatch{}
	s, cl := newLatchedServer(t, latch)
	defer s.Close()
	var pushed sync.WaitGroup
	snapshot := func() *frontendv1.StateSnapshot {
		returned := make(chan int, 1)
		pushed.Add(1)
		go func() {
			defer pushed.Done()
			returned <- s.Broadcast(topbarFrameFor("/w"))
		}()
		<-returned
		return sampleSnapshot()
	}

	// Act.
	done := make(chan error, 1)
	go func() { done <- s.ReleaseSessionPublication(latch.openFn, snapshot) }()
	if err := awaitRelease(t, done); err != nil {
		t.Fatalf("ReleaseSessionPublication: %v", err)
	}
	pushed.Wait()

	// Assert — the snapshot first, the parked topbar second.
	first := &frontendv1.FrontendFrame{}
	if err := protojson.Unmarshal(mustPop(t, cl), first); err != nil {
		t.Fatalf("decode first frame: %v", err)
	}
	if first.GetSnapshot() == nil {
		t.Fatalf("first frame = %T, want the authoritative snapshot", first.GetFrame())
	}
	second := &frontendv1.FrontendFrame{}
	if err := protojson.Unmarshal(mustPop(t, cl), second); err != nil {
		t.Fatalf("decode second frame: %v", err)
	}
	if second.GetTopbar().GetWorkspace() != "/w" {
		t.Fatalf("second frame = %T, want the topbar parked during the window", second.GetFrame())
	}
}

// A release whose open FAILS still closes its window. Leaving the hold engaged
// would park every session-scoped frame forever, which is the deadlock wearing
// a different hat.
func TestAFailedReleaseStillClosesItsPublicationWindow(t *testing.T) {
	// Arrange — open fails, so the durable latch never opens.
	latch := &publicationLatch{failed: errors.New("durable open refused")}
	s, cl := newLatchedServer(t, latch)
	defer s.Close()

	// Act.
	done := make(chan error, 1)
	go func() {
		done <- s.ReleaseSessionPublication(latch.openFn, func() *frontendv1.StateSnapshot {
			t.Error("snapshot resolved after open failed")
			return nil
		})
	}()
	if err := awaitRelease(t, done); err == nil {
		t.Fatal("ReleaseSessionPublication = nil, want the open failure")
	}
	latch.mu.Lock()
	latch.failed = nil
	latch.open = true
	latch.mu.Unlock()
	delivered := s.Broadcast(topbarFrameFor("/w"))

	// Assert — the window is closed, so a later frame goes out rather than
	// parking behind a release that already returned.
	if delivered != 1 {
		t.Fatalf("delivered = %d, want the frame to reach the client after the window closed", delivered)
	}
	if _, ok := cl.out.pop(); !ok {
		t.Fatal("client outbox is empty, want the post-window frame")
	}
}
