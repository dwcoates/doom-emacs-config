package sessionwatcher

import (
	"slices"
	"sync"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// TestEdgeMutexUnlockWithNothingQueuedDeliversNothing pins the fast path: a
// hold that queued no edge releases the mutex and records no delivery.
func TestEdgeMutexUnlockWithNothingQueuedDeliversNothing(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	m := &edgeMutex{log: log}

	// Act.
	m.Lock()
	m.Unlock()

	// Assert.
	if !m.TryLock() {
		t.Fatal("the mutex is still held after Unlock")
	}
	m.Mutex.Unlock()
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Operation: "daemon.sessionwatcher.view_edges_delivered"}); ok {
		t.Fatalf("records = %v, want no delivery recorded for a hold that queued nothing", log.Records())
	}
}

// TestEdgeMutexDeliversQueuedEdgesInOrderOffTheMutex pins the delivery: every
// edge a hold queued is handed over before its Unlock returns, in the order
// queued, with the mutex already released, and the delivery is recorded.
func TestEdgeMutexDeliversQueuedEdgesInOrderOffTheMutex(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	m := &edgeMutex{log: log}
	var got []string
	var heldDuring []bool
	edge := func(name string) func() {
		return func() {
			free := m.TryLock()
			if free {
				m.Mutex.Unlock()
			}
			heldDuring = append(heldDuring, !free)
			got = append(got, name)
		}
	}

	// Act.
	m.Lock()
	m.later(edge("first"))
	m.later(edge("second"))
	m.Unlock()

	// Assert.
	if want := []string{"first", "second"}; !slices.Equal(got, want) {
		t.Fatalf("delivered %v, want %v", got, want)
	}
	if slices.Contains(heldDuring, true) {
		t.Fatalf("held during delivery = %v, want the mutex released before every edge", heldDuring)
	}
	rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "debug", Operation: "daemon.sessionwatcher.view_edges_delivered"})
	if !ok {
		t.Fatalf("records = %v, want a view_edges_delivered record", log.Records())
	}
	if rec.Context["edges"] != 2 {
		t.Fatalf("edges = %v, want 2", rec.Context["edges"])
	}
}

// TestEdgeMutexOrdersTwoHoldsEdgesAndReturnsOnlyOnceItsOwnAreDelivered pins
// the guarantee across goroutines: a second hold's edge, queued while the
// first hold's edge is still being delivered, is delivered after it, and the
// second Unlock does not return before its own edge has been delivered.
func TestEdgeMutexOrdersTwoHoldsEdgesAndReturnsOnlyOnceItsOwnAreDelivered(t *testing.T) {
	// Arrange: the first hold's edge blocks mid-delivery until released.
	m := &edgeMutex{}
	var mu sync.Mutex
	var got []string
	record := func(name string) {
		mu.Lock()
		defer mu.Unlock()
		got = append(got, name)
	}
	firstStarted := make(chan struct{})
	releaseFirst := make(chan struct{})
	firstDone := make(chan struct{})
	go func() {
		defer close(firstDone)
		m.Lock()
		m.later(func() {
			close(firstStarted)
			<-releaseFirst
			record("first")
		})
		m.Unlock()
	}()
	<-firstStarted

	// Act: the second hold queues while the first edge is mid-delivery, and
	// notes what had been delivered when its own Unlock returned.
	secondReturned := make(chan struct{})
	var atReturn []string
	go func() {
		defer close(secondReturned)
		m.Lock()
		m.later(func() { record("second") })
		m.Unlock()
		mu.Lock()
		atReturn = slices.Clone(got)
		mu.Unlock()
	}()
	close(releaseFirst)

	// Assert.
	for _, done := range []chan struct{}{firstDone, secondReturned} {
		select {
		case <-done:
		case <-time.After(waitDeadline):
			t.Fatal("an Unlock never returned")
		}
	}
	if want := []string{"first", "second"}; !slices.Equal(got, want) {
		t.Fatalf("delivered %v, want %v", got, want)
	}
	if !slices.Contains(atReturn, "second") {
		t.Fatalf("delivered when the second Unlock returned = %v, want its own edge among them", atReturn)
	}
}

// TestEdgeMutexAwaitDeliveredJoinsADeliveryInFlight pins Close's join: a
// delivery begun before awaitDelivered is called is waited for.
func TestEdgeMutexAwaitDeliveredJoinsADeliveryInFlight(t *testing.T) {
	// Arrange: a delivery blocked mid-edge.
	m := &edgeMutex{}
	started := make(chan struct{})
	release := make(chan struct{})
	delivered := false
	go func() {
		m.Lock()
		m.later(func() {
			close(started)
			<-release
			delivered = true
		})
		m.Unlock()
	}()
	<-started

	// Act.
	joined := make(chan struct{})
	go func() {
		m.awaitDelivered()
		close(joined)
	}()
	close(release)

	// Assert.
	select {
	case <-joined:
	case <-time.After(waitDeadline):
		t.Fatal("awaitDelivered never returned")
	}
	if !delivered {
		t.Fatal("awaitDelivered returned before the delivery in flight ended")
	}
}

// lockProbeSidebar is a roster sink that records, on each session update,
// whether the watcher's lock was free when the roster was told.
type lockProbeSidebar struct {
	*sidebarSink
	w    *watcher
	free []bool
}

func (s *lockProbeSidebar) OnSessionUpdate(ws ids.WorkspaceID, update *conversationv1.SessionUpdate) {
	s.free = append(s.free, probeFree(s.w))
	s.sidebarSink.OnSessionUpdate(ws, update)
}

// lockProbeTopbar is lockProbeSidebar for the topbar.
type lockProbeTopbar struct {
	*topbarSink
	w    *watcher
	free []bool
}

func (s *lockProbeTopbar) OnSessionUpdate(ws ids.WorkspaceID, update *conversationv1.SessionUpdate) {
	s.free = append(s.free, probeFree(s.w))
	s.topbarSink.OnSessionUpdate(ws, update)
}

// probeFree answers whether the watcher's lock is free, leaving it as found.
func probeFree(w *watcher) bool {
	if !w.mu.TryLock() {
		return false
	}
	w.mu.Mutex.Unlock()
	return true
}

// TestTheRosterAndTopbarAreToldOffTheWatchersLock pins the fix for the
// adoption convoy (2026-11-12) and the topbar's store write (2026-11-10): a
// session update reaches the roster and the topbar with the watcher's lock
// released, while every other view is still told under it.
func TestTheRosterAndTopbarAreToldOffTheWatchersLock(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	sidebar := &lockProbeSidebar{sidebarSink: &sidebarSink{rec: h.rec}, w: h.w}
	topbar := &lockProbeTopbar{topbarSink: &topbarSink{rec: h.rec}, w: h.w}
	h.w.mu.Lock()
	h.w.sinks.Sidebar = sidebar
	h.w.sinks.Topbar = topbar
	h.w.mu.Unlock()

	// Act.
	got := h.routeNow(func(w *watcher) { w.routeSessionUpdateLocked(diagnosticsUpdate()) })

	// Assert.
	assertNames(t, got, []string{"lifecycle.OnSessionDiagnostics", "footer.OnSessionUpdate", "topbar.OnSessionUpdate", "sidebar.OnSessionUpdate"})
	if !slices.Equal(sidebar.free, []bool{true}) {
		t.Fatalf("roster told with the lock free = %v, want [true]", sidebar.free)
	}
	if !slices.Equal(topbar.free, []bool{true}) {
		t.Fatalf("topbar told with the lock free = %v, want [true]", topbar.free)
	}
}
