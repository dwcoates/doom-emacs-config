package main

import (
	"reflect"
	"sync"
	"testing"
	"time"

	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// heldSinks are the roster's two writes, recorded, and held mid-write on
// their gate for the first workspace named "ws-held" until it is closed.
type heldSinks struct {
	mu       sync.Mutex
	results  []*wsm.TurnResult
	statuses []time.Time
	gate     chan struct{}
	entered  chan struct{}
}

func newHeldSinks() *heldSinks {
	return &heldSinks{gate: make(chan struct{}), entered: make(chan struct{}, 2)}
}

func (h *heldSinks) hold(ws ids.WorkspaceID) bool {
	if ws != "ws-held" {
		return false
	}
	h.entered <- struct{}{}
	<-h.gate
	return true
}

func (h *heldSinks) result(ws ids.WorkspaceID, result *wsm.TurnResult) {
	if h.hold(ws) {
		return
	}
	h.mu.Lock()
	defer h.mu.Unlock()
	h.results = append(h.results, result)
}

func (h *heldSinks) status(ws ids.WorkspaceID, at time.Time) {
	if h.hold(ws) {
		return
	}
	h.mu.Lock()
	defer h.mu.Unlock()
	h.statuses = append(h.statuses, at)
}

func TestRosterWritesReturnWhileTheWriteIsHeld(t *testing.T) {
	tests := []struct {
		name   string
		submit func(w *rosterWrites)
	}{
		{name: "a result", submit: func(w *rosterWrites) { w.ResultSink()("ws-held", &wsm.TurnResult{End: "completed"}) }},
		{name: "a status change", submit: func(w *rosterWrites) { w.StatusChangeSink()("ws-held", statusChangeInstant) }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the state database's write is held.
			sinks := newHeldSinks()
			w := newRosterWrites(sinks.result, sinks.status)

			// Act
			tt.submit(w)

			// Assert: the sink returned, and its write is running on the
			// writer's goroutine.
			<-sinks.entered
			close(sinks.gate)
			w.Close()
		})
	}
}

func TestRosterWritesKeepTheNewerResult(t *testing.T) {
	// Arrange: a write is held, so two results of one workspace wait behind it.
	sinks := newHeldSinks()
	w := newRosterWrites(sinks.result, sinks.status)
	w.ResultSink()("ws-held", nil)
	<-sinks.entered
	first, second := &wsm.TurnResult{End: "failed"}, &wsm.TurnResult{End: "completed"}

	// Act
	w.ResultSink()("ws-1", first)
	w.ResultSink()("ws-1", second)
	close(sinks.gate)
	w.Close()

	// Assert
	if !reflect.DeepEqual(sinks.results, []*wsm.TurnResult{second}) {
		t.Fatalf("results written = %v, want only the newer one", sinks.results)
	}
}

func TestRosterWritesKeepTheLaterStatusChange(t *testing.T) {
	// Arrange
	sinks := newHeldSinks()
	w := newRosterWrites(sinks.result, sinks.status)
	w.StatusChangeSink()("ws-held", statusChangeInstant)
	<-sinks.entered
	earlier, later := statusChangeInstant.Add(time.Minute), statusChangeInstant.Add(time.Hour)

	// Act: the later instant arrives first, as a slower report overtaken.
	w.StatusChangeSink()("ws-1", later)
	w.StatusChangeSink()("ws-1", earlier)
	close(sinks.gate)
	w.Close()

	// Assert
	if !reflect.DeepEqual(sinks.statuses, []time.Time{later}) {
		t.Fatalf("instants written = %v, want only the later one", sinks.statuses)
	}
}

func TestRosterWritesCloseWritesWhatIsHeld(t *testing.T) {
	// Arrange
	sinks := newHeldSinks()
	w := newRosterWrites(sinks.result, sinks.status)
	result := &wsm.TurnResult{End: "completed"}
	w.ResultSink()("ws-1", result)
	w.StatusChangeSink()("ws-1", statusChangeInstant)

	// Act
	w.Close()

	// Assert
	if !reflect.DeepEqual(sinks.results, []*wsm.TurnResult{result}) || !reflect.DeepEqual(sinks.statuses, []time.Time{statusChangeInstant}) {
		t.Fatalf("written = %v, %v; want both changes written by Close", sinks.results, sinks.statuses)
	}
}
