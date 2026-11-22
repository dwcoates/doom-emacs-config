package main

import (
	"reflect"
	"sync"
	"testing"
	"time"

	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// heldSinks are the roster's three writes, recorded, and held mid-write on
// their gate for the first workspace named "ws-held" until it is closed.
type heldSinks struct {
	mu       sync.Mutex
	results  []*wsm.TurnResult
	statuses []time.Time
	visible  []wsm.Visibility
	gate     chan struct{}
	entered  chan struct{}
}

func newHeldSinks() *heldSinks {
	return &heldSinks{gate: make(chan struct{}), entered: make(chan struct{}, 3)}
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

func (h *heldSinks) visibility(ws ids.WorkspaceID, v wsm.Visibility) {
	if h.hold(ws) {
		return
	}
	h.mu.Lock()
	defer h.mu.Unlock()
	h.visible = append(h.visible, v)
}

// writes is the roster's writes over SINKS.
func (h *heldSinks) writes() *rosterWrites {
	return newRosterWrites(h.result, h.status, h.visibility)
}

func TestRosterWritesReturnWhileTheWriteIsHeld(t *testing.T) {
	tests := []struct {
		name   string
		submit func(w *rosterWrites)
	}{
		{name: "a result", submit: func(w *rosterWrites) { w.ResultSink()("ws-held", &wsm.TurnResult{End: "completed"}) }},
		{name: "a status change", submit: func(w *rosterWrites) { w.StatusChangeSink()("ws-held", statusChangeInstant) }},
		{name: "a visibility", submit: func(w *rosterWrites) {
			w.VisibilitySink()(map[ids.WorkspaceID]wsm.Visibility{"ws-held": wsm.NotVisible})
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the state database's write is held.
			sinks := newHeldSinks()
			w := sinks.writes()

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
	w := sinks.writes()
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
	w := sinks.writes()
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

func TestRosterWritesKeepTheLatestVisibility(t *testing.T) {
	// Arrange
	sinks := newHeldSinks()
	w := sinks.writes()
	w.VisibilitySink()(map[ids.WorkspaceID]wsm.Visibility{"ws-held": wsm.Selected})
	<-sinks.entered

	// Act: the workspace is shown, then stops being shown, before either is
	// written.
	w.VisibilitySink()(map[ids.WorkspaceID]wsm.Visibility{"ws-1": wsm.UnselectedVisible})
	w.VisibilitySink()(map[ids.WorkspaceID]wsm.Visibility{"ws-1": wsm.NotVisible})
	close(sinks.gate)
	w.Close()

	// Assert
	if !reflect.DeepEqual(sinks.visible, []wsm.Visibility{wsm.NotVisible}) {
		t.Fatalf("visibilities written = %v, want only the latest", sinks.visible)
	}
}

func TestRosterWritesCloseWritesWhatIsHeld(t *testing.T) {
	// Arrange
	sinks := newHeldSinks()
	w := sinks.writes()
	result := &wsm.TurnResult{End: "completed"}
	w.ResultSink()("ws-1", result)
	w.StatusChangeSink()("ws-1", statusChangeInstant)
	w.VisibilitySink()(map[ids.WorkspaceID]wsm.Visibility{"ws-1": wsm.Selected})

	// Act
	w.Close()

	// Assert
	if !reflect.DeepEqual(sinks.results, []*wsm.TurnResult{result}) || !reflect.DeepEqual(sinks.statuses, []time.Time{statusChangeInstant}) ||
		!reflect.DeepEqual(sinks.visible, []wsm.Visibility{wsm.Selected}) {
		t.Fatalf("written = %v, %v, %v; want every change written by Close", sinks.results, sinks.statuses, sinks.visible)
	}
}
