package wsedges

import (
	"context"
	"errors"
	"slices"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/wsm"
)

const ws = ids.WorkspaceID("ws-1")

// recorder is an observer that records every edge as "<edge>:<workspace>".
type recorder struct{ got []string }

func (r *recorder) TurnStarted(ws ids.WorkspaceID) { r.got = append(r.got, "turn_started:"+string(ws)) }
func (r *recorder) TurnEnded(ws ids.WorkspaceID)   { r.got = append(r.got, "turn_ended:"+string(ws)) }
func (r *recorder) TurnEndedUnobserved(ws ids.WorkspaceID) {
	r.got = append(r.got, "turn_ended_unobserved:"+string(ws))
}
func (r *recorder) NeedsInput(ws ids.WorkspaceID) { r.got = append(r.got, "needs_input:"+string(ws)) }
func (r *recorder) Landed(ws ids.WorkspaceID)     { r.got = append(r.got, "landed:"+string(ws)) }
func (r *recorder) Closed(before wsm.Workspace)   { r.got = append(r.got, "closed:"+string(before.ID)) }

func TestTheHubTellsEveryObserverEveryEdge(t *testing.T) {
	tests := []struct {
		name string
		edge func(h *Hub)
		want string
	}{
		{name: "a turn starting", edge: func(h *Hub) { h.TurnStarted(ws) }, want: "turn_started:ws-1"},
		{name: "a turn ending", edge: func(h *Hub) { h.TurnEnded(ws) }, want: "turn_ended:ws-1"},
		{name: "a turn ending unobserved", edge: func(h *Hub) { h.TurnEndedUnobserved(ws) }, want: "turn_ended_unobserved:ws-1"},
		{name: "an ask opening", edge: func(h *Hub) { h.NeedsInput(ws) }, want: "needs_input:ws-1"},
		{name: "a landing", edge: func(h *Hub) { h.Landed(ws) }, want: "landed:ws-1"},
		{name: "a close", edge: func(h *Hub) { h.Closed(wsm.Workspace{ID: ws}) }, want: "closed:ws-1"},
		{name: "a merge queue landing", edge: func(h *Hub) {
			h.QueueChanged(merge.QueueChange{Kind: merge.QueueChangeLanded, Subject: merge.QueueMember{Workspace: ws}})
		}, want: "landed:ws-1"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			a, b := &recorder{}, &recorder{}
			h := New(a, b)

			// Act
			tt.edge(h)

			// Assert
			for _, r := range []*recorder{a, b} {
				if !slices.Equal(r.got, []string{tt.want}) {
					t.Errorf("observer told %v, want [%s]", r.got, tt.want)
				}
			}
		})
	}
}

func TestOnlyALandedQueueChangeIsAnEdge(t *testing.T) {
	for _, kind := range []merge.QueueChangeKind{merge.QueueChangeEnqueued, merge.QueueChangeStarted, merge.QueueChangeFailed, merge.QueueChangeDequeued} {
		t.Run(kind.String(), func(t *testing.T) {
			// Arrange
			r := &recorder{}

			// Act
			New(r).QueueChanged(merge.QueueChange{Kind: kind, Subject: merge.QueueMember{Workspace: ws}})

			// Assert
			if len(r.got) != 0 {
				t.Errorf("observer told %v, want nothing", r.got)
			}
		})
	}
}

// closesDB is a registry whose MarkClosed is recorded and may fail.
type closesDB struct {
	wsm.DB
	ws       wsm.Workspace
	readErr  error
	closeErr error
	closed   int
}

func (d *closesDB) Workspace(context.Context, wsm.WorkspaceID) (wsm.Workspace, error) {
	return d.ws, d.readErr
}

func (d *closesDB) MarkClosed(context.Context, wsm.WorkspaceID, wsm.Closure) error {
	if d.closeErr != nil {
		return d.closeErr
	}
	d.closed++
	return nil
}

func TestReportingClosesTellsTheCloseOfAnOpenWorkspace(t *testing.T) {
	tests := []struct {
		name string
		ws   wsm.Workspace
		want []string
	}{
		{name: "an open workspace closing is told", ws: wsm.Workspace{ID: ws}, want: []string{"closed:ws-1"}},
		{name: "re-closing a closed workspace tells nothing", ws: wsm.Workspace{ID: ws, Closed: true}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := &recorder{}
			inner := &closesDB{ws: tt.ws}
			db := ReportingCloses(inner, New(r), dlog.NewTestLogger())

			// Act
			err := db.MarkClosed(context.Background(), ws, wsm.Closure{By: wsm.CloseByUser})

			// Assert
			if err != nil || inner.closed != 1 {
				t.Fatalf("MarkClosed = %v, inner closes = %d, want the close made", err, inner.closed)
			}
			if !slices.Equal(r.got, tt.want) {
				t.Errorf("told %v, want %v", r.got, tt.want)
			}
		})
	}
}

func TestReportingClosesFails(t *testing.T) {
	tests := []struct {
		name    string
		inner   *closesDB
		closes  int
		message string
	}{
		{name: "a workspace that cannot be read is not closed", inner: &closesDB{readErr: errors.New("disk gone")},
			message: "could not read a workspace before closing it, so the close is refused rather than left untold"},
		{name: "a close that fails is told to nobody", inner: &closesDB{ws: wsm.Workspace{ID: ws}, closeErr: errors.New("disk gone")}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r := &recorder{}
			log := dlog.NewTestLogger()
			db := ReportingCloses(tt.inner, New(r), log)

			// Act
			err := db.MarkClosed(context.Background(), ws, wsm.Closure{By: wsm.CloseByUser})

			// Assert
			if err == nil || tt.inner.closed != tt.closes || len(r.got) != 0 {
				t.Fatalf("MarkClosed = %v, closes %d, told %v; want a failure that closes and tells nothing", err, tt.inner.closed, r.got)
			}
			if tt.message != "" {
				if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: op, Message: tt.message}); !ok {
					t.Errorf("no error record: %+v", log.Records())
				}
			}
		})
	}
}
