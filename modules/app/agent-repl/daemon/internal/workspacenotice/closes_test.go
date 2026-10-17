package workspacenotice

import (
	"context"
	"errors"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

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

// pendingKinds answers the kinds the service holds, undelivered.
func pendingKinds(s *Service) []Kind {
	var kinds []Kind
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	s.loop.Run(ctx, func(_ context.Context, e Event) { kinds = append(kinds, e.Kind) })
	return kinds
}

func TestReportingClosesReportsTheCloseOfAnOpenWorkspace(t *testing.T) {
	tests := []struct {
		name string
		ws   wsm.Workspace
		want []Kind
	}{
		{name: "a landed workspace closing reports its landing", ws: wsm.Workspace{ID: child, Landed: &wsm.Landing{}}, want: []Kind{Landed}},
		{name: "an unlanded workspace closing reports closing without landing", ws: wsm.Workspace{ID: child}, want: []Kind{ClosedWithoutLanding}},
		{name: "re-closing a closed workspace reports nothing", ws: wsm.Workspace{ID: child, Closed: true}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture()
			inner := &closesDB{ws: tc.ws}
			db := ReportingCloses(inner, f.s)

			// Act
			err := db.MarkClosed(context.Background(), child, wsm.Closure{By: wsm.CloseByUser})

			// Assert
			if err != nil || inner.closed != 1 {
				t.Fatalf("MarkClosed = %v, inner closes = %d, want the close made", err, inner.closed)
			}
			if got := pendingKinds(f.s); len(got) != len(tc.want) || (len(got) == 1 && got[0] != tc.want[0]) {
				t.Errorf("reported = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestReportingClosesRefusesACloseItCannotDecideTheReportOf(t *testing.T) {
	// Arrange
	f := newFixture()
	inner := &closesDB{readErr: errors.New("disk gone")}
	db := ReportingCloses(inner, f.s)

	// Act
	err := db.MarkClosed(context.Background(), child, wsm.Closure{By: wsm.CloseByUser})

	// Assert
	if err == nil || inner.closed != 0 {
		t.Fatalf("MarkClosed = %v, inner closes = %d, want the close refused", err, inner.closed)
	}
	if _, ok := dlog.FindRecord(f.logs.Records(), dlog.RecordQuery{Level: "error", Operation: op}); !ok {
		t.Errorf("no error record: %+v", f.logs.Records())
	}
}

func TestReportingClosesReportsNothingWhenTheCloseFails(t *testing.T) {
	// Arrange
	f := newFixture()
	inner := &closesDB{ws: wsm.Workspace{ID: child}, closeErr: errors.New("busy")}
	db := ReportingCloses(inner, f.s)

	// Act
	err := db.MarkClosed(context.Background(), child, wsm.Closure{By: wsm.CloseByUser})

	// Assert
	if err == nil {
		t.Fatal("MarkClosed = nil, want the inner failure")
	}
	if got := pendingKinds(f.s); len(got) != 0 {
		t.Errorf("reported = %v, want nothing for a close that failed", got)
	}
}
