package sidebar_test

import (
	"sync"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/wsm"
)

// ---- the update recency the editor's switcher orders by ----

// rowsEverywhere is every row the roster draws: the repository grouping's,
// the hidden section's and the Recently Merged band's.
func rowsEverywhere(roster *frontendv1.WorkspaceRoster) []*frontendv1.RosterRow {
	var out []*frontendv1.RosterRow
	for _, section := range roster.GetRepository().GetSections() {
		out = append(out, section.GetRows().GetRows()...)
	}
	out = append(out, roster.GetHidden().GetRows().GetRows()...)
	return append(out, mergedRows(roster)...)
}

func TestTheRowStatesTheLatestUpdateItsRecordHolds(t *testing.T) {
	tests := []struct {
		name string
		edit func(*wsm.Workspace)
		want time.Time
	}{
		{name: "a fresh workspace was last updated when it was created", edit: func(*wsm.Workspace) {}, want: epoch},
		{name: "a turn start or close", edit: func(ws *wsm.Workspace) { ws.LastActivityAt = at(time.Minute) }, want: *at(time.Minute)},
		{name: "a turn end", edit: func(ws *wsm.Workspace) { ws.LastTurnEndedAt = at(2 * time.Minute) }, want: *at(2 * time.Minute)},
		{name: "a close", edit: func(ws *wsm.Workspace) {
			ws.Closed = true
			ws.Closure = &wsm.Closure{At: *at(3 * time.Minute), By: wsm.CloseByUser}
		}, want: *at(3 * time.Minute)},
		{name: "a hide", edit: func(ws *wsm.Workspace) { ws.Hidden, ws.HiddenAt = true, at(4*time.Minute) }, want: *at(4 * time.Minute)},
		{name: "a merge landing", edit: func(ws *wsm.Workspace) { ws.MergedAt = at(5 * time.Minute) }, want: *at(5 * time.Minute)},
		{name: "a requester's landing", edit: func(ws *wsm.Workspace) { ws.Landed = &wsm.Landing{Head: "abc", At: *at(6 * time.Minute)} }, want: *at(6 * time.Minute)},
		{name: "a durable status change", edit: func(ws *wsm.Workspace) { ws.StatusChangedAt = at(7 * time.Minute) }, want: *at(7 * time.Minute)},
		{name: "the latest of several wins", edit: func(ws *wsm.Workspace) {
			ws.LastActivityAt, ws.LastTurnEndedAt, ws.StatusChangedAt = at(9*time.Minute), at(8*time.Minute), at(time.Minute)
		}, want: *at(9 * time.Minute)},
		{name: "a selection is not an update", edit: func(ws *wsm.Workspace) { ws.LastSelectedAt = at(time.Hour) }, want: epoch},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)
			rec := workspace(string(theWS), "one")
			tt.edit(&rec)

			// Act.
			r.SetRegistry(registry(rec))

			// Assert.
			row := rowFor(rowsEverywhere(latest(t, r)), string(theWS))
			if row == nil {
				t.Fatalf("no row for %s", theWS)
			}
			if got := row.GetLastUpdated().GetAtMs(); got != tt.want.UnixMilli() {
				t.Fatalf("last_updated = %d, want %d", got, tt.want.UnixMilli())
			}
		})
	}
}

// statusChangeReport is one call of the status change sink.
type statusChangeReport struct {
	ws ids.WorkspaceID
	at time.Time
}

// statusChangeResolver builds a live resolver whose one workspace's durable
// record holds DURABLE as its status change, read at a clock the test moves,
// recording every status change it reports. The bring-up's own arm moves are
// reported before it returns, and are dropped.
func statusChangeResolver(t *testing.T, durable *time.Time) (sidebar.Resolver, *time.Time, func() []statusChangeReport) {
	t.Helper()
	var mu sync.Mutex
	var reports []statusChangeReport
	now := epoch
	r, _ := newResolver(t,
		sidebar.WithClock(func() time.Time { return now }, (&manualTimers{}).after),
		sidebar.WithStatusChangeSink(func(ws ids.WorkspaceID, at time.Time) {
			mu.Lock()
			defer mu.Unlock()
			reports = append(reports, statusChangeReport{ws: ws, at: at})
		}))
	rec := workspace(string(theWS), "one")
	rec.StatusChangedAt = durable
	r.SetRegistry(registry(rec))
	r.OnLink(theWS, shimclient.LinkConnected)
	r.OnSessionStarted(theWS, &conversationv1.SessionStarted{VendorSessionId: "vendor-1"})
	mu.Lock()
	reports = nil
	mu.Unlock()
	return r, &now, func() []statusChangeReport {
		mu.Lock()
		defer mu.Unlock()
		return append([]statusChangeReport(nil), reports...)
	}
}

func TestAStatusChangeIsStampedAndReportedOnce(t *testing.T) {
	// Arrange.
	r, now, reports := statusChangeResolver(t, nil)
	*now = epoch.Add(time.Hour)

	// Act: the arm moves from ready to submitting.
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})

	// Assert.
	row := onlyRow(t, r)
	if got := statusName(row); got != "submitting" {
		t.Fatalf("status = %q, want submitting", got)
	}
	if got := row.GetLastUpdated().GetAtMs(); got != now.UnixMilli() {
		t.Fatalf("last_updated = %d, want the status change at %d", got, now.UnixMilli())
	}
	got := reports()
	if len(got) != 1 || got[0].ws != theWS || !got[0].at.Equal(*now) {
		t.Fatalf("reports = %+v, want one status change for %s at %v", got, theWS, *now)
	}
}

func TestARenderThatMovesNoArmReportsNoStatusChange(t *testing.T) {
	// Arrange.
	r, now, reports := statusChangeResolver(t, nil)
	*now = epoch.Add(time.Hour)

	// Act: a registry snapshot that moves no arm.
	r.SetRegistry(registry(workspace(string(theWS), "one")))

	// Assert.
	if got := reports(); len(got) != 0 {
		t.Fatalf("reports = %+v, want none", got)
	}
	if got := onlyRow(t, r).GetLastUpdated().GetAtMs(); got >= now.UnixMilli() {
		t.Fatalf("last_updated = %d, want an instant before the unmoved render at %d", got, now.UnixMilli())
	}
}

func TestADurableStatusChangeLaterThanTheLiveOneWins(t *testing.T) {
	// Arrange: the record holds a status change later than any this daemon saw.
	durable := epoch.Add(2 * time.Hour)
	r, now, _ := statusChangeResolver(t, &durable)
	*now = epoch.Add(time.Hour)

	// Act.
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})

	// Assert.
	if got := onlyRow(t, r).GetLastUpdated().GetAtMs(); got != durable.UnixMilli() {
		t.Fatalf("last_updated = %d, want the durable status change at %d", got, durable.UnixMilli())
	}
}
