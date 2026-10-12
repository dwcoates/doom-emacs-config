package sidebar_test

import (
	"testing"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"
)

// mergedRows is every row of the Recently Merged band, across its repository
// groups in wire order.
func mergedRows(roster *frontendv1.WorkspaceRoster) []*frontendv1.RosterRow {
	var out []*frontendv1.RosterRow
	for _, group := range roster.GetRecentlyMerged().GetGroups() {
		out = append(out, group.GetRows().GetRows()...)
	}
	return out
}

// mergedAt is a merged workspace whose merge landed AGO before epoch.
func mergedAt(id string, ago time.Duration) wsm.Workspace {
	ws := workspace(id, id)
	ws.MergedAt = at(-ago)
	return ws
}

func TestRecentlyMergedHoldsAWeekOfMerges(t *testing.T) {
	tests := []struct {
		name string
		ago  time.Duration
		want []string
	}{
		{name: "a merge an hour old is shown", ago: time.Hour, want: []string{"w"}},
		{name: "a merge a second short of a week old is shown", ago: sidebar.MergedWindow - time.Second, want: []string{"w"}},
		{name: "a merge exactly a week old is dropped", ago: sidebar.MergedWindow, want: nil},
		{name: "a merge older than a week is dropped", ago: sidebar.MergedWindow + time.Hour, want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)

			// Act.
			r.SetRegistry(registry(mergedAt("w", tt.ago)))

			// Assert.
			got := rowNames(mergedRows(latest(t, r)))
			if !equal(got, tt.want) {
				t.Fatalf("merged rows = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestRecentlyMergedCountsOnlyTheWeek(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	r.SetRegistry(registry(mergedAt("new", time.Hour), mergedAt("old", 2*sidebar.MergedWindow)))

	// Assert.
	if got := latest(t, r).GetRecentlyMerged().GetHeader().GetCount().GetWorkspaces(); got != 1 {
		t.Fatalf("merged count = %d, want the one merge inside the week", got)
	}
}

func TestAMergeOlderThanAWeekIsDrawnInNoGrouping(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	r.SetRegistry(registry(mergedAt("old", 2*sidebar.MergedWindow)))

	// Assert.
	if got := len(latest(t, r).GetRepository().GetSections()[0].GetRows().GetRows()); got != 0 {
		t.Fatalf("repository rows = %d, want the aged merge drawn nowhere", got)
	}
}

func TestTheOldestShownMergeArmsItsAgingOut(t *testing.T) {
	// Arrange.
	timers := &manualTimers{}
	r, _ := newResolver(t, sidebar.WithClock(func() time.Time { return epoch }, timers.after))

	// Act.
	r.SetRegistry(registry(mergedAt("new", time.Hour), mergedAt("older", 3*time.Hour)))

	// Assert: the wait is until the OLDER merge leaves the window.
	live := timers.live()
	if len(live) != 1 || live[0].wait != sidebar.MergedWindow-3*time.Hour {
		t.Fatalf("armed = %d timers, want one waiting %v", len(live), sidebar.MergedWindow-3*time.Hour)
	}
}

func TestNoMergeArmsNoAgingOut(t *testing.T) {
	// Arrange.
	timers := &manualTimers{}
	r, _ := newResolver(t, sidebar.WithClock(func() time.Time { return epoch }, timers.after))

	// Act.
	r.SetRegistry(registry(workspace("live", "live")))

	// Assert.
	if got := len(timers.live()); got != 0 {
		t.Fatalf("armed = %d timers, want none with no merge shown", got)
	}
}

func TestARenderReplacesTheArmedAgingOut(t *testing.T) {
	// Arrange.
	timers := &manualTimers{}
	r, _ := newResolver(t, sidebar.WithClock(func() time.Time { return epoch }, timers.after))
	r.SetRegistry(registry(mergedAt("w", time.Hour)))

	// Act.
	r.SetRegistry(registry(mergedAt("w", time.Hour)))

	// Assert: exactly one timer stays armed.
	if got := len(timers.live()); got != 1 {
		t.Fatalf("armed = %d timers, want the second render to replace the first", got)
	}
}

func TestAgingOutDropsTheMergeFromTheBand(t *testing.T) {
	// Arrange: a merge a minute from aging out.
	now := epoch
	timers := &manualTimers{}
	r, surfaces := newResolver(t, sidebar.WithClock(func() time.Time { return now }, timers.after))
	r.SetRegistry(registry(mergedAt("w", sidebar.MergedWindow-time.Minute)))
	armed := timers.live()
	if len(armed) != 1 {
		t.Fatalf("armed = %d timers, want one", len(armed))
	}

	// Act: the minute passes and the timer fires.
	now = epoch.Add(time.Minute)
	armed[0].fire()

	// Assert.
	if got := rowNames(mergedRows(latest(t, r))); len(got) != 0 {
		t.Fatalf("merged rows = %v, want the aged merge dropped", got)
	}
	logged := false
	for _, rec := range surfaces.Records() {
		logged = logged || rec.Operation == "daemon.sidebar.merged_aged_out"
	}
	if !logged {
		t.Fatal("the aging-out re-render was not logged")
	}
}
