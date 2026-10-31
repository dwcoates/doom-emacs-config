package sidebar_test

import (
	"fmt"
	"testing"
	"time"

	"agentrepl/protohelpers/rosterwalk"

	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"
)

// mergedAt is a merged workspace whose merge landed AGO before epoch.
func mergedAt(id string, ago time.Duration) wsm.Workspace {
	ws := workspace(id, id)
	ws.MergedAt = at(-ago)
	ws.Closed = true
	return ws
}

func TestRecentlyMergedHoldsADayOfMerges(t *testing.T) {
	tests := []struct {
		name string
		ago  time.Duration
		want []string
	}{
		{name: "a merge an hour old is shown", ago: time.Hour, want: []string{"w"}},
		{name: "a merge a second short of a day old is shown", ago: sidebar.MergedWindow - time.Second, want: []string{"w"}},
		{name: "a merge exactly a day old is dropped", ago: sidebar.MergedWindow, want: nil},
		{name: "a merge older than a day is dropped", ago: sidebar.MergedWindow + time.Hour, want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)

			// Act.
			r.SetRegistry(registry(mergedAt("w", tt.ago)))

			// Assert.
			got := rowNames(rosterwalk.MergedRows(latest(t, r)))
			if !equal(got, tt.want) {
				t.Fatalf("merged rows = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestRecentlyMergedHasNoCountCap(t *testing.T) {
	// Arrange: more merges inside the day than the retired 100-merge cap.
	r, _ := newResolver(t)
	var all []wsm.Workspace
	for i := range 150 {
		all = append(all, mergedAt(fmt.Sprintf("m-%d", i), time.Duration(i)*time.Minute))
	}

	// Act.
	r.SetRegistry(registry(all...))

	// Assert.
	if got := len(rosterwalk.MergedRows(latest(t, r))); got != len(all) {
		t.Fatalf("merged rows = %d, want every one of the %d merges inside the day", got, len(all))
	}
}

func TestRecentlyMergedListsTheNewestMergeFirst(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	r.SetRegistry(registry(mergedAt("older", 3*time.Hour), mergedAt("newer", time.Hour)))

	// Assert.
	if got := rowNames(rosterwalk.MergedRows(latest(t, r))); !equal(got, []string{"newer", "older"}) {
		t.Fatalf("merged rows = %v, want newest first", got)
	}
}

func TestAMergeOlderThanADayIsDrawnNowhere(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	old := mergedAt("old", 2*sidebar.MergedWindow)

	// Act.
	r.SetRegistry(registry(old))

	// Assert.
	for _, row := range rosterwalk.AllRows(latest(t, r)) {
		if row.GetWorkspace().GetWorkspace().GetId() == string(old.ID) {
			t.Fatalf("the merge older than the window (%s) is still drawn", old.ID)
		}
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

func TestARenderWithTheSameOldestMergeKeepsTheArmedAgingOut(t *testing.T) {
	// Arrange.
	timers := &manualTimers{}
	r, _ := newResolver(t, sidebar.WithClock(func() time.Time { return epoch }, timers.after))
	r.SetRegistry(registry(mergedAt("w", time.Hour)))

	// Act.
	r.SetRegistry(registry(mergedAt("w", time.Hour)))

	// Assert: the first timer stays armed, and no second one is.
	if got := len(timers.armed); got != 1 {
		t.Fatalf("armed = %d timers, want the second render to keep the first", got)
	}
}

func TestARenderWithANewOldestMergeReplacesTheArmedAgingOut(t *testing.T) {
	// Arrange.
	timers := &manualTimers{}
	r, _ := newResolver(t, sidebar.WithClock(func() time.Time { return epoch }, timers.after))
	r.SetRegistry(registry(mergedAt("w", time.Hour)))

	// Act: an older merge joins, so the band ages out sooner.
	r.SetRegistry(registry(mergedAt("w", time.Hour), mergedAt("older", 2*time.Hour)))

	// Assert: exactly one timer stays armed, waiting for the older merge.
	live := timers.live()
	if len(live) != 1 || live[0].wait != sidebar.MergedWindow-2*time.Hour {
		t.Fatalf("armed = %d live timers, want one waiting %v", len(live), sidebar.MergedWindow-2*time.Hour)
	}
}

func TestTheLastMergeLeavingStopsTheArmedAgingOut(t *testing.T) {
	// Arrange.
	timers := &manualTimers{}
	r, _ := newResolver(t, sidebar.WithClock(func() time.Time { return epoch }, timers.after))
	r.SetRegistry(registry(mergedAt("w", time.Hour)))

	// Act.
	r.SetRegistry(registry(workspace("live", "live")))

	// Assert.
	if got := len(timers.live()); got != 0 {
		t.Fatalf("armed = %d live timers, want none once no merge is shown", got)
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
	if got := rowNames(rosterwalk.MergedRows(latest(t, r))); len(got) != 0 {
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
