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

// merges is N merged workspaces, m-0 the newest, each an hour older than the last.
func merges(n int) []wsm.Workspace {
	out := make([]wsm.Workspace, n)
	for i := range out {
		out[i] = mergedAt(fmt.Sprintf("m-%d", i), time.Duration(i)*time.Hour)
	}
	return out
}

func TestRecentlyMergedHoldsTheNewestMergesUpToTheCap(t *testing.T) {
	tests := []struct {
		name     string
		merged   int
		wantRows int
	}{
		{name: "one merge is shown", merged: 1, wantRows: 1},
		{name: "exactly the cap is shown whole", merged: sidebar.MergedShown, wantRows: sidebar.MergedShown},
		{name: "one past the cap drops one", merged: sidebar.MergedShown + 1, wantRows: sidebar.MergedShown},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)

			// Act.
			r.SetRegistry(registry(merges(tt.merged)...))

			// Assert.
			if got := len(rosterwalk.MergedRows(latest(t, r))); got != tt.wantRows {
				t.Fatalf("merged rows = %d, want %d", got, tt.wantRows)
			}
		})
	}
}

func TestRecentlyMergedDropsTheOldestMergePastTheCap(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	all := merges(sidebar.MergedShown + 1)
	oldest := all[len(all)-1]

	// Act.
	r.SetRegistry(registry(all...))

	// Assert.
	for _, row := range rosterwalk.AllRows(latest(t, r)) {
		if row.GetWorkspace().GetWorkspace().GetId() == string(oldest.ID) {
			t.Fatalf("the merge past the cap (%s) is still drawn", oldest.ID)
		}
	}
}

func TestRecentlyMergedCountsOnlyTheMergesItHolds(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	r.SetRegistry(registry(merges(sidebar.MergedShown + 5)...))

	// Assert.
	if got := len(rosterwalk.MergedRows(latest(t, r))); got != sidebar.MergedShown {
		t.Fatalf("merged count = %d, want %d", got, sidebar.MergedShown)
	}
}

func TestAMergeOfAnyAgeWithinTheCapIsShown(t *testing.T) {
	// Arrange: a merge a year old, with no newer merge displacing it.
	r, _ := newResolver(t)

	// Act.
	r.SetRegistry(registry(mergedAt("old", 365*24*time.Hour)))

	// Assert.
	if got := len(rosterwalk.MergedRows(latest(t, r))); got != 1 {
		t.Fatalf("merged rows = %d, want the year-old merge shown", got)
	}
}
