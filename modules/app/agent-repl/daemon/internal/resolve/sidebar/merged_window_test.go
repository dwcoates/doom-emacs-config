package sidebar_test

import (
	"fmt"
	"testing"
	"time"

	"agentrepl/protohelpers/rosterwalk"

	"agent-repld/internal/wsm"
)

// mergedAt is a merged workspace whose merge landed AGO before epoch.
func mergedAt(id string, ago time.Duration) wsm.Workspace {
	ws := workspace(id, id)
	ws.MergedAt = at(-ago)
	ws.Closed = true
	return ws
}

func TestRecentlyMergedHasNoTimeLimit(t *testing.T) {
	tests := []struct {
		name string
		ago  time.Duration
	}{
		{name: "a merge an hour old is shown", ago: time.Hour},
		{name: "a merge exactly a day old is shown", ago: 24 * time.Hour},
		{name: "a merge a year old is shown", ago: 365 * 24 * time.Hour},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)

			// Act.
			r.SetRegistry(registry(mergedAt("w", tt.ago)))

			// Assert.
			got := rowNames(rosterwalk.MergedRows(latest(t, r)))
			if !equal(got, []string{"w"}) {
				t.Fatalf("merged rows = %v, want [w]", got)
			}
		})
	}
}

func TestRecentlyMergedHasNoCountCap(t *testing.T) {
	// Arrange: more merges than the retired 100-merge cap.
	r, _ := newResolver(t)
	var all []wsm.Workspace
	for i := range 150 {
		all = append(all, mergedAt(fmt.Sprintf("m-%d", i), time.Duration(i)*time.Hour))
	}

	// Act.
	r.SetRegistry(registry(all...))

	// Assert.
	if got := len(rosterwalk.MergedRows(latest(t, r))); got != len(all) {
		t.Fatalf("merged rows = %d, want every one of the %d merges", got, len(all))
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
