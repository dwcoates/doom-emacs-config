package feed

import (
	"slices"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/feedid"
	"agent-repld/internal/sourcescan"
)

func TestEachStoredRowVisitsEveryRowFeedByFeedInOrder(t *testing.T) {
	// Arrange: three answered turns on the root feed.
	h := newHarness(t)
	threeTurns(h)
	h.resolver.mu.Lock()
	defer h.resolver.mu.Unlock()
	s := h.resolver.state(testWorkspace)
	want := append([]string(nil), h.resolver.feed(s, rootFeed()).order...)

	// Act
	var got []string
	h.resolver.eachStoredRow(s, func(_ feedid.Feed, _ *feedState, id string, _ *frontendv1.FeedRow) {
		got = append(got, id)
	})

	// Assert
	if !slices.Equal(got, want) {
		t.Fatalf("visited %v, want the feed's order %v", got, want)
	}
}

func TestEachStoredRowToleratesRetiringRowsMidWalk(t *testing.T) {
	// Arrange
	h := newHarness(t)
	threeTurns(h)
	h.resolver.mu.Lock()
	defer h.resolver.mu.Unlock()
	s := h.resolver.state(testWorkspace)
	total := len(h.resolver.feed(s, rootFeed()).order)

	// Act: every visit retires the row it is handed and the next one.
	visited := 0
	h.resolver.eachStoredRow(s, func(addr feedid.Feed, f *feedState, id string, _ *frontendv1.FeedRow) {
		visited++
		at := slices.Index(f.order, id)
		h.resolver.retire(s, addr, id)
		if at >= 0 && at < len(f.order) {
			h.resolver.retire(s, addr, f.order[at])
		}
	})

	// Assert: each visit retired two rows, and no retired row was visited.
	if want := (total + 1) / 2; visited != want {
		t.Fatalf("visited %d rows of %d, want %d", visited, total, want)
	}
}

func TestEveryStoredRowSweepGoesThroughEachStoredRow(t *testing.T) {
	// Arrange: the order snapshot a safe sweep needs is taken in one place.
	var offenders []string

	// Act
	for _, file := range sourcescan.Production(t) {
		if file.Name == "feeds.go" {
			continue
		}
		if strings.Contains(string(file.Source), "append([]string(nil), f.order...)") {
			offenders = append(offenders, file.Name)
		}
	}

	// Assert
	if len(offenders) > 0 {
		t.Fatalf("hand-rolled stored-row sweeps (use eachStoredRow): %s", strings.Join(offenders, ", "))
	}
}
