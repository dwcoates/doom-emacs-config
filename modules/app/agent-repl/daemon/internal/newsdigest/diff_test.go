package newsdigest

import (
	"fmt"
	"reflect"
	"strings"
	"testing"
	"time"
	"unicode/utf8"
)

// ids answers the ids of entries.
func ids(entries []entry) []string {
	var out []string
	for _, e := range entries {
		out = append(out, e.ID)
	}
	return out
}

func TestAFeedsFirstReadingKeepsOnlyEntriesDatedAfterSince(t *testing.T) {
	// Arrange
	since := now.Add(-24 * time.Hour)
	r := reading{entries: []entry{{ID: "fresh", At: now.Add(-time.Hour)}, {ID: "stale", At: since.Add(-time.Hour)}}}

	// Act
	n, err := diff(feedSource, r, "", false, since)

	// Assert
	if err != nil || !reflect.DeepEqual(ids(n.entries), []string{"fresh"}) || n.count != 1 {
		t.Fatalf("diff = (%+v, %v), want only the fresh entry", n, err)
	}
}

func TestAFeedsFirstReadingKeepsNoUndatedEntry(t *testing.T) {
	// Arrange
	r := reading{entries: []entry{{ID: "2.0.0"}}}

	// Act
	n, err := diff(feedSource, r, "", false, now.Add(-24*time.Hour))

	// Assert
	if err != nil || n.count != 0 {
		t.Fatalf("diff = (%+v, %v), want nothing new", n, err)
	}
}

func TestAFeedEntryIsNewWhenItsIDWasNotSeen(t *testing.T) {
	// Arrange: the unseen entry is OLDER than since; an id decides, not a date.
	r := reading{entries: []entry{{ID: "seen", At: now}, {ID: "unseen", At: now.Add(-48 * time.Hour)}}}

	// Act
	n, err := diff(feedSource, r, `["seen"]`, true, now.Add(-24*time.Hour))

	// Assert
	if err != nil || !reflect.DeepEqual(ids(n.entries), []string{"unseen"}) {
		t.Fatalf("diff = (%+v, %v), want only the unseen entry", n, err)
	}
}

func TestAFeedSnapshotKeepsEveryIDEverSeen(t *testing.T) {
	// Arrange
	r := reading{entries: []entry{{ID: "b", At: now}}}

	// Act
	n, err := diff(feedSource, r, `["a"]`, true, now)

	// Assert
	if err != nil || n.snapshot != `["a","b"]` {
		t.Fatalf("snapshot = %q (%v), want the union of seen ids", n.snapshot, err)
	}
}

func TestAFeedWhoseSnapshotDoesNotDecodeIsRefused(t *testing.T) {
	// Act
	_, err := diff(feedSource, reading{}, "not json", true, now)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "snapshot") {
		t.Fatalf("diff = %v, want a refusal naming the snapshot", err)
	}
}

func TestAFeedHandsTheModelItsNewestEntriesUpToTheBound(t *testing.T) {
	// Arrange
	var r reading
	for i := range maxEntriesPerSource + 5 {
		r.entries = append(r.entries, entry{ID: fmt.Sprint(i), At: now.Add(time.Duration(i) * time.Minute)})
	}

	// Act
	n, err := diff(feedSource, r, `[]`, true, now)

	// Assert
	if err != nil || len(n.entries) != maxEntriesPerSource || n.count != maxEntriesPerSource+5 {
		t.Fatalf("diff = %d entries, count %d (%v), want %d entries and the full count", len(n.entries), n.count, err, maxEntriesPerSource)
	}
	if n.entries[0].ID != fmt.Sprint(maxEntriesPerSource+4) {
		t.Fatalf("first entry = %s, want the newest", n.entries[0].ID)
	}
}

func TestAFeedEntrysBodyIsBounded(t *testing.T) {
	// Arrange
	r := reading{entries: []entry{{ID: "a", At: now, Body: strings.Repeat("x", maxEntryRunes+10)}}}

	// Act
	n, _ := diff(feedSource, r, `[]`, true, now)

	// Assert
	if got := utf8.RuneCountInString(n.entries[0].Body); got != maxEntryRunes {
		t.Fatalf("body runes = %d, want %d", got, maxEntryRunes)
	}
}

func TestAPagesFirstReadingIsOnlyItsBaseline(t *testing.T) {
	// Act
	n, err := diff(pageSource, reading{blocks: []string{"One", "Two"}}, "", false, now)

	// Assert
	if err != nil || n.count != 0 || n.snapshot != "One\nTwo" {
		t.Fatalf("diff = (%+v, %v), want nothing new and the blocks as the snapshot", n, err)
	}
}

func TestAPagesChangedBlocksAreNew(t *testing.T) {
	// Act
	n, err := diff(pageSource, reading{blocks: []string{"One", "Added", "Two"}}, "One\nTwo", true, now)

	// Assert
	if err != nil || n.count != 1 || len(n.entries) != 1 || n.entries[0].Body != "Added" || n.entries[0].Link != pageSource.Home {
		t.Fatalf("diff = (%+v, %v), want one entry holding the added block", n, err)
	}
}

func TestAnUnchangedPageHasNothingNew(t *testing.T) {
	// Act
	n, err := diff(pageSource, reading{blocks: []string{"One"}}, "One", true, now)

	// Assert
	if err != nil || n.count != 0 || n.entries != nil {
		t.Fatalf("diff = (%+v, %v), want nothing new", n, err)
	}
}

func TestAPagesChangedTextIsBounded(t *testing.T) {
	// Act
	n, _ := diff(pageSource, reading{blocks: []string{strings.Repeat("y", maxPageRunes+10)}}, "", true, now)

	// Assert
	if got := utf8.RuneCountInString(n.entries[0].Body); got != maxPageRunes {
		t.Fatalf("body runes = %d, want %d", got, maxPageRunes)
	}
}
