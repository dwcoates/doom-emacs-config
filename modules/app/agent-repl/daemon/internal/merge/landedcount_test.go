package merge

import (
	"context"
	"errors"
	"testing"

	"agent-repld/internal/gitclient"
)

// TestLandedCount covers the count a landed merge stamps: the commits its
// merge commit brought in, and none when that range was not read.
func TestLandedCount(t *testing.T) {
	cases := []struct {
		name    string
		commits []gitclient.Commit
		want    *uint32
	}{
		{name: "one commit per commit landed", commits: []gitclient.Commit{{SHA: "a"}, {SHA: "b"}, {SHA: "c"}}, want: ptrUint32(3)},
		{name: "no count for an unread range", commits: nil, want: nil},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := landedCount(outcome{landed: "abc", commits: tc.commits})

			// Assert
			if (got == nil) != (tc.want == nil) || (got != nil && *got != *tc.want) {
				t.Fatalf("landedCount = %v, want %v", deref(got), deref(tc.want))
			}
		})
	}
}

// TestALandedMergeStampsTheCommitsItBroughtIn covers the merged row's count:
// it is the landed range's length, written with merged_at.
func TestALandedMergeStampsTheCommitsItBroughtIn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.landed = []gitclient.Commit{{SHA: "c1"}, {SHA: "c2"}, {SHA: "abc123def4567"}}
	h.gatePasses("daemon")
	enqueue(t, h)

	// Act
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	if got, ok := h.db.landedCommits[theWorkspace]; !ok || got != 3 {
		t.Fatalf("landed commit count = %d (stamped %v), want 3", got, ok)
	}
}

// TestALandedMergeWithAnUnreadRangeKeepsTheRecordedCount covers the landing
// whose range would not read: merged_at is stamped, the count is left alone.
func TestALandedMergeWithAnUnreadRangeKeepsTheRecordedCount(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.landed, h.git.landedErr = nil, errors.New("rev-list: bad object")
	h.gatePasses("daemon")
	enqueue(t, h)

	// Act
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	if h.db.mergedAt[theWorkspace].IsZero() {
		t.Fatal("merged_at was not stamped")
	}
	if got, ok := h.db.landedCommits[theWorkspace]; ok {
		t.Fatalf("a count of %d was stamped from a range that did not read", got)
	}
}

// TestABranchAlreadyOnTheTargetKeepsTheRecordedCount covers the no-op merge:
// nothing landed, so it stamps no count.
func TestABranchAlreadyOnTheTargetKeepsTheRecordedCount(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.emacsRepo()
	h.git.contained = true
	enqueue(t, h)

	// Act
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	if h.db.mergedAt[theWorkspace].IsZero() {
		t.Fatal("merged_at was not stamped")
	}
	if got, ok := h.db.landedCommits[theWorkspace]; ok {
		t.Fatalf("a count of %d was stamped for a merge that landed nothing", got)
	}
}

func ptrUint32(n uint32) *uint32 { return &n }

func deref(p *uint32) any {
	if p == nil {
		return nil
	}
	return *p
}
