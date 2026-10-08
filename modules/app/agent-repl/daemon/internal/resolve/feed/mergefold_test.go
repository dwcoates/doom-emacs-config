package feed

import (
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/feedid"
)

// A MERGE BUBBLE NEVER COLLAPSES ON ITS OWN (owner ruling, 2026-10-08): a push
// never folds an open bubble, the reader's fold is recorded on the durable
// head, and a new daemon draws it again.

// foldedHead is a merge head on the root feed carrying FOLDED.
func foldedHead(folded bool) *frontendv1.FeedRow {
	row := mergeHeadRow("branch → master")
	row.GetActivity().GetMerge().GetHead().Fold = &frontendv1.FeedMergeFold{Folded: folded}
	return row
}

// headFold answers the stored head's fold on R's root feed.
func headFold(t *testing.T, r *resolver) bool {
	t.Helper()
	rows := feedRows(r, rootFeed())
	if len(rows) != 1 {
		t.Fatalf("root rows = %d, want the one head", len(rows))
	}
	fold, ok := mergeHeadFold(rows[0])
	if !ok {
		t.Fatal("the root row is not a merge head with a fold")
	}
	return fold.GetFolded()
}

func TestAMergeHeadFold(t *testing.T) {
	tests := []struct {
		name string
		act  func(r *resolver)
		want bool
	}{
		{
			name: "a push folds a new bubble",
			act:  func(r *resolver) { r.UpsertDurable(testWorkspace, rootFeed(), foldedHead(true)) },
			want: true,
		},
		{
			name: "a failure's push opens a folded bubble",
			act: func(r *resolver) {
				r.UpsertDurable(testWorkspace, rootFeed(), foldedHead(true))
				r.UpsertDurable(testWorkspace, rootFeed(), foldedHead(false))
			},
			want: false,
		},
		{
			name: "a push never folds a bubble the daemon opened",
			act: func(r *resolver) {
				r.UpsertDurable(testWorkspace, rootFeed(), foldedHead(false))
				r.UpsertDurable(testWorkspace, rootFeed(), foldedHead(true))
			},
			want: false,
		},
		{
			name: "a push never folds a bubble the reader opened",
			act: func(r *resolver) {
				r.UpsertDurable(testWorkspace, rootFeed(), foldedHead(true))
				r.SetMergeFold(testWorkspace, foldedHead(true).GetId(), false)
				r.UpsertDurable(testWorkspace, rootFeed(), foldedHead(true))
			},
			want: false,
		},
		{
			name: "the reader folds an open bubble",
			act: func(r *resolver) {
				r.UpsertDurable(testWorkspace, rootFeed(), foldedHead(false))
				r.SetMergeFold(testWorkspace, foldedHead(false).GetId(), true)
			},
			want: true,
		},
		{
			name: "a failure still opens a bubble the reader folded",
			act: func(r *resolver) {
				r.UpsertDurable(testWorkspace, rootFeed(), foldedHead(true))
				r.SetMergeFold(testWorkspace, foldedHead(true).GetId(), true)
				r.UpsertDurable(testWorkspace, rootFeed(), foldedHead(false))
			},
			want: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r, _ := daemonOver(t, newFakeDurableRows())

			// Act
			tt.act(r)

			// Assert
			if got := headFold(t, r); got != tt.want {
				t.Fatalf("folded = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestANewDaemonDrawsTheReadersOpenBubbleOpen(t *testing.T) {
	// Arrange: the reader opened a running merge's bubble on one daemon.
	store := newFakeDurableRows()
	first, _ := daemonOver(t, store)
	first.UpsertDurable(testWorkspace, rootFeed(), foldedHead(true))
	first.SetMergeFold(testWorkspace, foldedHead(true).GetId(), false)

	// Act: a new daemon, whose resumed merge pushes the head folded again.
	second, _ := daemonOver(t, store)
	second.UpsertDurable(testWorkspace, rootFeed(), foldedHead(true))

	// Assert
	if headFold(t, second) {
		t.Fatal("a new daemon folded the bubble the reader had opened")
	}
}

func TestSetMergeFoldRefusesARowThatIsNotAMergeHead(t *testing.T) {
	// Arrange
	r, _ := daemonOver(t, newFakeDurableRows())
	_, tab := mergeTabRow()
	r.UpsertDurable(testWorkspace, feedid.Feed{Root: true}, tab)

	// Act
	ok := r.SetMergeFold(testWorkspace, tab.GetId(), true)

	// Assert
	if ok {
		t.Fatal("a row that is not a merge head took a fold")
	}
}

func TestSetMergeFoldRefusesARowItDoesNotHold(t *testing.T) {
	// Arrange
	r, _ := daemonOver(t, newFakeDurableRows())

	// Act
	ok := r.SetMergeFold(testWorkspace, foldedHead(true).GetId(), true)

	// Assert
	if ok {
		t.Fatal("a row the daemon does not hold took a fold")
	}
}

func TestSetMergeFoldRecordsTheEdgeAtInfo(t *testing.T) {
	// Arrange
	r, log := daemonOver(t, newFakeDurableRows())
	r.UpsertDurable(testWorkspace, rootFeed(), foldedHead(true))

	// Act
	r.SetMergeFold(testWorkspace, foldedHead(true).GetId(), false)

	// Assert
	if !hasRecordIn(log, "info", "daemon.feed.merge_fold") {
		t.Fatal("recording the reader's fold logged no INFO daemon.feed.merge_fold")
	}
}
