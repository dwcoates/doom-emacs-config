package feed

import (
	"os"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/proto"

	"agent-repld/internal/feedid"
)

// A SYNC MERGE BUBBLE IS OPEN BY DEFAULT AND NEVER COLLAPSES AUTOMATICALLY,
// save a merge that ended in success (owner ruling, 2026-10-08); AN ASYNC
// MERGE BUBBLE IS FOLDED BY DEFAULT at every state (owner ruling,
// 2026-10-14); the reader's fold always wins, and all of it holds across a new
// daemon.

// mergeState names a merge head's state arm, for the tables.
type mergeState int

const (
	stateRunning mergeState = iota
	stateSuccess
	stateFailed
	stateAbandoned
)

// stateHead is a merge head on the root feed in STATE, composed as the merge
// orchestrator composes one: with no fold.
func stateHead(state mergeState) *frontendv1.FeedRow {
	row := mergeHeadRow("branch → master")
	merge := row.GetActivity().GetMerge()
	switch state {
	case stateRunning:
		merge.Result = &frontendv1.FeedMerge_Update{Update: &frontendv1.FeedMergeUpdate{}}
	case stateSuccess:
		merge.Result = &frontendv1.FeedMerge_Success{Success: &frontendv1.FeedMergeSuccess{Commit: "abc"}}
	case stateFailed:
		merge.Result = &frontendv1.FeedMerge_Error{Error: &frontendv1.FeedMergeError{
			Reason: &frontendv1.FeedMergeError_Failed{Failed: &frontendv1.FeedMergeFailed{}}}}
	case stateAbandoned:
		merge.Result = &frontendv1.FeedMerge_Error{Error: &frontendv1.FeedMergeError{
			Reason: &frontendv1.FeedMergeError_Abandoned{Abandoned: &frontendv1.FeedMergeAbandoned{}}}}
	}
	return row
}

// asyncStateHead is stateHead for an async merge.
func asyncStateHead(state mergeState) *frontendv1.FeedRow {
	row := stateHead(state)
	row.GetActivity().GetMerge().GetHead().Mode = &frontendv1.FeedMergeMode{
		Mode: &frontendv1.FeedMergeMode_Async{Async: &frontendv1.FeedMergeModeAsync{}}}
	return row
}

// headFoldOf answers the stored head's fold on R's root feed.
func headFoldOf(t *testing.T, r *resolver) *frontendv1.FeedMergeFold {
	t.Helper()
	rows := feedRows(r, rootFeed())
	if len(rows) != 1 {
		t.Fatalf("root rows = %d, want the one head", len(rows))
	}
	fold := rows[0].GetActivity().GetMerge().GetHead().GetFold()
	if fold == nil {
		t.Fatal("the root row is not a merge head with a fold")
	}
	return fold
}

// push publishes a merge head in STATE on R.
func push(r *resolver, state mergeState) {
	r.UpsertDurable(testWorkspace, rootFeed(), stateHead(state))
}

// pushAsync publishes an async merge head in STATE on R.
func pushAsync(r *resolver, state mergeState) {
	r.UpsertDurable(testWorkspace, rootFeed(), asyncStateHead(state))
}

// readerFolds records the reader's FOLDED on R's merge head.
func readerFolds(r *resolver, folded bool) {
	r.SetMergeFold(testWorkspace, stateHead(stateRunning).GetId(), folded)
}

func TestAMergeHeadFoldOnOneDaemon(t *testing.T) {
	tests := []struct {
		name       string
		act        func(r *resolver)
		wantFolded bool
		wantReader bool
	}{
		{
			name:       "a running merge is open",
			act:        func(r *resolver) { push(r, stateRunning) },
			wantFolded: false,
		},
		{
			name:       "a failed merge is open",
			act:        func(r *resolver) { push(r, stateRunning); push(r, stateFailed) },
			wantFolded: false,
		},
		{
			name:       "an abandoned merge is open",
			act:        func(r *resolver) { push(r, stateRunning); push(r, stateAbandoned) },
			wantFolded: false,
		},
		{
			name:       "a running merge's repeated pushes leave it open",
			act:        func(r *resolver) { push(r, stateRunning); push(r, stateRunning) },
			wantFolded: false,
		},
		{
			name:       "a merge that succeeds live collapses",
			act:        func(r *resolver) { push(r, stateRunning); push(r, stateSuccess) },
			wantFolded: true,
		},
		{
			name: "the reader's unfold after a success stays open under a later push",
			act: func(r *resolver) {
				push(r, stateSuccess)
				readerFolds(r, false)
				push(r, stateSuccess)
			},
			wantFolded: false,
			wantReader: true,
		},
		{
			name: "the reader's fold of a running merge stays folded under a later push",
			act: func(r *resolver) {
				push(r, stateRunning)
				readerFolds(r, true)
				push(r, stateRunning)
			},
			wantFolded: true,
			wantReader: true,
		},
		{
			name: "the reader's fold of a running merge stays folded when it fails",
			act: func(r *resolver) {
				push(r, stateRunning)
				readerFolds(r, true)
				push(r, stateFailed)
			},
			wantFolded: true,
			wantReader: true,
		},
		{
			name:       "an async queued or running merge is folded",
			act:        func(r *resolver) { pushAsync(r, stateRunning) },
			wantFolded: true,
		},
		{
			name:       "an async merge stays folded as it streams progress",
			act:        func(r *resolver) { pushAsync(r, stateRunning); pushAsync(r, stateRunning) },
			wantFolded: true,
		},
		{
			name:       "an async merge that lands is folded",
			act:        func(r *resolver) { pushAsync(r, stateRunning); pushAsync(r, stateSuccess) },
			wantFolded: true,
		},
		{
			name:       "an async merge that fails is folded",
			act:        func(r *resolver) { pushAsync(r, stateRunning); pushAsync(r, stateFailed) },
			wantFolded: true,
		},
		{
			name:       "an async merge that did not run is folded",
			act:        func(r *resolver) { pushAsync(r, stateRunning); pushAsync(r, stateAbandoned) },
			wantFolded: true,
		},
		{
			name: "the reader's unfold of an async merge stays open under a later push",
			act: func(r *resolver) {
				pushAsync(r, stateRunning)
				readerFolds(r, false)
				pushAsync(r, stateRunning)
			},
			wantFolded: false,
			wantReader: true,
		},
		{
			name: "the reader's unfold of an async merge stays open when it fails",
			act: func(r *resolver) {
				pushAsync(r, stateRunning)
				readerFolds(r, false)
				pushAsync(r, stateFailed)
			},
			wantFolded: false,
			wantReader: true,
		},
		{
			name: "the reader's unfold of a running merge stays open when it succeeds",
			act: func(r *resolver) {
				push(r, stateRunning)
				readerFolds(r, false)
				push(r, stateSuccess)
			},
			wantFolded: false,
			wantReader: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r, _ := daemonOver(t, newFakeDurableRows())

			// Act
			tt.act(r)

			// Assert
			fold := headFoldOf(t, r)
			if fold.GetFolded() != tt.wantFolded {
				t.Fatalf("folded = %v, want %v", fold.GetFolded(), tt.wantFolded)
			}
			if got := fold.GetReader() != nil; got != tt.wantReader {
				t.Fatalf("decided by the reader = %v, want %v (fold %v)", got, tt.wantReader, fold)
			}
			if tt.wantReader == (fold.GetDaemon() != nil) {
				t.Fatalf("the fold names the wrong decider: %v", fold)
			}
		})
	}
}

func TestAMergeHeadFoldOnANewDaemon(t *testing.T) {
	tests := []struct {
		name       string
		first      func(r *resolver)
		second     func(r *resolver)
		wantFolded bool
	}{
		{
			name:       "a restored success is collapsed",
			first:      func(r *resolver) { push(r, stateRunning); push(r, stateSuccess) },
			second:     func(*resolver) {},
			wantFolded: true,
		},
		{
			name:       "a restored running merge is open",
			first:      func(r *resolver) { push(r, stateRunning) },
			second:     func(*resolver) {},
			wantFolded: false,
		},
		{
			name:       "a restored failure is open",
			first:      func(r *resolver) { push(r, stateFailed) },
			second:     func(*resolver) {},
			wantFolded: false,
		},
		{
			name:       "a restored conflicted or waiting merge is open",
			first:      func(r *resolver) { push(r, stateRunning) },
			second:     func(r *resolver) { push(r, stateRunning) },
			wantFolded: false,
		},
		{
			name:       "a restored success the reader unfolded stays open",
			first:      func(r *resolver) { push(r, stateSuccess); readerFolds(r, false) },
			second:     func(*resolver) {},
			wantFolded: false,
		},
		{
			name:       "a restored async running merge is folded",
			first:      func(r *resolver) { pushAsync(r, stateRunning) },
			second:     func(*resolver) {},
			wantFolded: true,
		},
		{
			name:       "a restored async failure is folded",
			first:      func(r *resolver) { pushAsync(r, stateFailed) },
			second:     func(*resolver) {},
			wantFolded: true,
		},
		{
			name:       "a restored async merge the reader unfolded stays open",
			first:      func(r *resolver) { pushAsync(r, stateRunning); readerFolds(r, false) },
			second:     func(*resolver) {},
			wantFolded: false,
		},
		{
			name:       "a running merge the reader folded stays folded when the resumed merge pushes",
			first:      func(r *resolver) { push(r, stateRunning); readerFolds(r, true) },
			second:     func(r *resolver) { push(r, stateRunning) },
			wantFolded: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			store := newFakeDurableRows()
			first, _ := daemonOver(t, store)
			tt.first(first)

			// Act
			second, _ := daemonOver(t, store)
			tt.second(second)

			// Assert
			if got := headFoldOf(t, second).GetFolded(); got != tt.wantFolded {
				t.Fatalf("folded = %v, want %v", got, tt.wantFolded)
			}
		})
	}
}

// rewriteRecordedHead rewrites the head STORE recorded, through EDIT, as a row
// recorded by an older daemon would read.
func rewriteRecordedHead(t *testing.T, store *fakeDurableRows, edit func(head *frontendv1.FeedMergeHead)) {
	t.Helper()
	id := stateHead(stateRunning).GetId().GetValue()
	recorded, ok := store.recorded(testWorkspace, id)
	if !ok {
		t.Fatal("the head was never recorded")
	}
	row := &frontendv1.FeedRow{}
	if err := proto.Unmarshal(recorded.Row, row); err != nil {
		t.Fatalf("unmarshal: %v", err)
	}
	edit(row.GetActivity().GetMerge().GetHead())
	encoded, err := proto.Marshal(row)
	if err != nil {
		t.Fatalf("marshal: %v", err)
	}
	recorded.Row = encoded
	store.rows[testWorkspace][id] = recorded
}

// recordLegacyFold rewrites the recorded head as a row recorded before the
// fold said who decided it: FOLDED, with no decider.
func recordLegacyFold(t *testing.T, store *fakeDurableRows, folded bool) {
	t.Helper()
	rewriteRecordedHead(t, store, func(head *frontendv1.FeedMergeHead) {
		head.Fold = &frontendv1.FeedMergeFold{Folded: folded}
	})
}

func TestALegacyRecordedFoldIsRepaintedWithTheDefault(t *testing.T) {
	tests := []struct {
		name       string
		state      mergeState
		recorded   bool
		wantFolded bool
	}{
		{"a legacy folded running merge is restored open", stateRunning, true, false},
		{"a legacy open success is restored collapsed", stateSuccess, false, true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			store := newFakeDurableRows()
			first, _ := daemonOver(t, store)
			push(first, tt.state)
			recordLegacyFold(t, store, tt.recorded)

			// Act
			second, log := daemonOver(t, store)
			fold := headFoldOf(t, second)

			// Assert
			if fold.GetFolded() != tt.wantFolded || fold.GetDaemon() == nil {
				t.Fatalf("fold = %v, want folded=%v decided by the daemon", fold, tt.wantFolded)
			}
			found := false
			for _, rec := range log.Records() {
				if rec.Operation == "daemon.feed.merge_fold_restored" && rec.Level == "debug" {
					found = true
				}
			}
			if !found {
				t.Fatalf("no daemon.feed.merge_fold_restored record in %v", log.Records())
			}
		})
	}
}

// recordWithoutMode rewrites the recorded head as a row recorded before heads
// carried their mode.
func recordWithoutMode(t *testing.T, store *fakeDurableRows) {
	t.Helper()
	rewriteRecordedHead(t, store, func(head *frontendv1.FeedMergeHead) { head.Mode = nil })
}

func TestRewriteRecordedHeadRestatesTheStoredRowOnly(t *testing.T) {
	// Arrange
	store := newFakeDurableRows()
	r, _ := daemonOver(t, store)
	push(r, stateRunning)

	// Act
	rewriteRecordedHead(t, store, func(head *frontendv1.FeedMergeHead) {
		head.Label = &frontendv1.FeedMergeLabel{Text: "rewritten"}
	})

	// Assert
	recorded, _ := store.recorded(testWorkspace, stateHead(stateRunning).GetId().GetValue())
	row := &frontendv1.FeedRow{}
	if err := proto.Unmarshal(recorded.Row, row); err != nil {
		t.Fatalf("unmarshal: %v", err)
	}
	drawn := feedRows(r, rootFeed())[0].GetActivity().GetMerge().GetHead().GetLabel().GetText()
	if got := row.GetActivity().GetMerge().GetHead().GetLabel().GetText(); got != "rewritten" || drawn == "rewritten" {
		t.Fatalf("recorded label = %q, drawn label = %q; want only the record rewritten", got, drawn)
	}
}

func TestEveryRecordedHeadRewriteGoesThroughOneHelper(t *testing.T) {
	// Arrange
	source, err := os.ReadFile("mergefold_test.go")
	if err != nil {
		t.Fatalf("read: %v", err)
	}

	// The needle is assembled so this test's own source never matches it.
	needle := "encoded, err := proto." + "Marshal(row)"

	// Act
	marshals := strings.Count(string(source), needle)

	// Assert
	if marshals != 1 {
		t.Fatalf("a recorded head is re-marshalled at %d sites in mergefold_test.go, want one, in rewriteRecordedHead", marshals)
	}
}

func TestARestoredHeadWithNoModeTakesTheSyncDefault(t *testing.T) {
	tests := []struct {
		name       string
		state      mergeState
		wantFolded bool
	}{
		{"a modeless running merge is restored open", stateRunning, false},
		{"a modeless success is restored folded", stateSuccess, true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			store := newFakeDurableRows()
			first, _ := daemonOver(t, store)
			pushAsync(first, tt.state)
			recordWithoutMode(t, store)

			// Act
			second, log := daemonOver(t, store)
			fold := headFoldOf(t, second)

			// Assert
			if fold.GetFolded() != tt.wantFolded || fold.GetDaemon() == nil {
				t.Fatalf("fold = %v, want folded=%v decided by the daemon", fold, tt.wantFolded)
			}
			found := false
			for _, rec := range log.Records() {
				if rec.Operation == "daemon.feed.merge_mode_unrecorded" && rec.Level == "info" &&
					rec.Context["row"] == stateHead(stateRunning).GetId().GetValue() {
					found = true
				}
			}
			if !found {
				t.Fatalf("no daemon.feed.merge_mode_unrecorded record naming the row in %v", log.Records())
			}
		})
	}
}

func TestAHeadPublishedWithNoModePanics(t *testing.T) {
	// Arrange
	r, _ := daemonOver(t, newFakeDurableRows())
	row := stateHead(stateRunning)
	row.GetActivity().GetMerge().GetHead().Mode = nil
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("a head with no mode was published")
		}
		if rows := feedRows(r, rootFeed()); len(rows) != 0 {
			t.Fatalf("root rows = %d, want none published", len(rows))
		}
	}()

	// Act
	r.UpsertDurable(testWorkspace, rootFeed(), row)
}

func TestSetMergeFoldRecordsTheReadersArm(t *testing.T) {
	// Arrange
	store := newFakeDurableRows()
	r, _ := daemonOver(t, store)
	push(r, stateRunning)

	// Act
	ok := r.SetMergeFold(testWorkspace, stateHead(stateRunning).GetId(), true)

	// Assert
	if !ok {
		t.Fatal("the reader's fold was refused")
	}
	recorded, _ := store.recorded(testWorkspace, stateHead(stateRunning).GetId().GetValue())
	row := &frontendv1.FeedRow{}
	if err := proto.Unmarshal(recorded.Row, row); err != nil {
		t.Fatalf("unmarshal: %v", err)
	}
	if fold := row.GetActivity().GetMerge().GetHead().GetFold(); fold.GetReader() == nil || !fold.GetFolded() {
		t.Fatalf("recorded fold = %v, want folded by the reader", fold)
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
	ok := r.SetMergeFold(testWorkspace, stateHead(stateRunning).GetId(), true)

	// Assert
	if ok {
		t.Fatal("a row the daemon does not hold took a fold")
	}
}
