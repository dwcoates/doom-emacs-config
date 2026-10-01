package feed

import (
	"context"
	"errors"
	"maps"
	"sort"
	"sync"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	"google.golang.org/protobuf/proto"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// fakeDurableRows is an in-memory durable row record, shared between the
// resolvers that stand for two daemons.
type fakeDurableRows struct {
	mu   sync.Mutex
	rows map[ids.WorkspaceID]map[string]wsm.DurableFeedRow
	// putErr, readErr and clearErr fail their call when set.
	putErr, readErr, clearErr error
	cleared                   []ids.WorkspaceID
}

func newFakeDurableRows() *fakeDurableRows {
	return &fakeDurableRows{rows: map[ids.WorkspaceID]map[string]wsm.DurableFeedRow{}}
}

func (f *fakeDurableRows) PutDurableFeedRow(_ context.Context, row wsm.DurableFeedRow) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.putErr != nil {
		return f.putErr
	}
	if f.rows[row.Workspace] == nil {
		f.rows[row.Workspace] = map[string]wsm.DurableFeedRow{}
	}
	f.rows[row.Workspace][row.RowID] = row
	return nil
}

func (f *fakeDurableRows) DurableFeedRows(_ context.Context, id ids.WorkspaceID) ([]wsm.DurableFeedRow, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.readErr != nil {
		return nil, f.readErr
	}
	out := make([]wsm.DurableFeedRow, 0, len(f.rows[id]))
	for _, row := range f.rows[id] {
		out = append(out, row)
	}
	sort.Slice(out, func(i, j int) bool { return out[i].OrderKey < out[j].OrderKey })
	return out, nil
}

func (f *fakeDurableRows) ClearDurableFeedRows(_ context.Context, id ids.WorkspaceID) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.clearErr != nil {
		return f.clearErr
	}
	delete(f.rows, id)
	f.cleared = append(f.cleared, id)
	return nil
}

// recorded answers one recorded row.
func (f *fakeDurableRows) recorded(ws ids.WorkspaceID, id string) (wsm.DurableFeedRow, bool) {
	f.mu.Lock()
	defer f.mu.Unlock()
	row, ok := f.rows[ws][id]
	return row, ok
}

// daemonOver builds a resolver standing for one daemon over the durable
// record, with the harness's deterministic encoding.
func daemonOver(t *testing.T, store DurableRows) (*resolver, *dlog.TestLogger) {
	t.Helper()
	log := dlog.NewTestLogger()
	r, err := newResolver(Deps{
		Log:          &fakeSurfaces{log: log},
		WorkspaceDir: func(ids.WorkspaceID) (string, error) { return "/tmp/ws", nil },
		Encode:       testEncode,
		Decode:       testDecode,
		EncodeFeed:   testEncodeFeed,
		Painter:      &fakePainter{},
		ResolveImage: func(*conversationv1.ImageBlock) (string, string, error) { return "", "", nil },
		DurableRows:  store,
	})
	if err != nil {
		t.Fatalf("newResolver: %v", err)
	}
	return r, log
}

// mergeHeadRow is a merge head on the root feed with LABEL.
func mergeHeadRow(label string) *frontendv1.FeedRow {
	return &frontendv1.FeedRow{
		Id: testEncode(feedid.Ref{WS: testWorkspace, Feed: rootFeed(), Row: feedid.RowKey{Kind: feedid.KindMergeTab, ID: "lease-7", Sub: "head"}}),
		Row: &frontendv1.FeedRow_Activity{Activity: &frontendv1.FeedTurnActivity{
			Unit: &frontendv1.FeedTurnActivity_Merge{Merge: &frontendv1.FeedMerge{
				Head: &frontendv1.FeedMergeHead{Label: &frontendv1.FeedMergeLabel{Text: label}},
			}},
		}},
	}
}

// mergeTabRow is the tests tab of lease-7's sub-feed.
func mergeTabRow() (feedid.Feed, *frontendv1.FeedRow) {
	lease := ids.LeaseID("lease-7")
	feed := feedid.Feed{Merge: &lease}
	return feed, &frontendv1.FeedRow{
		Id:  testEncode(feedid.Ref{WS: testWorkspace, Feed: feed, Row: feedid.RowKey{Kind: feedid.KindMergeTab, ID: "lease-7", Sub: "tests:1"}}),
		Row: &frontendv1.FeedRow_MergeTab{MergeTab: &frontendv1.FeedMergeTab{Label: &frontendv1.FeedMergeTabLabel{Text: "tests", Round: 1}}},
	}
}

// labelOf answers a merge head row's label.
func labelOf(row *frontendv1.FeedRow) string {
	return row.GetActivity().GetMerge().GetHead().GetLabel().GetText()
}

func TestUpsertDurableRecordsTheRowAsPublishedAtItsOrderKey(t *testing.T) {
	// Arrange
	store := newFakeDurableRows()
	r, _ := daemonOver(t, store)
	row := mergeHeadRow("branch → master")

	// Act
	r.UpsertDurable(testWorkspace, rootFeed(), row)

	// Assert
	published := feedRows(r, rootFeed())[0]
	recorded, ok := store.recorded(testWorkspace, row.GetId().GetValue())
	if !ok {
		t.Fatal("the durable row was not recorded")
	}
	decoded := &frontendv1.FeedRow{}
	if err := proto.Unmarshal(recorded.Row, decoded); err != nil {
		t.Fatalf("the recorded row does not decode: %v", err)
	}
	if !proto.Equal(decoded, published) || recorded.OrderKey != published.GetOrder().GetKey() {
		t.Fatalf("recorded %+v at %q, want the published row %+v at %q", decoded, recorded.OrderKey, published, published.GetOrder().GetKey())
	}
}

func TestUpsertDurableRecordsTheLatestPublication(t *testing.T) {
	// Arrange
	store := newFakeDurableRows()
	r, _ := daemonOver(t, store)
	r.UpsertDurable(testWorkspace, rootFeed(), mergeHeadRow("running"))
	first, _ := store.recorded(testWorkspace, mergeHeadRow("").GetId().GetValue())

	// Act
	r.UpsertDurable(testWorkspace, rootFeed(), mergeHeadRow("landed"))

	// Assert
	latest, _ := store.recorded(testWorkspace, mergeHeadRow("").GetId().GetValue())
	decoded := &frontendv1.FeedRow{}
	if err := proto.Unmarshal(latest.Row, decoded); err != nil {
		t.Fatalf("the recorded row does not decode: %v", err)
	}
	if labelOf(decoded) != "landed" || latest.OrderKey != first.OrderKey {
		t.Fatalf("recorded %q at %q, want the landed row at the first key %q", labelOf(decoded), latest.OrderKey, first.OrderKey)
	}
}

func TestANewDaemonDrawsTheDurableRowsAgainWhereTheyStood(t *testing.T) {
	// Arrange: the first daemon draws a head and its tab; the second shares
	// only the record.
	store := newFakeDurableRows()
	first, _ := daemonOver(t, store)
	first.UpsertDurable(testWorkspace, rootFeed(), mergeHeadRow("landed"))
	tabFeed, tab := mergeTabRow()
	first.UpsertDurable(testWorkspace, tabFeed, tab)
	wantHead := feedRows(first, rootFeed())
	wantTabs := feedRows(first, tabFeed)

	// Act
	second, log := daemonOver(t, store)
	gotHead := feedRows(second, rootFeed())
	gotTabs := feedRows(second, tabFeed)

	// Assert
	if len(gotHead) != 1 || !proto.Equal(gotHead[0], wantHead[0]) {
		t.Fatalf("root = %+v, want %+v", gotHead, wantHead)
	}
	if len(gotTabs) != 1 || !proto.Equal(gotTabs[0], wantTabs[0]) {
		t.Fatalf("merge feed = %+v, want %+v", gotTabs, wantTabs)
	}
	if !hasRecordIn(log, "info", opDurable) {
		t.Fatalf("records = %+v, want the redraw recorded", log.Records())
	}
}

func TestARowDrawnAfterTheRedrawFollowsIt(t *testing.T) {
	// Arrange: a new daemon has drawn the bubble again.
	store := newFakeDurableRows()
	first, _ := daemonOver(t, store)
	first.UpsertDurable(testWorkspace, rootFeed(), mergeHeadRow("landed"))
	second, _ := daemonOver(t, store)
	redrawn := feedRows(second, rootFeed())[0]

	// Act: the new daemon makes a row of its own.
	later := &frontendv1.FeedRow{
		Id:  testEncode(feedid.Ref{WS: testWorkspace, Feed: rootFeed(), Row: feedid.RowKey{Kind: feedid.KindSynth, ID: "after"}}),
		Row: &frontendv1.FeedRow_MergeTab{MergeTab: &frontendv1.FeedMergeTab{}},
	}
	second.UpsertSynthesized(testWorkspace, rootFeed(), later)

	// Assert
	rows := feedRows(second, rootFeed())
	if len(rows) != 2 || rows[0].GetId().GetValue() != redrawn.GetId().GetValue() {
		t.Fatalf("rows = %+v, want the redrawn bubble first and the new row after it", rows)
	}
}

func TestARowDrawnAfterSeveralRedrawnRowsFollowsThemAll(t *testing.T) {
	// Arrange: the first daemon drew three rows on the merge's feed, each
	// minted to follow the one before; a new daemon has drawn them again.
	store := newFakeDurableRows()
	first, _ := daemonOver(t, store)
	tabFeed, _ := mergeTabRow()
	for _, id := range []string{"one", "two", "three"} {
		first.UpsertDurable(testWorkspace, tabFeed, &frontendv1.FeedRow{
			Id:  testEncode(feedid.Ref{WS: testWorkspace, Feed: tabFeed, Row: feedid.RowKey{Kind: feedid.KindSynth, ID: id}}),
			Row: &frontendv1.FeedRow_MergeTab{MergeTab: &frontendv1.FeedMergeTab{}},
		})
	}
	second, _ := daemonOver(t, store)

	// Act: the new daemon draws a row of its own on the same feed.
	later := &frontendv1.FeedRow{
		Id:  testEncode(feedid.Ref{WS: testWorkspace, Feed: tabFeed, Row: feedid.RowKey{Kind: feedid.KindSynth, ID: "four"}}),
		Row: &frontendv1.FeedRow_MergeTab{MergeTab: &frontendv1.FeedMergeTab{}},
	}
	second.UpsertDurable(testWorkspace, tabFeed, later)

	// Assert
	rows := feedRows(second, tabFeed)
	if len(rows) != 4 || rows[3].GetId().GetValue() != later.GetId().GetValue() {
		t.Fatalf("rows = %+v, want the new row after every redrawn row", rows)
	}
}

func TestARedrawnRowWhoseKeyCannotBeReadIsAnErrorAndDrawsNothing(t *testing.T) {
	// Arrange: a record whose order key carries no count after its separator.
	store := newFakeDurableRows()
	first, _ := daemonOver(t, store)
	first.UpsertDurable(testWorkspace, rootFeed(), mergeHeadRow("landed"))
	for id, row := range store.rows[testWorkspace] {
		row.OrderKey = "L.zz"
		store.rows[testWorkspace][id] = row
	}

	// Act
	second, log := daemonOver(t, store)

	// Assert
	if rows := feedRows(second, rootFeed()); len(rows) != 0 {
		t.Fatalf("rows = %+v, want none drawn", rows)
	}
	if !hasRecordIn(log, "error", opDurable) {
		t.Fatalf("records = %+v, want the unreadable key recorded at ERROR", log.Records())
	}
}

func TestAFailedRecordIsAnErrorAndTheRowStillStands(t *testing.T) {
	// Arrange
	store := newFakeDurableRows()
	store.putErr = errors.New("disk I/O error")
	r, log := daemonOver(t, store)

	// Act
	r.UpsertDurable(testWorkspace, rootFeed(), mergeHeadRow("landed"))

	// Assert
	if rows := feedRows(r, rootFeed()); len(rows) != 1 {
		t.Fatalf("rows = %d, want the row drawn for this daemon's readers", len(rows))
	}
	if !hasRecordIn(log, "error", opDurable) {
		t.Fatalf("records = %+v, want the failed record at error", log.Records())
	}
}

func TestAnUnreadableRecordIsAnErrorAndDrawsNothing(t *testing.T) {
	// Arrange
	store := newFakeDurableRows()
	first, _ := daemonOver(t, store)
	first.UpsertDurable(testWorkspace, rootFeed(), mergeHeadRow("landed"))
	store.readErr = errors.New("disk I/O error")

	// Act
	second, log := daemonOver(t, store)
	rows := feedRows(second, rootFeed())

	// Assert
	if len(rows) != 0 {
		t.Fatalf("rows = %+v, want nothing drawn from an unreadable record", rows)
	}
	if !hasRecordIn(log, "error", opDurable) {
		t.Fatalf("records = %+v, want the failed read at error", log.Records())
	}
}

func TestAnUndecodableRowIsAnErrorAndTheOthersAreStillDrawn(t *testing.T) {
	// Arrange: one good row, and one whose bytes are no FeedRow.
	store := newFakeDurableRows()
	first, _ := daemonOver(t, store)
	first.UpsertDurable(testWorkspace, rootFeed(), mergeHeadRow("landed"))
	if err := store.PutDurableFeedRow(context.Background(), wsm.DurableFeedRow{
		Workspace: testWorkspace, RowID: "row|garbage", Plane: int(planeLive), OrderKey: "2zzzz", Row: []byte{0xff, 0xff, 0xff},
	}); err != nil {
		t.Fatalf("PutDurableFeedRow: %v", err)
	}

	// Act
	second, log := daemonOver(t, store)
	rows := feedRows(second, rootFeed())

	// Assert
	if len(rows) != 1 || labelOf(rows[0]) != "landed" {
		t.Fatalf("rows = %+v, want the good row alone", rows)
	}
	if !hasRecordIn(log, "error", opDurable) {
		t.Fatalf("records = %+v, want the undecodable row at error", log.Records())
	}
}

func TestAResetForgetsTheDurableRows(t *testing.T) {
	// Arrange
	store := newFakeDurableRows()
	r, _ := daemonOver(t, store)
	r.UpsertDurable(testWorkspace, rootFeed(), mergeHeadRow("landed"))

	// Act
	r.ResetWorkspace(testWorkspace, "bound to another conversation")

	// Assert
	if _, ok := store.recorded(testWorkspace, mergeHeadRow("").GetId().GetValue()); ok {
		t.Fatal("a reset left the durable row recorded; a new daemon would draw it again")
	}
}

func TestAResetThatCannotForgetIsAnError(t *testing.T) {
	// Arrange
	store := newFakeDurableRows()
	r, log := daemonOver(t, store)
	r.UpsertDurable(testWorkspace, rootFeed(), mergeHeadRow("landed"))
	store.clearErr = errors.New("disk I/O error")

	// Act
	r.ResetWorkspace(testWorkspace, "bound to another conversation")

	// Assert
	if !hasRecordIn(log, "error", opDurable) {
		t.Fatalf("records = %+v, want the failed forget at error", log.Records())
	}
}

func TestReserveOrderAdvancesTheCounterItsKeyWasMintedFrom(t *testing.T) {
	tests := []struct {
		name          string
		key           string
		wantFollowers map[string]uint32
		wantEntryRows map[string]uint32
	}{
		{name: "a follower key", key: "L.0000000a", wantFollowers: map[string]uint32{"L": 10}, wantEntryRows: map[string]uint32{}},
		{name: "an entry key", key: "E000000000000000100000002" + "00000004", wantFollowers: map[string]uint32{}, wantEntryRows: map[string]uint32{"E000000000000000100000002": 5}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := &feedState{followers: map[string]uint32{}, entryRows: map[string]uint32{}}

			// Act
			err := reserveOrder(f, tt.key)

			// Assert
			if err != nil {
				t.Fatalf("reserveOrder(%q) = %v", tt.key, err)
			}
			if !maps.Equal(f.followers, tt.wantFollowers) || !maps.Equal(f.entryRows, tt.wantEntryRows) {
				t.Fatalf("followers = %v, entryRows = %v, want %v and %v", f.followers, f.entryRows, tt.wantFollowers, tt.wantEntryRows)
			}
		})
	}
}

func TestReserveOrderNeverMovesACounterBack(t *testing.T) {
	// Arrange
	f := &feedState{followers: map[string]uint32{"L": 9}, entryRows: map[string]uint32{}}

	// Act
	err := reserveOrder(f, "L.00000002")

	// Assert
	if err != nil || f.followers["L"] != 9 {
		t.Fatalf("reserveOrder = %v, followers = %v, want L kept at 9", err, f.followers)
	}
}

func TestReserveOrderRefusesAKeyTooShortToBeAnEntryKey(t *testing.T) {
	// Arrange
	f := &feedState{followers: map[string]uint32{}, entryRows: map[string]uint32{}}

	// Act
	err := reserveOrder(f, "E01")

	// Assert
	if err == nil {
		t.Fatal("reserveOrder accepted a key too short to be an entry key")
	}
}
