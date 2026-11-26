package wsm

import (
	"bytes"
	"context"
	"fmt"
	"path/filepath"
	"slices"
	"testing"

	"agent-repld/internal/dlog"
)

// durableRow is one durable row of a workspace.
func durableRow(ws WorkspaceID, id, key, row string) DurableFeedRow {
	return DurableFeedRow{Workspace: ws, RowID: id, Plane: 2, OrderKey: key, Row: []byte(row)}
}

// TestPutDurableFeedRowRoundTrips covers the record a new daemon draws from.
func TestPutDurableFeedRowRoundTrips(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	want := durableRow(ws.ID, "row-1", "2.a", "head")

	// Act
	if err := s.PutDurableFeedRow(context.Background(), want); err != nil {
		t.Fatalf("PutDurableFeedRow: %v", err)
	}
	got, err := s.DurableFeedRows(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("DurableFeedRows: %v", err)
	}
	if len(got) != 1 || got[0].RowID != want.RowID || got[0].Plane != want.Plane || got[0].OrderKey != want.OrderKey || !bytes.Equal(got[0].Row, want.Row) {
		t.Fatalf("rows = %+v, want %+v", got, want)
	}
}

// TestPutDurableFeedRowReplacesTheRowAndKeepsItsPlace covers a republication:
// the row is the last one published, at the place it was first drawn.
func TestPutDurableFeedRowReplacesTheRowAndKeepsItsPlace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.PutDurableFeedRow(context.Background(), durableRow(ws.ID, "row-1", "2.a", "running")); err != nil {
		t.Fatalf("PutDurableFeedRow: %v", err)
	}

	// Act
	err := s.PutDurableFeedRow(context.Background(), durableRow(ws.ID, "row-1", "2.a", "landed"))

	// Assert
	if err != nil {
		t.Fatalf("PutDurableFeedRow: %v", err)
	}
	got, _ := s.DurableFeedRows(context.Background(), ws.ID)
	if len(got) != 1 || string(got[0].Row) != "landed" {
		t.Fatalf("rows = %+v, want the one row as last published", got)
	}
}

// TestPutDurableFeedRowRefusesAMovedOrderKey covers the invariant: a row keeps
// the order key it was first drawn at, so another is a defect, refused loudly
// with the record unchanged.
func TestPutDurableFeedRowRefusesAMovedOrderKey(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.PutDurableFeedRow(context.Background(), durableRow(ws.ID, "row-1", "2.a", "running")); err != nil {
		t.Fatalf("PutDurableFeedRow: %v", err)
	}

	// Act
	err := s.PutDurableFeedRow(context.Background(), durableRow(ws.ID, "row-1", "2.b", "landed"))

	// Assert
	if err == nil {
		t.Fatal("a row moved to another order key was recorded")
	}
	got, _ := s.DurableFeedRows(context.Background(), ws.ID)
	if len(got) != 1 || got[0].OrderKey != "2.a" || string(got[0].Row) != "running" {
		t.Fatalf("rows = %+v, want the first record unchanged", got)
	}
	if !loggedOperation(log, "daemon.wsm.put_durable_feed_row", "error") {
		t.Fatalf("the refusal was not logged at error: %v", log.Records())
	}
}

// TestPutDurableFeedRowSettlesAPinnedRowOnce covers the one permitted move: a
// publication naming the recorded pin key as SettledFrom moves the row to its
// settled key, and the record carries the settling publication.
func TestPutDurableFeedRowSettlesAPinnedRowOnce(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.PutDurableFeedRow(context.Background(), durableRow(ws.ID, "row-1", "~a", "running")); err != nil {
		t.Fatalf("PutDurableFeedRow: %v", err)
	}
	settle := durableRow(ws.ID, "row-1", "2.b", "landed")
	settle.SettledFrom = "~a"

	// Act
	err := s.PutDurableFeedRow(context.Background(), settle)

	// Assert
	if err != nil {
		t.Fatalf("PutDurableFeedRow settle: %v", err)
	}
	got, _ := s.DurableFeedRows(context.Background(), ws.ID)
	if len(got) != 1 || got[0].OrderKey != "2.b" || string(got[0].Row) != "landed" {
		t.Fatalf("rows = %+v, want the row at its settled key as last published", got)
	}
}

// TestPutDurableFeedRowRefusesASettleFromAnotherKey covers each settle the
// record cannot honor: nothing moves, and the refusal is an ERROR.
func TestPutDurableFeedRowRefusesASettleFromAnotherKey(t *testing.T) {
	tests := []struct {
		name     string
		recorded string
		from     string
		want     []string
	}{
		{name: "the recorded key is not the one settled from", recorded: "~a", from: "~z", want: []string{"~a"}},
		{name: "the row was never recorded", recorded: "", from: "~a", want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			ws := testWorkspace(t, s)
			if tt.recorded != "" {
				if err := s.PutDurableFeedRow(context.Background(), durableRow(ws.ID, "row-1", tt.recorded, "running")); err != nil {
					t.Fatalf("PutDurableFeedRow: %v", err)
				}
			}
			settle := durableRow(ws.ID, "row-1", "2.b", "landed")
			settle.SettledFrom = tt.from

			// Act
			err := s.PutDurableFeedRow(context.Background(), settle)

			// Assert
			if err == nil {
				t.Fatal("a settle the record cannot honor was recorded")
			}
			got, _ := s.DurableFeedRows(context.Background(), ws.ID)
			var keys []string
			for _, row := range got {
				keys = append(keys, row.OrderKey)
			}
			if fmt.Sprint(keys) != fmt.Sprint(tt.want) {
				t.Fatalf("recorded keys = %v, want %v", keys, tt.want)
			}
			if !loggedOperation(log, "daemon.wsm.put_durable_feed_row", "error") {
				t.Fatalf("the refusal was not logged at error: %v", log.Records())
			}
		})
	}
}

// TestPutDurableFeedRowRefusesARowThatCannotBeDrawnAgain covers each missing
// field: nothing is recorded, and the refusal is an ERROR.
func TestPutDurableFeedRowRefusesARowThatCannotBeDrawnAgain(t *testing.T) {
	tests := []struct {
		name string
		row  func(WorkspaceID) DurableFeedRow
	}{
		{"no workspace", func(WorkspaceID) DurableFeedRow { return durableRow("", "row-1", "2.a", "x") }},
		{"no row id", func(ws WorkspaceID) DurableFeedRow { return durableRow(ws, "", "2.a", "x") }},
		{"no order key", func(ws WorkspaceID) DurableFeedRow { return durableRow(ws, "row-1", "", "x") }},
		{"no row", func(ws WorkspaceID) DurableFeedRow { return durableRow(ws, "row-1", "2.a", "") }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			ws := testWorkspace(t, s)

			// Act
			err := s.PutDurableFeedRow(context.Background(), tt.row(ws.ID))

			// Assert
			if err == nil {
				t.Fatal("an incomplete durable row was recorded")
			}
			if got, _ := s.DurableFeedRows(context.Background(), ws.ID); len(got) != 0 {
				t.Fatalf("rows = %+v, want none", got)
			}
			if !loggedOperation(log, "daemon.wsm.put_durable_feed_row", "error") {
				t.Fatalf("the refusal was not logged at error: %v", log.Records())
			}
		})
	}
}

// TestDurableFeedRowsAreTheWorkspacesOwnInOrderKeyOrder covers the load: one
// workspace's rows, sorted as the feed sorts them.
func TestDurableFeedRowsAreTheWorkspacesOwnInOrderKeyOrder(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	other := testWorkspaceNamed(t, s, "other")
	for _, r := range []DurableFeedRow{
		durableRow(ws.ID, "late", "2.b", "x"),
		durableRow(other.ID, "theirs", "2.0", "x"),
		durableRow(ws.ID, "early", "2.a", "x"),
	} {
		if err := s.PutDurableFeedRow(context.Background(), r); err != nil {
			t.Fatalf("PutDurableFeedRow: %v", err)
		}
	}

	// Act
	got, err := s.DurableFeedRows(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("DurableFeedRows: %v", err)
	}
	if len(got) != 2 || got[0].RowID != "early" || got[1].RowID != "late" {
		t.Fatalf("rows = %+v, want early then late, and none of the other workspace's", got)
	}
}

// TestClearDurableFeedRowsDropsOnlyTheWorkspacesRows covers a bind's reset.
func TestClearDurableFeedRowsDropsOnlyTheWorkspacesRows(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	other := testWorkspaceNamed(t, s, "other")
	for _, r := range []DurableFeedRow{durableRow(ws.ID, "mine", "2.a", "x"), durableRow(other.ID, "theirs", "2.a", "x")} {
		if err := s.PutDurableFeedRow(context.Background(), r); err != nil {
			t.Fatalf("PutDurableFeedRow: %v", err)
		}
	}

	// Act
	err := s.ClearDurableFeedRows(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("ClearDurableFeedRows: %v", err)
	}
	mine, _ := s.DurableFeedRows(context.Background(), ws.ID)
	theirs, _ := s.DurableFeedRows(context.Background(), other.ID)
	if len(mine) != 0 || len(theirs) != 1 {
		t.Fatalf("after the clear: mine %+v, theirs %+v; want none and one", mine, theirs)
	}
}

// TestTheMigrationAddsTheDurableFeedRowsTable pins the layout-16 step.
func TestTheMigrationAddsTheDurableFeedRowsTable(t *testing.T) {
	// Arrange — a file the build at layout 15 left.
	path := fixtureAt(t, 15)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-15 database: %v", err)
	}
	defer handle.Close()

	// Assert
	got := scalar[int](t, handle.(*store), `SELECT count(*) FROM sqlite_master WHERE type = 'table' AND name = 'durable_feed_rows'`)
	if got != 1 {
		t.Fatalf("durable_feed_rows exists %d times after the migration, want 1", got)
	}
}

func TestDurableFeedRowRankAnswersWhereARowIsRecorded(t *testing.T) {
	tests := []struct {
		name      string
		row       string
		wantFound bool
		wantKey   string
	}{
		{name: "a recorded row's plane and key", row: "row-1", wantFound: true, wantKey: "2.a"},
		{name: "a row never recorded", row: "row-2"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			stored := durableRow(ws.ID, "row-1", "2.a", "head")
			if err := s.PutDurableFeedRow(context.Background(), stored); err != nil {
				t.Fatal(err)
			}

			// Act
			plane, key, found, err := s.DurableFeedRowRank(context.Background(), ws.ID, tt.row)

			// Assert
			if err != nil || found != tt.wantFound || key != tt.wantKey || (found && plane != stored.Plane) {
				t.Fatalf("DurableFeedRowRank = (%d, %q, %v, %v), want key %q found %v", plane, key, found, err, tt.wantKey, tt.wantFound)
			}
		})
	}
}

// TestMatchDurableFeedRows covers the matched read: only the rows the match
// selects are loaded, whole, over the scope asked for or every workspace.
func TestMatchDurableFeedRows(t *testing.T) {
	cases := []struct {
		name  string
		scope func(a, b WorkspaceID) []WorkspaceID
		want  []string
	}{
		{name: "every workspace when the scope is empty", scope: func(a, b WorkspaceID) []WorkspaceID { return nil }, want: []string{"a/head-1", "b/head-2"}},
		{name: "only the scoped workspace", scope: func(a, b WorkspaceID) []WorkspaceID { return []WorkspaceID{b} }, want: []string{"b/head-2"}},
		{name: "several scoped workspaces", scope: func(a, b WorkspaceID) []WorkspaceID { return []WorkspaceID{a, b} }, want: []string{"a/head-1", "b/head-2"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			a, b := testWorkspace(t, s), testWorkspace(t, s)
			for _, row := range []DurableFeedRow{
				durableRow(a.ID, "head-1", "2.a", "a-head"),
				durableRow(a.ID, "tab-1", "2.b", "a-tab"),
				durableRow(b.ID, "head-2", "2.a", "b-head"),
			} {
				if err := s.PutDurableFeedRow(context.Background(), row); err != nil {
					t.Fatalf("PutDurableFeedRow: %v", err)
				}
			}
			names := map[WorkspaceID]string{a.ID: "a", b.ID: "b"}

			// Act
			got, err := s.MatchDurableFeedRows(context.Background(), tc.scope(a.ID, b.ID), func(_ WorkspaceID, id string) bool {
				return id == "head-1" || id == "head-2"
			})

			// Assert
			if err != nil {
				t.Fatalf("MatchDurableFeedRows: %v", err)
			}
			var drawn []string
			for _, row := range got {
				drawn = append(drawn, names[row.Workspace]+"/"+row.RowID)
				if want := names[row.Workspace] + "-head"; string(row.Row) != want || row.OrderKey != "2.a" || row.Plane != 2 {
					t.Errorf("row %s = %+v, want it whole (%q at 2.a)", row.RowID, row, want)
				}
			}
			slices.Sort(drawn)
			if !slices.Equal(drawn, tc.want) {
				t.Fatalf("matched %v, want %v", drawn, tc.want)
			}
		})
	}
}

// readOnlyDurableStore is a READ-ONLY handle on a file holding one workspace
// and the rows SEED records, as a joining successor opens it, with a logger
// the test reads.
func readOnlyDurableStore(t *testing.T, seed ...DurableFeedRow) (DB, WorkspaceID, *dlog.TestLogger) {
	t.Helper()
	path := filepath.Join(t.TempDir(), "wsm.db")
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	ws := testWorkspace(t, handle.(*store))
	for _, row := range seed {
		row.Workspace = ws.ID
		if err := handle.PutDurableFeedRow(context.Background(), row); err != nil {
			t.Fatalf("PutDurableFeedRow: %v", err)
		}
	}
	if err := handle.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}
	log := dlog.NewTestLogger()
	ro, err := OpenReadOnly(context.Background(), path, WithUnsyncedWrites(), WithLogger(log))
	if err != nil {
		t.Fatalf("OpenReadOnly: %v", err)
	}
	t.Cleanup(func() { ro.Close() })
	return ro, ws.ID, log
}

func TestPutDurableFeedRowOnAReadOnlyHandleIsHeldNotRefused(t *testing.T) {
	// Arrange
	ro, ws, log := readOnlyDurableStore(t)

	// Act
	err := ro.PutDurableFeedRow(context.Background(), durableRow(ws, "row-1", "2.a", "bubble"))

	// Assert
	if err != nil {
		t.Fatalf("PutDurableFeedRow on a read-only handle = %v, want nil: the joining successor's row is held, not refused", err)
	}
	if got, err := ro.DurableFeedRows(context.Background(), ws); err != nil || len(got) != 0 {
		t.Fatalf("rows before the promotion = %+v, %v; want none recorded", got, err)
	}
	for _, r := range log.Records() {
		if r.Level == "error" {
			t.Fatalf("a held durable row recorded an ERROR: %+v", r)
		}
	}
}

func TestPutDurableFeedRowHeldOnAReadOnlyHandleIsWrittenAtThePromotion(t *testing.T) {
	// Arrange
	ro, ws, _ := readOnlyDurableStore(t)
	want := durableRow(ws, "row-1", "2.a", "bubble")
	if err := ro.PutDurableFeedRow(context.Background(), want); err != nil {
		t.Fatalf("PutDurableFeedRow: %v", err)
	}

	// Act
	if err := ro.Promote(context.Background()); err != nil {
		t.Fatalf("Promote: %v", err)
	}

	// Assert
	got, err := ro.DurableFeedRows(context.Background(), ws)
	if err != nil {
		t.Fatalf("DurableFeedRows: %v", err)
	}
	if len(got) != 1 || got[0].RowID != want.RowID || got[0].Plane != want.Plane || got[0].OrderKey != want.OrderKey || !bytes.Equal(got[0].Row, want.Row) {
		t.Fatalf("rows after the promotion = %+v, want [%+v]", got, want)
	}
}

func TestPutDurableFeedRowHeldTwiceWritesTheLatest(t *testing.T) {
	// Arrange
	ro, ws, _ := readOnlyDurableStore(t)
	for _, row := range []DurableFeedRow{durableRow(ws, "row-1", "2.a", "running"), durableRow(ws, "row-1", "2.a", "landed")} {
		if err := ro.PutDurableFeedRow(context.Background(), row); err != nil {
			t.Fatalf("PutDurableFeedRow: %v", err)
		}
	}

	// Act
	if err := ro.Promote(context.Background()); err != nil {
		t.Fatalf("Promote: %v", err)
	}

	// Assert
	got, _ := ro.DurableFeedRows(context.Background(), ws)
	if len(got) != 1 || string(got[0].Row) != "landed" {
		t.Fatalf("rows after the promotion = %+v, want the one row as last held", got)
	}
}

func TestPutDurableFeedRowHeldSettleKeepsItsPinKeyThroughARestatement(t *testing.T) {
	// Arrange: the incumbent recorded the row at its pin key; the successor
	// settles it, then restates it at the settled key, while read-only.
	ro, ws, log := readOnlyDurableStore(t, durableRow("", "row-1", "~a", "running"))
	settle := durableRow(ws, "row-1", "2.b", "landed")
	settle.SettledFrom = "~a"
	for _, row := range []DurableFeedRow{settle, durableRow(ws, "row-1", "2.b", "landed again")} {
		if err := ro.PutDurableFeedRow(context.Background(), row); err != nil {
			t.Fatalf("PutDurableFeedRow: %v", err)
		}
	}

	// Act
	if err := ro.Promote(context.Background()); err != nil {
		t.Fatalf("Promote: %v", err)
	}

	// Assert
	got, _ := ro.DurableFeedRows(context.Background(), ws)
	if len(got) != 1 || got[0].OrderKey != "2.b" || string(got[0].Row) != "landed again" {
		t.Fatalf("rows after the promotion = %+v, want the row at its settled key as last held", got)
	}
	for _, r := range log.Records() {
		if r.Level == "error" {
			t.Fatalf("the held settle's write recorded an ERROR: %+v", r)
		}
	}
}

func TestPutDurableFeedRowHeldPinAndSettleLandAtTheSettledKey(t *testing.T) {
	// Arrange: nothing is recorded; the successor draws a pinned row and then
	// settles it, both while read-only, so the pin key is never written.
	ro, ws, log := readOnlyDurableStore(t)
	settle := durableRow(ws, "row-1", "2.b", "landed")
	settle.SettledFrom = "~a"
	for _, row := range []DurableFeedRow{durableRow(ws, "row-1", "~a", "running"), settle} {
		if err := ro.PutDurableFeedRow(context.Background(), row); err != nil {
			t.Fatalf("PutDurableFeedRow: %v", err)
		}
	}

	// Act
	if err := ro.Promote(context.Background()); err != nil {
		t.Fatalf("Promote: %v", err)
	}

	// Assert
	got, _ := ro.DurableFeedRows(context.Background(), ws)
	if len(got) != 1 || got[0].OrderKey != "2.b" || string(got[0].Row) != "landed" {
		t.Fatalf("rows after the promotion = %+v, want the row recorded once at its settled key", got)
	}
	for _, r := range log.Records() {
		if r.Level == "error" {
			t.Fatalf("the held settle's write recorded an ERROR: %+v", r)
		}
	}
}
