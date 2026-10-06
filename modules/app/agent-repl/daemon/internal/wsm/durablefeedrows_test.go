package wsm

import (
	"bytes"
	"context"
	"testing"
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
