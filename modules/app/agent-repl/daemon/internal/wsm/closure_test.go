package wsm

import (
	"context"
	"errors"
	"testing"
	"time"
)

var (
	firstClose  = time.Date(2026, 10, 14, 9, 0, 0, 0, time.UTC)
	secondClose = firstClose.Add(time.Hour)
)

func TestMarkClosedRecordsTheClose(t *testing.T) {
	tests := []struct {
		name   string
		first  *Closure // the close already standing, nil for an open row
		second Closure
		want   Closure
	}{
		{name: "an open row takes the close", second: Closure{At: secondClose, By: CloseByDaemon}, want: Closure{At: secondClose, By: CloseByDaemon}},
		{name: "a second daemon close keeps the first", first: &Closure{At: firstClose, By: CloseByDaemon}, second: Closure{At: secondClose, By: CloseByDaemon}, want: Closure{At: firstClose, By: CloseByDaemon}},
		{name: "a second user close keeps the first", first: &Closure{At: firstClose, By: CloseByUser}, second: Closure{At: secondClose, By: CloseByUser}, want: Closure{At: firstClose, By: CloseByUser}},
		{name: "a user close takes over the daemon's", first: &Closure{At: firstClose, By: CloseByDaemon}, second: Closure{At: secondClose, By: CloseByUser}, want: Closure{At: secondClose, By: CloseByUser}},
		{name: "a user close takes over an agent's", first: &Closure{At: firstClose, By: CloseByAgent}, second: Closure{At: secondClose, By: CloseByUser}, want: Closure{At: secondClose, By: CloseByUser}},
		{name: "a merge close never takes over the user's", first: &Closure{At: firstClose, By: CloseByUser}, second: Closure{At: secondClose, By: CloseByMerge}, want: Closure{At: firstClose, By: CloseByUser}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			ctx := context.Background()
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			if tt.first != nil {
				if err := s.MarkClosed(ctx, ws.ID, *tt.first); err != nil {
					t.Fatalf("MarkClosed(first): %v", err)
				}
			}

			// Act
			if err := s.MarkClosed(ctx, ws.ID, tt.second); err != nil {
				t.Fatalf("MarkClosed: %v", err)
			}

			// Assert
			got, err := s.Workspace(ctx, ws.ID)
			if err != nil {
				t.Fatalf("Workspace: %v", err)
			}
			if !got.Closed || got.Closure == nil {
				t.Fatalf("closed = %v, closure = %v, want a closed row with a record", got.Closed, got.Closure)
			}
			if !got.Closure.At.Equal(tt.want.At) || got.Closure.By != tt.want.By {
				t.Fatalf("closure = %+v, want %+v", *got.Closure, tt.want)
			}
		})
	}
}

func TestMarkClosedRecordsALegacyCloseTheUserMakes(t *testing.T) {
	// Arrange: a row the previous build closed, with no record.
	ctx := context.Background()
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	corrupt(t, s, `UPDATE workspaces SET closed = 1 WHERE id = ?`, ws.ID)

	// Act
	if err := s.MarkClosed(ctx, ws.ID, Closure{At: secondClose, By: CloseByUser}); err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}

	// Assert
	got, err := s.Workspace(ctx, ws.ID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if got.Closure == nil || got.Closure.By != CloseByUser || !got.Closure.At.Equal(secondClose) {
		t.Fatalf("closure = %v, want the user's close at %v", got.Closure, secondClose)
	}
}

func TestMarkClosedLeavesALegacyCloseUnrecordedForTheDaemon(t *testing.T) {
	// Arrange: a row the previous build closed, with no record.
	ctx := context.Background()
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	corrupt(t, s, `UPDATE workspaces SET closed = 1 WHERE id = ?`, ws.ID)

	// Act: the boot re-closes it to release its serving ownership.
	if err := s.MarkClosed(ctx, ws.ID, Closure{At: secondClose, By: CloseByDaemon}); err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}

	// Assert
	got, err := s.Workspace(ctx, ws.ID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if got.Closure != nil {
		t.Fatalf("closure = %+v, want none: the daemon cannot vouch for the old close", *got.Closure)
	}
}

func TestMarkClosedRefusesAnUndeclaredCause(t *testing.T) {
	// Arrange
	ctx := context.Background()
	s, log := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.MarkClosed(ctx, ws.ID, Closure{At: firstClose, By: CloseCause("someone")})

	// Assert
	if err == nil {
		t.Fatalf("MarkClosed with an undeclared cause succeeded")
	}
	if !loggedOperation(log, "daemon.wsm.mark_closed", "error") {
		t.Fatalf("the refusal was not logged at error: %v", log.Records())
	}
	got, err := s.Workspace(ctx, ws.ID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if got.Closed || got.Closure != nil {
		t.Fatalf("closed = %v, closure = %v, want the row untouched", got.Closed, got.Closure)
	}
}

func TestReopenClearsTheCloseRecord(t *testing.T) {
	// Arrange
	ctx := context.Background()
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.MarkClosed(ctx, ws.ID, Closure{At: firstClose, By: CloseByUser}); err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}

	// Act
	if err := s.Reopen(ctx, ws.ID); err != nil {
		t.Fatalf("Reopen: %v", err)
	}

	// Assert
	got, err := s.Workspace(ctx, ws.ID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if got.Closed || got.Closure != nil {
		t.Fatalf("closed = %v, closure = %v, want an open row with no record", got.Closed, got.Closure)
	}
}

func TestReopenRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.Reopen(context.Background(), WorkspaceID("absent"))

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("Reopen = %v, want ErrNotFound", err)
	}
}

func TestWorkspaceRefusesACorruptCloseRecord(t *testing.T) {
	tests := []struct {
		name   string
		update string
		field  string
	}{
		{name: "an instant without a cause", update: `UPDATE workspaces SET closed = 1, closure_at = 1 WHERE id = ?`, field: "closure_by"},
		{name: "a cause without an instant", update: `UPDATE workspaces SET closed = 1, closure_by = 'user' WHERE id = ?`, field: "closure_by"},
		{name: "a record on an open row", update: `UPDATE workspaces SET closure_at = 1, closure_by = 'user' WHERE id = ?`, field: "closure_at"},
		{name: "an undeclared cause", update: `UPDATE workspaces SET closed = 1, closure_at = 1, closure_by = 'someone' WHERE id = ?`, field: "closure_by"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			corrupt(t, s, tt.update, ws.ID)

			// Act
			_, err := s.Workspace(context.Background(), ws.ID)

			// Assert
			var decode *DecodeError
			if !errors.As(err, &decode) || decode.Field != tt.field {
				t.Fatalf("Workspace = %v, want a %s decode error", err, tt.field)
			}
		})
	}
}
