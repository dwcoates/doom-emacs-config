package wsm

import (
	"context"
	"errors"
	"reflect"
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

// testServing is the daemon instance the tests' re-opens claim serving for.
const testServing InstanceID = "instance-reopening"

func TestReopenClearsTheCloseRecord(t *testing.T) {
	// Arrange
	ctx := context.Background()
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.MarkClosed(ctx, ws.ID, Closure{At: firstClose, By: CloseByUser}); err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}

	// Act
	if err := s.Reopen(ctx, ws.ID, testServing); err != nil {
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

// mergedAndClosed stamps ws merged with a landed count and closes it, as the
// merge queue's landing leaves its requester.
func mergedAndClosed(t *testing.T, s *store, ws Workspace) {
	t.Helper()
	ctx := context.Background()
	commits := uint32(4)
	if err := s.SetMergedAt(ctx, ws.ID, firstClose, &commits); err != nil {
		t.Fatalf("SetMergedAt: %v", err)
	}
	if err := s.MarkClosed(ctx, ws.ID, Closure{At: firstClose, By: CloseByMerge}); err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}
}

func TestReopenRetiresAMergesSettlement(t *testing.T) {
	tests := []struct {
		name string
		read func(t *testing.T, s *store, ws Workspace) any
	}{
		{name: "merged_at", read: func(t *testing.T, s *store, ws Workspace) any {
			return scalar[*int64](t, s, `SELECT merged_at FROM workspaces WHERE id = ?`, ws.ID)
		}},
		{name: "commit_count", read: func(t *testing.T, s *store, ws Workspace) any {
			return scalar[*int64](t, s, `SELECT commit_count FROM workspaces WHERE id = ?`, ws.ID)
		}},
		{name: "landed_head", read: func(t *testing.T, s *store, ws Workspace) any {
			return scalar[*string](t, s, `SELECT landed_head FROM workspaces WHERE id = ?`, ws.ID)
		}},
		{name: "landed_at", read: func(t *testing.T, s *store, ws Workspace) any {
			return scalar[*int64](t, s, `SELECT landed_at FROM workspaces WHERE id = ?`, ws.ID)
		}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a requester retired after its detached merge landed
			// carries a landing as well as merged_at.
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			if err := s.SetLanded(context.Background(), ws.ID, &Landing{Head: "f00d", At: firstClose}); err != nil {
				t.Fatalf("SetLanded: %v", err)
			}
			mergedAndClosed(t, s, ws)

			// Act
			if err := s.Reopen(context.Background(), ws.ID, testServing); err != nil {
				t.Fatalf("Reopen: %v", err)
			}

			// Assert
			if got := tc.read(t, s, ws); !reflect.ValueOf(got).IsNil() {
				t.Fatalf("%s = %v after reopening a merged workspace, want NULL", tc.name, reflect.ValueOf(got).Elem())
			}
		})
	}
}

func TestReopenOpensAMergedWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	mergedAndClosed(t, s, ws)

	// Act
	if err := s.Reopen(context.Background(), ws.ID, testServing); err != nil {
		t.Fatalf("Reopen: %v", err)
	}

	// Assert
	got, err := s.Workspace(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if got.Closed || got.Closure != nil || got.MergedAt != nil {
		t.Fatalf("closed = %v, closure = %v, merged_at = %v, want an open, unmerged row", got.Closed, got.Closure, got.MergedAt)
	}
}

func TestReopenRevealsAMergedMergeWorkspace(t *testing.T) {
	// Arrange: a merge workspace that landed without ever being revealed.
	s, _ := testStore(t)
	requester := testWorkspace(t, s)
	ws := registerMergeWorkspace(t, s, requester.ID, NewDetachedMergeID())
	mergedAndClosed(t, s, ws)

	// Act
	if err := s.Reopen(context.Background(), ws.ID, testServing); err != nil {
		t.Fatalf("Reopen: %v", err)
	}

	// Assert
	got, err := s.Workspace(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if got.MergeHidden() {
		t.Fatalf("the reopened merge workspace is hidden (revealed = %v), want it drawn", got.MergeRevealed)
	}
}

func TestReopenKeepsTheLandingOfAWorkspaceThatIsNotMerged(t *testing.T) {
	// Arrange: a requester whose detached merge landed keeps its worktree and
	// is never stamped merged; the sweep still owes it its retirement.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	landing := Landing{Head: "f00d", At: firstClose}
	if err := s.SetLanded(context.Background(), ws.ID, &landing); err != nil {
		t.Fatalf("SetLanded: %v", err)
	}
	if err := s.MarkClosed(context.Background(), ws.ID, Closure{At: firstClose, By: CloseByUser}); err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}

	// Act
	if err := s.Reopen(context.Background(), ws.ID, testServing); err != nil {
		t.Fatalf("Reopen: %v", err)
	}

	// Assert
	got, err := s.Workspace(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if got.Landed == nil || got.Landed.Head != landing.Head {
		t.Fatalf("landed = %+v, want the landing %+v kept", got.Landed, landing)
	}
}

func TestReopenKeepsTheLiveCountOfAWorkspaceThatIsNotMerged(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if _, err := s.SetLiveCommitCount(context.Background(), ws.ID, 3); err != nil {
		t.Fatalf("SetLiveCommitCount: %v", err)
	}
	if err := s.MarkClosed(context.Background(), ws.ID, Closure{At: firstClose, By: CloseByUser}); err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}

	// Act
	if err := s.Reopen(context.Background(), ws.ID, testServing); err != nil {
		t.Fatalf("Reopen: %v", err)
	}

	// Assert
	if got := scalar[*int64](t, s, `SELECT commit_count FROM workspaces WHERE id = ?`, ws.ID); got == nil || *got != 3 {
		t.Fatalf("commit_count = %v, want the live count 3 kept", got)
	}
}

func TestReopenRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.Reopen(context.Background(), WorkspaceID("absent"), testServing)

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
