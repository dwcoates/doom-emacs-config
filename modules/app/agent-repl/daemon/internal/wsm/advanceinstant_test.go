package wsm

import (
	"context"
	"database/sql"
	"errors"
	"testing"
	"time"
)

func TestAdvanceInstantOnlyMovesForward(t *testing.T) {
	earlier := time.Date(2026, 10, 14, 9, 0, 0, 0, time.UTC)
	later := earlier.Add(time.Minute)
	tests := []struct {
		name   string
		column string
		writes []time.Time
		want   time.Time
	}{
		{name: "a first write wins over NULL", column: "status_changed_at", writes: []time.Time{earlier}, want: earlier},
		{name: "a later write moves it", column: "status_changed_at", writes: []time.Time{earlier, later}, want: later},
		{name: "an earlier write leaves it", column: "status_changed_at", writes: []time.Time{later, earlier}, want: later},
		{name: "the last-activity stamp shares the statement", column: "last_activity_at", writes: []time.Time{later, earlier}, want: later},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			ctx := context.Background()
			s, _ := testStore(t)
			ws := testWorkspace(t, s)

			// Act
			for _, at := range tt.writes {
				err := s.write(ctx, "test.advance_instant", map[string]any{}, func(ctx context.Context, tx *sql.Tx) error {
					_, err := advanceInstant(ctx, tx, tt.column, ws.ID, at)
					return err
				})
				if err != nil {
					t.Fatalf("advanceInstant(%v): %v", at, err)
				}
			}

			// Assert
			if got := scalar[int64](t, s, `SELECT `+tt.column+` FROM workspaces WHERE id = ?`, ws.ID); got != nanos(tt.want) {
				t.Fatalf("%s = %d, want %d", tt.column, got, nanos(tt.want))
			}
		})
	}
}

func TestAdvanceInstantAffectsNoRowForAnUnknownWorkspace(t *testing.T) {
	// Arrange
	ctx := context.Background()
	s, _ := testStore(t)
	var affected int64

	// Act
	err := s.write(ctx, "test.advance_instant", map[string]any{}, func(ctx context.Context, tx *sql.Tx) error {
		res, err := advanceInstant(ctx, tx, "status_changed_at", WorkspaceID("absent"), time.Now())
		if err != nil {
			return err
		}
		affected, err = res.RowsAffected()
		return err
	})

	// Assert
	if err != nil {
		t.Fatalf("advanceInstant: %v", err)
	}
	if affected != 0 {
		t.Fatalf("rows affected = %d, want 0", affected)
	}
}

func TestWriteAdvancedInstantMovesOnlyForwardAndRefusesAnUnknownWorkspace(t *testing.T) {
	earlier := time.Date(2026, 11, 23, 9, 0, 0, 0, time.UTC)
	later := earlier.Add(time.Minute)
	tests := []struct {
		name    string
		known   bool
		writes  []time.Time
		want    time.Time
		wantErr error
	}{
		{name: "a later write moves the instant", known: true, writes: []time.Time{earlier, later}, want: later},
		{name: "an earlier write leaves it", known: true, writes: []time.Time{later, earlier}, want: later},
		{name: "an unknown workspace is refused", writes: []time.Time{earlier}, wantErr: ErrNotFound},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			ctx := context.Background()
			s, _ := testStore(t)
			id := WorkspaceID("absent")
			if tt.known {
				id = testWorkspace(t, s).ID
			}

			// Act
			var err error
			for _, at := range tt.writes {
				err = s.writeAdvancedInstant(ctx, "test.advance", nil, "last_interacted_at", id, at)
			}

			// Assert
			if tt.wantErr != nil {
				if !errors.Is(err, tt.wantErr) {
					t.Fatalf("writeAdvancedInstant = %v, want %v", err, tt.wantErr)
				}
				return
			}
			got, rerr := s.Workspace(ctx, id)
			if err != nil || rerr != nil {
				t.Fatalf("write = %v, read = %v", err, rerr)
			}
			if got.InteractedAt == nil || !got.InteractedAt.Equal(tt.want) {
				t.Fatalf("instant = %v, want %v", got.InteractedAt, tt.want)
			}
		})
	}
}
