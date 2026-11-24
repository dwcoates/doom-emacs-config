package wsm

import (
	"context"
	"database/sql"
	"errors"
	"testing"
	"time"
)

var (
	earlierInteraction = time.Date(2026, 11, 23, 9, 0, 0, 0, time.UTC)
	laterInteraction   = earlierInteraction.Add(time.Minute)
)

func TestSetViewedStatusRecordsTheViewedArm(t *testing.T) {
	tests := []struct {
		name   string
		writes []string
		want   string
	}{
		{name: "a viewed arm is recorded", writes: []string{"thinking"}, want: "thinking"},
		{name: "a later arm replaces it", writes: []string{"thinking", "done"}, want: "done"},
		{name: "an empty arm clears it", writes: []string{"done", ""}, want: ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			ctx := context.Background()
			s, _ := testStore(t)
			ws := testWorkspace(t, s)

			// Act
			for _, arm := range tt.writes {
				if err := s.SetViewedStatus(ctx, ws.ID, arm); err != nil {
					t.Fatalf("SetViewedStatus(%q): %v", arm, err)
				}
			}

			// Assert
			got, err := s.Workspace(ctx, ws.ID)
			if err != nil {
				t.Fatalf("Workspace: %v", err)
			}
			if got.ViewedStatus != tt.want {
				t.Fatalf("ViewedStatus = %q, want %q", got.ViewedStatus, tt.want)
			}
		})
	}
}

func TestWorkspaceReadsNoViewedStatusOrInteractionBeforeOneIsRecorded(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	got, err := s.Workspace(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if got.ViewedStatus != "" || got.InteractedAt != nil {
		t.Fatalf("ViewedStatus = %q, InteractedAt = %v, want neither recorded", got.ViewedStatus, got.InteractedAt)
	}
}

func TestSetInteractedAtRecordsTheLatestInstant(t *testing.T) {
	tests := []struct {
		name   string
		writes []time.Time
		want   time.Time
	}{
		{name: "a first interaction is recorded", writes: []time.Time{earlierInteraction}, want: earlierInteraction},
		{name: "a later interaction moves it forward", writes: []time.Time{earlierInteraction, laterInteraction}, want: laterInteraction},
		{name: "an older interaction never moves it back", writes: []time.Time{laterInteraction, earlierInteraction}, want: laterInteraction},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			ctx := context.Background()
			s, _ := testStore(t)
			ws := testWorkspace(t, s)

			// Act
			for _, at := range tt.writes {
				if err := s.SetInteractedAt(ctx, ws.ID, at); err != nil {
					t.Fatalf("SetInteractedAt(%v): %v", at, err)
				}
			}

			// Assert
			got, err := s.Workspace(ctx, ws.ID)
			if err != nil {
				t.Fatalf("Workspace: %v", err)
			}
			if got.InteractedAt == nil || !got.InteractedAt.Equal(tt.want) {
				t.Fatalf("InteractedAt = %v, want %v", got.InteractedAt, tt.want)
			}
		})
	}
}

func TestTheInteractionWritesRefuseAnUnknownWorkspace(t *testing.T) {
	tests := []struct {
		name  string
		write func(s *store) error
	}{
		{name: "SetViewedStatus", write: func(s *store) error {
			return s.SetViewedStatus(context.Background(), WorkspaceID("absent"), "done")
		}},
		{name: "SetInteractedAt", write: func(s *store) error {
			return s.SetInteractedAt(context.Background(), WorkspaceID("absent"), earlierInteraction)
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)

			// Act
			err := tt.write(s)

			// Assert
			if !errors.Is(err, ErrNotFound) {
				t.Fatalf("%s = %v, want ErrNotFound", tt.name, err)
			}
		})
	}
}

func TestWorkspaceRefusesAnEmptyRecordedViewedStatus(t *testing.T) {
	// Arrange: a row whose viewed status names no arm is corrupt, since the
	// write spells "not viewed" as NULL.
	ctx := context.Background()
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.write(ctx, "test.corrupt", nil, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx, `UPDATE workspaces SET viewed_status = '' WHERE id = ?`, string(ws.ID))
		return err
	}); err != nil {
		t.Fatalf("corrupt the row: %v", err)
	}

	// Act
	_, err := s.Workspace(ctx, ws.ID)

	// Assert
	var refusal *DecodeError
	if !errors.As(err, &refusal) || refusal.Field != "viewed_status" {
		t.Fatalf("Workspace = %v, want a *DecodeError naming viewed_status", err)
	}
}

func TestTheMigrationAddsTheInteractionColumnsAndRetiresResultRead(t *testing.T) {
	tests := []struct {
		name   string
		column string
		want   int
	}{
		{name: "viewed_status is added", column: "viewed_status", want: 1},
		{name: "last_interacted_at is added", column: "last_interacted_at", want: 1},
		{name: "result_read is retired", column: "result_read", want: 0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange — a file written long before either column.
			path := layout3Fixture(t)

			// Act
			handle, err := Open(context.Background(), path, WithUnsyncedWrites())
			if err != nil {
				t.Fatalf("Open on a layout-3 database: %v", err)
			}
			defer handle.Close()

			// Assert
			got := scalar[int](t, handle.(*store),
				`SELECT count(*) FROM pragma_table_info('workspaces') WHERE name = '`+tt.column+`'`)
			if got != tt.want {
				t.Fatalf("workspaces.%s exists %d times after the migration, want %d", tt.column, got, tt.want)
			}
		})
	}
}
