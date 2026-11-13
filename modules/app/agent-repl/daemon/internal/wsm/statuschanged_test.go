package wsm

import (
	"context"
	"errors"
	"path/filepath"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

var (
	earlierStatusChange = time.Date(2026, 10, 14, 9, 0, 0, 0, time.UTC)
	laterStatusChange   = earlierStatusChange.Add(time.Minute)
)

func TestSetStatusChangedAtRecordsTheLatestInstant(t *testing.T) {
	tests := []struct {
		name   string
		writes []time.Time
		want   time.Time
	}{
		{name: "a first change is recorded", writes: []time.Time{earlierStatusChange}, want: earlierStatusChange},
		{name: "a later change moves it forward", writes: []time.Time{earlierStatusChange, laterStatusChange}, want: laterStatusChange},
		{name: "an older change never moves it back", writes: []time.Time{laterStatusChange, earlierStatusChange}, want: laterStatusChange},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			ctx := context.Background()
			s, _ := testStore(t)
			ws := testWorkspace(t, s)

			// Act
			for _, at := range tt.writes {
				if err := s.SetStatusChangedAt(ctx, ws.ID, at); err != nil {
					t.Fatalf("SetStatusChangedAt(%v): %v", at, err)
				}
			}

			// Assert
			got, err := s.Workspace(ctx, ws.ID)
			if err != nil {
				t.Fatalf("Workspace: %v", err)
			}
			if got.StatusChangedAt == nil || !got.StatusChangedAt.Equal(tt.want) {
				t.Fatalf("StatusChangedAt = %v, want %v", got.StatusChangedAt, tt.want)
			}
		})
	}
}

func TestWorkspaceReadsNoStatusChangeBeforeOneIsRecorded(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	got, err := s.Workspace(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if got.StatusChangedAt != nil {
		t.Fatalf("StatusChangedAt = %v, want nil on a fresh workspace", got.StatusChangedAt)
	}
}

func TestSetStatusChangedAtRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	s, log := testStore(t)

	// Act
	err := s.SetStatusChangedAt(context.Background(), WorkspaceID("absent"), laterStatusChange)

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("SetStatusChangedAt = %v, want ErrNotFound", err)
	}
	record, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Operation: "daemon.wsm.set_status_changed_at"})
	if !ok {
		t.Fatalf("the refusal was not recorded: %v", log.Records())
	}
	if record.Context["workspace"] != "absent" {
		t.Fatalf("the refusal's workspace = %v, want absent", record.Context["workspace"])
	}
}

// readOnlyWithWorkspace opens a READ-ONLY handle, as a joining successor
// does, on a file holding one workspace, with a logger the test reads.
func readOnlyWithWorkspace(t *testing.T) (DB, WorkspaceID, *dlog.TestLogger) {
	t.Helper()
	ctx := context.Background()
	path := filepath.Join(t.TempDir(), "wsm.db")
	w, err := Open(ctx, path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	ws := testWorkspace(t, w.(*store))
	if err := w.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}
	log := dlog.NewTestLogger()
	ro, err := OpenReadOnly(ctx, path, WithUnsyncedWrites(), WithLogger(log))
	if err != nil {
		t.Fatalf("OpenReadOnly: %v", err)
	}
	t.Cleanup(func() { ro.Close() })
	return ro, ws.ID, log
}

func TestAStatusChangeOnAReadOnlyHandleIsHeldNotRefused(t *testing.T) {
	// Arrange
	ro, id, log := readOnlyWithWorkspace(t)

	// Act
	err := ro.SetStatusChangedAt(context.Background(), id, laterStatusChange)

	// Assert
	if err != nil {
		t.Fatalf("SetStatusChangedAt on a read-only handle = %v, want nil: a joining successor's instant is held", err)
	}
	for _, r := range log.Records() {
		if r.Level == "error" || r.Level == "warn" {
			t.Fatalf("a held status change recorded %s: %+v", r.Level, r)
		}
	}
}

func TestStatusChangesHeldOnAReadOnlyHandleWriteTheLatestAtThePromotion(t *testing.T) {
	// Arrange: two instants held, the later first.
	ctx := context.Background()
	ro, id, _ := readOnlyWithWorkspace(t)
	for _, at := range []time.Time{laterStatusChange, earlierStatusChange} {
		if err := ro.SetStatusChangedAt(ctx, id, at); err != nil {
			t.Fatalf("SetStatusChangedAt: %v", err)
		}
	}

	// Act
	if err := ro.Promote(ctx); err != nil {
		t.Fatalf("Promote: %v", err)
	}

	// Assert
	got, err := ro.Workspace(ctx, id)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if got.StatusChangedAt == nil || !got.StatusChangedAt.Equal(laterStatusChange) {
		t.Fatalf("StatusChangedAt after the promotion = %v, want the latest held %v", got.StatusChangedAt, laterStatusChange)
	}
}
