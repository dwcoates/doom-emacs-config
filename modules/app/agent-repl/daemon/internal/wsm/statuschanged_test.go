package wsm

import (
	"context"
	"errors"
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
