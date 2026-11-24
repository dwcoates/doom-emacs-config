package wsm

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"regexp"
	"testing"
	"time"
)

func TestRecordWork(t *testing.T) {
	tests := []struct {
		name   string
		writes []time.Time
		want   time.Time
	}{
		{name: "the first work is recorded", writes: []time.Time{instant}, want: instant},
		{name: "later work moves the clock forward", writes: []time.Time{instant, instant.Add(time.Hour)}, want: instant.Add(time.Hour)},
		{name: "a late write never pulls the clock back", writes: []time.Time{instant.Add(time.Hour), instant}, want: instant.Add(time.Hour)},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)

			// Act
			for _, at := range tt.writes {
				if err := s.RecordWork(context.Background(), ws.ID, at, WorkEnded); err != nil {
					t.Fatalf("RecordWork: %v", err)
				}
			}

			// Assert
			got, err := s.Workspace(context.Background(), ws.ID)
			if err != nil {
				t.Fatalf("Workspace: %v", err)
			}
			if got.LastWorkAt == nil || !got.LastWorkAt.Equal(tt.want) {
				t.Fatalf("last work = %v, want %v", got.LastWorkAt, tt.want)
			}
		})
	}
}

func TestRecordWorkRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	s, log := testStore(t)

	// Act
	err := s.RecordWork(context.Background(), "ws-unknown", instant, WorkPromptDelivered)

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("RecordWork = %v, want ErrNotFound", err)
	}
	// A write naming a row the caller no longer holds is the caller's to
	// judge, so the store records it at DEBUG (store.writeTx); the keeper
	// and the prompt queue record their failed writes at ERROR.
	if !loggedOperation(log, "daemon.wsm.record_work", "debug") {
		t.Fatalf("the refused write was not recorded: %v", log.Records())
	}
}

// TestPutSessionNeverMovesTheIdleClock pins invariant A's session half: a
// session start of any cause (a keep-alive's restore, a deploy's start, a
// takeover) rewrites the session row and leaves the workspace's idle clock
// where real work left it.
func TestPutSessionNeverMovesTheIdleClock(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.RecordWork(context.Background(), ws.ID, instant, WorkEnded); err != nil {
		t.Fatalf("RecordWork: %v", err)
	}
	restored := instant.Add(3 * time.Hour)

	// Act
	if err := s.PutSession(context.Background(), Session{Workspace: ws.ID, HostSessionID: "host-1", StartedAt: instant, KeepaliveRestoredAt: &restored}); err != nil {
		t.Fatalf("PutSession: %v", err)
	}
	if err := s.PutSession(context.Background(), Session{Workspace: ws.ID, HostSessionID: "host-2", StartedAt: instant}); err != nil {
		t.Fatalf("PutSession: %v", err)
	}

	// Assert
	got, err := s.Workspace(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if got.LastWorkAt == nil || !got.LastWorkAt.Equal(instant) {
		t.Fatalf("last work = %v, want the untouched %v", got.LastWorkAt, instant)
	}
}

func TestIdleSince(t *testing.T) {
	worked := instant.Add(time.Hour)
	tests := []struct {
		name string
		ws   Workspace
		want time.Time
	}{
		{name: "a workspace that worked is idle since its last work", ws: Workspace{CreatedAt: instant, LastWorkAt: &worked}, want: worked},
		{name: "a workspace that never worked is idle since its creation", ws: Workspace{CreatedAt: instant}, want: instant},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := tt.ws.IdleSince()

			// Assert
			if !got.Equal(tt.want) {
				t.Fatalf("IdleSince = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestWorkedSince(t *testing.T) {
	worked := instant.Add(time.Hour)
	tests := []struct {
		name  string
		since time.Time
		want  bool
	}{
		{name: "work after the instant", since: instant, want: true},
		{name: "work at the instant is not since it", since: worked, want: false},
		{name: "no work after the instant", since: worked.Add(time.Minute), want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			ws := Workspace{CreatedAt: instant, LastWorkAt: &worked}

			// Act
			at, got := ws.WorkedSince(tt.since)

			// Assert
			if got != tt.want || !at.Equal(worked) {
				t.Fatalf("WorkedSince(%v) = %v, %v; want %v, %v", tt.since, at, got, worked, tt.want)
			}
		})
	}
}

// TestLastWorkAtHasOneWriter pins the structure invariant A rests on: the
// only statement in this package that writes last_work_at is RecordWork's
// (through advanceInstant), beside the layout's own DDL. A second writer is
// a path that is not work reaching the idle clock.
func TestLastWorkAtHasOneWriter(t *testing.T) {
	// Arrange
	files, err := filepath.Glob("*.go")
	if err != nil {
		t.Fatalf("Glob: %v", err)
	}
	writes := regexp.MustCompile(`last_work_at\s*=|"last_work_at"`)
	var found []string

	// Act
	for _, file := range files {
		if filepath.Ext(file) != ".go" || regexp.MustCompile(`_test\.go$`).MatchString(file) {
			continue
		}
		body, err := os.ReadFile(file)
		if err != nil {
			t.Fatalf("ReadFile %s: %v", file, err)
		}
		for range writes.FindAllIndex(body, -1) {
			found = append(found, file)
		}
	}

	// Assert
	want := []string{"lastwork.go", "lastwork.go"}
	if len(found) != len(want) || found[0] != want[0] || found[1] != want[1] {
		t.Fatalf("last_work_at is written in %v, want only lastwork.go's DDL backfill and RecordWork", found)
	}
}
