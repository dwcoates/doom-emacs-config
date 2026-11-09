package cachelapse

import (
	"context"
	"reflect"
	"testing"
	"time"

	"agent-repld/internal/ids"
)

func TestStateNextKeepalives(t *testing.T) {
	at := now.Add(KeepaliveInterval)
	later := at.Add(time.Hour)
	tests := []struct {
		name    string
		stated  map[ids.WorkspaceID]time.Time
		beats   map[ids.WorkspaceID]time.Time
		want    []nextKeepaliveCall
		wantMap map[ids.WorkspaceID]time.Time
	}{
		{
			name:    "a newly armed beat is stated",
			beats:   map[ids.WorkspaceID]time.Time{theWorkspace: at},
			want:    []nextKeepaliveCall{{ws: theWorkspace, next: at}},
			wantMap: map[ids.WorkspaceID]time.Time{theWorkspace: at},
		},
		{
			name:    "an unchanged beat is not stated again",
			stated:  map[ids.WorkspaceID]time.Time{theWorkspace: at},
			beats:   map[ids.WorkspaceID]time.Time{theWorkspace: at},
			wantMap: map[ids.WorkspaceID]time.Time{theWorkspace: at},
		},
		{
			name:    "a moved beat states its new instant",
			stated:  map[ids.WorkspaceID]time.Time{theWorkspace: at},
			beats:   map[ids.WorkspaceID]time.Time{theWorkspace: later},
			want:    []nextKeepaliveCall{{ws: theWorkspace, next: later}},
			wantMap: map[ids.WorkspaceID]time.Time{theWorkspace: later},
		},
		{
			name:    "a disarmed beat states none",
			stated:  map[ids.WorkspaceID]time.Time{theWorkspace: at},
			beats:   map[ids.WorkspaceID]time.Time{},
			want:    []nextKeepaliveCall{{ws: theWorkspace, next: time.Time{}}},
			wantMap: map[ids.WorkspaceID]time.Time{},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t, beatable(visible(), 10*time.Minute))
			h.keeper.beats = tt.beats
			if tt.stated != nil {
				h.keeper.stated = tt.stated
			}

			// Act
			h.keeper.stateNextKeepalives()

			// Assert
			if !reflect.DeepEqual(h.nextKeepalives, tt.want) {
				t.Errorf("stated = %v, want %v", h.nextKeepalives, tt.want)
			}
			if !reflect.DeepEqual(h.keeper.stated, tt.wantMap) {
				t.Errorf("last stated = %v, want %v", h.keeper.stated, tt.wantMap)
			}
		})
	}
}

func TestAParkedSessionsScheduleIsStatedOnStart(t *testing.T) {
	// Arrange: a session parked inside the cutoff, owed its next beat.
	h := newHarness(t, beatable(visible(), 10*time.Minute))
	h.db.sessions[theWorkspace] = parkedSession(theWorkspace)

	// Act
	if err := h.keeper.restore(context.Background()); err != nil {
		t.Fatalf("restore: %v", err)
	}

	// Assert
	due, armed := h.keeper.beats[theWorkspace]
	if !armed {
		t.Fatal("no beat was armed for the parked session")
	}
	want := []nextKeepaliveCall{{ws: theWorkspace, next: due}}
	if !reflect.DeepEqual(h.nextKeepalives, want) {
		t.Fatalf("stated = %v, want %v", h.nextKeepalives, want)
	}
}

func TestAParkPastTheCutoffIsStatedNone(t *testing.T) {
	// Arrange: the park the idle sweep leaves; a beat was stated before.
	h := newHarness(t, beatable(visible(), 10*time.Minute))
	s := parkedSession(theWorkspace)
	s.LastEngagementAt = now.Add(-6 * time.Hour)
	h.db.sessions[theWorkspace] = s
	h.keeper.stated[theWorkspace] = now.Add(KeepaliveInterval)

	// Act
	h.keeper.schedule(context.Background(), theWorkspace, "test")
	h.keeper.stateNextKeepalives()

	// Assert
	want := []nextKeepaliveCall{{ws: theWorkspace, next: time.Time{}}}
	if !reflect.DeepEqual(h.nextKeepalives, want) {
		t.Fatalf("stated = %v, want %v", h.nextKeepalives, want)
	}
}
