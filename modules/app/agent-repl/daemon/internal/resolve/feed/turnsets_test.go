package feed

import (
	"context"
	"errors"
	"testing"

	"agent-repld/internal/ids"
)

// DURABLE TURN SETS (turnsets.go): every set loads through restoreTurnSet.

// testRestore is a turnSetRestore over LOAD, with recognizable messages.
func testRestore(load func(context.Context, ids.WorkspaceID) ([]ids.TurnID, error)) turnSetRestore {
	return turnSetRestore{op: "daemon.feed.test_restore", load: load, failed: "failed", warning: "warned", loaded: "loaded"}
}

func TestRestoreTurnSet(t *testing.T) {
	cases := []struct {
		name     string
		turns    []ids.TurnID
		err      error
		wantSet  map[ids.TurnID]bool
		wantInfo bool
		wantErr  bool
	}{
		{name: "a set holding turns fills the map and says so", turns: []ids.TurnID{"t1", "t2"},
			wantSet: map[ids.TurnID]bool{"pre": true, "t1": true, "t2": true}, wantInfo: true},
		{name: "an empty set changes nothing and records nothing", wantSet: map[ids.TurnID]bool{"pre": true}},
		{name: "a failed load is logged, raised, and leaves the map as it was", turns: []ids.TurnID{"t1"},
			err: errors.New("corrupt page"), wantSet: map[ids.TurnID]bool{"pre": true}, wantErr: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			into := map[ids.TurnID]bool{"pre": true}
			restore := testRestore(func(context.Context, ids.WorkspaceID) ([]ids.TurnID, error) { return tc.turns, tc.err })
			h.resolver.mu.Lock()
			s := h.resolver.state(testWorkspace)

			// Act
			h.resolver.restoreTurnSet(s, restore, into)
			h.resolver.mu.Unlock()

			// Assert
			if len(into) != len(tc.wantSet) {
				t.Fatalf("set = %v, want %v", into, tc.wantSet)
			}
			for turn := range tc.wantSet {
				if !into[turn] {
					t.Fatalf("set = %v, want %v", into, tc.wantSet)
				}
			}
			if got := h.hasRecord("info", restore.op); got != tc.wantInfo {
				t.Fatalf("info recorded = %v, want %v", got, tc.wantInfo)
			}
			if tc.wantErr {
				assertRecordedFault(t, h, restore.op, "corrupt page")
			} else if h.hasRecord("error", restore.op) {
				t.Fatalf("records = %+v, want no ERROR", h.records())
			}
		})
	}
}

// TestEveryDurableTurnSetIsRestoredThroughTheOneHelper pins that each set the
// feed refuses rows by is loaded at state build into its own map, under its own
// operation: a hand-rolled restore that skipped the helper's record would fail.
func TestEveryDurableTurnSetIsRestoredThroughTheOneHelper(t *testing.T) {
	cases := []struct {
		name  string
		op    string
		build func(t *testing.T, store *fakeTurnSet) *harness
		set   func(s *wsState) map[ids.TurnID]bool
	}{
		{name: "rolled back", op: opRollbackRestore,
			build: func(t *testing.T, store *fakeTurnSet) *harness {
				return newDurableHarness(t, durableStores{rolledBack: store})
			},
			set: func(s *wsState) map[ids.TurnID]bool { return s.rolledBack }},
		{name: "signed in", op: opSignedInRestore,
			build: func(t *testing.T, store *fakeTurnSet) *harness {
				return newDurableHarness(t, durableStores{signedIn: store})
			},
			set: func(s *wsState) map[ids.TurnID]bool { return s.signedIn }},
		{name: "reconnected", op: opReconnectedRestore,
			build: func(t *testing.T, store *fakeTurnSet) *harness {
				return newDurableHarness(t, durableStores{reconnected: store})
			},
			set: func(s *wsState) map[ids.TurnID]bool { return s.reconnected }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			store := &fakeTurnSet{turns: []ids.TurnID{"t1"}}

			// Act
			h := tc.build(t, store)

			// Assert
			h.resolver.mu.Lock()
			restored := tc.set(h.resolver.state(testWorkspace))["t1"]
			h.resolver.mu.Unlock()
			if !restored {
				t.Fatal("the stored turn was not restored into its set")
			}
			if !h.hasRecordWith("info", tc.op, "turns", 1) {
				t.Fatalf("records = %+v, want the helper's INFO under %s", h.records(), tc.op)
			}
		})
	}
}
