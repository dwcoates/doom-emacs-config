package feed

import (
	"context"
	"errors"
	"slices"
	"testing"

	"agent-repld/internal/dlog"
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

// testAnswer answers every connection-failed card's turn, recording into
// STORE, with recognizable messages.
func testAnswer(s *wsState, store *fakeTurnSet) turnAnswer {
	return turnAnswer{
		op: "daemon.feed.test_answer", names: unreachableEnding, retires: unreachableEnding,
		into: s.reconnected, record: store.RecordReconnectedTurns, fields: dlog.Context{"why": "test"},
		nothing: "nothing", failed: "failed", warning: "warned", answered: "answered",
	}
}

func TestAnswerTurns(t *testing.T) {
	cases := []struct {
		name        string
		draw        func(h *harness)
		putErr      error
		wantErr     bool
		wantSet     []ids.TurnID
		wantDrawn   map[string]bool
		wantLevel   string
		wantRecords []ids.TurnID
	}{
		{name: "no row names a turn: nothing is answered or recorded",
			draw:      func(h *harness) { h.deliverPrompt("t1", "q"); h.outageTurn("t1", overloaded()) },
			wantDrawn: map[string]bool{"t1": true}, wantLevel: "debug"},
		{name: "the named turns are answered, their rows retired and recorded",
			draw: func(h *harness) {
				h.connectionFailedTurn("t1")
				h.deliverPrompt("t2", "q")
				h.outageTurn("t2", overloaded())
			},
			wantSet: []ids.TurnID{"t1"}, wantDrawn: map[string]bool{"t1": false, "t2": true},
			wantLevel: "info", wantRecords: []ids.TurnID{"t1"}},
		{name: "a failed recording still retires the rows, and is returned",
			draw: func(h *harness) { h.connectionFailedTurn("t1") }, putErr: errors.New("disk full"), wantErr: true,
			wantSet: []ids.TurnID{"t1"}, wantDrawn: map[string]bool{"t1": false}, wantLevel: "error"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			tc.draw(h)
			store := &fakeTurnSet{putErr: tc.putErr}
			h.resolver.mu.Lock()
			s := h.resolver.state(testWorkspace)
			answer := testAnswer(s, store)

			// Act
			err := h.resolver.answerTurns(s, answer)
			h.resolver.mu.Unlock()

			// Assert
			if (err != nil) != tc.wantErr {
				t.Fatalf("err = %v, want error %v", err, tc.wantErr)
			}
			if len(s.reconnected) != len(tc.wantSet) {
				t.Fatalf("set = %v, want %v", s.reconnected, tc.wantSet)
			}
			for _, turn := range tc.wantSet {
				if !s.reconnected[turn] {
					t.Fatalf("set = %v, want %v", s.reconnected, tc.wantSet)
				}
			}
			for turn, drawn := range tc.wantDrawn {
				if h.endingDrawn(turn) != drawn {
					t.Fatalf("%s drawn = %v, want %v", turn, h.endingDrawn(turn), drawn)
				}
			}
			if !slices.Equal(store.turns, tc.wantRecords) {
				t.Fatalf("recorded %v, want %v", store.turns, tc.wantRecords)
			}
			if !h.hasRecordWith(tc.wantLevel, answer.op, "why", "test") {
				t.Fatalf("records = %+v, want a %s record carrying the answer's fields", h.records(), tc.wantLevel)
			}
			if tc.wantErr {
				assertRecordedFault(t, h, answer.op, "disk full")
			}
		})
	}
}

// TestEveryTurnAnswerGoesThroughTheOneHelper pins that each act answering
// turns for good records the helper's INFO shape: a hand-rolled answer that
// skipped it would not carry the turns, the first turn and the retired count.
func TestEveryTurnAnswerGoesThroughTheOneHelper(t *testing.T) {
	cases := []struct {
		name string
		op   string
		act  func(h *harness)
	}{
		{name: "a sign-in", op: opSignedIn, act: func(h *harness) {
			h.failedTurn("t1", authFailed())
			if err := h.resolver.OnSignedIn(testWorkspace); err != nil {
				h.t.Fatalf("OnSignedIn: %v", err)
			}
		}},
		{name: "the connection coming back", op: opReconnected, act: func(h *harness) {
			h.resolver.NetworkFaultOpened(testWorkspace, "f-1")
			h.connectionFailedTurn("t1")
			h.resolver.NetworkFaultClosed(testWorkspace, "f-1")
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			tc.act(h)

			// Assert
			if !h.hasRecordWith("info", tc.op, "first", "t1") || !h.hasRecordWith("info", tc.op, "turns", 1) {
				t.Fatalf("records = %+v, want the helper's INFO under %s", h.records(), tc.op)
			}
		})
	}
}
