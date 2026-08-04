package statedb

import (
	"path/filepath"
	"testing"
)

func newKeepAliveWindows(t *testing.T) *KeepAliveWindows {
	t.Helper()
	db, err := Open(filepath.Join(t.TempDir(), "state.db"))
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(func() { _ = db.Close() })
	k, err := NewKeepAliveWindows(db)
	if err != nil {
		t.Fatalf("NewKeepAliveWindows: %v", err)
	}
	return k
}

// THE WINDOW IS THE VERDICT. An item's own timestamp is the only thing a
// file-plane record carries that can be joined to a ping, so placing it in a
// closed interval is the whole exclusion.
func TestKeepAliveWindowCovers(t *testing.T) {
	tests := []struct {
		name string
		tsMs int64
		want bool
	}{
		{name: "before the ping started", tsMs: 900, want: false},
		{name: "exactly at the start", tsMs: 1_000, want: true},
		{name: "inside the window", tsMs: 1_500, want: true},
		{name: "exactly at the end", tsMs: 2_000, want: true},
		{name: "after the ping ended", tsMs: 2_100, want: false},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			k := newKeepAliveWindows(t)
			if err := k.Open(KeepAliveWindow{TurnID: "ka_1", Workspace: "/ws", StartedAtMs: 1_000}); err != nil {
				t.Fatalf("Open: %v", err)
			}
			if err := k.Close("ka_1", 2_000); err != nil {
				t.Fatalf("Close: %v", err)
			}

			// Act.
			got, err := k.Covers("/ws", tc.tsMs)

			// Assert.
			if err != nil {
				t.Fatalf("Covers: %v", err)
			}
			if got != tc.want {
				t.Fatalf("Covers(%d) = %v, want %v", tc.tsMs, got, tc.want)
			}
		})
	}
}

// AN OPEN WINDOW HAS NO UPPER BOUND. While a ping is in flight everything the
// vendor writes for that workspace belongs to it, and treating an unclosed
// window as empty would leak exactly the records a live ping produces — the
// ones a user is most likely to be watching for.
func TestKeepAliveWindowOpenWindowCoversEverythingAfterItsStart(t *testing.T) {
	// Arrange.
	k := newKeepAliveWindows(t)
	if err := k.Open(KeepAliveWindow{TurnID: "ka_1", Workspace: "/ws", StartedAtMs: 1_000}); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Act.
	got, err := k.Covers("/ws", 9_999_999)

	// Assert.
	if err != nil {
		t.Fatalf("Covers: %v", err)
	}
	if !got {
		t.Fatal("an open window did not cover a later item; a live ping's records would leak into the conversation")
	}
}

// A window on one workspace says nothing about another's items.
func TestKeepAliveWindowIsScopedToItsWorkspace(t *testing.T) {
	// Arrange.
	k := newKeepAliveWindows(t)
	if err := k.Open(KeepAliveWindow{TurnID: "ka_1", Workspace: "/ws", StartedAtMs: 1_000}); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Act.
	got, err := k.Covers("/other", 1_500)

	// Assert.
	if err != nil {
		t.Fatalf("Covers: %v", err)
	}
	if got {
		t.Fatal("a window on one workspace covered another workspace's item")
	}
}

// THE VERDICT IS STABLE ACROSS LIVE AND REPLAY. The ledger is keyed on facts
// the item carries, so the same timestamp asked twice — once while the ping ran
// and once long after — answers the same way. That is the property the whole
// exclusion depends on: this path also runs on a resync replaying old history.
func TestKeepAliveWindowVerdictIsStableAcrossLiveAndReplay(t *testing.T) {
	// Arrange.
	k := newKeepAliveWindows(t)
	if err := k.Open(KeepAliveWindow{TurnID: "ka_1", Workspace: "/ws", StartedAtMs: 1_000}); err != nil {
		t.Fatalf("Open: %v", err)
	}
	live, err := k.Covers("/ws", 1_500)
	if err != nil {
		t.Fatalf("Covers (live): %v", err)
	}

	// Act: the ping ends and much later a resync asks about the same instant.
	if err := k.Close("ka_1", 2_000); err != nil {
		t.Fatalf("Close: %v", err)
	}
	replay, err := k.Covers("/ws", 1_500)
	if err != nil {
		t.Fatalf("Covers (replay): %v", err)
	}

	// Assert.
	if live != replay {
		t.Fatalf("live verdict %v and replay verdict %v disagree about the same instant; the exclusion would render on a resync what it withheld live", live, replay)
	}
}

// A window is never deleted, so the evidence lasts as long as the turns it
// excludes.
func TestKeepAliveWindowRowsSurviveClosing(t *testing.T) {
	// Arrange.
	k := newKeepAliveWindows(t)
	if err := k.Open(KeepAliveWindow{TurnID: "ka_1", Workspace: "/ws", StartedAtMs: 1_000}); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Act.
	if err := k.Close("ka_1", 2_000); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert.
	got, err := k.List("/ws")
	if err != nil {
		t.Fatalf("List: %v", err)
	}
	if len(got) != 1 || got[0].EndedAtMs != 2_000 {
		t.Fatalf("windows = %+v, want the one window retained with its end stamped", got)
	}
}

// An incomplete window is refused rather than written: a row with no start
// would cover nothing and silently disable the exclusion for that ping.
func TestKeepAliveWindowRefusesAnIncompleteOpen(t *testing.T) {
	// Arrange.
	k := newKeepAliveWindows(t)

	// Act.
	err := k.Open(KeepAliveWindow{TurnID: "ka_1", Workspace: "/ws"})

	// Assert.
	if err == nil {
		t.Fatal("Open accepted a window with no start instant")
	}
}

// Closing an unknown turn is a NO-OP: a daemon that restarted mid-ping
// legitimately sees the end of a window it did not open.
func TestKeepAliveWindowCloseOfAnUnknownTurnIsBenign(t *testing.T) {
	// Arrange.
	k := newKeepAliveWindows(t)

	// Act.
	err := k.Close("ka_never_opened", 2_000)

	// Assert.
	if err != nil {
		t.Fatalf("Close of an unknown turn = %v, want a benign no-op", err)
	}
}
