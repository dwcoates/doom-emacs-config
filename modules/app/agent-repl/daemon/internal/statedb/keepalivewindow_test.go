package statedb

import (
	"errors"
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

// ---------------------------------------------------------------------------
// Boot reconciliation
// ---------------------------------------------------------------------------

// errUnreadableTurnEnd stands in for a turn-end lookup that cannot answer.
var errUnreadableTurnEnd = errors.New("turn accounting is unreadable")

// fakeTurnEnds is a durable turn-end lookup with no database behind it.
type fakeTurnEnds struct {
	ends map[string]int64
	err  error
}

func (f fakeTurnEnds) EndedAtMs(turnID string) (int64, bool, error) {
	if f.err != nil {
		return 0, false, f.err
	}
	at, ok := f.ends[turnID]
	return at, ok, nil
}

// AT BOOT AN OPEN WINDOW IS AN ORPHAN. Nothing can be pinging before any
// session controller exists, so a row with no end can only belong to a daemon
// that died mid-ping — and it is holding an unbounded exclusion over its
// workspace's whole conversation.
func TestReconcileOpenWindowsClosesAnOrphanLeftByAPreviousDaemon(t *testing.T) {
	// Arrange.
	k := newKeepAliveWindows(t)
	if err := k.Open(KeepAliveWindow{TurnID: "ka_1", Workspace: "/ws", StartedAtMs: 1_000}); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Act.
	closed, err := k.ReconcileOpenWindows(fakeTurnEnds{})

	// Assert.
	if err != nil {
		t.Fatalf("ReconcileOpenWindows: %v", err)
	}
	if closed != 1 {
		t.Fatalf("closed %d window(s), want the one orphan", closed)
	}
}

// THE END COMES FROM THE TURN'S OWN DURABLE RECORD when the store holds one:
// that is where the ping actually stopped, and it is the bound the vendor's own
// record timestamps are compared against.
func TestReconcileOpenWindowsStampsTheDurableTurnEnd(t *testing.T) {
	// Arrange.
	k := newKeepAliveWindows(t)
	if err := k.Open(KeepAliveWindow{TurnID: "ka_1", Workspace: "/ws", StartedAtMs: 1_000}); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Act.
	if _, err := k.ReconcileOpenWindows(fakeTurnEnds{ends: map[string]int64{"ka_1": 2_000}}); err != nil {
		t.Fatalf("ReconcileOpenWindows: %v", err)
	}

	// Assert.
	if got := listOneWindow(t, k, "/ws"); got.EndedAtMs != 2_000 {
		t.Fatalf("ended_at_ms = %d, want the turn's own recorded end 2000", got.EndedAtMs)
	}
}

// WITH NO DURABLE TURN END the window closes at its OWN START. The ping never
// reported an end, so the only interval the daemon can honestly claim is the
// instant it committed to a ping it never finished — never now, which would
// swallow every real turn the outage spanned.
func TestReconcileOpenWindowsClosesAtItsStartWhenNoTurnEndIsRecorded(t *testing.T) {
	// Arrange.
	k := newKeepAliveWindows(t)
	if err := k.Open(KeepAliveWindow{TurnID: "ka_1", Workspace: "/ws", StartedAtMs: 1_000}); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Act.
	if _, err := k.ReconcileOpenWindows(fakeTurnEnds{}); err != nil {
		t.Fatalf("ReconcileOpenWindows: %v", err)
	}

	// Assert.
	if got := listOneWindow(t, k, "/ws"); got.EndedAtMs != 1_000 {
		t.Fatalf("ended_at_ms = %d, want the window's own start 1000", got.EndedAtMs)
	}
}

// A TURN END EARLIER THAN THE WINDOW'S START cannot narrow the window below the
// instant the ping was committed to: an interval ending before it begins would
// misreport when the daemon was pinging.
func TestReconcileOpenWindowsNeverClosesBeforeItsStart(t *testing.T) {
	// Arrange.
	k := newKeepAliveWindows(t)
	if err := k.Open(KeepAliveWindow{TurnID: "ka_1", Workspace: "/ws", StartedAtMs: 1_000}); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Act.
	if _, err := k.ReconcileOpenWindows(fakeTurnEnds{ends: map[string]int64{"ka_1": 400}}); err != nil {
		t.Fatalf("ReconcileOpenWindows: %v", err)
	}

	// Assert.
	if got := listOneWindow(t, k, "/ws"); got.EndedAtMs != 1_000 {
		t.Fatalf("ended_at_ms = %d, want the window's own start 1000", got.EndedAtMs)
	}
}

// AN ALREADY-CLOSED WINDOW IS NOT TOUCHED. Its end is the ping's real one, and
// re-stamping it would move a settled verdict.
func TestReconcileOpenWindowsLeavesAClosedWindowAlone(t *testing.T) {
	// Arrange.
	k := newKeepAliveWindows(t)
	if err := k.Open(KeepAliveWindow{TurnID: "ka_1", Workspace: "/ws", StartedAtMs: 1_000}); err != nil {
		t.Fatalf("Open: %v", err)
	}
	if err := k.Close("ka_1", 2_000); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Act.
	closed, err := k.ReconcileOpenWindows(fakeTurnEnds{ends: map[string]int64{"ka_1": 9_000}})

	// Assert.
	if err != nil {
		t.Fatalf("ReconcileOpenWindows: %v", err)
	}
	if closed != 0 {
		t.Fatalf("closed %d window(s), want none — the row was already settled", closed)
	}
}

// THE REPAIR IS WHAT LETS THE CONVERSATION RENDER AGAIN: before it, every later
// item on the workspace falls inside the unbounded window and is withheld.
func TestReconcileOpenWindowsStopsAnOrphanWithholdingLaterItems(t *testing.T) {
	// Arrange.
	k := newKeepAliveWindows(t)
	if err := k.Open(KeepAliveWindow{TurnID: "ka_1", Workspace: "/ws", StartedAtMs: 1_000}); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Act.
	if _, err := k.ReconcileOpenWindows(fakeTurnEnds{}); err != nil {
		t.Fatalf("ReconcileOpenWindows: %v", err)
	}

	// Assert.
	covered, err := k.Covers("/ws", 500_000)
	if err != nil {
		t.Fatalf("Covers: %v", err)
	}
	if covered {
		t.Fatal("a later item is still withheld after the orphan was reconciled; the blackout outlived the daemon that caused it")
	}
}

// A RECONCILIATION WITH NO DURABLE END SOURCE IS REFUSED rather than silently
// falling back to now: stamping from the clock is precisely the failure this
// repair exists to avoid.
func TestReconcileOpenWindowsRefusesWithoutATurnEndLookup(t *testing.T) {
	// Arrange.
	k := newKeepAliveWindows(t)

	// Act.
	_, err := k.ReconcileOpenWindows(nil)

	// Assert.
	if err == nil {
		t.Fatal("ReconcileOpenWindows(nil) = nil, want a refusal")
	}
}

// A TURN-END LOOKUP FAILURE STOPS THE REPAIR rather than closing the row at a
// bound nobody verified.
func TestReconcileOpenWindowsFailsOnAnUnreadableTurnEnd(t *testing.T) {
	// Arrange.
	k := newKeepAliveWindows(t)
	if err := k.Open(KeepAliveWindow{TurnID: "ka_1", Workspace: "/ws", StartedAtMs: 1_000}); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Act.
	_, err := k.ReconcileOpenWindows(fakeTurnEnds{err: errUnreadableTurnEnd})

	// Assert.
	if err == nil {
		t.Fatal("ReconcileOpenWindows with an unreadable turn end = nil, want the failure surfaced")
	}
}

// listOneWindow returns the workspace's single window, failing if there is not
// exactly one.
func listOneWindow(t *testing.T, k *KeepAliveWindows, workspace string) KeepAliveWindow {
	t.Helper()
	got, err := k.List(workspace)
	if err != nil {
		t.Fatalf("List: %v", err)
	}
	if len(got) != 1 {
		t.Fatalf("%d windows, want exactly one", len(got))
	}
	return got[0]
}

// A CLOSE THAT WOULD INVERT THE INTERVAL IS REFUSED BY NAME. An end below its
// own start can only come from two clocks disagreeing, and the row it would
// write covers nothing at all — the ping stops being excluded by the very act
// meant to bound it.
func TestKeepAliveWindowCloseRefusesAnInvertedInterval(t *testing.T) {
	// Arrange.
	k := newKeepAliveWindows(t)
	if err := k.Open(KeepAliveWindow{TurnID: "ka_1", Workspace: "/ws", StartedAtMs: 5_000}); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Act.
	err := k.Close("ka_1", 1_000)

	// Assert.
	if !errors.Is(err, ErrKeepAliveWindowInverted) {
		t.Fatalf("Close = %v, want ErrKeepAliveWindowInverted; an end below its own start was written silently", err)
	}
}

// THE REFUSED CLOSE STILL BOUNDS THE ROW, clamped to its own start. Leaving it
// open would trade a covers-nothing interval for an unbounded one that withholds
// the workspace's whole conversation from here on — worse than the fault being
// reported.
func TestKeepAliveWindowCloseClampsAnInvertedIntervalToItsStart(t *testing.T) {
	// Arrange.
	k := newKeepAliveWindows(t)
	if err := k.Open(KeepAliveWindow{TurnID: "ka_1", Workspace: "/ws", StartedAtMs: 5_000}); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Act.
	_ = k.Close("ka_1", 1_000)

	// Assert.
	if got := listOneWindow(t, k, "/ws"); got.EndedAtMs != 5_000 {
		t.Fatalf("ended_at_ms = %d, want the window's own start 5000", got.EndedAtMs)
	}
}

// THE RE-STAMP IS WHAT THE ON CONFLICT CLAUSE IS FOR. The pre-submit bound comes
// off the daemon's clock; the ping's own start boundary replaces it with the
// vendor's, the clock that stamps every item the window is compared against.
func TestKeepAliveWindowOpenRestampsTheStart(t *testing.T) {
	// Arrange — the provisional bound, taken before the ping was submitted.
	k := newKeepAliveWindows(t)
	if err := k.Open(KeepAliveWindow{TurnID: "ka_1", Workspace: "/ws", StartedAtMs: 9_000}); err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Act — the turn's own start boundary, on the vendor's clock.
	if err := k.Open(KeepAliveWindow{TurnID: "ka_1", Workspace: "/ws", StartedAtMs: 1_000}); err != nil {
		t.Fatalf("Open re-stamp: %v", err)
	}

	// Assert.
	if got := listOneWindow(t, k, "/ws"); got.StartedAtMs != 1_000 {
		t.Fatalf("started_at_ms = %d, want the boundary's own instant 1000", got.StartedAtMs)
	}
}
