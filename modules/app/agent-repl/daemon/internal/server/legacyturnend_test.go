package server

import (
	"testing"

	"claude-repld/internal/registry"
)

// ---------------------------------------------------------------------------
// LegacyTurnEndStamps writes a session's last-turn-end from the WORKSPACE's
// dated state history, for a session whose turn ends the daemon never saw. The
// instant is real and the keep-alive policy is right to measure from it; what
// it is not is evidence that a turn ever ran under this record's conversation.
// These tests pin the mark that keeps the two questions apart.
// ---------------------------------------------------------------------------

// datedActivity is a workspace whose state history last moved at atMs.
type datedActivity struct {
	atMs  int64
	dated bool
	err   error
}

func (a datedActivity) LastActivityMs(string) (int64, bool, error) { return a.atMs, a.dated, a.err }

func TestLegacyStampMarksTheValueItBackfilled(t *testing.T) {
	// Arrange — a session with no observed turn end, in a workspace whose state
	// history has a dated instant. This is every freshly created workspace a
	// few seconds after bring-up.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w", ConfigDir: "/cfg", ClaudeSessionID: "uuid-1"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	stamps := LegacyTurnEndStamps{Reg: reg, Activity: datedActivity{atMs: 1786317445505, dated: true}, Logf: t.Logf}

	// Act
	atMs, ok := stamps.StampLegacyTurnEnd("s1", "/w")

	// Assert — the instant lands for the keep-alive policy, MARKED so the
	// resume ladder does not read it as a turn.
	if !ok || atMs != 1786317445505 {
		t.Fatalf("StampLegacyTurnEnd = (%d, %v), want the dated instant", atMs, ok)
	}
	rec, _ := reg.Get("s1")
	if rec.LastTurnEndMs != 1786317445505 || !rec.LastTurnEndBackfilled {
		t.Fatalf("record = %+v, want the instant marked as backfilled", rec)
	}
}

func TestLegacyStampLeavesAnObservedTurnAlone(t *testing.T) {
	// Arrange — a record that already has a real turn end.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w", ConfigDir: "/cfg", LastTurnEndMs: 1786117000000}); err != nil {
		t.Fatalf("put: %v", err)
	}
	stamps := LegacyTurnEndStamps{Reg: reg, Activity: datedActivity{atMs: 1786317445505, dated: true}, Logf: t.Logf}

	// Act
	stamps.StampLegacyTurnEnd("s1", "/w")

	// Assert — untouched, and still not marked as backfilled.
	rec, _ := reg.Get("s1")
	if rec.LastTurnEndMs != 1786117000000 || rec.LastTurnEndBackfilled {
		t.Fatalf("record = %+v, want the observed turn end left alone and unmarked", rec)
	}
}

func TestLegacyStampReportsNothingWithNoDatedFact(t *testing.T) {
	// Arrange — a workspace whose state history has no dated instant. The stamp
	// never guesses one, and never uses now().
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w", ConfigDir: "/cfg"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	stamps := LegacyTurnEndStamps{Reg: reg, Activity: datedActivity{}, Logf: t.Logf}

	// Act
	atMs, ok := stamps.StampLegacyTurnEnd("s1", "/w")

	// Assert
	if ok || atMs != 0 {
		t.Fatalf("StampLegacyTurnEnd = (%d, %v), want no stamp", atMs, ok)
	}
	if rec, _ := reg.Get("s1"); rec.LastTurnEndMs != 0 || rec.LastTurnEndBackfilled {
		t.Fatalf("record = %+v, want it untouched", rec)
	}
}

func TestBackfilledMarkSurvivesAReopen(t *testing.T) {
	// Arrange — the mark must be DURABLE: the wedge it prevents is one a daemon
	// restart walks straight back into.
	path := t.TempDir() + "/sessions.db"
	logf := func(string, ...any) {}
	reg := registry.Open(path, logf)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w", ConfigDir: "/cfg", LastTurnEndMs: 42, LastTurnEndBackfilled: true}); err != nil {
		t.Fatalf("put: %v", err)
	}

	// Act
	reopened := registry.Open(path, logf)

	// Assert
	rec, ok := reopened.Get("s1")
	if !ok || !rec.LastTurnEndBackfilled {
		t.Fatalf("reopened record = %+v (ok=%v), want the backfill mark persisted", rec, ok)
	}
}

func TestAnObservedTurnEndClearsTheBackfillMark(t *testing.T) {
	// Arrange — a record whose instant was backfilled, which then runs a real
	// turn. The mark must come off with the value it described, or the record
	// would go on denying a conversation it has.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{
		SessionID: "s1", CWD: "/w", ConfigDir: "/cfg",
		LastTurnEndMs: 1786117000000, LastTurnEndBackfilled: true,
	}); err != nil {
		t.Fatalf("put: %v", err)
	}
	r := &RegistryRegistrar{Reg: reg, Logf: t.Logf}

	// Act
	r.TurnEndObserved("s1", 1786317000000)

	// Assert
	rec, _ := reg.Get("s1")
	if rec.LastTurnEndMs != 1786317000000 || rec.LastTurnEndBackfilled {
		t.Fatalf("record = %+v, want the observed instant with the mark cleared", rec)
	}
	if !resumeTargetCarriesAConversation(rec) {
		t.Fatal("a record that has run a real turn is not reporting a conversation")
	}
}

func TestAStaleTurnEndLeavesTheBackfillMarkAlone(t *testing.T) {
	// Arrange — a late-arriving end for an OLDER turn never rewinds the clock,
	// so it must not clear a mark describing the newer value either.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{
		SessionID: "s1", CWD: "/w", ConfigDir: "/cfg",
		LastTurnEndMs: 1786317000000, LastTurnEndBackfilled: true,
	}); err != nil {
		t.Fatalf("put: %v", err)
	}
	r := &RegistryRegistrar{Reg: reg, Logf: t.Logf}

	// Act
	r.TurnEndObserved("s1", 1786117000000)

	// Assert
	rec, _ := reg.Get("s1")
	if rec.LastTurnEndMs != 1786317000000 || !rec.LastTurnEndBackfilled {
		t.Fatalf("record = %+v, want the newer backfilled value untouched", rec)
	}
}
