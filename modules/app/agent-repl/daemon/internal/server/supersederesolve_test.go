package server

import (
	"testing"

	"claude-repld/internal/errclass"
	"claude-repld/internal/registry"
)

// ---------------------------------------------------------------------------
// A SUPERSEDE IS A WINDOW, NOT AN EVENT.
//
// The card says "a new Claude session was started for this workspace, so this
// session was stopped". It is true from the supersede until the successor is
// genuinely up, and false afterwards — but the item is re-derived from the
// registry's death_reason on every push, so with nothing durable to close it
// the card reopened on every snapshot of every boot forever.
//
// These tests pin the live close: the successor reaching operational, and
// nothing weaker. The boot half is in supersedereconcile_test.go.
// ---------------------------------------------------------------------------

// superseded is a terminal record standing down for a newer session on ws.
func superseded(id, ws string) registry.Record {
	return registry.Record{SessionID: id, CWD: ws, Terminal: true, DeathReason: supersedeReason}
}

// seedRecords puts every record, failing the test rather than the assertion.
func seedRecords(t *testing.T, reg *registry.Registry, recs ...registry.Record) {
	t.Helper()
	for _, rec := range recs {
		if err := reg.Put(rec); err != nil {
			t.Fatalf("put %s: %v", rec.SessionID, err)
		}
	}
}

func TestSupersedeStaysOpenUntilTheSuccessorIsOperational(t *testing.T) {
	// Arrange — the supersede has JUST happened; the successor exists as a
	// record but has not reached the bring-up gate.
	reg := openTestRegistry(t)
	seedRecords(t, reg,
		superseded("s_old", "/w"),
		registry.Record{SessionID: "s_new", CWD: "/w"},
	)

	// Act — nothing but the create has run.

	// Assert — the card the frontend renders is OPEN, so a handover the user
	// just triggered is still reported.
	rec, _ := reg.Get("s_old")
	item := errclass.Death(t.Logf, rec.SessionID, rec.DeathReason, rec.DeathResolvedAtMs)
	if errclass.ResolvedAtMs(item) != 0 {
		t.Fatalf("resolved_at_ms = %d before the successor was up; a fresh supersede must still show its card", errclass.ResolvedAtMs(item))
	}
	if errclass.TypeName(item) != string(errclass.TypeSessionSuperseded) {
		t.Fatalf("error_type = %q, want %s", errclass.TypeName(item), errclass.TypeSessionSuperseded)
	}
}

func TestSupersedeResolvesWhenTheSuccessorReachesOperational(t *testing.T) {
	// Arrange.
	reg := openTestRegistry(t)
	seedRecords(t, reg,
		superseded("s_old", "/w"),
		registry.Record{SessionID: "s_new", CWD: "/w"},
	)
	r := &RegistryRegistrar{Reg: reg, Now: func() int64 { return 1700 }}

	// Act — the bring-up gate closes for the successor.
	r.SessionOperational("/w", "s_new")

	// Assert.
	rec, _ := reg.Get("s_old")
	if rec.DeathResolvedAtMs != 1700 {
		t.Fatalf("death_resolved_at_ms = %d, want 1700", rec.DeathResolvedAtMs)
	}
}

func TestResolvedSupersedeKeepsItsHistory(t *testing.T) {
	// Arrange — resolution must CLOSE the card, never erase the account of
	// what happened to the session.
	reg := openTestRegistry(t)
	seedRecords(t, reg,
		superseded("s_old", "/w"),
		registry.Record{SessionID: "s_new", CWD: "/w"},
	)
	r := &RegistryRegistrar{Reg: reg, Now: func() int64 { return 1700 }}

	// Act.
	r.SessionOperational("/w", "s_new")

	// Assert.
	rec, ok := reg.Get("s_old")
	if !ok {
		t.Fatal("the superseded record was DELETED; resolution is not deletion")
	}
	if !rec.Terminal || rec.DeathReason != supersedeReason {
		t.Fatalf("record = {terminal:%v reason:%q}, want the supersede still recorded", rec.Terminal, rec.DeathReason)
	}
}

func TestResolvedSupersedeRepushesItsSessionView(t *testing.T) {
	// Arrange — a frontend already holding the OPEN card only learns of the
	// close if the settled view is pushed to it.
	reg := openTestRegistry(t)
	seedRecords(t, reg,
		superseded("s_old", "/w"),
		registry.Record{SessionID: "s_new", CWD: "/w"},
	)
	var pushed []string
	r := &RegistryRegistrar{Reg: reg, Now: func() int64 { return 1700 },
		PushView: func(id string) { pushed = append(pushed, id) }}

	// Act.
	r.SessionOperational("/w", "s_new")

	// Assert.
	if len(pushed) != 1 || pushed[0] != "s_old" {
		t.Fatalf("pushed = %v, want exactly [s_old]", pushed)
	}
}

func TestResolvedSupersedeSettlesTheSameCardItOpened(t *testing.T) {
	// Arrange — the settled item must carry the OPEN item's uuid, or a
	// frontend keyed on item_uuid renders a second card beside the first.
	reg := openTestRegistry(t)
	seedRecords(t, reg,
		superseded("s_old", "/w"),
		registry.Record{SessionID: "s_new", CWD: "/w"},
	)
	open, _ := reg.Get("s_old")
	openUUID := errclass.DeathItemUUID(open.SessionID)
	r := &RegistryRegistrar{Reg: reg, Now: func() int64 { return 1700 }}

	// Act.
	r.SessionOperational("/w", "s_new")

	// Assert — the card's address is its ENVELOPE's now, and both edges derive
	// it from the same session, so a settled re-push replaces the open card
	// rather than landing beside it.
	settled, _ := reg.Get("s_old")
	settledUUID := errclass.DeathItemUUID(settled.SessionID)
	if settledUUID == "" || settledUUID != openUUID {
		t.Fatalf("settled uuid = %q, open uuid = %q; they must be the same card",
			settledUUID, openUUID)
	}
	if !errclass.IsResolved(errclass.Death(t.Logf, settled.SessionID, settled.DeathReason, settled.DeathResolvedAtMs)) {
		t.Fatal("the operational successor left the superseded card open")
	}
}

func TestOperationalLeavesAnotherWorkspacesSupersedeOpen(t *testing.T) {
	// Arrange — one workspace's handover says nothing about another's.
	reg := openTestRegistry(t)
	seedRecords(t, reg,
		superseded("s_other", "/other"),
		registry.Record{SessionID: "s_new", CWD: "/w"},
	)
	r := &RegistryRegistrar{Reg: reg, Now: func() int64 { return 1700 }}

	// Act.
	r.SessionOperational("/w", "s_new")

	// Assert.
	rec, _ := reg.Get("s_other")
	if rec.DeathResolvedAtMs != 0 {
		t.Fatalf("death_resolved_at_ms = %d on an unrelated workspace, want 0", rec.DeathResolvedAtMs)
	}
}

func TestOperationalLeavesOtherDeathReasonsOpen(t *testing.T) {
	// Arrange — only the supersede is window-shaped. A deleted conversation
	// stays deleted; a dead shim stayed dead.
	tests := []struct {
		name   string
		reason string
	}{
		{name: "deleted", reason: errclass.DeathReasonDeleted},
		{name: "shim died", reason: errclass.DeathReasonShimDied},
		{name: "unclassified legacy reason", reason: "some ancient reason"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			reg := openTestRegistry(t)
			seedRecords(t, reg,
				registry.Record{SessionID: "s_old", CWD: "/w", Terminal: true, DeathReason: tc.reason},
				registry.Record{SessionID: "s_new", CWD: "/w"},
			)
			r := &RegistryRegistrar{Reg: reg, Now: func() int64 { return 1700 }}

			// Act.
			r.SessionOperational("/w", "s_new")

			// Assert.
			rec, _ := reg.Get("s_old")
			if rec.DeathResolvedAtMs != 0 {
				t.Fatalf("death_resolved_at_ms = %d for reason %q, want 0", rec.DeathResolvedAtMs, tc.reason)
			}
		})
	}
}

func TestOperationalDoesNotRestampAnAlreadyResolvedSupersede(t *testing.T) {
	// Arrange — the shim reattaches repeatedly over a session's life, so the
	// operational edge fires more than once. The FIRST close is the true one.
	reg := openTestRegistry(t)
	seedRecords(t, reg,
		superseded("s_old", "/w"),
		registry.Record{SessionID: "s_new", CWD: "/w"},
	)
	clock := int64(1700)
	r := &RegistryRegistrar{Reg: reg, Now: func() int64 { return clock }}
	r.SessionOperational("/w", "s_new")

	// Act — a later reattach.
	clock = 9900
	r.SessionOperational("/w", "s_new")

	// Assert.
	rec, _ := reg.Get("s_old")
	if rec.DeathResolvedAtMs != 1700 {
		t.Fatalf("death_resolved_at_ms = %d, want the first close at 1700", rec.DeathResolvedAtMs)
	}
}
