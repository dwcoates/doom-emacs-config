package server

import (
	"testing"

	"claude-repld/internal/errclass"
	"claude-repld/internal/registry"
)

// ---------------------------------------------------------------------------
// A SUPERSEDE WITH A SUCCESSOR STILL COMING UP IS NOT NEWS.
//
// supersederesolve.go closes the window on the successor's OPERATIONAL edge,
// which is the only honest close — but the gap between the supersede (inside
// Create) and that edge (the shim's handshake) is wide enough for a connect
// snapshot to fall into, and every snapshot taken in it handed the user an open
// "a new Claude session was started for this workspace" card about a handover
// that was proceeding normally.
//
// These tests pin the presentation rule that closes the gap, and pin the two
// things it must NOT do: touch the record, or hide a supersede that genuinely
// has nobody succeeding it.
// ---------------------------------------------------------------------------

func TestDeathForViewWithholdsSupersedeWhileASuccessorClaimsTheWorkspace(t *testing.T) {
	// Arrange — the churn shape: the create stood the predecessor down and
	// minted its own record, but the successor's shim has not handshaked, so
	// nothing has stamped the predecessor's resolution.
	reg := openTestRegistry(t)
	seedRecords(t, reg,
		superseded("s_old", "/w"),
		registry.Record{SessionID: "s_new", CWD: "/w"},
	)
	rec, _ := reg.Get("s_old")

	// Act — a snapshot or push shapes the retired record's view right now.
	item := deathForView(t.Logf, reg, rec)

	// Assert — no death rides the wire, so no card can be raised for it.
	if item != nil {
		t.Fatalf("death item = %+v while a successor still claims the workspace; a handover in flight must not present as an open failure", item)
	}
}

func TestDeathForViewPresentsSupersedeWithNoSuccessor(t *testing.T) {
	// Arrange — the genuine orphan: the predecessor was stood down and nothing
	// on its workspace ever took the claim.
	reg := openTestRegistry(t)
	seedRecords(t, reg, superseded("s_old", "/w"))
	rec, _ := reg.Get("s_old")

	// Act.
	item := deathForView(t.Logf, reg, rec)

	// Assert — the failure is reported, unchanged.
	if item == nil {
		t.Fatal("death item = nil for a supersede with no successor; an orphaned handover is exactly the case the card exists for")
	}
	if errclass.TypeName(item) != string(errclass.TypeSessionSuperseded) {
		t.Fatalf("error_type = %q, want %s", errclass.TypeName(item), errclass.TypeSessionSuperseded)
	}
}

func TestDeathForViewPresentsSupersedeAgainOnceTheSuccessorGoesTerminal(t *testing.T) {
	// Arrange — the successor was minted, then died or was itself stood down
	// before reaching operational. The claim is gone; the predecessor's window
	// is still open and now nothing is going to close it.
	reg := openTestRegistry(t)
	seedRecords(t, reg,
		superseded("s_old", "/w"),
		registry.Record{SessionID: "s_new", CWD: "/w", Terminal: true, DeathReason: errclass.DeathReasonShimDied},
	)
	rec, _ := reg.Get("s_old")

	// Act.
	item := deathForView(t.Logf, reg, rec)

	// Assert — withholding is scoped to a LIVE claim, so it lifts the moment
	// the claim does.
	if item == nil {
		t.Fatal("death item = nil after the successor went terminal; withholding must last exactly as long as the claim")
	}
}

func TestDeathForViewLeavesTheRecordUnresolved(t *testing.T) {
	// Arrange — withholding is a presentation rule, not a resolution.
	reg := openTestRegistry(t)
	seedRecords(t, reg,
		superseded("s_old", "/w"),
		registry.Record{SessionID: "s_new", CWD: "/w"},
	)
	rec, _ := reg.Get("s_old")

	// Act.
	deathForView(t.Logf, reg, rec)

	// Assert — the durable claim "this window is open" is untouched, so the
	// successor's operational edge and the boot reconciliation remain the only
	// writers of its close.
	after, ok := reg.Get("s_old")
	if !ok {
		t.Fatal("record s_old vanished")
	}
	if after.DeathResolvedAtMs != 0 {
		t.Fatalf("death_resolved_at_ms = %d after shaping a view; presentation must never stamp the record", after.DeathResolvedAtMs)
	}
}

func TestDeathForViewPresentsAlreadyResolvedSupersede(t *testing.T) {
	// Arrange — the settled re-push: the successor reached operational and
	// stamped the predecessor, and the frontend holding the open card is owed
	// the closed one under the same item uuid.
	reg := openTestRegistry(t)
	settled := superseded("s_old", "/w")
	settled.DeathResolvedAtMs = 1234
	seedRecords(t, reg,
		settled,
		registry.Record{SessionID: "s_new", CWD: "/w"},
	)
	rec, _ := reg.Get("s_old")

	// Act.
	item := deathForView(t.Logf, reg, rec)

	// Assert — a RESOLVED supersede is not withheld: withholding it would
	// strand the open card a frontend already rendered, with nothing left to
	// settle it.
	if item == nil {
		t.Fatal("death item = nil for an already-resolved supersede; the settling re-push must still carry the item that closes the card")
	}
	if errclass.ResolvedAtMs(item) != 1234 {
		t.Fatalf("resolved_at_ms = %d, want 1234", errclass.ResolvedAtMs(item))
	}
}

func TestDeathForViewPresentsNonSupersedeDeathsDespiteASuccessor(t *testing.T) {
	// Arrange — an EVENT-shaped death. A later session on the same workspace
	// says nothing about a session that was deleted.
	reg := openTestRegistry(t)
	seedRecords(t, reg,
		registry.Record{SessionID: "s_old", CWD: "/w", Terminal: true, DeathReason: errclass.DeathReasonDeleted},
		registry.Record{SessionID: "s_new", CWD: "/w"},
	)
	rec, _ := reg.Get("s_old")

	// Act.
	item := deathForView(t.Logf, reg, rec)

	// Assert.
	if item == nil {
		t.Fatal("death item = nil for a deleted session; only the superseded window is withheld")
	}
	if errclass.TypeName(item) != string(errclass.TypeSessionDeleted) {
		t.Fatalf("error_type = %q, want %s", errclass.TypeName(item), errclass.TypeSessionDeleted)
	}
}

func TestDeathForViewWithNilRegistryWithholdsNothing(t *testing.T) {
	// Arrange — a caller shaping a record in isolation cannot know whether a
	// successor exists, and an unknown must not silence a failure.
	rec := superseded("s_old", "/w")

	// Act.
	item := deathForView(t.Logf, nil, rec)

	// Assert.
	if item == nil {
		t.Fatal("death item = nil with no registry context; an unknowable claim must default to reporting, never to hiding")
	}
}

func TestSupersedeClaimedIgnoresASuccessorOnAnotherWorkspace(t *testing.T) {
	// Arrange — a live session elsewhere is not a claim on this workspace.
	reg := openTestRegistry(t)
	seedRecords(t, reg,
		superseded("s_old", "/w"),
		registry.Record{SessionID: "s_other", CWD: "/elsewhere"},
	)

	// Act.
	claimed := supersedeClaimed(reg, "/w", "s_old")

	// Assert.
	if claimed {
		t.Fatal("supersedeClaimed = true from a record on a different cwd; the claim is workspace-scoped")
	}
}

func TestSupersedeClaimedIgnoresTheSupersededRecordItself(t *testing.T) {
	// Arrange — a record must never count as its own successor. Only the
	// terminal flag distinguishes them otherwise, and a future path that marks
	// a record terminal late would make identity the load-bearing test.
	reg := openTestRegistry(t)
	seedRecords(t, reg, registry.Record{SessionID: "s_old", CWD: "/w"})

	// Act.
	claimed := supersedeClaimed(reg, "/w", "s_old")

	// Assert.
	if claimed {
		t.Fatal("supersedeClaimed = true counting the superseded session as its own successor")
	}
}

func TestSupersedeClaimedIsFalseForAWorkspacelessRecord(t *testing.T) {
	// Arrange — the degenerate transcript-only stand-down has no cwd on which
	// a successor could be found.
	reg := openTestRegistry(t)
	seedRecords(t, reg, registry.Record{SessionID: "s_new", CWD: "/w"})

	// Act.
	claimed := supersedeClaimed(reg, "", "s_old")

	// Assert.
	if claimed {
		t.Fatal("supersedeClaimed = true for an empty workspace; a record claiming no cwd can be succeeded by nobody")
	}
}

func TestRegistrySessionsWithholdsSupersedeDuringChurn(t *testing.T) {
	// Arrange — the reported symptom end to end: a frontend connects while a
	// supersede's successor is still coming up, and the connect snapshot is
	// what it receives.
	reg := openTestRegistry(t)
	seedRecords(t, reg,
		superseded("s_old", "/w"),
		registry.Record{SessionID: "s_new", CWD: "/w"},
	)

	// Act.
	views := RegistrySessions{Reg: reg, Logf: t.Logf}.SessionViews()

	// Assert — the retired session is still listed (the roster needs it), but
	// carries nothing for a frontend to raise a card from.
	var retired int
	for _, v := range views {
		if v.GetSessionId() != "s_old" {
			continue
		}
		retired++
		if v.GetDeath() != nil {
			t.Fatalf("connect snapshot carried an open death for s_old: %+v", v.GetDeath())
		}
	}
	if retired != 1 {
		t.Fatalf("snapshot listed s_old %d time(s), want 1; the retired record must stay in the roster", retired)
	}
}

func TestRegistrySessionsPresentsAnOrphanedSupersedeInTheSnapshot(t *testing.T) {
	// Arrange — same door, genuine orphan.
	reg := openTestRegistry(t)
	seedRecords(t, reg, superseded("s_old", "/w"))

	// Act.
	views := RegistrySessions{Reg: reg, Logf: t.Logf}.SessionViews()

	// Assert.
	for _, v := range views {
		if v.GetSessionId() != "s_old" {
			continue
		}
		if v.GetDeath() == nil {
			t.Fatal("connect snapshot dropped the death of an orphaned supersede; nothing else would ever report it")
		}
		return
	}
	t.Fatal("snapshot did not list s_old at all")
}
