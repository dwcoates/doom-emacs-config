package sessiondrv

import (
	"testing"
)

// ---------------------------------------------------------------------------
// ADOPT LATE, driver side: the uuid a shim announces before any turn is HELD,
// not dropped, and offered again the moment a turn proves the vendor wrote the
// conversation. A session that dies turn-less leaves no pointer at all.
// ---------------------------------------------------------------------------

func TestTurnlessSessionLeavesNoVendorPointer(t *testing.T) {
	// Arrange.
	reg := &fakeRegistrar{}
	m := newRegistrarManager(t, reg)

	// Act — the shim announces its uuid; no turn ever runs.
	m.persistVendorSessionID("s1", "cli-uuid-1")

	// Assert.
	if got := reg.writeThroughs(); len(got) != 0 {
		t.Fatalf("writes = %v, want none for a session that never ran a turn", got)
	}
}

func TestFirstTurnAdoptsTheHeldVendorSessionID(t *testing.T) {
	// Arrange — the announcement arrives first, as it always does.
	reg := &fakeRegistrar{}
	m := newRegistrarManager(t, reg)
	m.persistVendorSessionID("s1", "cli-uuid-1")

	// Act.
	m.noteTurnEvidence("s1")

	// Assert.
	if got := reg.writeThroughs(); len(got) != 1 || got[0] != "s1=cli-uuid-1" {
		t.Fatalf("writes = %v, want [s1=cli-uuid-1]", got)
	}
}

func TestAHeldAnnouncementIsOfferedOnlyOnce(t *testing.T) {
	// Arrange — every persistent store event re-offers the envelope's uuid, so
	// a hold that re-called the registry per event would spray.
	reg := &fakeRegistrar{}
	m := newRegistrarManager(t, reg)

	// Act.
	m.persistVendorSessionID("s1", "cli-uuid-1")
	m.persistVendorSessionID("s1", "cli-uuid-1")
	m.persistVendorSessionID("s1", "cli-uuid-1")
	m.noteTurnEvidence("s1")

	// Assert.
	if got := reg.writeThroughs(); len(got) != 1 {
		t.Fatalf("writes = %v, want exactly one", got)
	}
}

func TestTurnEvidenceIsRecordedOncePerSession(t *testing.T) {
	// Arrange — every turn boundary reports evidence; only the first is news.
	reg := &fakeRegistrar{}
	m := newRegistrarManager(t, reg)
	m.noteTurnEvidence("s1")
	m.persistVendorSessionID("s1", "cli-uuid-1")

	// Act.
	m.noteTurnEvidence("s1")

	// Assert.
	if got := reg.writeThroughs(); len(got) != 1 || got[0] != "s1=cli-uuid-1" {
		t.Fatalf("writes = %v, want [s1=cli-uuid-1]", got)
	}
}
