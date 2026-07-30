package sessioncontroller

import (
	"testing"
)

// ---------------------------------------------------------------------------
// ADOPT EAGERLY, controller side.
//
// This file used to assert the opposite: that a uuid announced before any turn
// was HELD out of the registry, and that a turn-less session left no pointer at
// all. That gate moved to the point of USE — server.ConversationResolver stats
// the transcript when it resolves a resume target — so the session controller now writes
// the uuid through the moment it learns it.
//
// The hold was not free. While the registry withheld a uuid the shim and the
// webapp both already knew, every client log failed identity validation against
// the registry and nacked; a workspace opened and never prompted produced tens
// of thousands of rejected records.
// ---------------------------------------------------------------------------

func TestTurnlessSessionStillAdoptsItsVendorPointer(t *testing.T) {
	// Arrange.
	reg := &fakeRegistrar{}
	m := newRegistrarManager(t, reg)

	// Act — the shim announces its uuid; no turn ever runs.
	m.persistVendorSessionID("s1", "cli-uuid-1")

	// Assert — the record carries the uuid regardless. Whether the transcript
	// exists is the resolver's question, asked at resume.
	if got := reg.writeThroughs(); len(got) != 1 || got[0] != "s1=cli-uuid-1" {
		t.Fatalf("writes = %v, want [s1=cli-uuid-1] even with no turn behind it", got)
	}
}

func TestARepeatedAnnouncementIsWrittenOnlyOnce(t *testing.T) {
	// Arrange — every persistent store event re-offers the envelope's uuid, so
	// a session controller that re-called the registry per event would spray writes.
	reg := &fakeRegistrar{}
	m := newRegistrarManager(t, reg)

	// Act.
	m.persistVendorSessionID("s1", "cli-uuid-1")
	m.persistVendorSessionID("s1", "cli-uuid-1")
	m.persistVendorSessionID("s1", "cli-uuid-1")

	// Assert.
	if got := reg.writeThroughs(); len(got) != 1 {
		t.Fatalf("writes = %v, want exactly one", got)
	}
}

func TestANewUUIDForTheSameSessionIsWrittenThrough(t *testing.T) {
	// Arrange — a rotation (a /clear or a compact) retires one uuid for
	// another, and the dedupe must not swallow the second one.
	reg := &fakeRegistrar{}
	m := newRegistrarManager(t, reg)
	m.persistVendorSessionID("s1", "cli-uuid-1")

	// Act.
	m.persistVendorSessionID("s1", "cli-uuid-2")

	// Assert.
	got := reg.writeThroughs()
	if len(got) != 2 || got[1] != "s1=cli-uuid-2" {
		t.Fatalf("writes = %v, want the rotation written through", got)
	}
}

func TestAnEmptyAnnouncementIsNotWritten(t *testing.T) {
	// Arrange — a fresh shim that has not learned its uuid announces "".
	reg := &fakeRegistrar{}
	m := newRegistrarManager(t, reg)

	// Act.
	m.persistVendorSessionID("s1", "")

	// Assert — an empty uuid would erase a real one.
	if got := reg.writeThroughs(); len(got) != 0 {
		t.Fatalf("writes = %v, want none for an empty announcement", got)
	}
}
