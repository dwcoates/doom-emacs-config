package server

import (
	"testing"

	"claude-repld/internal/errclass"
	"claude-repld/internal/registry"
)

// putDeathRecord writes one session record for a workspace.
func putDeathRecord(t *testing.T, reg *registry.Registry, rec registry.Record) {
	t.Helper()
	if err := reg.Put(rec); err != nil {
		t.Fatalf("Put %s: %v", rec.SessionID, err)
	}
}

func TestDeletedSessionReportsADeliberatelyDeletedSession(t *testing.T) {
	// Arrange — the workspace's only session was destroyed by the user.
	reg := openTestRegistry(t)
	putDeathRecord(t, reg, registry.Record{
		SessionID: "s_1", CWD: "/w", CreatedAt: "2026-07-25T10:00:00Z",
		Terminal: true, DeathReason: errclass.DeathReasonDeleted,
	})

	// Act.
	id, deleted, err := RegistrySessionDeaths{Reg: reg}.DeletedSession("/w")

	// Assert.
	if err != nil {
		t.Fatalf("DeletedSession() error = %v", err)
	}
	if !deleted || id != "s_1" {
		t.Fatalf("DeletedSession() = (%q, %v), want the deleted session named", id, deleted)
	}
}

func TestDeletedSessionReportsNothingForALiveSession(t *testing.T) {
	// Arrange — an ordinary, non-terminal session.
	reg := openTestRegistry(t)
	putDeathRecord(t, reg, registry.Record{SessionID: "s_1", CWD: "/w", CreatedAt: "2026-07-25T10:00:00Z"})

	// Act.
	_, deleted, err := RegistrySessionDeaths{Reg: reg}.DeletedSession("/w")

	// Assert.
	if err != nil {
		t.Fatalf("DeletedSession() error = %v", err)
	}
	if deleted {
		t.Fatal("DeletedSession() reported a deletion for a live session")
	}
}

func TestDeletedSessionReportsNothingForAnotherDeathReason(t *testing.T) {
	// Arrange — terminal, but because the shim died rather than by deletion.
	reg := openTestRegistry(t)
	putDeathRecord(t, reg, registry.Record{
		SessionID: "s_1", CWD: "/w", CreatedAt: "2026-07-25T10:00:00Z",
		Terminal: true, DeathReason: errclass.DeathReasonShimDied,
	})

	// Act.
	_, deleted, err := RegistrySessionDeaths{Reg: reg}.DeletedSession("/w")

	// Assert — only a deliberate deletion is this port's subject.
	if err != nil {
		t.Fatalf("DeletedSession() error = %v", err)
	}
	if deleted {
		t.Fatal("DeletedSession() reported a deletion for a session that died some other way")
	}
}

func TestDeletedSessionReadsTheNewestRecordOnly(t *testing.T) {
	// Arrange — a deleted session followed by a NEWER one the user re-created.
	reg := openTestRegistry(t)
	putDeathRecord(t, reg, registry.Record{
		SessionID: "s_old", CWD: "/w", CreatedAt: "2026-07-25T10:00:00Z",
		Terminal: true, DeathReason: errclass.DeathReasonDeleted,
	})
	putDeathRecord(t, reg, registry.Record{SessionID: "s_new", CWD: "/w", CreatedAt: "2026-07-25T11:00:00Z"})

	// Act.
	_, deleted, err := RegistrySessionDeaths{Reg: reg}.DeletedSession("/w")

	// Assert — one deletion must not make a workspace permanently unmergeable.
	if err != nil {
		t.Fatalf("DeletedSession() error = %v", err)
	}
	if deleted {
		t.Fatal("DeletedSession() reported a deletion although a newer session exists")
	}
}

func TestDeletedSessionReportsNothingForAWorkspaceWithNoRecord(t *testing.T) {
	// Arrange — a workspace that never had a session at all.
	reg := openTestRegistry(t)

	// Act.
	_, deleted, err := RegistrySessionDeaths{Reg: reg}.DeletedSession("/never-opened")

	// Assert — no record is not a deletion; the bring-up creates one.
	if err != nil {
		t.Fatalf("DeletedSession() error = %v", err)
	}
	if deleted {
		t.Fatal("DeletedSession() reported a deletion for a workspace with no record")
	}
}

func TestDeletedSessionRefusesAnUnwiredRegistry(t *testing.T) {
	// Arrange, Act.
	_, deleted, err := RegistrySessionDeaths{}.DeletedSession("/w")

	// Assert — "there is no registry" must never resolve to the benign answer.
	if err == nil {
		t.Fatal("DeletedSession() error = nil, want a refusal naming the missing registry")
	}
	if deleted {
		t.Fatal("DeletedSession() reported a deletion alongside its error")
	}
}

func TestDeletedSessionRefusesAnEmptyWorkspace(t *testing.T) {
	// Arrange.
	reg := openTestRegistry(t)

	// Act.
	_, _, err := RegistrySessionDeaths{Reg: reg}.DeletedSession("")

	// Assert.
	if err == nil {
		t.Fatal("DeletedSession(\"\") error = nil, want error")
	}
}
