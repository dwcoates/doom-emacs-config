package server

import (
	"path/filepath"
	"testing"

	"claude-repld/internal/registry"
)

// The durable REPLAY FLOOR mark: newest_clear_or_compact_seq, the store seq of
// the newest clear or compaction on a conversation. sessiondrv reads it to
// decide where a frontend replay may start; these cover the persistence and
// conversation-scoping half, which is the half that has to survive a restart.

func TestReplayFloorIsZeroBeforeAnyClearOrCompaction(t *testing.T) {
	// Arrange — a registered session that has seen neither.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w"}); err != nil {
		t.Fatalf("put: %v", err)
	}

	// Act
	got := NewRegistrySeqStore(reg, nil).NewestClearOrCompactSeq("s1")

	// Assert — 0 means "no floor", which replays from the client's own mark.
	if got != 0 {
		t.Fatalf("newest_clear_or_compact_seq = %d, want 0", got)
	}
}

func TestReplayFloorIsZeroForAnUnregisteredSession(t *testing.T) {
	// Arrange — nothing registered at all.
	reg := openTestRegistry(t)

	// Act
	got := NewRegistrySeqStore(reg, nil).NewestClearOrCompactSeq("nope")

	// Assert — an absent record must not be reported as a floor.
	if got != 0 {
		t.Fatalf("newest_clear_or_compact_seq = %d, want 0", got)
	}
}

func TestReplayFloorSurvivesADaemonRestart(t *testing.T) {
	// Arrange — the whole reason the mark is durable rather than in-memory: the
	// daemon re-Subscribes from last_seen_seq, so a clear observed before a
	// restart is never re-delivered and could never be re-derived.
	path := filepath.Join(t.TempDir(), "sessions.json")
	reg := registry.Open(path, func(string, ...any) {})
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	NewRegistrySeqStore(reg, nil).SetNewestClearOrCompactSeq("s1", 77)

	// Act — reopen from disk, as a restarted daemon does.
	reopened := NewRegistrySeqStore(registry.Open(path, func(string, ...any) {}), nil)

	// Assert
	if got := reopened.NewestClearOrCompactSeq("s1"); got != 77 {
		t.Fatalf("post-restart newest_clear_or_compact_seq = %d, want 77", got)
	}
}

func TestReplayFloorIsMonotonicAndAnOlderSeqNeverLowersIt(t *testing.T) {
	// Arrange — a re-delivery after a reattach can present a clear the daemon
	// already recorded. Accepting it would replay history the newer compaction
	// had already made irrelevant.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	store := NewRegistrySeqStore(reg, nil)
	store.SetNewestClearOrCompactSeq("s1", 90)

	// Act — an older mark arrives second.
	store.SetNewestClearOrCompactSeq("s1", 30)

	// Assert
	if got := store.NewestClearOrCompactSeq("s1"); got != 90 {
		t.Fatalf("newest_clear_or_compact_seq = %d, want 90 — an older seq must never lower the floor", got)
	}
}

func TestReplayFloorInheritsTheMarkFromTheSameConversation(t *testing.T) {
	// Arrange — every restart mints a fresh s_ id for the same conversation. A
	// floor filed only under the retired id would be 0 exactly when it matters.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s_old", CWD: "/w", ClaudeSessionID: "uuid-1"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	NewRegistrySeqStore(reg, nil).SetNewestClearOrCompactSeq("s_old", 512)
	if err := reg.Put(registry.Record{SessionID: "s_new", CWD: "/w", ClaudeSessionID: "uuid-1"}); err != nil {
		t.Fatalf("put: %v", err)
	}

	// Act
	got := NewRegistrySeqStore(reg, nil).NewestClearOrCompactSeq("s_new")

	// Assert
	if got != 512 {
		t.Fatalf("newest_clear_or_compact_seq = %d, want 512 — a fresh session id must inherit its conversation's floor", got)
	}
}

func TestReplayFloorIgnoresOtherConversations(t *testing.T) {
	// Arrange — another conversation's clear must not floor this one, or this
	// one would skip history it never discarded.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s_other", CWD: "/a", ClaudeSessionID: "uuid-other"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	if err := reg.Put(registry.Record{SessionID: "s_mine", CWD: "/b", ClaudeSessionID: "uuid-mine"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	store := NewRegistrySeqStore(reg, nil)
	store.SetNewestClearOrCompactSeq("s_other", 4000)

	// Act / Assert
	if got := store.NewestClearOrCompactSeq("s_mine"); got != 0 {
		t.Fatalf("newest_clear_or_compact_seq = %d, want 0", got)
	}
}

func TestReplayFloorUsesItsOwnMarkBeforeTheConversationIsKnown(t *testing.T) {
	// Arrange — a brand-new session has no vendor uuid until system:init, so
	// only its own record can apply.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s_fresh", CWD: "/w", NewestClearOrCompactSeq: 9}); err != nil {
		t.Fatalf("put: %v", err)
	}

	// Act / Assert
	if got := NewRegistrySeqStore(reg, nil).NewestClearOrCompactSeq("s_fresh"); got != 9 {
		t.Fatalf("newest_clear_or_compact_seq = %d, want 9", got)
	}
}

func TestReplayFloorWriteForAnUnregisteredSessionIsLoudNotSilent(t *testing.T) {
	// Arrange — a floor that could not be written must be reported, never
	// swallowed: the next resync would replay the discarded history.
	reg := openTestRegistry(t)
	var logged []string
	store := NewRegistrySeqStore(reg, func(f string, a ...any) { logged = append(logged, f) })

	// Act
	store.SetNewestClearOrCompactSeq("ghost", 5)

	// Assert
	if len(logged) != 1 {
		t.Fatalf("log lines = %d, want 1 — an unwritable replay floor must be loud", len(logged))
	}
}
