package server

import (
	"testing"

	"claude-repld/internal/errclass"
	"claude-repld/internal/registry"
)

// ---------------------------------------------------------------------------
// ConversationResolver — the daemon answering "which conversation belongs to
// this workspace?" from its own records, so no frontend has to remember.
//
// The resolver selects durable identity only. CreateSession owns transcript
// viability, so resolver tests remain filesystem-free and the create tests pin
// the shared hard-fail gate.
// ---------------------------------------------------------------------------

// resolverOver builds a resolver over the supplied durable registry.
func resolverOver(t *testing.T, reg *registry.Registry) *ConversationResolver {
	t.Helper()
	return &ConversationResolver{
		Reg:  reg,
		Logf: t.Logf,
	}
}

func TestResolveResumeFindsTheWorkspacesConversation(t *testing.T) {
	// Arrange
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{
		SessionID: "s1", CWD: "/w", ConfigDir: "/cfg",
		ClaudeSessionID: "uuid-1", CreatedAt: "2026-07-29T10:00:00Z",
	}); err != nil {
		t.Fatalf("put: %v", err)
	}

	// Act
	got, ok := resolverOver(t, reg).ResolveResume("/cfg", "/w")

	// Assert
	if !ok || got != "uuid-1" {
		t.Fatalf("ResolveResume = (%q, %v), want (uuid-1, true)", got, ok)
	}
}

func TestResolveResumeReportsNothingForABrandNewWorkspace(t *testing.T) {
	// Arrange — a workspace the registry has never seen.
	reg := openTestRegistry(t)

	// Act
	got, ok := resolverOver(t, reg).ResolveResume("/cfg", "/unknown")

	// Assert — "no conversation yet" is an honest answer, not a failure.
	if ok || got != "" {
		t.Fatalf("ResolveResume = (%q, %v), want (\"\", false)", got, ok)
	}
}

func TestResolveResumePrefersTheNewestConversation(t *testing.T) {
	// Arrange — two conversations at one location; the later one is current.
	reg := openTestRegistry(t)
	for _, rec := range []registry.Record{
		{SessionID: "s1", CWD: "/w", ConfigDir: "/cfg", ClaudeSessionID: "uuid-old", CreatedAt: "2026-07-01T10:00:00Z"},
		{SessionID: "s2", CWD: "/w", ConfigDir: "/cfg", ClaudeSessionID: "uuid-new", CreatedAt: "2026-07-29T10:00:00Z"},
	} {
		if err := reg.Put(rec); err != nil {
			t.Fatalf("put: %v", err)
		}
	}

	// Act
	got, _ := resolverOver(t, reg).ResolveResume("/cfg", "/w")

	// Assert
	if got != "uuid-new" {
		t.Fatalf("ResolveResume = %q, want uuid-new", got)
	}
}

func TestResolveResumeSelectsTheNewestRecordedConversationBeforeViabilityCheck(t *testing.T) {
	// Arrange — the newest record names a UUID whose transcript is unavailable.
	// The resolver must preserve this exact identity for CreateSession's shared
	// gate instead of silently choosing the older conversation.
	reg := openTestRegistry(t)
	for _, rec := range []registry.Record{
		{SessionID: "s1", CWD: "/w", ConfigDir: "/cfg", ClaudeSessionID: "uuid-real", CreatedAt: "2026-07-01T10:00:00Z"},
		{SessionID: "s2", CWD: "/w", ConfigDir: "/cfg", ClaudeSessionID: "uuid-phantom", CreatedAt: "2026-07-29T10:00:00Z"},
	} {
		if err := reg.Put(rec); err != nil {
			t.Fatalf("put: %v", err)
		}
	}

	// Act.
	got, ok := resolverOver(t, reg).ResolveResume("/cfg", "/w")

	// Assert
	if !ok || got != "uuid-phantom" {
		t.Fatalf("ResolveResume = (%q, %v), want (uuid-phantom, true)", got, ok)
	}
}

func TestResolveResumeReturnsRecordedConversationForSharedViabilityGate(t *testing.T) {
	// Arrange — the registry remembers a conversation whose transcript is gone.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{
		SessionID: "s1", CWD: "/w", ConfigDir: "/cfg",
		ClaudeSessionID: "uuid-gone", CreatedAt: "2026-07-29T10:00:00Z",
	}); err != nil {
		t.Fatalf("put: %v", err)
	}

	// Act.
	got, ok := resolverOver(t, reg).ResolveResume("/cfg", "/w")

	// Assert — CreateSession validates the returned UUID and hard-fails when
	// its transcript is absent. Returning false here would instead start fresh.
	if !ok || got != "uuid-gone" {
		t.Fatalf("ResolveResume = (%q, %v), want (uuid-gone, true)", got, ok)
	}
}

func TestResolveResumeExcludesADeletedConversation(t *testing.T) {
	// Arrange — the user deleted this conversation. Its transcript may well
	// still be on disk; reattaching to it anyway would undo their decision.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{
		SessionID: "s1", CWD: "/w", ConfigDir: "/cfg", ClaudeSessionID: "uuid-deleted",
		CreatedAt: "2026-07-29T10:00:00Z", Terminal: true,
		DeathReason: errclass.DeathReasonDeleted,
	}); err != nil {
		t.Fatalf("put: %v", err)
	}

	// Act
	_, ok := resolverOver(t, reg).ResolveResume("/cfg", "/w")

	// Assert
	if ok {
		t.Fatal("resolved a conversation the user deleted")
	}
}

func TestResolveResumeIncludesASupersededConversation(t *testing.T) {
	// Arrange — superseded means a newer session claimed the workspace, which
	// is a mechanical stand-down and not a decision about the conversation.
	// This is the exact shape every restore takes.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{
		SessionID: "s1", CWD: "/w", ConfigDir: "/cfg", ClaudeSessionID: "uuid-superseded",
		CreatedAt: "2026-07-29T10:00:00Z", Terminal: true,
		DeathReason: errclass.DeathReasonSuperseded,
	}); err != nil {
		t.Fatalf("put: %v", err)
	}

	// Act
	got, ok := resolverOver(t, reg).ResolveResume("/cfg", "/w")

	// Assert
	if !ok || got != "uuid-superseded" {
		t.Fatalf("ResolveResume = (%q, %v), want (uuid-superseded, true)", got, ok)
	}
}

func TestResolveResumeIgnoresAnotherWorkspacesConversation(t *testing.T) {
	// Arrange — resolution is keyed on the location, and crossing workspaces
	// would reattach one worktree's session to another's transcript.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{
		SessionID: "s1", CWD: "/other", ConfigDir: "/cfg",
		ClaudeSessionID: "uuid-other", CreatedAt: "2026-07-29T10:00:00Z",
	}); err != nil {
		t.Fatalf("put: %v", err)
	}

	// Act
	_, ok := resolverOver(t, reg).ResolveResume("/cfg", "/w")

	// Assert
	if ok {
		t.Fatal("resolved a conversation belonging to a different workspace")
	}
}

func TestResolveResumeIgnoresAnotherAccountsConversation(t *testing.T) {
	// Arrange — same worktree, different CLAUDE_CONFIG_DIR. The transcript for
	// one account is not readable as the other's, so config_dir is part of the
	// key rather than a detail.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{
		SessionID: "s1", CWD: "/w", ConfigDir: "/other-cfg",
		ClaudeSessionID: "uuid-other-account", CreatedAt: "2026-07-29T10:00:00Z",
	}); err != nil {
		t.Fatalf("put: %v", err)
	}

	// Act
	_, ok := resolverOver(t, reg).ResolveResume("/cfg", "/w")

	// Assert
	if ok {
		t.Fatal("resolved a conversation belonging to a different account")
	}
}

func TestResolveResumeIgnoresARecordWithNoVendorUUID(t *testing.T) {
	// Arrange — a session that died before the shim ever announced a uuid has
	// nothing to resume, and must not shadow one that does.
	reg := openTestRegistry(t)
	for _, rec := range []registry.Record{
		{SessionID: "s1", CWD: "/w", ConfigDir: "/cfg", ClaudeSessionID: "uuid-real", CreatedAt: "2026-07-01T10:00:00Z"},
		{SessionID: "s2", CWD: "/w", ConfigDir: "/cfg", ClaudeSessionID: "", CreatedAt: "2026-07-29T10:00:00Z"},
	} {
		if err := reg.Put(rec); err != nil {
			t.Fatalf("put: %v", err)
		}
	}

	// Act
	got, _ := resolverOver(t, reg).ResolveResume("/cfg", "/w")

	// Assert
	if got != "uuid-real" {
		t.Fatalf("ResolveResume = %q, want uuid-real", got)
	}
}

func TestResolveResumeReportsNothingWithoutARegistry(t *testing.T) {
	// Arrange — an unwired resolver must not claim to know anything.
	r := &ConversationResolver{Logf: t.Logf}

	// Act
	_, ok := r.ResolveResume("/cfg", "/w")

	// Assert
	if ok {
		t.Fatal("a resolver with no registry reported a resolution")
	}
}

// ---------------------------------------------------------------------------
// Deletion is judged per CONVERSATION, on its newest record.
//
// The regression these pin: the first version excluded a uuid the moment it
// saw ANY deleted record naming it, and Reg.All() iterates by session id
// rather than by time. A workspace accumulates a record per restore, so an old
// tombstone routinely sits beside newer live records for the same uuid — and
// the tombstone won whenever its session id happened to sort first.
//
// Two real workspaces lost their conversations to that, fell through to
// unrelated candidates, and one resumed a transcript another live process was
// writing; the shim replayed into it, saw a turn end for a turn it never saw
// begin, and died with a protocol error.
// ---------------------------------------------------------------------------

func TestResolveResumeRevivesAConversationDeletedThenRecreated(t *testing.T) {
	// Arrange — one uuid, an old delete and a newer supersede. The session ids
	// are chosen so the DELETED record sorts first by session id, which is the
	// order Reg.All() returns and the order that used to decide the outcome.
	reg := openTestRegistry(t)
	for _, rec := range []registry.Record{
		{SessionID: "s_a_deleted", CWD: "/w", ConfigDir: "/cfg", ClaudeSessionID: "uuid-1",
			CreatedAt: "2026-07-01T10:00:00Z", Terminal: true, DeathReason: errclass.DeathReasonDeleted},
		{SessionID: "s_z_live", CWD: "/w", ConfigDir: "/cfg", ClaudeSessionID: "uuid-1",
			CreatedAt: "2026-07-29T10:00:00Z", Terminal: true, DeathReason: errclass.DeathReasonSuperseded},
	} {
		if err := reg.Put(rec); err != nil {
			t.Fatalf("put: %v", err)
		}
	}

	// Act
	got, ok := resolverOver(t, reg).ResolveResume("/cfg", "/w")

	// Assert
	if !ok || got != "uuid-1" {
		t.Fatalf("ResolveResume = (%q, %v), want (uuid-1, true): the newest record revived it", got, ok)
	}
}

func TestResolveResumeIgnoresATombstoneThatSortsBeforeItsRevival(t *testing.T) {
	// Arrange — the exact shape that broke: the tombstone's session id sorts
	// FIRST, so an implementation keyed on iteration order excludes the uuid
	// and falls through to the decoy.
	reg := openTestRegistry(t)
	for _, rec := range []registry.Record{
		{SessionID: "s_aaa", CWD: "/w", ConfigDir: "/cfg", ClaudeSessionID: "uuid-real",
			CreatedAt: "2026-07-01T10:00:00Z", Terminal: true, DeathReason: errclass.DeathReasonDeleted},
		{SessionID: "s_zzz", CWD: "/w", ConfigDir: "/cfg", ClaudeSessionID: "uuid-real",
			CreatedAt: "2026-07-29T12:00:00Z", Terminal: true, DeathReason: errclass.DeathReasonSuperseded},
		// Older than the revival, and the wrong answer.
		{SessionID: "s_mmm", CWD: "/w", ConfigDir: "/cfg", ClaudeSessionID: "uuid-decoy",
			CreatedAt: "2026-07-15T10:00:00Z"},
	} {
		if err := reg.Put(rec); err != nil {
			t.Fatalf("put: %v", err)
		}
	}

	// Act
	got, _ := resolverOver(t, reg).ResolveResume("/cfg", "/w")

	// Assert
	if got != "uuid-real" {
		t.Fatalf("ResolveResume = %q, want uuid-real: a tombstone must not shadow a newer record for the same conversation", got)
	}
}

func TestResolveResumeStaysExcludedWhenTheDeleteIsTheNewestRecord(t *testing.T) {
	// Arrange — the other direction, and the reason the exclusion exists at
	// all: a conversation superseded and THEN deleted is still deleted.
	reg := openTestRegistry(t)
	for _, rec := range []registry.Record{
		{SessionID: "s_zzz_superseded", CWD: "/w", ConfigDir: "/cfg", ClaudeSessionID: "uuid-1",
			CreatedAt: "2026-07-01T10:00:00Z", Terminal: true, DeathReason: errclass.DeathReasonSuperseded},
		{SessionID: "s_aaa_deleted", CWD: "/w", ConfigDir: "/cfg", ClaudeSessionID: "uuid-1",
			CreatedAt: "2026-07-29T10:00:00Z", Terminal: true, DeathReason: errclass.DeathReasonDeleted},
	} {
		if err := reg.Put(rec); err != nil {
			t.Fatalf("put: %v", err)
		}
	}

	// Act
	_, ok := resolverOver(t, reg).ResolveResume("/cfg", "/w")

	// Assert
	if ok {
		t.Fatal("resolved a conversation whose newest record is a user delete")
	}
}

func TestResolveResumeRanksAConversationByItsNewestRecord(t *testing.T) {
	// Arrange — uuid-old was FIRST created earlier but restored most recently.
	// Ranking on the newest record per conversation is what makes "the one the
	// user was last talking to" the answer.
	reg := openTestRegistry(t)
	for _, rec := range []registry.Record{
		{SessionID: "s_1", CWD: "/w", ConfigDir: "/cfg", ClaudeSessionID: "uuid-old",
			CreatedAt: "2026-07-01T10:00:00Z", Terminal: true, DeathReason: errclass.DeathReasonSuperseded},
		{SessionID: "s_2", CWD: "/w", ConfigDir: "/cfg", ClaudeSessionID: "uuid-other",
			CreatedAt: "2026-07-10T10:00:00Z"},
		{SessionID: "s_3", CWD: "/w", ConfigDir: "/cfg", ClaudeSessionID: "uuid-old",
			CreatedAt: "2026-07-29T10:00:00Z", Terminal: true, DeathReason: errclass.DeathReasonSuperseded},
	} {
		if err := reg.Put(rec); err != nil {
			t.Fatalf("put: %v", err)
		}
	}

	// Act
	got, _ := resolverOver(t, reg).ResolveResume("/cfg", "/w")

	// Assert
	if got != "uuid-old" {
		t.Fatalf("ResolveResume = %q, want uuid-old: its newest record is the newest of any conversation here", got)
	}
}
