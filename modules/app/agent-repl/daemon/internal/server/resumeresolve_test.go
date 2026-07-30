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
// Each test pins the transcript check rather than touching the real filesystem:
// the resolver's whole contract is that a conversation is resumable only when
// the vendor actually wrote it, and that answer must be injectable to test the
// skip path at all.
// ---------------------------------------------------------------------------

// resolverOver builds a resolver whose transcript check reports true for
// exactly the uuids in present.
func resolverOver(t *testing.T, reg *registry.Registry, present ...string) *ConversationResolver {
	t.Helper()
	set := make(map[string]bool, len(present))
	for _, p := range present {
		set[p] = true
	}
	return &ConversationResolver{
		Reg:  reg,
		Logf: t.Logf,
		transcriptExists: func(_, _, csid string) (string, bool) {
			return "/transcripts/" + csid + ".jsonl", set[csid]
		},
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
	got, ok := resolverOver(t, reg, "uuid-1").ResolveResume("/cfg", "/w")

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
	got, _ := resolverOver(t, reg, "uuid-old", "uuid-new").ResolveResume("/cfg", "/w")

	// Assert
	if got != "uuid-new" {
		t.Fatalf("ResolveResume = %q, want uuid-new", got)
	}
}

func TestResolveResumeSkipsAConversationTheVendorNeverWrote(t *testing.T) {
	// Arrange — the newest record names a uuid with no transcript behind it,
	// which is precisely the case the old adopt-late hold existed to prevent.
	// Resolving it would make the CLI hard-exit on --resume.
	reg := openTestRegistry(t)
	for _, rec := range []registry.Record{
		{SessionID: "s1", CWD: "/w", ConfigDir: "/cfg", ClaudeSessionID: "uuid-real", CreatedAt: "2026-07-01T10:00:00Z"},
		{SessionID: "s2", CWD: "/w", ConfigDir: "/cfg", ClaudeSessionID: "uuid-phantom", CreatedAt: "2026-07-29T10:00:00Z"},
	} {
		if err := reg.Put(rec); err != nil {
			t.Fatalf("put: %v", err)
		}
	}

	// Act — only the older one exists on disk.
	got, ok := resolverOver(t, reg, "uuid-real").ResolveResume("/cfg", "/w")

	// Assert
	if !ok || got != "uuid-real" {
		t.Fatalf("ResolveResume = (%q, %v), want (uuid-real, true)", got, ok)
	}
}

func TestResolveResumeStartsFreshWhenNoTranscriptSurvives(t *testing.T) {
	// Arrange — the registry remembers a conversation whose transcript is gone.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{
		SessionID: "s1", CWD: "/w", ConfigDir: "/cfg",
		ClaudeSessionID: "uuid-gone", CreatedAt: "2026-07-29T10:00:00Z",
	}); err != nil {
		t.Fatalf("put: %v", err)
	}

	// Act
	_, ok := resolverOver(t, reg).ResolveResume("/cfg", "/w")

	// Assert
	if ok {
		t.Fatal("resolved a conversation with no transcript on disk")
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
	_, ok := resolverOver(t, reg, "uuid-deleted").ResolveResume("/cfg", "/w")

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
	got, ok := resolverOver(t, reg, "uuid-superseded").ResolveResume("/cfg", "/w")

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
	_, ok := resolverOver(t, reg, "uuid-other").ResolveResume("/cfg", "/w")

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
	_, ok := resolverOver(t, reg, "uuid-other-account").ResolveResume("/cfg", "/w")

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
	got, _ := resolverOver(t, reg, "uuid-real").ResolveResume("/cfg", "/w")

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
