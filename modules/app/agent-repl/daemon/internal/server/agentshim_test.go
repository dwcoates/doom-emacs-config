package server

import (
	"path/filepath"
	"slices"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/registry"
	"claude-repld/internal/ssm"
)

// TestShimUDSArgvAppendsSocketContract covers the spawn contract: the existing
// stdio argv PLUS `--daemon-socket <path>`, the socket the shim DIALS.
func TestShimUDSArgvAppendsSocketContract(t *testing.T) {
	// Arrange
	opts := CreateOpts{Model: "haiku", CWD: "/w"}
	// Act
	got := ShimUDSArgv("node", "shim.js", "s1", false, opts, "/tmp/sock/daemon-shim.sock")
	// Assert — base argv unchanged, daemon socket appended last.
	want := []string{"node", "shim.js", "--session-id", "s1", "--cwd", "/w", "--model", "haiku",
		"--daemon-socket", "/tmp/sock/daemon-shim.sock"}
	if !slices.Equal(got, want) {
		t.Fatalf("argv = %v, want %v", got, want)
	}
}

// openTestRegistry opens a registry on a temp path.
func openTestRegistry(t *testing.T) *registry.Registry {
	t.Helper()
	path := filepath.Join(t.TempDir(), "sessions.db")
	return registry.Open(path, func(string, ...any) {})
}

// TestRegistrySeqStoreRoundTrip covers seq-store persistence: a watermark
// written through the adapter survives a registry reopen (daemon restart).
func TestRegistrySeqStoreRoundTrip(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "sessions.db")
	reg := registry.Open(path, func(string, ...any) {})
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	store := NewRegistrySeqStore(reg, nil)
	// Act — advance the watermark, then reopen from disk.
	if got := store.LastSeq("s1"); got != 0 {
		t.Fatalf("initial last_seq = %d, want 0", got)
	}
	store.SetLastSeq("s1", 42)
	reopened := registry.Open(path, func(string, ...any) {})
	reopenedStore := NewRegistrySeqStore(reopened, nil)
	// Assert
	if got := reopenedStore.LastSeq("s1"); got != 42 {
		t.Fatalf("post-reopen last_seq = %d, want 42", got)
	}
}

// TestRegistrySeqStoreUnknownSessionLogs: a write for an unregistered session
// is loud-logged and returns 0, never silently accepted.
func TestRegistrySeqStoreUnknownSessionZero(t *testing.T) {
	// Arrange
	reg := openTestRegistry(t)
	store := NewRegistrySeqStore(reg, nil)
	// Act / Assert — no record -> 0.
	if got := store.LastSeq("nope"); got != 0 {
		t.Fatalf("last_seq for unknown session = %d, want 0", got)
	}
}

// TestRegistryResolverBindsSSMToWorkspace covers the SSM-resolver binding: an
// event for a registered session resolves to that session's CWD workspace, and
// the SSM records the transition under it.
func TestRegistryResolverBindsSSMToWorkspace(t *testing.T) {
	// Arrange — a registered session bound to workspace /w, and an SSM whose
	// Resolver is the registry adapter.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1", CWD: "/w"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	dbPath := filepath.Join(t.TempDir(), "state.db")
	mgr, err := ssm.Open(ssm.Options{DBPath: dbPath, Resolver: NewRegistryResolver(reg), Logf: func(string, ...any) {}})
	if err != nil {
		t.Fatalf("ssm open: %v", err)
	}
	defer mgr.Close()
	// The workspace has to be WIRED for anything the agent reports to show
	// through: blue means no live session, and this test is about the RESOLVER
	// binding a session to a workspace, not about the wiring.
	if err := mgr.ApplyWired("/w", ssm.WiringWired, "test arrangement"); err != nil {
		t.Fatalf("apply wired: %v", err)
	}
	// Act — a turn-started event for s1 (seq 1).
	ev := &corev1.Event{
		SessionId: "s1",
		Seq:       1,
		Plane:     corev1.Plane_PLANE_STREAM,
		RequestId: "turn-1",
		Payload:   &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{TurnId: "turn-1"}},
	}
	if _, err := mgr.ApplyTurnBoundary("/w", "s1", "", ev); err != nil {
		t.Fatalf("apply: %v", err)
	}
	// Assert — the SSM resolved workspace /w and moved it to THINKING.
	cur, found, err := mgr.Current("/w")
	if err != nil {
		t.Fatalf("current: %v", err)
	}
	if !found {
		t.Fatal("workspace /w not resolved; the resolver did not bind the session")
	}
	if cur.GetState() != frontendv1.RenderState_RENDER_STATE_THINKING {
		t.Fatalf("state = %v, want THINKING", cur.GetState())
	}
	if cur.GetSessionId() != "s1" {
		t.Fatalf("session_id = %q, want s1", cur.GetSessionId())
	}
}

// TestRegistryResolverMissWorkspaceless: a registered session with no CWD is an
// explicit resolver miss (workspace-less), not a bind to the empty workspace.
func TestRegistryResolverMissWorkspaceless(t *testing.T) {
	// Arrange
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s1"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	r := NewRegistryResolver(reg)
	// Act
	b, ok := r.Session("s1")
	// Assert
	if ok || b.Workspace != "" {
		t.Fatalf("Session = (%+v,%v), want an empty miss for a workspace-less session", b, ok)
	}
}

// --- resolving the VENDOR session id ----------------------------------------
//
// The SSM resolves the id carried on the EVENT, and events are keyed by the
// vendor uuid (the store files them under it, because the shim reads it off
// the SDK message and the sidecar derives it from `<uuid>.jsonl`). The registry
// is keyed by the daemon's s_ id, so resolving only that meant every lifecycle
// event failed with "no workspace bound to session <uuid>" and no turn or task
// state ever reached a workspace.

func TestRegistryResolverResolvesTheVendorSessionID(t *testing.T) {
	// Arrange: a record keyed by s_ id, carrying its vendor uuid.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{
		SessionID:       "s_abc",
		CWD:             "/w",
		ClaudeSessionID: "96a0baaf-uuid",
		CreatedAt:       time.Now().UTC().Format(time.RFC3339),
	}); err != nil {
		t.Fatalf("put: %v", err)
	}

	// Act: resolve by the id an EVENT would carry.
	b, ok := NewRegistryResolver(reg).Session("96a0baaf-uuid")

	// Assert
	if !ok || b.Workspace != "/w" {
		t.Fatalf("Session(uuid).Workspace = (%q, %v), want (/w, true)", b.Workspace, ok)
	}
}

func TestRegistryResolverStillResolvesTheDaemonSessionID(t *testing.T) {
	// Arrange
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s_abc", CWD: "/w", ClaudeSessionID: "96a0baaf-uuid"}); err != nil {
		t.Fatalf("put: %v", err)
	}

	// Act / Assert: the registry's own key must keep working.
	b, ok := NewRegistryResolver(reg).Session("s_abc")
	if !ok || b.Workspace != "/w" {
		t.Fatalf("Session(s_id).Workspace = (%q, %v), want (/w, true)", b.Workspace, ok)
	}
}

func TestRegistryResolverPrefersTheNewestRecordForAVendorID(t *testing.T) {
	// Arrange: one uuid carried by two records (a superseded resume), each
	// bound to a different workspace.
	reg := openTestRegistry(t)
	older := time.Now().Add(-time.Hour).UTC().Format(time.RFC3339)
	newer := time.Now().UTC().Format(time.RFC3339)
	if err := reg.Put(registry.Record{SessionID: "s_old", CWD: "/old", ClaudeSessionID: "shared-uuid", CreatedAt: older}); err != nil {
		t.Fatalf("put: %v", err)
	}
	if err := reg.Put(registry.Record{SessionID: "s_new", CWD: "/new", ClaudeSessionID: "shared-uuid", CreatedAt: newer}); err != nil {
		t.Fatalf("put: %v", err)
	}

	// Act
	b, ok := NewRegistryResolver(reg).Session("shared-uuid")

	// Assert
	if !ok || b.Workspace != "/new" {
		t.Fatalf("Session(shared uuid).Workspace = (%q, %v), want (/new, true)", b.Workspace, ok)
	}
}

func TestRegistryResolverMissesAnUnknownVendorID(t *testing.T) {
	// Arrange
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s_abc", CWD: "/w", ClaudeSessionID: "96a0baaf-uuid"}); err != nil {
		t.Fatalf("put: %v", err)
	}

	// Act / Assert: an unknown id is an explicit miss the SSM surfaces loudly,
	// never a bind to some arbitrary workspace.
	if b, ok := NewRegistryResolver(reg).Session("nobody-uuid"); ok {
		t.Fatalf("Session(unknown) = (%+v, true), want a miss", b)
	}
}

func TestRegistryResolverIgnoresAVendorMatchWithNoWorkspace(t *testing.T) {
	// Arrange: a workspace-less session has no per-workspace state to bind.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s_abc", ClaudeSessionID: "96a0baaf-uuid"}); err != nil {
		t.Fatalf("put: %v", err)
	}

	// Act / Assert
	if b, ok := NewRegistryResolver(reg).Session("96a0baaf-uuid"); ok {
		t.Fatalf("Session(uuid with no cwd) = (%+v, true), want a miss", b)
	}
}

// --- the seq mark belongs to the CONVERSATION -------------------------------
//
// Every restart mints a fresh s_ id for the same conversation, but the store's
// seq space is keyed by the vendor uuid. Reading only the new record's own mark
// returned 0, so the shim re-subscribed from 0 and replayed the whole history —
// and the first prompt's Ack queued behind those thousands of frames until it
// timed out.

func TestSeqStoreInheritsTheMarkFromTheSameConversation(t *testing.T) {
	// Arrange: a superseded record with a high-water mark, and the fresh
	// session that replaced it for the SAME conversation.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s_old", CWD: "/w", ClaudeSessionID: "uuid-1", LastSeq: 6037}); err != nil {
		t.Fatalf("put: %v", err)
	}
	if err := reg.Put(registry.Record{SessionID: "s_new", CWD: "/w", ClaudeSessionID: "uuid-1"}); err != nil {
		t.Fatalf("put: %v", err)
	}

	// Act
	got := NewRegistrySeqStore(reg, nil).LastSeq("s_new")

	// Assert: continues where the conversation stopped, not from zero.
	if got != 6037 {
		t.Fatalf("last_seq = %d, want 6037 — a fresh session id must not replay the conversation", got)
	}
}

func TestSeqStoreIgnoresOtherConversations(t *testing.T) {
	// Arrange: a busy conversation must not lift another's mark, or the second
	// would skip events it never saw.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s_busy", CWD: "/a", ClaudeSessionID: "uuid-busy", LastSeq: 9000}); err != nil {
		t.Fatalf("put: %v", err)
	}
	if err := reg.Put(registry.Record{SessionID: "s_quiet", CWD: "/b", ClaudeSessionID: "uuid-quiet", LastSeq: 12}); err != nil {
		t.Fatalf("put: %v", err)
	}

	// Act / Assert
	if got := NewRegistrySeqStore(reg, nil).LastSeq("s_quiet"); got != 12 {
		t.Fatalf("last_seq = %d, want 12", got)
	}
}

func TestSeqStoreSeparatesTheSameUUIDAcrossRootsAndWorkspaces(t *testing.T) {
	// Arrange: vendor uuids are not globally unique across account roots, and
	// the same uuid under another cwd names another transcript/store sequence.
	reg := openTestRegistry(t)
	for _, rec := range []registry.Record{
		{SessionID: "s_a", ConfigDir: "/cfg-a", CWD: "/w", ClaudeSessionID: "shared", LastSeq: 101},
		{SessionID: "s_b", ConfigDir: "/cfg-b", CWD: "/w", ClaudeSessionID: "shared", LastSeq: 202},
		{SessionID: "s_c", ConfigDir: "/cfg-a", CWD: "/other", ClaudeSessionID: "shared", LastSeq: 303},
	} {
		if err := reg.Put(rec); err != nil {
			t.Fatalf("Put(%s): %v", rec.SessionID, err)
		}
	}
	store := NewRegistrySeqStore(reg, nil)

	// Act / Assert.
	for id, want := range map[string]uint64{"s_a": 101, "s_b": 202, "s_c": 303} {
		if got := store.LastSeq(id); got != want {
			t.Fatalf("LastSeq(%s) = %d, want %d", id, got, want)
		}
	}
}

func TestSeqStoreUsesItsOwnMarkBeforeTheConversationIsKnown(t *testing.T) {
	// Arrange: a brand-new session has no vendor uuid until system:init.
	reg := openTestRegistry(t)
	if err := reg.Put(registry.Record{SessionID: "s_fresh", CWD: "/w", LastSeq: 5}); err != nil {
		t.Fatalf("put: %v", err)
	}

	// Act / Assert
	if got := NewRegistrySeqStore(reg, nil).LastSeq("s_fresh"); got != 5 {
		t.Fatalf("last_seq = %d, want 5", got)
	}
}
