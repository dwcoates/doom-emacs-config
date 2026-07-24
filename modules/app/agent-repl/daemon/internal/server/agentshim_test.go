package server

import (
	"context"
	"net"
	"os"
	"path/filepath"
	"slices"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
	"agentrepl/wire"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/types/known/anypb"

	"claude-repld/internal/registry"
	"claude-repld/internal/ssm"
)

// TestShimUDSArgvAppendsSocketContract covers the UDS spawn contract: the
// existing stdio argv PLUS `--uds-socket <path>` (design §8, §4.4).
func TestShimUDSArgvAppendsSocketContract(t *testing.T) {
	// Arrange
	opts := CreateOpts{Model: "haiku", CWD: "/w"}
	// Act
	got := ShimUDSArgv("node", "shim.js", "s1", false, opts, "/tmp/sock/session-s1.sock")
	// Assert — base argv unchanged, socket flag appended last.
	want := []string{"node", "shim.js", "--session-id", "s1", "--cwd", "/w", "--model", "haiku",
		"--uds-socket", "/tmp/sock/session-s1.sock"}
	if !slices.Equal(got, want) {
		t.Fatalf("argv = %v, want %v", got, want)
	}
}

// TestSessionSocketPathConvention covers the shared UDS path convention.
func TestSessionSocketPathConvention(t *testing.T) {
	// Act
	got := SessionSocketPath("s1")
	// Assert — non-empty and ends with the per-session socket name.
	if got == "" {
		t.Fatal("empty socket path")
	}
	if base := filepath.Base(got); base != "session-s1.sock" {
		t.Fatalf("socket base = %q, want session-s1.sock", base)
	}
}

// writeHelloFrame writes one framed google.protobuf.Any(msg) to conn, the same
// encoding the shimclient reads.
func writeHelloFrame(t *testing.T, conn net.Conn, msg proto.Message) {
	t.Helper()
	env, err := anypb.New(msg)
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	payload, err := proto.Marshal(env)
	if err != nil {
		t.Fatalf("marshal: %v", err)
	}
	if err := wire.WriteFrame(conn, payload); err != nil {
		t.Fatalf("write frame: %v", err)
	}
}

// shortSocketPath returns a temp socket path short enough for the macOS
// sun_path limit (~104 bytes), which a t.TempDir() path can exceed.
func shortSocketPath(t *testing.T, name string) string {
	t.Helper()
	dir, err := os.MkdirTemp("", "sk")
	if err != nil {
		t.Fatalf("mkdir temp: %v", err)
	}
	t.Cleanup(func() { os.RemoveAll(dir) })
	return filepath.Join(dir, name)
}

// listenUnix opens a unix listener at a temp socket path and returns both.
func listenUnix(t *testing.T) (net.Listener, string) {
	t.Helper()
	path := shortSocketPath(t, "s.sock")
	l, err := net.Listen("unix", path)
	if err != nil {
		t.Fatalf("listen unix: %v", err)
	}
	t.Cleanup(func() { l.Close() })
	return l, path
}

// TestReattachDecisionLiveShimReattaches: a live listener that opens with a
// ShimHello (the listener speaks first) -> reattach.
func TestReattachDecisionLiveShimReattaches(t *testing.T) {
	// Arrange — a fake shim listener that greets with ShimHello.
	l, path := listenUnix(t)
	go func() {
		conn, err := l.Accept()
		if err != nil {
			return
		}
		defer conn.Close()
		writeHelloFrame(t, conn, &corev1.ShimHello{ShimVersion: "test", TurnInFlight: true})
		time.Sleep(50 * time.Millisecond)
	}()
	// Act
	reattach, err := ReattachDecision(context.Background(), path)
	// Assert
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if !reattach {
		t.Fatal("want reattach=true for a live greeting shim")
	}
}

// TestReattachDecisionNoSocketSpawns: no listener at the path -> spawn (false),
// no error (the expected fresh-session / dead-shim case).
func TestReattachDecisionNoSocketSpawns(t *testing.T) {
	// Arrange — a path with nothing listening.
	path := shortSocketPath(t, "absent.sock")
	// Act
	reattach, err := ReattachDecision(context.Background(), path)
	// Assert
	if err != nil {
		t.Fatalf("dial failure must not be an error: %v", err)
	}
	if reattach {
		t.Fatal("want reattach=false when no shim is listening")
	}
}

// TestReattachDecisionWrongFrameErrors: a listener answering with a non-Hello
// frame is an anomaly surfaced as an error (never a silent respawn).
func TestReattachDecisionWrongFrameErrors(t *testing.T) {
	// Arrange — a listener that greets with the wrong message type.
	l, path := listenUnix(t)
	go func() {
		conn, err := l.Accept()
		if err != nil {
			return
		}
		defer conn.Close()
		writeHelloFrame(t, conn, &corev1.Heartbeat{SentAtMs: 1})
		time.Sleep(50 * time.Millisecond)
	}()
	// Act
	reattach, err := ReattachDecision(context.Background(), path)
	// Assert
	if err == nil {
		t.Fatal("want an error when the socket answers with a non-ShimHello frame")
	}
	if reattach {
		t.Fatal("want reattach=false on a probe error")
	}
}

// openTestRegistry opens a registry on a temp path.
func openTestRegistry(t *testing.T) *registry.Registry {
	t.Helper()
	path := filepath.Join(t.TempDir(), "sessions.json")
	return registry.Open(path, func(string, ...any) {})
}

// TestRegistrySeqStoreRoundTrip covers seq-store persistence: a watermark
// written through the adapter survives a registry reopen (daemon restart).
func TestRegistrySeqStoreRoundTrip(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "sessions.json")
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
	// Act — a turn-started event for s1 (seq 1).
	ev := &corev1.Event{SessionId: "s1", Seq: 1, Payload: &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{}}}
	if err := mgr.Apply(ev); err != nil {
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
	ws, ok := r.Workspace("s1")
	// Assert
	if ok || ws != "" {
		t.Fatalf("Workspace = (%q,%v), want (\"\",false) for a workspace-less session", ws, ok)
	}
}
