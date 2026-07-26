// backfill_test.go pins the NEVER-BLUE backfill contract end to end.
//
// When the daemon discovers a workspace's on-disk transcript and binds it as a
// resume target, that conversation's history has to reach the store or the
// workspace renders with an empty feed. The sidecar is what carries it: a
// transcript it has no persisted cursor for is tailed from offset 0, so the
// WHOLE file round-trips as events. Re-reading a transcript whose events are
// already persisted must therefore be free — that is what the store's dedup
// keys are for — otherwise every rediscovery would duplicate the feed.
//
// These tests exercise the real tailer, the real transcript handler, and the
// REAL shim-store binary, so the dedup claim is verified against the store's
// own unique index rather than a mock.
package main

import (
	"encoding/json"
	"fmt"
	"net"
	"os"
	"os/exec"
	"path/filepath"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/shim-claude-sidecar/internal/handler"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

const backfillSession = "11111111-2222-3333-4444-555555555555"

// repoSubdir walks up from the test's working directory to a sibling path.
func repoSubdir(t *testing.T, marker string, parts ...string) string {
	t.Helper()
	dir, _ := os.Getwd()
	for {
		cand := filepath.Join(append([]string{dir}, parts...)...)
		if _, err := os.Stat(filepath.Join(cand, marker)); err == nil {
			return cand
		}
		p := filepath.Dir(dir)
		if p == dir {
			t.Fatalf("%v not found above %s", parts, dir)
		}
		dir = p
	}
}

// startStore builds and spawns the REAL shim-store on a temp socket.
func startStore(t *testing.T) string {
	t.Helper()
	srcDir := repoSubdir(t, "main.go", "agent-shim", "shim-store")
	tmp := t.TempDir()
	bin := filepath.Join(tmp, "shim-store")
	build := exec.Command("go", "build", "-o", bin, ".")
	build.Dir = srcDir
	if out, err := build.CombinedOutput(); err != nil {
		t.Fatalf("building shim-store: %v\n%s", err, out)
	}
	// macOS sun_path is short; keep the socket under /tmp.
	sockDir, err := os.MkdirTemp("/tmp", "backfill")
	if err != nil {
		t.Fatalf("mkdtemp: %v", err)
	}
	t.Cleanup(func() { os.RemoveAll(sockDir) })
	sock := filepath.Join(sockDir, "s")

	cmd := exec.Command(bin, "-socket", sock, "-db", filepath.Join(tmp, "events.db"), "-log", filepath.Join(tmp, "store.log"))
	cmd.Stderr = os.Stderr
	if err := cmd.Start(); err != nil {
		t.Fatalf("starting shim-store: %v", err)
	}
	t.Cleanup(func() {
		_ = cmd.Process.Kill()
		_, _ = cmd.Process.Wait()
	})
	// Readiness poll on an EXTERNAL process's socket; no in-process channel can
	// signal it.
	deadline := time.Now().Add(10 * time.Second)
	for time.Now().Before(deadline) {
		if c, err := net.Dial("unix", sock); err == nil {
			c.Close()
			return sock
		}
		time.Sleep(20 * time.Millisecond)
	}
	t.Fatalf("shim-store did not start listening on %s", sock)
	return ""
}

// writeHistory lays down a config root holding one session transcript of n
// assistant lines, and returns (root, transcriptPath). Assistant lines are used
// deliberately: they carry a uuid, so the store derives a `uuid:` dedup key for
// each and the overlap assertion is about the store's index, not a
// producer-supplied key.
func writeHistory(t *testing.T, n int) (string, string) {
	t.Helper()
	root := t.TempDir()
	dir := filepath.Join(root, "projects", "-w")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	path := filepath.Join(dir, backfillSession+".jsonl")
	f, err := os.Create(path)
	if err != nil {
		t.Fatalf("create: %v", err)
	}
	defer f.Close()
	for i := 0; i < n; i++ {
		line := map[string]any{
			"type":      "assistant",
			"uuid":      fmt.Sprintf("hist-uuid-%d", i),
			"sessionId": backfillSession,
			"timestamp": "2026-07-25T12:00:00.000Z",
			"message": map[string]any{
				"id":      fmt.Sprintf("msg_%d", i),
				"role":    "assistant",
				"model":   "claude",
				"content": []any{map[string]any{"type": "text", "text": "history"}},
			},
		}
		raw, err := json.Marshal(line)
		if err != nil {
			t.Fatalf("marshal: %v", err)
		}
		if _, err := f.Write(append(raw, '\n')); err != nil {
			t.Fatalf("write: %v", err)
		}
	}
	return root, path
}

// pollTranscript builds a sidecar over root, discovers the transcript with NO
// cursor (exactly the newly-discovered case), and polls it once.
func pollTranscript(t *testing.T, sock, root string) (*sidecar, tail.PollResult, string) {
	t.Helper()
	s := newSidecar(sock, []string{root}, t.TempDir(), func(string, ...any) {})
	t.Cleanup(func() { s.store.Close() })
	s.rescan(nil)
	if len(s.watchers) != 1 {
		t.Fatalf("discovered %d targets; want exactly the transcript", len(s.watchers))
	}
	var path string
	for p := range s.watchers {
		path = p
	}
	res, err := s.watchers[path].tailer.Poll()
	if err != nil {
		t.Fatalf("poll: %v", err)
	}
	return s, res, path
}

func TestNewlyDiscoveredTranscriptIsTailedFromOffsetZero(t *testing.T) {
	// Arrange — a transcript with history and no persisted cursor.
	sock := startStore(t)
	root, path := writeHistory(t, 5)

	// Act
	_, res, _ := pollTranscript(t, sock, root)

	// Assert — the WHOLE file was read, not just its tail.
	fi, err := os.Stat(path)
	if err != nil {
		t.Fatalf("stat: %v", err)
	}
	if res.Next.GetOffset() != fi.Size() {
		t.Fatalf("cursor advanced to %d; want the full file size %d", res.Next.GetOffset(), fi.Size())
	}
}

func TestBackfillCarriesEveryHistoryLineIntoTheStore(t *testing.T) {
	// Arrange
	sock := startStore(t)
	root, _ := writeHistory(t, 5)
	s, res, _ := pollTranscript(t, sock, root)

	// Act
	ack, err := s.store.Write(handler.Producer, &corev1.EventBatch{Events: res.Events, CursorAdvance: res.Next})
	if err != nil {
		t.Fatalf("store write: %v", err)
	}

	// Assert — every one of the five history lines was accepted.
	if ack.GetAccepted() != 5 {
		t.Fatalf("accepted = %d (deduped %d); want the 5 backfilled lines", ack.GetAccepted(), ack.GetDeduped())
	}
}

func TestBackfillOverlapIsAbsorbedByStoreDedup(t *testing.T) {
	// Arrange — the history is already persisted (a prior backfill, or the
	// stream plane having written the same messages).
	sock := startStore(t)
	root, _ := writeHistory(t, 5)
	first, firstRes, _ := pollTranscript(t, sock, root)
	if _, err := first.store.Write(handler.Producer, &corev1.EventBatch{Events: firstRes.Events, CursorAdvance: firstRes.Next}); err != nil {
		t.Fatalf("first write: %v", err)
	}

	// Act — a rediscovery with no cursor re-reads the whole file from 0.
	second, secondRes, _ := pollTranscript(t, sock, root)
	ack, err := second.store.Write(handler.Producer, &corev1.EventBatch{Events: secondRes.Events, CursorAdvance: secondRes.Next})
	if err != nil {
		t.Fatalf("second write: %v", err)
	}

	// Assert — the overlap costs nothing: every event dedups, none is accepted.
	if ack.GetAccepted() != 0 || ack.GetDeduped() != 5 {
		t.Fatalf("re-backfill accepted=%d deduped=%d; want 0 accepted / 5 deduped", ack.GetAccepted(), ack.GetDeduped())
	}
}
