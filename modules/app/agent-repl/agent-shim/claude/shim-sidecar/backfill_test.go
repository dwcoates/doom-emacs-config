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
	"os"
	"path/filepath"
	"testing"

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

// startStore spawns the REAL shim-store on a temp socket and returns its path.
// It is the "just give me a running store" shorthand over the stoppable harness
// in link_test.go.
func startStore(t *testing.T) string {
	t.Helper()
	h := newStoreHarness(t)
	h.start()
	return h.sock
}

// writeHistory lays down a config root holding one session transcript of n
// assistant lines, and returns (root, transcriptPath). Assistant lines are used
// deliberately: they carry a uuid, so the store derives a `uuid:` dedup key for
// each and the overlap assertion is about the store's index, not a
// producer-supplied key.
func writeHistory(t *testing.T, n int) (string, string) {
	t.Helper()
	root := t.TempDir()
	return root, writeSessionHistory(t, root, backfillSession, n)
}

// writeSessionHistory lays down one session transcript of n assistant lines
// under an EXISTING config root, so several sessions can share a root the way
// they do on disk.
func writeSessionHistory(t *testing.T, root, session string, n int) string {
	t.Helper()
	dir := filepath.Join(root, "projects", "-w")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	path := filepath.Join(dir, session+".jsonl")
	f, err := os.Create(path)
	if err != nil {
		t.Fatalf("create: %v", err)
	}
	defer f.Close()
	for i := 0; i < n; i++ {
		if _, err := f.Write(historyLineFor(t, session, i)); err != nil {
			t.Fatalf("write: %v", err)
		}
	}
	return path
}

// appendHistory appends n further assistant lines to an existing transcript,
// numbered from `from` so their uuids (and therefore their dedup keys) stay
// distinct from the lines already there.
func appendHistory(t *testing.T, path string, from, n int) {
	t.Helper()
	f, err := os.OpenFile(path, os.O_WRONLY|os.O_APPEND, 0o644)
	if err != nil {
		t.Fatalf("open %s for append: %v", path, err)
	}
	defer f.Close()
	for i := from; i < from+n; i++ {
		if _, err := f.Write(historyLineFor(t, backfillSession, i)); err != nil {
			t.Fatalf("append: %v", err)
		}
	}
}

// historyLineFor renders one newline-terminated assistant transcript line. The
// record uuid is deliberately independent of the session: a rewind's truncated
// copy carries the SAME record uuids under a new sessionId, which is exactly
// what makes the per-session scope of store dedup observable.
func historyLineFor(t *testing.T, session string, i int) []byte {
	t.Helper()
	raw, err := json.Marshal(map[string]any{
		"type":      "assistant",
		"uuid":      fmt.Sprintf("hist-uuid-%d", i),
		"sessionId": session,
		"timestamp": "2026-07-25T12:00:00.000Z",
		"message": map[string]any{
			"id":      fmt.Sprintf("msg_%d", i),
			"role":    "assistant",
			"model":   "claude",
			"content": []any{map[string]any{"type": "text", "text": "history"}},
		},
	})
	if err != nil {
		t.Fatalf("marshal: %v", err)
	}
	return append(raw, '\n')
}

// pollTranscript builds a sidecar over root, brings its store link up (which
// recovers cursors before anything is read), and polls the discovered
// transcript once.
func pollTranscript(t *testing.T, sock, root string) (*sidecar, tail.PollResult, string) {
	t.Helper()
	s := newSidecar(sock, []string{root}, t.TempDir(), quietLog)
	t.Cleanup(func() { s.store.Close() })
	if err := s.establish(); err != nil {
		t.Fatalf("establish: %v", err)
	}
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

// rewoundSession is the uuid a rewind's truncated copy is written under.
const rewoundSession = "99999999-8888-7777-6666-555555555555"

// pollOnePath brings a sidecar's store link up over root and polls exactly the
// named watcher, leaving every other discovered target alone. It is the
// multi-transcript sibling of pollTranscript.
func pollOnePath(t *testing.T, sock, root, path string) (*sidecar, tail.PollResult) {
	t.Helper()
	s := newSidecar(sock, []string{root}, t.TempDir(), quietLog)
	t.Cleanup(func() { s.store.Close() })
	if err := s.establish(); err != nil {
		t.Fatalf("establish: %v", err)
	}
	w, ok := s.watchers[path]
	if !ok {
		t.Fatalf("no watcher for %s (watchers: %d)", path, len(s.watchers))
	}
	res, err := w.tailer.Poll()
	if err != nil {
		t.Fatalf("poll: %v", err)
	}
	return s, res
}

// backfilledCopy is the arrangement every rewind test shares: an original
// session already fully persisted with its cursor committed, plus the truncated
// COPY a rewind produces beside it — the first `retain` records under a new
// uuid filename and sessionId — polled and written to the store. It returns the
// copy's ack and the copy's path.
func backfilledCopy(t *testing.T, retain int) (*corev1.StoreWriteAck, string) {
	t.Helper()
	sock := startStore(t)
	root, _ := writeHistory(t, 5)
	original, originalRes, _ := pollTranscript(t, sock, root)
	if _, err := original.store.Write(handler.Producer, &corev1.EventBatch{Events: originalRes.Events, CursorAdvance: originalRes.Next}); err != nil {
		t.Fatalf("original write: %v", err)
	}

	copyPath := writeSessionHistory(t, root, rewoundSession, retain)
	rewound, rewoundRes := pollOnePath(t, sock, root, copyPath)
	ack, err := rewound.store.Write(handler.Producer, &corev1.EventBatch{Events: rewoundRes.Events, CursorAdvance: rewoundRes.Next})
	if err != nil {
		t.Fatalf("rewound copy write: %v", err)
	}
	return ack, copyPath
}

// The copy is a brand-new file to discovery, so it backfills from 0 and its
// records land in the new session's OWN seq space — the re-ingest that renders
// the post-rewind conversation.
func TestRewoundCopyBackfillsIntoItsOwnSeqSpace(t *testing.T) {
	// Arrange + Act
	ack, _ := backfilledCopy(t, 3)

	// Assert
	if ack.GetLastSeq() != 3 {
		t.Fatalf("last_seq = %d; want the 3 retained records numbered 1..3 in a fresh seq space", ack.GetLastSeq())
	}
}

// Store dedup is scoped to (session_id, dedup_key), so the copy's records —
// which carry the SAME vendor uuids as the originals — must all be accepted.
// That cross-session duplication is the design, not a leak: the two seq spaces
// are two separate conversations as far as the store is concerned.
func TestRewoundCopyIsNotDedupedAgainstTheRetiredSession(t *testing.T) {
	// Arrange + Act
	ack, _ := backfilledCopy(t, 3)

	// Assert
	if ack.GetAccepted() != 3 || ack.GetDeduped() != 0 {
		t.Fatalf("copy accepted=%d deduped=%d; want 3 accepted / 0 deduped (dedup is per-session)", ack.GetAccepted(), ack.GetDeduped())
	}
}

// The retired transcript keeps its committed cursor across the rewind, so the
// copy's arrival must not make the sidecar re-read the original.
func TestRewindLeavesTheRetiredTranscriptCursorIntact(t *testing.T) {
	// Arrange
	sock := startStore(t)
	root, originalPath := writeHistory(t, 5)
	original, originalRes, _ := pollTranscript(t, sock, root)
	if _, err := original.store.Write(handler.Producer, &corev1.EventBatch{Events: originalRes.Events, CursorAdvance: originalRes.Next}); err != nil {
		t.Fatalf("original write: %v", err)
	}
	writeSessionHistory(t, root, rewoundSession, 3)

	// Act — a fresh sidecar sees both files and polls the RETIRED one.
	_, res := pollOnePath(t, sock, root, originalPath)

	// Assert
	if res.Changed {
		t.Fatalf("retired transcript re-read %d record(s); want nothing after the rewind", res.Records)
	}
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
	// Arrange — the history is already persisted, but its CURSOR never was.
	// That is the sad path the dedup contract exists for: the events reached the
	// store and the cursor commit did not, so the next reader legitimately has
	// no cursor and replays the file. (With cursor recovery now scoped to the
	// connection, an ordinary rediscovery DOES find its cursor and reads
	// nothing — see TestReconnectResumesFromTheCommittedCursorNotZero.)
	sock := startStore(t)
	root, _ := writeHistory(t, 5)
	first, firstRes, _ := pollTranscript(t, sock, root)
	if _, err := first.store.Write(handler.Producer, &corev1.EventBatch{Events: firstRes.Events}); err != nil {
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
