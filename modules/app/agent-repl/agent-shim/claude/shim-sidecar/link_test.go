// link_test.go pins the store-link state machine (link.go).
//
// The contract under test is that cursor recovery is CONNECTION-scoped, not
// BOOT-scoped: every established connection recovers cursors as its first act,
// and while no connection exists the sidecar reads nothing at all. The
// regression it guards is the silent cold start — a store that was not
// listening yet made the sidecar re-read every watched file from offset 0.
//
// These run against the REAL shim-store binary, stopped and restarted under a
// live sidecar, so "the store went away" is a genuine dead socket rather than a
// mocked error.
package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/shim-claude-sidecar/internal/discover"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/storeclient"
	"agentrepl/wire"
)

var quietLog = logging.New(io.Discard, io.Discard).With(logging.Context{Component: "test"})

// probeComponent labels the events used to prove a store subscription is live,
// so the real degraded report can be told apart from the handshake.
const probeComponent = "link-test-probe"

// subscribeEvents opens a store subscription for session and returns a channel
// of every event delivered on it.
func subscribeEvents(t *testing.T, sock, session string) <-chan *corev1.Event {
	t.Helper()
	conn, err := net.Dial("unix", sock)
	if err != nil {
		t.Fatalf("dial store: %v", err)
	}
	t.Cleanup(func() { conn.Close() })
	if err := wire.WriteAny(conn, &corev1.Subscribe{SessionId: session, FromSeq: 0}); err != nil {
		t.Fatalf("Subscribe: %v", err)
	}
	ch := make(chan *corev1.Event, 64)
	go func() {
		defer close(ch)
		for {
			msg, err := wire.ReadAny(conn)
			if err != nil {
				return
			}
			if ev, ok := msg.(*corev1.Event); ok {
				ch <- ev
			}
		}
	}()
	awaitSubscriberRegistered(t, sock, session, ch)
	return ch
}

// awaitSubscriberRegistered publishes ephemeral probes until one comes back.
// The store registers a subscriber on its own accept loop, so this round trip
// is the only in-band proof that a later publish will actually be delivered.
func awaitSubscriberRegistered(t *testing.T, sock, session string, ch <-chan *corev1.Event) {
	t.Helper()
	probe := storeclient.New(sock, quietLog)
	defer probe.Close()
	if err := probe.Connect(); err != nil {
		t.Fatalf("probe connect: %v", err)
	}
	deadline := time.After(10 * time.Second)
	for {
		batch := &corev1.EventBatch{Events: []*corev1.Event{{
			SessionId: session,
			Plane:     corev1.Plane_PLANE_SYNTHETIC,
			Class:     corev1.EventClass_EVENT_CLASS_EPHEMERAL,
			Payload: &corev1.Event_DegradedState{
				DegradedState: &corev1.DegradedState{Component: probeComponent},
			},
		}}}
		if _, err := probe.Write(probeComponent, batch); err != nil {
			t.Fatalf("probe write: %v", err)
		}
		select {
		case ev, ok := <-ch:
			if !ok {
				t.Fatal("subscription closed during the registration handshake")
			}
			if ev.GetDegradedState().GetComponent() == probeComponent {
				return
			}
		case <-time.After(50 * time.Millisecond):
		case <-deadline:
			t.Fatal("store never delivered the subscription probe")
		}
	}
}

// awaitDegraded waits for the sidecar's own degraded report, skipping the
// registration probes.
func awaitDegraded(t *testing.T, ch <-chan *corev1.Event) *corev1.DegradedState {
	t.Helper()
	deadline := time.After(10 * time.Second)
	for {
		select {
		case ev, ok := <-ch:
			if !ok {
				t.Fatal("subscription closed before a degraded report arrived")
			}
			if ds := ev.GetDegradedState(); ds != nil && ds.GetComponent() != probeComponent {
				return ds
			}
		case <-deadline:
			t.Fatal("no DegradedState was delivered after the link recovered")
		}
	}
}

// ---------------------------------------------------------------------------
// A stoppable, restartable real store
// ---------------------------------------------------------------------------

// storeHarness runs the REAL shim-store binary and can stop and restart it on
// the SAME socket and database, which is what lets a test bounce the store
// underneath a live sidecar.
type storeHarness struct {
	t    *testing.T
	bin  string
	sock string
	db   string
	logp string
	cmd  *exec.Cmd
}

// newStoreHarness builds the store binary and picks its paths, but starts
// nothing: a harness that has never been started is exactly the "store is not
// up yet" boot race.
func newStoreHarness(t *testing.T) *storeHarness {
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
	sockDir, err := os.MkdirTemp("/tmp", "sidecarlink")
	if err != nil {
		t.Fatalf("mkdtemp: %v", err)
	}
	h := &storeHarness{
		t:    t,
		bin:  bin,
		sock: filepath.Join(sockDir, "s"),
		db:   filepath.Join(tmp, "events.db"),
		logp: filepath.Join(tmp, "store.log"),
	}
	t.Cleanup(func() {
		h.stop()
		os.RemoveAll(sockDir)
	})
	return h
}

// start spawns the store and waits until it accepts connections.
func (h *storeHarness) start() {
	h.t.Helper()
	cmd := exec.Command(h.bin, "-socket", h.sock, "-db", h.db, "-log", h.logp)
	cmd.Stderr = os.Stderr
	if err := cmd.Start(); err != nil {
		h.t.Fatalf("starting shim-store: %v", err)
	}
	h.cmd = cmd
	// Readiness poll on an EXTERNAL process's socket: no in-process channel can
	// signal when another process has finished binding.
	deadline := time.Now().Add(10 * time.Second)
	for time.Now().Before(deadline) {
		if c, err := net.Dial("unix", h.sock); err == nil {
			c.Close()
			return
		}
		time.Sleep(20 * time.Millisecond)
	}
	h.t.Fatalf("shim-store did not start listening on %s", h.sock)
}

// stop kills the store and waits until its socket refuses connections, so a
// test never races a still-draining listener.
func (h *storeHarness) stop() {
	h.t.Helper()
	if h.cmd == nil {
		return
	}
	_ = h.cmd.Process.Kill()
	_, _ = h.cmd.Process.Wait()
	h.cmd = nil
	deadline := time.Now().Add(10 * time.Second)
	for time.Now().Before(deadline) {
		c, err := net.Dial("unix", h.sock)
		if err != nil {
			return
		}
		c.Close()
		time.Sleep(20 * time.Millisecond)
	}
	h.t.Fatalf("shim-store still accepting on %s after being killed", h.sock)
}

// sidecarOver builds a sidecar watching root and pointed at the harness.
func (h *storeHarness) sidecarOver(root string) *sidecar {
	h.t.Helper()
	s := newSidecar(h.sock, []string{root}, h.t.TempDir(), quietLog)
	h.t.Cleanup(func() { s.store.Close() })
	return s
}

// cursorFor returns the store's persisted cursor for path, or nil.
func (h *storeHarness) cursorFor(path string) *corev1.CursorState {
	h.t.Helper()
	c := storeclient.New(h.sock, quietLog)
	defer c.Close()
	cs, err := c.RecoverCursors("")
	if err != nil {
		h.t.Fatalf("RecoverCursors: %v", err)
	}
	return indexCursorsByPath(cs)[path]
}

func fileSize(t *testing.T, path string) int64 {
	t.Helper()
	fi, err := os.Stat(path)
	if err != nil {
		t.Fatalf("stat %s: %v", path, err)
	}
	return fi.Size()
}

// ---------------------------------------------------------------------------
// Boot with the store down
// ---------------------------------------------------------------------------

func TestBootWithNoStoreBuildsNoTailer(t *testing.T) {
	// Arrange — a transcript on disk and nothing listening on the socket.
	h := newStoreHarness(t)
	root, _ := writeHistory(t, 5)
	s := h.sidecarOver(root)

	// Act
	s.dial()

	// Assert — no watcher exists, so not one file could have been read.
	if len(s.watchers) != 0 {
		t.Fatalf("built %d watcher(s) with no store connection; a tailer must never exist without a recovered cursor", len(s.watchers))
	}
}

func TestBootWithNoStoreLeavesTheLinkDown(t *testing.T) {
	// Arrange
	h := newStoreHarness(t)
	root, _ := writeHistory(t, 5)
	s := h.sidecarOver(root)

	// Act
	s.dial()

	// Assert — a failed dial is an outage, never a cold start.
	if s.link != linkDown {
		t.Fatalf("link = %v, want linkDown while the store is unreachable", s.link)
	}
}

func TestFailedDialWarnsBecauseTheFilePlaneStops(t *testing.T) {
	// Arrange — "reading no files" is an ingestion outage, not a retry note.
	var sink bytes.Buffer
	h := newStoreHarness(t)
	root, _ := writeHistory(t, 5)
	s := newSidecar(h.sock, []string{root}, t.TempDir(),
		logging.New(io.Discard, &sink).With(logging.Context{Component: "test"}))
	t.Cleanup(func() { s.store.Close() })

	// Act
	s.dial()

	// Assert
	if got := recordLevelsFor(t, &sink, "dial"); len(got) != 1 || got[0] != "warn" {
		t.Fatalf("dial-failure levels = %v, want exactly one warn", got)
	}
}

// recordLevelsFor returns the level of every persisted record with operation.
func recordLevelsFor(t *testing.T, sink *bytes.Buffer, operation string) []string {
	t.Helper()
	var out []string
	for _, line := range strings.Split(strings.TrimSpace(sink.String()), "\n") {
		if line == "" {
			continue
		}
		var record struct {
			Level     string `json:"level"`
			Operation string `json:"operation"`
		}
		if err := json.Unmarshal([]byte(line), &record); err != nil {
			t.Fatalf("persisted record is not JSON: %v", err)
		}
		if record.Operation == operation {
			out = append(out, record.Level)
		}
	}
	return out
}

func TestBootWithNoStoreRecoversNoCursors(t *testing.T) {
	// Arrange
	h := newStoreHarness(t)
	root, _ := writeHistory(t, 5)
	s := h.sidecarOver(root)

	// Act
	s.dial()

	// Assert — nil cursors is what makes rescan refuse to build a tailer.
	if s.cursors != nil {
		t.Fatalf("cursors = %v, want nil with no store connection", s.cursors)
	}
}

func TestRescanDoesNotTreatTaskArtifactAsOpenLifecycle(t *testing.T) {
	root := t.TempDir()
	path := filepath.Join(root, "projects", "-p", "s1", "subagents", "agent-a1.jsonl")
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("mkdir artifact dir: %v", err)
	}
	if err := os.WriteFile(path, nil, 0o644); err != nil {
		t.Fatalf("write artifact: %v", err)
	}
	s := newSidecar(filepath.Join(t.TempDir(), "unused.sock"), []string{root}, t.TempDir(), quietLog)
	s.link = linkUp
	s.cursors = map[string]*corev1.CursorState{}

	s.rescan()

	if _, ok := s.watchers[path]; !ok {
		t.Fatal("rescan did not build a tailer for discovered artifact")
	}
	if s.tracker.IsOpen("s1", "a1") {
		t.Fatal("artifact presence opened task a1; liveness must come only from persisted lifecycle state")
	}
}

// writeSpool creates one /tmp-shaped spool file and returns the spool root and
// the file's path. The directory embeds a session-SHAPED segment on purpose:
// the point is that it is NOT read.
func writeSpool(t *testing.T, taskID string) (string, string) {
	t.Helper()
	spoolRoot := t.TempDir()
	path := filepath.Join(spoolRoot, "claude-501", "slug", "a4f52dc5-runtime-id", "tasks", taskID+".output")
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("mkdir spool dir: %v", err)
	}
	if err := os.WriteFile(path, []byte("hello\n"), 0o644); err != nil {
		t.Fatalf("write spool: %v", err)
	}
	return spoolRoot, path
}

func TestRescanBuildsNoTailerForAnUnattributedSpool(t *testing.T) {
	// Arrange — a spool exists but nothing has announced which session
	// launched it.
	spoolRoot, path := writeSpool(t, "b1pi0nmip")
	s := newSidecar(filepath.Join(t.TempDir(), "unused.sock"), nil, spoolRoot, quietLog)
	s.link = linkUp
	s.cursors = map[string]*corev1.CursorState{}

	// Act
	s.rescan()

	// Assert — held, not read. Reading it would require inventing an owner.
	if _, ok := s.watchers[path]; ok {
		t.Fatal("rescan tailed a spool with no known launching session")
	}
	if _, ok := s.held.active[normalizeOwnerOutputPath(path)]; !ok {
		t.Fatal("unattributed spool was skipped without being recorded as held")
	}
}

func TestRescanTailsASpoolOnceItsLaunchingSessionIsKnown(t *testing.T) {
	// Arrange — same spool, but the launch has been attributed.
	spoolRoot, path := writeSpool(t, "b1pi0nmip")
	s := newSidecar(filepath.Join(t.TempDir(), "unused.sock"), nil, spoolRoot, quietLog)
	s.link = linkUp
	s.cursors = map[string]*corev1.CursorState{}
	s.noteTaskOwner("b1pi0nmip", "9b6a4f2d-transcript-id", "", OwnerSourceLiveLaunch)

	// Act
	s.rescan()

	// Assert — tailed, and stamped with the TRANSCRIPT id rather than the
	// runtime id sitting in its own path.
	w, ok := s.watchers[path]
	if !ok {
		t.Fatal("rescan did not tail an attributed spool")
	}
	if w.target.SessionID != "" {
		t.Fatalf("target session = %q, want empty — identity must not come from the path", w.target.SessionID)
	}
	if w.sessionID != "9b6a4f2d-transcript-id" {
		t.Fatalf("resolved watched session = %q, want transcript owner", w.sessionID)
	}
}

// launchLine is a transcript line whose tool result reports a background bash
// launch — the record that names which session owns a task, and therefore its
// spool. Shape taken from real transcripts: the task id rides `toolUseResult`,
// and the spool path is NOT in it (which is why attribution goes by task id).
func launchLine(t *testing.T, taskID string) []byte {
	t.Helper()
	raw, err := json.Marshal(map[string]any{
		"type":      "user",
		"uuid":      "launch-uuid-1",
		"sessionId": backfillSession,
		"timestamp": "2026-07-25T12:00:00.000Z",
		"message": map[string]any{
			"role": "user",
			"content": []any{map[string]any{
				"tool_use_id": "toolu_launch1",
				"type":        "tool_result",
				"content":     "Command running in background with ID: " + taskID,
			}},
		},
		"toolUseResult": map[string]any{
			"stdout": "", "stderr": "", "interrupted": false,
			"isImage": false, "noOutputExpected": false,
			"backgroundTaskId": taskID,
		},
	})
	if err != nil {
		t.Fatalf("marshal launch line: %v", err)
	}
	return append(raw, '\n')
}

// The whole point of the change, end to end: a spool is attributed from the
// TRANSCRIPT that launched it, even though its own path spells a different,
// runtime-minted session id. Before this, the path segment was read as the
// owner and the same task landed under two session ids.
func TestSpoolIsAttributedFromTheLaunchingTranscriptNotItsPath(t *testing.T) {
	// Arrange — a transcript carrying the launch, plus that task's spool under
	// a directory named with a DIFFERENT (runtime) session id.
	h := newStoreHarness(t)
	h.start()
	root := t.TempDir()
	dir := filepath.Join(root, "projects", "-w")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("mkdir transcript dir: %v", err)
	}
	transcript := filepath.Join(dir, backfillSession+".jsonl")
	if err := os.WriteFile(transcript, launchLine(t, "b1pi0nmip"), 0o644); err != nil {
		t.Fatalf("write transcript: %v", err)
	}
	spoolRoot, spoolPath := writeSpool(t, "b1pi0nmip")
	s := newSidecar(h.sock, []string{root}, spoolRoot, quietLog)
	t.Cleanup(func() { s.store.Close() })
	if err := s.establish(); err != nil {
		t.Fatalf("establish: %v", err)
	}
	// The first rescan runs inside establish, before anything is read, so the
	// spool starts out unattributed.
	if _, ok := s.watchers[spoolPath]; ok {
		t.Fatal("spool was tailed before its launching session was known")
	}

	// Act — read the transcript (which announces the launch), then rescan.
	s.pollAll()
	s.rescan()

	// Assert — now tailed, and attributed to the TRANSCRIPT's session. The
	// runtime id in its own path never becomes an identity.
	if _, ok := s.watchers[spoolPath]; !ok {
		t.Fatal("spool was not tailed after its launch was observed")
	}
	if got := s.owners["b1pi0nmip"]; got != backfillSession {
		t.Fatalf("owner = %q, want the transcript session %q", got, backfillSession)
	}
	if _, held := s.held.active[normalizeOwnerOutputPath(spoolPath)]; held {
		t.Fatal("spool is still recorded as held after being attributed")
	}
}

func TestStoreUpLateRecoversCursorsOnTheFirstConnection(t *testing.T) {
	// Arrange — the store already holds an end-of-file cursor from an earlier
	// sidecar, and is then taken down. A fresh sidecar boots against the dead
	// socket: this is the exact boot race that used to log "cursor recovery
	// failed (starting cold)".
	h := newStoreHarness(t)
	h.start()
	root, path := writeHistory(t, 5)
	seed := h.sidecarOver(root)
	if err := seed.establish(); err != nil {
		t.Fatalf("seed establish: %v", err)
	}
	seed.pollAll()
	seed.store.Close()
	h.stop()

	s := h.sidecarOver(root)
	s.dial() // fails: nothing is listening

	// Act — the store finally comes up and the next dial lands.
	h.start()
	s.dial()

	// Assert — the first established connection recovered the cursor.
	if s.link != linkUp {
		t.Fatalf("link = %v after the store came up, want linkUp", s.link)
	}
	if got := s.cursors[path].GetOffset(); got != fileSize(t, path) {
		t.Fatalf("recovered cursor offset = %d, want the full file size %d", got, fileSize(t, path))
	}
}

func TestStoreUpLateNeverReReadsTheFileFromZero(t *testing.T) {
	// Arrange — as above: a cursor at EOF in the store, a sidecar that boots
	// while the store is down.
	h := newStoreHarness(t)
	h.start()
	root, path := writeHistory(t, 5)
	seed := h.sidecarOver(root)
	if err := seed.establish(); err != nil {
		t.Fatalf("seed establish: %v", err)
	}
	seed.pollAll()
	seed.store.Close()
	h.stop()

	s := h.sidecarOver(root)
	s.dial()
	h.start()
	s.dial()

	// Act — poll the transcript the late connection discovered.
	res, err := s.watchers[path].tailer.Poll()
	if err != nil {
		t.Fatalf("poll: %v", err)
	}

	// Assert — the history is NOT re-ingested. Re-reading it from 0 is the
	// incident this design exists to prevent.
	if len(res.Events) != 0 {
		t.Fatalf("re-read %d event(s) of already-cursored history; want none", len(res.Events))
	}
}

// ---------------------------------------------------------------------------
// Mid-run store bounce
// ---------------------------------------------------------------------------

// bouncedSidecar brings a sidecar up over a 5-line transcript, drains it into
// the store, then kills the store and drives one more poll so the failed write
// tears the link down. It returns the sidecar, the transcript path, and the
// offset the store durably acked (the end of the first five lines).
func bouncedSidecar(t *testing.T, h *storeHarness) (*sidecar, string, int64) {
	t.Helper()
	h.start()
	root, path := writeHistory(t, 5)
	s := h.sidecarOver(root)
	if err := s.establish(); err != nil {
		t.Fatalf("establish: %v", err)
	}
	s.pollAll()
	acked := fileSize(t, path)

	h.stop()
	appendHistory(t, path, 5, 2)
	s.pollAll() // the read succeeds, the write cannot land
	return s, path, acked
}

func TestStoreBounceTearsTheLinkDown(t *testing.T) {
	// Arrange / Act
	h := newStoreHarness(t)
	s, _, _ := bouncedSidecar(t, h)

	// Assert — a write that cannot reach a dead store is a lost link, not just
	// a dropped batch.
	if s.link != linkDown {
		t.Fatalf("link = %v after the store died, want linkDown", s.link)
	}
}

func TestStoreBounceDropsTheRecoveredCursors(t *testing.T) {
	// Arrange / Act
	h := newStoreHarness(t)
	s, _, _ := bouncedSidecar(t, h)

	// Assert — cursors are connection-scoped, so they do not outlive the
	// connection that produced them.
	if s.cursors != nil {
		t.Fatalf("cursors = %v after the link was lost, want nil", s.cursors)
	}
}

func TestReadsPauseWhileTheLinkIsDown(t *testing.T) {
	// Arrange — a sidecar whose link just died, with unread bytes waiting.
	h := newStoreHarness(t)
	s, path, _ := bouncedSidecar(t, h)
	appendHistory(t, path, 7, 3)

	// Act — the loop's gate is asked to run a poll. pollAll asserts the link is
	// up, so if the gate ever stopped holding, this panics.
	s.whenUp(s.pollAll)

	// Assert — the poll never ran.
	if s.link != linkDown {
		t.Fatalf("link = %v, want the sidecar still down and still reading nothing", s.link)
	}
}

func TestReconnectReRecoversCursors(t *testing.T) {
	// Arrange — a bounced sidecar and a store that comes back.
	h := newStoreHarness(t)
	s, path, _ := bouncedSidecar(t, h)
	h.start()

	// Act
	s.dial()

	// Assert — recovery is the first act of the RECONNECT too, not just of boot.
	if s.link != linkUp {
		t.Fatalf("link = %v after the store returned, want linkUp", s.link)
	}
	if s.cursors[path] == nil {
		t.Fatal("reconnect recovered no cursor for the watched transcript")
	}
}

func TestReconnectResumesFromTheCommittedCursorNotZero(t *testing.T) {
	// Arrange — 5 lines are durably cursored; 2 more were read but their write
	// died with the store, so they were never committed.
	h := newStoreHarness(t)
	s, path, acked := bouncedSidecar(t, h)
	h.start()

	// Act
	s.dial()

	// Assert — the reconnect resumes at the last ACKED offset. Not 0 (that is
	// the cold start), and not the uncommitted 7-line offset (that would lose
	// the two events whose write never landed).
	if got := s.cursors[path].GetOffset(); got != acked {
		t.Fatalf("resumed cursor offset = %d, want the last acked offset %d", got, acked)
	}
}

// ---------------------------------------------------------------------------
// The honest cold case: a connected store that truly has no cursor
// ---------------------------------------------------------------------------

func TestConnectedStoreWithNoCursorReadsTheWholeFile(t *testing.T) {
	// Arrange — a live store that genuinely holds no cursor for this file. This
	// is the ONLY surviving meaning of "cold", and it is honest.
	h := newStoreHarness(t)
	h.start()
	root, path := writeHistory(t, 5)
	s := h.sidecarOver(root)
	if err := s.establish(); err != nil {
		t.Fatalf("establish: %v", err)
	}

	// Act
	s.pollAll()

	// Assert — the whole file was ingested and cursored.
	if got := h.cursorFor(path).GetOffset(); got != fileSize(t, path) {
		t.Fatalf("cursor offset = %d, want the full file size %d", got, fileSize(t, path))
	}
}

func TestConnectedStoreWithNoCursorReadsFromZeroExactlyOnce(t *testing.T) {
	// Arrange — the honest cold read has already happened.
	h := newStoreHarness(t)
	h.start()
	root, path := writeHistory(t, 5)
	s := h.sidecarOver(root)
	if err := s.establish(); err != nil {
		t.Fatalf("establish: %v", err)
	}
	s.pollAll()

	// Act — poll the same unchanged file again.
	res, err := s.watchers[path].tailer.Poll()
	if err != nil {
		t.Fatalf("poll: %v", err)
	}

	// Assert — once, not on every pass.
	if len(res.Events) != 0 {
		t.Fatalf("second poll produced %d event(s); the cold read must happen exactly once", len(res.Events))
	}
}

// ---------------------------------------------------------------------------
// The outage is surfaced, not just logged
// ---------------------------------------------------------------------------

func TestOutageIsSurfacedAsDegradedStateOnRecovery(t *testing.T) {
	// Arrange — a sidecar that fails a dial (the outage), then a store that
	// comes up with a subscriber already listening on the watched session.
	h := newStoreHarness(t)
	root, _ := writeHistory(t, 5)
	s := h.sidecarOver(root)
	s.dial() // fails; the degraded window opens

	h.start()
	events := subscribeEvents(t, h.sock, backfillSession)

	// Act — the link comes back, and reports the window it just spent down.
	s.dial()

	// Assert — the OPENING half comes first, naming the component.
	ds := awaitDegraded(t, events)
	if ds.GetComponent() != degradedComponent {
		t.Fatalf("component = %q, want %q", ds.GetComponent(), degradedComponent)
	}
	if ds.GetRecovered() {
		t.Fatal("first report is already recovered; the window must be OPENED before it is closed")
	}
}

func TestRecoveryClosesTheDegradedWindowItOpened(t *testing.T) {
	// Arrange — a consumer only resolves a runtime fault it has an open window
	// for, so the closing half is worthless without the opening one preceding
	// it on the same connection.
	h := newStoreHarness(t)
	root, _ := writeHistory(t, 5)
	s := h.sidecarOver(root)
	s.dial() // fails; the outage happens

	h.start()
	events := subscribeEvents(t, h.sock, backfillSession)

	// Act
	s.dial()

	// Assert
	if opened := awaitDegraded(t, events); opened.GetRecovered() {
		t.Fatal("first report is recovered; want the opening half")
	}
	closed := awaitDegraded(t, events)
	if !closed.GetRecovered() {
		t.Fatal("second report is not recovered; the window was never closed")
	}
}

func TestNoDegradedReportWhenTheLinkComesUpFirstTry(t *testing.T) {
	// Arrange — a store that was already up. There is no outage to report.
	h := newStoreHarness(t)
	h.start()
	root, _ := writeHistory(t, 5)
	s := h.sidecarOver(root)

	// Act
	if err := s.establish(); err != nil {
		t.Fatalf("establish: %v", err)
	}

	// Assert
	if s.dialFailures != 0 {
		t.Fatalf("dialFailures = %d, want 0 on a first-try connection", s.dialFailures)
	}
}

func TestDegradedEventsAreEphemeralAndSynthetic(t *testing.T) {
	// Arrange / Act — an operational notice about the pipe, one per session.
	evs := degradedEvents([]string{"a", "b"}, "store was unreachable", true)

	// Assert — EPHEMERAL keeps it out of the durable conversation history the
	// pipe carries, matching how the shim reports its own degraded windows.
	if len(evs) != 2 {
		t.Fatalf("built %d event(s), want one per session", len(evs))
	}
	for _, ev := range evs {
		if ev.GetClass() != corev1.EventClass_EVENT_CLASS_EPHEMERAL {
			t.Fatalf("class = %v, want EPHEMERAL", ev.GetClass())
		}
		if ev.GetPlane() != corev1.Plane_PLANE_SYNTHETIC {
			t.Fatalf("plane = %v, want SYNTHETIC", ev.GetPlane())
		}
	}
}

func TestWatchedSessionsAreDeduplicated(t *testing.T) {
	// Arrange — two files of the same session, plus one with no session id.
	s := &sidecar{watchers: map[string]*watched{
		"/a": {target: discover.Target{SessionID: "s1"}, sessionID: "s1"},
		"/b": {target: discover.Target{SessionID: "s1"}, sessionID: "s1"},
		"/c": {target: discover.Target{}},
	}}

	// Act
	got := s.watchedSessions()

	// Assert — the report goes to each channel once.
	if len(got) != 1 || got[0] != "s1" {
		t.Fatalf("watchedSessions() = %v, want [s1]", got)
	}
}

// ---------------------------------------------------------------------------
// Invariants and backoff
// ---------------------------------------------------------------------------

func TestRescanWithTheLinkDownFailsHard(t *testing.T) {
	// Arrange — a sidecar that never connected.
	h := newStoreHarness(t)
	root, _ := writeHistory(t, 5)
	s := h.sidecarOver(root)

	// Assert — building a tailer without a recovered cursor is the cold-start
	// bug, so it panics rather than quietly starting at offset 0.
	defer func() {
		if recover() == nil {
			t.Fatal("rescan with the link down did not panic")
		}
	}()

	// Act
	s.rescan()
}

func TestNextBackoffStartsAtTheFloor(t *testing.T) {
	if got := nextBackoff(0); got != dialBackoffMin {
		t.Fatalf("nextBackoff(0) = %v, want %v", got, dialBackoffMin)
	}
}

func TestNextBackoffDoubles(t *testing.T) {
	if got := nextBackoff(dialBackoffMin); got != 2*dialBackoffMin {
		t.Fatalf("nextBackoff(%v) = %v, want %v", dialBackoffMin, got, 2*dialBackoffMin)
	}
}

func TestNextBackoffSaturatesAtTheCeiling(t *testing.T) {
	if got := nextBackoff(dialBackoffMax); got != dialBackoffMax {
		t.Fatalf("nextBackoff(%v) = %v, want the ceiling %v", dialBackoffMax, got, dialBackoffMax)
	}
}

func TestFailedDialsAccumulateWhileDown(t *testing.T) {
	// Arrange — nothing listening.
	h := newStoreHarness(t)
	root, _ := writeHistory(t, 5)
	s := h.sidecarOver(root)

	// Act
	s.dial()
	s.dial()

	// Assert — the outage's cost is counted, for the closing report.
	if s.dialFailures != 2 {
		t.Fatalf("dialFailures = %d, want 2", s.dialFailures)
	}
}

// ---------------------------------------------------------------------------
// The steady-state redial ladder
// ---------------------------------------------------------------------------

// fakeClock is the ladder's injected clock: its schedule is asserted by moving
// time, never by waiting for it.
type fakeClock struct{ t time.Time }

func (c *fakeClock) now() time.Time          { return c.t }
func (c *fakeClock) advance(d time.Duration) { c.t = c.t.Add(d) }

// onFakeClock puts s on a controllable clock and removes the backoff jitter, so
// an armed delay is exactly the rung nextBackoff produced.
func onFakeClock(s *sidecar) *fakeClock {
	c := &fakeClock{t: time.Now()}
	s.now = c.now
	s.jitter = func(d time.Duration) time.Duration { return d }
	s.downSince = c.t
	s.nextDialAt = c.t
	return c
}

func TestLadderKeepsRedialingWhileTheStoreIsDown(t *testing.T) {
	// Arrange — the regression: after the first failed dial the ladder went
	// quiet, and the link stayed down until unrelated work dialed again.
	h := newStoreHarness(t)
	root, _ := writeHistory(t, 5)
	s := h.sidecarOver(root)
	clock := onFakeClock(s)

	// Act — nothing but the ladder runs, and time passes.
	for i := 0; i < 5; i++ {
		s.dialDue()
		clock.advance(dialBackoffMax)
	}

	// Assert — every armed deadline produced another attempt.
	if s.dialFailures != 5 {
		t.Fatalf("dialFailures = %d after five due deadlines, want 5; the ladder stopped redialing", s.dialFailures)
	}
}

func TestLadderReconnectsWithoutAnyReadDemandingIt(t *testing.T) {
	// Arrange — a store that comes back while only the ladder is running.
	h := newStoreHarness(t)
	root, _ := writeHistory(t, 5)
	s := h.sidecarOver(root)
	clock := onFakeClock(s)
	s.dialDue()
	clock.advance(dialBackoffMax)

	// Act
	h.start()
	s.dialDue()

	// Assert
	if s.link != linkUp {
		t.Fatalf("link = %v after the store returned, want linkUp", s.link)
	}
}

func TestLadderWaitsForTheArmedDeadline(t *testing.T) {
	// Arrange — the first failed dial arms dialBackoffMin.
	h := newStoreHarness(t)
	root, _ := writeHistory(t, 5)
	s := h.sidecarOver(root)
	clock := onFakeClock(s)
	s.dialDue()

	// Act — a tick landing before that deadline.
	clock.advance(dialBackoffMin / 2)
	s.dialDue()

	// Assert — backing off means backing off, not dialing on every tick.
	if s.dialFailures != 1 {
		t.Fatalf("dialFailures = %d, want 1; the ladder dialed before its deadline", s.dialFailures)
	}
}

func TestLadderDoesNotDialAlongsideAnInFlightDial(t *testing.T) {
	// Arrange — a demand-driven dial is inside establish and the ladder's
	// deadline has passed underneath it.
	h := newStoreHarness(t)
	root, _ := writeHistory(t, 5)
	s := h.sidecarOver(root)
	onFakeClock(s)
	s.dialing = true

	// Act
	s.dialDue()

	// Assert — one dial at a time, so two cursor recoveries never race.
	if s.dialFailures != 0 {
		t.Fatalf("dialFailures = %d, want 0; the ladder double-dialed", s.dialFailures)
	}
}

func TestDemandDialRunsImmediatelyWhileTheLadderIsBackedOff(t *testing.T) {
	// Arrange — the ladder is waiting out a rung, but a read wants the link now.
	h := newStoreHarness(t)
	root, _ := writeHistory(t, 5)
	s := h.sidecarOver(root)
	clock := onFakeClock(s)
	s.dialDue()
	h.start()
	clock.advance(dialBackoffMin / 2)

	// Act
	s.dial()

	// Assert
	if s.link != linkUp {
		t.Fatalf("link = %v, want linkUp; a demand dial must not wait on the ladder", s.link)
	}
}

func TestOutageDurationMeasuresUnreachabilityNotTimeUntilDemand(t *testing.T) {
	// Arrange — a store away for exactly one backoff rung. Before the ladder
	// existed this number reported the wait until some read wanted a file,
	// which made a seconds-long store bounce read as a 15-minute outage.
	var sink bytes.Buffer
	h := newStoreHarness(t)
	root, _ := writeHistory(t, 5)
	s := newSidecar(h.sock, []string{root}, t.TempDir(),
		logging.New(io.Discard, &sink).With(logging.Context{Component: "test"}))
	t.Cleanup(func() { s.store.Close() })
	clock := onFakeClock(s)
	s.dialDue()
	clock.advance(dialBackoffMin)
	h.start()

	// Act
	s.dialDue()

	// Assert
	if s.link != linkUp {
		t.Fatalf("link = %v, want linkUp", s.link)
	}
	want := fmt.Sprintf("store unreachable for %dms", dialBackoffMin.Milliseconds())
	if !strings.Contains(sink.String(), want) {
		t.Fatalf("recovery record does not quote the real outage %q:\n%s", want, sink.String())
	}
}

func TestDegradedWindowOpensBeforeItCloses(t *testing.T) {
	// Arrange / Act — the pair one session gets for one outage.
	evs := degradedWindowEvents([]string{"s1"}, "store was unreachable")

	// Assert — the consumer opens a runtime fault on the first and resolves it
	// on the second; the closing half alone resolves a window that is not open.
	if len(evs) != 2 {
		t.Fatalf("built %d event(s) for one session, want the opening and closing pair", len(evs))
	}
	if evs[0].GetDegradedState().GetRecovered() {
		t.Fatal("the first half is recovered; the window must open first")
	}
	if !evs[1].GetDegradedState().GetRecovered() {
		t.Fatal("the second half is not recovered; the window never closes")
	}
}

func TestDegradedWindowPairsEverySession(t *testing.T) {
	// Arrange / Act
	evs := degradedWindowEvents([]string{"a", "b"}, "store was unreachable")

	// Assert — each session's channel carries its own complete window.
	if len(evs) != 4 {
		t.Fatalf("built %d event(s) for two sessions, want a pair each", len(evs))
	}
}

func TestBootDialIsDueImmediately(t *testing.T) {
	// Arrange — boot is not a case: it is a link that is not up yet, owed its
	// first attempt at once, with the same ladder behind it.
	h := newStoreHarness(t)
	root, _ := writeHistory(t, 5)

	// Act
	s := h.sidecarOver(root)

	// Assert
	if s.nextDialAt.After(time.Now()) {
		t.Fatalf("first dial is due at %v, want immediately", s.nextDialAt)
	}
}

func TestJitterStaysWithinItsFraction(t *testing.T) {
	// Arrange / Act / Assert — the spread never collapses a rung to nothing nor
	// stretches it past itself.
	lo := time.Duration(float64(dialBackoffMax) * (1 - dialJitterFraction))
	hi := time.Duration(float64(dialBackoffMax) * (1 + dialJitterFraction))
	for i := 0; i < 100; i++ {
		if got := jitterBackoff(dialBackoffMax); got < lo || got > hi {
			t.Fatalf("jitterBackoff(%v) = %v, want within [%v,%v]", dialBackoffMax, got, lo, hi)
		}
	}
}

func TestJitterLeavesAnImmediateRedialImmediate(t *testing.T) {
	if got := jitterBackoff(0); got != 0 {
		t.Fatalf("jitterBackoff(0) = %v, want 0", got)
	}
}
