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
	"net"
	"os"
	"os/exec"
	"path/filepath"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/shim-claude-sidecar/internal/storeclient"
)

func quietLog(string, ...any) {}

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
	c := storeclient.New(h.sock, nil)
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
