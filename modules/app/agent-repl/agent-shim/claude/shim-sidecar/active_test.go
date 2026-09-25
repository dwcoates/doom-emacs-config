package main

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	sharedlogging "agentrepl/logging"
	storev1 "agentrepl/proto/store/v1"
)

// active_test.go — ONLY FILES THAT BELONG TO AN ACTIVE WORKSPACE ARE WATCHED.
//
// A workspace is active while its shim holds the workspace lock; the harness
// holds a REAL flock for a session it activates (support_test.go), and releasing
// it is what a shim's death looks like. One poll tick is discoverChanged (which
// asks which workspaces are active first) followed by pollAll.

// tick runs one poll tick exactly as Run does.
func (h *harness) tick() {
	h.sc.discoverChanged()
	h.sc.pollAll()
}

// closedWorkspace records session as a workspace's conversation whose shim is
// NOT running: the identity record is there, the lock is not held.
func (h *harness) closedWorkspace(t *testing.T, session string) string {
	t.Helper()
	key := workspaceKeyOf(session)
	h.writeAgentID(t, key, session)
	return key
}

// subagentTranscript writes a subagent transcript and its required meta under
// session's directory, and returns the transcript's resolved path.
func (h *harness) subagentTranscript(t *testing.T, session, agent string, lines ...string) string {
	t.Helper()
	dir := filepath.Join(h.rootA, "projects", "proj", session, "subagents")
	h.write(t, filepath.Join(dir, "agent-"+agent+".meta.json"),
		`{"agentType":"general-purpose","description":"probe","toolUseId":"toolu_`+agent+`","spawnDepth":1}`)
	path := filepath.Join(dir, "agent-"+agent+".jsonl")
	h.write(t, path, strings.Join(lines, "\n")+"\n")
	return normalized(path)
}

func TestAnInactiveWorkspacesFileIsNotPolled(t *testing.T) {
	// Arrange.
	store := &fakeStore{}
	h := newHarness(t, store)
	h.closedWorkspace(t, "sess-closed")
	path := h.inactiveTranscript(t, "sess-closed", promptLine, assistantLine)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Act.
	h.tick()

	// Assert.
	if _, watched := h.sc.watchers[path]; watched {
		t.Fatal("a closed workspace's transcript is watched")
	}
	if polls := len(h.ops(t, "tailer-poll")); polls != 0 {
		t.Fatalf("the tick polled %d file(s), want none", polls)
	}
}

func TestAnExternalSessionsFileIsNotWatched(t *testing.T) {
	// Arrange: a session no shim ever wrote a record for.
	store := &fakeStore{}
	h := newHarness(t, store)
	path := h.inactiveTranscript(t, "sess-external", promptLine, assistantLine)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Act.
	h.tick()

	// Assert.
	if _, watched := h.sc.watchers[path]; watched {
		t.Fatal("a session run outside agent-repl is watched")
	}
}

func TestAGatedOutFileIsKeptDormant(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	path := h.inactiveTranscript(t, "sess-external", promptLine)

	// Act.
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Assert.
	if _, dormant := h.sc.dormant[path]; !dormant {
		t.Fatalf("the gated-out transcript is not dormant; dormant=%v", h.sc.dormant)
	}
}

func TestAGatedOutFileIsStatedOnceAtDebug(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	h.inactiveTranscript(t, "sess-external", promptLine)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Act: a second full scan offers the same file again.
	h.sc.rescan()

	// Assert.
	h.requireOnce(t, "watch-dormant", "debug")
}

func TestActivationCatchesUpFromTheCursor(t *testing.T) {
	// Arrange: two turns the store already holds a cursor past, written while the
	// workspace was closed.
	h := newHarness(t, &fakeStore{})
	key := h.closedWorkspace(t, "sess-reopened")
	turn := promptLine + "\n" + assistantLine + "\n"
	path := h.inactiveTranscript(t, "sess-reopened", promptLine, assistantLine, promptLine, assistantLine)
	h.store.cursors = []*storev1.CursorState{{FileId: identityOf(t, path), Path: path, Offset: int64(2 * len(turn))}}
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Act: the workspace's shim starts.
	h.hold(t, key)
	h.sc.discoverChanged()

	// Assert: the position came from the store (rewound to the last turn start
	// below it, as every restored cursor is), never from zero.
	w, watched := h.sc.watchers[path]
	if !watched {
		t.Fatal("the reopened workspace's transcript is not watched")
	}
	if got := w.tailer.Offset(); got != int64(len(turn)) {
		t.Fatalf("resumed offset = %d, want %d (the last turn start below the store's cursor)", got, int64(len(turn)))
	}
}

func TestActivationIsStatedOnceAtInfo(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	key := h.closedWorkspace(t, "sess-reopened")
	h.inactiveTranscript(t, "sess-reopened", promptLine)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	before := len(h.opsAt(t, "watched-set", "info"))

	// Act.
	h.hold(t, key)
	h.tick()
	h.tick()

	// Assert.
	if got := len(h.opsAt(t, "watched-set", "info")) - before; got != 1 {
		t.Fatalf("the activation was stated %d time(s) at info, want once", got)
	}
}

func TestASubagentTranscriptOfAnActiveSessionIsWatched(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	h.transcript(t, "sess-live", promptLine)
	path := h.subagentTranscript(t, "sess-live", "a1", promptLine)

	// Act.
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Assert.
	if _, watched := h.sc.watchers[path]; !watched {
		t.Fatal("a live session's subagent transcript is not watched")
	}
}

func TestASubagentTranscriptOfAClosedSessionIsNotWatched(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	h.closedWorkspace(t, "sess-closed")
	path := h.subagentTranscript(t, "sess-closed", "a1", promptLine)

	// Act.
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Assert.
	if _, watched := h.sc.watchers[path]; watched {
		t.Fatal("a closed session's subagent transcript is watched")
	}
}

func TestASpoolClaimedByAnActiveSessionIsWatched(t *testing.T) {
	// Arrange: the harness's session-1 is live.
	h := newHarness(t, &fakeStore{})
	spool := h.spoolFile(t, "b1", "hello\n")
	h.sc.TaskSpawned("b1", "call-1", "", "", false, "/workspace", "workspace-id", "session-1")

	// Act.
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Assert.
	if _, watched := h.sc.watchers[spool]; !watched {
		t.Fatal("a live session's shell spool is not watched")
	}
}

func TestASpoolClaimedByAClosedSessionIsNotWatched(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	h.closedWorkspace(t, "sess-closed")
	spool := h.spoolFile(t, "b2", "hello\n")
	h.sc.TaskSpawned("b2", "call-2", "", "", false, "/workspace", "workspace-id", "sess-closed")

	// Act.
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Assert.
	if _, watched := h.sc.watchers[spool]; watched {
		t.Fatal("a closed session's shell spool is watched")
	}
}

// endedWorkspace arranges a live session's transcript, read to its end, whose
// shim then goes away.
func endedWorkspace(t *testing.T, h *harness) string {
	t.Helper()
	key := h.activate(t, "sess-ending")
	path := h.transcript(t, "sess-ending", promptLine, assistantLine)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.tick()
	h.release(t, key)
	return path
}

func TestAnEndedWorkspacesTranscriptDrainsWithinTheSilenceWindow(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	path := endedWorkspace(t, h)

	// Act: the shim is gone, but the transcript grew inside the agent-silence
	// window, and a vendor process that outlived its shim may still write.
	h.advance(h.sc.tracker.Windows().AgentSilence / 2)
	h.tick()

	// Assert.
	if _, watched := h.sc.watchers[path]; !watched {
		t.Fatal("an ended workspace's transcript was dropped inside its drain window")
	}
}

func TestAnEndedWorkspacesTranscriptIsDroppedOnceSilent(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	path := endedWorkspace(t, h)

	// Act.
	h.advance(h.sc.tracker.Windows().AgentSilence + time.Minute)
	h.tick()

	// Assert.
	if _, watched := h.sc.watchers[path]; watched {
		t.Fatal("an ended workspace's transcript is still watched past its drain window")
	}
	if _, dormant := h.sc.dormant[path]; !dormant {
		t.Fatal("the dropped transcript was not kept dormant for the next activation")
	}
}

func TestAnEndedWorkspaceIsRetiredOnceDrained(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	endedWorkspace(t, h)

	// Act.
	h.advance(h.sc.tracker.Windows().AgentSilence + time.Minute)
	h.tick()

	// Assert.
	if _, draining := h.sc.draining["sess-ending"]; draining {
		t.Fatalf("a drained workspace is still draining; draining=%v", h.sc.draining)
	}
}

func TestAnEndedWorkspacesUnreadBytesKeepItWatched(t *testing.T) {
	// Arrange: bytes land after the last read and are never polled.
	h := newHarness(t, &fakeStore{})
	path := endedWorkspace(t, h)
	h.write(t, path, promptLine+"\n"+assistantLine+"\n"+assistantLine+"\n")
	h.advance(h.sc.tracker.Windows().AgentSilence + time.Minute)
	if err := os.Chtimes(path, h.clock.Add(-2*h.sc.tracker.Windows().AgentSilence), h.clock.Add(-2*h.sc.tracker.Windows().AgentSilence)); err != nil {
		t.Fatalf("aging %s: %v", path, err)
	}

	// Act: only the active-set half of the tick, so nothing reads the bytes.
	h.sc.refreshActive(h.clock)

	// Assert.
	if _, watched := h.sc.watchers[path]; !watched {
		t.Fatal("an ended workspace's transcript with unread bytes was dropped")
	}
}

func TestAnEndedWorkspacesOpenRunKeepsItsSpoolWatched(t *testing.T) {
	// Arrange: a shell run the LOST policy still holds open.
	h := newHarness(t, &fakeStore{})
	spool := h.spoolFile(t, "b1", "still running\n")
	h.sc.TaskSpawned("b1", "call-1", "", "", false, "/workspace", "workspace-id", "session-1")
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.tick()
	h.release(t, workspaceKeyOf("session-1"))

	// Act.
	h.advance(h.sc.tracker.Windows().AgentSilence + time.Minute)
	h.tick()

	// Assert.
	if _, watched := h.sc.watchers[spool]; !watched {
		t.Fatal("an open run's spool was dropped before the LOST policy concluded it")
	}
}

func TestADroppedFileIsStatedAtDebug(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	endedWorkspace(t, h)

	// Act.
	h.advance(h.sc.tracker.Windows().AgentSilence + time.Minute)
	h.tick()

	// Assert.
	h.requireOnce(t, "watch-drop", "debug")
}

func TestAReopenedWorkspaceIsReadAgainFromItsCursor(t *testing.T) {
	// Arrange: a workspace that ended, drained, and was dropped.
	h := newHarness(t, &fakeStore{})
	path := endedWorkspace(t, h)
	committed := h.sc.watchers[path].tailer.Offset()
	h.advance(h.sc.tracker.Windows().AgentSilence + time.Minute)
	h.tick()
	h.store.cursors = []*storev1.CursorState{{FileId: identityOf(t, path), Path: path, Offset: committed}}

	// Act: the workspace is opened again.
	h.activate(t, "sess-ending")
	h.tick()

	// Assert.
	w, watched := h.sc.watchers[path]
	if !watched {
		t.Fatal("the reopened workspace's transcript is not watched again")
	}
	if got := w.tailer.Offset(); got != committed {
		t.Fatalf("the re-watched transcript resumed at %d, want the committed %d", got, committed)
	}
}

func TestAnUnprobeableLockIsTreatedAsActive(t *testing.T) {
	// Arrange: a closed workspace whose lock cannot be asked about.
	h := newHarness(t, &fakeStore{})
	h.closedWorkspace(t, "sess-unknown")
	path := h.inactiveTranscript(t, "sess-unknown", promptLine)
	h.sc.lockHeld = failingProbe(errors.New("probe refused"))

	// Act.
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Assert.
	if _, watched := h.sc.watchers[path]; !watched {
		t.Fatal("a workspace whose lock could not be probed was read as closed")
	}
}

func TestAnUnprobeableLockIsWarnedOnce(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	h.sc.lockHeld = failingProbe(errors.New("probe refused"))
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Act.
	h.tick()
	h.tick()

	// Assert.
	h.requireOnce(t, "workspace-lock-probe", "warn")
}

// failingProbe answers every lock path with err.
func failingProbe(err error) liveProbe {
	return func(paths []string) (map[string]bool, map[string]error) {
		errs := map[string]error{}
		for _, path := range paths {
			errs[path] = err
		}
		return map[string]bool{}, errs
	}
}

// corpus writes `total` transcripts of which the first `active` belong to live
// workspaces, and answers the harness after its first cycle has run.
func corpus(tb testing.TB, total, active int, level sharedlogging.Level) *harness {
	tb.Helper()
	h := newHarnessAtLevel(tb, &fakeStore{}, level)
	for i := 0; i < total; i++ {
		session := fmt.Sprintf("sess-%04d", i)
		if i < active {
			h.transcript(tb, session, promptLine, assistantLine)
			continue
		}
		h.inactiveTranscript(tb, session, promptLine, assistantLine)
	}
	if err := h.sc.beginCycle(); err != nil {
		tb.Fatalf("beginCycle: %v", err)
	}
	h.sc.pollAll()
	return h
}

// TestThePollWalksOnlyActiveFiles is the cost claim, counted rather than timed:
// a tick polls the active workspaces' files and nothing else, however large
// the corpus beside them is.
func TestThePollWalksOnlyActiveFiles(t *testing.T) {
	// Arrange: 200 transcripts, 5 of them live.
	h := corpus(t, 200, 5, sharedlogging.LevelDebug)
	before := len(h.ops(t, "tailer-poll"))

	// Act.
	h.tick()

	// Assert: every poll is stated by its tailer, so the records name the files.
	polled := map[string]bool{}
	for _, rec := range h.ops(t, "tailer-poll")[before:] {
		polled[ctxString(t, rec, "path")] = true
	}
	if len(polled) != 5 {
		t.Fatalf("one tick polled %d file(s) over a 200-file corpus with 5 live, want 5: %v", len(polled), polled)
	}
}

// BenchmarkPollTick measures one poll tick over a synthetic corpus. The
// per-tick cost tracks the ACTIVE file count and not the corpus size: compare
// total=2000/active=20 with total=20/active=20 (about equal) and with
// total=2000/active=2000 (the pre-ruling shape, where every file was watched).
func BenchmarkPollTick(b *testing.B) {
	for _, shape := range []struct{ total, active int }{
		{20, 20},
		{2000, 20},
		{2000, 2000},
	} {
		b.Run(fmt.Sprintf("total=%d/active=%d", shape.total, shape.active), func(b *testing.B) {
			h := corpus(b, shape.total, shape.active, sharedlogging.LevelInfo)
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				h.tick()
			}
		})
	}
}
