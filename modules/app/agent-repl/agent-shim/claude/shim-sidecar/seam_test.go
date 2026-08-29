package main

import (
	"strings"
	"testing"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/discover"
	"agentrepl/shim-claude-sidecar/internal/stale"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// lostCapable is a handler that can spell a LOST run's terminal.
type lostCapable struct {
	calls  []string
	reason string
}

func (l *lostCapable) Handle([]tail.Frame, *tail.Context) []*storev1.StoreEntry { return nil }

func (l *lostCapable) LostTerminal(taskID, runActivityID, ownerAgentID, reason string) []*storev1.StoreEntry {
	l.calls = append(l.calls, taskID)
	l.reason = reason
	return []*storev1.StoreEntry{{WriteId: "lost:" + taskID, UpsertKey: "bash:" + runActivityID}}
}

// observerCapable is a handler whose converter accepts spawn observations.
type observerCapable struct {
	observer func(taskID, toolUseID, agentID, outputPath string)
}

func (o *observerCapable) Handle([]tail.Frame, *tail.Context) []*storev1.StoreEntry { return nil }

func (o *observerCapable) SetTaskObserver(fn func(taskID, toolUseID, agentID, outputPath string)) {
	o.observer = fn
}

// watchWith installs a handler for one path so the seam can be exercised.
func watchWith(h *harness, path string, handler tail.Handler) {
	h.sc.watchers[path] = &watched{
		target: discover.Target{Path: path, Kind: tail.KindShellSpool},
		tailer: tail.New(path, tail.RawTextCodec{}, handler, &tail.Context{Path: path}, h.sc.log),
	}
}

func TestLostConclusionBecomesATerminal(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	spool := h.spoolFile(t, "b1", "hello\n")
	handler := &lostCapable{}
	watchWith(h, spool, handler)

	// Act.
	got := h.sc.lostEntries([]stale.Lost{{
		Work:   stale.Work{Path: spool, TaskID: "b1", RunActivityID: "call-1", Kind: tail.KindShellSpool},
		Reason: stale.ReasonWentSilent,
	}})

	// Assert.
	if len(got) != 1 || handler.reason != string(stale.ReasonWentSilent) {
		t.Fatalf("entries=%d reason=%q, want one terminal naming how we stopped seeing the run", len(got), handler.reason)
	}
}

func TestLostWithoutAConverterIsStatedLoudly(t *testing.T) {
	// Arrange: a handler that cannot spell a terminal.
	h := newHarness(t, &fakeStore{})
	spool := h.spoolFile(t, "b1", "hello\n")
	watchWith(h, spool, &observerCapable{})

	// Act.
	got := h.sc.lostEntries([]stale.Lost{{
		Work:   stale.Work{Path: spool, TaskID: "b1", Kind: tail.KindShellSpool},
		Reason: stale.ReasonWentSilent,
	}})

	// Assert: the run stays open downstream, and the log says exactly that.
	if len(got) != 0 {
		t.Fatalf("entries = %d, want none", len(got))
	}
	if !strings.Contains(h.logText(), "implements no LostTerminal") {
		t.Fatalf("the missing terminal was silent; got %s", h.logText())
	}
}

func TestLostForAnUnwatchedFileIsStated(t *testing.T) {
	// Arrange: the file vanished, so its tailer is gone with it.
	h := newHarness(t, &fakeStore{})

	// Act.
	got := h.sc.lostEntries([]stale.Lost{{
		Work:   stale.Work{Path: "/private/tmp/gone.output", TaskID: "b1"},
		Reason: stale.ReasonFileVanished,
	}})

	// Assert.
	if len(got) != 0 {
		t.Fatalf("entries = %d, want none", len(got))
	}
	if !strings.Contains(h.logText(), "no longer watched") {
		t.Fatalf("the missing terminal was silent; got %s", h.logText())
	}
}

func TestSpawnObservationsArePlumbedIntoTheConverter(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	handler := &observerCapable{}

	// Act.
	h.sc.plumbObserver(tail.KindSessionTranscript, handler, h.sc.log)

	// Assert.
	if handler.observer == nil {
		t.Fatal("the converter was never handed the reader's spawn callback")
	}
}

func TestPlumbedObservationsReachTheOwnerIndex(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	handler := &observerCapable{}
	h.sc.plumbObserver(tail.KindSessionTranscript, handler, h.sc.log)

	// Act.
	handler.observer("b1", "call-1", "", "")

	// Assert.
	if got := h.sc.owners.activityFor("b1"); got != "call-1" {
		t.Fatalf("activity for b1 = %q, want the spawning call", got)
	}
}

func TestAConverterReportingNoSpawnsIsStatedLoudly(t *testing.T) {
	// Arrange: a TRANSCRIPT is the only thing that can teach the reader who owns
	// a spool, because a launch is stated in a tool result and nothing else
	// carries one.
	h := newHarness(t, &fakeStore{})

	// Act.
	h.sc.plumbObserver(tail.KindSessionTranscript, &lostCapable{}, h.sc.log)

	// Assert.
	if !strings.Contains(h.logText(), "implements no SetTaskObserver") {
		t.Fatalf("the missing plumbing was silent; got %s", h.logText())
	}
}

func TestLostSweepArmsFromSilence(t *testing.T) {
	// Arrange: a claimed spool that stops growing.
	h := newHarness(t, &fakeStore{})
	spool := h.spoolFile(t, "b1", "hello\n")
	h.sc.TaskSpawned("b1", "call-1", "", "")
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	handler := &lostCapable{}
	h.sc.watchers[spool].tailer = tail.New(spool, tail.RawTextCodec{}, handler, &tail.Context{Path: spool}, h.sc.log)

	// Act.
	h.advance(stale.DefaultShellSilence)
	h.sc.sweep()

	// Assert.
	if len(handler.calls) != 1 {
		t.Fatalf("terminals minted = %d, want one for the silent run", len(handler.calls))
	}
}

func TestATranscriptIsNeverLostSwept(t *testing.T) {
	// Arrange: a transcript is an agent's own record, and its silence concludes
	// nothing about anything.
	h := newHarness(t, &fakeStore{})
	path := h.transcript(t, "sess-1", promptLine)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Act, Assert.
	if h.sc.tracker.Open(path) {
		t.Fatal("a session transcript was armed for a LOST conclusion")
	}
}

func TestTheVanishedFilesTailerIsDroppedOnceItsTerminalIsStated(t *testing.T) {
	// Arrange: a run whose file is gone, still holding the converter that spells
	// its terminal.
	h := newHarness(t, &fakeStore{})
	spool := h.spoolFile(t, "b1", "hello\n")
	watchWith(h, spool, &lostCapable{})

	// Act.
	h.sc.lostEntries([]stale.Lost{{
		Work:   stale.Work{Path: spool, TaskID: "b1", RunActivityID: "call-1", Kind: tail.KindShellSpool},
		Reason: stale.ReasonFileVanished,
	}})

	// Assert: nothing will ever be read from it again.
	if _, ok := h.sc.watchers[spool]; ok {
		t.Fatal("the vanished file is still being tailed after its terminal was stated")
	}
}

func TestASilentRunKeepsItsTailer(t *testing.T) {
	// Arrange: the file is still there, so anything appended later must land.
	h := newHarness(t, &fakeStore{})
	spool := h.spoolFile(t, "b1", "hello\n")
	watchWith(h, spool, &lostCapable{})

	// Act.
	h.sc.lostEntries([]stale.Lost{{
		Work:   stale.Work{Path: spool, TaskID: "b1", RunActivityID: "call-1", Kind: tail.KindShellSpool},
		Reason: stale.ReasonWentSilent,
	}})

	// Assert.
	if _, ok := h.sc.watchers[spool]; !ok {
		t.Fatal("a run that merely went quiet lost its tailer, so later bytes would be dropped")
	}
}

func TestASpoolConverterReportingNoLaunchesIsNotADefect(t *testing.T) {
	// Arrange. A launch is stated in a TOOL RESULT and a spool carries none, so
	// the shell converter has nothing to report and its silence must not be
	// logged as the error that a transcript's silence genuinely is.
	store := &fakeStore{}
	h := newHarness(t, store)

	// Act.
	h.sc.newHandler(tail.KindShellSpool, h.sc.log)

	// Assert.
	if strings.Contains(h.logText(), "reports no spawn observations (it implements no SetTaskObserver)") {
		t.Fatalf("a spool converter's silence was recorded as a defect: %s", h.logText())
	}
}
