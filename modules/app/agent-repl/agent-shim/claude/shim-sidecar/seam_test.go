package main

import (
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

func (l *lostCapable) LostTerminal(taskID, runActivityID, ownerAgentID, reason string, catchup bool) []*storev1.StoreEntry {
	l.calls = append(l.calls, taskID)
	l.reason = reason
	return []*storev1.StoreEntry{{WriteId: "lost:" + taskID, UpsertKey: "bash:" + runActivityID}}
}

// observerCapable is a handler whose converter accepts spawn observations.
type observerCapable struct {
	observer func(taskID, toolUseID, agentID, outputPath string, backgrounded bool, workspaceDir, workspaceID, claudeSessionID string)
}

func (o *observerCapable) Handle([]tail.Frame, *tail.Context) []*storev1.StoreEntry { return nil }

func (o *observerCapable) SetTaskObserver(fn func(taskID, toolUseID, agentID, outputPath string, backgrounded bool, workspaceDir, workspaceID, claudeSessionID string)) {
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

	// Assert: the run stays open downstream, and the log says exactly that —
	// as its own operation at error, carrying the reason it was concluded on.
	if len(got) != 0 {
		t.Fatalf("entries = %d, want none", len(got))
	}
	rec := h.requireOnce(t, "lost-terminal-unsupported", "error")
	if got := ctxString(t, rec, "reason"); got != string(stale.ReasonWentSilent) {
		t.Fatalf("reason = %q, want the conclusion the sweep reached", got)
	}
	if got := ctxString(t, rec, "task_id"); got != "b1" {
		t.Fatalf("task_id = %q, want the run left open", got)
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
	rec := h.requireOnce(t, "lost-terminal-unwatched", "warn")
	if got := ctxString(t, rec, "reason"); got != string(stale.ReasonFileVanished) {
		t.Fatalf("reason = %q, want the conclusion the sweep reached", got)
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
	handler.observer("b1", "call-1", "", "", false, "/workspace", "workspace-id", "session-1")

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
	h.requireOnce(t, "plumb-observer", "error")
}

func TestLostSweepArmsFromSilence(t *testing.T) {
	// Arrange: a claimed spool that stops growing.
	h := newHarness(t, &fakeStore{})
	spool := h.spoolFile(t, "b1", "hello\n")
	h.sc.TaskSpawned("b1", "call-1", "", "", false, "/workspace", "workspace-id", "session-1")
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	handler := &lostCapable{}
	h.sc.watchers[spool].tailer = tail.New(spool, tail.RawTextCodec{}, handler, &tail.Context{Path: spool}, h.sc.log)
	// The reader has seen every byte the spool holds: only then is it silent.
	if _, err := h.sc.watchers[spool].tailer.Poll(); err != nil {
		t.Fatalf("Poll: %v", err)
	}

	// Act.
	h.advance(stale.DefaultShellSilence)
	h.sc.sweep()

	// Assert.
	if len(handler.calls) != 1 {
		t.Fatalf("terminals minted = %d, want one for the silent run", len(handler.calls))
	}
}

func TestALostSweepNeverConcludesASpoolTheReaderHasNotSeen(t *testing.T) {
	// Arrange: a claimed spool whose bytes the reader has not polled yet --
	// they may be its own terminator.
	h := newHarness(t, &fakeStore{})
	spool := h.spoolFile(t, "b1", "hello\nEXIT=0\n")
	h.sc.TaskSpawned("b1", "call-1", "", "", false, "/workspace", "workspace-id", "session-1")
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	handler := &lostCapable{}
	h.sc.watchers[spool].tailer = tail.New(spool, tail.RawTextCodec{}, handler, &tail.Context{Path: spool}, h.sc.log)

	// Act.
	h.advance(stale.DefaultShellSilence)
	h.sc.sweep()

	// Assert.
	if len(handler.calls) != 0 {
		t.Fatalf("terminals minted = %d, want none for a spool with unseen bytes", len(handler.calls))
	}
	if !h.sc.tracker.Open(spool) {
		t.Fatal("the unseen spool's run stopped being tracked")
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
	h.requireNone(t, "plumb-observer", "error")
}

func TestAResidueSpoolConcludedLostNeedsNoTerminal(t *testing.T) {
	// Arrange. A residue spool's task-id prefix failed classification, so it is
	// never read, never watched, and NO unit was opened for it. Concluding it
	// LOST is a fair statement about the file; demanding a terminal for it —
	// or calling it an unwatched run — reports a hole that does not exist,
	// because there is no run row downstream holding anything open.
	h := newHarness(t, &fakeStore{})
	spool := h.spoolFile(t, "z0uncla551f1able", "bytes nobody can classify\n")
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Act.
	entries := h.sc.lostEntries([]stale.Lost{{
		Work:   stale.Work{Path: spool, TaskID: "z0uncla551f1able", Kind: tail.KindResidueSpool},
		Reason: stale.ReasonSweptUp,
	}})

	// Assert.
	if len(entries) != 0 {
		t.Fatalf("entries = %d, want 0: a residue spool names no run to settle", len(entries))
	}
	h.requireNone(t, "lost-terminal-unsupported", "error")
	h.requireNone(t, "lost-terminal-unwatched", "warn")
	rec := h.requireOnce(t, "lost-terminal-residue", "info")
	if got := ctxString(t, rec, "reason"); got != string(stale.ReasonSweptUp) {
		t.Fatalf("reason = %q, want the conclusion the sweep reached", got)
	}
}

func TestLostForAConcludedRunIsRefusedLoudly(t *testing.T) {
	// Arrange: the run's transcript already settled it, yet a sweep concluded it.
	h := newHarness(t, &fakeStore{})
	spool := h.spoolFile(t, "a1refused", "work\n")
	handler := &lostCapable{}
	watchWith(h, spool, handler)
	h.sc.concluded["a1refused"] = struct{}{}

	// Act.
	got := h.sc.lostEntries([]stale.Lost{{
		Work:   stale.Work{Path: spool, TaskID: "a1refused", RunActivityID: "call-1", Kind: tail.KindShellSpool},
		Reason: stale.ReasonWentSilent,
	}})

	// Assert.
	if len(got) != 0 || len(handler.calls) != 0 {
		t.Fatalf("entries=%d calls=%v, want no LOST terminal over a concluded run", len(got), handler.calls)
	}
	rec := h.requireOnce(t, "lost-terminal-refused", "error")
	if got := ctxString(t, rec, "task_id"); got != "a1refused" {
		t.Fatalf("task_id = %q, want the concluded run", got)
	}
}

// stopCapable is a transcript converter that reports task stops.
type stopCapable struct{ stopped func(taskID string) }

func (s *stopCapable) Handle([]tail.Frame, *tail.Context) []*storev1.StoreEntry { return nil }
func (s *stopCapable) SetTaskStopObserver(fn func(taskID string))               { s.stopped = fn }

// conclusionCapable is a transcript converter that reports run conclusions.
type conclusionCapable struct{ concluded func(taskID string) }

func (c *conclusionCapable) Handle([]tail.Frame, *tail.Context) []*storev1.StoreEntry { return nil }
func (c *conclusionCapable) SetTaskConclusionObserver(fn func(taskID string))         { c.concluded = fn }

// shellConclusionCapable is a transcript converter that reports shell conclusions.
type shellConclusionCapable struct {
	concluded func(taskID, status string, atMs int64)
}

func (c *shellConclusionCapable) Handle([]tail.Frame, *tail.Context) []*storev1.StoreEntry {
	return nil
}
func (c *shellConclusionCapable) SetShellConclusionObserver(fn func(taskID, status string, atMs int64)) {
	c.concluded = fn
}

// TestPlumbTranscriptFactStatesEachOutcome pins the one helper every
// transcript-fact plumbing goes through: an adopted fact is recorded verbose, a
// spool declining one is ordinary (verbose), and a transcript declining one is
// a defect stated once at error.
func TestPlumbTranscriptFactStatesEachOutcome(t *testing.T) {
	fact := transcriptFact{
		operation: "plumb-test-fact", noun: "test facts", onlyWhy: "only a transcript carries them",
		method: "SetTestObserver", consequence: "nothing downstream learns them",
	}
	cases := []struct {
		name      string
		kind      tail.Kind
		adopted   bool
		wantLevel string
		verbose   bool
	}{
		{"a transcript that adopts it", tail.KindSessionTranscript, true, "", true},
		{"a transcript that declines it", tail.KindAgentTranscript, false, "error", false},
		{"a spool that declines it", tail.KindShellSpool, false, "", true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t, &fakeStore{})

			// Act.
			h.sc.plumbTranscriptFact(tc.kind, fact, func() bool { return tc.adopted }, h.sc.log)

			// Assert.
			got := h.ops(t, fact.operation)
			if len(got) != 1 {
				t.Fatalf("records for %q = %d, want exactly one", fact.operation, len(got))
			}
			if tc.wantLevel != "" && got[0].Level != tc.wantLevel {
				t.Fatalf("level = %q, want %q", got[0].Level, tc.wantLevel)
			}
			if tc.verbose != (got[0].Verbosity == "verbose") {
				t.Fatalf("verbosity = %q, want verbose=%t", got[0].Verbosity, tc.verbose)
			}
		})
	}
}

// TestTranscriptFactPlumbingSharesOneShape runs every transcript-fact plumbing
// through the same three checks, so a caller hand-rolling its own divergent
// plumbing fails here: each adopts its fact on a capable converter, and each
// states a transcript converter lacking the method once at error under its own
// operation.
func TestTranscriptFactPlumbingSharesOneShape(t *testing.T) {
	cases := []struct {
		name      string
		operation string
		plumb     func(s *sidecar, kind tail.Kind, built tail.Handler)
		capable   func() (tail.Handler, func() bool)
	}{
		{
			"task stops", "plumb-task-stop",
			func(s *sidecar, kind tail.Kind, built tail.Handler) { s.plumbTaskStops(kind, built, s.log) },
			func() (tail.Handler, func() bool) {
				c := &stopCapable{}
				return c, func() bool { return c.stopped != nil }
			},
		},
		{
			"run conclusions", "plumb-task-conclusion",
			func(s *sidecar, kind tail.Kind, built tail.Handler) { s.plumbTaskConclusions(kind, built, s.log) },
			func() (tail.Handler, func() bool) {
				c := &conclusionCapable{}
				return c, func() bool { return c.concluded != nil }
			},
		},
		{
			"shell conclusions", "plumb-shell-conclusion",
			func(s *sidecar, kind tail.Kind, built tail.Handler) { s.plumbShellConclusions(kind, built, s.log) },
			func() (tail.Handler, func() bool) {
				c := &shellConclusionCapable{}
				return c, func() bool { return c.concluded != nil }
			},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name+" adopted", func(t *testing.T) {
			// Arrange.
			h := newHarness(t, &fakeStore{})
			built, adopted := tc.capable()

			// Act.
			tc.plumb(h.sc, tail.KindSessionTranscript, built)

			// Assert.
			if !adopted() {
				t.Fatal("the converter was never handed the reader's callback")
			}
			h.requireNone(t, tc.operation, "error")
		})
		t.Run(tc.name+" missing on a transcript", func(t *testing.T) {
			// Arrange.
			h := newHarness(t, &fakeStore{})

			// Act.
			tc.plumb(h.sc, tail.KindSessionTranscript, &lostCapable{})

			// Assert.
			h.requireOnce(t, tc.operation, "error")
		})
	}
}
