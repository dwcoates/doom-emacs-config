package main

import (
	"context"
	"io"
	"os"
	"path/filepath"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/discover"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

func ownerIndexFor(t *testing.T) (*ownerIndex, *[]string) {
	t.Helper()
	var logs []string
	log := logging.New(sliceWriter{lines: &logs}, io.Discard).With(logging.Context{Component: "owner-test"})
	return newOwnerIndex(log), &logs
}

func spoolTarget(path, taskID string) discover.Target {
	return discover.Target{Path: path, TaskID: taskID}
}

func TestOwnerResolvesByTaskID(t *testing.T) {
	// Arrange.
	index, _ := ownerIndexFor(t)
	index.observe(observation{taskID: "b1", activityID: "call-1", agentID: ""})

	// Act.
	got, ok := index.resolve(spoolTarget("/private/tmp/b1.output", "b1"))

	// Assert.
	if !ok || got.activityID != "call-1" {
		t.Fatalf("resolve = %+v ok=%t, want the spawning call", got, ok)
	}
}

func TestOwnerResolvesByExactOutputPath(t *testing.T) {
	// Arrange: the vendor named this exact file, which needs no id comparison.
	index, _ := ownerIndexFor(t)
	index.observe(observation{taskID: "b1", activityID: "call-1", outputPath: "/private/tmp/b1.output"})

	// Act.
	got, ok := index.resolve(spoolTarget("/private/tmp/b1.output", "b1"))

	// Assert.
	if !ok || got.activityID != "call-1" {
		t.Fatalf("resolve = %+v ok=%t, want the spawning call", got, ok)
	}
}

func TestOwnerIsUnknownUntilASpawnIsObserved(t *testing.T) {
	// Arrange.
	index, _ := ownerIndexFor(t)

	// Act.
	_, ok := index.resolve(spoolTarget("/private/tmp/b1.output", "b1"))

	// Assert.
	if ok {
		t.Fatal("a spool resolved to an owner nobody reported")
	}
}

func TestConflictingSpawnsResolveToNothing(t *testing.T) {
	// Arrange: two calls claim one task.
	index, _ := ownerIndexFor(t)
	index.observe(observation{taskID: "b1", activityID: "call-1"})
	index.observe(observation{taskID: "b1", activityID: "call-2"})

	// Act.
	_, ok := index.resolve(spoolTarget("/private/tmp/b1.output", "b1"))

	// Assert: guessing between two claims is how one run's output lands in
	// another run's card.
	if ok {
		t.Fatal("a conflicted task resolved to a guess")
	}
}

func TestConflictingSpawnsAreLoggedAsAnError(t *testing.T) {
	// Arrange.
	index, logs := ownerIndexFor(t)
	index.observe(observation{taskID: "b1", activityID: "call-1"})

	// Act.
	index.observe(observation{taskID: "b1", activityID: "call-2"})

	// Assert: the conflict is its own operation at error, naming the task it
	// made permanently unresolvable.
	rec := requireOnceIn(t, parseLogLines(t, *logs), "record-spawn-conflict", "error")
	if got := ctxString(t, rec, "task_id"); got != "b1" {
		t.Fatalf("task_id = %q, want the conflicted task", got)
	}
}

func TestASpawnRePortedByTheSameCallIsNotAConflict(t *testing.T) {
	// Arrange: a re-read record reports the same spawn again.
	index, _ := ownerIndexFor(t)
	index.observe(observation{taskID: "b1", activityID: "call-1"})

	// Act.
	index.observe(observation{taskID: "b1", activityID: "call-1"})
	_, ok := index.resolve(spoolTarget("/private/tmp/b1.output", "b1"))

	// Assert.
	if !ok {
		t.Fatal("a replayed spawn observation was treated as a conflict")
	}
}

func TestAMismatchedOutputPathRefusesResolution(t *testing.T) {
	// Arrange: the task's authoritative output is elsewhere.
	index, _ := ownerIndexFor(t)
	index.observe(observation{taskID: "b1", activityID: "call-1", outputPath: "/private/tmp/elsewhere.output"})

	// Act.
	_, ok := index.resolve(spoolTarget("/private/tmp/b1.output", "b1"))

	// Assert.
	if ok {
		t.Fatal("a spool resolved against a task whose authoritative output is a different file")
	}
}

func TestAnOwnerRefusalIsStatedOncePerSpool(t *testing.T) {
	tests := []struct {
		name      string
		arrange   func(index *ownerIndex)
		operation string
	}{
		{
			name: "a task two calls claim",
			arrange: func(index *ownerIndex) {
				index.observe(observation{taskID: "b1", activityID: "call-1"})
				index.observe(observation{taskID: "b1", activityID: "call-2"})
			},
			operation: "resolve-spool-owner-conflicted",
		},
		{
			name: "a task whose authoritative output is another file",
			arrange: func(index *ownerIndex) {
				index.observe(observation{taskID: "b1", activityID: "call-1", outputPath: "/private/tmp/elsewhere.output"})
			},
			operation: "resolve-spool-owner-path-mismatch",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a refused spool is never read, so every rescan resolves it
			// again.
			index, logs := ownerIndexFor(t)
			tc.arrange(index)

			// Act.
			index.resolve(spoolTarget("/private/tmp/b1.output", "b1"))
			index.resolve(spoolTarget("/private/tmp/b1.output", "b1"))

			// Assert: one ERROR naming the spool, not one per pass.
			rec := requireOnceIn(t, parseLogLines(t, *logs), tc.operation, "error")
			if got := ctxString(t, rec, "path"); got != "/private/tmp/b1.output" {
				t.Fatalf("path = %q, want the refused spool", got)
			}
		})
	}
}

func TestASpawnWithNoCallIsRejected(t *testing.T) {
	// Arrange.
	index, logs := ownerIndexFor(t)

	// Act.
	index.observe(observation{taskID: "b1"})

	// Assert.
	if _, ok := index.resolve(spoolTarget("/private/tmp/b1.output", "b1")); ok {
		t.Fatal("a spawn naming no call was recorded")
	}
	requireOnceIn(t, parseLogLines(t, *logs), "record-spawn", "error")
}

func TestTaskSpawnedRejectsIncompleteWorkspaceAttribution(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})

	// Act.
	h.sc.TaskSpawned("b1", "call-1", "agent-1", "/tmp/b1.output", false, "", "", "")

	// Assert.
	if _, ok := h.sc.owners.resolve(spoolTarget("/tmp/b1.output", "b1")); ok {
		t.Fatal("spawn with no workspace attribution was recorded")
	}
	h.requireOnce(t, "record-spawn", "error")
}

func TestMainAgentOfASessionTranscriptIsItsFileName(t *testing.T) {
	// Arrange: the per-record sessionId diverges from the file's; the file wins.
	index, _ := ownerIndexFor(t)

	// Act.
	got := index.mainAgentFor(discover.Target{Path: "/c/projects/p/sess-1.jsonl", SessionID: "sess-1"})

	// Assert.
	if got != "sess-1" {
		t.Fatalf("main agent = %q, want the transcript file's own session uuid", got)
	}
}

func TestObserverNormalizesTheOutputPath(t *testing.T) {
	// Arrange: the converter reports the /tmp spelling of a /private/tmp file.
	h := newHarness(t, &fakeStore{})
	spool := h.spoolFile(t, "b1", "hello\n")

	// Act.
	h.sc.TaskSpawned("b1", "call-1", "", filepath.Join(h.base, "spool", "claude-501", "proj", "runtime-sess", "tasks", "b1.output"), false, "/workspace", "workspace-id", "session-1")
	got, ok := h.sc.owners.resolve(spoolTarget(spool, "b1"))

	// Assert: the same file must not read as two.
	if !ok || got.activityID != "call-1" {
		t.Fatalf("resolve = %+v ok=%t, want the spawn found under the resolved spelling", got, ok)
	}
}

func TestAStopMintsTheCancelledTerminalThroughTheSpoolsReader(t *testing.T) {
	// Arrange. The terminal owes the OUTPUT the run produced, and only the
	// spool's handler holds those bytes — so the transcript's converter reports
	// the stop and the reader mints the terminal here.
	store := &fakeStore{}
	h := newHarness(t, store)
	spool := h.spoolFile(t, "b1stopped", "partial work\n")
	h.sc.TaskSpawned("b1stopped", "toolu_stopped_run", "", spool, false, "/workspace", "workspace-id", "session-1")
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.sc.pollAll()

	// Act.
	h.sc.TaskStopped("b1stopped")

	// Assert.
	cut := interruptedFor(store.writes, "toolu_stopped_run")
	if cut == nil {
		t.Fatalf("no cancelled terminal was written for the stopped run: %s", h.logText())
	}
	if cut.GetByUser() == nil {
		t.Fatalf("a stop is a person's decision and must state by_user: %v", cut.GetCause())
	}
	if got := cut.GetOutput().GetText().GetStdout(); got != "partial work\n" {
		t.Fatalf("cancelled stdout = %q, want the output the spool held", got)
	}
}

func TestAStoppedRunIsNeverConcludedLost(t *testing.T) {
	// Arrange. Deliberately-stopped work must resolve CANCELLED, never LOST:
	// the sweep is what would restate it, so the subject is the sweep.
	store := &fakeStore{}
	h := newHarness(t, store)
	spool := h.spoolFile(t, "b1stopswept", "partial work\n")
	h.sc.TaskSpawned("b1stopswept", "toolu_stopswept_run", "", spool, false, "/workspace", "workspace-id", "session-1")
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.sc.pollAll()
	h.sc.TaskStopped("b1stopswept")

	// Act.
	h.advance(24 * time.Hour)
	h.sc.sweep()

	// Assert: the policy reached no LOST conclusion (its conclusions are the
	// warn-level `lost-policy` records), so no terminal branch of the seam ran.
	h.requireNone(t, "lost-policy", "warn")
	for _, operation := range []string{"lost-terminal", "lost-terminal-unwatched", "lost-terminal-residue", "lost-terminal-unsupported"} {
		h.requireNone(t, operation, "")
	}
}

func TestAStopForAnUnclaimedSpoolIsHeldAndAppliedOnClaim(t *testing.T) {
	// Arrange. A spool is frequently written before the transcript line naming
	// it, so a stop arriving in that window has no reader to mint its terminal —
	// and dropping it would leave the run to be concluded LOST instead.
	store := &fakeStore{}
	h := newHarness(t, store)
	spool := h.spoolFile(t, "b1late", "work before the claim\n")
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Act: the stop arrives while the spool is still unowned, and only then does
	// its launch appear.
	h.sc.TaskStopped("b1late")
	if cut := interruptedFor(store.writes, "toolu_late_run"); cut != nil {
		t.Fatal("a terminal was minted for a spool that had no reader yet")
	}
	h.sc.TaskSpawned("b1late", "toolu_late_run", "", spool, false, "/workspace", "workspace-id", "session-1")
	h.sc.rescan()
	h.sc.pollAll()

	// Assert.
	cut := interruptedFor(store.writes, "toolu_late_run")
	if cut == nil {
		t.Fatalf("the held stop was never applied once the spool was claimed: %s", h.logText())
	}
	if cut.GetByUser() == nil {
		t.Fatalf("the applied stop must still state by_user: %v", cut.GetCause())
	}
}

// lapsedSpoolWithAStoppedRun arranges the shape realtest 3 produced: a spool
// whose hold lapsed before the transcript backlog delivered the launch line, and
// whose spawning call and stop are only then read off that transcript. A lapsed
// spool is not read, so the launch CLAIMS it: the next rescan reads it as the
// shell run it is, and its first durable batch applies the waiting stop. It
// answers the harness.
func lapsedSpoolWithAStoppedRun(t *testing.T, store *fakeStore, task, run, output string) *harness {
	t.Helper()
	h := newHarness(t, store)
	spool := h.spoolFile(t, task, output)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.advance(UnownedSpoolWindow)
	h.sc.rescan()
	if _, watched := h.sc.watchers[spool]; watched {
		t.Fatal("a lapsed unclaimed spool was read before anything claimed it")
	}
	// Only now does the transcript catch up and state who owned it and that a
	// person stopped it.
	h.sc.TaskSpawned(task, run, "", spool, false, "/workspace", "workspace-id", "session-1")
	h.sc.TaskStopped(task)
	h.sc.rescan()
	h.sc.pollAll()
	return h
}

func TestAStopMintsTheCancelledTerminalForASpoolClaimedAfterItsHoldLapsed(t *testing.T) {
	// Arrange. A lapsed hold says nothing about the run. The launch line naming
	// it arrives after the hold lapsed, which is the ordinary shape of a restart
	// with a transcript backlog, and the stop that follows must still settle it.
	store := &fakeStore{}

	// Act.
	lapsedSpoolWithAStoppedRun(t, store, "b1lapsedstop", "toolu_lapsed_run", "partial work\n")

	// Assert.
	cut := interruptedFor(store.writes, "toolu_lapsed_run")
	if cut == nil {
		t.Fatal("no cancelled terminal was written for a run stopped after its spool's hold lapsed")
	}
	if cut.GetByUser() == nil {
		t.Fatalf("a stop is a person's decision and must state by_user: %v", cut.GetCause())
	}
}

func TestTheLapsedSpoolsCancelledTerminalCarriesTheOutputItRead(t *testing.T) {
	// Arrange. A terminal owes the run's output, and the claimed spool's handler
	// is its sole reader — so it must carry what it read rather than settling
	// the run as though nothing had been observed.
	store := &fakeStore{}

	// Act.
	lapsedSpoolWithAStoppedRun(t, store, "b1lapsedbytes", "toolu_lapsed_bytes", "partial work\n")

	// Assert.
	got := interruptedFor(store.writes, "toolu_lapsed_bytes").GetOutput().GetText().GetStdout()
	if got != "partial work\n" {
		t.Fatalf("cancelled stdout = %q, want the output the claimed spool held", got)
	}
}

func TestAStopForASpoolClaimedAfterItsHoldLapsedStatesNoConverterGap(t *testing.T) {
	// Arrange. The reader states a converter that could not be asked for a
	// terminal at error level; a claimed shell spool CAN be asked for one.
	store := &fakeStore{}

	// Act.
	h := lapsedSpoolWithAStoppedRun(t, store, "b1lapsedquiet", "toolu_lapsed_quiet", "partial work\n")

	// Assert.
	h.requireNone(t, "cancel-terminal", "error")
}

func TestAStopWithNoTaskIsRefusedLoudly(t *testing.T) {
	// Arrange. A stop that names no task names no run, and attributing it to
	// anything would settle a row on a guess.
	h := newHarness(t, &fakeStore{})

	// Act.
	h.sc.TaskStopped("")

	// Assert.
	h.requireOnce(t, "task-stopped", "error")
}

// interruptedFor answers the interrupted terminal a producer wrote for a run,
// or nil when it wrote none.
func interruptedFor(batches []*storev1.EntryBatch, run string) *conversationv1.AgentBashInterrupted {
	var out *conversationv1.AgentBashInterrupted
	for _, batch := range batches {
		for _, e := range batch.GetEntries() {
			bash := e.GetAgentUpdate().GetBash()
			if bash.GetRun().GetValue() != run {
				continue
			}
			if cut := bash.GetFrame().GetSuccess().GetInterrupted(); cut != nil {
				out = cut
			}
		}
	}
	return out
}

func TestABackgroundedSpawnIsKnownByItsTaskIdForTheSpool(t *testing.T) {
	// Arrange. A task SPOOL is keyed by task id, which is the identity its
	// discovery target carries.
	index, _ := ownerIndexFor(t)
	index.observe(observation{taskID: "a15", activityID: "toolu_spawn", backgrounded: true})

	// Act.
	got := index.backgroundedFor(spoolTarget("/private/tmp/a15.output", "a15"))

	// Assert.
	if !got {
		t.Fatal("a backgrounded spawn is not reported for its own task spool")
	}
}

func TestABackgroundedSpawnIsKnownByItsCallForTheSidechain(t *testing.T) {
	// Arrange. THE SAME AGENT'S SIDECHAIN TRANSCRIPT NAMES NO TASK: its only
	// identity is the spawning call its meta file states. A flag reachable only
	// by task id answered false here, so the sidechain's frames named the
	// session's main agent as top_level while the spool's named the subagent —
	// one agent, two planes, two answers no consumer could reconcile.
	index, _ := ownerIndexFor(t)
	index.observe(observation{taskID: "a15", activityID: "toolu_spawn", backgrounded: true})

	// Act.
	got := index.backgroundedFor(discover.Target{
		// A sidechain's discovery TaskID is its `agent-<id>` LOCATOR, which no
		// launch ever names — so the task lookup MUST miss and the call-id
		// index is what answers.
		Path: "/p/s/subagents/agent-a15.jsonl", AgentID: "toolu_spawn", SessionID: "s", TaskID: "a15locator",
	})

	// Assert.
	if !got {
		t.Fatal("a backgrounded spawn is not reported for the same agent's sidechain transcript")
	}
}

func TestAForegroundSpawnIsNeverReportedAsBackgrounded(t *testing.T) {
	// Arrange. A synchronous subagent's stream ends with the turn, so its work
	// belongs to the session's main agent and marking it backgrounded would move
	// every one of its frames into a top_level of its own.
	index, _ := ownerIndexFor(t)
	index.observe(observation{taskID: "a16", activityID: "toolu_sync", backgrounded: false})

	// Act.
	got := index.backgroundedFor(discover.Target{
		Path: "/p/s/subagents/agent-a16.jsonl", AgentID: "toolu_sync", SessionID: "s", TaskID: "a16locator",
	})

	// Assert.
	if got {
		t.Fatal("a foreground spawn is reported as backgrounded")
	}
}

// TestAStopWhoseSpawningCallIsUnknownIsHeldNotErrored pins the LEVEL. The spool
// lapsed with nothing naming its spawning call, so it is not read; the launch
// line is tens of megabytes back in a transcript the restarted reader is still
// catching up on. There is nothing to settle YET — a wait, not a failure.
func TestAStopWhoseSpawningCallIsUnknownIsHeldNotErrored(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	h.spoolFile(t, "b1unknown", "work with no launch in sight\n")
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.advance(UnownedSpoolWindow)
	h.sc.rescan()

	// Act.
	h.sc.TaskStopped("b1unknown")

	// Assert.
	h.requireNone(t, "cancel-terminal", "error")
}

// claimedWorkflowSpoolWithAStoppedRun arranges the remaining converter-gap case:
// a w* spool a spawning call CLAIMED, so it is tailed under the declared-residue
// handler rather than demoted, and a person then stops its run.
func claimedWorkflowSpoolWithAStoppedRun(t *testing.T, store *fakeStore, task, run, output string) *harness {
	t.Helper()
	h := newHarness(t, store)
	spool := h.spoolFile(t, task, output)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.sc.TaskSpawned(task, run, "", spool, false, "/workspace", "workspace-id", "session-1")
	h.sc.rescan()
	if got := h.sc.watchers[spool].target.Kind; got != tail.KindWorkflowSpool {
		t.Fatalf("kind = %s, want the claimed w* spool tailed as a declared-kind spool", got)
	}
	h.sc.pollAll()
	h.sc.TaskStopped(task)
	return h
}

func TestAStopMintsTheCancelledTerminalForAClaimedWorkflowSpool(t *testing.T) {
	// Arrange. Kicking workflow CONVERSION says nothing about whether a workflow
	// run can be stopped: the reader knows the run and its owner, so the unit is
	// open downstream and a stop must settle it.
	store := &fakeStore{}

	// Act.
	claimedWorkflowSpoolWithAStoppedRun(t, store, "w1stopped", "toolu_workflow_run", "workflow work\n")

	// Assert.
	cut := interruptedFor(store.writes, "toolu_workflow_run")
	if cut == nil {
		t.Fatal("no cancelled terminal was written for a stopped run whose spool is a claimed w* spool")
	}
	if cut.GetByUser() == nil {
		t.Fatalf("a stop is a person's decision and must state by_user: %v", cut.GetCause())
	}
}

func TestTheWorkflowSpoolsCancelledTerminalCarriesTheOutputItRead(t *testing.T) {
	// Arrange. A terminal owes the run's output, and this handler is the spool's
	// sole reader — so it must carry what it read rather than claiming nothing
	// was observed.
	store := &fakeStore{}

	// Act.
	claimedWorkflowSpoolWithAStoppedRun(t, store, "w1bytes", "toolu_workflow_bytes", "workflow work\n")

	// Assert.
	got := interruptedFor(store.writes, "toolu_workflow_bytes").GetOutput().GetText().GetStdout()
	if got != "workflow work\n" {
		t.Fatalf("cancelled stdout = %q, want the output the workflow spool held", got)
	}
}

func TestAStopForAClaimedWorkflowSpoolStatesNoConverterGap(t *testing.T) {
	// Arrange. The reader states a converter that could not be asked for a
	// terminal at error level. The declared-residue handler CAN be asked for one
	// now, so that record is a false alarm and must not be written.
	store := &fakeStore{}

	// Act.
	h := claimedWorkflowSpoolWithAStoppedRun(t, store, "w1nogap", "toolu_workflow_nogap", "workflow work\n")

	// Assert.
	h.requireNone(t, "cancel-terminal", "error")
}

// TestAShutdownWithdrawingTheTerminalWriteIsNotAStoreFailure closes the last
// storeWrite caller that had no interrupted() guard. Its two siblings in
// cycle.go return quietly when the shutdown withdraws a write — storeWrite has
// already stated the one INFO `shutdown` record — while this one accused the
// store of a failure it did not have.
func TestAShutdownWithdrawingTheTerminalWriteIsNotAStoreFailure(t *testing.T) {
	// Arrange: a stop ready to settle, against a store that never answers.
	store := &fakeStore{}
	h := newHarness(t, store)
	spool := h.spoolFile(t, "b1withdrawn", "work\n")
	h.sc.TaskSpawned("b1withdrawn", "toolu_withdrawn", "", spool, false, "/workspace", "workspace-id", "session-1")
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.sc.pollAll()
	shutdown, cancel := context.WithCancel(context.Background())
	h.sc.shutdown = shutdown
	// Wedged only NOW: the setup's own reads must commit normally.
	entered := make(chan struct{})
	store.entered = entered
	store.writeWedged = true
	applied := make(chan struct{})

	// Act: the terminal write wedges, then the shutdown withdraws it.
	go func() {
		defer close(applied)
		h.sc.TaskStopped("b1withdrawn")
	}()
	select {
	case <-entered:
	case <-time.After(2 * time.Second):
		t.Fatal("the store was never asked to write, so nothing is wedged to withdraw")
	}
	cancel()
	select {
	case <-applied:
	case <-time.After(2 * time.Second):
		t.Fatal("the withdrawn write never returned")
	}

	// Assert.
	h.requireNone(t, "cancel-terminal", "error")
}

// renamedSpoolHarness claims and reads a spool at its launch-named path, then
// lays down a file for the same task at a new runtime-session path — the SAME
// file moved there when rename is true, a different one otherwise.
func renamedSpoolHarness(t *testing.T, rename bool) (*harness, string) {
	t.Helper()
	h := newHarness(t, &fakeStore{})
	spool := h.spoolFile(t, "b1moved", "before the move\n")
	h.sc.TaskSpawned("b1moved", "toolu_moved", "agent-1", spool, false, "/workspace", "workspace-id", "session-1")
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.sc.pollAll()
	moved := filepath.Join(h.spool, "claude-501", "proj", "resumed-sess", "tasks", "b1moved.output")
	if err := os.MkdirAll(filepath.Dir(moved), 0o755); err != nil {
		t.Fatalf("creating %s: %v", filepath.Dir(moved), err)
	}
	if rename {
		if err := os.Rename(spool, moved); err != nil {
			t.Fatalf("renaming the spool: %v", err)
		}
	} else {
		h.write(t, moved, "a different file for the same task\n")
	}
	return h, normalized(moved)
}

func TestARenamedClaimedSpoolIsReadAtItsNewPath(t *testing.T) {
	// Arrange.
	h, moved := renamedSpoolHarness(t, true)

	// Act.
	h.sc.rescan()

	// Assert: the same file is the same run, so its reader follows it.
	if _, watched := h.sc.watchers[moved]; !watched {
		t.Fatalf("the renamed spool of a claimed run is not read at its new path: %s", h.logText())
	}
	h.requireOnce(t, "spool-rename", "info")
}

func TestADifferentFileAtANewPathIsNotTakenForARename(t *testing.T) {
	// Arrange.
	h, moved := renamedSpoolHarness(t, false)

	// Act.
	h.sc.rescan()

	// Assert: a name is never evidence, so another file stays refused.
	if _, watched := h.sc.watchers[moved]; watched {
		t.Fatal("a different file was read as the claimed run's renamed spool")
	}
	h.requireOnce(t, "resolve-spool-owner-path-mismatch", "error")
}

func TestAClaimAppliesAStopThatWasWaitingForIt(t *testing.T) {
	// Arrange: the run is stopped while nothing names its call, so its spool is
	// held and unread and the stop waits as one pending value for the task.
	store := &fakeStore{}
	h := newHarness(t, store)
	spool := h.spoolFile(t, "b1waiting", "everything this run ever said\n")
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.sc.TaskStopped("b1waiting")
	if cut := interruptedFor(store.writes, "toolu_waiting_run"); cut != nil {
		t.Fatal("a terminal was minted before anything named the run's spawning call")
	}

	// Act: the transcript catches up and states who spawned it; the claim reads
	// the spool and its first durable batch applies the waiting stop.
	h.sc.TaskSpawned("b1waiting", "toolu_waiting_run", "", spool, false, "/workspace", "workspace-id", "session-1")
	h.sc.rescan()
	h.sc.pollAll()

	// Assert.
	cut := interruptedFor(store.writes, "toolu_waiting_run")
	if cut == nil {
		t.Fatalf("the claim did not apply the stop that was waiting for it: %s", h.logText())
	}
	if cut.GetByUser() == nil {
		t.Fatalf("the applied stop must state by_user: %v", cut.GetCause())
	}
}

func TestAConcludedAgentRunIsNeverConcludedLost(t *testing.T) {
	tests := []struct {
		name          string
		concludeFirst bool
	}{
		{name: "concluded while its spool is read", concludeFirst: false},
		{name: "concluded before its spool is claimed", concludeFirst: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t, &fakeStore{})
			spool := h.spoolFile(t, "a1notified", "work\n")
			if tt.concludeFirst {
				h.sc.TaskConcluded("a1notified")
			}
			h.sc.TaskSpawned("a1notified", "toolu_notified", "agent-1", spool, true, "/workspace", "workspace-id", "session-1")
			if err := h.sc.beginCycle(); err != nil {
				t.Fatalf("beginCycle: %v", err)
			}
			h.sc.pollAll()
			if !tt.concludeFirst {
				h.sc.TaskConcluded("a1notified")
			}

			// Act: the run passes the silence window.
			h.advance(24 * time.Hour)
			h.sc.sweep()

			// Assert.
			h.requireNone(t, "lost-policy", "warn")
			for _, operation := range []string{"lost-terminal", "lost-terminal-refused", "lost-terminal-unwatched", "lost-terminal-residue", "lost-terminal-unsupported"} {
				h.requireNone(t, operation, "")
			}
		})
	}
}

func TestASilentUnconcludedAgentRunIsStillConcludedLost(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	spool := h.spoolFile(t, "a1silent", "work\n")
	h.sc.TaskSpawned("a1silent", "toolu_silent", "agent-1", spool, true, "/workspace", "workspace-id", "session-1")
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.sc.pollAll()

	// Act.
	h.advance(24 * time.Hour)
	h.sc.sweep()

	// Assert.
	h.requireOnce(t, "lost-terminal", "")
}

func TestAConclusionWithNoTaskIsRefusedLoudly(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})

	// Act.
	h.sc.TaskConcluded("")

	// Assert.
	h.requireOnce(t, "task-concluded", "error")
}
