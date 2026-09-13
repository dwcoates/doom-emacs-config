package main

import (
	"io"
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

// residueSpoolWithAStoppedRun arranges the shape realtest 3 produced: a spool
// whose hold expired before the transcript backlog delivered the launch line,
// so it is being read as residue, and whose spawning call and stop are only
// then read off that transcript. It answers the harness and the run's id.
func residueSpoolWithAStoppedRun(t *testing.T, store *fakeStore, task, run, output string) *harness {
	t.Helper()
	h := newHarness(t, store)
	spool := h.spoolFile(t, task, output)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	// The hold lapses with no owner in sight: the spool is demoted and tailed as
	// residue, and its bytes are read under that handler.
	h.advance(UnownedSpoolWindow)
	h.sc.rescan()
	if got := h.sc.watchers[spool].target.Kind; got != tail.KindResidueSpool {
		t.Fatalf("kind = %s, want the spool demoted to residue before the stop arrives", got)
	}
	h.sc.pollAll()
	// Only now does the transcript catch up and state who owned it and that a
	// person stopped it.
	h.sc.TaskSpawned(task, run, "", spool, false, "/workspace", "workspace-id", "session-1")
	h.sc.TaskStopped(task)
	return h
}

func TestAStopMintsTheCancelledTerminalForASpoolBeingReadAsResidue(t *testing.T) {
	// Arrange. Ingesting a spool as residue says what could be made of its
	// BYTES; it never says the run is unknown. The launch line naming the run
	// arrives after the hold lapsed, which is the ordinary shape of a restart
	// with a transcript backlog, and the stop that follows must still settle it.
	store := &fakeStore{}

	// Act.
	residueSpoolWithAStoppedRun(t, store, "b1residuestop", "toolu_residue_run", "partial work\n")

	// Assert.
	cut := interruptedFor(store.writes, "toolu_residue_run")
	if cut == nil {
		t.Fatal("no cancelled terminal was written for a run stopped while its spool was residue")
	}
	if cut.GetByUser() == nil {
		t.Fatalf("a stop is a person's decision and must state by_user: %v", cut.GetCause())
	}
}

func TestTheResidueSpoolsCancelledTerminalCarriesTheOutputItRead(t *testing.T) {
	// Arrange. A terminal owes the run's output, and a residue handler IS the
	// spool's sole reader — so it must carry what it read rather than settling
	// the run as though nothing had been observed.
	store := &fakeStore{}

	// Act.
	residueSpoolWithAStoppedRun(t, store, "b1residuebytes", "toolu_residue_bytes", "partial work\n")

	// Assert.
	got := interruptedFor(store.writes, "toolu_residue_bytes").GetOutput().GetText().GetStdout()
	if got != "partial work\n" {
		t.Fatalf("cancelled stdout = %q, want the output the residue spool held", got)
	}
}

func TestAStopForASpoolBeingReadAsResidueStatesNoConverterGap(t *testing.T) {
	// Arrange. The reader states a converter that could not be asked for a
	// terminal at error level. A residue spool CAN be asked for one, so that
	// record is now a false alarm and must not be written.
	store := &fakeStore{}

	// Act.
	h := residueSpoolWithAStoppedRun(t, store, "b1residuequiet", "toolu_residue_quiet", "partial work\n")

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

// unownedResidueSpool arranges the shape that wrote 31 cancel-terminal ERRORs
// on the owner's machine: a spool whose hold lapsed with nothing naming its
// spawning call, so it is demoted to residue and TAILED — watched, readable,
// and still owned by nobody. The launch line naming its call is tens of
// megabytes back in a transcript the restarted reader is still catching up on.
func unownedResidueSpool(t *testing.T, store *fakeStore, task, output string) (*harness, string) {
	t.Helper()
	h := newHarness(t, store)
	spool := h.spoolFile(t, task, output)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.advance(UnownedSpoolWindow)
	h.sc.rescan()
	if got := h.sc.watchers[spool].target.Kind; got != tail.KindResidueSpool {
		t.Fatalf("kind = %s, want the spool demoted to residue and tailed", got)
	}
	h.sc.pollAll()
	return h, spool
}

// TestAStopWhoseSpawningCallIsUnknownIsHeldNotErrored pins the LEVEL. The spool
// is being read, but the terminal is keyed on the spawning call and nothing has
// named it yet, so there is nothing to settle YET — a wait, not a failure.
func TestAStopWhoseSpawningCallIsUnknownIsHeldNotErrored(t *testing.T) {
	// Arrange.
	store := &fakeStore{}
	h, _ := unownedResidueSpool(t, store, "b1unknown", "work with no launch in sight\n")

	// Act.
	h.sc.TaskStopped("b1unknown")

	// Assert.
	h.requireNone(t, "cancel-terminal", "error")
}

// TestALaunchAppliesAStopThatWasWaitingForIt is the retry edge the seam was
// missing. applyStop was reached only after a batch of the run's spool
// committed — and a run a person stopped has stopped writing, so a stop learned
// before its launch was never retried at all and the run stayed open forever.
// The launch itself must close it, with no further poll.
func TestALaunchAppliesAStopThatWasWaitingForIt(t *testing.T) {
	// Arrange: the spool is read and stopped while nothing names its call.
	store := &fakeStore{}
	h, spool := unownedResidueSpool(t, store, "b1waiting", "everything this run ever said\n")
	h.sc.TaskStopped("b1waiting")
	if cut := interruptedFor(store.writes, "toolu_waiting_run"); cut != nil {
		t.Fatal("a terminal was minted before anything named the run's spawning call")
	}

	// Act: the transcript catches up and states who spawned it. Nothing polls
	// afterwards — a stopped run writes no more bytes, so no poll would come.
	h.sc.TaskSpawned("b1waiting", "toolu_waiting_run", "", spool, false, "/workspace", "workspace-id", "session-1")

	// Assert.
	cut := interruptedFor(store.writes, "toolu_waiting_run")
	if cut == nil {
		t.Fatalf("the launch did not apply the stop that was waiting for it: %s", h.logText())
	}
	if cut.GetByUser() == nil {
		t.Fatalf("the applied stop must state by_user: %v", cut.GetCause())
	}
}
