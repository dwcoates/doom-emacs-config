package main

import (
	"strings"
	"testing"
	"time"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/discover"
	"agentrepl/shim-claude-sidecar/internal/stale"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// ---- a settled run is never tracked again, by this process or a later one ----

// lostTerminalsFor counts the LOST terminals written for run: a shell run's
// interrupted-by-lost frame, or a subagent spawn settled failed-by-lost.
func lostTerminalsFor(batches []*storev1.EntryBatch, run string) int {
	count := 0
	for _, batch := range batches {
		for _, e := range batch.GetEntries() {
			if bash := e.GetAgentUpdate().GetBash(); bash.GetRun().GetValue() == run &&
				bash.GetFrame().GetSuccess().GetInterrupted().GetLost() != nil {
				count++
			}
			activity := e.GetAgentUpdate().GetServeableFrame().GetAgentItem().GetAgentFrame().GetUpdate().GetActivity()
			if activity.GetActivityId().GetValue() == run && activity.GetSubagent().GetFailure().GetLost() != nil {
				count++
			}
		}
	}
	return count
}

// requireMessageOnce finds the one record of operation at level whose message
// holds substring.
func requireMessageOnce(t *testing.T, records []logRecord, operation, level, substring string) logRecord {
	t.Helper()
	var found []logRecord
	for _, r := range opsAt(records, operation, level) {
		if strings.Contains(r.Message, substring) {
			found = append(found, r)
		}
	}
	if len(found) != 1 {
		t.Fatalf("operation %q at %q saying %q was recorded %d times, want once; the log held %v",
			operation, level, substring, len(found), operationLevels(records))
	}
	return found[0]
}

// beginOrFail begins a production cycle, which must succeed.
func beginOrFail(t *testing.T, h *harness) {
	t.Helper()
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
}

func TestAWatchedRunTheStoreHoldsLiveIsTracked(t *testing.T) {
	// Arrange.
	store := &fakeStore{}
	h := newHarness(t, store)
	spool := h.spoolFile(t, "b1live", "work\n")
	h.sc.TaskSpawned("b1live", "toolu_live", "", spool, false, "/workspace", "workspace-id", "session-1")

	// Act.
	beginOrFail(t, h)

	// Assert: the store was asked about the run, by its spawning call, and the
	// run's clock started.
	if len(store.settlementsAsked) != 1 || strings.Join(store.settlementsAsked[0], ",") != "toolu_live" {
		t.Fatalf("settlements asked %v, want one read naming toolu_live", store.settlementsAsked)
	}
	if !h.sc.tracker.Open(spool) {
		t.Fatalf("a run the record holds live is not tracked: %s", h.logText())
	}
}

func TestAWatchedRunTheStoreHoldsEndedIsSettledNotTracked(t *testing.T) {
	// Arrange.
	store := &fakeStore{settled: map[string]int64{"toolu_done": 42}}
	h := newHarness(t, store)
	spool := h.spoolFile(t, "b1done", "work\n")
	h.sc.TaskSpawned("b1done", "toolu_done", "", spool, false, "/workspace", "workspace-id", "session-1")

	// Act.
	beginOrFail(t, h)

	// Assert.
	if h.sc.tracker.Open(spool) || !h.sc.tracker.Settled(spool) {
		t.Fatalf("open=%t settled=%t, want a run the record holds as ended settled and untracked",
			h.sc.tracker.Open(spool), h.sc.tracker.Settled(spool))
	}
	rec := requireMessageOnce(t, h.records(t), "lost-policy", "info", "run already settled per the store (ended_at_ms=42)")
	if got := ctxString(t, rec, "activity_id"); got != "toolu_done" {
		t.Fatalf("activity_id = %q, want toolu_done", got)
	}
}

func TestARunTheStoreCannotAnswerForIsNotTrackedAndIsAskedAgain(t *testing.T) {
	// Arrange.
	store := &fakeStore{settlementsFail: "database is locked"}
	h := newHarness(t, store)
	spool := h.spoolFile(t, "b1unknown", "work\n")
	h.sc.TaskSpawned("b1unknown", "toolu_unknown", "", spool, false, "/workspace", "workspace-id", "session-1")
	beginOrFail(t, h)
	if h.sc.tracker.Open(spool) || !h.sc.tracking(spool) {
		t.Fatalf("open=%t tracking=%t, want the run untracked and still awaiting its settlement",
			h.sc.tracker.Open(spool), h.sc.tracking(spool))
	}
	requireMessageOnce(t, h.records(t), "run-settlements", "error", "1 detached run(s) are not tracked")

	// Act: the store recovers, and the next ordinary pass asks again.
	store.settlementsFail = ""
	h.advance(30 * time.Second)
	h.sc.rescan()

	// Assert.
	if !h.sc.tracker.Open(spool) {
		t.Fatalf("the run was not tracked once the store could answer: %s", h.logText())
	}
}

func TestARunSettledInThisProcessIsNotQueuedAgain(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	spool := h.spoolFile(t, "b1again", "work\n")
	h.sc.TaskSpawned("b1again", "toolu_again", "", spool, false, "/workspace", "workspace-id", "session-1")
	h.sc.tracker.Settle(spool)

	// Act: its watcher is rebuilt (a vanished file that came back).
	h.sc.trackDetached(discover.Target{Path: spool, TaskID: "b1again", Kind: tail.KindShellSpool}, h.clock)

	// Assert.
	if h.sc.tracking(spool) {
		t.Fatal("a run settled in this process was queued to be tracked again")
	}
}

func TestTrackingADetachedRunWithNoSpawningCallPanics(t *testing.T) {
	// Arrange: a watched detached file is always claimed by its spawning call.
	h := newHarness(t, &fakeStore{})
	target := discover.Target{Path: h.spoolFile(t, "b1orphan", "work\n"), TaskID: "b1orphan", Kind: tail.KindShellSpool}
	defer func() {
		// Assert.
		if recover() == nil {
			t.Fatal("tracking a detached run that names no spawning call did not panic")
		}
	}()

	// Act.
	h.sc.trackDetached(target, h.clock)
}

func TestTrackDetachedIgnoresATranscript(t *testing.T) {
	// Arrange: a transcript is an agent's own record, not a detached run.
	h := newHarness(t, &fakeStore{})
	target := discover.Target{Path: "/x/agent-1.jsonl", TaskID: "a1", SessionID: "sess-1", Kind: tail.KindAgentTranscript}

	// Act.
	h.sc.trackDetached(target, h.clock)

	// Assert.
	if h.sc.tracking(target.Path) {
		t.Fatal("a transcript was queued as a detached run")
	}
}

func TestSettleRunAnswersWhetherTheRunWasOwedAConclusion(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness, path string)
		want    bool
	}{
		{name: "awaiting its settlement", arrange: func(h *harness, path string) {
			h.sc.trackPending[path] = stale.Work{Path: path, TaskID: "b1", RunActivityID: "toolu_1"}
		}, want: true},
		{name: "tracked", arrange: func(h *harness, path string) {
			h.sc.tracker.Observe(stale.Work{Path: path, TaskID: "b1", RunActivityID: "toolu_1"}, 1)
		}, want: true},
		{name: "neither", arrange: func(*harness, string) {}, want: false},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t, &fakeStore{})
			path := "/private/tmp/b1.output"
			test.arrange(h, path)

			// Act.
			got := h.sc.settleRun(path)

			// Assert.
			if got != test.want || h.sc.tracking(path) || !h.sc.tracker.Settled(path) {
				t.Fatalf("settleRun = %t (tracking=%t settled=%t), want %t and the run settled",
					got, h.sc.tracking(path), h.sc.tracker.Settled(path), test.want)
			}
		})
	}
}

func TestAConclusionTheStoreHoldsEndedMintsNoLostAndSettles(t *testing.T) {
	// Arrange: the run was ended by another plane while its spool went quiet.
	store := &fakeStore{}
	h := newHarness(t, store)
	spool := h.spoolFile(t, "b1other", "work\n")
	h.sc.TaskSpawned("b1other", "toolu_other", "", spool, false, "/workspace", "workspace-id", "session-1")
	beginOrFail(t, h)
	h.sc.pollAll()
	store.settled = map[string]int64{"toolu_other": 77}

	// Act.
	h.advance(24 * time.Hour)
	h.sc.sweep()

	// Assert.
	if got := lostTerminalsFor(store.writes, "toolu_other"); got != 0 {
		t.Fatalf("%d LOST terminal(s) written over a run the record holds as ended, want none", got)
	}
	if !h.sc.tracker.Settled(spool) {
		t.Fatal("the run the record holds as ended was not settled")
	}
	requireMessageOnce(t, h.records(t), "lost-terminal-settled", "info", "ended_at_ms=77")
}

func TestAConclusionTheStoreCannotAnswerForMintsNothingAndSuspends(t *testing.T) {
	// Arrange.
	store := &fakeStore{}
	h := newHarness(t, store)
	spool := h.spoolFile(t, "b1blind", "work\n")
	h.sc.TaskSpawned("b1blind", "toolu_blind", "", spool, false, "/workspace", "workspace-id", "session-1")
	beginOrFail(t, h)
	h.sc.pollAll()
	store.settlementsFail = "database is locked"

	// Act.
	h.advance(24 * time.Hour)
	h.sc.sweep()

	// Assert: nothing minted, nothing settled, and production suspended so the
	// next cycle re-derives the conclusion.
	if got := lostTerminalsFor(store.writes, "toolu_blind"); got != 0 {
		t.Fatalf("%d LOST terminal(s) minted without the record's answer, want none", got)
	}
	if h.sc.tracker.Settled(spool) {
		t.Fatal("a conclusion nobody could check was settled")
	}
	if h.sc.cursors != nil {
		t.Fatal("production was not suspended by a store that could not answer")
	}
	requireMessageOnce(t, h.records(t), "lost-terminal", "error", "1 lost sweep conclusion(s) minted no LOST terminal")
}

func TestAConclusionWhoseLostWriteFailedIsNotSettled(t *testing.T) {
	// Arrange.
	store := &fakeStore{}
	h := newHarness(t, store)
	spool := h.spoolFile(t, "b1unwritten", "work\n")
	h.sc.TaskSpawned("b1unwritten", "toolu_unwritten", "", spool, false, "/workspace", "workspace-id", "session-1")
	beginOrFail(t, h)
	h.sc.pollAll()
	store.writeFail = "disk is gone"

	// Act.
	h.advance(24 * time.Hour)
	h.sc.sweep()

	// Assert: the next cycle must be able to restate it.
	if h.sc.tracker.Settled(spool) {
		t.Fatal("a run whose LOST terminal never became durable was settled")
	}
}

func TestAConclusionWhoseLostIsDurableIsSettled(t *testing.T) {
	// Arrange.
	store := &fakeStore{}
	h := newHarness(t, store)
	spool := h.spoolFile(t, "b1written", "work\n")
	h.sc.TaskSpawned("b1written", "toolu_written", "", spool, false, "/workspace", "workspace-id", "session-1")
	beginOrFail(t, h)
	h.sc.pollAll()

	// Act.
	h.advance(24 * time.Hour)
	h.sc.sweep()

	// Assert.
	if got := lostTerminalsFor(store.writes, "toolu_written"); got != 1 {
		t.Fatalf("%d LOST terminal(s) written, want one", got)
	}
	if !h.sc.tracker.Settled(spool) {
		t.Fatal("a run whose LOST terminal is durable was not settled")
	}
}

// ---- the regression: a settled run, then a fresh sidecar, then nothing ----

func TestAFreshSidecarNeverTracksOrLosesARunThatSettledBeforeIt(t *testing.T) {
	tests := []struct {
		name   string
		taskID string
		run    string
		spool  string
		agent  string
		bg     bool
		// settle is how the FIRST process saw the run end.
		settle func(h *harness)
	}{
		{
			name: "a shell run that read its own EXIT marker", taskID: "b1exited", run: "toolu_exited",
			spool:  "work\nEXIT=0\n",
			settle: func(h *harness) { h.sc.pollAll() },
		},
		{
			name: "a backgrounded agent its transcript's notification concluded", taskID: "a1notified", run: "toolu_notified",
			spool: "work\n", agent: "agent-1", bg: true,
			settle: func(h *harness) {
				h.sc.pollAll()
				h.sc.TaskConcluded("a1notified")
				// The transcript's conversion of that notification is the
				// spawn's durable settle; the record holds the run as ended.
				h.store.settled = map[string]int64{"toolu_notified": 1}
			},
		},
		{
			name: "a shell run a person stopped", taskID: "b1stopped", run: "toolu_stopped",
			spool: "partial work\n",
			settle: func(h *harness) {
				h.sc.pollAll()
				h.sc.TaskStopped("b1stopped")
			},
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange: the first process watches the run end.
			store := &fakeStore{}
			h := newHarness(t, store)
			spool := h.spoolFile(t, test.taskID, test.spool)
			h.sc.TaskSpawned(test.taskID, test.run, test.agent, spool, test.bg, "/workspace", "workspace-id", "session-1")
			beginOrFail(t, h)
			test.settle(h)
			if h.sc.tracking(spool) {
				t.Fatalf("the first process still tracks the settled run: %s", h.logText())
			}

			// Act: a deploy restarts the sidecar. The new process re-reads the
			// launch from its transcript, resumes the spool at its committed
			// cursor, and runs long past every silence window.
			h.restart(t)
			h.sc.TaskSpawned(test.taskID, test.run, test.agent, spool, test.bg, "/workspace", "workspace-id", "session-1")
			beginOrFail(t, h)
			h.sc.pollAll()
			h.advance(24 * time.Hour)
			h.sc.sweep()

			// Assert: never tracked, never concluded, never minted.
			if h.sc.tracker.Open(spool) || !h.sc.tracker.Settled(spool) {
				t.Fatalf("open=%t settled=%t, want the fresh process to settle the run without tracking it",
					h.sc.tracker.Open(spool), h.sc.tracker.Settled(spool))
			}
			records := h.records(t)
			requireMessageOnce(t, records, "lost-policy", "info", "run already settled per the store")
			for _, r := range opsAt(records, "lost-policy", "") {
				if strings.Contains(r.Message, "tracking detached run") || strings.Contains(r.Message, "run concluded LOST") {
					t.Fatalf("the fresh process tracked or concluded the settled run: %q", r.Message)
				}
			}
			requireNoneIn(t, records, "lost-terminal", "")
			if got := lostTerminalsFor(store.writes, test.run); got != 0 {
				t.Fatalf("%d LOST terminal(s) written for the settled run, want none", got)
			}
		})
	}
}
