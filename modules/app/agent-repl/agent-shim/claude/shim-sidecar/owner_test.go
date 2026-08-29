package main

import (
	"io"
	"path/filepath"
	"strings"
	"testing"

	"agentrepl/shim-claude-sidecar/internal/discover"
	"agentrepl/shim-claude-sidecar/internal/logging"
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

	// Assert.
	joined := strings.Join(*logs, "\n")
	if !strings.Contains(joined, "CONFLICTING") || !strings.Contains(joined, `"level":"error"`) {
		t.Fatalf("the conflict was not stated loudly; got %v", *logs)
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
	if !strings.Contains(strings.Join(*logs, "\n"), "names no task or no spawning call") {
		t.Fatalf("the rejection was silent; got %v", *logs)
	}
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
	h.sc.TaskSpawned("b1", "call-1", "", filepath.Join(h.base, "spool", "claude-501", "proj", "runtime-sess", "tasks", "b1.output"))
	got, ok := h.sc.owners.resolve(spoolTarget(spool, "b1"))

	// Assert: the same file must not read as two.
	if !ok || got.activityID != "call-1" {
		t.Fatalf("resolve = %+v ok=%t, want the spawn found under the resolved spelling", got, ok)
	}
}
