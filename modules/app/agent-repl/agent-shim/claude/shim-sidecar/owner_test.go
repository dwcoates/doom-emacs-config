package main

import (
	"os"
	"path/filepath"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/shim-claude-sidecar/internal/discover"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

func ownerTarget(path, taskID string) discover.Target {
	return discover.Target{Path: path, Kind: tail.KindShellSpool, TaskID: taskID, Raw: true}
}

func TestOwnerResolutionPrefersExactNormalizedOutputPath(t *testing.T) {
	s, _ := ownerSidecar(t)
	path := "/tmp/claude-501/slug/runtime/tasks/b1.output"
	if !s.noteTaskOwner("b1", "S1", path, OwnerSourceLiveLaunch) {
		t.Fatal("did not record live task owner")
	}
	got := s.resolveOwnerResult(ownerTarget("/tmp/claude-501/slug/runtime/tasks/./b1.output", "b1"))
	if !got.Resolved() || got.Outcome != OwnerResolvedPath || got.SessionID != "S1" || got.OutputPath != normalizeOwnerOutputPath(path) {
		t.Fatalf("resolution = %+v, want exact path S1", got)
	}
}

func TestOwnerResolutionRejectsExactOutputPathTaskMismatch(t *testing.T) {
	s, _ := ownerSidecar(t)
	path := "/tmp/claude-501/slug/runtime/tasks/b1.output"
	s.noteTaskOwner("b1", "S1", path, OwnerSourceLiveLaunch)

	got := s.resolveOwnerResult(ownerTarget(path, "b2"))
	if got.Outcome != OwnerUnresolvedConflict || got.Resolved() || !got.MayArrive() {
		t.Fatalf("resolution = %+v, want retryable path-task conflict", got)
	}
}

func TestOwnerResolutionRejectsConflictingTaskIDWithoutMatchingPath(t *testing.T) {
	s, read := ownerSidecar(t)
	s.noteTaskOwner("b1", "S1", "", OwnerSourceLiveLaunch)
	s.noteTaskOwner("b1", "S2", "", OwnerSourceLiveLaunch)

	got := s.resolveOwnerResult(ownerTarget("/tmp/claude-501/slug/runtime/tasks/b1.output", "b1"))
	if got.Outcome != OwnerUnresolvedConflict || got.Resolved() || !got.MayArrive() {
		t.Fatalf("resolution = %+v, want retryable conflict", got)
	}
	if lines := linesContaining(read(), "conflicting task ownership"); len(lines) != 1 {
		t.Fatalf("conflict resolution logs = %v, want one", lines)
	}
}

func TestOwnerResolutionKeepsDistinctExactPathsWhenTaskAssociationConflicts(t *testing.T) {
	s, _ := ownerSidecar(t)
	firstPath := "/tmp/claude-501/slug/one/tasks/b1.output"
	secondPath := "/tmp/claude-501/slug/two/tasks/b1.output"
	s.noteTaskOwner("b1", "S1", firstPath, OwnerSourceLiveLaunch)
	s.noteTaskOwner("b1", "S2", secondPath, OwnerSourceLiveLaunch)

	for path, session := range map[string]string{firstPath: "S1", secondPath: "S2"} {
		got := s.resolveOwnerResult(ownerTarget(path, "b1"))
		if got.Outcome != OwnerResolvedPath || got.SessionID != session {
			t.Fatalf("resolution for %s = %+v, want exact path %s", path, got, session)
		}
	}
	if got := s.resolveOwnerResult(ownerTarget("/tmp/claude-501/slug/three/tasks/b1.output", "b1")); got.Outcome != OwnerUnresolvedConflict {
		t.Fatalf("task-only resolution = %+v, want conflict", got)
	}
}

func TestOwnerResolutionRejectsConflictingExactOutputPath(t *testing.T) {
	s, _ := ownerSidecar(t)
	path := "/tmp/claude-501/slug/runtime/tasks/b1.output"
	s.noteTaskOwner("b1", "S1", path, OwnerSourceLiveLaunch)
	s.noteTaskOwner("b2", "S2", path, OwnerSourceLiveLaunch)

	if got := s.resolveOwnerResult(ownerTarget(path, "b1")); got.Outcome != OwnerUnresolvedConflict {
		t.Fatalf("resolution = %+v, want poisoned path conflict", got)
	}
}

func TestOwnerResolutionRejectsExactOutputPathClaimedByDifferentTaskInSameSession(t *testing.T) {
	s, _ := ownerSidecar(t)
	path := "/tmp/claude-501/slug/runtime/tasks/shared.output"
	s.noteTaskOwner("b1", "S1", path, OwnerSourceLiveLaunch)
	s.noteTaskOwner("b2", "S1", path, OwnerSourceLiveLaunch)

	if got := s.resolveOwnerResult(ownerTarget(path, "b1")); got.Outcome != OwnerUnresolvedConflict {
		t.Fatalf("resolution = %+v, want poisoned path conflict", got)
	}
}

func TestOwnerResolutionRejectsTaskAssociationWithDifferentRecordedOutputPath(t *testing.T) {
	s, _ := ownerSidecar(t)
	s.noteTaskOwner("b1", "S1", "/tmp/claude-501/slug/runtime/tasks/b1.output", OwnerSourceLiveLaunch)

	got := s.resolveOwnerResult(ownerTarget("/tmp/claude-501/other-runtime/tasks/b1.output", "b1"))
	if got.Outcome != OwnerUnresolvedConflict || got.Resolved() {
		t.Fatalf("resolution = %+v, want task path conflict", got)
	}
}

func TestResetOwnersDiscardsPriorConnectionMappings(t *testing.T) {
	s, _ := ownerSidecar(t)
	s.noteTaskOwner("b1", "S1", "/tmp/claude-501/slug/runtime/tasks/b1.output", OwnerSourceLiveLaunch)
	s.resetOwners()

	got := s.resolveOwnerResult(ownerTarget("/tmp/claude-501/slug/runtime/tasks/b1.output", "b1"))
	if got.Outcome != OwnerUnresolvedAwaitingOwner || !got.MayArrive() {
		t.Fatalf("resolution = %+v, want cleared retryable mapping", got)
	}
}

func TestOpenTaskIndexTracksAuthoritativeLifecycle(t *testing.T) {
	s, _ := ownerSidecar(t)
	started := &corev1.Event{SessionId: "S1", Payload: &corev1.Event_TaskStarted{TaskStarted: &corev1.TaskStarted{
		TaskId: "b1", Kind: corev1.TaskKind_TASK_KIND_SHELL, OutputPath: "/tmp/b1.output",
	}}}
	s.applyLifecycle([]*corev1.Event{started}, 1000)
	if !s.taskOpen("b1") {
		t.Fatal("live TaskStarted did not mark task open")
	}

	ended := &corev1.Event{SessionId: "S1", Payload: &corev1.Event_TaskEnded{TaskEnded: &corev1.TaskEnded{
		TaskId: "b1", Status: corev1.TerminalStatus_TERMINAL_STATUS_DONE,
	}}}
	s.applyLifecycle([]*corev1.Event{ended}, 2000)
	if s.taskOpen("b1") {
		t.Fatal("terminal TaskEnded did not clear open task")
	}
}

func TestOpenTaskIndexRejectsMissingLifecycleIdentity(t *testing.T) {
	s, read := ownerSidecar(t)
	s.markTaskOpen("", OwnerSourceLiveLaunch)
	s.markTaskClosed("")
	if len(s.openTasks) != 0 {
		t.Fatalf("invalid lifecycle observation mutated open tasks: %v", s.openTasks)
	}
	if got := linesContaining(read(), "observation rejected missing task id"); len(got) != 2 {
		t.Fatalf("invalid lifecycle logs = %v, want two errors", got)
	}
}

func TestOwnerResolutionUsesUniqueTaskOnlyWhenNoContradictionExists(t *testing.T) {
	s, _ := ownerSidecar(t)
	s.noteTaskOwner("b1", "S1", "", OwnerSourceLiveLaunch)

	got := s.resolveOwnerResult(ownerTarget("/tmp/claude-501/other-runtime/tasks/b1.output", "b1"))
	if got.Outcome != OwnerResolvedTask || got.SessionID != "S1" || got.Source != OwnerSourceLiveLaunch {
		t.Fatalf("resolution = %+v, want unique live task owner", got)
	}
}

func TestOwnerResolutionAwaitingOwnerCanResolveAfterLiveObservation(t *testing.T) {
	s, _ := ownerSidecar(t)
	target := ownerTarget("/tmp/claude-501/slug/runtime/tasks/b1.output", "b1")

	before := s.resolveOwnerResult(target)
	if before.Outcome != OwnerUnresolvedAwaitingOwner || !before.MayArrive() {
		t.Fatalf("before = %+v, want retryable unresolved", before)
	}
	s.noteTaskOwner("b1", "S1", target.Path, OwnerSourceLiveLaunch)
	after := s.resolveOwnerResult(target)
	if after.Outcome != OwnerResolvedPath || after.SessionID != "S1" {
		t.Fatalf("after = %+v, want exact path S1", after)
	}
}

func TestOwnerResolutionRejectsMalformedSpoolTarget(t *testing.T) {
	s, read := ownerSidecar(t)
	got := s.resolveOwnerResult(ownerTarget("", ""))
	if got.Outcome != OwnerUnresolvedInvalid || got.MayArrive() {
		t.Fatalf("resolution = %+v, want terminal invalid", got)
	}
	if lines := linesContaining(read(), "rejected invalid spool target"); len(lines) != 1 {
		t.Fatalf("invalid resolution logs = %v, want one", lines)
	}
}

// symlinkedSpoolDir returns two spellings of one tasks directory: the real one
// and one reached through a symlinked ancestor, mirroring macOS /tmp.
func symlinkedSpoolDir(t *testing.T) (real string, linked string) {
	t.Helper()
	base := t.TempDir()
	real = filepath.Join(base, "private", "runtime", "tasks")
	if err := os.MkdirAll(real, 0o755); err != nil {
		t.Fatalf("mkdir spool dir: %v", err)
	}
	if err := os.Symlink(filepath.Join(base, "private"), filepath.Join(base, "link")); err != nil {
		t.Fatalf("symlink spool root: %v", err)
	}
	return real, filepath.Join(base, "link", "runtime", "tasks")
}

func TestOwnerResolutionResolvesSymlinkedSpellingOfRecordedPath(t *testing.T) {
	s, _ := ownerSidecar(t)
	realDir, linkedDir := symlinkedSpoolDir(t)
	recorded := filepath.Join(realDir, "b1.output")
	if !s.noteTaskOwner("b1", "S1", recorded, OwnerSourceLiveLaunch) {
		t.Fatal("did not record live task owner")
	}

	got := s.resolveOwnerResult(ownerTarget(filepath.Join(linkedDir, "b1.output"), "b1"))

	if !got.Resolved() || got.SessionID != "S1" {
		t.Fatalf("resolution = %+v, want S1 resolved through symlinked spelling", got)
	}
}

func TestOwnerResolutionResolvesTaskOnlyAssociationAcrossSymlinkedSpellings(t *testing.T) {
	s, read := ownerSidecar(t)
	realDir, linkedDir := symlinkedSpoolDir(t)
	s.owners["b1"] = "S1"
	s.ownerSource["b1"] = OwnerSourceDurableOpenTask
	s.ownerTaskOutput["b1"] = normalizeOwnerOutputPath(filepath.Join(realDir, "b1.output"))

	got := s.resolveOwnerResult(ownerTarget(filepath.Join(linkedDir, "b1.output"), "b1"))

	if got.Outcome != OwnerResolvedTask || got.SessionID != "S1" {
		t.Fatalf("resolution = %+v, want task association across symlinked spellings", got)
	}
	if lines := linesContaining(read(), "different authoritative output path"); len(lines) != 0 {
		t.Fatalf("conflict logs = %v, want none", lines)
	}
}

func TestNormalizeOwnerOutputPathResolvesSymlinkForMissingSpoolFile(t *testing.T) {
	realDir, linkedDir := symlinkedSpoolDir(t)

	got := normalizeOwnerOutputPath(filepath.Join(linkedDir, "not-created-yet.output"))

	if want := normalizeOwnerOutputPath(realDir) + "/not-created-yet.output"; got != want {
		t.Fatalf("normalized = %q, want %q", got, want)
	}
}

func TestNormalizeOwnerOutputPathKeepsFullyMissingPath(t *testing.T) {
	want := "/agent-repl-absent-root/runtime/tasks/b1.output"

	if got := normalizeOwnerOutputPath(want); got != want {
		t.Fatalf("normalized = %q, want %q unchanged", got, want)
	}
}

func TestOwnerResolutionStillRejectsGenuinelyDifferentAuthoritativePath(t *testing.T) {
	s, read := ownerSidecar(t)
	realDir, _ := symlinkedSpoolDir(t)
	s.owners["b1"] = "S1"
	s.ownerSource["b1"] = OwnerSourceDurableOpenTask
	s.ownerTaskOutput["b1"] = normalizeOwnerOutputPath(filepath.Join(realDir, "b1.output"))

	got := s.resolveOwnerResult(ownerTarget(filepath.Join(realDir, "other.output"), "b1"))

	if got.Outcome != OwnerUnresolvedConflict || got.Resolved() {
		t.Fatalf("resolution = %+v, want conflict for a genuinely different file", got)
	}
	if lines := linesContaining(read(), "different authoritative output path"); len(lines) != 1 {
		t.Fatalf("conflict logs = %v, want one", lines)
	}
}

func TestOwnerResolutionStillRejectsTwoSessionsClaimingOneTaskAcrossSymlinkedSpellings(t *testing.T) {
	s, read := ownerSidecar(t)
	realDir, linkedDir := symlinkedSpoolDir(t)
	s.noteTaskOwner("b1", "S1", filepath.Join(realDir, "b1.output"), OwnerSourceLiveLaunch)
	s.noteTaskOwner("b1", "S2", filepath.Join(linkedDir, "b1.output"), OwnerSourceLiveLaunch)

	got := s.resolveOwnerResult(ownerTarget(filepath.Join(realDir, "b1.output"), "b1"))

	if got.Outcome != OwnerUnresolvedConflict || got.Resolved() {
		t.Fatalf("resolution = %+v, want conflict for two sessions claiming one task", got)
	}
	if lines := linesContaining(read(), "CONFLICTING owner"); len(lines) != 1 {
		t.Fatalf("conflict logs = %v, want one", lines)
	}
}
