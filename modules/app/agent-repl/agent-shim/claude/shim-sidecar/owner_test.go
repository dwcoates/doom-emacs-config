package main

import (
	"testing"

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
	if !got.Resolved() || got.Outcome != OwnerResolvedPath || got.SessionID != "S1" || got.OutputPath != path {
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
