package main

import (
	"context"
	"path/filepath"
	"strings"
	"testing"

	"claude-repld/internal/registry"
	"claude-repld/internal/statedb"
	workspacecreate "claude-repld/internal/workspace/create"
	"claude-repld/internal/workspace/geometry"
)

func newGeometryStore(t *testing.T) *geometry.Store {
	t.Helper()
	db, err := statedb.Open(filepath.Join(t.TempDir(), "state.db"))
	if err != nil {
		t.Fatalf("statedb.Open: %v", err)
	}
	t.Cleanup(func() { db.Close() })
	store, err := geometry.Open(db, func(string, ...any) {})
	if err != nil {
		t.Fatalf("geometry.Open: %v", err)
	}
	return store
}

func TestRecordedGeometryTargetsTheNominatedSourceWorkspace(t *testing.T) {
	// Arrange — a workspace spawned FROM another workspace merges back into it.
	store := newGeometryStore(t)
	recorder := daemonGeometryRecorder{Store: store, Logf: func(string, ...any) {}}
	job := workspacecreate.Job{
		ID:           "j1",
		WorktreePath: "/worktrees/child",
		Branch:       "DWC/child",
		Request:      workspacecreate.Request{Name: "DWC/child", GitRoot: "/repo", SourceWorkspace: "parent", SourceDir: "/worktrees/parent"},
	}

	// Act.
	if err := recorder.RecordWorkspaceGeometry(context.Background(), job); err != nil {
		t.Fatalf("RecordWorkspaceGeometry: %v", err)
	}

	// Assert.
	got, found, err := store.Lookup(context.Background(), "/worktrees/child")
	if err != nil || !found {
		t.Fatalf("Lookup found=%t err=%v", found, err)
	}
	want := geometry.Record{Workspace: "/worktrees/child", SourceBranch: "DWC/child", SourceDir: "/worktrees/child", TargetDir: "/worktrees/parent", Origin: geometry.OriginCreated}
	if got != want {
		t.Fatalf("record = %#v, want %#v", got, want)
	}
}

func TestRecordedGeometryTargetsTheGitRootWhenNoSourceWorkspaceWasNamed(t *testing.T) {
	// Arrange — a workspace cut straight off the repository root.
	store := newGeometryStore(t)
	recorder := daemonGeometryRecorder{Store: store, Logf: func(string, ...any) {}}
	job := workspacecreate.Job{
		ID:           "j2",
		WorktreePath: "/worktrees/solo",
		Branch:       "DWC/solo",
		Request:      workspacecreate.Request{Name: "DWC/solo", GitRoot: "/repo"},
	}

	// Act.
	if err := recorder.RecordWorkspaceGeometry(context.Background(), job); err != nil {
		t.Fatalf("RecordWorkspaceGeometry: %v", err)
	}

	// Assert.
	got, _, err := store.Lookup(context.Background(), "/worktrees/solo")
	if err != nil {
		t.Fatal(err)
	}
	if got.TargetDir != "/repo" {
		t.Fatalf("target = %q, want the git root", got.TargetDir)
	}
}

func TestRecordingRefusesAJobWithNoPersistedWorktreeIdentity(t *testing.T) {
	// Arrange — the manager must never reach the recorder before the plan is
	// checkpointed, so this is an invariant violation and fails hard.
	store := newGeometryStore(t)
	recorder := daemonGeometryRecorder{Store: store, Logf: func(string, ...any) {}}
	job := workspacecreate.Job{ID: "j3", Request: workspacecreate.Request{Name: "DWC/x", GitRoot: "/repo"}}

	// Act.
	err := recorder.RecordWorkspaceGeometry(context.Background(), job)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "no persisted worktree identity") {
		t.Fatalf("RecordWorkspaceGeometry error = %v, want an identity refusal", err)
	}
}

func TestRecordingRefusesARelativeMergeTarget(t *testing.T) {
	// Arrange — the skill contract allows a leading ~/ and nothing else.
	store := newGeometryStore(t)
	recorder := daemonGeometryRecorder{Store: store, Logf: func(string, ...any) {}}
	job := workspacecreate.Job{
		ID:           "j4",
		WorktreePath: "/worktrees/rel",
		Branch:       "DWC/rel",
		Request:      workspacecreate.Request{Name: "DWC/rel", GitRoot: "some/relative/root"},
	}

	// Act.
	err := recorder.RecordWorkspaceGeometry(context.Background(), job)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "absolute path") {
		t.Fatalf("RecordWorkspaceGeometry error = %v, want a path refusal", err)
	}
	if _, found, lookupErr := store.Lookup(context.Background(), "/worktrees/rel"); lookupErr != nil || found {
		t.Fatalf("a refused job was recorded: found=%t err=%v", found, lookupErr)
	}
}

func TestRecordingRefusesAJobWithNoMergeTargetAtAll(t *testing.T) {
	// Arrange.
	store := newGeometryStore(t)
	recorder := daemonGeometryRecorder{Store: store, Logf: func(string, ...any) {}}
	job := workspacecreate.Job{
		ID:           "j5",
		WorktreePath: "/worktrees/orphan",
		Branch:       "DWC/orphan",
		Request:      workspacecreate.Request{Name: "DWC/orphan"},
	}

	// Act.
	err := recorder.RecordWorkspaceGeometry(context.Background(), job)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "no merge target") {
		t.Fatalf("RecordWorkspaceGeometry error = %v, want a missing-target refusal", err)
	}
}

func TestRecordingRefusesAnUnconfiguredRecorder(t *testing.T) {
	// Arrange.
	recorder := daemonGeometryRecorder{}

	// Act.
	err := recorder.RecordWorkspaceGeometry(context.Background(), workspacecreate.Job{ID: "j6"})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "not fully configured") {
		t.Fatalf("RecordWorkspaceGeometry error = %v, want a configuration refusal", err)
	}
}

func TestBackfillCandidatesAreTheLiveSessionWorkspaces(t *testing.T) {
	// Arrange — a terminal session names a workspace nobody can merge, and two
	// sessions in one workspace are one candidate.
	db, err := statedb.Open(filepath.Join(t.TempDir(), "registry.db"))
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { db.Close() })
	reg := registry.OpenWith(registry.Options{DB: db, Logf: func(string, ...any) {}})
	if err := reg.Prepare(); err != nil {
		t.Fatal(err)
	}
	reg.Put(registry.Record{SessionID: "s1", CWD: "/worktrees/alpha"})
	reg.Put(registry.Record{SessionID: "s2", CWD: "/worktrees/alpha/"})
	reg.Put(registry.Record{SessionID: "s3", CWD: "/worktrees/beta"})
	reg.Put(registry.Record{SessionID: "s4", CWD: "/worktrees/dead", Terminal: true})

	// Act.
	got := registryGeometryLister{Reg: reg}.GeometryBackfillCandidates()

	// Assert.
	want := []string{"/worktrees/alpha", "/worktrees/beta"}
	if len(got) != len(want) {
		t.Fatalf("candidates = %v, want %v", got, want)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("candidates = %v, want %v", got, want)
		}
	}
}
