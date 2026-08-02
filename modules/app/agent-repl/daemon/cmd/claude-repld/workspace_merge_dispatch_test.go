package main

import (
	"context"
	"errors"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/server"
	workspacecreate "claude-repld/internal/workspace/create"
	"claude-repld/internal/workspace/geometry"
)

// stubGeometry answers the merge dispatch's exact-project_dir lookup.
type stubGeometry struct {
	records map[string]geometry.Record
}

func (s stubGeometry) Lookup(_ context.Context, workspace string) (geometry.Record, bool, error) {
	rec, ok := s.records[workspace]
	return rec, ok, nil
}

// stubMergeCommand stands in for the frontend command handler the dispatch
// delegates to once a project_dir has resolved.
type stubMergeCommand struct {
	merged []string
	err    error
}

func (s *stubMergeCommand) MergeWorkspace(_ context.Context, workspace, _ string, _ *frontendv1.MergeWorkspaceCmd) error {
	s.merged = append(s.merged, workspace)
	return s.err
}

func newBoundBinding(t *testing.T, records map[string]geometry.Record) (*WorkspaceMergeDispatchBinding, *stubMergeCommand) {
	t.Helper()
	merges := &stubMergeCommand{}
	dispatch, err := server.NewMergeDispatch(stubGeometry{records: records}, merges, func(string, ...any) {})
	if err != nil {
		t.Fatalf("NewMergeDispatch: %v", err)
	}
	b := &WorkspaceMergeDispatchBinding{Logf: func(string, ...any) {}}
	b.SetTarget(dispatch)
	return b, merges
}

// A resolved merge reaches the merge command surface keyed by its project_dir.
func TestWorkspaceMergeDispatchBindingRoutesAResolvedMerge(t *testing.T) {
	// Arrange
	b, merges := newBoundBinding(t, map[string]geometry.Record{
		"/worktrees/ws1": {Workspace: "/worktrees/ws1", SourceBranch: "DWC/x", SourceDir: "/worktrees/ws1", TargetDir: "/repo", Origin: geometry.OriginCreated},
	})

	// Act
	err := b.DispatchMerge(context.Background(), workspacecreate.MergeCommand{Workspace: "DWC/x", ProjectDir: "/worktrees/ws1", ID: "file:0"})

	// Assert
	if err != nil {
		t.Fatalf("DispatchMerge = %v", err)
	}
	if len(merges.merged) != 1 || merges.merged[0] != "/worktrees/ws1" {
		t.Fatalf("merged = %v, want the project_dir key", merges.merged)
	}
}

// THE SENTINEL TRANSLATION. The ingress quarantines on its own sentinel, so an
// unrecorded project_dir must arrive wearing it.
func TestWorkspaceMergeDispatchBindingTranslatesTheRejectionSentinel(t *testing.T) {
	// Arrange
	b, _ := newBoundBinding(t, map[string]geometry.Record{})

	// Act
	err := b.DispatchMerge(context.Background(), workspacecreate.MergeCommand{Workspace: "DWC/ghost", ProjectDir: "/worktrees/ghost"})

	// Assert
	if !errors.Is(err, workspacecreate.ErrUnknownMergeWorkspace) {
		t.Fatalf("DispatchMerge = %v, want the ingress rejection sentinel", err)
	}
}

// A structural failure must NOT wear the rejection sentinel, or the emitter's
// file would be quarantined for the daemon's own fault.
func TestWorkspaceMergeDispatchBindingLeavesAStructuralFailureUntranslated(t *testing.T) {
	// Arrange
	b, merges := newBoundBinding(t, map[string]geometry.Record{
		"/worktrees/ws1": {Workspace: "/worktrees/ws1", SourceBranch: "DWC/x", SourceDir: "/worktrees/ws1", TargetDir: "/repo", Origin: geometry.OriginCreated},
	})
	merges.err = errors.New("queue is unreachable")

	// Act
	err := b.DispatchMerge(context.Background(), workspacecreate.MergeCommand{Workspace: "DWC/x", ProjectDir: "/worktrees/ws1"})

	// Assert
	if err == nil || errors.Is(err, workspacecreate.ErrUnknownMergeWorkspace) {
		t.Fatalf("DispatchMerge = %v, want an untranslated structural failure", err)
	}
}

// An unbound binding is a construction-order bug: it fails structurally so the
// claim is retried rather than the merge being blamed on its emitter.
func TestWorkspaceMergeDispatchBindingFailsStructurallyBeforeSetTarget(t *testing.T) {
	// Arrange
	b := &WorkspaceMergeDispatchBinding{Logf: func(string, ...any) {}}

	// Act
	err := b.DispatchMerge(context.Background(), workspacecreate.MergeCommand{Workspace: "DWC/x", ProjectDir: "/worktrees/ws1"})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "not wired") {
		t.Fatalf("DispatchMerge = %v, want an unwired-binding refusal", err)
	}
	if errors.Is(err, workspacecreate.ErrUnknownMergeWorkspace) {
		t.Fatal("an unwired binding must not look like a rejected merge")
	}
}
