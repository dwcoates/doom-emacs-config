package server

import (
	"context"
	"errors"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/workspace/geometry"
)

// recordingMergeCommand captures what the dispatch handed the command surface.
type recordingMergeCommand struct {
	workspaces []string
	requestIDs []string
	names      []string
	err        error
}

func (r *recordingMergeCommand) MergeWorkspace(_ context.Context, workspace, requestID string, cmd *frontendv1.MergeWorkspaceCmd) error {
	r.workspaces = append(r.workspaces, workspace)
	r.requestIDs = append(r.requestIDs, requestID)
	r.names = append(r.names, cmd.GetWorkspaceName())
	return r.err
}

func newMergeDispatch(t *testing.T, records map[string]geometry.Record) (*MergeDispatch, *recordingMergeCommand, *fakeGeometry) {
	t.Helper()
	g := &fakeGeometry{records: records}
	merges := &recordingMergeCommand{}
	d, err := NewMergeDispatch(g, merges, func(string, ...any) {})
	if err != nil {
		t.Fatalf("NewMergeDispatch: %v", err)
	}
	return d, merges, g
}

// The project_dir IS the key: it is what the merge is filed under, never the
// display name that rode alongside it.
func TestMergeDispatchKeysTheMergeOnProjectDir(t *testing.T) {
	// Arrange
	d, merges, _ := newMergeDispatch(t, map[string]geometry.Record{
		"/worktrees/ws1": {Workspace: "/worktrees/ws1", SourceBranch: "DWC/x", SourceDir: "/worktrees/ws1", TargetDir: "/repo", Origin: geometry.OriginCreated},
	})

	// Act
	err := d.DispatchMerge(context.Background(), "DWC/x", "/worktrees/ws1", "req-1")

	// Assert
	if err != nil {
		t.Fatalf("DispatchMerge = %v", err)
	}
	if len(merges.workspaces) != 1 || merges.workspaces[0] != "/worktrees/ws1" {
		t.Fatalf("merged workspaces = %v, want the project_dir key", merges.workspaces)
	}
}

// The display name is carried, not resolved.
func TestMergeDispatchCarriesTheDisplayNameWithoutResolvingIt(t *testing.T) {
	// Arrange
	d, merges, _ := newMergeDispatch(t, map[string]geometry.Record{
		"/worktrees/ws1": {Workspace: "/worktrees/ws1", SourceBranch: "DWC/x", SourceDir: "/worktrees/ws1", TargetDir: "/repo", Origin: geometry.OriginCreated},
	})

	// Act
	if err := d.DispatchMerge(context.Background(), "a name that matches nothing", "/worktrees/ws1", "req-1"); err != nil {
		t.Fatalf("DispatchMerge = %v", err)
	}

	// Assert
	if len(merges.names) != 1 || merges.names[0] != "a name that matches nothing" {
		t.Fatalf("carried names = %v, want the emitter's name passed through verbatim", merges.names)
	}
}

// An unrecorded project_dir is a REJECTION, so the ingress can quarantine the
// file rather than retry it forever.
func TestMergeDispatchRejectsAnUnrecordedProjectDir(t *testing.T) {
	// Arrange
	d, merges, _ := newMergeDispatch(t, map[string]geometry.Record{})

	// Act
	err := d.DispatchMerge(context.Background(), "DWC/ghost", "/worktrees/ghost", "req-1")

	// Assert
	if !errors.Is(err, ErrMergeWorkspaceUnrecorded) {
		t.Fatalf("DispatchMerge = %v, want ErrMergeWorkspaceUnrecorded", err)
	}
	if len(merges.workspaces) != 0 {
		t.Fatalf("merged %v, want nothing enqueued for an unrecorded workspace", merges.workspaces)
	}
}

// A relative project_dir cannot be a key at all.
func TestMergeDispatchRejectsARelativeProjectDir(t *testing.T) {
	// Arrange
	d, _, _ := newMergeDispatch(t, map[string]geometry.Record{})

	// Act
	err := d.DispatchMerge(context.Background(), "DWC/x", "worktrees/ws1", "req-1")

	// Assert
	if !errors.Is(err, ErrMergeWorkspaceUnrecorded) {
		t.Fatalf("DispatchMerge = %v, want ErrMergeWorkspaceUnrecorded", err)
	}
}

// The key is CLEANED, never matched loosely: "/a//b/." and "/a/b" are one
// workspace, and nothing else is.
func TestMergeDispatchLooksUpTheCleanedProjectDir(t *testing.T) {
	// Arrange
	d, _, g := newMergeDispatch(t, map[string]geometry.Record{
		"/worktrees/ws1": {Workspace: "/worktrees/ws1", SourceBranch: "DWC/x", SourceDir: "/worktrees/ws1", TargetDir: "/repo", Origin: geometry.OriginCreated},
	})

	// Act
	if err := d.DispatchMerge(context.Background(), "DWC/x", "/worktrees//ws1/.", "req-1"); err != nil {
		t.Fatalf("DispatchMerge = %v", err)
	}

	// Assert
	if len(g.lookups) != 1 || g.lookups[0] != "/worktrees/ws1" {
		t.Fatalf("lookups = %v, want the cleaned path", g.lookups)
	}
}

// A broken map is STRUCTURAL, not a rejection: retrying is right, quarantining
// the emitter's file is not.
func TestMergeDispatchPropagatesAGeometryLookupFailureStructurally(t *testing.T) {
	// Arrange
	d, _, g := newMergeDispatch(t, map[string]geometry.Record{})
	g.err = errors.New("state store is unreadable")

	// Act
	err := d.DispatchMerge(context.Background(), "DWC/x", "/worktrees/ws1", "req-1")

	// Assert
	if err == nil || errors.Is(err, ErrMergeWorkspaceUnrecorded) {
		t.Fatalf("DispatchMerge = %v, want a structural failure that is not a rejection", err)
	}
}

// A daemon with no map REFUSES rather than guessing, and says so.
func TestMergeDispatchRefusesWhenTheGeometryMapIsNotWired(t *testing.T) {
	// Arrange
	merges := &recordingMergeCommand{}
	d, err := NewMergeDispatch(nil, merges, func(string, ...any) {})
	if err != nil {
		t.Fatalf("NewMergeDispatch: %v", err)
	}

	// Act
	err = d.DispatchMerge(context.Background(), "DWC/x", "/worktrees/ws1", "req-1")

	// Assert
	if err == nil || !strings.Contains(err.Error(), "merge-geometry record is not wired") {
		t.Fatalf("DispatchMerge = %v, want a loud unwired-map refusal", err)
	}
}

// The merge command's own failure reaches the caller unchanged: the ingress
// must be able to tell a refused enqueue from a rejected entry.
func TestMergeDispatchPropagatesTheMergeCommandFailure(t *testing.T) {
	// Arrange
	d, merges, _ := newMergeDispatch(t, map[string]geometry.Record{
		"/worktrees/ws1": {Workspace: "/worktrees/ws1", SourceBranch: "DWC/x", SourceDir: "/worktrees/ws1", TargetDir: "/repo", Origin: geometry.OriginCreated},
	})
	merges.err = errors.New("queue refused the request")

	// Act
	err := d.DispatchMerge(context.Background(), "DWC/x", "/worktrees/ws1", "req-1")

	// Assert
	if err == nil || !strings.Contains(err.Error(), "queue refused the request") {
		t.Fatalf("DispatchMerge = %v, want the enqueue failure surfaced", err)
	}
}

// A dispatch without a command surface could only drop the merge.
func TestNewMergeDispatchRequiresACommandSurface(t *testing.T) {
	// Arrange / Act
	_, err := NewMergeDispatch(&fakeGeometry{}, nil, func(string, ...any) {})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "merge command surface") {
		t.Fatalf("NewMergeDispatch = %v, want a command-surface refusal", err)
	}
}
