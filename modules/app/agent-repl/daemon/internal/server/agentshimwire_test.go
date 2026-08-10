// agentshimwire_test.go covers what WIRING the daemon's boot does to the
// filesystem. The boot runs merge's orphan rebase worktree sweep, and a sweep
// that resolved its own root deleted the LIVE daemon's rebase worktrees the
// moment a test constructed this wiring — including the tree the merge gate's
// own `go test ./...` was running inside. The root is therefore an injected,
// required field, and these are its two tests.
package server

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/progress"
	"claude-repld/internal/workspace/geometry"
)

// bootSweepConfig is the production wiring's config with everything but the
// rebase root filled in, so each test names ONLY the field it is about.
func bootSweepConfig(t *testing.T, root string) AgentShimConfig {
	t.Helper()
	return AgentShimConfig{
		RebaseRoot:        root,
		Resumes:           &fakeResumes{},
		SSM:               openTestSSM(t, openTestRegistry(t)),
		Progress:          progress.New(progress.Options{Logf: func(string, ...any) {}}),
		Prompts:           &fakePrompts{},
		Turns:             &fakePrompts{},
		Lifecycle:         &fakeLifecycle{},
		SessionDeaths:     stubSessionDeaths{},
		SessionCommands:   &SessionCommandBinding{},
		WorkspaceCreation: newFakeWorkspaceCreation(),
		MergeLease:        stubMergeLease{},
		MergeQueue:        newTestMergeQueue(t),
		LogVerbosef:       t.Logf,
	}
}

// arrangeLeftoverRebaseWorktree plants a directory shaped exactly like the one a
// daemon killed mid-merge leaves behind.
func arrangeLeftoverRebaseWorktree(t *testing.T, dir, suffix string) string {
	t.Helper()
	leftover := filepath.Join(dir, "agent-repl-merge-rebase-"+suffix)
	if err := os.MkdirAll(filepath.Join(leftover, "rebase"), 0o755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}
	return leftover
}

func TestWireAgentShimSweepsOnlyItsInjectedRebaseRoot(t *testing.T) {
	// Arrange — the production incident in miniature: a live daemon's rebase
	// worktree sits in the REAL process temp dir (stood in for here, and
	// installed as $TMPDIR so an implicit fallback would reach it), while this
	// test wires the daemon with a root of its own that holds a leftover.
	realTmp := t.TempDir()
	production := arrangeLeftoverRebaseWorktree(t, realTmp, "live-production-merge")
	t.Setenv("TMPDIR", realTmp)
	root := t.TempDir()
	leftover := arrangeLeftoverRebaseWorktree(t, root, "boot-orphan")

	// Act — the boot sweep runs inside WireAgentShim.
	shim, err := WireAgentShim(bootSweepConfig(t, root))
	if err != nil {
		t.Fatalf("WireAgentShim: %v", err)
	}
	defer shim.Close()

	// Assert — the production tree outside the injected root survives, and the
	// leftover inside it is the proof the sweep actually ran.
	if _, err := os.Lstat(production); err != nil {
		t.Fatalf("Lstat(%s) = %v, want the live daemon's rebase worktree untouched by a TEST's boot wiring", production, err)
	}
	if _, err := os.Lstat(leftover); !os.IsNotExist(err) {
		t.Fatalf("Lstat(%s) = %v, want the leftover inside the injected root swept", leftover, err)
	}
}

func TestWireAgentShimRejectsAnEmptyRebaseRoot(t *testing.T) {
	// Arrange — the wiring a caller that forgot to inject the root produces.
	cfg := bootSweepConfig(t, "")

	// Act
	_, err := WireAgentShim(cfg)

	// Assert — LOUD at construction. A default here is what made the incident
	// representable, so its absence must fail rather than pick a directory.
	if err == nil || !strings.Contains(err.Error(), "RebaseRoot") {
		t.Fatalf("WireAgentShim err = %v, want a refusal naming the missing RebaseRoot", err)
	}
}

// stubGeometrySource is a distinguishable merge-geometry map. Identity is what
// the two tests below assert on: which CONSUMER got which map is the whole
// point of splitting MergeCommandGeometry off MergeGeometry.
type stubGeometrySource struct{ name string }

func (s stubGeometrySource) Lookup(context.Context, string) (geometry.Record, bool, error) {
	return geometry.Record{}, false, nil
}

// TestWireAgentShimGatesOnlyTheMergeCommandPathOnTheGeometryBackfill proves the
// boot repair's gate reaches the merge command path and NOT the resolved-view
// publisher: main defers the geometry backfill off the serial boot path so the
// frontend serves connect snapshots immediately, and a view made to wait on
// that repair would put it right back in front of the listener.
func TestWireAgentShimGatesOnlyTheMergeCommandPathOnTheGeometryBackfill(t *testing.T) {
	// Arrange
	cfg := bootSweepConfig(t, t.TempDir())
	raw := stubGeometrySource{name: "raw"}
	gated := stubGeometrySource{name: "gated"}
	cfg.MergeGeometry = raw
	cfg.MergeCommandGeometry = gated

	// Act
	shim, err := WireAgentShim(cfg)
	if err != nil {
		t.Fatalf("WireAgentShim: %v", err)
	}
	defer shim.Close()

	// Assert
	if got := shim.MergeDispatch.geometry; got != MergeGeometrySource(gated) {
		t.Fatalf("merge dispatch geometry = %#v, want the gated map", got)
	}
	if got := shim.WorkspaceViews.branches; got != WorkspaceBranchSource(raw) {
		t.Fatalf("workspace view branch source = %#v, want the ungated map so a snapshot never waits on the backfill", got)
	}
}

// TestWireAgentShimDefaultsTheMergeCommandGeometryToTheViewMap covers the
// harness wiring: a caller that names one map must get that map everywhere,
// never a nil merge-command source that refuses every bare merge.
func TestWireAgentShimDefaultsTheMergeCommandGeometryToTheViewMap(t *testing.T) {
	// Arrange
	cfg := bootSweepConfig(t, t.TempDir())
	raw := stubGeometrySource{name: "raw"}
	cfg.MergeGeometry = raw

	// Act
	shim, err := WireAgentShim(cfg)
	if err != nil {
		t.Fatalf("WireAgentShim: %v", err)
	}
	defer shim.Close()

	// Assert
	if got := shim.MergeDispatch.geometry; got != MergeGeometrySource(raw) {
		t.Fatalf("merge dispatch geometry = %#v, want the single supplied map", got)
	}
}

// blockedGeometrySource is the test seam for a deferred boot phase that has not
// finished: it never answers. It stands in for main's backfill gate before the
// backfill completes.
type blockedGeometrySource struct{}

func (blockedGeometrySource) Lookup(ctx context.Context, _ string) (geometry.Record, bool, error) {
	<-ctx.Done()
	return geometry.Record{}, false, ctx.Err()
}

// TestWireAgentShimPublishesAViewWhileTheDeferredGeometryBackfillIsBlocked is
// the defect's own test: the frontend must be answerable while the deferred
// boot work is still running. A view resolved through the merge-command map
// would hang here forever.
func TestWireAgentShimPublishesAViewWhileTheDeferredGeometryBackfillIsBlocked(t *testing.T) {
	// Arrange
	cfg := bootSweepConfig(t, t.TempDir())
	cfg.MergeCommandGeometry = blockedGeometrySource{}
	shim, err := WireAgentShim(cfg)
	if err != nil {
		t.Fatalf("WireAgentShim: %v", err)
	}
	defer shim.Close()

	// Act
	published := make(chan struct{})
	go func() {
		defer close(published)
		shim.WorkspaceViews.PublishState(&frontendv1.WorkspaceState{Workspace: "/ws"})
	}()

	// Assert
	<-published
}
