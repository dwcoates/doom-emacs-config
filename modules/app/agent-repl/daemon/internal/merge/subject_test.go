package merge

import (
	"context"
	"path/filepath"
	"testing"

	"claude-repld/internal/gitclient"
	"claude-repld/internal/wsm"
)

// subjectOf resolves the harness workspace's subject for one source.
func subjectOf(t *testing.T, h *harness, source wsm.MergeSource) subject {
	t.Helper()
	r := &run{o: h.o, ws: theWorkspace, source: source, lease: wsm.Lease{ID: "lease-1"}}
	s, err := h.o.resolveSubject(context.Background(), r)
	if err != nil {
		t.Fatalf("resolveSubject: %v", err)
	}
	return s
}

func TestTheOwnBranchIsRebasedInTheRequestersWorktreeAndClosesIt(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	s := subjectOf(t, h, ownBranch)

	// Assert.
	want := subject{branch: "feature", dir: h.sourceD, targetDir: h.targetD, closes: theWorkspace}
	if s != want {
		t.Fatalf("subject = %+v, want %+v", s, want)
	}
}

func TestAnOwnBranchKeptOpenClosesNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	s := subjectOf(t, h, wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch, KeepOpen: true})

	// Assert.
	if s.closes != "" {
		t.Fatalf("subject closes %q, want nothing kept open", s.closes)
	}
}

func TestAnotherWorkspacesBranchIsRebasedInItsWorktreeAndClosesIt(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	dir := h.registerOther(otherWorkspace, "ws-two", "other-branch")

	// Act.
	s := subjectOf(t, h, wsm.MergeSource{Kind: wsm.MergeSourceWorkspace, Workspace: otherWorkspace})

	// Assert.
	want := subject{branch: "other-branch", dir: dir, targetDir: h.targetD, closes: otherWorkspace, other: otherWorkspace}
	if s != want {
		t.Fatalf("subject = %+v, want %+v", s, want)
	}
}

func TestABranchWithAWorktreeIsRebasedInIt(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.git.worktrees = []gitclient.Worktree{{Dir: h.sourceD, Branch: "feature"}, {Dir: "/wt/agent", Branch: "agent-1/fix"}}

	// Act.
	s := subjectOf(t, h, wsm.MergeSource{Kind: wsm.MergeSourceBranch, Branch: "agent-1/fix"})

	// Assert.
	if s.dir != "/wt/agent" || s.made || s.closes != "" {
		t.Fatalf("subject = %+v, want the branch's own worktree, not made, closing nothing", s)
	}
	if len(h.git.addedWorktrees) != 0 {
		t.Fatalf("a worktree was made for a branch that has one: %v", h.git.addedWorktrees)
	}
}

func TestABranchWithNoWorktreeGetsOneUnderTheStateDirectory(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	s := subjectOf(t, h, wsm.MergeSource{Kind: wsm.MergeSourceBranch, Branch: "agent-1/fix"})

	// Assert.
	want := filepath.Join(h.stateDir, mergeWorktreesDir, "lease-1")
	if s.dir != want || !s.made {
		t.Fatalf("subject = %+v, want a made worktree at %s", s, want)
	}
	if len(h.git.addedWorktrees) != 1 || h.git.addedWorktrees[0] != want {
		t.Fatalf("worktrees added = %v, want %s", h.git.addedWorktrees, want)
	}
}

func TestABranchLandsInTheRepositorysMainWorktree(t *testing.T) {
	// Arrange: the fake answers a directory's own path as its main worktree.
	h := newHarness(t)

	// Act.
	s := subjectOf(t, h, wsm.MergeSource{Kind: wsm.MergeSourceBranch, Branch: "agent-1/fix"})

	// Assert.
	if s.targetDir != h.sourceD {
		t.Fatalf("target = %s, want the main worktree %s", s.targetDir, h.sourceD)
	}
}

func TestABranchMergedUpstreamUpdatesTheMainWorktreeAndClosesTheRequester(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	s := subjectOf(t, h, wsm.MergeSource{Kind: wsm.MergeSourceMergedUpstream})

	// Assert.
	if s.targetDir != h.sourceD || s.closes != theWorkspace || s.dir != "" {
		t.Fatalf("subject = %+v, want the main worktree, closing the requester, rebasing nowhere", s)
	}
}

func TestTheQueuedLabelIsTheOneTheRunningMergeDraws(t *testing.T) {
	tests := []struct {
		name   string
		source wsm.MergeSource
	}{
		{name: "own branch", source: ownBranch},
		{name: "another workspace", source: wsm.MergeSource{Kind: wsm.MergeSourceWorkspace, Workspace: otherWorkspace}},
		{name: "a branch", source: wsm.MergeSource{Kind: wsm.MergeSourceBranch, Branch: "agent-1/fix"}},
		{name: "merged upstream", source: wsm.MergeSource{Kind: wsm.MergeSourceMergedUpstream}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.registerOther(otherWorkspace, "ws-two", "other-branch")
			running := (&run{subject: subjectOf(t, h, tt.source)}).label()

			// Act.
			queued, err := h.o.sourceLabel(context.Background(), theWorkspace, tt.source)

			// Assert.
			if err != nil || queued != running {
				t.Fatalf("queued label = (%q, %v), want the running label %q", queued, err, running)
			}
		})
	}
}
