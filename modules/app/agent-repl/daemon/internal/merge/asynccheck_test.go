package merge

import (
	"context"
	"errors"
	"reflect"
	"testing"

	"agent-repld/internal/gitclient"
	"agent-repld/internal/wsm"

	workspacev1 "agentrepl/proto/workspace/v1"
)

// asyncRequest asks for the harness workspace's async merge of asyncBranch as
// RUN says, as the user.
func asyncRequest(h *harness, run *DetachedRun) error {
	return enqueueErr(h.o.Enqueue(context.Background(), Request{Workspace: theWorkspace, Source: asyncSource, By: RequestedByUser, Detached: run}))
}

func TestAnAsyncRequestIsRefusedWhatItCannotRunAsAsked(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		run     *DetachedRun
		arm     string
	}{
		{name: "the requester's own branch", arrange: func(h *harness) {
			h.db.mu.Lock()
			w := h.db.workspaces[theWorkspace]
			w.Branch = asyncBranch
			h.db.workspaces[theWorkspace] = w
			h.db.mu.Unlock()
		}, run: &DetachedRun{Maker: theWorkspace, Name: "n"}, arm: ArmBranchOwned},
		{name: "another workspace's own branch", arrange: func(h *harness) {
			h.registerOther(otherWorkspace, "ws-two", asyncBranch)
		}, run: &DetachedRun{Maker: theWorkspace, Name: "n"}, arm: ArmBranchOwned},
		{name: "a maker no workspace is registered as", run: &DetachedRun{Maker: "ws-9", Name: "n"}, arm: ArmUnknownMaker},
		{name: "a maker of another repository", arrange: func(h *harness) {
			h.registerOther(otherWorkspace, "ws-two", "b")
			h.db.mu.Lock()
			w := h.db.workspaces[otherWorkspace]
			w.Repo = "repo-9"
			h.db.workspaces[otherWorkspace] = w
			h.db.mu.Unlock()
		}, run: &DetachedRun{Maker: otherWorkspace, Name: "n"}, arm: ArmUnknownMaker},
		{name: "a one-shot naming the requester", run: &DetachedRun{Maker: theWorkspace, Name: "n", CloseOnLanding: theWorkspace}, arm: ArmUnknownCloseAfter},
		{name: "a one-shot naming no registered workspace", run: &DetachedRun{Maker: theWorkspace, Name: "n", CloseOnLanding: "ws-9"}, arm: ArmUnknownCloseAfter},
		{name: "a one-shot naming a closed workspace", arrange: func(h *harness) {
			h.registerOther(otherWorkspace, "ws-two", "b")
			h.db.mu.Lock()
			w := h.db.workspaces[otherWorkspace]
			w.Closed = true
			h.db.workspaces[otherWorkspace] = w
			h.db.mu.Unlock()
		}, run: &DetachedRun{Maker: theWorkspace, Name: "n", CloseOnLanding: otherWorkspace}, arm: ArmUnknownCloseAfter},
		{name: "a one-shot naming a workspace of another repository", arrange: func(h *harness) {
			h.registerOther(otherWorkspace, "ws-two", "b")
			h.db.mu.Lock()
			w := h.db.workspaces[otherWorkspace]
			w.Repo = "repo-9"
			h.db.workspaces[otherWorkspace] = w
			h.db.mu.Unlock()
		}, run: &DetachedRun{Maker: theWorkspace, Name: "n", CloseOnLanding: otherWorkspace}, arm: ArmUnknownCloseAfter},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			if tt.arrange != nil {
				tt.arrange(h)
			}

			// Act.
			err := asyncRequest(h, tt.run)

			// Assert: the refusal arm, and nothing recorded.
			var refusal *RefusalError
			if !errors.As(err, &refusal) || refusal.Arm != tt.arm {
				t.Fatalf("Enqueue = %v, want the %s refusal", err, tt.arm)
			}
			if _, ok := h.entryOf(theWorkspace); ok {
				t.Fatal("a refused async request was recorded")
			}
		})
	}
}

// TestAnAsyncRequestFromAWorkspaceTheDaemonNeverCreatedIsAccepted covers the
// main checkout: it records no creation job, and it is the workspace that
// always merges async.
func TestAnAsyncRequestFromAWorkspaceTheDaemonNeverCreatedIsAccepted(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.db.mu.Lock()
	delete(h.db.jobs, theWorkspace)
	h.db.mu.Unlock()
	h.inFlight = "turn-asking"

	// Act: an agent's request, still waiting on the turn that asked.
	_, err := h.o.Enqueue(context.Background(), Request{Workspace: theWorkspace, Source: asyncSource, By: RequestedByAgent, Asker: theWorkspace,
		Detached: &DetachedRun{Maker: theWorkspace, Name: "n"}})

	// Assert.
	if err != nil {
		t.Fatalf("Enqueue = %v, want the async request accepted", err)
	}
	if _, ok := h.entryOf(theWorkspace); !ok {
		t.Fatal("the async request was not recorded")
	}
}

func TestAnAsyncRequestRecordsItsLandingEffects(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.registerOther(otherWorkspace, "ws-two", "b")
	h.git.mu.Lock()
	h.git.worktrees = append(h.git.worktrees, gitclient.Worktree{Dir: "/wt/agent-fix", Branch: asyncBranch})
	h.git.mu.Unlock()
	h.inFlight = "turn-asking"

	// Act: an agent's request, still waiting on the turn that asked.
	_, err := h.o.Enqueue(context.Background(), Request{Workspace: theWorkspace, Source: asyncSource, By: RequestedByAgent, Asker: theWorkspace,
		Detached: &DetachedRun{Maker: theWorkspace, Name: "n", Pick: "rebase it", CloseOnLanding: otherWorkspace, CleanupWorktree: true}})

	// Assert.
	if err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	entry, ok := h.entryOf(theWorkspace)
	if !ok {
		t.Fatal("the async request was not recorded")
	}
	want := wsm.MergeSource{Kind: wsm.MergeSourceBranch, Branch: asyncBranch, Detach: entry.Source.Detach, DetachMaker: theWorkspace, DetachName: "n", Pick: "rebase it",
		CloseOnLanding: otherWorkspace, CleanupWorktree: true, CleanupDir: "/wt/agent-fix"}
	if !reflect.DeepEqual(entry.Source, want) {
		t.Fatalf("source = %+v, want %+v", entry.Source, want)
	}
}

func TestACleanupNeverRecordsAWorktreeItMustNotRemove(t *testing.T) {
	tests := []struct {
		name string
		dir  func(h *harness) string
	}{
		{name: "the repository's main worktree", dir: func(h *harness) string { return h.sourceD }},
		{name: "a registered workspace's worktree", dir: func(h *harness) string { return h.registerOther(otherWorkspace, "ws-two", "b") }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the branch is checked out in a worktree a cleanup never
			// removes.
			h := newHarness(t)
			dir := tt.dir(h)
			h.git.mu.Lock()
			h.git.worktrees = append(h.git.worktrees, gitclient.Worktree{Dir: dir, Branch: asyncBranch})
			h.git.mu.Unlock()

			// Act.
			got, err := h.o.cleanupDirFor(context.Background(), h.db.workspaces[theWorkspace], asyncBranch)

			// Assert.
			if err != nil || got != "" {
				t.Fatalf("cleanupDirFor = (%q, %v), want none recorded", got, err)
			}
		})
	}
}

func TestACleanupRecordsNothingWhenNoWorktreeHasTheBranch(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	got, err := h.o.cleanupDirFor(context.Background(), h.db.workspaces[theWorkspace], asyncBranch)

	// Assert.
	if err != nil || got != "" {
		t.Fatalf("cleanupDirFor = (%q, %v), want none", got, err)
	}
}

func TestEnqueueAnswersWhatTheRequestReserved(t *testing.T) {
	tests := []struct {
		name string
		req  Request
		want Enqueued
	}{
		{name: "an async request reserves its merge workspace's branch",
			req:  Request{Workspace: theWorkspace, Source: asyncSource, By: RequestedByUser, Detached: &DetachedRun{Maker: theWorkspace, Name: "reconnect-fix"}},
			want: Enqueued{MergeBranch: "agent/reconnect-fix"}},
		{name: "an in-place request reserves nothing",
			req:  Request{Workspace: theWorkspace, Source: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch}, By: RequestedByUser},
			want: Enqueued{}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			got, err := h.o.Enqueue(context.Background(), tt.req)

			// Assert
			if err != nil {
				t.Fatalf("Enqueue: %v", err)
			}
			if got != tt.want {
				t.Errorf("Enqueue = %+v, want %+v", got, tt.want)
			}
		})
	}
}

func TestAnAsyncRefusalNamesWhatItRefused(t *testing.T) {
	tests := []struct {
		name   string
		source wsm.MergeSource
		run    *DetachedRun
		arm    string
		field  string
		want   string
	}{
		{name: "an unknown maker names the maker", source: asyncSource, run: &DetachedRun{Maker: "ws-9", Name: "n"},
			arm: ArmUnknownMaker, field: "workspace", want: "ws-9"},
		{name: "an unknown branch names the branch", source: wsm.MergeSource{Kind: wsm.MergeSourceBranch, Branch: "no-such"}, run: &DetachedRun{Maker: theWorkspace, Name: "n"},
			arm: ArmUnknownBranch, field: "branch", want: "no-such"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			_, err := h.o.Enqueue(context.Background(), Request{Workspace: theWorkspace, Source: tt.source, By: RequestedByUser, Detached: tt.run})

			// Assert
			refusal, ok := Refused(err)
			if !ok || refusal.Arm != tt.arm {
				t.Fatalf("Enqueue = %v, want the %s refusal", err, tt.arm)
			}
			got := refusal.Fields[tt.field]
			if ref, isRef := got.(*workspacev1.WorkspaceRef); isRef {
				got = ref.GetId()
			}
			if got != tt.want {
				t.Errorf("field %s = %v, want %q", tt.field, got, tt.want)
			}
		})
	}
}
