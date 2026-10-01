package merge

import (
	"context"
	"errors"
	"testing"

	"claude-repld/internal/wsm"
)

var ownBranch = wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch}

func TestAnAgentsRequestIsRecordedButNotInLineWhileItsTurnRuns(t *testing.T) {
	// Arrange: the requesting turn is in flight.
	h := newHarness(t)
	h.inFlight = "turn-asking"

	// Act.
	if err := h.request(t, ownBranch, RequestedByAgent); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Assert.
	if state, ok := h.entryState(); !ok || state != wsm.MergeRequested {
		t.Fatalf("queue entry = (%v, %v), want a durable request", state, ok)
	}
}

func TestNoMergeFactReachesAnySurfaceBeforeTheRequestingTurnEnds(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.inFlight = "turn-asking"

	// Act.
	if err := h.request(t, ownBranch, RequestedByAgent); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Assert: no footer or roster fact, no feed row, no ledger identity.
	if len(h.footer.all()) != 0 || len(h.sidebar.facts) != 0 {
		t.Fatalf("facts were published before the turn ended: footer %+v", h.footer.all())
	}
	if len(h.feed.rows) != 0 {
		t.Fatalf("feed rows were drawn before the turn ended: %d", len(h.feed.rows))
	}
	if _, minted := h.o.leaseOf(theWorkspace); minted {
		t.Fatal("the bubble's identity was minted before the turn ended")
	}
}

func TestAnAgentsRequestIsPutInLineOnceItsTurnEnds(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.inFlight = "turn-asking"
	if err := h.request(t, ownBranch, RequestedByAgent); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Act.
	h.runWait(t)

	// Assert.
	if state, _ := h.entryState(); state != wsm.MergeQueued {
		t.Fatalf("queue entry state = %v, want queued", state)
	}
	if len(h.awaitedTurns) != 1 || h.awaitedTurns[0] != "turn-asking" {
		t.Fatalf("awaited turns = %v, want the requesting turn", h.awaitedTurns)
	}
	if got := h.footer.last(); got.State != StateQueued || got.QueuePlace != 1 || got.QueueWaiting != 1 {
		t.Fatalf("facts = %+v, want enqueued 1/1", got)
	}
}

func TestAnAgentsRequestWithNoTurnInFlightIsPutInLineWithoutWaiting(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	if err := h.request(t, ownBranch, RequestedByAgent); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Act.
	h.runWait(t)

	// Assert.
	if state, _ := h.entryState(); state != wsm.MergeQueued {
		t.Fatalf("queue entry state = %v, want queued", state)
	}
	if len(h.awaitedTurns) != 0 {
		t.Fatalf("awaited turns = %v, want none", h.awaitedTurns)
	}
}

func TestAUsersRequestIsPutInLineAtOnce(t *testing.T) {
	// Arrange: a turn in flight is no requesting turn of a user's ask.
	h := newHarness(t)
	h.inFlight = "turn-running"

	// Act.
	if err := h.request(t, ownBranch, RequestedByUser); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Assert.
	if state, _ := h.entryState(); state != wsm.MergeQueued {
		t.Fatalf("queue entry state = %v, want queued at once", state)
	}
}

func TestARequestRecordsItsSource(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.registerOther(otherWorkspace, "ws-two", "other-branch")
	source := wsm.MergeSource{Kind: wsm.MergeSourceWorkspace, Workspace: otherWorkspace}
	// The request is recorded with the branch checked out in the other worktree.
	recorded := source
	recorded.Branch = "other-branch"

	// Act.
	if err := h.request(t, source, RequestedByAgent); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Assert.
	entries, _ := h.db.MergeQueue(context.Background(), h.repoKey())
	if len(entries) != 1 || entries[0].Source != recorded {
		t.Fatalf("queue = %+v, want the request's source recorded", entries)
	}
}

// TestAnOwnBranchRequestRecordsTheBranchCheckedOutNotTheCreationBranch covers
// the 2026-10-01 failure: the worktree had switched off the branch it was
// created on, and the merge must take the branch checked out.
func TestAnOwnBranchRequestRecordsTheBranchCheckedOutNotTheCreationBranch(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.git.mu.Lock()
	h.git.branches[h.sourceD] = "renamed"
	h.git.mu.Unlock()

	// Act.
	err := h.request(t, ownBranch, RequestedByAgent)

	// Assert.
	if err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	entries, _ := h.db.MergeQueue(context.Background(), h.repoKey())
	if len(entries) != 1 || entries[0].Source.Branch != "renamed" {
		t.Fatalf("queue = %+v, want the checked-out branch recorded", entries)
	}
}

func TestAMergedUpstreamRequestRecordsTheBranchCheckedOut(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.git.mu.Lock()
	h.git.branches[h.sourceD] = "renamed"
	h.git.mu.Unlock()

	// Act.
	err := h.request(t, wsm.MergeSource{Kind: wsm.MergeSourceMergedUpstream}, RequestedByAgent)

	// Assert.
	if err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	entries, _ := h.db.MergeQueue(context.Background(), h.repoKey())
	if len(entries) != 1 || entries[0].Source.Branch != "renamed" {
		t.Fatalf("queue = %+v, want the checked-out branch recorded", entries)
	}
}

func TestWithCheckedOutBranchRecordsTheBranchOfTheNamedWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.registerOther(otherWorkspace, "ws-two", "other-branch")

	// Act.
	got, err := h.o.withCheckedOutBranch(context.Background(), theWorkspace, otherWorkspace, wsm.MergeSource{Kind: wsm.MergeSourceWorkspace, Workspace: otherWorkspace})

	// Assert.
	if err != nil || got.Branch != "other-branch" {
		t.Fatalf("withCheckedOutBranch = %+v, %v, want other-branch", got, err)
	}
}

func TestWithCheckedOutBranchRefusesAWorkspaceWithNoGeometry(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.db.mu.Lock()
	delete(h.db.jobs, theWorkspace)
	h.db.mu.Unlock()

	// Act.
	_, err := h.o.withCheckedOutBranch(context.Background(), theWorkspace, theWorkspace, ownBranch)

	// Assert.
	if refusal, refused := Refused(err); !refused || refusal.Arm != ArmNoLayoutFacts {
		t.Fatalf("withCheckedOutBranch = %v, want the %s refusal", err, ArmNoLayoutFacts)
	}
}

// TestEveryWorkspaceSourceRecordsTheBranchCheckedOut holds every arm that
// merges a workspace's branch to the one shape: the checked-out branch is
// recorded with the request.
func TestEveryWorkspaceSourceRecordsTheBranchCheckedOut(t *testing.T) {
	tests := []struct {
		name   string
		source wsm.MergeSource
	}{
		{name: "own branch", source: ownBranch},
		{name: "merged upstream", source: wsm.MergeSource{Kind: wsm.MergeSourceMergedUpstream}},
		{name: "another workspace", source: wsm.MergeSource{Kind: wsm.MergeSourceWorkspace, Workspace: otherWorkspace}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.registerOther(otherWorkspace, "ws-two", "checked-out")
			h.git.mu.Lock()
			h.git.branches[h.sourceD] = "checked-out"
			h.git.mu.Unlock()

			// Act.
			err := h.request(t, tt.source, RequestedByAgent)

			// Assert.
			if err != nil {
				t.Fatalf("Enqueue: %v", err)
			}
			entries, _ := h.db.MergeQueue(context.Background(), h.repoKey())
			if len(entries) != 1 || entries[0].Source.Branch != "checked-out" {
				t.Fatalf("queue = %+v, want the checked-out branch recorded", entries)
			}
		})
	}
}

func TestARequestIsRefusedWhenItsSourceCannotBeMerged(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		source  wsm.MergeSource
		arm     string
	}{
		{name: "the requester itself as the other workspace", source: wsm.MergeSource{Kind: wsm.MergeSourceWorkspace, Workspace: theWorkspace}, arm: ArmUnknownSourceWorkspace},
		{name: "an unregistered workspace", source: wsm.MergeSource{Kind: wsm.MergeSourceWorkspace, Workspace: "ws-9"}, arm: ArmUnknownSourceWorkspace},
		{name: "a closed workspace", arrange: func(h *harness) {
			h.registerOther(otherWorkspace, "ws-two", "b")
			h.db.mu.Lock()
			w := h.db.workspaces[otherWorkspace]
			w.Closed = true
			h.db.workspaces[otherWorkspace] = w
			h.db.mu.Unlock()
		}, source: wsm.MergeSource{Kind: wsm.MergeSourceWorkspace, Workspace: otherWorkspace}, arm: ArmUnknownSourceWorkspace},
		{name: "a workspace of another repository", arrange: func(h *harness) {
			h.registerOther(otherWorkspace, "ws-two", "b")
			h.db.mu.Lock()
			w := h.db.workspaces[otherWorkspace]
			w.Repo = "repo-9"
			h.db.workspaces[otherWorkspace] = w
			h.db.mu.Unlock()
		}, source: wsm.MergeSource{Kind: wsm.MergeSourceWorkspace, Workspace: otherWorkspace}, arm: ArmUnknownSourceWorkspace},
		{name: "a branch that does not exist", source: wsm.MergeSource{Kind: wsm.MergeSourceBranch, Branch: "no-such"}, arm: ArmUnknownBranch},
		{name: "an own branch with no recorded geometry", arrange: func(h *harness) {
			h.db.mu.Lock()
			delete(h.db.jobs, theWorkspace)
			h.db.mu.Unlock()
		}, source: ownBranch, arm: ArmNoLayoutFacts},
		{name: "an own branch whose worktree has a detached HEAD", arrange: func(h *harness) {
			h.git.mu.Lock()
			h.git.branches[h.sourceD] = ""
			h.git.mu.Unlock()
		}, source: ownBranch, arm: ArmUnknownBranch},
		{name: "another workspace whose worktree has a detached HEAD", arrange: func(h *harness) {
			h.registerOther(otherWorkspace, "ws-two", "b")
			h.db.mu.Lock()
			dir := h.db.jobs[otherWorkspace].Layout.SourceDir
			h.db.mu.Unlock()
			h.git.mu.Lock()
			h.git.branches[dir] = ""
			h.git.mu.Unlock()
		}, source: wsm.MergeSource{Kind: wsm.MergeSourceWorkspace, Workspace: otherWorkspace}, arm: ArmUnknownBranch},
		{name: "merged upstream whose worktree has a detached HEAD", arrange: func(h *harness) {
			h.git.mu.Lock()
			h.git.branches[h.sourceD] = ""
			h.git.mu.Unlock()
		}, source: wsm.MergeSource{Kind: wsm.MergeSourceMergedUpstream}, arm: ArmUnknownBranch},
		{name: "a deleted session", arrange: func(h *harness) {
			h.db.mu.Lock()
			h.db.sessions[theWorkspace] = wsm.Session{Workspace: theWorkspace, Terminal: &wsm.SessionTerminal{Kind: "deleted"}}
			h.db.mu.Unlock()
		}, source: ownBranch, arm: ArmSessionDeleted},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			if tt.arrange != nil {
				tt.arrange(h)
			}

			// Act.
			err := h.request(t, tt.source, RequestedByAgent)

			// Assert.
			refusal, refused := Refused(err)
			if !refused || refusal.Arm != tt.arm {
				t.Fatalf("Enqueue = %v, want the %s refusal", err, tt.arm)
			}
			if _, recorded := h.entryState(); recorded {
				t.Fatal("a refused request left a queue entry")
			}
			if _, logged := h.recordFor("warn", "daemon.merge.enqueue"); !logged {
				t.Fatalf("the refusal was not recorded at WARN: %+v", h.logs.Records())
			}
		})
	}
}

func TestABranchThatExistsIsRequested(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.git.refs["refs/heads/agent-1/fix"] = "abc"

	// Act.
	err := h.request(t, wsm.MergeSource{Kind: wsm.MergeSourceBranch, Branch: "agent-1/fix"}, RequestedByAgent)

	// Assert.
	if err != nil {
		t.Fatalf("Enqueue = %v, want the branch requested", err)
	}
}

func TestASecondRequestIsRefusedAsAlreadyQueued(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	if err := h.request(t, ownBranch, RequestedByAgent); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Act.
	err := h.request(t, ownBranch, RequestedByAgent)

	// Assert.
	if refusal, refused := Refused(err); !refused || refusal.Arm != ArmAlreadyQueued {
		t.Fatalf("Enqueue = %v, want already_queued", err)
	}
}

func TestARequestTheStoreCannotRecordIsAnError(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.db.enqueueErr = errors.New("database is locked")

	// Act.
	err := h.request(t, ownBranch, RequestedByAgent)

	// Assert.
	if _, refused := Refused(err); err == nil || refused {
		t.Fatalf("Enqueue = %v, want the store's failure", err)
	}
	if _, logged := h.recordFor("error", "daemon.merge.enqueue"); !logged {
		t.Fatalf("the failure was not recorded at ERROR: %+v", h.logs.Records())
	}
}

func TestEvictingARequestWithdrawsItsWaitAndDrawsNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.inFlight = "turn-asking"
	if err := h.request(t, ownBranch, RequestedByAgent); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	h.o.mu.Lock()
	wait := h.o.requested[theWorkspace]
	h.o.mu.Unlock()

	// Act.
	if err := h.o.Evict(context.Background(), theWorkspace); err != nil {
		t.Fatalf("Evict: %v", err)
	}

	// Assert.
	if _, recorded := h.entryState(); recorded {
		t.Fatal("the evicted request is still on the queue")
	}
	if wait.ctx.Err() == nil {
		t.Fatal("the request's wait was not withdrawn")
	}
	if len(h.feed.rows) != 0 {
		t.Fatalf("an evicted request drew %d feed rows; it was never reported", len(h.feed.rows))
	}
}

func TestAWithdrawnWaitPutsNothingInLine(t *testing.T) {
	// Arrange: the requesting turn never ends; the request is withdrawn.
	h := newHarness(t)
	h.inFlight = "turn-asking"
	h.awaitGate = make(chan struct{})
	if err := h.request(t, ownBranch, RequestedByAgent); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	h.o.withdrawRequest(theWorkspace)

	// Act.
	wait := &requestWait{ws: theWorkspace, repo: h.repoKey(), ctx: cancelled(), cancel: func() {}}
	h.o.waitThenQueue(wait)

	// Assert.
	if state, _ := h.entryState(); state != wsm.MergeRequested {
		t.Fatalf("queue entry state = %v, want the request still recorded and not in line", state)
	}
}

func TestAWaitThatFailsLeavesTheRequestRecordedAndSaysSo(t *testing.T) {
	// Arrange: the requester's session is gone, so its turn cannot be waited on.
	h := newHarness(t)
	h.inFlight = "turn-asking"
	if err := h.request(t, ownBranch, RequestedByAgent); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	h.o.deps.AwaitTurnEnd = func(context.Context, wsm.WorkspaceID, wsm.TurnID) (wsm.TurnClose, error) {
		return 0, errors.New("no live session")
	}

	// Act.
	h.runWait(t)

	// Assert.
	if state, _ := h.entryState(); state != wsm.MergeRequested {
		t.Fatalf("queue entry state = %v, want the request still recorded", state)
	}
	if _, logged := h.recordFor("error", "daemon.merge.request"); !logged {
		t.Fatalf("the failed wait was not recorded at ERROR: %+v", h.logs.Records())
	}
}

// cancelled answers a context that has already ended.
func cancelled() context.Context {
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	return ctx
}
