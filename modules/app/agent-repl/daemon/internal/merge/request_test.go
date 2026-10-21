package merge

import (
	repoconfigv1 "agentrepl/proto/repoconfig/v1"
	"context"
	"errors"
	"reflect"
	"slices"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"

	workspacev1 "agentrepl/proto/workspace/v1"
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
	h.git.mu.Lock()
	h.git.branches[h.sourceD] = "checked-out"
	h.git.mu.Unlock()
	source := wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch, KeepOpen: true}
	// The request is recorded with the branch checked out in its worktree.
	recorded := source
	recorded.Branch = "checked-out"

	// Act.
	if err := h.request(t, source, RequestedByAgent); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Assert.
	entries, _ := h.db.MergeQueue(context.Background(), h.repoKey())
	if len(entries) != 1 || !reflect.DeepEqual(entries[0].Source, recorded) {
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
		}, source: ownBranch, arm: ArmNoBranchCheckedOut},
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

func TestOnlyAnAgentsInPlaceRequestFlushesTheHeldPrompts(t *testing.T) {
	tests := []struct {
		name string
		req  Request
		want []ids.WorkspaceID
	}{
		{name: "an agent's in-place request", req: Request{Workspace: theWorkspace, Source: ownBranch, By: RequestedByAgent, Asker: theWorkspace}, want: []ids.WorkspaceID{theWorkspace}},
		{name: "an agent's one-shot", req: Request{Workspace: theWorkspace, Source: ownBranch, By: RequestedByAgent, Asker: theWorkspace, OneShot: true}, want: []ids.WorkspaceID{theWorkspace}},
		{name: "an agent's async request", req: Request{Workspace: theWorkspace, Source: asyncSource, By: RequestedByAgent, Asker: theWorkspace, Detached: &DetachedRun{Maker: theWorkspace, Name: "flush-async"}}, want: nil},
		{name: "the user's request", req: Request{Workspace: theWorkspace, Source: ownBranch, By: RequestedByUser}, want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.inFlight = "turn-asking"

			// Act.
			if _, err := h.o.Enqueue(context.Background(), tt.req); err != nil {
				t.Fatalf("Enqueue: %v", err)
			}

			// Assert.
			if got := h.queue.flushes(); !slices.Equal(got, tt.want) {
				t.Fatalf("flushed = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestARequestWhoseFlushFailsIsRefusedWithNothingRecorded(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.inFlight = "turn-asking"
	h.queue.flushErr = errors.New("the holds could not be read")

	// Act.
	err := h.request(t, ownBranch, RequestedByAgent)

	// Assert.
	if err == nil || !errors.Is(err, h.queue.flushErr) {
		t.Fatalf("Enqueue = %v, want the flush's error", err)
	}
	if _, ok := h.entryState(); ok {
		t.Fatal("a request whose flush failed was recorded")
	}
}

func TestNewDetachedRunTrimsTheNameAndTheInstructionAndKeepsTheAsks(t *testing.T) {
	hooks := wsm.DeployHooks{PreDeploy: []*repoconfigv1.DeployHook{promptHookOf("lint")}, PostDeploy: []*repoconfigv1.DeployHook{scriptHookOf("bin/announce.sh")}}
	tests := []struct {
		name             string
		inName, instruct string
		compactAfter     bool
		closeOnLanding   ids.WorkspaceID
		cleanupWorktree  bool
		hooks            wsm.DeployHooks
		maker            ids.WorkspaceID
		intoMaker        bool
		want             DetachedRun
	}{
		{name: "its maker and its target", inName: "reconnect-fix", maker: "maker", intoMaker: true, want: DetachedRun{Name: "reconnect-fix", Maker: "maker", IntoMaker: true}},
		{name: "a branch as it is", inName: " reconnect-fix ", want: DetachedRun{Name: "reconnect-fix"}},
		{name: "an instruction", inName: "reconnect-fix", instruct: "\tthe fix \n", want: DetachedRun{Name: "reconnect-fix", Pick: "the fix"}},
		{name: "a blank instruction", inName: "reconnect-fix", instruct: "   ", want: DetachedRun{Name: "reconnect-fix"}},
		{name: "compacting after", inName: "reconnect-fix", compactAfter: true, want: DetachedRun{Name: "reconnect-fix", CompactAfter: true}},
		{name: "closing another workspace", inName: "reconnect-fix", closeOnLanding: "child", want: DetachedRun{Name: "reconnect-fix", CloseOnLanding: "child"}},
		{name: "cleaning up", inName: "reconnect-fix", cleanupWorktree: true, want: DetachedRun{Name: "reconnect-fix", CleanupWorktree: true}},
		{name: "its own deploy hooks", inName: "reconnect-fix", hooks: hooks, want: DetachedRun{Name: "reconnect-fix", Hooks: hooks}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := NewDetachedRun(tt.inName, tt.instruct, tt.compactAfter, tt.closeOnLanding, tt.cleanupWorktree, tt.hooks, tt.maker, tt.intoMaker)

			// Assert
			if got == nil || !reflect.DeepEqual(*got, tt.want) {
				t.Fatalf("NewDetachedRun = %+v, want %+v", got, tt.want)
			}
		})
	}
}

func TestAnInPlaceMergeLandsOnlyOnTheAskOfWhoMayLandIt(t *testing.T) {
	owner := ids.WorkspaceID("ws-owner")
	tests := []struct {
		name       string
		ownerLands bool
		by         Requester
		asker      ids.WorkspaceID
		arm        string
	}{
		{name: "the user lands any workspace", ownerLands: true, by: RequestedByUser},
		{name: "a workspace lands itself", by: RequestedByAgent, asker: theWorkspace},
		{name: "a child its owner lands may not land itself", ownerLands: true, by: RequestedByAgent, asker: theWorkspace, arm: ArmOwnerLands},
		{name: "the owner lands its child", ownerLands: true, by: RequestedByAgent, asker: owner},
		{name: "the owner lands a child that forked it", by: RequestedByAgent, asker: owner},
		{name: "any other workspace may not land it", by: RequestedByAgent, asker: "ws-stranger", arm: ArmRequesterMayNotLand},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.db.mu.Lock()
			w := h.db.workspaces[theWorkspace]
			w.Parent, w.OwnerLands = &owner, tt.ownerLands
			h.db.workspaces[theWorkspace] = w
			h.db.mu.Unlock()

			// Act
			_, err := h.o.Enqueue(context.Background(), Request{Workspace: theWorkspace, Source: ownBranch, By: tt.by, Asker: tt.asker})

			// Assert
			if tt.arm == "" {
				if err != nil {
					t.Fatalf("Enqueue = %v, want accepted", err)
				}
				return
			}
			refusal, ok := Refused(err)
			if !ok || refusal.Arm != tt.arm {
				t.Fatalf("Enqueue = %v, want the %s refusal", err, tt.arm)
			}
			if _, queued := h.entryOf(theWorkspace); queued {
				t.Error("a refused merge was recorded")
			}
			if record, found := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "warn", Operation: "daemon.merge.enqueue", Message: "refused a merge its asker may not ask for"}); !found || record.Context["asker"] != string(tt.asker) {
				t.Errorf("no warn record naming the asker: %+v", h.logs.Records())
			}
			if tt.arm == ArmOwnerLands && refusal.Fields["owner"].(*workspacev1.WorkspaceRef).GetId() != string(owner) {
				t.Errorf("owner_lands fields = %+v, want the owner", refusal.Fields)
			}
		})
	}
}

func TestAMergeRequestThatContradictsWhoAskedFailsHard(t *testing.T) {
	tests := []struct {
		name  string
		by    Requester
		asker ids.WorkspaceID
	}{
		{name: "an agent's ask naming no workspace", by: RequestedByAgent},
		{name: "the user's ask naming a workspace", by: RequestedByUser, asker: theWorkspace},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			_, err := h.o.Enqueue(context.Background(), Request{Workspace: theWorkspace, Source: ownBranch, By: tt.by, Asker: tt.asker})

			// Assert
			if _, refused := Refused(err); err == nil || refused {
				t.Fatalf("Enqueue = %v, want a hard failure", err)
			}
			if _, found := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.merge.enqueue", Message: "a merge request contradicts itself about who asked"}); !found {
				t.Errorf("no error record: %+v", h.logs.Records())
			}
		})
	}
}

func TestARequestThatWouldCloseAParentWithOpenChildrenIsRefusedBeforeItFlushes(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.inFlight = "turn-asking"
	h.db.openChildrenErr = &wsm.HasOpenChildrenError{
		Parent:   wsm.FamilyRef{ID: theWorkspace, Dir: "/w/parent"},
		Children: []wsm.FamilyRef{{ID: "ws-child", Dir: "/w/child"}},
	}

	// Act.
	err := h.request(t, ownBranch, RequestedByAgent)

	// Assert.
	if !wsm.IsFamilyRefusal(err) {
		t.Fatalf("Enqueue = %v, want the family refusal", err)
	}
	if got := h.queue.flushes(); len(got) != 0 {
		t.Fatalf("flushed = %v, want nothing flushed for a refused merge", got)
	}
	if _, ok := h.entryState(); ok {
		t.Fatal("a refused merge was recorded")
	}
	if !slices.Equal(h.db.openChildrenAsked, []ids.WorkspaceID{theWorkspace}) {
		t.Fatalf("asked about %v, want the requester the merge closes", h.db.openChildrenAsked)
	}
	if _, logged := h.recordFor("info", "daemon.merge.enqueue"); !logged {
		t.Fatalf("the refusal was not recorded at INFO: %+v", h.logs.Records())
	}
}

func TestARequestAsksAboutEveryWorkspaceItsLandingCloses(t *testing.T) {
	tests := []struct {
		name string
		req  Request
		want []ids.WorkspaceID
	}{
		{name: "an in-place merge closes its requester", req: Request{Workspace: theWorkspace, Source: ownBranch, By: RequestedByUser}, want: []ids.WorkspaceID{theWorkspace}},
		{name: "an async merge closes nothing in place", req: Request{Workspace: theWorkspace, Source: asyncSource, By: RequestedByAgent, Asker: theWorkspace, Detached: &DetachedRun{Maker: theWorkspace, Name: "ask-async"}}, want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.inFlight = "turn-asking"

			// Act.
			if _, err := h.o.Enqueue(context.Background(), tt.req); err != nil {
				t.Fatalf("Enqueue: %v", err)
			}

			// Assert.
			if !slices.Equal(h.db.openChildrenAsked, tt.want) {
				t.Fatalf("asked about %v, want %v", h.db.openChildrenAsked, tt.want)
			}
		})
	}
}

func TestARequestWhoseFamilyReadFailsIsRefusedWithNothingRecorded(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.inFlight = "turn-asking"
	h.db.openChildrenErr = errors.New("disk I/O error")

	// Act.
	err := h.request(t, ownBranch, RequestedByAgent)

	// Assert.
	if !errors.Is(err, h.db.openChildrenErr) {
		t.Fatalf("Enqueue = %v, want the read's error", err)
	}
	if _, ok := h.entryState(); ok {
		t.Fatal("a request whose family read failed was recorded")
	}
	if _, logged := h.recordFor("error", "daemon.merge.enqueue"); !logged {
		t.Fatalf("the failed read was not recorded at ERROR: %+v", h.logs.Records())
	}
}
