package merge

import (
	"context"
	"errors"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/mergetab"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/wsm"
)

// conflictHeld starts a merge whose rebase conflicts, ASYNC or SYNC, and holds
// it in its conflict resolution: the repair turn is submitted and its end is
// held until RELEASE is called. It answers the bubble's ledger identity.
func conflictHeld(t *testing.T, h *harness, async bool) (ids.LeaseID, func()) {
	t.Helper()
	landing(h, 1)
	h.git.rebaseConflicts[1] = []string{"a.go"}
	gate := make(chan struct{})
	submitted := make(chan struct{}, 1)
	h.mu.Lock()
	h.awaitGate = gate
	h.mu.Unlock()
	h.queue.onSubmit = func(promptqueue.Submission) {
		select {
		case submitted <- struct{}{}:
		default:
		}
	}
	if async {
		if err := requestDetached(t, h, RequestedByUser); err != nil {
			t.Fatalf("Enqueue: %v", err)
		}
	} else {
		enqueue(t, h)
	}
	done := admitAsync(h, context.Background())
	<-submitted
	merging := theWorkspace
	if async {
		merging = theMergeWorkspace
	}
	lease, ok := h.o.leaseOf(merging)
	if !ok {
		t.Fatal("the merge holds no ledger identity")
	}
	return lease, func() {
		close(gate)
		<-done
	}
}

func TestAMergeTabInterruptStopsAnAsyncMergesAgentAndRevealsItsWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.interruptRunning = true
	lease, release := conflictHeld(t, h, true)
	defer release()

	// Act
	running, err := h.o.InterruptTab(context.Background(), theWorkspace, tabRef(theWorkspace, lease, commitKey(1, 0)))

	// Assert
	if err != nil || !running {
		t.Fatalf("InterruptTab = %v, %v; want the running turn stopped", running, err)
	}
	h.mu.Lock()
	interrupted := append([]ids.WorkspaceID(nil), h.interrupted...)
	h.mu.Unlock()
	if len(interrupted) != 1 || interrupted[0] != theMergeWorkspace {
		t.Fatalf("interrupted = %v, want the merge workspace's turn", interrupted)
	}
	if revealed := h.detached.revealed(); len(revealed) != 1 || revealed[0] != theMergeWorkspace {
		t.Fatalf("reveals = %v, want the merge workspace revealed", revealed)
	}
}

func TestAMergeTabInterruptStopsASyncMergesOwnAgentAndRevealsNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.interruptRunning = true
	lease, release := conflictHeld(t, h, false)
	defer release()

	// Act
	running, err := h.o.InterruptTab(context.Background(), theWorkspace, tabRef(theWorkspace, lease, commitKey(1, 0)))

	// Assert
	if err != nil || !running {
		t.Fatalf("InterruptTab = %v, %v; want the running turn stopped", running, err)
	}
	h.mu.Lock()
	interrupted := append([]ids.WorkspaceID(nil), h.interrupted...)
	h.mu.Unlock()
	if len(interrupted) != 1 || interrupted[0] != theWorkspace {
		t.Fatalf("interrupted = %v, want the workspace's own turn", interrupted)
	}
	if revealed := h.detached.revealed(); len(revealed) != 0 {
		t.Fatalf("reveals = %v, want none for a sync merge", revealed)
	}
}

func TestAMergeTabInterruptThatCannotStopTheTurnIsAnError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.interruptErr = errors.New("the shim refused")
	lease, release := conflictHeld(t, h, true)
	defer release()

	// Act
	_, err := h.o.InterruptTab(context.Background(), theWorkspace, tabRef(theWorkspace, lease, commitKey(1, 0)))

	// Assert
	if err == nil {
		t.Fatal("InterruptTab = nil, want the failure")
	}
	if _, ok := h.recordFor(dlog.LevelError, "daemon.merge.interrupt_tab"); !ok {
		t.Fatal("the failed stop was not recorded at ERROR")
	}
	if revealed := h.detached.revealed(); len(revealed) != 0 {
		t.Fatalf("reveals = %v, want none when nothing was stopped", revealed)
	}
}

func TestAMergeTabInterruptOfALivePickStopsTheMergeWorkspacesTurn(t *testing.T) {
	// Arrange
	h := pickHarness(t)
	requestPick(t, h)
	lease, _ := h.o.leaseOf(theMergeWorkspace)

	// Act
	_, err := h.o.InterruptTab(context.Background(), theWorkspace, tabRef(theWorkspace, lease, roundTabKey(TabPick, 1)))

	// Assert
	h.mu.Lock()
	interrupted := append([]ids.WorkspaceID(nil), h.interrupted...)
	h.mu.Unlock()
	if err != nil || len(interrupted) != 1 || interrupted[0] != theMergeWorkspace {
		t.Fatalf("InterruptTab = %v, interrupted = %v; want the pick's turn stopped", err, interrupted)
	}
}

func TestAMergeTabInterruptNamingNoLiveAgenticTabIsRefused(t *testing.T) {
	tests := []struct {
		name string
		tab  func(lease ids.LeaseID) feedid.Ref
	}{
		{name: "a resolved tab", tab: func(lease ids.LeaseID) feedid.Ref { return tabRef(theWorkspace, lease, rebasingKey()) }},
		{name: "a commit whose conflict is not the live one", tab: func(lease ids.LeaseID) feedid.Ref { return tabRef(theWorkspace, lease, commitKey(1, 1)) }},
		{name: "a fixing row with no fixing live", tab: func(lease ids.LeaseID) feedid.Ref { return tabRef(theWorkspace, lease, fixingKey(1)) }},
		{name: "another feed's tab", tab: func(lease ids.LeaseID) feedid.Ref { return tabRef(theMergeWorkspace, lease, commitKey(1, 0)) }},
		{name: "an unknown bubble", tab: func(ids.LeaseID) feedid.Ref { return tabRef(theWorkspace, "lease-unknown", commitKey(1, 0)) }},
		{name: "a head", tab: func(lease ids.LeaseID) feedid.Ref { return headRef(theWorkspace, lease) }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			lease, release := conflictHeld(t, h, true)
			defer release()

			// Act
			_, err := h.o.InterruptTab(context.Background(), theWorkspace, tt.tab(lease))

			// Assert
			if refusal, ok := Refused(err); !ok || refusal.Arm != ArmNotMergeTab {
				t.Fatalf("InterruptTab = %v, want the not_merge_tab refusal", err)
			}
			h.mu.Lock()
			interrupted := len(h.interrupted)
			h.mu.Unlock()
			if interrupted != 0 {
				t.Fatal("a refused interrupt stopped a turn")
			}
			if _, ok := h.recordFor(dlog.LevelInfo, "daemon.merge.interrupt_tab"); !ok {
				t.Fatal("the refusal was not recorded")
			}
		})
	}
}

func TestAnAsyncMergesRepairTurnStandsAMirrorIntoTheRequestersTab(t *testing.T) {
	// Arrange
	h := newHarness(t)
	lease, release := conflictHeld(t, h, true)
	defer release()

	// Act / Assert: the merge workspace's repair is mirrored into the
	// requester's subtab of the conflicted commit, and no address draws it in a
	// bubble of its own.
	var mirrored *wsm.MirrorAddress
	for _, call := range h.feed.mirrorsStood() {
		if call.ws == theMergeWorkspace && call.mirror != nil {
			mirrored = call.mirror
		}
	}
	want := tabRef(theWorkspace, lease, commitKey(1, 0))
	if mirrored == nil || mirrored.Workspace != theWorkspace || mirrored.Parent.WS != want.WS || mirrored.Parent.Row != want.Row {
		t.Fatalf("mirror = %+v, want the requester's subtab of the conflicted commit", mirrored)
	}
	for _, addr := range h.feed.installed() {
		if addr != nil {
			t.Fatalf("address = %+v, want none for an async merge's workspace", addr)
		}
	}
}

func TestTheAgenticTabKindsAreTheAgenticArms(t *testing.T) {
	// Arrange: every tab kind, built by its own builder.
	built := map[string]*frontendv1.FeedMergeTab{
		TabPick: pickTab(tabState{}),
		rowRebaseCommit: {Kind: &frontendv1.FeedMergeTab_RebaseCommit{RebaseCommit: &frontendv1.FeedMergeTabRebaseCommit{
			Conflict: &frontendv1.FeedMergeRebaseCommitConflict{}}}},
		rowTestsRun:       runTab(1, tabState{}, nil, nil),
		rowTestsRunFixing: fixingTab(tabState{}, 1),
		TabQueue:          {Kind: &frontendv1.FeedMergeTab_Queue{Queue: &frontendv1.FeedMergeTabQueue{}}},
		TabPreDeploy:      hookPromptTab(tabState{}),
		TabDeploy:         hookPromptTab(tabState{}),
		TabRebasing:       {Kind: &frontendv1.FeedMergeTab_Rebasing{Rebasing: &frontendv1.FeedMergeTabRebasing{}}},
		TabTests:          {Kind: &frontendv1.FeedMergeTab_Tests{Tests: &frontendv1.FeedMergeTabTests{}}},
		TabCommitting:     {Kind: &frontendv1.FeedMergeTab_Committing{Committing: &frontendv1.FeedMergeTabCommitting{}}},
		TabPostDeploy:     hookPromptTab(tabState{}),
	}
	for kind, tab := range built {
		t.Run(kind, func(t *testing.T) {
			// Act / Assert: the interrupt's kinds and the proto's arms agree.
			if agenticTabs[kind] != mergetab.Agentic(tab) {
				t.Fatalf("agenticTabs[%s] = %v, mergetab.Agentic = %v", kind, agenticTabs[kind], mergetab.Agentic(tab))
			}
		})
	}
}
