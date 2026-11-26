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
)

// conflictHeld starts a merge whose rebase conflicts and holds it in its
// conflict resolution: the repair turn is submitted and its end is
// held until RELEASE is called. It answers the bubble's ledger identity.
func conflictHeld(t *testing.T, h *harness) (ids.LeaseID, func()) {
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
	enqueue(t, h)
	done := admitAsync(h, context.Background())
	<-submitted
	lease, ok := h.o.leaseOf(theWorkspace)
	if !ok {
		t.Fatal("the merge holds no ledger identity")
	}
	return lease, func() {
		close(gate)
		<-done
	}
}

func TestAMergeTabInterruptStopsTheMergesOwnAgent(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.interruptRunning = true
	lease, release := conflictHeld(t, h)
	defer release()

	// Act
	running, err := h.o.InterruptTab(context.Background(), theWorkspace, tabRef(theWorkspace, lease, resolutionKey(1, 0)))

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
}

func TestAMergeTabInterruptThatCannotStopTheTurnIsAnError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.interruptErr = errors.New("the shim refused")
	lease, release := conflictHeld(t, h)
	defer release()

	// Act
	_, err := h.o.InterruptTab(context.Background(), theWorkspace, tabRef(theWorkspace, lease, resolutionKey(1, 0)))

	// Assert
	if err == nil {
		t.Fatal("InterruptTab = nil, want the failure")
	}
	if _, ok := h.recordFor(dlog.LevelError, "daemon.merge.interrupt_tab"); !ok {
		t.Fatal("the failed stop was not recorded at ERROR")
	}
}

func TestAMergeTabInterruptNamingNoLiveAgenticTabIsRefused(t *testing.T) {
	tests := []struct {
		name string
		tab  func(lease ids.LeaseID) feedid.Ref
	}{
		{name: "a resolved tab", tab: func(lease ids.LeaseID) feedid.Ref { return tabRef(theWorkspace, lease, rebasingKey()) }},
		{name: "the subtab of the commit whose conflict is live", tab: func(lease ids.LeaseID) feedid.Ref { return tabRef(theWorkspace, lease, commitKey(1, 0)) }},
		{name: "a commit whose conflict is not the live one", tab: func(lease ids.LeaseID) feedid.Ref { return tabRef(theWorkspace, lease, resolutionKey(1, 1)) }},
		{name: "a fixing row with no fixing live", tab: func(lease ids.LeaseID) feedid.Ref { return tabRef(theWorkspace, lease, fixingKey(1)) }},
		{name: "another feed's tab", tab: func(lease ids.LeaseID) feedid.Ref { return tabRef(otherWorkspace, lease, resolutionKey(1, 0)) }},
		{name: "an unknown bubble", tab: func(ids.LeaseID) feedid.Ref { return tabRef(theWorkspace, "lease-unknown", resolutionKey(1, 0)) }},
		{name: "a head", tab: func(lease ids.LeaseID) feedid.Ref { return headRef(theWorkspace, lease) }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			lease, release := conflictHeld(t, h)
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

func TestTheAgenticTabKindsAreTheAgenticArms(t *testing.T) {
	// Arrange: every tab kind, built by its own builder.
	built := map[string]*frontendv1.FeedMergeTab{
		rowRebaseCommit:   {Kind: &frontendv1.FeedMergeTab_RebaseCommit{RebaseCommit: &frontendv1.FeedMergeTabRebaseCommit{}}},
		TabConflicts:      (&parentView{}).conflictsTab(modeRunning),
		rowTestsRun:       runTab(1, tabState{}, &frontendv1.FeedMergeTestsRunOutput{}),
		TabFixes:          (&parentView{}).fixingTestsTab(modeRunning),
		rowTestsRunFixing: fixingTab(1, tabState{}, 1),
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
