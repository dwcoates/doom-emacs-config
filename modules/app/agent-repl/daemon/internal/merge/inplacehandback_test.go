package merge

import (
	"context"
	"testing"
)

func TestASyncMergeThatDidNotLandIsHandedBackToItsOwnWorkspace(t *testing.T) {
	tests := []struct {
		name string
		kind DetachedOutcomeKind
	}{
		{name: "a failure", kind: DetachedFailed},
		{name: "an abandonment nobody asked for", kind: DetachedAbandoned},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: a sync merge, which has no async requester.
			h := newHarness(t)

			// Act.
			h.o.handBack(context.Background(), theWorkspace, DetachedOutcome{Kind: tt.kind, Summary: "the gate failed"})

			// Assert: told to the workspace it ran in, marked in place, naming
			// that workspace's branch.
			sent := h.detached.sent()
			if len(sent) != 1 || sent[0].requester != theWorkspace || sent[0].mergeWS != theWorkspace {
				t.Fatalf("handbacks = %+v, want one to the workspace itself", sent)
			}
			out := sent[0].outcome
			if !out.InPlace || out.Kind != tt.kind || out.Branch != h.db.workspaces[theWorkspace].Branch {
				t.Fatalf("outcome = %+v, want an in-place %v naming the workspace's branch", out, tt.kind)
			}
		})
	}
}

func TestASyncMergeThatLandedOrWasTakenOffIsNotHandedBack(t *testing.T) {
	tests := []struct {
		name string
		out  DetachedOutcome
	}{
		{name: "a landing, which closed the workspace", out: DetachedOutcome{Kind: DetachedLanded}},
		{name: "an end somebody asked for", out: DetachedOutcome{Kind: DetachedAbandoned, Requested: true}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			h.o.handBack(context.Background(), theWorkspace, tt.out)

			// Assert.
			if sent := h.detached.sent(); len(sent) != 0 {
				t.Fatalf("handbacks = %+v, want none", sent)
			}
		})
	}
}

func TestAnInPlaceHandbackWhoseWorkspaceCannotBeReadIsAnError(t *testing.T) {
	// Arrange: the workspace's record is gone.
	h := newHarness(t)
	h.db.mu.Lock()
	delete(h.db.workspaces, theWorkspace)
	h.db.mu.Unlock()

	// Act.
	h.o.handBack(context.Background(), theWorkspace, DetachedOutcome{Kind: DetachedFailed, Summary: "x"})

	// Assert: nothing is sent, and the read failure is recorded at ERROR.
	if sent := h.detached.sent(); len(sent) != 0 {
		t.Fatalf("handbacks = %+v, want none", sent)
	}
	if !loggedLike(h, logLike{level: "error", op: "daemon.merge.handback"}) {
		t.Fatal("the unreadable workspace was not recorded at ERROR")
	}
}
