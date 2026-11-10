package workspace

import (
	"context"
	"slices"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

func TestSwitchConversationBindsTheConversationTheSwitchCallsFor(t *testing.T) {
	cases := []struct {
		name        string
		current     string
		parked      string
		taking      bool
		wantBound   string
		wantForkOf  string
		wantRebound bool
	}{
		{name: "the first switch into the mode forks the queue conversation", current: "vendor-queue", taking: true, wantForkOf: "vendor-queue", wantRebound: true},
		{name: "a later switch into the mode resumes the parked bypass conversation", current: "vendor-queue", parked: "vendor-bypass", taking: true, wantBound: "vendor-bypass", wantRebound: true},
		{name: "a release resumes the parked queue conversation", current: "vendor-bypass", parked: "vendor-queue", wantBound: "vendor-queue", wantRebound: true},
		{name: "a release with nothing parked starts fresh", current: "vendor-bypass"},
		{name: "the first switch from a workspace with no conversation starts fresh", taking: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			bindTo(f, "w1", tc.current)

			// Act
			err := f.verbs.(*verbs).switchConversation(context.Background(), dlog.NewTestLogger(), "UpdateQueueBypassMode", "w1", tc.parked, tc.taking)

			// Assert
			if err != nil {
				t.Fatalf("switchConversation: %v", err)
			}
			if got := f.db.sessions["w1"].VendorSessionID; got != tc.wantBound {
				t.Fatalf("bound conversation = %q, want %q", got, tc.wantBound)
			}
			fork, owed := f.db.pendingForks["w1"]
			if tc.wantForkOf == "" && owed {
				t.Fatalf("pending fork = %+v, want none", fork)
			}
			if want := (wsm.PendingFork{Workspace: "w1", Parent: "w1", ParentVendorSessionID: tc.wantForkOf}); tc.wantForkOf != "" && (!owed || fork != want) {
				t.Fatalf("pending fork = (%+v, %v), want %+v", fork, owed, want)
			}
			if got := slices.Contains(f.fleet.rebound, ids.WorkspaceID("w1")); got != tc.wantRebound {
				t.Fatalf("started as a rebind = %v, want %v", got, tc.wantRebound)
			}
		})
	}
}

func TestSwitchConversationParksTheConversationSteppedOutOf(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	bindTo(f, "w1", "vendor-queue")

	// Act
	err := f.verbs.(*verbs).switchConversation(context.Background(), dlog.NewTestLogger(), "UpdateQueueBypassMode", "w1", "", true)

	// Assert
	if err != nil {
		t.Fatalf("switchConversation: %v", err)
	}
	if got := f.db.parkedConversations["w1"]; got != "vendor-queue" {
		t.Fatalf("parked = %q, want the queue conversation kept for the release", got)
	}
}
