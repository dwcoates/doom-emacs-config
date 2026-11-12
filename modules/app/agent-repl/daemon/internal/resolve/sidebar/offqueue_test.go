package sidebar_test

import (
	"testing"

	"agent-repld/internal/resolve/sidebar"
)

func TestAMergeOffItsQueueIsDrawnWithItsControls(t *testing.T) {
	cases := []struct {
		name      string
		entry     sidebar.OffQueueEntry
		wantName  string
		wantClick string
	}{
		{name: "a workspace's own merge is named and selected as itself",
			entry: sidebar.OffQueueEntry{Workspace: "w1", Credited: "w1", Cause: "evicted"}, wantName: "one", wantClick: "w1"},
		{name: "an async merge is named as its merge workspace and selects its requester",
			entry: sidebar.OffQueueEntry{Workspace: "w2", Credited: "w1", Async: true, Cause: "failed"}, wantName: "two-merge", wantClick: "w1"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: no merge in line, one off the queue.
			r, _ := newResolver(t)
			r.SetOffQueue([]sidebar.OffQueueEntry{tc.entry})

			// Act.
			r.SetRegistry(queueView(workspace("w1", "one"), workspace("w2", "two-merge")))

			// Assert: the repository is listed for it alone, the row names it
			// and its cause, and both controls echo the merge's own workspace.
			queue := firstQueue(t, r)
			if queue.GetHead() != nil || len(queue.GetOffQueue()) != 1 {
				t.Fatalf("queue = %v, want no head and the one merge off the queue", queue)
			}
			row := queue.GetOffQueue()[0]
			if row.GetSubject().GetName() != tc.wantName || row.GetSubject().GetWorkspace().GetId() != tc.wantClick || row.GetCause().GetText() != tc.entry.Cause {
				t.Fatalf("row = %v, want %q selecting %s for %q", row, tc.wantName, tc.wantClick, tc.entry.Cause)
			}
			if row.GetRequeue().GetMerge().GetId() != string(tc.entry.Workspace) || row.GetDiscard().GetMerge().GetId() != string(tc.entry.Workspace) {
				t.Fatalf("controls = %v / %v, want both naming %s", row.GetRequeue(), row.GetDiscard(), tc.entry.Workspace)
			}
		})
	}
}

func TestAClosedWorkspacesMergeOffItsQueueIsNotDrawn(t *testing.T) {
	// Arrange: the close ended the request in the store; the roster has the
	// close before the orchestrator's next set.
	r, _ := newResolver(t)
	r.SetOffQueue([]sidebar.OffQueueEntry{{Workspace: "w1", Credited: "w1", Cause: "evicted"}})

	// Act.
	r.SetRegistry(queueView(closedWorkspace(workspace("w1", "one"))))

	// Assert.
	if group := queueGroup(latest(t, r), repo); group != nil {
		t.Fatalf("queue group = %v, want none for a closed workspace's merge", group)
	}
}

