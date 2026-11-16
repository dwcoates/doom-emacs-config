package merge

import (
	"context"
	"testing"
	"time"

	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"
)

func TestTheRosterClockOfEveryMergeIsItsEntrysClock(t *testing.T) {
	head := wsm.MergeClock{Phase: wsm.MergeClockActive, Since: time.UnixMilli(1_700_000_100_000)}
	behind := wsm.MergeClock{Phase: wsm.MergeClockWaiting, Since: time.UnixMilli(1_700_000_200_000), Waited: 3 * time.Minute}
	waiting := wsm.MergeClock{Phase: wsm.MergeClockWaiting, Since: time.UnixMilli(1_700_000_300_000)}
	aside := wsm.MergeClock{Phase: wsm.MergeClockActive, Since: time.UnixMilli(1_700_000_400_000)}
	tests := []struct {
		name string
		pick func(sidebar.MergeQueue) sidebar.QueueEntry
		want wsm.MergeClock
	}{
		{name: "the head", pick: func(q sidebar.MergeQueue) sidebar.QueueEntry { return q.Line[0] }, want: head},
		{name: "a merge waiting after an active stretch", pick: func(q sidebar.MergeQueue) sidebar.QueueEntry { return q.Line[1] }, want: behind},
		{name: "a merge waiting", pick: func(q sidebar.MergeQueue) sidebar.QueueEntry { return q.Line[2] }, want: waiting},
		{name: "a merge fixing tests out of line", pick: func(q sidebar.MergeQueue) sidebar.QueueEntry { return q.Remediating[0] }, want: aside},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: four merges, each in a different slot on its own clock.
			h := newHarness(t)
			names := []ids.WorkspaceID{"ws-head", "ws-behind", "ws-waiting", "ws-aside"}
			for _, ws := range names {
				h.register(ws, string(ws))
			}
			h.db.mu.Lock()
			h.db.queues[h.repoKey()] = []wsm.MergeQueueEntry{
				{Repo: h.repoKey(), EnqueuedAt: head.Since, Workspace: names[0], State: wsm.MergeAdmitted, Slot: wsm.MergeSlotHead, Clock: head},
				{Repo: h.repoKey(), EnqueuedAt: head.Since, Workspace: names[1], State: wsm.MergeQueued, Slot: wsm.MergeSlotWaiting, Clock: behind},
				{Repo: h.repoKey(), EnqueuedAt: head.Since, Workspace: names[2], State: wsm.MergeQueued, Slot: wsm.MergeSlotWaiting, Clock: waiting},
				{Repo: h.repoKey(), EnqueuedAt: head.Since, Workspace: names[3], State: wsm.MergeRemediating, Aside: wsm.MergeAsideTests, Slot: wsm.MergeSlotFixingTests, Clock: aside},
			}
			h.db.mu.Unlock()

			// Act
			if err := h.o.republishQueue(context.Background(), h.repoKey()); err != nil {
				t.Fatalf("republishQueue: %v", err)
			}

			// Assert
			if got := tc.pick(h.sidebar.queueOf(h.repoKey())).Clock; got != tc.want {
				t.Fatalf("clock = %+v, want the entry's own %+v", got, tc.want)
			}
		})
	}
}
