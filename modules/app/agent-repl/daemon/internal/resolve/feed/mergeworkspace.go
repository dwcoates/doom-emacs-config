package feed

import (
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
)

// removedSwitchLabel is a merge-workspace switch's text once the workspace it
// named has been removed.
const removedSwitchLabel = "merge workspace removed"

// MarkMergeWorkspaceRemoved restates every switch in WS's feeds that takes the
// user to MERGEWS -- an async merge bubble head's -- as removed at REMOVEDATMS, and answers how many rows it restated. The
// requester's bubble outlives the merge workspace it names, so its switches
// say the workspace is gone rather than offer a switch that would be refused.
//
// The workspace's feed state is brought up if this daemon holds none yet, so
// the rows a restart restored from the durable record are the ones restated.
func (r *resolver) MarkMergeWorkspaceRemoved(ws, mergeWS ids.WorkspaceID, removedAtMS int64) int {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	marked := 0
	r.eachStoredRow(s, func(addr feedid.Feed, f *feedState, id string, stored *frontendv1.FeedRow) {
		if !switchesTo(stored, mergeWS) {
			return
		}
		at := placement{feed: addr}
		durable := !f.nonDurable[id]
		restated := r.restateRow(s, at, stored, durable, unclonable{
			operation: "daemon.feed.merge_workspace_removed_unclonable",
			message:   "a merge bubble row could not be cloned; its switch still offers the removed merge workspace",
			context:   dlog.Context{"row": id, "merge_workspace": string(mergeWS)},
		}, func(row *frontendv1.FeedRow) { markSwitchRemoved(row, removedAtMS) })
		if !restated {
			return
		}
		if durable {
			r.recordDurable(s, at.feed, id, "")
		}
		marked++
	})
	r.logger(ws).Info("daemon.feed.merge_workspace_removed",
		"a removed merge workspace's switches were restated as removed", dlog.Context{
			"merge_workspace": string(mergeWS), "rows": marked, "removed_at_ms": removedAtMS})
	return marked
}

// switchesTo reports whether ROW is a merge head carrying an available switch
// to MERGEWS.
func switchesTo(row *frontendv1.FeedRow, mergeWS ids.WorkspaceID) bool {
	target := row.GetActivity().GetMerge().GetHead().GetMode().GetAsync().GetWorkspaceSwitch().GetAvailable().GetTarget()
	return target != nil && target.GetId() == string(mergeWS)
}

// markSwitchRemoved turns ROW's switch to its removed arm.
func markSwitchRemoved(row *frontendv1.FeedRow, removedAtMS int64) {
	if async := row.GetActivity().GetMerge().GetHead().GetMode().GetAsync(); async.GetWorkspaceSwitch() != nil {
		async.WorkspaceSwitch = &frontendv1.FeedMergeWorkspaceSwitch{
			Label: &frontendv1.FeedMergeWorkspaceSwitchLabel{Text: removedSwitchLabel},
			Availability: &frontendv1.FeedMergeWorkspaceSwitch_Removed{
				Removed: &frontendv1.FeedMergeWorkspaceSwitchRemoved{RemovedAtMs: removedAtMS},
			},
		}
	}
}
