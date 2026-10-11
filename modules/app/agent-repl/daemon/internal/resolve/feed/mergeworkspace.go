package feed

import (
	"sort"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/mergetab"
)

// removedSwitchLabel is a merge-workspace switch's text once the workspace it
// named has been removed.
const removedSwitchLabel = "merge workspace removed"

// MarkMergeWorkspaceRemoved restates every switch in WS's feeds that takes the
// user to MERGEWS -- a detached merge bubble head's and its agentic tabs' --
// as removed at REMOVEDATMS, and answers how many rows it restated. The
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
	keys := make([]string, 0, len(s.feeds))
	for key := range s.feeds {
		keys = append(keys, key)
	}
	sort.Strings(keys)
	for _, key := range keys {
		f := s.feeds[key]
		at := placement{feed: s.feedAddrs[key]}
		for _, id := range append([]string(nil), f.order...) {
			stored := f.rows[id]
			if !switchesTo(stored, mergeWS) {
				continue
			}
			durable := !f.nonDurable[id]
			restated := r.restateRow(s, at, stored, durable, unclonable{
				operation: "daemon.feed.merge_workspace_removed_unclonable",
				message:   "a merge bubble row could not be cloned; its switch still offers the removed merge workspace",
				context:   dlog.Context{"row": id, "merge_workspace": string(mergeWS)},
			}, func(row *frontendv1.FeedRow) { markSwitchRemoved(row, removedAtMS) })
			if !restated {
				continue
			}
			if durable {
				r.recordDurable(s, at.feed, id, "")
			}
			marked++
		}
	}
	r.logger(ws).Info("daemon.feed.merge_workspace_removed",
		"a removed merge workspace's switches were restated as removed", dlog.Context{
			"merge_workspace": string(mergeWS), "rows": marked, "removed_at_ms": removedAtMS})
	return marked
}

// switchesTo reports whether ROW carries an available switch to MERGEWS: a
// merge head's, or an agentic merge tab's.
func switchesTo(row *frontendv1.FeedRow, mergeWS ids.WorkspaceID) bool {
	if target := row.GetActivity().GetMerge().GetHead().GetWorkspaceSwitch().GetAvailable().GetTarget(); target != nil {
		return target.GetId() == string(mergeWS)
	}
	if sw := mergetab.Switch(row.GetMergeTab()); sw != nil {
		return sw.GetAvailable().GetTarget().GetId() == string(mergeWS)
	}
	return false
}

// markSwitchRemoved turns ROW's switch to its removed arm.
func markSwitchRemoved(row *frontendv1.FeedRow, removedAtMS int64) {
	if head := row.GetActivity().GetMerge().GetHead(); head.GetWorkspaceSwitch() != nil {
		head.WorkspaceSwitch = &frontendv1.FeedMergeWorkspaceSwitch{
			Label: &frontendv1.FeedMergeWorkspaceSwitchLabel{Text: removedSwitchLabel},
			Availability: &frontendv1.FeedMergeWorkspaceSwitch_Removed{
				Removed: &frontendv1.FeedMergeWorkspaceSwitchRemoved{RemovedAtMs: removedAtMS},
			},
		}
		return
	}
	if sw := mergetab.Switch(row.GetMergeTab()); sw != nil {
		sw.Label = &frontendv1.FeedMergeTabWorkspaceSwitchLabel{Text: removedSwitchLabel}
		sw.Availability = &frontendv1.FeedMergeTabWorkspaceSwitch_Removed{
			Removed: &frontendv1.FeedMergeTabWorkspaceSwitchRemoved{RemovedAtMs: removedAtMS},
		}
	}
}
