package feed

import (
	"context"

	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/proto"

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
// A WORKSPACE THIS DAEMON HOLDS NO FEED STATE FOR IS NOT WOKEN: its durable
// record is restated in place (markDurableMergeWorkspaceRemoved), which is
// what its feed draws when it is next brought up. Bringing the state up here
// would wake a feed nobody reads -- a closed requester's, whose worktree the
// reaper may already have removed, so neither its log sink nor its footer
// binding exists (0.439.2: workspace_sink_unavailable and
// footer.unbound_workspace for agent-repl-theme-update).
func (r *resolver) MarkMergeWorkspaceRemoved(ws, mergeWS ids.WorkspaceID, removedAtMS int64) int {
	r.mu.Lock()
	defer r.mu.Unlock()
	s, held := r.workspaces[ws]
	if !held {
		return r.markDurableMergeWorkspaceRemoved(ws, mergeWS, removedAtMS)
	}
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

// markDurableMergeWorkspaceRemoved restates, in WS's durable record alone,
// every recorded row whose switch takes the user to MERGEWS as removed at
// REMOVEDATMS, and answers how many rows it restated. The caller holds the
// lock and holds no feed state for WS.
//
// ITS RECORDS GO TO THE GLOBAL LOG, naming the workspace: the workspace's own
// sink lives in its directory, which a closed workspace may no longer have.
// A row that cannot be read or recorded is an ERROR, and the others are still
// restated.
func (r *resolver) markDurableMergeWorkspaceRemoved(ws, mergeWS ids.WorkspaceID, removedAtMS int64) int {
	if r.deps.DurableRows == nil {
		return 0
	}
	log := r.deps.Log.Global().With(dlog.Context{"workspace": string(ws), "merge_workspace": string(mergeWS)})
	ctx := context.Background()
	rows, err := r.deps.DurableRows.DurableFeedRows(ctx, ws)
	if err != nil {
		log.Error("daemon.feed.merge_workspace_removed_unread",
			"a workspace's durable rows could not be read; its switches still offer the removed merge workspace",
			dlog.Context{"cause": err.Error()})
		return 0
	}
	marked := 0
	for _, stored := range rows {
		fields := dlog.Context{"row": stored.RowID}
		row := &frontendv1.FeedRow{}
		if err := proto.Unmarshal(stored.Row, row); err != nil {
			fields["cause"] = err.Error()
			log.Error("daemon.feed.merge_workspace_removed_undecoded",
				"a durable row could not be decoded; if it is a switch it still offers the removed merge workspace", fields)
			continue
		}
		if !switchesTo(row, mergeWS) {
			continue
		}
		markSwitchRemoved(row, removedAtMS)
		encoded, err := proto.Marshal(row)
		if err != nil {
			fields["cause"] = err.Error()
			log.Error("daemon.feed.merge_workspace_removed_unrecorded",
				"a restated switch could not be encoded; it still offers the removed merge workspace", fields)
			continue
		}
		stored.Row = encoded
		stored.SettledFrom = ""
		if err := r.deps.DurableRows.PutDurableFeedRow(ctx, stored); err != nil {
			fields["cause"] = err.Error()
			log.Error("daemon.feed.merge_workspace_removed_unrecorded",
				"a restated switch could not be recorded; it still offers the removed merge workspace", fields)
			continue
		}
		marked++
	}
	log.Info("daemon.feed.merge_workspace_removed",
		"a removed merge workspace's switches were restated as removed in the durable record of a workspace whose feed is not held",
		dlog.Context{"rows": marked, "removed_at_ms": removedAtMS})
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
