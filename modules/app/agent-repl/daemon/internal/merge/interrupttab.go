package merge

import (
	"context"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
)

// This file is the merge-tab interrupt (agentrepl.v1.InterruptRequest
// .merge_tab): the user stops the agent an AGENTIC tab draws, from the tab.
// For an ASYNC merge that agent runs in the hidden merge workspace, and
// stopping it reveals that workspace (owner ruling, 2026-10-11); for a SYNC
// merge it is the requester's own agent. Either way the merge then goes on as
// its step decides on a turn that ended interrupted — which, for every agentic
// step, fails the merge and hands the workspace to the user.

// agenticTabs are the row ids of the tabs whose content is a session's
// turns: the rows a merge-tab interrupt may name. A conflict resolution is
// drawn in its commit's subtab and a fixing attempt in its run's fixing
// sub-subtab. Each row's tab is an agentic arm mergetab.Agentic names, which
// interrupttab_test.go holds them to.
var agenticTabs = map[string]bool{
	TabPick:           true,
	TabPreDeploy:      true,
	rowRebaseCommit:   true,
	rowTestsRunFixing: true,
	TabDeploy:         true,
	TabPostDeploy:     true,
}

// InterruptTab stops the turn a live agentic tab of REQUESTER's feed draws
// (Orchestrator).
func (o *orchestrator) InterruptTab(ctx context.Context, requester ids.WorkspaceID, tab feedid.Ref) (bool, error) {
	const op = "daemon.merge.interrupt_tab"
	log := o.log(ctx, requester)
	fields := dlog.Context{"workspace": string(requester), "tab_kind": tab.Row.Kind, "tab": tab.Row.ID, "round": tab.Row.Sub}
	ws, live := o.liveAgenticTab(requester, tab)
	if !live {
		refusal := refuse(ArmNotMergeTab, requester, "the row is no live agentic tab of a merge drawn in this workspace's feed")
		logRefusal(log, op, "refused a merge-tab interrupt naming no live agentic tab", fields, refusal)
		return false, refusal
	}
	fields["merging_workspace"] = string(ws)
	running, err := o.deps.InterruptTurn(ctx, ws)
	if err != nil {
		log.Error(op, "could not stop the turn the merge tab draws", withField(fields, "error", err.Error()))
		return false, err
	}
	log.Info(op, "the user stopped the agent a merge tab draws", withField(fields, "running", running))
	if _, async := o.requesterOf(ws); async {
		o.reveal(ctx, ws, "the user interrupted its agent from the bubble")
	}
	return running, nil
}

// liveAgenticTab answers the merging workspace whose live agentic tab TAB names
// in REQUESTER's feed, and false when it names none: another kind of row, a
// tab of a merge drawn in another feed, a resolved tab, or a row other than
// the one the live agentic round is drawn in.
func (o *orchestrator) liveAgenticTab(requester ids.WorkspaceID, tab feedid.Ref) (ids.WorkspaceID, bool) {
	if tab.WS != requester || tab.Feed.Merge == nil || tab.Row.Kind != feedid.KindMergeTab || !agenticTabs[tab.Row.ID] {
		return "", false
	}
	lease := *tab.Feed.Merge
	o.mu.Lock()
	var ws ids.WorkspaceID
	for candidate, ledger := range o.ledgerOf {
		if ledger == lease {
			ws = candidate
			break
		}
	}
	p, picking := o.picks[ws]
	o.mu.Unlock()
	if ws == "" || o.bubbleOf(ws).ws != requester {
		return "", false
	}
	if picking {
		pickKey := roundTabKey(TabPick, 1)
		return ws, tab.Row.ID == pickKey.id && tab.Row.Sub == pickKey.sub && p.ledger == lease
	}
	r, running := o.runFor(ws)
	if !running {
		return "", false
	}
	at, live := r.liveAgentic()
	return ws, live && at.id == tab.Row.ID && at.sub == tab.Row.Sub
}
