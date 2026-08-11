package frontend

import (
	"sort"

	"google.golang.org/protobuf/proto"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// ---------------------------------------------------------------------------
// Connect-snapshot batching
// ---------------------------------------------------------------------------
//
// THE HOST APPLIES A CONNECT SNAPSHOT ONE WORKSPACE AT A TIME, and that apply
// — not the decode — is what a workspace's recovery clock waits on. Measured on
// the real fleet (178 workspaces / 289 sessions): the 299KB protojson frame
// decodes in ~3ms in Emacs, and the per-workspace apply that follows costs
// ~18ms each, so the LAST workspace in `workspaces` stamped its recovery signal
// ~3.2s after the frame landed. A workspace's recovery was therefore a function
// of how many other workspaces happened to precede it — the roster size, not
// anything about that workspace.
//
// The fix is to make the delivery incremental and to put the workspaces the
// host cares about most at the front of it. splitConnectSnapshot cuts the
// snapshot's `workspaces` list into batches carried by several StateSnapshot
// frames, enqueued back-to-back in the SAME delivery-lock operation as the
// unbatched frame was. No new protocol semantics are required for the split
// itself: `workspaces` is already a repeated per-workspace field the host
// applies per workspace, and WorkspaceState is already the transport's
// per-workspace frame.
//
// What IS new is the accounting, because "I have applied some workspaces" must
// never be mistaken for "I have the fleet": every batch states
// workspace_total, the count the WHOLE delivery will carry, and a consumer's
// view is complete only when it has applied that many distinct workspaces.
// See frame.proto's fields 16/17 and frontend-state.el's completeness tracker.

// connectBatchSize is the number of workspaces carried by each batch AFTER the
// lead one. It is a latency knob, not a correctness one: any positive value
// delivers the same fleet.
const connectBatchSize = 24

// leadBatchFloor is the minimum size of the LEAD batch. The lead batch always
// carries at least every live-session workspace (see orderConnectWorkspaces);
// this floor keeps a fleet with no live session at all from leading with a
// single-workspace frame.
const leadBatchFloor = 24

// workspaceLiveOnConnect reports whether a workspace has a session the host is
// likely to be sitting in: one whose current controller generation is attached,
// whether it is healthy, still coming up, or degraded. HIBERNATED,
// UNAVAILABLE and UNSPECIFIED are not live.
//
// It is deliberately generous. Getting a dormant workspace into the lead batch
// costs one batch slot; leaving the workspace the user is looking at out of it
// costs that workspace its whole recovery budget.
func workspaceLiveOnConnect(ws *frontendv1.WorkspaceState) bool {
	switch ws.GetConnectivity() {
	case frontendv1.SessionConnectivity_SESSION_CONNECTIVITY_CONNECTING,
		frontendv1.SessionConnectivity_SESSION_CONNECTIVITY_OPERATIONAL,
		frontendv1.SessionConnectivity_SESSION_CONNECTIVITY_DEGRADED:
		return true
	default:
		return false
	}
}

// orderConnectWorkspaces returns states ordered by how urgently the host needs
// them, WITHOUT mutating the caller's slice.
//
// THE ORDERING RULE, in full:
//
//  1. Workspaces with a LIVE session first (workspaceLiveOnConnect). These are
//     the workspaces the host is actually working in — the selected one is
//     essentially always among them — and they are the ones whose recovery the
//     SLO is measuring.
//  2. Then workspaces that HAVE a session but are not live (hibernated,
//     unavailable): a revival target is still closer to the user's attention
//     than a workspace with no session at all.
//  3. Then everything else.
//  4. Within each tier, most recently transitioned first (at_ms descending),
//     with the workspace path as the tiebreak so the order is TOTAL — a stable
//     connect order is what makes the batching reproducible in a test and in a
//     log.
//
// The daemon deliberately does not ask the host which workspace is selected:
// the host sends nothing before its connect snapshot, and waiting to be told
// would spend the very milliseconds this ordering exists to save.
func orderConnectWorkspaces(states []*frontendv1.WorkspaceState) []*frontendv1.WorkspaceState {
	ordered := make([]*frontendv1.WorkspaceState, len(states))
	copy(ordered, states)
	tier := func(ws *frontendv1.WorkspaceState) int {
		switch {
		case workspaceLiveOnConnect(ws):
			return 0
		case ws.GetSessionId() != "":
			return 1
		default:
			return 2
		}
	}
	sort.SliceStable(ordered, func(i, j int) bool {
		a, b := ordered[i], ordered[j]
		if ta, tb := tier(a), tier(b); ta != tb {
			return ta < tb
		}
		if a.GetAtMs() != b.GetAtMs() {
			return a.GetAtMs() > b.GetAtMs()
		}
		return a.GetWorkspace() < b.GetWorkspace()
	})
	return ordered
}

// splitConnectSnapshot returns the batches ONE connect delivery is made of.
//
// The lead batch (index 0) is the caller's snapshot with its `workspaces`
// replaced by the first slice of the connect order; it keeps every other field,
// because every other field is stated exactly once per connect — the wholesale
// rebuilds (sessions, inits) and the daemon-global views. Continuation batches
// carry `workspaces` and nothing else.
//
// The lead batch is sized to hold EVERY live-session workspace, so no workspace
// with a live session is ever left for a later batch, floored at
// leadBatchFloor so a fleet with nothing live still leads with a useful slice.
// A fleet whose workspaces are ALL live therefore ships as one batch, exactly
// as before: there is no ordering that makes 178 equally-urgent workspaces
// arrive sooner, and pretending otherwise would only add frames.
//
// Every batch states the same workspace_total, which is the total across the
// whole delivery and NOT the length of the batch. A snapshot with no workspaces
// yields exactly one batch, so a connect always delivers at least the lead
// frame and its globals.
func splitConnectSnapshot(snapshot *frontendv1.StateSnapshot) []*frontendv1.StateSnapshot {
	if snapshot == nil {
		return nil
	}
	ordered := orderConnectWorkspaces(snapshot.GetWorkspaces())
	total := int32(len(ordered))

	lead := proto.Clone(snapshot).(*frontendv1.StateSnapshot)
	lead.WorkspaceTotal = total
	lead.WorkspaceBatchIndex = 0

	leadSize := leadBatchFloor
	if live := liveWorkspaceCount(ordered); live > leadSize {
		leadSize = live
	}
	if leadSize > len(ordered) {
		leadSize = len(ordered)
	}
	lead.Workspaces = ordered[:leadSize]
	batches := []*frontendv1.StateSnapshot{lead}

	for i := leadSize; i < len(ordered); i += connectBatchSize {
		end := i + connectBatchSize
		if end > len(ordered) {
			end = len(ordered)
		}
		batches = append(batches, &frontendv1.StateSnapshot{
			Workspaces:          ordered[i:end],
			WorkspaceTotal:      total,
			WorkspaceBatchIndex: int32(len(batches)),
		})
	}
	return batches
}

// liveWorkspaceCount returns how many workspaces in an ORDERED connect list
// have a live session. The order puts every live workspace first, so this is
// the length of that leading run and the lead batch's floor.
func liveWorkspaceCount(ordered []*frontendv1.WorkspaceState) int {
	for i, ws := range ordered {
		if !workspaceLiveOnConnect(ws) {
			return i
		}
	}
	return len(ordered)
}
