package workspace

import (
	"context"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/feed"
)

// THE FEED'S HISTORY SOURCE (feed.HistorySource). History is loaded only on a
// reader's request (feed paging on demand): the feed asks the fleet for one
// store page of one agent, the fleet reads it from the workspace's shim, and
// hands it to the session watcher's views before the feed draws it, so a
// loaded page names the main agent and reconciles the footer exactly as a
// watch's opening page used to.

// historyReader is the one shim call ReadHistory needs.
type historyReader interface {
	ReadHistory(ctx context.Context, req *shimv1.ReadHistoryRequest) (*shimv1.ReadHistoryResponse, error)
}

// ReadHistory reads TARGET's newest page (AFTER nil) or the page before AFTER
// from the workspace's shim. A workspace with no session up answers
// feed.ErrNoHistorySource.
func (f *Fleet) ReadHistory(ctx context.Context, ws ids.WorkspaceID, target *conversationv1.AgentId, after *conversationv1.HistoryPointer) (*conversationv1.HistoryPage, error) {
	client, ok := f.Client(ws)
	if !ok {
		return nil, feed.ErrNoHistorySource
	}
	// THE SESSION IS NOT UP FOR A READER UNTIL ITS WATCHER IS: the watcher is
	// what names the main agent a page's rows are placed by, so a page read
	// before it would be drawn unplaceable. The feed serves what it holds and
	// loads the page when the watcher's first watch opens.
	watcher, ok := f.sessionWatcher(ws)
	if !ok {
		return nil, feed.ErrNoHistorySource
	}
	page, err := readHistory(ctx, client, target, after)
	if err != nil {
		return nil, err
	}
	watcher.NoteHistoryLoaded(target, page, after == nil)
	return page, nil
}

// readHistory makes the one ReadHistory call and unwraps its outcome.
func readHistory(ctx context.Context, client historyReader, target *conversationv1.AgentId, after *conversationv1.HistoryPointer) (*conversationv1.HistoryPage, error) {
	req := &shimv1.ReadHistoryRequest{Target: target}
	if after != nil {
		req.Position = &shimv1.ReadHistoryRequest_After{After: after}
	} else {
		req.Position = &shimv1.ReadHistoryRequest_First{First: &shimv1.ReadHistoryFirst{}}
	}
	response, err := client.ReadHistory(ctx, req)
	if err != nil {
		return nil, err
	}
	if failure := response.GetFailure(); failure != nil {
		return nil, &ShimRefusal{Verb: "ReadHistory", Arm: readHistoryArm(failure), Detail: failure.GetDetail()}
	}
	success := response.GetSuccess()
	if success == nil || success.GetPage() == nil {
		return nil, fmt.Errorf("shim ReadHistory answered neither a page nor a failure for agent %q", target.GetValue())
	}
	return success.GetPage(), nil
}

// readHistoryArm names a ReadHistory refusal's arm.
func readHistoryArm(failure *shimv1.ReadHistoryFailure) string {
	switch {
	case failure.GetUnknownAgent() != nil:
		return "unknown_agent"
	case failure.GetStalePointer() != nil:
		return "stale_pointer"
	case failure.GetStoreUnavailable() != nil:
		return "store_unavailable"
	default:
		return ArmShimUnspecified
	}
}
