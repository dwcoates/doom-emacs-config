package server

import (
	"context"
	"errors"
	"fmt"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/feed"
	"claude-repld/internal/resolve/footer"
)

// The footer fault kinds a LoadFeedThrough's target-scoped failure is
// announced under: a transient `fault` line on the workspace's activity cell,
// where the selection was made (endpoint_load_feed_through.proto).
const (
	// faultFeedEntryNotFound is a walk that reached the conversation's start
	// without drawing the selected item's row.
	faultFeedEntryNotFound = "feed_entry_not_found"
	// faultFeedHistoryUnavailable is a walk a page of which could not be read.
	faultFeedHistoryUnavailable = "feed_history_unavailable"
)

// LoadFeedThrough brings one root-feed row into the reader's loaded pages,
// streaming every page between (endpoint_load_feed_through.proto): zero or
// more `page` frames, then exactly one terminal `reached` or `error`.
//
// The walk is the reader's own GetFeedPage walk, keyed exactly as GetFeedPage
// keys a root-feed walk, so a `next` after it continues from the oldest page
// it delivered.
func (s *server) LoadFeedThrough(
	ctx context.Context,
	req *connect.Request[agentreplv1.LoadFeedThroughRequest],
	out *connect.ServerStream[agentreplv1.LoadFeedThroughResponse],
) error {
	const rpc = "LoadFeedThrough"
	if err := validateLoadFeedThroughRequest(req.Msg); err != nil {
		return err
	}
	resp := &agentreplv1.LoadFeedThroughResponse{}
	subject, r, err := s.resolveRef(ctx, rpc, req.Msg.GetWorkspace())
	if err != nil {
		return fail(s.log, rpc, err)
	}
	if r != nil {
		return sendTerminal(out, resp, s.refuse(s.log, rpc, resp, *r))
	}
	ws := subject.Record.ID
	target := req.Msg.GetTarget()
	if reason, ok := rootRowOf(ws, target); !ok {
		return sendTerminal(out, resp, s.refuse(subject.Log, rpc, resp, refusal{
			Arm: "target_undecodable", Reason: reason, Fields: map[string]any{},
		}))
	}

	// THE READER'S OWN WALK, advanced; an unnamed one begins at the newest page.
	walk := req.Msg.GetWalk().GetValue()
	if walk == "" {
		walk = mintWalk()
	}
	reader := readerFor(walk, ws, nil)
	pages := 0
	reached, err := s.deps.Feed.LoadThrough(ctx, ws, reader, target, func(page *frontendv1.FeedPage) error {
		pages++
		return out.Send(&agentreplv1.LoadFeedThroughResponse{
			Frame: &agentreplv1.LoadFeedThroughResponse_Page{Page: stampWalk(page, walk)},
		})
	})
	switch {
	case err == nil:
		subject.Log.Info("daemon.server.load_feed_through",
			"a walk to a selected row reached it",
			dlog.Context{"target": target.GetValue(), "pages": pages})
		return out.Send(&agentreplv1.LoadFeedThroughResponse{Frame: &agentreplv1.LoadFeedThroughResponse_Reached{
			Reached: &agentreplv1.LoadFeedThroughReached{Target: reached},
		}})
	case errors.Is(err, feed.ErrTargetNotFound):
		detail := fmt.Sprintf("the selected item's row %q is not in this conversation", target.GetValue())
		s.announceWalkFault(subject.Log, ws, faultFeedEntryNotFound, detail, target, pages)
		return sendTerminal(out, resp, s.refuse(subject.Log, rpc, resp, refusal{
			Arm: "not_found", Reason: detail, Fields: map[string]any{}, Info: true,
		}))
	case errors.Is(err, feed.ErrHistoryUnavailable):
		s.announceWalkFault(subject.Log, ws, faultFeedHistoryUnavailable, err.Error(), target, pages)
		return sendTerminal(out, resp, s.refuse(subject.Log, rpc, resp, refusal{
			Arm: "history_unavailable", Reason: err.Error(), Fields: map[string]any{"detail": err.Error()}, Info: true,
		}))
	}
	return fail(subject.Log, rpc, err)
}

// rootRowOf reports whether TARGET decodes to a row of WS's root feed, and
// why not when it does not.
func rootRowOf(ws ids.WorkspaceID, target *frontendv1.FeedId) (string, bool) {
	ref, err := feedid.Decode(target)
	switch {
	case err != nil:
		return fmt.Sprintf("the target %q does not decode: %v", target.GetValue(), err), false
	case ref.WS != ws:
		return fmt.Sprintf("the target %q belongs to workspace %q, not %q", target.GetValue(), ref.WS, ws), false
	case !ref.Feed.Root:
		return fmt.Sprintf("the target %q is not a row of the root feed", target.GetValue()), false
	}
	return "", true
}

// announceWalkFault publishes a walk's target-scoped failure as a transient
// fault line on the workspace's footer activity: non-escalating, so it is
// announced once and never stands (footer.OpenFault).
func (s *server) announceWalkFault(log dlog.Logger, ws ids.WorkspaceID, kind, detail string, target *frontendv1.FeedId, pages int) {
	at := s.now()
	// INFO, not WARN: the footer line below IS the warning, and a workspace
	// WARN would be teed onto the same strip a second time.
	log.Info("daemon.server.load_feed_through_failed",
		"a walk to a selected row could not bring it into the feed; the footer says so",
		dlog.Context{"kind": kind, "detail": detail, "target": target.GetValue(), "pages": pages})
	s.deps.Footer.OpenFault(ws, footer.Fault{
		ID:     fmt.Sprintf("load_feed_through:%s:%d", target.GetValue(), at.UnixNano()),
		Kind:   kind,
		Detail: detail,
		At:     at,
	})
}

// sendTerminal sends a refusal's terminal error frame, or answers the
// transport error a refusal with no landed arm became.
func sendTerminal(out *connect.ServerStream[agentreplv1.LoadFeedThroughResponse], resp *agentreplv1.LoadFeedThroughResponse, cerr *connect.Error) error {
	if cerr != nil {
		return cerr
	}
	return out.Send(resp)
}
