package server

import (
	"context"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/publish"
)

// SelectResponse moves or clears the per-workspace response-selection cursor
// (reply-to-a-past-response mode). See endpoint_select_response.proto.
//
// The client sends only a DIRECTION; the DAEMON owns the state. It reads the
// ordered selectable final-response rows from the feed resolver (the rows drawn
// with the green final-answer border), computes the new cursor — PREV/NEXT both
// START at the most recent when nothing is selected and WRAP at each end — and,
// on any change, PUSHES the selection on the root feed's watch (FeedSelection)
// so the webapp recolors and center-scrolls, and ACKS the resulting feedid
// here so Emacs can track state. Zero final responses makes PREV/NEXT a no-op
// that answers "none", never an error.
func (s *server) SelectResponse(
	ctx context.Context,
	req *connect.Request[agentreplv1.SelectResponseRequest],
) (*connect.Response[agentreplv1.SelectResponseResponse], error) {
	const rpc = "SelectResponse"
	if err := validateSelectResponseRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.SelectResponseResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}

	finals := s.deps.Feed.FinalResponses(subject.Record.ID)
	selected, changed := s.applySelection(subject.Record.ID, finals, req.Msg.GetDirection())
	if changed {
		s.pushSelection(subject.Record.ID, selected)
		subject.Log.Debug("daemon.server.select_response", "moved the response-selection cursor",
			dlog.Context{"direction": req.Msg.GetDirection().String(), "selected": selected.GetValue()})
	}

	success := &agentreplv1.SelectResponseSuccess{}
	if selected != nil {
		success.Selected = selected
	}
	resp.Result = &agentreplv1.SelectResponseResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}

// applySelection computes and stores the workspace's new selection cursor for
// one direction, reporting whether the cursor actually moved. It is the whole
// state machine: PREV/NEXT start at the most recent from no selection and wrap
// at each end; CLEAR drops the selection; an empty selectable set makes
// PREV/NEXT a no-op answering none. It holds mu across the read-and-write so two
// concurrent navs never race the cursor.
func (s *server) applySelection(
	ws ids.WorkspaceID,
	finals []*frontendv1.FeedId,
	direction agentreplv1.SelectResponseDirection,
) (*frontendv1.FeedId, bool) {
	s.mu.Lock()
	defer s.mu.Unlock()
	current := s.selections[ws]

	next := nextSelection(finals, current, direction)
	if feedIDValue(next) == feedIDValue(current) {
		return current, false
	}
	if next == nil {
		delete(s.selections, ws)
	} else {
		s.selections[ws] = next
	}
	return next, true
}

// nextSelection is the pure cursor arithmetic, factored out so the wrap and the
// start-at-most-recent rules are tested directly. It never touches server
// state.
//
//   - CLEAR: none.
//   - empty set: none (PREV/NEXT are no-ops; CLEAR is already none).
//   - PREV/NEXT from no selection: the most recent (the last element).
//   - PREV from the oldest wraps to the newest; NEXT from the newest wraps to
//     the oldest.
//   - a current selection no longer in the set (a stale cursor) is treated as
//     no selection, so the walk restarts at the most recent.
func nextSelection(
	finals []*frontendv1.FeedId,
	current *frontendv1.FeedId,
	direction agentreplv1.SelectResponseDirection,
) *frontendv1.FeedId {
	if direction == agentreplv1.SelectResponseDirection_SELECT_RESPONSE_DIRECTION_CLEAR {
		return nil
	}
	if len(finals) == 0 {
		return nil
	}
	last := len(finals) - 1

	idx := indexOfFeedID(finals, current)
	if idx < 0 {
		// No selection, or a stale one: both PREV and NEXT start at the most
		// recent.
		return finals[last]
	}
	switch direction {
	case agentreplv1.SelectResponseDirection_SELECT_RESPONSE_DIRECTION_PREV:
		idx--
		if idx < 0 {
			idx = last
		}
	case agentreplv1.SelectResponseDirection_SELECT_RESPONSE_DIRECTION_NEXT:
		idx++
		if idx > last {
			idx = 0
		}
	}
	return finals[idx]
}

// pushSelection publishes the current selection on the workspace's root-feed
// watch. active is true whenever a row is selected; on a clear (selected nil)
// active is false and the webapp returns to the feed bottom. center rides the
// newly selected row so the webapp center-scrolls it.
func (s *server) pushSelection(ws ids.WorkspaceID, selected *frontendv1.FeedId) {
	sel := &frontendv1.FeedSelection{Active: selected != nil}
	if selected != nil {
		sel.Selected = selected
		sel.Center = selected
	}
	s.selectionTopic(ws).Publish(sel)
}

// selectionTopic answers a workspace's FeedSelection push topic, minting it on
// first use exactly as hostTopic mints the host one.
func (s *server) selectionTopic(ws ids.WorkspaceID) *publish.Topic[*frontendv1.FeedSelection] {
	s.mu.Lock()
	defer s.mu.Unlock()
	t, ok := s.selectionTopics[ws]
	if !ok {
		t = &publish.Topic[*frontendv1.FeedSelection]{}
		s.selectionTopics[ws] = t
	}
	return t
}

// clearSelection drops the workspace's selection cursor and pushes the cleared
// state, reporting whether there was a selection to clear. It is the path a
// successful reply-consuming submit takes to drop the reference it used.
func (s *server) clearSelection(ws ids.WorkspaceID) bool {
	s.mu.Lock()
	_, had := s.selections[ws]
	delete(s.selections, ws)
	s.mu.Unlock()
	if had {
		s.pushSelection(ws, nil)
	}
	return had
}

// indexOfFeedID answers the position of id in finals by FeedId value, or -1.
func indexOfFeedID(finals []*frontendv1.FeedId, id *frontendv1.FeedId) int {
	if id == nil {
		return -1
	}
	for i, f := range finals {
		if f.GetValue() == id.GetValue() {
			return i
		}
	}
	return -1
}

// feedIDValue answers a FeedId's value, treating nil as the empty string so a
// "none" cursor compares equal to another "none".
func feedIDValue(id *frontendv1.FeedId) string { return id.GetValue() }
