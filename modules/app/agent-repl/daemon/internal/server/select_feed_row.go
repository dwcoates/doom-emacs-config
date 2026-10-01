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

const opSelectFeedRow = "daemon.server.select_feed_row"

// SelectFeedRow moves, clears or ends the workspace's feed selection. See
// endpoint_select_feed_row.proto.
//
// THE DAEMON IS THE ONLY HOLDER OF THE SELECTION. A step reads the ordered
// selectable rows of its kind from the feed resolver (final responses, or the
// prompts a rollback can reach), computes the new row — both directions start
// at the newest when nothing of that kind is selected, and wrap at each end —
// and pushes the result to the webapp's root feed watch and to Emacs's host
// watch. A sent prompt and a rollback read this same state, so nothing a
// client holds can disagree with what the webapp draws.
func (s *server) SelectFeedRow(
	ctx context.Context,
	req *connect.Request[agentreplv1.SelectFeedRowRequest],
) (*connect.Response[agentreplv1.SelectFeedRowResponse], error) {
	const rpc = "SelectFeedRow"
	if err := validateSelectFeedRowRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.SelectFeedRowResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	ws := subject.Record.ID

	var success *agentreplv1.SelectFeedRowSuccess
	switch move := req.Msg.GetMove().(type) {
	case *agentreplv1.SelectFeedRowRequest_Response:
		success = s.stepSelection(subject.Log, ws, selectionResponse,
			s.deps.Feed.FinalResponses(ws), move.Response.GetDirection())
	case *agentreplv1.SelectFeedRowRequest_Prompt:
		success = s.stepSelection(subject.Log, ws, selectionPrompt,
			s.deps.Feed.RollbackPrompts(ws), move.Prompt.GetDirection())
	case *agentreplv1.SelectFeedRowRequest_Clear:
		s.endSelection(subject.Log, ws, nil, returnToTail(), "cleared")
		success = selectedNone()
	case *agentreplv1.SelectFeedRowRequest_LeftView:
		s.endSelection(subject.Log, ws, move.LeftView.GetRow(), stayInView(), "left_view")
		success = selectedNone()
	}
	resp.Result = &agentreplv1.SelectFeedRowResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}

// selectionKind is which kind of row a step walks.
type selectionKind int

const (
	selectionResponse selectionKind = iota
	selectionPrompt
)

func (k selectionKind) String() string {
	if k == selectionPrompt {
		return "prompt"
	}
	return "response"
}

// selectionOf composes the selection of ROW as KIND.
func selectionOf(kind selectionKind, row *frontendv1.FeedId) *frontendv1.FeedSelection {
	if kind == selectionPrompt {
		return &frontendv1.FeedSelection{Selection: &frontendv1.FeedSelection_Prompt{
			Prompt: &frontendv1.FeedSelectionPrompt{Row: row}}}
	}
	return &frontendv1.FeedSelection{Selection: &frontendv1.FeedSelection_Response{
		Response: &frontendv1.FeedSelectionResponse{Row: row}}}
}

// selectedRow answers the row a selection names, and of which kind; ok is
// false when nothing is selected.
func selectedRow(sel *frontendv1.FeedSelection) (*frontendv1.FeedId, selectionKind, bool) {
	switch arm := sel.GetSelection().(type) {
	case *frontendv1.FeedSelection_Response:
		return arm.Response.GetRow(), selectionResponse, true
	case *frontendv1.FeedSelection_Prompt:
		return arm.Prompt.GetRow(), selectionPrompt, true
	}
	return nil, 0, false
}

func returnToTail() *frontendv1.FeedSelectionNone {
	return &frontendv1.FeedSelectionNone{Viewport: &frontendv1.FeedSelectionNone_ReturnToTail{
		ReturnToTail: &frontendv1.FeedSelectionNoneReturnToTail{}}}
}

func stayInView() *frontendv1.FeedSelectionNone {
	return &frontendv1.FeedSelectionNone{Viewport: &frontendv1.FeedSelectionNone_Stay{
		Stay: &frontendv1.FeedSelectionNoneStay{}}}
}

func selectedNone() *agentreplv1.SelectFeedRowSuccess {
	return &agentreplv1.SelectFeedRowSuccess{Outcome: &agentreplv1.SelectFeedRowSuccess_None{
		None: &agentreplv1.SelectFeedRowSuccessNone{}}}
}

// stepSelection moves the selection one row of KIND in DIRECTION along ROWS
// (oldest first) and pushes the result. No rows of that kind ends any
// selection and answers nothing_selectable, so the caller can say so.
func (s *server) stepSelection(
	log dlog.Logger,
	ws ids.WorkspaceID,
	kind selectionKind,
	rows []*frontendv1.FeedId,
	direction agentreplv1.SelectFeedRowDirection,
) *agentreplv1.SelectFeedRowSuccess {
	s.selectionMu.Lock()
	defer s.selectionMu.Unlock()
	current, currentKind, held := selectedRow(s.selections[ws])
	if len(rows) == 0 {
		if held {
			s.setSelection(ws, &frontendv1.FeedSelection{Selection: &frontendv1.FeedSelection_None{None: returnToTail()}})
		}
		log.Info(opSelectFeedRow, "a selection step found no row of its kind to select",
			dlog.Context{"kind": kind.String(), "direction": direction.String(), "ended": held})
		return &agentreplv1.SelectFeedRowSuccess{Outcome: &agentreplv1.SelectFeedRowSuccess_NothingSelectable{
			NothingSelectable: &agentreplv1.SelectFeedRowSuccessNothingSelectable{}}}
	}
	at := -1
	if held && currentKind == kind {
		at = indexOfFeedID(rows, current)
	}
	at = stepIndex(at, len(rows), direction)
	sel := selectionOf(kind, rows[at])
	s.setSelection(ws, sel)
	log.Info(opSelectFeedRow, "moved the feed selection",
		dlog.Context{"kind": kind.String(), "direction": direction.String(), "row": rows[at].GetValue(), "index": at, "of": len(rows)})
	return &agentreplv1.SelectFeedRowSuccess{Outcome: &agentreplv1.SelectFeedRowSuccess_Selected{
		Selected: &agentreplv1.SelectFeedRowSuccessSelected{Selection: sel}}}
}

// stepIndex is the index a step lands on among N rows from AT (-1 when
// nothing of the kind is selected): the newest from nothing, wrapping at
// each end.
func stepIndex(at, n int, direction agentreplv1.SelectFeedRowDirection) int {
	if at < 0 {
		return n - 1
	}
	if direction == agentreplv1.SelectFeedRowDirection_SELECT_FEED_ROW_DIRECTION_OLDER {
		return (at - 1 + n) % n
	}
	return (at + 1) % n
}

// endSelection ends the workspace's selection and pushes NONE. With ONLY set,
// it ends the selection only while that row is still the selected one: a
// left-view report about a row the user has since stepped away from changes
// nothing. It answers whether a selection ended.
func (s *server) endSelection(
	log dlog.Logger,
	ws ids.WorkspaceID,
	only *frontendv1.FeedId,
	none *frontendv1.FeedSelectionNone,
	because string,
) bool {
	s.selectionMu.Lock()
	defer s.selectionMu.Unlock()
	row, kind, held := selectedRow(s.selections[ws])
	if !held || (only != nil && row.GetValue() != only.GetValue()) {
		log.Debug(opSelectFeedRow, "a selection end found nothing to end",
			dlog.Context{"because": because, "held": held, "selected": row.GetValue(), "named": only.GetValue()})
		return false
	}
	s.setSelection(ws, &frontendv1.FeedSelection{Selection: &frontendv1.FeedSelection_None{None: none}})
	log.Info(opSelectFeedRow, "ended the feed selection",
		dlog.Context{"because": because, "kind": kind.String(), "row": row.GetValue()})
	return true
}

// currentSelection answers the workspace's selected row and its kind; held is
// false when nothing is selected. A caller acting on a selection reads it
// once here and ends only that row (endSelection with `only`).
func (s *server) currentSelection(ws ids.WorkspaceID) (row *frontendv1.FeedId, kind selectionKind, held bool) {
	s.selectionMu.Lock()
	defer s.selectionMu.Unlock()
	return selectedRow(s.selections[ws])
}

// setSelection is THE ONE WAY a workspace's selection changes: it stores SEL
// (a `none` selection is stored as absence) and publishes it to the topic the
// root feed's watches and Emacs's host watch both read. The caller holds
// selectionMu, so the store and the publication are one step and the topic's
// order is the order the changes were made: no client can be handed an older
// selection after a newer one.
func (s *server) setSelection(ws ids.WorkspaceID, sel *frontendv1.FeedSelection) {
	if _, _, held := selectedRow(sel); held {
		s.selections[ws] = sel
	} else {
		delete(s.selections, ws)
	}
	s.selectionTopic(ws).Publish(sel)
}

// hostSelectionOf is the selection as Emacs needs it: only the kind of row.
func hostSelectionOf(sel *frontendv1.FeedSelection) *agentreplv1.HostWorkspaceSelection {
	_, kind, held := selectedRow(sel)
	switch {
	case !held:
		return &agentreplv1.HostWorkspaceSelection{Selection: &agentreplv1.HostWorkspaceSelection_None{
			None: &agentreplv1.HostWorkspaceSelectionNone{}}}
	case kind == selectionPrompt:
		return &agentreplv1.HostWorkspaceSelection{Selection: &agentreplv1.HostWorkspaceSelection_Prompt{
			Prompt: &agentreplv1.HostWorkspaceSelectionPrompt{}}}
	}
	return &agentreplv1.HostWorkspaceSelection{Selection: &agentreplv1.HostWorkspaceSelection_Response{
		Response: &agentreplv1.HostWorkspaceSelectionResponse{}}}
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

// indexOfFeedID answers the position of id in rows by FeedId value, or -1.
func indexOfFeedID(rows []*frontendv1.FeedId, id *frontendv1.FeedId) int {
	if id == nil {
		return -1
	}
	for i, f := range rows {
		if f.GetValue() == id.GetValue() {
			return i
		}
	}
	return -1
}
