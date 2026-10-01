package server

import (
	"context"
	"fmt"

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

	var (
		success *agentreplv1.SelectFeedRowSuccess
		err     error
	)
	switch move := req.Msg.GetMove().(type) {
	case *agentreplv1.SelectFeedRowRequest_Response:
		success, err = s.stepSelection(subject.Log, ws, selectionResponse,
			s.deps.Feed.FinalResponses(ws), move.Response.GetDirection())
	case *agentreplv1.SelectFeedRowRequest_Prompt:
		success, err = s.stepSelection(subject.Log, ws, selectionPrompt,
			s.deps.Feed.RollbackPrompts(ws), move.Prompt.GetDirection())
	case *agentreplv1.SelectFeedRowRequest_Clear:
		s.endSelection(subject.Log, ws, nil, returnToTail(), "cleared")
		success = selectedNone()
	case *agentreplv1.SelectFeedRowRequest_LeftView:
		s.endSelection(subject.Log, ws, move.LeftView.GetRow(), stayInView(), "left_view")
		success = selectedNone()
	case *agentreplv1.SelectFeedRowRequest_Bubble:
		row := move.Bubble.GetRow()
		var selectable bool
		success, selectable, err = s.selectBubble(subject.Log, ws, row)
		if err == nil && !selectable {
			return answer(resp, s.refuse(subject.Log, rpc, resp, refusal{
				Arm:    "not_selectable",
				Reason: fmt.Sprintf("row %q is not a selectable bubble of this workspace's root feed", row.GetValue()),
				Fields: map[string]any{"row": row},
			}))
		}
	}
	if err != nil {
		return nil, fail(subject.Log, rpc, err)
	}
	resp.Result = &agentreplv1.SelectFeedRowResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}

// selectionKind is which kind of row a step walks.
type selectionKind int

const (
	selectionResponse selectionKind = iota
	selectionPrompt
	selectionBubble
)

func (k selectionKind) String() string {
	switch k {
	case selectionPrompt:
		return "prompt"
	case selectionBubble:
		return "bubble"
	}
	return "response"
}

// selectionOf composes the selection of ROW as KIND.
func selectionOf(kind selectionKind, row *frontendv1.FeedId) *frontendv1.FeedSelection {
	switch kind {
	case selectionPrompt:
		return &frontendv1.FeedSelection{Selection: &frontendv1.FeedSelection_Prompt{
			Prompt: &frontendv1.FeedSelectionPrompt{Row: row}}}
	case selectionBubble:
		return &frontendv1.FeedSelection{Selection: &frontendv1.FeedSelection_Bubble{
			Bubble: &frontendv1.FeedSelectionBubble{Row: row}}}
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
	case *frontendv1.FeedSelection_Bubble:
		return arm.Bubble.GetRow(), selectionBubble, true
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
) (*agentreplv1.SelectFeedRowSuccess, error) {
	s.selectionMu.Lock()
	defer s.selectionMu.Unlock()
	current, currentKind, held := selectedRow(s.selections[ws].GetSelection())
	if len(rows) == 0 {
		if held {
			s.setNoSelection(ws, returnToTail())
		}
		log.Info(opSelectFeedRow, "a selection step found no row of its kind to select",
			dlog.Context{"kind": kind.String(), "direction": direction.String(), "ended": held})
		return &agentreplv1.SelectFeedRowSuccess{Outcome: &agentreplv1.SelectFeedRowSuccess_NothingSelectable{
			NothingSelectable: &agentreplv1.SelectFeedRowSuccessNothingSelectable{}}}, nil
	}
	at := -1
	if held && currentKind == kind {
		at = indexOfFeedID(rows, current)
	}
	at = stepIndex(at, len(rows), direction)
	sel := selectionOf(kind, rows[at])
	if err := s.setRowSelection(ws, sel); err != nil {
		return nil, err
	}
	log.Info(opSelectFeedRow, "moved the feed selection",
		dlog.Context{"kind": kind.String(), "direction": direction.String(), "row": rows[at].GetValue(), "index": at, "of": len(rows)})
	return selectedSuccess(sel), nil
}

func selectedSuccess(sel *frontendv1.FeedSelection) *agentreplv1.SelectFeedRowSuccess {
	return &agentreplv1.SelectFeedRowSuccess{Outcome: &agentreplv1.SelectFeedRowSuccess_Selected{
		Selected: &agentreplv1.SelectFeedRowSuccessSelected{Selection: sel}}}
}

// selectBubble selects exactly ROW, the bubble the reader clicked, as the arm
// its kind takes: a final response is a `response`, a prompt a rollback can
// reach a `prompt`, and any other selectable bubble a `bubble`. selectable is
// false (and nothing changes) for a row the resolver did not publish
// selectable. Clicking the selected bubble again is the webapp's CLEAR, so a
// click naming the selected row restates it.
func (s *server) selectBubble(log dlog.Logger, ws ids.WorkspaceID, row *frontendv1.FeedId) (*agentreplv1.SelectFeedRowSuccess, bool, error) {
	if _, ok := s.deps.Feed.SelectableText(ws, row); !ok {
		log.Info(opSelectFeedRow, "a clicked row is not selectable", dlog.Context{"row": row.GetValue()})
		return nil, false, nil
	}
	kind := selectionBubble
	switch {
	case indexOfFeedID(s.deps.Feed.FinalResponses(ws), row) >= 0:
		kind = selectionResponse
	case indexOfFeedID(s.deps.Feed.RollbackPrompts(ws), row) >= 0:
		kind = selectionPrompt
	}
	sel := selectionOf(kind, row)
	s.selectionMu.Lock()
	defer s.selectionMu.Unlock()
	if err := s.setRowSelection(ws, sel); err != nil {
		return nil, true, err
	}
	log.Info(opSelectFeedRow, "selected a clicked bubble", dlog.Context{"kind": kind.String(), "row": row.GetValue()})
	return selectedSuccess(sel), true, nil
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
	row, kind, held := selectedRow(s.selections[ws].GetSelection())
	if !held || (only != nil && row.GetValue() != only.GetValue()) {
		log.Debug(opSelectFeedRow, "a selection end found nothing to end",
			dlog.Context{"because": because, "held": held, "selected": row.GetValue(), "named": only.GetValue()})
		return false
	}
	s.setNoSelection(ws, none)
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
	return selectedRow(s.selections[ws].GetSelection())
}

// selectionState is one workspace's selection as every client receives it:
// the selection the webapp draws and, while a row is selected, that row's
// text, which Emacs searches. A value is never mutated after it is published.
type selectionState struct {
	selection *frontendv1.FeedSelection
	markdown  string
}

// GetSelection answers the state's selection; nil (no state) is nothing
// selected.
func (st *selectionState) GetSelection() *frontendv1.FeedSelection {
	if st == nil {
		return nil
	}
	return st.selection
}

// setRowSelection selects the row SEL names, with its text, through
// setSelection. The rows a selection names are selectable by construction
// (final responses and rollback prompts are landed root-feed rows, and a click
// is checked before this), so a row whose text cannot be read is a resolver
// defect, answered as an error and nothing is changed. The caller holds
// selectionMu.
func (s *server) setRowSelection(ws ids.WorkspaceID, sel *frontendv1.FeedSelection) error {
	row, kind, _ := selectedRow(sel)
	text, ok := s.deps.Feed.SelectableText(ws, row)
	if !ok {
		return fmt.Errorf("the %s row %q to select is not a selectable root-feed row", kind, row.GetValue())
	}
	s.setSelection(ws, &selectionState{selection: sel, markdown: text.Markdown})
	return nil
}

// setNoSelection ends the workspace's selection with NONE through
// setSelection. The caller holds selectionMu.
func (s *server) setNoSelection(ws ids.WorkspaceID, none *frontendv1.FeedSelectionNone) {
	s.setSelection(ws, &selectionState{selection: &frontendv1.FeedSelection{
		Selection: &frontendv1.FeedSelection_None{None: none}}})
}

// setSelection is THE ONE WAY a workspace's selection changes: it stores STATE
// (nothing selected is stored as absence) and publishes it to the topic the
// root feed's watches and Emacs's host watch both read. The caller holds
// selectionMu, so the store and the publication are one step and the topic's
// order is the order the changes were made: no client can be handed an older
// selection after a newer one.
func (s *server) setSelection(ws ids.WorkspaceID, state *selectionState) {
	if _, _, held := selectedRow(state.selection); held {
		s.selections[ws] = state
	} else {
		delete(s.selections, ws)
	}
	s.selectionTopic(ws).Publish(state)
}

// hostSelectionOf is the selection as Emacs needs it: the kind of row, and the
// selected row's text for the composer's search.
func hostSelectionOf(state *selectionState) *agentreplv1.HostWorkspaceSelection {
	_, kind, held := selectedRow(state.selection)
	if !held {
		return &agentreplv1.HostWorkspaceSelection{Selection: &agentreplv1.HostWorkspaceSelection_None{
			None: &agentreplv1.HostWorkspaceSelectionNone{}}}
	}
	text := &agentreplv1.HostWorkspaceSelectionMarkdown{Text: state.markdown}
	switch kind {
	case selectionPrompt:
		return &agentreplv1.HostWorkspaceSelection{Selection: &agentreplv1.HostWorkspaceSelection_Prompt{
			Prompt: &agentreplv1.HostWorkspaceSelectionPrompt{Markdown: text}}}
	case selectionBubble:
		return &agentreplv1.HostWorkspaceSelection{Selection: &agentreplv1.HostWorkspaceSelection_Bubble{
			Bubble: &agentreplv1.HostWorkspaceSelectionBubble{Markdown: text}}}
	}
	return &agentreplv1.HostWorkspaceSelection{Selection: &agentreplv1.HostWorkspaceSelection_Response{
		Response: &agentreplv1.HostWorkspaceSelectionResponse{Markdown: text}}}
}

// selectionTopic answers a workspace's selection topic, minting it on first
// use exactly as hostTopic mints the host one.
func (s *server) selectionTopic(ws ids.WorkspaceID) *publish.Topic[*selectionState] {
	s.mu.Lock()
	defer s.mu.Unlock()
	t, ok := s.selectionTopics[ws]
	if !ok {
		t = &publish.Topic[*selectionState]{}
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
