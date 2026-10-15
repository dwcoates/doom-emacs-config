package server

import (
	"context"
	"fmt"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/publish"
	"agent-repld/internal/resolve/feed"
)

const opSelectFeedRow = "daemon.server.select_feed_row"

// SelectFeedRow moves, clears or ends the workspace's feed selection. See
// endpoint_select_feed_row.proto.
//
// THE DAEMON IS THE ONLY HOLDER OF THE SELECTION. A step reads the ordered
// turn boundaries from the feed resolver (the prompts a rollback can reach and
// the final responses, interleaved in feed order), computes the new row — see
// stepIndex for where a step starts and how it runs off each end — and pushes
// the result to the webapp's root feed watch and to Emacs's host watch. A sent prompt and a rollback read this same state, so nothing a
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
	case *agentreplv1.SelectFeedRowRequest_TurnBoundary:
		direction := move.TurnBoundary.GetDirection()
		var rows []feed.TurnBoundary
		rows, err = s.turnBoundaries(ctx, subject.Log, ws, direction)
		if err == nil {
			success, err = s.stepSelection(subject.Log, ws, rows, direction)
		}
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

// selectionKind is which kind of row is selected.
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

// turnBoundaryKind is the selection kind a turn-boundary row is selected as.
func turnBoundaryKind(k feed.TurnBoundaryKind) selectionKind {
	if k == feed.TurnBoundaryPrompt {
		return selectionPrompt
	}
	return selectionResponse
}

// turnBoundaries answers the turn boundaries, oldest first, from the pages the
// daemon holds — LOADING THE NEXT OLDER ROOT PAGE while a step OLDER would
// otherwise run past the oldest loaded row (feed paging on demand, ruling 7).
// A loaded page's rows are pushed to the root feed's tails, so the row the
// step lands on is on the reader's screen. Nothing older to load leaves the
// rows as held, and the step wraps as it always has.
func (s *server) turnBoundaries(
	ctx context.Context,
	log dlog.Logger,
	ws ids.WorkspaceID,
	direction agentreplv1.SelectFeedRowDirection,
) ([]feed.TurnBoundary, error) {
	read := func() []feed.TurnBoundary { return s.deps.Feed.TurnBoundaries(ws) }
	rows := read()
	loads := 0
	for direction == agentreplv1.SelectFeedRowDirection_SELECT_FEED_ROW_DIRECTION_OLDER && s.atOldestOf(ws, rows) {
		loaded, err := s.deps.Feed.LoadOlder(ctx, ws)
		if err != nil {
			return nil, err
		}
		if !loaded {
			break
		}
		loads++
		rows = read()
	}
	if loads > 0 {
		log.Info(opSelectFeedRow, "a step older ran past the oldest loaded row; older pages were loaded for it",
			dlog.Context{"pages": loads, "rows": len(rows)})
	}
	return rows, nil
}

// atOldestOf reports whether a step older along ROWS would run past the
// oldest loaded turn boundary: none is loaded, or the selection stands on the
// oldest one.
func (s *server) atOldestOf(ws ids.WorkspaceID, rows []feed.TurnBoundary) bool {
	if len(rows) == 0 {
		return true
	}
	s.selectionMu.Lock()
	defer s.selectionMu.Unlock()
	return indexOfTurnBoundary(rows, s.selections[ws].GetSelection()) == 0
}

// indexOfTurnBoundary answers the position in ROWS of the row SEL selects, or
// -1 when SEL selects nothing or a row the walk does not hold as that kind (a
// clicked bubble selected as `bubble`).
func indexOfTurnBoundary(rows []feed.TurnBoundary, sel *frontendv1.FeedSelection) int {
	current, currentKind, held := selectedRow(sel)
	if !held {
		return -1
	}
	for i, row := range rows {
		if row.Row.GetValue() == current.GetValue() && turnBoundaryKind(row.Kind) == currentKind {
			return i
		}
	}
	return -1
}

// stepSelection moves the selection one turn boundary in DIRECTION along ROWS
// (oldest first), selecting the landed row as its own kind, and pushes the
// result. No rows ends any selection and answers nothing_selectable, so the
// caller can say so.
func (s *server) stepSelection(
	log dlog.Logger,
	ws ids.WorkspaceID,
	rows []feed.TurnBoundary,
	direction agentreplv1.SelectFeedRowDirection,
) (*agentreplv1.SelectFeedRowSuccess, error) {
	s.selectionMu.Lock()
	defer s.selectionMu.Unlock()
	selection := s.selections[ws].GetSelection()
	current, currentKind, held := selectedRow(selection)
	if len(rows) == 0 {
		if held {
			s.setNoSelection(ws, returnToTail())
		}
		log.Info(opSelectFeedRow, "a selection step found no turn boundary to select",
			dlog.Context{"direction": direction.String(), "ended": held})
		return &agentreplv1.SelectFeedRowSuccess{Outcome: &agentreplv1.SelectFeedRowSuccess_NothingSelectable{
			NothingSelectable: &agentreplv1.SelectFeedRowSuccessNothingSelectable{}}}, nil
	}
	at, ended := stepIndex(indexOfTurnBoundary(rows, selection), len(rows), held, direction)
	if ended {
		s.setNoSelection(ws, returnToTail())
		log.Info(opSelectFeedRow, "a step newer ran past the newest row; the selection ended and the feed returns to its tail",
			dlog.Context{"kind": currentKind.String(), "row": current.GetValue(), "of": len(rows)})
		return selectedNone(), nil
	}
	landed := rows[at]
	kind := turnBoundaryKind(landed.Kind)
	sel := selectionOf(kind, landed.Row)
	if err := s.setRowSelection(ws, sel); err != nil {
		return nil, err
	}
	log.Info(opSelectFeedRow, "moved the feed selection",
		dlog.Context{"kind": kind.String(), "direction": direction.String(), "row": landed.Row.GetValue(), "index": at, "of": len(rows)})
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
// selectable. A click naming the SELECTED row deselects it, leaving the
// viewport where the reader has it (stay): the daemon, the selection's one
// holder, decides the toggle, so a click can never act on a selection the
// webapp has not yet heard has moved.
func (s *server) selectBubble(log dlog.Logger, ws ids.WorkspaceID, row *frontendv1.FeedId) (*agentreplv1.SelectFeedRowSuccess, bool, error) {
	if _, ok := s.deps.Feed.SelectableText(ws, row); !ok {
		log.Info(opSelectFeedRow, "a clicked row is not selectable", dlog.Context{"row": row.GetValue()})
		return nil, false, nil
	}
	if s.endSelection(log, ws, row, stayInView(), "clicked_selected") {
		return selectedNone(), true, nil
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

// stepIndex is the index a step lands on among N rows (oldest first) from AT,
// -1 when no row the walk holds is selected; HELD is whether any row (of any
// kind) is selected. ended is true, and the index meaningless, when the step
// ends the selection instead.
//
//   - From nothing selected at all, OLDER starts at the newest row and NEWER
//     at the oldest, so a NEWER right after a step ran past the newest rolls
//     round to the oldest.
//   - From a selected row the walk does not hold, either direction starts at
//     the newest.
//   - OLDER past the oldest wraps to the newest.
//   - NEWER past the newest ends the selection (the feed returns to its tail).
func stepIndex(at, n int, held bool, direction agentreplv1.SelectFeedRowDirection) (index int, ended bool) {
	older := direction == agentreplv1.SelectFeedRowDirection_SELECT_FEED_ROW_DIRECTION_OLDER
	switch {
	case at < 0 && !held && !older:
		return 0, false
	case at < 0:
		return n - 1, false
	case older:
		return (at - 1 + n) % n, false
	case at == n-1:
		return 0, true
	}
	return at + 1, false
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

// returnFeedToTail ends WS's selection, if one stands, with return_to_tail:
// the user switched to the workspace, and a switch lands on the newest row
// with the follow re-armed. The end goes through endSelection, so the root
// feed's watch and Emacs's host watch both receive it, in order with every
// other selection change.
func (s *server) returnFeedToTail(ctx context.Context, ws ids.WorkspaceID) {
	const op = "daemon.server.return_feed_to_tail"
	log, err := s.workspaceLog(ctx, "SelectWorkspace", ws)
	if err != nil {
		s.log.Error(op, "could not resolve the selected workspace; its feed selection was not ended",
			dlog.Context{"workspace": string(ws), "cause": err.Error()})
		return
	}
	s.endSelection(log, ws, nil, returnToTail(), "workspace_selected")
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
// use exactly as hostNoticeTopic mints the host one.
func (s *server) selectionTopic(ws ids.WorkspaceID) *publish.Topic[*selectionState] {
	return perWorkspace(s, s.selectionTopics, ws)
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
