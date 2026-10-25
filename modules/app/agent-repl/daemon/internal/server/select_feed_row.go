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
			return answer(resp, s.refuse(subject.Log, rpc, resp, notSelectable(row, "is not a selectable bubble of this workspace's root feed")))
		}
	case *agentreplv1.SelectFeedRowRequest_Answer:
		row := move.Answer.GetRow()
		var selectable bool
		success, selectable, err = s.selectAnswer(subject.Log, ws, row, move.Answer.GetLabel())
		if err == nil && !selectable {
			return answer(resp, s.refuse(subject.Log, rpc, resp, notSelectable(row, "is not a selectable prompt of this workspace's root feed")))
		}
	case *agentreplv1.SelectFeedRowRequest_Entry:
		row, label := move.Entry.GetRow(), move.Entry.GetLabel()
		var selectable bool
		success, selectable, err = s.selectEntry(subject.Log, ws, row, label)
		if err == nil && !selectable {
			return answer(resp, s.refuse(subject.Log, rpc, resp,
				notSelectable(row, fmt.Sprintf("is not a prompt of this workspace's root feed whose entry answering %q answers a selectable response", label))))
		}
	case *agentreplv1.SelectFeedRowRequest_Branch:
		row, label := move.Branch.GetRow(), move.Branch.GetLabel()
		var selectable bool
		success, selectable, err = s.selectBranch(subject.Log, ws, row, label)
		if err == nil && !selectable {
			return answer(resp, s.refuse(subject.Log, rpc, resp,
				notSelectable(row, fmt.Sprintf("is not a selectable response of this workspace's root feed whose tree holds %q", label))))
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

// selectionOf composes the selection of ROW as KIND, with what the webapp's
// viewport does about it: a step centers the row (centerRow), a click holds
// the feed still under it (holdUnderClick).
func selectionOf(kind selectionKind, row *frontendv1.FeedId, viewport *frontendv1.FeedSelectionRowViewport) *frontendv1.FeedSelection {
	switch kind {
	case selectionPrompt:
		return &frontendv1.FeedSelection{Selection: &frontendv1.FeedSelection_Prompt{
			Prompt: &frontendv1.FeedSelectionPrompt{Row: row, Viewport: viewport}}}
	case selectionBubble:
		return &frontendv1.FeedSelection{Selection: &frontendv1.FeedSelection_Bubble{
			Bubble: &frontendv1.FeedSelectionBubble{Row: row, Viewport: viewport}}}
	}
	return &frontendv1.FeedSelection{Selection: &frontendv1.FeedSelection_Response{
		Response: &frontendv1.FeedSelectionResponse{Row: row, Viewport: viewport}}}
}

// centerRow is the viewport of a selection a step from Emacs made: the webapp
// centers the newly selected row.
func centerRow() *frontendv1.FeedSelectionRowViewport {
	return &frontendv1.FeedSelectionRowViewport{Viewport: &frontendv1.FeedSelectionRowViewport_Center{
		Center: &frontendv1.FeedSelectionRowViewportCenter{}}}
}

// holdUnderClick is the viewport of a selection the reader's click made. THE
// READER'S CLICK NEVER MOVES THE FEED: the webapp holds the clicked bubble
// still while it expands, and centers nothing.
func holdUnderClick() *frontendv1.FeedSelectionRowViewport {
	return &frontendv1.FeedSelectionRowViewport{Viewport: &frontendv1.FeedSelectionRowViewport_Hold{
		Hold: &frontendv1.FeedSelectionRowViewportHold{}}}
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

// holdUnderClickNone ends a selection the reader clicked off: the webapp holds
// the clicked bubble still while it collapses (see holdUnderClick).
func holdUnderClickNone() *frontendv1.FeedSelectionNone {
	return &frontendv1.FeedSelectionNone{Viewport: &frontendv1.FeedSelectionNone_Hold{
		Hold: &frontendv1.FeedSelectionNoneHold{}}}
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
	sel := selectionOf(kind, landed.Row, centerRow())
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
// selectable. A click never moves the feed: a selection holds the clicked
// bubble still (holdUnderClick), and a click naming the SELECTED row
// deselects it, holding it still too (holdUnderClickNone). The daemon, the selection's one
// holder, decides the toggle, so a click can never act on a selection the
// webapp has not yet heard has moved.
func (s *server) selectBubble(log dlog.Logger, ws ids.WorkspaceID, row *frontendv1.FeedId) (*agentreplv1.SelectFeedRowSuccess, bool, error) {
	if _, ok := s.deps.Feed.SelectableText(ws, row); !ok {
		log.Info(opSelectFeedRow, "a clicked row is not selectable", dlog.Context{"row": row.GetValue()})
		return nil, false, nil
	}
	if s.endSelection(log, ws, row, holdUnderClickNone(), "clicked_selected") {
		return selectedNone(), true, nil
	}
	kind := s.promptKind(ws, row)
	if indexOfFeedID(s.deps.Feed.FinalResponses(ws), row) >= 0 {
		kind = selectionResponse
	}
	return s.selectClicked(ws, kind, row, holdUnderClick(), func() {
		log.Info(opSelectFeedRow, "selected a clicked bubble", dlog.Context{"kind": kind.String(), "row": row.GetValue()})
		if kind == selectionResponse {
			s.finalResponseClicked(log, ws, row)
		}
	})
}

// selectClicked sets ROW's selection as KIND with VIEWPORT, the one way a
// click selects, then runs THEN while still holding the selection lock, so
// what the click asks of the host follows the selection it made. It answers
// the move's success, that ROW was selectable, and any failure to set it.
func (s *server) selectClicked(
	ws ids.WorkspaceID,
	kind selectionKind,
	row *frontendv1.FeedId,
	viewport *frontendv1.FeedSelectionRowViewport,
	then func(),
) (*agentreplv1.SelectFeedRowSuccess, bool, error) {
	sel := selectionOf(kind, row, viewport)
	s.selectionMu.Lock()
	defer s.selectionMu.Unlock()
	if err := s.setRowSelection(ws, sel); err != nil {
		return nil, true, err
	}
	then()
	return selectedSuccess(sel), true, nil
}

// notSelectable is the refusal of a move naming ROW, which WHY says is not
// selectable as the move needs.
func notSelectable(row *frontendv1.FeedId, why string) refusal {
	return refusal{
		Arm:    "not_selectable",
		Reason: fmt.Sprintf("row %q %s", row.GetValue(), why),
		Fields: map[string]any{"row": row},
	}
}

// promptKind is the arm a selected prompt ROW takes: `prompt` when a rollback
// can reach it, else `bubble`.
func (s *server) promptKind(ws ids.WorkspaceID, row *frontendv1.FeedId) selectionKind {
	if indexOfFeedID(s.deps.Feed.RollbackPrompts(ws), row) >= 0 {
		return selectionPrompt
	}
	return selectionBubble
}

// selectAnswer selects ROW, the prompt whose entry answers the LABEL the
// reader clicked in a response, centered, and names the entry for the webapp
// to flash (reveal). It never toggles: the click was on the response, not on
// the prompt. selectable is false (and nothing changes) for a row that is not
// a selectable prompt of the root feed.
func (s *server) selectAnswer(log dlog.Logger, ws ids.WorkspaceID, row *frontendv1.FeedId, label string) (*agentreplv1.SelectFeedRowSuccess, bool, error) {
	if text, ok := s.deps.Feed.SelectableText(ws, row); !ok || !text.Prompt {
		log.Info(opSelectFeedRow, "a linked label named a row that is not a selectable prompt", dlog.Context{"row": row.GetValue(), "label": label})
		return nil, false, nil
	}
	kind := s.promptKind(ws, row)
	return s.selectClicked(ws, kind, row, revealEntry(label), func() {
		log.Info(opSelectFeedRow, "selected the prompt a linked label answers", dlog.Context{"kind": kind.String(), "row": row.GetValue(), "label": label})
	})
}

// revealEntry is the viewport of a selection a linked label made: center the
// prompt and flash its entry answering LABEL.
func revealEntry(label string) *frontendv1.FeedSelectionRowViewport {
	return &frontendv1.FeedSelectionRowViewport{Viewport: &frontendv1.FeedSelectionRowViewport_Reveal{
		Reveal: &frontendv1.FeedSelectionRowViewportReveal{EntryLabel: label}}}
}

// selectEntry selects the response the entry answering LABEL of the prompt ROW
// answers (feed.Resolver.EntryTarget), centered, naming the node the webapp
// boxes in it while the selection stands (revealNode). It never toggles: the
// click was on the prompt, not on the response, and it asks nothing of the
// host. selectable is false (and nothing changes) when the entry answers no
// selectable response.
func (s *server) selectEntry(log dlog.Logger, ws ids.WorkspaceID, row *frontendv1.FeedId, label string) (*agentreplv1.SelectFeedRowSuccess, bool, error) {
	target, ok := s.deps.Feed.EntryTarget(ws, row, label)
	if !ok {
		log.Info(opSelectFeedRow, "a clicked entry answers no response", dlog.Context{"row": row.GetValue(), "label": label})
		return nil, false, nil
	}
	if text, ok := s.deps.Feed.SelectableText(ws, target); !ok || text.Prompt {
		log.Info(opSelectFeedRow, "a clicked entry's response is not selectable",
			dlog.Context{"row": row.GetValue(), "label": label, "response": target.GetValue()})
		return nil, false, nil
	}
	kind := s.responseKind(ws, target)
	return s.selectClicked(ws, kind, target, revealNode(label), func() {
		log.Info(opSelectFeedRow, "selected the response a clicked entry answers, revealing its node",
			dlog.Context{"kind": kind.String(), "row": row.GetValue(), "label": label, "response": target.GetValue()})
	})
}

// revealNode is the viewport of a selection a prompt's entry made: center the
// response and box its node labelled LABEL, with the node's subtree.
func revealNode(label string) *frontendv1.FeedSelectionRowViewport {
	return &frontendv1.FeedSelectionRowViewport{Viewport: &frontendv1.FeedSelectionRowViewport_RevealNode{
		RevealNode: &frontendv1.FeedSelectionRowViewportRevealNode{Label: label}}}
}

// responseKind is the arm a selected response ROW takes: `response` when it
// is a final response, else `bubble`.
func (s *server) responseKind(ws ids.WorkspaceID, row *frontendv1.FeedId) selectionKind {
	if indexOfFeedID(s.deps.Feed.FinalResponses(ws), row) >= 0 {
		return selectionResponse
	}
	return selectionBubble
}

// selectBranch answers a click on the node LABEL of ROW's tree by asking the
// host to open an entry answering LABEL in the composer, and NOTHING ELSE
// (owner ruling, 2026-10-25): the click selects the entry, never the bubble,
// so the selection stands as it was and the success names it (unchanged).
// selectable is false (and nothing is asked of the host) for a row that is no
// selectable response holding the node.
func (s *server) selectBranch(log dlog.Logger, ws ids.WorkspaceID, row *frontendv1.FeedId, label string) (*agentreplv1.SelectFeedRowSuccess, bool, error) {
	if !s.deps.Feed.ResponseNode(ws, row, label) {
		log.Info(opSelectFeedRow, "a clicked branch is not a node of a response's tree", dlog.Context{"row": row.GetValue(), "label": label})
		return nil, false, nil
	}
	if text, ok := s.deps.Feed.SelectableText(ws, row); !ok || text.Prompt {
		log.Info(opSelectFeedRow, "a clicked branch's response is not selectable", dlog.Context{"row": row.GetValue(), "label": label})
		return nil, false, nil
	}
	s.selectionMu.Lock()
	defer s.selectionMu.Unlock()
	sel := s.selections[ws].GetSelection()
	selected, _, _ := selectedRow(sel)
	log.Info(opSelectFeedRow, "asked the host to open an entry answering a clicked branch; the selection is unchanged",
		dlog.Context{"row": row.GetValue(), "label": label, "selected": selected.GetValue()})
	s.hostEventStream(ws).Emit(&agentreplv1.WatchHostWorkspaceResponse{
		Push: &agentreplv1.WatchHostWorkspaceResponse_ComposeEntry{
			ComposeEntry: &agentreplv1.HostWorkspaceComposeEntry{Label: label},
		},
	})
	return unchanged(sel), true, nil
}

// unchanged is the success of a move that leaves the selection SEL in force:
// `selected` naming it, or `none` when nothing is selected.
func unchanged(sel *frontendv1.FeedSelection) *agentreplv1.SelectFeedRowSuccess {
	if _, _, held := selectedRow(sel); held {
		return selectedSuccess(sel)
	}
	return selectedNone()
}

// finalResponseClicked pushes final_response_clicked onto the workspace's host
// stream: the reader's click selected a final response, the one the next
// prompt replies to, so Emacs puts the keyboard in the composer in insert
// state. It rides the host EVENT stream, never a topic, because a topic
// replays its latest value to a late subscriber and a reconnect would focus
// the composer again for a click made long before.
func (s *server) finalResponseClicked(log dlog.Logger, ws ids.WorkspaceID, row *frontendv1.FeedId) {
	log.Info(opSelectFeedRow, "a click selected a final response; asked the host to focus the composer",
		dlog.Context{"row": row.GetValue()})
	s.hostEventStream(ws).Emit(&agentreplv1.WatchHostWorkspaceResponse{
		Push: &agentreplv1.WatchHostWorkspaceResponse_FinalResponseClicked{
			FinalResponseClicked: &agentreplv1.HostWorkspaceFinalResponseClicked{},
		},
	})
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
	// What the composer's entries answer reads the selection
	// (composer_entries.go).
	s.composerChanges.Emit(ws)
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
