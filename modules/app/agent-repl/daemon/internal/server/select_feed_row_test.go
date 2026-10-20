package server

import (
	"context"
	"errors"
	"os"
	"slices"
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
)

const (
	older = agentreplv1.SelectFeedRowDirection_SELECT_FEED_ROW_DIRECTION_OLDER
	newer = agentreplv1.SelectFeedRowDirection_SELECT_FEED_ROW_DIRECTION_NEWER
)

// feedIDs renders values as an ordered selectable row slice, oldest first.
func feedIDs(values ...string) []*frontendv1.FeedId {
	out := make([]*frontendv1.FeedId, len(values))
	for i, v := range values {
		out[i] = &frontendv1.FeedId{Value: v}
	}
	return out
}

// boundaryStep builds a SelectFeedRow request stepping the turn boundaries.
func boundaryStep(direction agentreplv1.SelectFeedRowDirection) *agentreplv1.SelectFeedRowRequest {
	return &agentreplv1.SelectFeedRowRequest{
		Workspace: ref(),
		Move: &agentreplv1.SelectFeedRowRequest_TurnBoundary{
			TurnBoundary: &agentreplv1.SelectFeedRowStep{Direction: direction},
		},
	}
}

// clearMove builds a SelectFeedRow request that clears the selection.
func clearMove() *agentreplv1.SelectFeedRowRequest {
	return &agentreplv1.SelectFeedRowRequest{
		Workspace: ref(),
		Move:      &agentreplv1.SelectFeedRowRequest_Clear{Clear: &agentreplv1.SelectFeedRowClear{}},
	}
}

// leftViewMove builds a SelectFeedRow request reporting a row left the
// viewport.
func leftViewMove(row *frontendv1.FeedId) *agentreplv1.SelectFeedRowRequest {
	return &agentreplv1.SelectFeedRowRequest{
		Workspace: ref(),
		Move:      &agentreplv1.SelectFeedRowRequest_LeftView{LeftView: &agentreplv1.SelectFeedRowLeftView{Row: row}},
	}
}

// TestStepIndex is the pure index arithmetic every step rides on: OLDER
// starts at the newest and NEWER at the oldest from nothing selected, either
// starts at the newest from a row of another kind, OLDER wraps past the
// oldest, and NEWER past the newest ends the selection.
func TestStepIndex(t *testing.T) {
	tests := []struct {
		name      string
		at        int
		n         int
		held      bool
		direction agentreplv1.SelectFeedRowDirection
		want      int
		wantEnded bool
	}{
		{name: "from nothing, OLDER starts at the newest", at: -1, n: 3, direction: older, want: 2},
		{name: "from nothing, NEWER starts at the oldest", at: -1, n: 3, direction: newer, want: 0},
		{name: "from another kind, OLDER starts at the newest", at: -1, n: 3, held: true, direction: older, want: 2},
		{name: "from another kind, NEWER starts at the newest", at: -1, n: 3, held: true, direction: newer, want: 2},
		{name: "OLDER walks older", at: 2, n: 3, held: true, direction: older, want: 1},
		{name: "NEWER walks newer", at: 0, n: 3, held: true, direction: newer, want: 1},
		{name: "OLDER wraps past the oldest to the newest", at: 0, n: 3, held: true, direction: older, want: 2},
		{name: "NEWER past the newest ends the selection", at: 2, n: 3, held: true, direction: newer, wantEnded: true},
		{name: "single row OLDER wraps to itself", at: 0, n: 1, held: true, direction: older, want: 0},
		{name: "single row NEWER ends the selection", at: 0, n: 1, held: true, direction: newer, wantEnded: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got, ended := stepIndex(tc.at, tc.n, tc.held, tc.direction)

			// Assert.
			if ended != tc.wantEnded || (!ended && got != tc.want) {
				t.Fatalf("stepIndex(%d, %d, %v, %v) = (%d, %v), want (%d, %v)",
					tc.at, tc.n, tc.held, tc.direction, got, ended, tc.want, tc.wantEnded)
			}
		})
	}
}

// TestSelectFeedRowTurnBoundaryStepAcksTheNewest pins that a step with
// nothing selected lands on, and acks, the most recent final response.
func TestSelectFeedRowTurnBoundaryStepAcksTheNewest(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.finals = feedIDs("a", "b", "c")

	// Act.
	resp, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(boundaryStep(older)))

	// Assert.
	if err != nil {
		t.Fatalf("SelectFeedRow: %v", err)
	}
	if got := resp.Msg.GetSuccess().GetSelected().GetSelection().GetResponse().GetRow().GetValue(); got != "c" {
		t.Fatalf("selected response = %q, want the most recent c", got)
	}
}

// TestSelectFeedRowTurnBoundaryStepWalksPromptsAndResponsesInFeedOrder pins
// that one step walks the prompts and the final responses as one sequence,
// selecting each row as its own kind.
func TestSelectFeedRowTurnBoundaryStepWalksPromptsAndResponsesInFeedOrder(t *testing.T) {
	type landed struct {
		row    string
		prompt bool
	}
	tests := []struct {
		name      string
		direction agentreplv1.SelectFeedRowDirection
		want      []landed
	}{
		{
			name:      "NEWER walks oldest to newest",
			direction: newer,
			want:      []landed{{"a", true}, {"b", false}, {"c", true}, {"d", false}},
		},
		{
			name:      "OLDER walks newest to oldest",
			direction: older,
			want:      []landed{{"d", false}, {"c", true}, {"b", false}, {"a", true}},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Feed.prompts = feedIDs("a", "c")
			h.Feed.finals = feedIDs("b", "d")
			var got []landed

			// Act.
			for range tc.want {
				resp, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(boundaryStep(tc.direction)))
				if err != nil {
					t.Fatalf("SelectFeedRow: %v", err)
				}
				sel := resp.Msg.GetSuccess().GetSelected().GetSelection()
				row, _, _ := selectedRow(sel)
				got = append(got, landed{row.GetValue(), sel.GetPrompt() != nil})
			}

			// Assert.
			if !slices.Equal(got, tc.want) {
				t.Fatalf("walk = %v, want %v", got, tc.want)
			}
		})
	}
}

// TestSelectFeedRowTurnBoundaryStepFromAClickedBubbleStartsAtTheNewest pins
// that a step from a selected bubble the walk does not hold starts at the
// newest turn boundary, in either direction.
func TestSelectFeedRowTurnBoundaryStepFromAClickedBubbleStartsAtTheNewest(t *testing.T) {
	for _, direction := range []agentreplv1.SelectFeedRowDirection{older, newer} {
		t.Run(direction.String(), func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Feed.prompts = feedIDs("a", "c")
			h.Feed.finals = feedIDs("b", "d")
			h.Feed.promptRows = map[string]bool{"agent-prompt": true}
			if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(bubbleMove(feedIDs("agent-prompt")[0]))); err != nil {
				t.Fatalf("seed a bubble selection: %v", err)
			}

			// Act.
			resp, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(boundaryStep(direction)))

			// Assert.
			if err != nil {
				t.Fatalf("SelectFeedRow: %v", err)
			}
			if got := resp.Msg.GetSuccess().GetSelected().GetSelection().GetResponse().GetRow().GetValue(); got != "d" {
				t.Fatalf("selected response = %q, want the newest d", got)
			}
		})
	}
}

// TestSelectFeedRowEmptySetEndsAHeldSelectionWithReturnToTail pins that a
// step with no turn boundary to select answers nothing_selectable AND, when
// a selection was held, ends it with a return_to_tail push.
func TestSelectFeedRowEmptySetEndsAHeldSelectionWithReturnToTail(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.finals = feedIDs("a", "b", "c")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := openRootWatch(t, h, ctx)
	if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(boundaryStep(older))); err != nil {
		t.Fatalf("seed a selection: %v", err)
	}
	if got := receiveSelection(t, stream).GetResponse().GetRow().GetValue(); got != "c" {
		t.Fatalf("seed selection = %q, want c", got)
	}
	h.Feed.finals = nil

	// Act.
	resp, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(boundaryStep(newer)))

	// Assert.
	if err != nil {
		t.Fatalf("SelectFeedRow: %v", err)
	}
	if resp.Msg.GetSuccess().GetNothingSelectable() == nil {
		t.Fatalf("outcome = %v, want nothing_selectable", resp.Msg.GetSuccess().GetOutcome())
	}
	sel := receiveSelection(t, stream)
	if sel.GetNone().GetReturnToTail() == nil {
		t.Fatalf("selection = %v, want none{return_to_tail} after the held selection ended", sel)
	}
}

// TestSelectFeedRowClearPushesReturnToTail pins that CLEAR answers `none` and
// pushes a return-to-tail frame, which is what returns the webapp to the feed
// bottom.
func TestSelectFeedRowClearPushesReturnToTail(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.finals = feedIDs("a", "b", "c")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := openRootWatch(t, h, ctx)
	if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(boundaryStep(older))); err != nil {
		t.Fatalf("seed a selection: %v", err)
	}
	receiveSelection(t, stream)

	// Act.
	resp, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(clearMove()))

	// Assert.
	if err != nil {
		t.Fatalf("SelectFeedRow: %v", err)
	}
	if resp.Msg.GetSuccess().GetNone() == nil {
		t.Fatalf("outcome = %v, want none", resp.Msg.GetSuccess().GetOutcome())
	}
	sel := receiveSelection(t, stream)
	if sel.GetNone().GetReturnToTail() == nil {
		t.Fatalf("selection = %v, want none{return_to_tail}", sel)
	}
}

// TestSelectFeedRowLeftViewOfTheSelectedRowPushesStay pins that a left-view
// report about the row currently selected ends the selection and pushes
// `stay`, leaving the viewport where the reader left it.
func TestSelectFeedRowLeftViewOfTheSelectedRowPushesStay(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.finals = feedIDs("a", "b", "c")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := openRootWatch(t, h, ctx)
	if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(boundaryStep(older))); err != nil {
		t.Fatalf("seed a selection: %v", err)
	}
	receiveSelection(t, stream)

	// Act.
	resp, err := h.Client.SelectFeedRow(context.Background(),
		connect.NewRequest(leftViewMove(&frontendv1.FeedId{Value: "c"})))

	// Assert.
	if err != nil {
		t.Fatalf("SelectFeedRow: %v", err)
	}
	if resp.Msg.GetSuccess().GetNone() == nil {
		t.Fatalf("outcome = %v, want none", resp.Msg.GetSuccess().GetOutcome())
	}
	sel := receiveSelection(t, stream)
	if sel.GetNone().GetStay() == nil {
		t.Fatalf("selection = %v, want none{stay}", sel)
	}
}

// TestSelectFeedRowLeftViewOfANonSelectedRowKeepsTheSelection pins that a
// left-view report naming a row the reader has since stepped away from
// changes nothing: no push fires, and the held selection stands for the next
// step to continue from. Steeping OLDER after a held "c" lands on "b"; a
// cleared selection would instead restart at the newest, "c" (stepIndex treats
// -1 with nothing held as "start at the newest" for OLDER).
func TestSelectFeedRowLeftViewOfANonSelectedRowKeepsTheSelection(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.finals = feedIDs("a", "b", "c")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := openRootWatch(t, h, ctx)
	if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(boundaryStep(older))); err != nil {
		t.Fatalf("seed a selection: %v", err)
	}
	receiveSelection(t, stream)

	// Act.
	if _, err := h.Client.SelectFeedRow(context.Background(),
		connect.NewRequest(leftViewMove(&frontendv1.FeedId{Value: "a"}))); err != nil {
		t.Fatalf("report a non-selected row leaving the viewport: %v", err)
	}
	resp, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(boundaryStep(older)))

	// Assert: the very next frame on the topic is the OLDER step's own push —
	// had the left-view report pushed anything, it would have arrived first.
	if err != nil {
		t.Fatalf("SelectFeedRow: %v", err)
	}
	if got := resp.Msg.GetSuccess().GetSelected().GetSelection().GetResponse().GetRow().GetValue(); got != "b" {
		t.Fatalf("selected response = %q, want b (the held c stepped older)", got)
	}
	if got := receiveSelection(t, stream).GetResponse().GetRow().GetValue(); got != "b" {
		t.Fatalf("pushed selection = %q, want b, with no interceding push from the left-view report", got)
	}
}

// TestSelectFeedRowValidation pins the InvalidArgument surface: an unset
// move, an unspecified step direction, and a left-view with no row are every
// refused before the handler runs, exactly as the proto states.
func TestSelectFeedRowValidation(t *testing.T) {
	tests := []struct {
		name string
		req  *agentreplv1.SelectFeedRowRequest
	}{
		{name: "unset move", req: &agentreplv1.SelectFeedRowRequest{Workspace: ref()}},
		{name: "unspecified direction on a turn-boundary step", req: boundaryStep(agentreplv1.SelectFeedRowDirection_SELECT_FEED_ROW_DIRECTION_UNSPECIFIED)},
		{name: "left_view without a row", req: leftViewMove(nil)},
		{name: "bubble without a row", req: bubbleMove(nil)},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			_, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(tc.req))

			// Assert.
			if connectCode(t, err) != connect.CodeInvalidArgument {
				t.Fatalf("code = %v, want InvalidArgument", connectCode(t, err))
			}
		})
	}
}

// TestSelectFeedRowRefusesAnUnknownWorkspace pins that the ref refusals mirror
// SelectWorkspace: an unregistered id answers the unknown_workspace arm.
func TestSelectFeedRowRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	req := boundaryStep(newer)
	req.Workspace = &workspacev1.WorkspaceRef{Id: "ws-nope"}

	// Act.
	resp, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(req))

	// Assert.
	if err != nil {
		t.Fatalf("SelectFeedRow: %v", err)
	}
	if resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("result = %v, want unknown_workspace", resp.Msg.GetResult())
	}
}

// TestSelectFeedRowMoveIsRecordedAtInfo pins that a move is a visible,
// recorded daemon action: an owner diagnosing a wrong selection must be able
// to find it in the workspace's own log.
func TestSelectFeedRowMoveIsRecordedAtInfo(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.finals = feedIDs("a", "b", "c")
	log := dlog.NewTestLogger()
	h.Surfaces.workspace = log

	// Act.
	if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(boundaryStep(older))); err != nil {
		t.Fatalf("SelectFeedRow: %v", err)
	}

	// Assert.
	var found []dlog.Record
	for _, rec := range log.Records() {
		if rec.Level == "info" && rec.Operation == opSelectFeedRow {
			found = append(found, rec)
		}
	}
	if len(found) != 1 {
		t.Fatalf("info records under %q = %v, want exactly one", opSelectFeedRow, log.Records())
	}
}

// TestSelectFeedRowChangesReachTheWatchInTheOrderMade pins the one-writer
// rule: every change is stored and published as one step, so a run of
// changes reaches the webapp in the order the daemon made them.
func TestSelectFeedRowChangesReachTheWatchInTheOrderMade(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.finals = feedIDs("a", "b", "c")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := openRootWatch(t, h, ctx)

	// Act: newer from none lands on the oldest, a, then walks to b and c.
	for range 3 {
		if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(boundaryStep(newer))); err != nil {
			t.Fatalf("SelectFeedRow: %v", err)
		}
	}
	var got []string
	for range 3 {
		got = append(got, receiveSelection(t, stream).GetResponse().GetRow().GetValue())
	}

	// Assert.
	if strings.Join(got, ",") != "a,b,c" {
		t.Fatalf("pushed rows = %v, want a,b,c", got)
	}
}

// TestSelectFeedRowEndStoresNoneAsAbsence pins that an ended selection is no
// selection at all: a later send or rollback reads nothing selected.
func TestSelectFeedRowEndStoresNoneAsAbsence(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.finals = feedIDs("a")
	if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(boundaryStep(older))); err != nil {
		t.Fatalf("seed a selection: %v", err)
	}

	// Act.
	if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(clearMove())); err != nil {
		t.Fatalf("SelectFeedRow clear: %v", err)
	}

	// Assert: a clear of nothing is now a no-op, which only holds if the
	// first clear left no selection standing.
	resp, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(leftViewMove(&frontendv1.FeedId{Value: "a"})))
	if err != nil || resp.Msg.GetSuccess().GetNone() == nil {
		t.Fatalf("left-view after clear = (%v, %v), want none with nothing to end", resp, err)
	}
}

// bubbleMove is the click move naming ROW.
func bubbleMove(row *frontendv1.FeedId) *agentreplv1.SelectFeedRowRequest {
	return &agentreplv1.SelectFeedRowRequest{
		Workspace: ref(),
		Move:      &agentreplv1.SelectFeedRowRequest_Bubble{Bubble: &agentreplv1.SelectFeedRowBubble{Row: row}},
	}
}

// clickRow sends the click move for ROW and answers the response.
func clickRow(t *testing.T, h *harness, row string) *connect.Response[agentreplv1.SelectFeedRowResponse] {
	t.Helper()
	resp, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(bubbleMove(&frontendv1.FeedId{Value: row})))
	if err != nil {
		t.Fatalf("SelectFeedRow bubble %q: %v", row, err)
	}
	return resp
}

// TestSelectFeedRowBubbleSelectsAsTheRowsKind pins the classification of a
// click: a final response is the response arm, a rollback prompt the prompt
// arm, and any other selectable bubble the bubble arm.
func TestSelectFeedRowBubbleSelectsAsTheRowsKind(t *testing.T) {
	tests := []struct {
		name string
		row  string
		want string
	}{
		{name: "a final response", row: "final-1", want: "response"},
		{name: "a rollback prompt", row: "prompt-1", want: "prompt"},
		{name: "an interim response", row: "interim-1", want: "bubble"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Feed.finals = feedIDs("final-1")
			h.Feed.prompts = feedIDs("prompt-1")
			h.Feed.markdown = map[string]string{"interim-1": "meanwhile"}

			// Act.
			resp := clickRow(t, h, tc.row)

			// Assert.
			sel := resp.Msg.GetSuccess().GetSelected().GetSelection()
			row, kind, held := selectedRow(sel)
			if !held || kind.String() != tc.want || row.GetValue() != tc.row {
				t.Fatalf("selection = %v, want %s of %s", sel, tc.want, tc.row)
			}
		})
	}
}

// TestSelectFeedRowSelectionViewportNamesTheGesture pins what the viewport
// does about a newly selected row, on the push the webapp draws: a click holds
// the feed still under the clicked bubble, whatever arm the row takes, and a
// step from Emacs centers the row.
func TestSelectFeedRowSelectionViewportNamesTheGesture(t *testing.T) {
	tests := []struct {
		name string
		move *agentreplv1.SelectFeedRowRequest
		want string
	}{
		{name: "a click on a final response holds", move: bubbleMove(&frontendv1.FeedId{Value: "final-1"}), want: "hold"},
		{name: "a click on a rollback prompt holds", move: bubbleMove(&frontendv1.FeedId{Value: "prompt-1"}), want: "hold"},
		{name: "a click on an interim response holds", move: bubbleMove(&frontendv1.FeedId{Value: "interim-1"}), want: "hold"},
		{name: "a step landing on a prompt centers", move: boundaryStep(older), want: "center"},
		{name: "a step landing on a final response centers", move: boundaryStep(newer), want: "center"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Feed.finals = feedIDs("final-1")
			h.Feed.prompts = feedIDs("prompt-1")
			h.Feed.markdown = map[string]string{"interim-1": "meanwhile"}
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			stream := openRootWatch(t, h, ctx)

			// Act.
			if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(tc.move)); err != nil {
				t.Fatalf("SelectFeedRow: %v", err)
			}

			// Assert.
			sel := receiveSelection(t, stream)
			if got := selectionViewport(t, sel); got != tc.want {
				t.Fatalf("viewport = %s, want %s (selection %v)", got, tc.want, sel)
			}
		})
	}
}

// selectionViewport names the viewport arm a selected SEL carries, failing the
// test when SEL selects nothing or carries none.
func selectionViewport(t *testing.T, sel *frontendv1.FeedSelection) string {
	t.Helper()
	var viewport *frontendv1.FeedSelectionRowViewport
	switch arm := sel.GetSelection().(type) {
	case *frontendv1.FeedSelection_Response:
		viewport = arm.Response.GetViewport()
	case *frontendv1.FeedSelection_Prompt:
		viewport = arm.Prompt.GetViewport()
	case *frontendv1.FeedSelection_Bubble:
		viewport = arm.Bubble.GetViewport()
	default:
		t.Fatalf("selection = %v, want a selected row", sel)
	}
	switch viewport.GetViewport().(type) {
	case *frontendv1.FeedSelectionRowViewport_Center:
		return "center"
	case *frontendv1.FeedSelectionRowViewport_Hold:
		return "hold"
	}
	t.Fatalf("selection = %v carries no viewport", sel)
	return ""
}

// TestSelectFeedRowBubbleRefusesAnUnselectableRow pins the not_selectable arm:
// the row is echoed and the standing selection is left as it was.
func TestSelectFeedRowBubbleRefusesAnUnselectableRow(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.finals = feedIDs("final-1")
	clickRow(t, h, "final-1")

	// Act.
	resp := clickRow(t, h, "streaming-1")

	// Assert.
	refused := resp.Msg.GetError().GetNotSelectable()
	if refused.GetRow().GetValue() != "streaming-1" {
		t.Fatalf("result = %v, want not_selectable echoing streaming-1", resp.Msg.GetResult())
	}
	if row, _, held := h.Server.(*server).currentSelection(testWorkspaceID); !held || row.GetValue() != "final-1" {
		t.Fatalf("standing selection = %v (held %v), want final-1 left in place", row, held)
	}
}

// TestSelectFeedRowBubbleLogsTheRefusal pins that the refusal enters the
// workspace log with its arm and a reason naming the row.
func TestSelectFeedRowBubbleLogsTheRefusal(t *testing.T) {
	// Arrange.
	h, log := newLoggedHarness(t)

	// Act.
	clickRow(t, h, "streaming-1")

	// Assert.
	for _, rec := range log.records {
		if rec.Context["arm"] == "not_selectable" {
			if reason, _ := rec.Context["reason"].(string); !strings.Contains(reason, "streaming-1") {
				t.Fatalf("reason = %q, want it to name streaming-1", reason)
			}
			return
		}
	}
	t.Fatalf("records = %v, want the not_selectable refusal", log.records)
}

// TestSelectFeedRowNewerFromNothingLandsOnTheOldest pins that a NEWER step
// with nothing selected lands on the oldest loaded turn boundary, whichever
// kind it is.
func TestSelectFeedRowNewerFromNothingLandsOnTheOldest(t *testing.T) {
	tests := []struct {
		name    string
		finals  []*frontendv1.FeedId
		prompts []*frontendv1.FeedId
		want    string
	}{
		{name: "the oldest is a final response", finals: feedIDs("b", "d"), prompts: feedIDs("c"), want: "b"},
		{name: "the oldest is a prompt", finals: feedIDs("b", "d"), prompts: feedIDs("a", "c"), want: "a"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Feed.finals = tc.finals
			h.Feed.prompts = tc.prompts

			// Act.
			resp, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(boundaryStep(newer)))

			// Assert.
			if err != nil {
				t.Fatalf("SelectFeedRow: %v", err)
			}
			row, _, held := selectedRow(resp.Msg.GetSuccess().GetSelected().GetSelection())
			if !held || row.GetValue() != tc.want {
				t.Fatalf("selected = %v, want the oldest %s", resp.Msg.GetSuccess().GetOutcome(), tc.want)
			}
		})
	}
}

// TestSelectFeedRowNewerPastTheNewestEndsWithReturnToTail pins that a NEWER
// step from the newest turn boundary ends the selection, acks none and pushes
// none{return_to_tail}, so the feed scrolls to its bottom, whichever kind the
// newest is.
func TestSelectFeedRowNewerPastTheNewestEndsWithReturnToTail(t *testing.T) {
	tests := []struct {
		name    string
		finals  []*frontendv1.FeedId
		prompts []*frontendv1.FeedId
	}{
		{name: "the newest is a final response", finals: feedIDs("b", "d"), prompts: feedIDs("a", "c")},
		{name: "the newest is a prompt", finals: feedIDs("b"), prompts: feedIDs("a", "c")},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Feed.finals = tc.finals
			h.Feed.prompts = tc.prompts
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			stream := openRootWatch(t, h, ctx)
			if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(boundaryStep(older))); err != nil {
				t.Fatalf("seed the newest: %v", err)
			}
			receiveSelection(t, stream)

			// Act.
			resp, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(boundaryStep(newer)))

			// Assert.
			if err != nil {
				t.Fatalf("SelectFeedRow: %v", err)
			}
			if resp.Msg.GetSuccess().GetNone() == nil {
				t.Fatalf("outcome = %v, want none", resp.Msg.GetSuccess().GetOutcome())
			}
			if sel := receiveSelection(t, stream); sel.GetNone().GetReturnToTail() == nil {
				t.Fatalf("selection = %v, want none{return_to_tail}", sel)
			}
			if _, _, held := h.Server.(*server).currentSelection(testWorkspaceID); held {
				t.Fatalf("a selection is still stored after the step ran past the newest")
			}
		})
	}
}

// TestSelectFeedRowNewerAfterRunningPastTheNewestRollsRoundToTheOldest pins
// the round trip: newest, NEWER (ended, at the tail), NEWER again lands on
// the oldest prompt.
func TestSelectFeedRowNewerAfterRunningPastTheNewestRollsRoundToTheOldest(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.prompts = feedIDs("p1", "p2", "p3")
	for _, dir := range []agentreplv1.SelectFeedRowDirection{older, newer} {
		if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(boundaryStep(dir))); err != nil {
			t.Fatalf("SelectFeedRow %v: %v", dir, err)
		}
	}

	// Act.
	resp, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(boundaryStep(newer)))

	// Assert.
	if err != nil {
		t.Fatalf("SelectFeedRow: %v", err)
	}
	if got := resp.Msg.GetSuccess().GetSelected().GetSelection().GetPrompt().GetRow().GetValue(); got != "p1" {
		t.Fatalf("selected prompt = %q, want the oldest p1", got)
	}
}

// TestSelectFeedRowNewerPastTheNewestIsRecordedAtInfo pins that the end is a
// visible action in the workspace's log, naming the kind and the row it left.
func TestSelectFeedRowNewerPastTheNewestIsRecordedAtInfo(t *testing.T) {
	// Arrange.
	h, log := newLoggedHarness(t)
	h.Feed.prompts = feedIDs("p1")
	if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(boundaryStep(older))); err != nil {
		t.Fatalf("seed: %v", err)
	}

	// Act.
	if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(boundaryStep(newer))); err != nil {
		t.Fatalf("SelectFeedRow: %v", err)
	}

	// Assert.
	for _, rec := range log.at("INFO") {
		if strings.Contains(rec.Message, "ran past the newest") {
			if rec.Context["kind"] != "prompt" || rec.Context["row"] != "p1" {
				t.Fatalf("context = %v, want kind prompt and row p1", rec.Context)
			}
			return
		}
	}
	t.Fatalf("records = %v, want the end at INFO", log.records)
}

// TestSelectFeedRowBubbleOnTheSelectedRowDeselectsWithHold pins the click
// toggle: a click naming the selected row ends the selection, acks none and
// pushes none{hold}, so the feed holds still under the clicked bubble,
// whichever arm the row was selected as.
func TestSelectFeedRowBubbleOnTheSelectedRowDeselectsWithHold(t *testing.T) {
	tests := []struct {
		name string
		seed *agentreplv1.SelectFeedRowRequest
		row  string
	}{
		{name: "a response selected by a step", seed: boundaryStep(newer), row: "final-1"},
		{name: "a prompt selected by a step", seed: boundaryStep(older), row: "prompt-1"},
		{name: "a bubble selected by a click", seed: bubbleMove(&frontendv1.FeedId{Value: "interim-1"}), row: "interim-1"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Feed.finals = feedIDs("final-1")
			h.Feed.prompts = feedIDs("prompt-1")
			h.Feed.markdown = map[string]string{"interim-1": "meanwhile"}
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			stream := openRootWatch(t, h, ctx)
			if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(tc.seed)); err != nil {
				t.Fatalf("seed: %v", err)
			}
			receiveSelection(t, stream)

			// Act.
			resp := clickRow(t, h, tc.row)

			// Assert.
			if resp.Msg.GetSuccess().GetNone() == nil {
				t.Fatalf("outcome = %v, want none", resp.Msg.GetSuccess().GetOutcome())
			}
			if sel := receiveSelection(t, stream); sel.GetNone().GetHold() == nil {
				t.Fatalf("selection = %v, want none{hold}", sel)
			}
			if _, _, held := h.Server.(*server).currentSelection(testWorkspaceID); held {
				t.Fatalf("a selection is still stored after the click on the selected row")
			}
		})
	}
}

// TestSelectFeedRowBubbleOnAnotherRowMovesTheSelection pins that the toggle is
// only for the selected row: a click on another selectable row selects it.
func TestSelectFeedRowBubbleOnAnotherRowMovesTheSelection(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.finals = feedIDs("final-1", "final-2")
	clickRow(t, h, "final-1")

	// Act.
	resp := clickRow(t, h, "final-2")

	// Assert.
	if got := resp.Msg.GetSuccess().GetSelected().GetSelection().GetResponse().GetRow().GetValue(); got != "final-2" {
		t.Fatalf("selected = %q, want final-2", got)
	}
}

// TestSelectFeedRowBubbleDeselectIsRecordedAtInfo pins that the click toggle
// enters the workspace log under the shared end record, naming why.
func TestSelectFeedRowBubbleDeselectIsRecordedAtInfo(t *testing.T) {
	// Arrange.
	h, log := newLoggedHarness(t)
	h.Feed.finals = feedIDs("final-1")
	clickRow(t, h, "final-1")

	// Act.
	clickRow(t, h, "final-1")

	// Assert.
	for _, rec := range log.at("INFO") {
		if rec.Context["because"] == "clicked_selected" {
			if rec.Context["row"] != "final-1" {
				t.Fatalf("context = %v, want row final-1", rec.Context)
			}
			return
		}
	}
	t.Fatalf("records = %v, want the deselect at INFO", log.records)
}

// TestSelectFeedRowStepFailsOnAnUnreadableFinal pins the invariant: a final
// response the resolver cannot read is a defect, answered Internal and logged
// at ERROR, and nothing is selected or published.
func TestSelectFeedRowStepFailsOnAnUnreadableFinal(t *testing.T) {
	// Arrange.
	h, log := newLoggedHarness(t)
	h.Feed.finals = feedIDs("final-1")
	h.Feed.unreadable = map[string]bool{"final-1": true}

	// Act.
	_, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(boundaryStep(older)))

	// Assert.
	if connectCode(t, err) != connect.CodeInternal {
		t.Fatalf("code = %v, want Internal", connectCode(t, err))
	}
	if len(log.at("ERROR")) == 0 {
		t.Fatalf("records = %v, want the failure at ERROR", log.records)
	}
	if _, _, held := h.Server.(*server).currentSelection(testWorkspaceID); held {
		t.Fatalf("a selection was stored for an unreadable final")
	}
}

// receiveHostSelection reads the host stream until a selection push.
func receiveHostSelection(
	t *testing.T,
	stream *connect.ServerStreamForClient[agentreplv1.WatchHostWorkspaceResponse],
) *agentreplv1.HostWorkspaceSelection {
	t.Helper()
	for stream.Receive() {
		if sel := stream.Msg().GetSelection(); sel != nil {
			return sel
		}
	}
	t.Fatalf("the host stream ended before a selection arrived: %v", stream.Err())
	return nil
}

// TestHostSelectionCarriesTheSelectedText pins what Emacs is handed for each
// selected arm: its kind and the bubble's text, which the composer searches.
func TestHostSelectionCarriesTheSelectedText(t *testing.T) {
	tests := []struct {
		name string
		row  string
		text func(*agentreplv1.HostWorkspaceSelection) string
	}{
		{name: "a final response", row: "final-1", text: func(s *agentreplv1.HostWorkspaceSelection) string {
			return s.GetResponse().GetMarkdown().GetText()
		}},
		{name: "a rollback prompt", row: "prompt-1", text: func(s *agentreplv1.HostWorkspaceSelection) string {
			return s.GetPrompt().GetMarkdown().GetText()
		}},
		{name: "another bubble", row: "interim-1", text: func(s *agentreplv1.HostWorkspaceSelection) string {
			return s.GetBubble().GetMarkdown().GetText()
		}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Feed.finals = feedIDs("final-1")
			h.Feed.prompts = feedIDs("prompt-1")
			h.Feed.markdown = map[string]string{"final-1": "the answer", "prompt-1": "the question", "interim-1": "meanwhile"}
			want := h.Feed.markdown[tc.row]
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			stream := dialHostStream(t, h, ctx)

			// Act.
			clickRow(t, h, tc.row)
			sel := receiveHostSelection(t, stream)

			// Assert.
			if got := tc.text(sel); got != want {
				t.Fatalf("selection = %v, want text %q", sel, want)
			}
		})
	}
}

// TestReturnFeedToTailEndsAHeldSelectionWithReturnToTail pins that a
// workspace switch (the verbs' HostRelay.ReturnFeedToTail) ends a standing
// selection and pushes return_to_tail, which re-arms the webapp's follow.
func TestReturnFeedToTailEndsAHeldSelectionWithReturnToTail(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.finals = feedIDs("a", "b")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := openRootWatch(t, h, ctx)
	if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(boundaryStep(older))); err != nil {
		t.Fatalf("seed a selection: %v", err)
	}
	receiveSelection(t, stream)

	// Act.
	h.Server.Relay().ReturnFeedToTail(testWorkspaceID)

	// Assert.
	sel := receiveSelection(t, stream)
	if sel.GetNone().GetReturnToTail() == nil {
		t.Fatalf("selection = %v, want none{return_to_tail}", sel)
	}
}

// TestReturnFeedToTailLeavesNothingSelected pins that the ended selection is
// absence, so the next send or rollback reads nothing selected.
func TestReturnFeedToTailLeavesNothingSelected(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.finals = feedIDs("a")
	if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(boundaryStep(older))); err != nil {
		t.Fatalf("seed a selection: %v", err)
	}

	// Act.
	h.Server.Relay().ReturnFeedToTail(testWorkspaceID)

	// Assert.
	if _, _, held := h.Server.(*server).currentSelection(testWorkspaceID); held {
		t.Fatal("a selection still stands after the switch returned the feed to its tail")
	}
}

// TestReturnFeedToTailWithNothingSelectedPublishesNothing pins that a switch
// to a workspace with no selection changes no selection state: nothing is
// published, so no watch receives a restated `none`.
func TestReturnFeedToTailWithNothingSelectedPublishesNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	s := h.Server.(*server)

	// Act.
	h.Server.Relay().ReturnFeedToTail(testWorkspaceID)

	// Assert.
	if latest, ok := s.selectionTopic(testWorkspaceID).Latest(); ok {
		t.Fatalf("selection topic = %v, want nothing published", latest)
	}
}

// TestReturnFeedToTailIsRecordedAtInfo pins that ending a selection on a
// switch is a visible daemon action in the workspace's own log, naming why.
func TestReturnFeedToTailIsRecordedAtInfo(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.finals = feedIDs("a")
	if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(boundaryStep(older))); err != nil {
		t.Fatalf("seed a selection: %v", err)
	}
	log := dlog.NewTestLogger()
	h.Surfaces.workspace = log

	// Act.
	h.Server.Relay().ReturnFeedToTail(testWorkspaceID)

	// Assert.
	for _, rec := range log.Records() {
		if rec.Level == "info" && rec.Operation == opSelectFeedRow && rec.Context["because"] == "workspace_selected" {
			return
		}
	}
	t.Fatalf("records = %v, want an info end with because=workspace_selected", log.Records())
}

// TestReturnFeedToTailForAnUnknownWorkspaceIsAnError pins that a switch the
// registry cannot resolve is surfaced at ERROR and ends nothing.
func TestReturnFeedToTailForAnUnknownWorkspaceIsAnError(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })

	// Act.
	h.Server.Relay().ReturnFeedToTail("ws-nope")

	// Assert.
	for _, rec := range log.Records() {
		if rec.Level == "error" && rec.Operation == "daemon.server.return_feed_to_tail" {
			return
		}
	}
	t.Fatalf("records = %v, want an error under daemon.server.return_feed_to_tail", log.Records())
}

// TestSelectFeedRowStepOlderPastTheOldestLoadsTheOlderPage pins ruling 7 of
// feed paging on demand: a step older from the oldest loaded final response
// loads the next older page and lands on its newest final, rather than
// wrapping to the newest.
func TestSelectFeedRowStepOlderPastTheOldestLoadsTheOlderPage(t *testing.T) {
	// Arrange: "b" is selected and is the oldest loaded final.
	h := newHarness(t)
	h.Feed.finals = feedIDs("b", "c")
	h.Feed.olderFinals = [][]*frontendv1.FeedId{feedIDs("a")}
	h.selectResponse(t, "b")

	// Act.
	resp, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(boundaryStep(older)))

	// Assert.
	if err != nil {
		t.Fatalf("SelectFeedRow: %v", err)
	}
	if got := resp.Msg.GetSuccess().GetSelected().GetSelection().GetResponse().GetRow().GetValue(); got != "a" {
		t.Fatalf("selected response = %q, want a from the older page", got)
	}
}

// TestSelectFeedRowStepOlderWithNothingOlderWraps pins the conversation's
// start: nothing older to load leaves the wrap as it always was.
func TestSelectFeedRowStepOlderWithNothingOlderWraps(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.finals = feedIDs("b", "c")
	h.selectResponse(t, "b")

	// Act.
	resp, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(boundaryStep(older)))

	// Assert.
	if err != nil {
		t.Fatalf("SelectFeedRow: %v", err)
	}
	if got := resp.Msg.GetSuccess().GetSelected().GetSelection().GetResponse().GetRow().GetValue(); got != "c" {
		t.Fatalf("selected response = %q, want the wrap to c", got)
	}
}

// TestSelectFeedRowStepNewerLoadsNothing pins that only a step older past the
// oldest loaded row reads history.
func TestSelectFeedRowStepNewerLoadsNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.finals = feedIDs("b", "c")
	h.Feed.olderFinals = [][]*frontendv1.FeedId{feedIDs("a")}
	h.selectResponse(t, "b")

	// Act.
	if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(boundaryStep(newer))); err != nil {
		t.Fatalf("SelectFeedRow: %v", err)
	}

	// Assert.
	if h.Feed.loadOlderCalls != 0 {
		t.Fatalf("LoadOlder calls = %d, want none", h.Feed.loadOlderCalls)
	}
}

// TestSelectFeedRowStepOlderWhoseLoadFailsIsAnError pins that a page the step
// needed and could not read fails the rpc loudly rather than wrapping.
func TestSelectFeedRowStepOlderWhoseLoadFailsIsAnError(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.finals = feedIDs("b")
	h.Feed.loadOlderErr = errors.New("store unreachable")
	h.selectResponse(t, "b")

	// Act.
	_, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(boundaryStep(older)))

	// Assert.
	if err == nil {
		t.Fatal("SelectFeedRow = nil error, want the failed load surfaced")
	}
}

// TestSelectFeedRowStepOlderPastTheOldestPromptLoadsTheOlderPage pins the
// same for the prompts a rollback can reach.
func TestSelectFeedRowStepOlderPastTheOldestPromptLoadsTheOlderPage(t *testing.T) {
	// Arrange: nothing of the kind is loaded yet.
	h := newHarness(t)
	h.Feed.olderPrompts = [][]*frontendv1.FeedId{feedIDs("p1")}

	// Act.
	resp, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(boundaryStep(older)))

	// Assert.
	if err != nil {
		t.Fatalf("SelectFeedRow: %v", err)
	}
	if got := resp.Msg.GetSuccess().GetSelected().GetSelection().GetPrompt().GetRow().GetValue(); got != "p1" {
		t.Fatalf("selected prompt = %q, want p1 from the older page", got)
	}
}

// selectResponse selects one final response by clicking it.
func (h *harness) selectResponse(t *testing.T, row string) {
	t.Helper()
	if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(&agentreplv1.SelectFeedRowRequest{
		Workspace: ref(),
		Move: &agentreplv1.SelectFeedRowRequest_Bubble{Bubble: &agentreplv1.SelectFeedRowBubble{
			Row: &frontendv1.FeedId{Value: row},
		}},
	})); err != nil {
		t.Fatalf("select %q: %v", row, err)
	}
}

// newLoggedHarness is a harness whose workspace and global logs both record
// into the answered logger, for tests asserting what a move logged.
func newLoggedHarness(t *testing.T) (*harness, *recordingLogger) {
	t.Helper()
	log := &recordingLogger{}
	h := newHarness(t, func(deps *Deps) {
		deps.Log = &fakeSurfaces{workspace: log, global: log}
	})
	return h, log
}

// TestNewLoggedHarnessRecordsTheWorkspaceLog pins the helper's guarantee: a
// move's workspace log record reaches the answered logger.
func TestNewLoggedHarnessRecordsTheWorkspaceLog(t *testing.T) {
	// Arrange.
	h, log := newLoggedHarness(t)
	h.Feed.finals = feedIDs("a")

	// Act.
	if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(boundaryStep(older))); err != nil {
		t.Fatalf("SelectFeedRow: %v", err)
	}

	// Assert.
	for _, rec := range log.at("INFO") {
		if rec.Operation == opSelectFeedRow {
			return
		}
	}
	t.Fatalf("records = %v, want the move under %q", log.records, opSelectFeedRow)
}

// TestSelectFeedRowTestsBuildTheirLoggedHarnessThroughTheHelper pins that the
// recording-logger wiring lives only in newLoggedHarness, so no test in this
// file hand-rolls a variant that drifts from it.
func TestSelectFeedRowTestsBuildTheirLoggedHarnessThroughTheHelper(t *testing.T) {
	// Arrange.
	src, err := os.ReadFile("select_feed_row_test.go")
	if err != nil {
		t.Fatalf("read the test source: %v", err)
	}

	// Act.
	// The needle is split so this line is not itself counted.
	got := strings.Count(string(src), "deps.Log = &fakeSurfaces"+"{workspace: log, global: log}")

	// Assert.
	if got != 1 {
		t.Fatalf("hand-rolled logged harnesses = %d, want only newLoggedHarness's own", got)
	}
}

// receiveHostEventPastSelection reads the host stream until a push that is
// neither the host state nor the selection: the next EVENT.
func receiveHostEventPastSelection(
	t *testing.T,
	stream *connect.ServerStreamForClient[agentreplv1.WatchHostWorkspaceResponse],
) *agentreplv1.WatchHostWorkspaceResponse {
	t.Helper()
	for stream.Receive() {
		if stream.Msg().GetHost() != nil || stream.Msg().GetSelection() != nil {
			continue
		}
		return stream.Msg()
	}
	t.Fatalf("the host stream ended before an event arrived: %v", stream.Err())
	return nil
}

// TestSelectFeedRowClickOnAFinalResponseAsksTheHostToFocusTheComposer pins
// which moves push final_response_clicked: only a click that selects a final
// response. Every other move is followed by a notification click, which must
// then be the first event the host stream carries, so a stray
// final_response_clicked shows up as the wrong first event.
func TestSelectFeedRowClickOnAFinalResponseAsksTheHostToFocusTheComposer(t *testing.T) {
	tests := []struct {
		name      string
		seed      *agentreplv1.SelectFeedRowRequest
		move      *agentreplv1.SelectFeedRowRequest
		wantFocus bool
	}{
		{name: "a click selecting a final response", move: bubbleMove(&frontendv1.FeedId{Value: "final-1"}), wantFocus: true},
		{name: "a click moving to another final response", seed: bubbleMove(&frontendv1.FeedId{Value: "final-1"}), move: bubbleMove(&frontendv1.FeedId{Value: "final-2"}), wantFocus: true},
		{name: "a click deselecting the selected final response", seed: bubbleMove(&frontendv1.FeedId{Value: "final-1"}), move: bubbleMove(&frontendv1.FeedId{Value: "final-1"})},
		{name: "a click on a rollback prompt", move: bubbleMove(&frontendv1.FeedId{Value: "prompt-1"})},
		{name: "a click on an interim response", move: bubbleMove(&frontendv1.FeedId{Value: "interim-1"})},
		{name: "a step landing on a final response", move: boundaryStep(older)},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Feed.finals = feedIDs("final-1", "final-2")
			h.Feed.prompts = feedIDs("prompt-1")
			h.Feed.markdown = map[string]string{"interim-1": "meanwhile"}
			if tc.seed != nil {
				if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(tc.seed)); err != nil {
					t.Fatalf("seed: %v", err)
				}
			}
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			stream := dialHostStream(t, h, ctx)

			// Act.
			if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(tc.move)); err != nil {
				t.Fatalf("SelectFeedRow: %v", err)
			}
			h.Server.NotificationClicked(testWorkspaceID)
			first := receiveHostEventPastSelection(t, stream)

			// Assert.
			if got := first.GetFinalResponseClicked() != nil; got != tc.wantFocus {
				t.Fatalf("first event = %v, want final_response_clicked %v", first, tc.wantFocus)
			}
		})
	}
}

// TestSelectFeedRowFinalResponseClickIsNeverReplayed pins that the event is
// a moment, not state: a host stream opened after the click is handed the
// selection but not the click, so a reconnect never refocuses the composer.
func TestSelectFeedRowFinalResponseClickIsNeverReplayed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.finals = feedIDs("final-1")
	clickRow(t, h, "final-1")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := dialHostStream(t, h, ctx)

	// Act.
	h.Server.NotificationClicked(testWorkspaceID)
	first := receiveHostEventPastSelection(t, stream)

	// Assert.
	if first.GetNotificationClicked() == nil {
		t.Fatalf("first event = %v, want notification_clicked (final_response_clicked is never replayed)", first)
	}
}

// TestSelectFeedRowFinalResponseClickIsRecordedAtInfo pins that the request
// to focus the composer enters the workspace log, naming the clicked row.
func TestSelectFeedRowFinalResponseClickIsRecordedAtInfo(t *testing.T) {
	// Arrange.
	h, log := newLoggedHarness(t)
	h.Feed.finals = feedIDs("final-1")

	// Act.
	clickRow(t, h, "final-1")

	// Assert.
	for _, rec := range log.at("INFO") {
		if strings.Contains(rec.Message, "focus the composer") {
			if rec.Context["row"] != "final-1" {
				t.Fatalf("context = %v, want row final-1", rec.Context)
			}
			return
		}
	}
	t.Fatalf("records = %v, want the focus request at INFO", log.records)
}
