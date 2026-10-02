package server

import (
	"context"
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/internal/dlog"
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

// responseStep builds a SelectFeedRow request stepping the final responses.
func responseStep(direction agentreplv1.SelectFeedRowDirection) *agentreplv1.SelectFeedRowRequest {
	return &agentreplv1.SelectFeedRowRequest{
		Workspace: ref(),
		Move: &agentreplv1.SelectFeedRowRequest_Response{
			Response: &agentreplv1.SelectFeedRowStep{Direction: direction},
		},
	}
}

// promptStep builds a SelectFeedRow request stepping the rollback prompts.
func promptStep(direction agentreplv1.SelectFeedRowDirection) *agentreplv1.SelectFeedRowRequest {
	return &agentreplv1.SelectFeedRowRequest{
		Workspace: ref(),
		Move: &agentreplv1.SelectFeedRowRequest_Prompt{
			Prompt: &agentreplv1.SelectFeedRowStep{Direction: direction},
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

// TestStepIndex is the pure index arithmetic every step rides on: the newest
// row from nothing selected regardless of direction, wrapping at each end,
// and a single row wrapping to itself.
func TestStepIndex(t *testing.T) {
	tests := []struct {
		name      string
		at        int
		n         int
		direction agentreplv1.SelectFeedRowDirection
		want      int
	}{
		{name: "from nothing, OLDER starts at the newest", at: -1, n: 3, direction: older, want: 2},
		{name: "from nothing, NEWER starts at the newest", at: -1, n: 3, direction: newer, want: 2},
		{name: "OLDER walks older", at: 2, n: 3, direction: older, want: 1},
		{name: "NEWER walks newer", at: 0, n: 3, direction: newer, want: 1},
		{name: "OLDER wraps past the oldest to the newest", at: 0, n: 3, direction: older, want: 2},
		{name: "NEWER wraps past the newest to the oldest", at: 2, n: 3, direction: newer, want: 0},
		{name: "single row OLDER wraps to itself", at: 0, n: 1, direction: older, want: 0},
		{name: "single row NEWER wraps to itself", at: 0, n: 1, direction: newer, want: 0},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := stepIndex(tc.at, tc.n, tc.direction)

			// Assert.
			if got != tc.want {
				t.Fatalf("stepIndex(%d, %d, %v) = %d, want %d", tc.at, tc.n, tc.direction, got, tc.want)
			}
		})
	}
}

// TestSelectFeedRowResponseStepAcksTheNewest pins that a response step with
// nothing selected lands on, and acks, the most recent final response.
func TestSelectFeedRowResponseStepAcksTheNewest(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.finals = feedIDs("a", "b", "c")

	// Act.
	resp, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(responseStep(newer)))

	// Assert.
	if err != nil {
		t.Fatalf("SelectFeedRow: %v", err)
	}
	if got := resp.Msg.GetSuccess().GetSelected().GetSelection().GetResponse().GetRow().GetValue(); got != "c" {
		t.Fatalf("selected response = %q, want the most recent c", got)
	}
}

// TestSelectFeedRowPromptStepWalksRollbackPrompts pins that the prompt step
// reads its own ordered set from RollbackPrompts, distinct from FinalResponses.
func TestSelectFeedRowPromptStepWalksRollbackPrompts(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.finals = feedIDs("resp-only")
	h.Feed.prompts = feedIDs("p1", "p2")

	// Act.
	resp, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(promptStep(newer)))

	// Assert.
	if err != nil {
		t.Fatalf("SelectFeedRow: %v", err)
	}
	if got := resp.Msg.GetSuccess().GetSelected().GetSelection().GetPrompt().GetRow().GetValue(); got != "p2" {
		t.Fatalf("selected prompt = %q, want the most recent p2", got)
	}
}

// TestSelectFeedRowPromptStepReplacesASelectedResponse pins that stepping
// prompts while a response is selected drops the response and lands on the
// newest prompt, rather than continuing from the response's position.
func TestSelectFeedRowPromptStepReplacesASelectedResponse(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.finals = feedIDs("a", "b", "c")
	h.Feed.prompts = feedIDs("p1", "p2")
	if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(responseStep(newer))); err != nil {
		t.Fatalf("seed a response selection: %v", err)
	}

	// Act.
	resp, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(promptStep(newer)))

	// Assert.
	sel := resp.Msg.GetSuccess().GetSelected().GetSelection()
	if err != nil {
		t.Fatalf("SelectFeedRow: %v", err)
	}
	if sel.GetResponse() != nil {
		t.Fatalf("selection = %v, want the response selection replaced", sel)
	}
	if got := sel.GetPrompt().GetRow().GetValue(); got != "p2" {
		t.Fatalf("selected prompt = %q, want the most recent p2", got)
	}
}

// TestSelectFeedRowEmptySetEndsAHeldSelectionWithReturnToTail pins that a
// step with nothing of its kind to select answers nothing_selectable AND, when
// a selection was held, ends it with a return_to_tail push.
func TestSelectFeedRowEmptySetEndsAHeldSelectionWithReturnToTail(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.finals = feedIDs("a", "b", "c")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := openRootWatch(t, h, ctx)
	if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(responseStep(newer))); err != nil {
		t.Fatalf("seed a selection: %v", err)
	}
	if got := receiveSelection(t, stream).GetResponse().GetRow().GetValue(); got != "c" {
		t.Fatalf("seed selection = %q, want c", got)
	}
	h.Feed.finals = nil

	// Act.
	resp, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(responseStep(newer)))

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
	if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(responseStep(newer))); err != nil {
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
	if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(responseStep(newer))); err != nil {
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
// -1 as "start at the newest" for either direction).
func TestSelectFeedRowLeftViewOfANonSelectedRowKeepsTheSelection(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.finals = feedIDs("a", "b", "c")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := openRootWatch(t, h, ctx)
	if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(responseStep(newer))); err != nil {
		t.Fatalf("seed a selection: %v", err)
	}
	receiveSelection(t, stream)

	// Act.
	if _, err := h.Client.SelectFeedRow(context.Background(),
		connect.NewRequest(leftViewMove(&frontendv1.FeedId{Value: "a"}))); err != nil {
		t.Fatalf("report a non-selected row leaving the viewport: %v", err)
	}
	resp, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(responseStep(older)))

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
		{name: "unspecified direction on a response step", req: responseStep(agentreplv1.SelectFeedRowDirection_SELECT_FEED_ROW_DIRECTION_UNSPECIFIED)},
		{name: "unspecified direction on a prompt step", req: promptStep(agentreplv1.SelectFeedRowDirection_SELECT_FEED_ROW_DIRECTION_UNSPECIFIED)},
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
	req := responseStep(newer)
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
	if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(responseStep(newer))); err != nil {
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

	// Act: newer from none lands on c, then wraps to a, then b.
	for range 3 {
		if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(responseStep(newer))); err != nil {
			t.Fatalf("SelectFeedRow: %v", err)
		}
	}
	var got []string
	for range 3 {
		got = append(got, receiveSelection(t, stream).GetResponse().GetRow().GetValue())
	}

	// Assert.
	if strings.Join(got, ",") != "c,a,b" {
		t.Fatalf("pushed rows = %v, want c,a,b", got)
	}
}

// TestSelectFeedRowEndStoresNoneAsAbsence pins that an ended selection is no
// selection at all: a later send or rollback reads nothing selected.
func TestSelectFeedRowEndStoresNoneAsAbsence(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.finals = feedIDs("a")
	if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(responseStep(newer))); err != nil {
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
	log := &recordingLogger{}
	h := newHarness(t, func(deps *Deps) {
		deps.Log = &fakeSurfaces{workspace: log, global: log}
	})

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

// TestSelectFeedRowStepFailsOnAnUnreadableFinal pins the invariant: a final
// response the resolver cannot read is a defect, answered Internal and logged
// at ERROR, and nothing is selected or published.
func TestSelectFeedRowStepFailsOnAnUnreadableFinal(t *testing.T) {
	// Arrange.
	log := &recordingLogger{}
	h := newHarness(t, func(deps *Deps) {
		deps.Log = &fakeSurfaces{workspace: log, global: log}
	})
	h.Feed.finals = feedIDs("final-1")
	h.Feed.unreadable = map[string]bool{"final-1": true}

	// Act.
	_, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(responseStep(newer)))

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
			stream, err := h.Client.WatchHostWorkspace(ctx, connect.NewRequest(&agentreplv1.WatchHostWorkspaceRequest{Workspace: ref()}))
			if err != nil {
				t.Fatalf("open the host stream: %v", err)
			}

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
	if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(responseStep(newer))); err != nil {
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
	if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(responseStep(newer))); err != nil {
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
	if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(responseStep(newer))); err != nil {
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
