package server

import (
	"context"
	"errors"
	"slices"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
)

// THE COMPOSER'S ENTRIES: what a report targets is pushed on the root feed's
// watch (the highlighted node) and the host view (the labels answering
// nothing), and restated when the selection or a response's tree moves.

// composerReportRequest is a ReportComposerEntries of LABELS with point on AT ("" for
// no entry).
func composerReportRequest(at string, labels ...string) *agentreplv1.ReportComposerEntriesRequest {
	point := &agentreplv1.ComposerEntryAtPoint{At: &agentreplv1.ComposerEntryAtPoint_None{None: &agentreplv1.ComposerEntryAtPointNone{}}}
	if at != "" {
		point = &agentreplv1.ComposerEntryAtPoint{At: &agentreplv1.ComposerEntryAtPoint_Entry{Entry: &agentreplv1.ComposerEntryAtPointEntry{Label: at}}}
	}
	return &agentreplv1.ReportComposerEntriesRequest{Workspace: ref(), Labels: labels, AtPoint: point}
}

// report sends REQ, failing the test unless it is answered success.
func report(t *testing.T, h *harness, req *agentreplv1.ReportComposerEntriesRequest) {
	t.Helper()
	resp, err := h.Client.ReportComposerEntries(context.Background(), connect.NewRequest(req))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("ReportComposerEntries = (%v, %v), want success", resp, err)
	}
}

// receiveComposerEntry reads the watch stream until a composer-entry frame arrives.
func receiveComposerEntry(t *testing.T, stream *connect.ServerStreamForClient[agentreplv1.WatchFeedResponse]) *frontendv1.FeedComposerEntry {
	t.Helper()
	return receivePush(t, stream, "a composer entry", (*agentreplv1.WatchFeedResponse).GetComposerEntry)
}

// awaitUnanswered waits for the host view to carry WANT as the labels
// answering nothing.
func awaitUnanswered(t *testing.T, h *harness, ctx context.Context, want []string) {
	t.Helper()
	views := h.Server.(*server).hostStateTopic(testWorkspaceID).Subscribe(ctx)
	for {
		select {
		case <-ctx.Done():
			t.Fatalf("the host view never carried unanswered %v", want)
		case view := <-views:
			if slices.Equal(view.GetComposerEntries().GetUnanswered(), want) {
				return
			}
		}
	}
}

func TestAReportHighlightsTheNodePointsEntryAnswers(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Feed.targets = map[string]string{"3.1": "r1", "3.2": "r1"}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := openRootWatch(t, h, ctx)

	// Act
	report(t, h, composerReportRequest("3.2", "3.1", "3.2"))

	// Assert
	node := receiveComposerEntry(t, stream).GetNode()
	if node.GetRow().GetValue() != "r1" || node.GetLabel() != "3.2" {
		t.Fatalf("composer entry = %v, want r1's node 3.2", node)
	}
}

func TestAReportWithPointOnNoEntryHighlightsNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Feed.targets = map[string]string{"3.1": "r1"}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := openRootWatch(t, h, ctx)

	// Act
	report(t, h, composerReportRequest("", "3.1"))

	// Assert
	if entry := receiveComposerEntry(t, stream); entry.GetNone() == nil {
		t.Fatalf("composer entry = %v, want none", entry)
	}
}

func TestAPointEntryAnsweringNothingHighlightsNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := openRootWatch(t, h, ctx)

	// Act
	report(t, h, composerReportRequest("9.9", "9.9"))

	// Assert
	if entry := receiveComposerEntry(t, stream); entry.GetNone() == nil {
		t.Fatalf("composer entry = %v, want none for a label nothing holds", entry)
	}
}

func TestTheHostViewListsTheLabelsAnsweringNothingOnceInOrder(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Feed.targets = map[string]string{"3.1": "r1"}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act
	report(t, h, composerReportRequest("", "9.9", "3.1", "8.8", "9.9"))

	// Assert
	awaitUnanswered(t, h, ctx, []string{"9.9", "8.8"})
}

func TestTheHostViewAlwaysCarriesTheComposersEntries(t *testing.T) {
	// Arrange
	h := newHarness(t)
	surface := h.Server.(*server)

	// Act
	surface.PublishHostWorkspace(context.Background(), testWorkspaceID)

	// Assert
	view, _ := surface.hostStateTopic(testWorkspaceID).Latest()
	if view.GetComposerEntries() == nil || len(view.GetComposerEntries().GetUnanswered()) != 0 {
		t.Fatalf("composer entries = %v, want set and empty", view.GetComposerEntries())
	}
}

func TestASelectedResponseIsWhatTheEntriesReplyTo(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Feed.markdown = map[string]string{"r1": "1 🔧 a"}
	h.Feed.targets = map[string]string{"1": "r1"}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := openRootWatch(t, h, ctx)
	h.selectResponse(t, "r1")

	// Act
	report(t, h, composerReportRequest("1", "1"))
	receiveComposerEntry(t, stream)

	// Assert
	h.Feed.sentMu.Lock()
	defer h.Feed.sentMu.Unlock()
	if !slices.Contains(h.Feed.targetReplies, "r1") {
		t.Fatalf("ComposerTargets was asked for replies %v, want r1, the selected response", h.Feed.targetReplies)
	}
}

func TestWhileAPromptIsSelectedTheComposerAnswersNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Feed.promptRows = map[string]bool{"p1": true}
	h.Feed.targets = map[string]string{"3.1": "r1"}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := openRootWatch(t, h, ctx)
	h.selectResponse(t, "p1")

	// Act
	report(t, h, composerReportRequest("3.1", "3.1", "9.9"))

	// Assert: nothing highlighted, and no label is marked as answering nothing.
	for {
		entry := receiveComposerEntry(t, stream)
		if entry.GetNone() != nil {
			break
		}
	}
	if got := h.Server.(*server).composerUnansweredOf(testWorkspaceID); len(got) != 0 {
		t.Fatalf("unanswered = %v, want none while a prompt is selected", got)
	}
}

func TestALabelChangeRestatesTheHighlight(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := openRootWatch(t, h, ctx)
	report(t, h, composerReportRequest("3.1", "3.1"))
	receiveComposerEntry(t, stream)

	// Act: a response holding 3.1 is drawn.
	h.Feed.targets = map[string]string{"3.1": "r2"}
	h.Feed.labelMoves.Emit(testWorkspaceID)

	// Assert
	if node := receiveComposerEntry(t, stream).GetNode(); node.GetRow().GetValue() != "r2" {
		t.Fatalf("composer entry = %v, want r2's node after the response was drawn", node)
	}
}

func TestALateRootWatchIsToldTheStandingHighlight(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Feed.targets = map[string]string{"3.1": "r1"}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	first := openRootWatch(t, h, ctx)
	report(t, h, composerReportRequest("3.1", "3.1"))
	receiveComposerEntry(t, first)

	// Act
	node := receiveComposerEntry(t, openRootWatch(t, h, ctx)).GetNode()

	// Assert
	if node.GetRow().GetValue() != "r1" {
		t.Fatalf("a late watch's composer entry = %v, want r1's node", node)
	}
}

func TestReportComposerEntriesRefusesAMalformedReport(t *testing.T) {
	tests := []struct {
		name string
		req  *agentreplv1.ReportComposerEntriesRequest
	}{
		{"no entry at point stated", &agentreplv1.ReportComposerEntriesRequest{Workspace: ref(), Labels: []string{"3.1"}}},
		{"a point entry the labels do not hold", composerReportRequest("4.1", "3.1")},
		{"an empty label", composerReportRequest("", "")},
		{"no workspace", &agentreplv1.ReportComposerEntriesRequest{AtPoint: composerReportRequest("").GetAtPoint()}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			_, err := h.Client.ReportComposerEntries(context.Background(), connect.NewRequest(tc.req))

			// Assert
			var cerr *connect.Error
			if !errors.As(err, &cerr) || cerr.Code() != connect.CodeInvalidArgument {
				t.Fatalf("ReportComposerEntries = %v, want InvalidArgument", err)
			}
		})
	}
}

func TestReportComposerEntriesOnAnUnknownWorkspaceIsRefused(t *testing.T) {
	// Arrange
	h := newHarness(t)
	req := composerReportRequest("", "3.1")
	req.Workspace = &workspacev1.WorkspaceRef{Id: "ws-nope"}

	// Act
	resp, err := h.Client.ReportComposerEntries(context.Background(), connect.NewRequest(req))

	// Assert
	if err != nil || resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("ReportComposerEntries = (%v, %v), want the unknown_workspace arm", resp, err)
	}
}

// neighborRequest is a FindEntryNeighbor of LABEL, NEXT or previous.
func neighborRequest(label string, way *agentreplv1.EntryNeighborDirection) *agentreplv1.FindEntryNeighborRequest {
	return &agentreplv1.FindEntryNeighborRequest{Workspace: ref(), Label: label, Direction: way}
}

// Each direction FindEntryNeighbor is asked for.
var (
	wayNext      = &agentreplv1.EntryNeighborDirection{Way: &agentreplv1.EntryNeighborDirection_Next{Next: &agentreplv1.EntryNeighborNext{}}}
	wayPrevious  = &agentreplv1.EntryNeighborDirection{Way: &agentreplv1.EntryNeighborDirection_Previous{Previous: &agentreplv1.EntryNeighborPrevious{}}}
	wayFollowing = &agentreplv1.EntryNeighborDirection{Way: &agentreplv1.EntryNeighborDirection_Following{Following: &agentreplv1.EntryNeighborFollowing{}}}
	wayParent    = &agentreplv1.EntryNeighborDirection{Way: &agentreplv1.EntryNeighborDirection_Parent{Parent: &agentreplv1.EntryNeighborParent{}}}
)

func TestFindEntryNeighborAnswersTheFeedsNeighbor(t *testing.T) {
	tests := []struct {
		name  string
		way   *agentreplv1.EntryNeighborDirection
		want  string
		found bool
	}{
		{"the next node", wayNext, "3.3", true},
		{"the previous node", wayPrevious, "3.1", true},
		{"the following node", wayFollowing, "3.2.1", true},
		{"the parent", wayParent, "3", true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.Feed.neighbors = map[string]string{"3.2\x00next": "3.3", "3.2\x00previous": "3.1", "3.2\x00following": "3.2.1", "3.2\x00parent": "3"}

			// Act
			resp, err := h.Client.FindEntryNeighbor(context.Background(), connect.NewRequest(neighborRequest("3.2", tc.way)))

			// Assert
			if err != nil || resp.Msg.GetSuccess().GetFound().GetLabel() != tc.want {
				t.Fatalf("FindEntryNeighbor = (%v, %v), want %q found", resp, err, tc.want)
			}
		})
	}
}

func TestFindEntryNeighborAnswersNoneWhereThereIsNone(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	resp, err := h.Client.FindEntryNeighbor(context.Background(), connect.NewRequest(neighborRequest("9.9", wayNext)))

	// Assert
	if err != nil || resp.Msg.GetSuccess().GetNone() == nil {
		t.Fatalf("FindEntryNeighbor = (%v, %v), want the none arm", resp, err)
	}
}

func TestFindEntryNeighborWhileAPromptIsSelectedAnswersNone(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Feed.promptRows = map[string]bool{"p1": true}
	h.Feed.neighbors = map[string]string{"3.1\x00next": "3.2"}
	h.selectResponse(t, "p1")

	// Act
	resp, err := h.Client.FindEntryNeighbor(context.Background(), connect.NewRequest(neighborRequest("3.1", wayNext)))

	// Assert
	if err != nil || resp.Msg.GetSuccess().GetNone() == nil {
		t.Fatalf("FindEntryNeighbor = (%v, %v), want none while a prompt is selected", resp, err)
	}
}

func TestFindEntryNeighborRefusesAMalformedRequest(t *testing.T) {
	tests := []struct {
		name string
		req  *agentreplv1.FindEntryNeighborRequest
	}{
		{"no label", neighborRequest("", wayNext)},
		{"no direction", &agentreplv1.FindEntryNeighborRequest{Workspace: ref(), Label: "3.1"}},
		{"no workspace", &agentreplv1.FindEntryNeighborRequest{Label: "3.1", Direction: neighborRequest("3.1", wayNext).GetDirection()}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			_, err := h.Client.FindEntryNeighbor(context.Background(), connect.NewRequest(tc.req))

			// Assert
			var cerr *connect.Error
			if !errors.As(err, &cerr) || cerr.Code() != connect.CodeInvalidArgument {
				t.Fatalf("FindEntryNeighbor = %v, want InvalidArgument", err)
			}
		})
	}
}

func TestFindEntryNeighborOnAnUnknownWorkspaceIsRefused(t *testing.T) {
	// Arrange
	h := newHarness(t)
	req := neighborRequest("3.1", wayNext)
	req.Workspace = &workspacev1.WorkspaceRef{Id: "ws-nope"}

	// Act
	resp, err := h.Client.FindEntryNeighbor(context.Background(), connect.NewRequest(req))

	// Assert
	if err != nil || resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("FindEntryNeighbor = (%v, %v), want the unknown_workspace arm", resp, err)
	}
}

func TestASelectionMadeAfterTheReportRestatesTheHighlight(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Feed.promptRows = map[string]bool{"p1": true}
	h.Feed.targets = map[string]string{"3.1": "r1"}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := openRootWatch(t, h, ctx)
	report(t, h, composerReportRequest("3.1", "3.1"))
	if node := receiveComposerEntry(t, stream).GetNode(); node.GetRow().GetValue() != "r1" {
		t.Fatalf("composer entry = %v, want r1's node before the selection", node)
	}

	// Act: selecting a prompt puts the composer to editing it.
	h.selectResponse(t, "p1")

	// Assert
	if entry := receiveComposerEntry(t, stream); entry.GetNone() == nil {
		t.Fatalf("composer entry = %v, want none once a prompt is selected", entry)
	}
}
