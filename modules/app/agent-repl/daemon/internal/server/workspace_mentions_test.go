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

// mentionRow is an open row naming workspace ID as NAME, with CHILDREN nested under it.
func mentionRow(id, name string, children ...*frontendv1.RosterOpenRow) *frontendv1.RosterOpenRow {
	return &frontendv1.RosterOpenRow{
		Row: &frontendv1.RosterRow{
			Workspace: &frontendv1.RosterRowWorkspace{Workspace: &workspacev1.WorkspaceRef{Id: id, Dir: "/w/" + id}},
			Name:      &frontendv1.RosterRowName{Text: name},
		},
		Children: children,
	}
}

// mentionRoster is a roster whose one repository section holds ROWS, in no
// subsection, and whose hidden section holds HIDDEN.
func mentionRoster(rows []*frontendv1.RosterOpenRow, hidden ...*frontendv1.RosterOpenRow) *frontendv1.WorkspaceRoster {
	section := &frontendv1.RosterRepoSection{}
	for _, row := range rows {
		section.Entries = append(section.Entries, &frontendv1.RosterRepoEntry{Entry: &frontendv1.RosterRepoEntry_Row{Row: row}})
	}
	var hiddenRows []*frontendv1.RosterHiddenRow
	for _, row := range hidden {
		hiddenRows = append(hiddenRows, &frontendv1.RosterHiddenRow{Row: row.GetRow()})
	}
	return &frontendv1.WorkspaceRoster{
		Repository: &frontendv1.RosterRepositoryView{Sections: []*frontendv1.RosterRepoSection{section}},
		Hidden:     &frontendv1.RosterHiddenSection{HiddenRows: &frontendv1.RosterHiddenRows{Rows: hiddenRows}},
	}
}

// targetNames reads TARGETS back as "name=id" pairs, in order.
func targetNames(targets *frontendv1.WorkspaceMentionTargets) []string {
	var out []string
	for _, target := range targets.GetTargets() {
		out = append(out, target.GetName().GetText()+"="+target.GetWorkspace().GetId())
	}
	return out
}

// TestMentionTargetsResolvesTheViewersSet pins which open workspaces a page
// may link: the Open section's, in its order, less the viewer's own, every
// shared name, every unnamed row, and every hidden workspace.
func TestMentionTargetsResolvesTheViewersSet(t *testing.T) {
	cases := []struct {
		name   string
		roster *frontendv1.WorkspaceRoster
		viewer string
		want   []string
	}{
		{
			name:   "every open workspace in the Open section's order",
			roster: mentionRoster([]*frontendv1.RosterOpenRow{mentionRow("b", "beta"), mentionRow("a", "alpha")}),
			viewer: "v",
			want:   []string{"beta=b", "alpha=a"},
		},
		{
			name:   "a child workspace is a target after its parent",
			roster: mentionRoster([]*frontendv1.RosterOpenRow{mentionRow("p", "parent", mentionRow("c", "child"))}),
			viewer: "v",
			want:   []string{"parent=p", "child=c"},
		},
		{
			name:   "the viewer's own workspace is no target",
			roster: mentionRoster([]*frontendv1.RosterOpenRow{mentionRow("v", "viewer"), mentionRow("a", "alpha")}),
			viewer: "v",
			want:   []string{"alpha=a"},
		},
		{
			name:   "a name two open workspaces share is no target",
			roster: mentionRoster([]*frontendv1.RosterOpenRow{mentionRow("a", "same"), mentionRow("b", "same"), mentionRow("c", "gamma")}),
			viewer: "v",
			want:   []string{"gamma=c"},
		},
		{
			name:   "a name the viewer shares with another workspace is no target either",
			roster: mentionRoster([]*frontendv1.RosterOpenRow{mentionRow("v", "same"), mentionRow("b", "same")}),
			viewer: "v",
			want:   nil,
		},
		{
			name:   "an unnamed row is no target",
			roster: mentionRoster([]*frontendv1.RosterOpenRow{mentionRow("a", ""), mentionRow("b", "beta")}),
			viewer: "v",
			want:   []string{"beta=b"},
		},
		{
			name:   "a hidden workspace is no target",
			roster: mentionRoster([]*frontendv1.RosterOpenRow{mentionRow("a", "alpha")}, mentionRow("h", "hidden")),
			viewer: "v",
			want:   []string{"alpha=a"},
		},
		{
			name:   "an empty roster has no targets",
			roster: &frontendv1.WorkspaceRoster{},
			viewer: "v",
			want:   nil,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := targetNames(mentionTargets(tc.roster, tc.viewer))

			// Assert.
			if !slices.Equal(got, tc.want) {
				t.Fatalf("targets = %v, want %v", got, tc.want)
			}
		})
	}
}

// mentionSink records every frame a changedOnlySink let through, failing
// the send FAIL names.
type mentionSink struct {
	sent []*agentreplv1.WatchWorkspaceMentionsResponse
	fail error
}

func (s *mentionSink) Send(m *agentreplv1.WatchWorkspaceMentionsResponse) error {
	if s.fail != nil {
		return s.fail
	}
	s.sent = append(s.sent, m)
	return nil
}

// mentionFrame is a targets frame carrying NAMES, each its own workspace.
func mentionFrame(names ...string) *agentreplv1.WatchWorkspaceMentionsResponse {
	var rows []*frontendv1.RosterOpenRow
	for _, name := range names {
		rows = append(rows, mentionRow(name, name))
	}
	return &agentreplv1.WatchWorkspaceMentionsResponse{
		Push: &agentreplv1.WatchWorkspaceMentionsResponse_Targets{Targets: mentionTargets(mentionRoster(rows), "v")},
	}
}

// TestChangedOnlySinkSendsOnlyChanges pins the derived stream's one rule: a
// frame equal to the last one sent is dropped, and every other goes out.
func TestChangedOnlySinkSendsOnlyChanges(t *testing.T) {
	cases := []struct {
		name   string
		frames []*agentreplv1.WatchWorkspaceMentionsResponse
		want   [][]string
	}{
		{
			name:   "the first frame is always sent",
			frames: []*agentreplv1.WatchWorkspaceMentionsResponse{mentionFrame("a")},
			want:   [][]string{{"a=a"}},
		},
		{
			name:   "an equal frame is dropped",
			frames: []*agentreplv1.WatchWorkspaceMentionsResponse{mentionFrame("a"), mentionFrame("a")},
			want:   [][]string{{"a=a"}},
		},
		{
			name:   "a changed frame is sent",
			frames: []*agentreplv1.WatchWorkspaceMentionsResponse{mentionFrame("a"), mentionFrame("a", "b")},
			want:   [][]string{{"a=a"}, {"a=a", "b=b"}},
		},
		{
			name:   "a frame equal to an earlier but not the last one is sent",
			frames: []*agentreplv1.WatchWorkspaceMentionsResponse{mentionFrame("a"), mentionFrame("b"), mentionFrame("a")},
			want:   [][]string{{"a=a"}, {"b=b"}, {"a=a"}},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			out := &mentionSink{}
			var told int
			sink := &changedOnlySink[agentreplv1.WatchWorkspaceMentionsResponse, *agentreplv1.WatchWorkspaceMentionsResponse]{
				out:  out,
				sent: func(*agentreplv1.WatchWorkspaceMentionsResponse) { told++ },
			}

			// Act.
			for _, frame := range tc.frames {
				if err := sink.Send(frame); err != nil {
					t.Fatalf("Send: %v", err)
				}
			}

			// Assert.
			var got [][]string
			for _, frame := range out.sent {
				got = append(got, targetNames(frame.GetTargets()))
			}
			if !slices.EqualFunc(got, tc.want, slices.Equal) {
				t.Fatalf("sent = %v, want %v", got, tc.want)
			}
			if told != len(tc.want) {
				t.Fatalf("sent was told %d times, want once per frame sent (%d)", told, len(tc.want))
			}
		})
	}
}

// TestChangedOnlySinkRetriesAFrameItFailedToSend pins that a failed send is
// not remembered as sent: the same frame offered again goes out.
func TestChangedOnlySinkRetriesAFrameItFailedToSend(t *testing.T) {
	// Arrange.
	gone := errors.New("the client went away")
	out := &mentionSink{fail: gone}
	sink := &changedOnlySink[agentreplv1.WatchWorkspaceMentionsResponse, *agentreplv1.WatchWorkspaceMentionsResponse]{out: out}

	// Act.
	failed := sink.Send(mentionFrame("a"))
	out.fail = nil
	retried := sink.Send(mentionFrame("a"))

	// Assert.
	if !errors.Is(failed, gone) {
		t.Fatalf("the failed send answered %v, want the sink's own error", failed)
	}
	if retried != nil || len(out.sent) != 1 {
		t.Fatalf("the retry answered %v and sent %d frames, want the frame sent once", retried, len(out.sent))
	}
}

// TestWatchWorkspaceMentionsPushesOnlyAChangedSet pins the stream end to end:
// the first roster's set arrives, a roster that changes no target pushes
// nothing, and the next change arrives as the very next frame.
func TestWatchWorkspaceMentionsPushesOnlyAChangedSet(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream, err := h.Client.WatchWorkspaceMentions(ctx, connect.NewRequest(&agentreplv1.WatchWorkspaceMentionsRequest{
		Viewer: &workspacev1.WorkspaceRef{Id: "v"},
	}))
	if err != nil {
		t.Fatalf("open the stream: %v", err)
	}
	first := mentionRoster([]*frontendv1.RosterOpenRow{mentionRow("v", "viewer"), mentionRow("a", "alpha")})
	sameTargets := mentionRoster([]*frontendv1.RosterOpenRow{mentionRow("v", "viewer"), mentionRow("a", "alpha")})
	sameTargets.Revision = 2
	changed := mentionRoster([]*frontendv1.RosterOpenRow{mentionRow("a", "alpha"), mentionRow("b", "beta")})

	// Act.
	h.Sidebar.topic.Publish(first)
	var got [][]string
	if !stream.Receive() {
		t.Fatalf("receive the first set: %v", stream.Err())
	}
	got = append(got, targetNames(stream.Msg().GetTargets()))
	h.Sidebar.topic.Publish(sameTargets)
	h.Sidebar.topic.Publish(changed)
	if !stream.Receive() {
		t.Fatalf("receive the changed set: %v", stream.Err())
	}
	got = append(got, targetNames(stream.Msg().GetTargets()))

	// Assert.
	want := [][]string{{"alpha=a"}, {"alpha=a", "beta=b"}}
	if !slices.EqualFunc(got, want, slices.Equal) {
		t.Fatalf("frames = %v, want %v", got, want)
	}
}

// TestWatchWorkspaceMentionsRefusesAMissingViewer pins that a stream naming no
// viewer is refused as an invalid argument before it is accepted.
func TestWatchWorkspaceMentionsRefusesAMissingViewer(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act.
	stream, err := h.Client.WatchWorkspaceMentions(ctx, connect.NewRequest(&agentreplv1.WatchWorkspaceMentionsRequest{}))
	if err == nil {
		for stream.Receive() {
		}
		err = stream.Err()
	}

	// Assert.
	if connect.CodeOf(err) != connect.CodeInvalidArgument {
		t.Fatalf("the stream ended with %v, want invalid_argument", err)
	}
}

// TestAPageCarriesItsWorkspaceMentions pins the page-mux arm: a page's
// `workspace_mentions` subscription carries the same targets the dedicated rpc does.
func TestAPageCarriesItsWorkspaceMentions(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := attachPageStream(t, h, ctx, "page-1")

	// Act.
	subscribePage(t, ctx, h, &agentreplv1.SubscribePageRequest{
		Page:         "page-1",
		Subscription: "mentions-1",
		Request: &agentreplv1.SubscribePageRequest_WorkspaceMentions{
			WorkspaceMentions: &agentreplv1.WatchWorkspaceMentionsRequest{Viewer: &workspacev1.WorkspaceRef{Id: "v"}},
		},
	})
	h.Sidebar.topic.Publish(mentionRoster([]*frontendv1.RosterOpenRow{mentionRow("a", "alpha")}))

	// Assert.
	if !stream.Receive() {
		t.Fatalf("the page's stream ended before the mentions push: %v", stream.Err())
	}
	push := stream.Msg().GetPush()
	if push.GetSubscription() != "mentions-1" {
		t.Fatalf("the push was addressed to %q, not to the subscription that asked for it", push.GetSubscription())
	}
	if got := targetNames(push.GetWorkspaceMentions().GetTargets()); !slices.Equal(got, []string{"alpha=a"}) {
		t.Fatalf("the push carried targets %v, want [alpha=a]", got)
	}
}
