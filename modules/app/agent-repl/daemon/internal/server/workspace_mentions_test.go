package server

import (
	"context"
	"errors"
	"slices"
	"testing"
	"time"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
)

// mentionRow is an open row naming workspace ID as NAME, in no stated
// repository, with CHILDREN nested under it.
func mentionRow(id, name string, children ...*frontendv1.RosterOpenRow) *frontendv1.RosterOpenRow {
	return mentionRowIn(id, name, "", children...)
}

// mentionRowIn is mentionRow in REPOSITORY ("" for none stated).
func mentionRowIn(id, name, repository string, children ...*frontendv1.RosterOpenRow) *frontendv1.RosterOpenRow {
	row := &frontendv1.RosterRow{
		Workspace: &frontendv1.RosterRowWorkspace{Workspace: &workspacev1.WorkspaceRef{Id: id, Dir: "/w/" + id}},
		Name:      &frontendv1.RosterRowName{Text: name},
	}
	if repository != "" {
		row.Repository = &frontendv1.RosterRowRepository{Name: repository}
	}
	return &frontendv1.RosterOpenRow{Row: row, Children: children}
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

// mentionNow is the instant every resolve in these tests runs at.
var mentionNow = time.Date(2026, 11, 17, 12, 0, 0, 0, time.UTC)

// closedRow is a row naming workspace ID as NAME, closed AGO before
// mentionNow.
func closedRow(id, name string, ago time.Duration) *frontendv1.RosterRow {
	row := mentionRow(id, name).GetRow()
	row.Closure = &frontendv1.RosterRowClosure{
		AtMs: mentionNow.Add(-ago).UnixMilli(),
		By:   &frontendv1.RosterRowClosure_User{User: &frontendv1.RosterRowClosureByUser{}},
	}
	return row
}

// closedRowIn is closedRow in REPOSITORY.
func closedRowIn(id, name, repository string, ago time.Duration) *frontendv1.RosterRow {
	row := closedRow(id, name, ago)
	row.Repository = &frontendv1.RosterRowRepository{Name: repository}
	return row
}

// withClosed is ROSTER with MERGED in its recently-merged band and CLOSED in
// its closed list.
func withClosed(roster *frontendv1.WorkspaceRoster, merged, closed []*frontendv1.RosterRow) *frontendv1.WorkspaceRoster {
	var mergedRows []*frontendv1.RosterMergedRow
	for _, row := range merged {
		mergedRows = append(mergedRows, &frontendv1.RosterMergedRow{Row: row})
	}
	roster.RecentlyMerged = &frontendv1.RosterMergedSection{Groups: []*frontendv1.RosterMergedRepoGroup{
		{MergedRows: &frontendv1.RosterMergedRows{Rows: mergedRows}},
	}}
	roster.Closed = &frontendv1.RosterClosedRows{}
	for _, row := range closed {
		roster.Closed.Rows = append(roster.Closed.Rows, &frontendv1.RosterClosedRow{Row: row})
	}
	return roster
}

// targetNames reads TARGETS back as "name=id" pairs, in order, a closed
// target's suffixed " (closed)". A target with no standing fails the test.
func targetNames(targets *frontendv1.WorkspaceMentionTargets) []string {
	var out []string
	for _, target := range targets.GetTargets() {
		pair := target.GetName().GetText() + "=" + target.GetWorkspace().GetId()
		switch target.GetStanding().(type) {
		case *frontendv1.WorkspaceMentionTarget_Open:
		case *frontendv1.WorkspaceMentionTarget_Closed:
			pair += " (closed)"
		default:
			panic("a mention target carries no standing: " + pair)
		}
		out = append(out, pair)
	}
	return out
}

// TestMentionTargetsResolvesTheViewersSet pins which workspaces a viewer may
// mention: the Open section's in its order then the hidden section's, less
// the viewer's own, every unnamed row, and every shared name that the
// viewer's repository does not settle.
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
			name:   "a hidden workspace is a target after the Open section's",
			roster: mentionRoster([]*frontendv1.RosterOpenRow{mentionRow("a", "alpha")}, mentionRow("h", "hidden")),
			viewer: "v",
			want:   []string{"alpha=a", "hidden=h"},
		},
		{
			name:   "a hidden viewer is no target",
			roster: mentionRoster([]*frontendv1.RosterOpenRow{mentionRow("a", "alpha")}, mentionRow("v", "viewer")),
			viewer: "v",
			want:   []string{"alpha=a"},
		},
		{
			name: "a shared name names the one workspace in the viewer's repository",
			roster: mentionRoster([]*frontendv1.RosterOpenRow{
				mentionRowIn("v", "viewer", "doom"), mentionRowIn("z", "fix", "zettel"), mentionRowIn("d", "fix", "doom"),
			}),
			viewer: "v",
			want:   []string{"fix=d"},
		},
		{
			name: "a shared name open twice in the viewer's repository is no target",
			roster: mentionRoster([]*frontendv1.RosterOpenRow{
				mentionRowIn("v", "viewer", "doom"), mentionRowIn("a", "fix", "doom"), mentionRowIn("b", "fix", "doom"),
			}),
			viewer: "v",
			want:   nil,
		},
		{
			name: "a shared name in none of the viewer's repository is no target",
			roster: mentionRoster([]*frontendv1.RosterOpenRow{
				mentionRowIn("v", "viewer", "doom"), mentionRowIn("a", "fix", "zettel"), mentionRowIn("b", "fix", "cee"),
			}),
			viewer: "v",
			want:   nil,
		},
		{
			name: "a shared name prefers no one when the viewer's repository is unknown",
			roster: mentionRoster([]*frontendv1.RosterOpenRow{
				mentionRowIn("a", "fix", "doom"), mentionRowIn("b", "fix", "zettel"),
			}),
			viewer: "v",
			want:   nil,
		},
		{
			name: "a name the viewer shares with a workspace of another repository is no target",
			roster: mentionRoster([]*frontendv1.RosterOpenRow{
				mentionRowIn("v", "same", "doom"), mentionRowIn("b", "same", "zettel"),
			}),
			viewer: "v",
			want:   nil,
		},
		{
			name:   "a name shared between an open and a hidden workspace is settled the same way",
			roster: mentionRoster([]*frontendv1.RosterOpenRow{mentionRowIn("v", "viewer", "doom"), mentionRowIn("a", "fix", "zettel")}, mentionRowIn("h", "fix", "doom")),
			viewer: "v",
			want:   []string{"fix=h"},
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
			got := targetNames(mentionTargets(tc.roster, tc.viewer, mentionNow))

			// Assert.
			if !slices.Equal(got, tc.want) {
				t.Fatalf("targets = %v, want %v", got, tc.want)
			}
		})
	}
}

// TestMentionTargetsLinksRecentlyClosedWorkspaces pins the closed half of
// the set: a workspace closed within RecentlyClosedMentionWindow is a closed
// target, the window's edge included, and one closed earlier, or with no
// close record, is none.
func TestMentionTargetsLinksRecentlyClosedWorkspaces(t *testing.T) {
	window := RecentlyClosedMentionWindow
	open := func(rows ...*frontendv1.RosterOpenRow) *frontendv1.WorkspaceRoster { return mentionRoster(rows) }
	unrecorded := mentionRow("u", "unrecorded").GetRow()
	cases := []struct {
		name   string
		roster *frontendv1.WorkspaceRoster
		want   []string
	}{
		{
			name:   "an open workspace links as before",
			roster: withClosed(open(mentionRow("a", "alpha")), nil, nil),
			want:   []string{"alpha=a"},
		},
		{
			name:   "a workspace closed within the window links as closed",
			roster: withClosed(open(), nil, []*frontendv1.RosterRow{closedRow("c", "gone", window-time.Hour)}),
			want:   []string{"gone=c (closed)"},
		},
		{
			name:   "a merged workspace closed within the window links as closed",
			roster: withClosed(open(), []*frontendv1.RosterRow{closedRow("m", "landed", time.Hour)}, nil),
			want:   []string{"landed=m (closed)"},
		},
		{
			name:   "a workspace closed exactly the window ago still links",
			roster: withClosed(open(), nil, []*frontendv1.RosterRow{closedRow("c", "edge", window)}),
			want:   []string{"edge=c (closed)"},
		},
		{
			name:   "a workspace closed a millisecond past the window does not link",
			roster: withClosed(open(), nil, []*frontendv1.RosterRow{closedRow("c", "stale", window+time.Millisecond)}),
			want:   nil,
		},
		{
			name:   "a workspace closed well beyond the window does not link",
			roster: withClosed(open(), []*frontendv1.RosterRow{closedRow("m", "old", 2*window)}, nil),
			want:   nil,
		},
		{
			name:   "a closed workspace with no close record does not link",
			roster: withClosed(open(), nil, []*frontendv1.RosterRow{unrecorded}),
			want:   nil,
		},
		{
			name:   "closed targets follow the open ones, merged before closed",
			roster: withClosed(open(mentionRow("a", "alpha")), []*frontendv1.RosterRow{closedRow("m", "landed", time.Hour)}, []*frontendv1.RosterRow{closedRow("c", "gone", time.Hour)}),
			want:   []string{"alpha=a", "landed=m (closed)", "gone=c (closed)"},
		},
		{
			name:   "an open workspace's name wins over a closed one's",
			roster: withClosed(open(mentionRow("a", "same")), nil, []*frontendv1.RosterRow{closedRow("c", "same", time.Hour)}),
			want:   []string{"same=a"},
		},
		{
			name:   "a name two closed workspaces share is no target",
			roster: withClosed(open(), []*frontendv1.RosterRow{closedRow("m", "same", time.Hour)}, []*frontendv1.RosterRow{closedRow("c", "same", time.Hour)}),
			want:   nil,
		},
		{
			name: "a name two closed workspaces share names the one in the viewer's repository",
			roster: withClosed(open(mentionRowIn("v", "viewer", "doom")), nil, []*frontendv1.RosterRow{
				closedRowIn("c", "same", "doom", time.Hour), closedRowIn("d", "same", "other", time.Hour),
			}),
			want: []string{"same=c (closed)"},
		},
		{
			name:   "a closed workspace sharing a hidden workspace's name is no target",
			roster: withClosed(mentionRoster(nil, mentionRow("h", "same")), nil, []*frontendv1.RosterRow{closedRow("c", "same", time.Hour)}),
			want:   []string{"same=h"},
		},
		{
			name:   "the viewer's own closed workspace is no target",
			roster: withClosed(open(), nil, []*frontendv1.RosterRow{closedRow("v", "viewer", time.Hour)}),
			want:   nil,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := targetNames(mentionTargets(tc.roster, "v", mentionNow))

			// Assert.
			if !slices.Equal(got, tc.want) {
				t.Fatalf("targets = %v, want %v", got, tc.want)
			}
		})
	}
}

// TestMentionTargetsStateEachTargetsRepository pins the repository the
// composer annotates a completion with: the row's, or none when the row
// states none.
func TestMentionTargetsStateEachTargetsRepository(t *testing.T) {
	cases := []struct {
		name       string
		repository string
		want       *frontendv1.WorkspaceMentionRepository
	}{
		{name: "a stated repository rides on the target", repository: "doom", want: &frontendv1.WorkspaceMentionRepository{Name: "doom"}},
		{name: "an unstated repository leaves the target's unset", repository: "", want: nil},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			roster := mentionRoster([]*frontendv1.RosterOpenRow{mentionRowIn("a", "alpha", tc.repository)})

			// Act.
			got := mentionTargets(roster, "v", mentionNow).GetTargets()

			// Assert.
			if len(got) != 1 {
				t.Fatalf("targets = %v, want one", got)
			}
			if !proto.Equal(got[0].GetRepository(), tc.want) || (tc.want == nil) != (got[0].Repository == nil) {
				t.Fatalf("repository = %v, want %v", got[0].GetRepository(), tc.want)
			}
		})
	}
}

// TestMentionTargetsTrackerAnswersOnlyChanges pins the host stream's change
// filter: the first set is a change, an equal set is not, a different one is.
func TestMentionTargetsTrackerAnswersOnlyChanges(t *testing.T) {
	// Arrange.
	tracker := mentionTargetsTracker{viewer: "v", now: func() time.Time { return mentionNow }}
	alpha := mentionRoster([]*frontendv1.RosterOpenRow{mentionRow("a", "alpha")})
	beta := mentionRoster([]*frontendv1.RosterOpenRow{mentionRow("a", "alpha"), mentionRow("b", "beta")})

	steps := []struct {
		name        string
		roster      *frontendv1.WorkspaceRoster
		wantChanged bool
		wantNames   []string
	}{
		{name: "the first set is a change", roster: alpha, wantChanged: true, wantNames: []string{"alpha=a"}},
		{name: "an equal set is no change", roster: alpha, wantChanged: false, wantNames: []string{"alpha=a"}},
		{name: "a different set is a change", roster: beta, wantChanged: true, wantNames: []string{"alpha=a", "beta=b"}},
	}
	for _, step := range steps {
		t.Run(step.name, func(t *testing.T) {
			// Act.
			set, changed := tracker.next(step.roster)

			// Assert.
			if changed != step.wantChanged {
				t.Fatalf("changed = %v, want %v", changed, step.wantChanged)
			}
			if got := targetNames(set); !slices.Equal(got, step.wantNames) {
				t.Fatalf("targets = %v, want %v", got, step.wantNames)
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
		Push: &agentreplv1.WatchWorkspaceMentionsResponse_Targets{Targets: mentionTargets(mentionRoster(rows), "v", mentionNow)},
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

// receiveHostMentionTargets reads the host stream up to its next
// mention_targets push, skipping every other arm.
func receiveHostMentionTargets(
	t *testing.T,
	stream *connect.ServerStreamForClient[agentreplv1.WatchHostWorkspaceResponse],
) *frontendv1.WorkspaceMentionTargets {
	t.Helper()
	return receivePush(t, stream, "the mention targets", func(r *agentreplv1.WatchHostWorkspaceResponse) *frontendv1.WorkspaceMentionTargets {
		return r.GetMentionTargets()
	})
}

// TestTheHostStreamPushesOnlyAChangedSetOfMentionTargets pins the composer's
// feed end to end: the first roster's set arrives, a roster that changes no
// target pushes nothing, and the next change arrives as the next set.
func TestTheHostStreamPushesOnlyAChangedSetOfMentionTargets(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	viewer := string(testWorkspaceID)
	first := mentionRoster([]*frontendv1.RosterOpenRow{mentionRow(viewer, "viewer"), mentionRow("a", "alpha")})
	sameTargets := mentionRoster([]*frontendv1.RosterOpenRow{mentionRow(viewer, "viewer"), mentionRow("a", "alpha")})
	sameTargets.Revision = 2
	changed := mentionRoster([]*frontendv1.RosterOpenRow{mentionRow("a", "alpha"), mentionRow("b", "beta")})
	h.Sidebar.topic.Publish(first)
	stream := dialHostStream(t, h, ctx)

	// Act.
	var got [][]string
	got = append(got, targetNames(receiveHostMentionTargets(t, stream)))
	h.Sidebar.topic.Publish(sameTargets)
	h.Sidebar.topic.Publish(changed)
	got = append(got, targetNames(receiveHostMentionTargets(t, stream)))

	// Assert.
	want := [][]string{{"alpha=a"}, {"alpha=a", "beta=b"}}
	if !slices.EqualFunc(got, want, slices.Equal) {
		t.Fatalf("sets = %v, want %v", got, want)
	}
}

// TestTheComposerCompletesExactlyTheNamesThePageLinks pins the invariant the
// one resolve exists for: for one viewer and one roster, the host stream's
// mention_targets (what the composer completes) equal the page's
// WatchWorkspaceMentions targets (what the feed links).
func TestTheComposerCompletesExactlyTheNamesThePageLinks(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	viewer := string(testWorkspaceID)
	h.Sidebar.topic.Publish(mentionRoster(
		[]*frontendv1.RosterOpenRow{
			mentionRowIn(viewer, "viewer", "doom"), mentionRowIn("a", "fix", "doom"), mentionRowIn("b", "fix", "zettel"),
			mentionRowIn("c", "gamma", "zettel"),
		},
		mentionRowIn("h", "hidden", "doom"),
	))
	host := dialHostStream(t, h, ctx)
	page, err := h.Client.WatchWorkspaceMentions(ctx, connect.NewRequest(&agentreplv1.WatchWorkspaceMentionsRequest{
		Viewer: &workspacev1.WorkspaceRef{Id: viewer},
	}))
	if err != nil {
		t.Fatalf("open the page's stream: %v", err)
	}

	// Act.
	completable := receiveHostMentionTargets(t, host)
	if !page.Receive() {
		t.Fatalf("receive the page's set: %v", page.Err())
	}
	linkable := page.Msg().GetTargets()

	// Assert.
	if !proto.Equal(completable, linkable) {
		t.Fatalf("completable %v != linkable %v", targetNames(completable), targetNames(linkable))
	}
	if want := []string{"fix=a", "gamma=c", "hidden=h"}; !slices.Equal(targetNames(linkable), want) {
		t.Fatalf("targets = %v, want %v", targetNames(linkable), want)
	}
}
