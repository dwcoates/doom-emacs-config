package children

import (
	"slices"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/resolve/topbar"
	"agent-repld/internal/wsm"
)

const (
	parent = ids.WorkspaceID("ws-parent")
	childA = ids.WorkspaceID("ws-a")
	childB = ids.WorkspaceID("ws-b")
)

var epoch = time.Unix(1_800_000_000, 0)

type upsert struct {
	ws  ids.WorkspaceID
	row *frontendv1.FeedRow
}

type fakeFeed struct{ upserts []upsert }

func (f *fakeFeed) UpsertDurable(ws ids.WorkspaceID, _ feedid.Feed, row *frontendv1.FeedRow) {
	f.upserts = append(f.upserts, upsert{ws: ws, row: row})
}

// bubble is the last bubble drawn for CHILD, nil when none was.
func (f *fakeFeed) bubble(child ids.WorkspaceID) *frontendv1.FeedChild {
	want := Row(parent, child, nil).GetId().GetValue()
	for i := len(f.upserts) - 1; i >= 0; i-- {
		if f.upserts[i].row.GetId().GetValue() == want {
			return f.upserts[i].row.GetChild()
		}
	}
	return nil
}

type fakeFooter struct{ sets [][]footer.OpenChild }

func (f *fakeFooter) SetOpenChildren(_ ids.WorkspaceID, children []footer.OpenChild) {
	f.sets = append(f.sets, children)
}

func (f *fakeFooter) last() []footer.OpenChild {
	if len(f.sets) == 0 {
		return nil
	}
	return f.sets[len(f.sets)-1]
}

type fakeTopbar struct{ sets [][]topbar.ClosedChild }

func (f *fakeTopbar) SetClosedChildren(_ ids.WorkspaceID, children []topbar.ClosedChild) {
	f.sets = append(f.sets, children)
}

func (f *fakeTopbar) last() []topbar.ClosedChild {
	if len(f.sets) == 0 {
		return nil
	}
	return f.sets[len(f.sets)-1]
}

type fixture struct {
	t      *Tracker
	feed   *fakeFeed
	footer *fakeFooter
	topbar *fakeTopbar
	now    time.Time
}

func newFixture() *fixture {
	f := &fixture{feed: &fakeFeed{}, footer: &fakeFooter{}, topbar: &fakeTopbar{}, now: epoch.Add(time.Hour)}
	f.t = New(Deps{Feed: f.feed, Footer: f.footer, Topbar: f.topbar, Home: "/home/u",
		Now: func() time.Time { return f.now }, Log: dlog.NewTestLogger()})
	return f
}

// child is an open child of the parent named NAME, created AFTER the epoch.
func child(id ids.WorkspaceID, name string, after time.Duration) wsm.Workspace {
	p := parent
	return wsm.Workspace{ID: id, Name: name, Parent: &p, Repo: "repo-1", CreatedAt: epoch.Add(after)}
}

func registry(workspaces ...wsm.Workspace) sidebar.Registry {
	return sidebar.Registry{Workspaces: append([]wsm.Workspace{{ID: parent, Name: "parent", Repo: "repo-1"}}, workspaces...),
		Repositories: []wsm.Repository{{ID: "repo-1", Dir: "/home/u/.config/doom"}}}
}

func names(open []footer.OpenChild) []string {
	var out []string
	for _, c := range open {
		out = append(out, c.Name)
	}
	return out
}

func TestAnOpenChildsBubbleIsDrawnInItsParentsFeed(t *testing.T) {
	// Arrange
	f := newFixture()

	// Act
	f.t.SetRegistry(registry(child(childA, "fix-a", time.Minute)))
	f.t.SetArm(childA, "thinking", "red")
	f.t.SetSummary(childA, wsm.WorkspaceSummary{Short: "Fix the a", Long: "It fixes a. It pins b."})

	// Assert
	b := f.feed.bubble(childA)
	if f.feed.upserts[0].ws != parent || b == nil {
		t.Fatalf("upserts = %+v, want the bubble in the parent's feed", f.feed.upserts)
	}
	head := b.GetHead()
	if head.GetName().GetText() != "fix-a" || head.GetName().GetSelect().GetTarget().GetId() != string(childA) ||
		head.GetTarget().GetText() != "~/.config/doom" || head.GetStanding().GetOpen() == nil ||
		head.GetRuntime().GetStartedAtMs() != epoch.Add(time.Minute).UnixMilli() || head.GetRuntime().EndedAtMs != nil {
		t.Fatalf("head = %v, want the open child's head", head)
	}
	if b.GetBorder().GetTone() != "red" || b.GetSummary().GetShortSummary().GetText() != "Fix the a" ||
		b.GetSummary().GetLongSummary().GetMarkdown() == "" {
		t.Fatalf("bubble = %v, want its red border and both summaries", b)
	}
}

func TestAnUnchangedBubbleIsNotDrawnAgain(t *testing.T) {
	// Arrange
	f := newFixture()
	f.t.SetRegistry(registry(child(childA, "fix-a", 0)))
	f.t.SetArm(childA, "thinking", "red")
	drawn := len(f.feed.upserts)

	// Act
	f.t.SetRegistry(registry(child(childA, "fix-a", 0)))
	f.t.SetArm(childA, "thinking", "red")

	// Assert
	if len(f.feed.upserts) != drawn {
		t.Fatalf("upserts = %d, want %d: nothing changed", len(f.feed.upserts), drawn)
	}
}

func TestAChildsStandingFollowsItsArmAndItsRecord(t *testing.T) {
	merged := epoch.Add(2 * time.Hour)
	tests := []struct {
		name       string
		record     func(ws *wsm.Workspace)
		arm        string
		want       string
		wantBorder bool
	}{
		{name: "an open child", arm: "ready", want: "open", wantBorder: true},
		{name: "a child waiting in its queue", arm: "merge_queued", want: "waiting", wantBorder: true},
		{name: "a child being merged", arm: "merging", want: "merging", wantBorder: true},
		{name: "a landed child", arm: "merged", record: func(ws *wsm.Workspace) { ws.Closed, ws.MergedAt = true, &merged }, want: "merged"},
		{name: "a child closed without landing", arm: "ready", record: func(ws *wsm.Workspace) {
			ws.Closed, ws.Closure = true, &wsm.Closure{At: merged}
		}, want: "closed"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture()
			c := child(childA, "fix-a", 0)
			if tt.record != nil {
				tt.record(&c)
			}

			// Act
			f.t.SetArm(childA, tt.arm, "purple")
			f.t.SetRegistry(registry(c))

			// Assert
			s := f.feed.bubble(childA).GetHead().GetStanding()
			got := map[bool]string{true: "open"}[s.GetOpen() != nil]
			switch {
			case s.GetMerging().GetWaiting() != nil:
				got = "waiting"
			case s.GetMerging().GetMerging() != nil:
				got = "merging"
			case s.GetMerged() != nil:
				got = "merged"
			case s.GetClosed() != nil:
				got = "closed"
			}
			if got != tt.want || (f.feed.bubble(childA).GetBorder() != nil) != tt.wantBorder {
				t.Fatalf("standing = %v border %v, want %s with border %v", s, f.feed.bubble(childA).GetBorder(), tt.want, tt.wantBorder)
			}
		})
	}
}

func TestARemovedChildKeepsItsBubbleWithNoDoorAndLeavesBothLists(t *testing.T) {
	// Arrange
	f := newFixture()
	merged := epoch.Add(30 * time.Minute)
	landed := child(childA, "fix-a", 0)
	landed.Closed, landed.MergedAt = true, &merged
	f.t.SetRegistry(registry(landed))

	// Act: the registry stops carrying it.
	f.t.SetRegistry(registry())

	// Assert
	b := f.feed.bubble(childA)
	if b.GetHead().GetName().GetSelect() != nil || b.GetHead().GetStanding().GetRemoved().GetRemovedAtMs() != f.now.UnixMilli() {
		t.Fatalf("bubble = %v, want removed at now with no door", b)
	}
	if len(f.topbar.last()) != 0 || len(f.footer.last()) != 0 {
		t.Fatalf("menu %v, footer %v; want the removed child in neither", f.topbar.last(), f.footer.last())
	}
}

func TestEachChildIsInExactlyOneOfTheFooterAndTheMenu(t *testing.T) {
	// Arrange
	f := newFixture()
	early, late := epoch.Add(time.Hour), epoch.Add(2*time.Hour)
	mergedChild := child(childA, "landed", 0)
	mergedChild.Closed, mergedChild.MergedAt = true, &early
	closedChild := child(childB, "abandoned", time.Minute)
	closedChild.Closed, closedChild.Closure = true, &wsm.Closure{At: late}
	reopened := child("ws-c", "reopened", 2*time.Minute)
	reopened.MergedAt = &early
	open := child("ws-d", "working", 3*time.Minute)

	// Act
	f.t.SetRegistry(registry(open, mergedChild, closedChild, reopened))
	f.t.SetArm("ws-d", "permission", "green")

	// Assert: open children oldest first, a reopened merged one included;
	// the rest most recently ended first.
	if got := names(f.footer.last()); !slices.Equal(got, []string{"reopened", "working"}) {
		t.Fatalf("footer = %v, want the reopened and the working child", got)
	}
	if w := f.footer.last()[1]; !w.WaitingOnUser || w.Status != "permission" || w.Tone != "green" {
		t.Fatalf("working child = %+v, want it waiting on the user, green", w)
	}
	menu := f.topbar.last()
	if len(menu) != 2 || menu[0].Name != "abandoned" || menu[0].Merged || menu[1].Name != "landed" || !menu[1].Merged {
		t.Fatalf("menu = %+v, want abandoned then landed", menu)
	}
}

func TestAChildRetargetedAwayRedrawsItsOldParent(t *testing.T) {
	// Arrange
	f := newFixture()
	f.t.SetRegistry(registry(child(childA, "fix-a", 0)))

	// Act: it is no longer the parent's child.
	moved := child(childA, "fix-a", 0)
	moved.Parent = nil
	f.t.SetRegistry(registry(moved))

	// Assert
	if got := f.footer.last(); len(got) != 0 {
		t.Fatalf("footer = %v, want the old parent's list emptied", got)
	}
}

func TestATargetIsTheWorkspaceTheChildLandsInto(t *testing.T) {
	tests := []struct {
		name string
		reg  func() sidebar.Registry
		want string
	}{
		{name: "a child landing into another workspace", reg: func() sidebar.Registry {
			into := ids.WorkspaceID("ws-into")
			c := child(childA, "fix-a", 0)
			c.LandsInto = &into
			return registry(c, wsm.Workspace{ID: into, Dir: "/home/u/wt/into"})
		}, want: "~/wt/into"},
		{name: "a child whose landing target is unknown draws no arrow", reg: func() sidebar.Registry {
			into := ids.WorkspaceID("ws-gone")
			c := child(childA, "fix-a", 0)
			c.LandsInto = &into
			return registry(c)
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture()

			// Act
			f.t.SetRegistry(tt.reg())

			// Assert
			head := f.feed.bubble(childA).GetHead()
			if (head.GetTarget() != nil) != (tt.want != "") || head.GetTarget().GetText() != tt.want {
				t.Fatalf("target = %v, want %q", head.GetTarget(), tt.want)
			}
		})
	}
}

func TestARetractedSummaryLeavesTheBubbleTheHeadAlone(t *testing.T) {
	// Arrange
	f := newFixture()
	f.t.SetRegistry(registry(child(childA, "fix-a", 0)))
	f.t.SetSummary(childA, wsm.WorkspaceSummary{Short: "Fix the a"})

	// Act
	f.t.SetSummary(childA, wsm.WorkspaceSummary{})

	// Assert
	if s := f.feed.bubble(childA).GetSummary(); s != nil {
		t.Fatalf("summary = %v, want none", s)
	}
}

func TestNewRefusesAMissingCollaborator(t *testing.T) {
	defer func() {
		if recover() == nil {
			t.Fatal("New with no collaborators did not panic")
		}
	}()
	New(Deps{})
}
