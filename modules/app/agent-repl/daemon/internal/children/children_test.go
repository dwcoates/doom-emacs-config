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
	"agent-repld/internal/vocab"
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

// closedRegistry is registry with the parent closed.
func closedRegistry(workspaces ...wsm.Workspace) sidebar.Registry {
	reg := registry(workspaces...)
	reg.Workspaces[0].Closed = true
	return reg
}

// closedAndOpenChildren are an abandoned child and a working one.
func closedAndOpenChildren() []wsm.Workspace {
	abandoned := child(childA, "abandoned", 0)
	abandoned.Closed, abandoned.Closure = true, &wsm.Closure{At: epoch.Add(time.Hour)}
	return []wsm.Workspace{abandoned, child(childB, "working", time.Minute)}
}

// handed is how many bubbles, footer lists and menus F was handed.
func (f *fixture) handed() [3]int {
	return [3]int{len(f.feed.upserts), len(f.footer.sets), len(f.topbar.sets)}
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

// THE CHILD'S HEAD DRAWS THE MERGE HEAD'S GLYPH: both carry the one glyph
// name (vocab.MergeHeadGlyph), so the two heads cannot drift apart.
func TestAChildsHeadCarriesTheMergeHeadGlyph(t *testing.T) {
	// Arrange
	f := newFixture()

	// Act
	f.t.SetRegistry(registry(child(childA, "fix-a", time.Minute)))

	// Assert
	if got := f.feed.bubble(childA).GetHead().GetGlyph().GetIcon(); got != vocab.MergeHeadGlyph {
		t.Fatalf("the child's glyph = %q, want %q (vocab.MergeHeadGlyph)", got, vocab.MergeHeadGlyph)
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
	f.t.SetArm("ws-c", "ready", "turquoise")
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
	f.t.SetArm(childA, "thinking", "red")
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

func TestAParentThatIsNotOpenDrawsNothing(t *testing.T) {
	tests := []struct {
		name string
		reg  func() sidebar.Registry
	}{
		{name: "a closed parent with a closed and an open child", reg: func() sidebar.Registry {
			return closedRegistry(closedAndOpenChildren()...)
		}},
		{name: "a parent absent from the registry", reg: func() sidebar.Registry {
			unknown := ids.WorkspaceID("ws-unknown")
			reg := registry(closedAndOpenChildren()...)
			for i := range reg.Workspaces[1:] {
				reg.Workspaces[i+1].Parent = &unknown
			}
			return reg
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture()

			// Act
			f.t.SetRegistry(tt.reg())
			f.t.SetArm(childB, "thinking", "red")
			f.t.SetSummary(childB, wsm.WorkspaceSummary{Short: "Work"})

			// Assert
			if got := f.handed(); got != [3]int{} {
				t.Fatalf("handed %v bubbles, footer lists and menus; want none", got)
			}
		})
	}
}

func TestAParentClosedAfterBeingDrawnIsHandedNothingMore(t *testing.T) {
	// Arrange
	f := newFixture()
	f.t.SetRegistry(registry(closedAndOpenChildren()...))
	drawn := f.handed()

	// Act
	f.t.SetRegistry(closedRegistry(closedAndOpenChildren()...))
	f.t.SetArm(childB, "thinking", "red")
	f.t.SetSummary(childB, wsm.WorkspaceSummary{Short: "Work"})

	// Assert
	if got := f.handed(); got != drawn {
		t.Fatalf("handed %v bubbles, footer lists and menus; want %v, nothing after the close", got, drawn)
	}
}

func TestAReopenedParentIsDrawnAfresh(t *testing.T) {
	tests := []struct {
		name  string
		first func() sidebar.Registry
	}{
		{name: "a parent drawn, closed, then reopened", first: func() sidebar.Registry {
			return registry(closedAndOpenChildren()...)
		}},
		{name: "a parent closed from the start, then reopened", first: func() sidebar.Registry {
			return closedRegistry(closedAndOpenChildren()...)
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture()
			f.t.SetArm(childB, "thinking", "red")
			f.t.SetRegistry(tt.first())
			f.t.SetRegistry(closedRegistry(closedAndOpenChildren()...))
			before := f.handed()

			// Act
			f.t.SetRegistry(registry(closedAndOpenChildren()...))

			// Assert
			if got := f.handed(); got != [3]int{before[0] + 2, before[1] + 1, before[2] + 1} {
				t.Fatalf("handed %v bubbles, footer lists and menus, from %v; want both bubbles, the footer list and the menu again", got, before)
			}
			if f.feed.upserts[len(f.feed.upserts)-1].ws != parent || f.feed.bubble(childA) == nil || f.feed.bubble(childB) == nil {
				t.Fatalf("upserts = %+v, want both bubbles in the parent's feed", f.feed.upserts)
			}
			if got := names(f.footer.last()); !slices.Equal(got, []string{"working"}) {
				t.Fatalf("footer = %v, want the working child", got)
			}
			if menu := f.topbar.last(); len(menu) != 1 || menu[0].Name != "abandoned" {
				t.Fatalf("menu = %+v, want the abandoned child", menu)
			}
		})
	}
}

func TestAChildWhoseArmIsUndecidedIsNotListedInTheFooter(t *testing.T) {
	tests := []struct {
		name      string
		children  []wsm.Workspace
		arms      map[ids.WorkspaceID]string
		wantSets  int
		wantNames []string
	}{
		{name: "the only child, with no arm, hands the footer nothing",
			children: []wsm.Workspace{child(childA, "fix-a", 0)}},
		{name: "of two children only the armed one is listed",
			children: []wsm.Workspace{child(childA, "fix-a", 0), child(childB, "fix-b", time.Minute)},
			arms:     map[ids.WorkspaceID]string{childB: "thinking"}, wantSets: 1, wantNames: []string{"fix-b"}},
		{name: "the armed children stay in creation order around an unarmed one",
			children: []wsm.Workspace{child(childA, "fix-a", 0), child(childB, "fix-b", time.Minute), child("ws-c", "fix-c", 2*time.Minute)},
			arms:     map[ids.WorkspaceID]string{childA: "ready", "ws-c": "thinking"}, wantSets: 1, wantNames: []string{"fix-a", "fix-c"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture()
			for ws, name := range tt.arms {
				f.t.SetArm(ws, name, "red")
			}

			// Act
			f.t.SetRegistry(registry(tt.children...))

			// Assert
			if len(f.footer.sets) != tt.wantSets || !slices.Equal(names(f.footer.last()), tt.wantNames) {
				t.Fatalf("footer sets = %+v, want %d set listing %v", f.footer.sets, tt.wantSets, tt.wantNames)
			}
			for _, c := range f.footer.last() {
				if c.Status == "" || c.Tone == "" {
					t.Fatalf("footer child = %+v, want its status word and tone", c)
				}
			}
		})
	}
}

func TestAnArmsArrivalListsTheChildWithItsStatusAndTone(t *testing.T) {
	// Arrange
	f := newFixture()
	f.t.SetRegistry(registry(child(childA, "fix-a", 0)))

	// Act
	f.t.SetArm(childA, "merge_queued", "purple")

	// Assert
	got := f.footer.last()
	if len(f.footer.sets) != 1 || len(got) != 1 || got[0].Name != "fix-a" || got[0].Status != "merge queued" || got[0].Tone != "purple" {
		t.Fatalf("footer sets = %+v, want one list carrying the child, merge queued and purple", f.footer.sets)
	}
}

func TestAChildWhoseArmIsUndecidedStillHasItsBubbleWithNoBorder(t *testing.T) {
	// Arrange
	f := newFixture()

	// Act
	f.t.SetRegistry(registry(child(childA, "fix-a", 0)))

	// Assert
	b := f.feed.bubble(childA)
	if b == nil || b.GetHead().GetName().GetText() != "fix-a" || b.GetHead().GetStanding().GetOpen() == nil {
		t.Fatalf("bubble = %v, want the open child's bubble in the parent's feed", b)
	}
	if b.GetBorder() != nil {
		t.Fatalf("border = %v, want none while no arm is decided", b.GetBorder())
	}
}
