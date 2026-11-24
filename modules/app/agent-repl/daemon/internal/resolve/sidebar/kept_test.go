package sidebar_test

import (
	"fmt"
	"sync"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	"agentrepl/protohelpers/rosterwalk"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/repodivergence"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/wsm"
)

// The registry of the handover that wedged (2026-11-23): 1,529 workspaces,
// of which about 1,500 were closed.
const (
	realisticOpen   = 29
	realisticClosed = 1500
)

// realisticRegistry builds the realistic registry's workspaces: the open ones
// first, then the closed ones.
func realisticRegistry() []wsm.Workspace {
	out := make([]wsm.Workspace, 0, realisticOpen+realisticClosed)
	for i := range realisticOpen + realisticClosed {
		ws := workspace(fmt.Sprintf("ws%04d", i), fmt.Sprintf("name%04d", i))
		ws.Closed = i >= realisticOpen
		out = append(out, ws)
	}
	return out
}

// countedSurfaces are test surfaces that count every log sink resolved, and
// refuse a sink for the directories in refused, as the real surfaces refuse a
// directory that is gone.
type countedSurfaces struct {
	*dlog.TestSurfaces

	mu       sync.Mutex
	resolved int
	refused  map[string]bool
}

func newCountedSurfaces() *countedSurfaces {
	return &countedSurfaces{TestSurfaces: dlog.NewTestSurfaces(), refused: map[string]bool{}}
}

// Workspace counts the resolve and refuses a refused directory.
func (s *countedSurfaces) Workspace(dir string) (dlog.Logger, error) {
	s.mu.Lock()
	s.resolved++
	refused := s.refused[dir]
	s.mu.Unlock()
	if refused {
		return nil, fmt.Errorf("resolve workspace %q for its log sink: %w", dir, dlog.ErrWorkspaceDirGone)
	}
	return s.TestSurfaces.Workspace(dir)
}

// WorkspaceOrCentral counts the resolve and falls back for a refused
// directory.
func (s *countedSurfaces) WorkspaceOrCentral(dir string) dlog.Logger {
	if log, err := s.Workspace(dir); err == nil {
		return log
	}
	return s.TestSurfaces.Global()
}

// resolves answers how many sinks have been resolved so far.
func (s *countedSurfaces) resolves() int {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.resolved
}

// refuse makes DIR a directory no sink resolves for.
func (s *countedSurfaces) refuse(dir string) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.refused[dir] = true
}

// renderCost is what the renders since a mark derived and built, summed.
type renderCost struct {
	renders, armsDerived, rowsBuilt int
}

// rendersSince sums the `daemon.sidebar.rendered` records after the first
// MARK records.
func rendersSince(t *testing.T, records []dlog.Record, mark int) renderCost {
	t.Helper()
	var out renderCost
	for _, rec := range records[mark:] {
		if rec.Operation != "daemon.sidebar.rendered" {
			continue
		}
		out.renders++
		out.armsDerived += intField(t, rec, "arms_derived")
		out.rowsBuilt += intField(t, rec, "rows_built")
	}
	if out.renders == 0 {
		t.Fatal("no render was recorded since the mark")
	}
	return out
}

// intField reads REC's integer context field KEY.
func intField(t *testing.T, rec dlog.Record, key string) int {
	t.Helper()
	v, ok := rec.Context[key].(int)
	if !ok {
		t.Fatalf("record %s carries %s = %#v, want an int", rec.Operation, key, rec.Context[key])
	}
	return v
}

// rowOf finds WS's row in PUSH.
func rowOf(t *testing.T, push *frontendv1.RosterPush, ws ids.WorkspaceID) *frontendv1.RosterRow {
	t.Helper()
	for _, row := range rosterwalk.AllRows(push) {
		if row.GetWorkspace().GetWorkspace().GetId() == string(ws) {
			return row
		}
	}
	t.Fatalf("the push carries no row for %s", ws)
	return nil
}

func TestAnEdgeAboutOneWorkspaceRendersOnlyThatWorkspace(t *testing.T) {
	cases := []struct {
		name string
		// act delivers the edge about ws0000.
		act func(r sidebar.Resolver)
	}{
		{name: "a link edge", act: func(r sidebar.Resolver) { r.OnLink("ws0000", shimclient.LinkConnected) }},
		{name: "a live-work edge", act: func(r sidebar.Resolver) {
			r.OnLiveWorkChanged("ws0000", sidebar.LiveWorkSet{Agents: []*conversationv1.AgentId{{Value: "agent-1"}}})
		}},
		{name: "the footer's status edge", act: func(r sidebar.Resolver) { r.FooterStatusChanged("ws0000") }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			surfaces := newCountedSurfaces()
			r := newResolverOn(t, surfaces)
			r.SetRegistry(registry(realisticRegistry()...))
			r.Settle()
			mark, resolvesBefore := len(surfaces.Records()), surfaces.resolves()

			// Act
			tc.act(r)
			r.Settle()

			// Assert
			cost := rendersSince(t, surfaces.Records(), mark)
			if cost.armsDerived > cost.renders || cost.rowsBuilt > cost.renders {
				t.Fatalf("%d renders derived %d arms and built %d rows, want at most one of each per render", cost.renders, cost.armsDerived, cost.rowsBuilt)
			}
			if got := surfaces.resolves() - resolvesBefore; got != 0 {
				t.Fatalf("the edge resolved %d log sinks, want none: every sink was resolved at the first render", got)
			}
		})
	}
}

func TestAnEdgeKeepsTheLastPushsClosedList(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)
	r.SetRegistry(registry(realisticRegistry()...))
	before := latestPush(t, r)

	// Act
	r.OnLink("ws0000", shimclient.LinkConnected)
	r.Settle()

	// Assert
	after := latestPush(t, r)
	if after == before {
		t.Fatal("the link edge published no new push")
	}
	if after.Closed != before.Closed {
		t.Fatal("the push carries a new closed list though no closed row changed, want the last push's own")
	}
}

func TestARegistryInstallRendersWhatItChanged(t *testing.T) {
	parent := workspace("parent", "parent")
	kid := workspace("kid", "kid")
	kid.Parent = &parent.ID
	others := realisticRegistry()
	base := append([]wsm.Workspace{parent, kid}, others...)
	cases := []struct {
		name string
		// change edits a copy of the base registry.
		change func(reg *sidebar.Registry)
		// wantArms is the number of arms the install derives.
		wantArms int
		// check reads the push the install published.
		check func(t *testing.T, push *frontendv1.RosterPush)
	}{
		{name: "an unchanged registry derives nothing", change: func(*sidebar.Registry) {}, wantArms: 0},
		{name: "a renamed workspace derives its own arm alone", change: func(reg *sidebar.Registry) {
			reg.Workspaces[2].Name = "renamed"
		}, wantArms: 1, check: func(t *testing.T, push *frontendv1.RosterPush) {
			if got := rowOf(t, push, others[0].ID).GetName().GetText(); got != "renamed" {
				t.Fatalf("the renamed row reads %q, want renamed", got)
			}
		}},
		{name: "a closed child derives its parent too", change: func(reg *sidebar.Registry) {
			reg.Workspaces[1].Closed = true
		}, wantArms: 2, check: func(t *testing.T, push *frontendv1.RosterPush) {
			children := rowOf(t, push, parent.ID).GetDetail().GetChildren()
			if len(children) != 1 || children[0].GetReceded() == nil {
				t.Fatalf("the parent's detail lists %v, want its one child receded", children)
			}
		}},
		{name: "a session record derives its workspace and its parent", change: func(reg *sidebar.Registry) {
			reg.Sessions = append(reg.Sessions, wsm.Session{Workspace: kid.ID})
		}, wantArms: 2},
		{name: "a repository change derives every arm", change: func(reg *sidebar.Registry) {
			reg.Repositories[0].Name = "renamed-repository"
		}, wantArms: len(base)},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			r, surfaces := newResolver(t)
			r.SetRegistry(registry(base...))
			next := registry(append([]wsm.Workspace(nil), base...)...)
			next.Repositories = append([]wsm.Repository(nil), next.Repositories...)
			tc.change(&next)
			mark := len(surfaces.Records())

			// Act
			r.SetRegistry(next)
			r.Settle()

			// Assert
			if got := rendersSince(t, surfaces.Records(), mark).armsDerived; got != tc.wantArms {
				t.Fatalf("the install derived %d arms, want %d", got, tc.wantArms)
			}
			if tc.check != nil {
				tc.check(t, latestPush(t, r))
			}
		})
	}
}

func TestAChangeAboutNoWorkspaceDerivesEveryArm(t *testing.T) {
	// Arrange
	r, surfaces := newResolver(t)
	all := realisticRegistry()
	r.SetRegistry(registry(all...))
	mark := len(surfaces.Records())

	// Act
	r.SetRepoDivergence(repo.ID, repodivergence.NoUpstream{Branch: "main"})
	r.Settle()

	// Assert
	if got := rendersSince(t, surfaces.Records(), mark).armsDerived; got != len(all) {
		t.Fatalf("the change derived %d arms, want every one of %d", got, len(all))
	}
}

func TestASelectionRedrawsTheRowsItMoves(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)
	all := realisticRegistry()
	r.SetRegistry(registry(all...))

	// Act
	r.SetSelected(all[1].ID)
	r.Settle()
	r.SetSelected(all[2].ID)
	r.Settle()

	// Assert
	push := latestPush(t, r)
	if rowOf(t, push, all[1].ID).GetCurrent().GetCurrent() {
		t.Fatal("the row the selection left is still drawn current")
	}
	if !rowOf(t, push, all[2].ID).GetCurrent().GetCurrent() {
		t.Fatal("the selected row is not drawn current")
	}
}

func TestAnArmThatReadsTheClockIsDerivedOnEveryRender(t *testing.T) {
	// Arrange
	r, surfaces := newResolver(t)
	hidden := workspace("hidden", "hidden")
	hidden.Hidden = true
	hidden.HiddenAt = at(0)
	gated := uint64(1)
	hidden.CacheLapse = &wsm.CacheLapse{LapsesAt: epoch, GatedTokens: &gated}
	r.SetRegistry(registry(hidden, workspace("other", "other")))
	mark := len(surfaces.Records())

	// Act
	r.OnLink("other", shimclient.LinkConnected)
	r.Settle()

	// Assert
	cost := rendersSince(t, surfaces.Records(), mark)
	if cost.armsDerived != 2*cost.renders {
		t.Fatalf("%d renders derived %d arms, want the edge's own and the clock-read one on each", cost.renders, cost.armsDerived)
	}
}

func TestARowsLogSinkIsResolvedAgainOnlyForAnOpenWorkspaceThatFellBack(t *testing.T) {
	cases := []struct {
		name   string
		closed bool
		// again is whether the second edge resolves the sink again.
		again bool
	}{
		{name: "an open workspace's fallback is resolved again", closed: false, again: true},
		{name: "a closed workspace's fallback is kept", closed: true, again: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			surfaces := newCountedSurfaces()
			r := newResolverOn(t, surfaces)
			gone := workspace("gone", "gone")
			gone.Closed = tc.closed
			surfaces.refuse(gone.Dir)
			r.SetRegistry(registry(gone))
			r.OnLink(gone.ID, shimclient.LinkConnected)
			r.Settle()
			before := surfaces.resolves()

			// Act
			r.OnLink(gone.ID, shimclient.LinkDead)
			r.Settle()

			// Assert
			if got := surfaces.resolves() - before; (got > 0) != tc.again {
				t.Fatalf("the edge resolved %d sinks, want resolving again %v", got, tc.again)
			}
		})
	}
}
