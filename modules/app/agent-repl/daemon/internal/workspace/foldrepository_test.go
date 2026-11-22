package workspace

import (
	"context"
	"errors"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/sourcescan"
	"agent-repld/internal/wsm"
)

func TestFoldRepositoryRecordsTheFoldAndRepublishesTheRoster(t *testing.T) {
	tests := []struct {
		name   string
		folded bool
	}{{"collapse", true}, {"expand", false}}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.db.repositories = append(f.db.repositories, wsm.Repository{ID: "repo-1", Folded: !tt.folded})

			// Act
			err := f.verbs.FoldRepository(context.Background(), "repo-1", tt.folded, testRequester)

			// Assert
			if err != nil {
				t.Fatalf("FoldRepository: %v", err)
			}
			if f.db.repositories[0].Folded != tt.folded {
				t.Fatalf("recorded fold = %v, want %v", f.db.repositories[0].Folded, tt.folded)
			}
			if len(f.sidebar.registries) != 1 {
				t.Fatalf("roster republications = %d, want exactly one", len(f.sidebar.registries))
			}
		})
	}
}

func TestFoldRepositoryRefusesAnUnknownRepository(t *testing.T) {
	// Arrange
	f := newFixture(t)

	// Act
	err := f.verbs.FoldRepository(context.Background(), "absent", true, testRequester)

	// Assert
	asRefusal(t, err, ArmUnknownRepository)
	if len(f.sidebar.registries) != 0 {
		t.Fatalf("roster republications = %d, want none for a refused fold", len(f.sidebar.registries))
	}
}

func TestFoldRepositorySurfacesAFailedWriteAtError(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.db.repositories = append(f.db.repositories, wsm.Repository{ID: "repo-1"})
	f.db.setFoldedErr = errors.New("disk I/O error")

	// Act
	err := f.verbs.FoldRepository(context.Background(), "repo-1", true, testRequester)

	// Assert
	if err == nil {
		t.Fatal("a failed fold write was answered as success")
	}
	if _, refused := AsRefusal(err); refused {
		t.Fatalf("a failed write was answered as a refusal: %v", err)
	}
	found := false
	for _, r := range f.log.logger.Records() {
		if r.Level == dlog.LevelError && r.Operation == opFoldRepository {
			found = true
		}
	}
	if !found {
		t.Fatal("the failed write was not recorded at error")
	}
	if len(f.sidebar.registries) != 0 {
		t.Fatalf("roster republications = %d, want none for a failed fold", len(f.sidebar.registries))
	}
}

// inRepo files a fixture workspace under a repository.
func inRepo(f *fixture, ws ids.WorkspaceID, repo ids.RepoID, closed bool) {
	record := f.db.workspaces[ws]
	record.Repo, record.Closed = repo, closed
	f.db.workspaces[ws] = record
}

func TestCollapsingARepositoryStandsItsOpenWorkspacesDown(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.db.repositories = append(f.db.repositories, wsm.Repository{ID: "repo-1"})
	f.workspace("w1", t.TempDir())
	inRepo(f, "w1", "repo-1", false)

	// Act
	if err := f.verbs.FoldRepository(context.Background(), "repo-1", true, testRequester); err != nil {
		t.Fatalf("FoldRepository: %v", err)
	}

	// Assert
	if !slices.Equal(f.standDowns, []ids.WorkspaceID{"w1"}) {
		t.Fatalf("stand-downs = %v, want the section's workspace", f.standDowns)
	}
}

func TestCollapsingARepositoryLeavesAnotherRepositorysWorkspaces(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.db.repositories = append(f.db.repositories, wsm.Repository{ID: "repo-1"})
	f.workspace("w1", t.TempDir())
	inRepo(f, "w1", "repo-2", false)

	// Act
	if err := f.verbs.FoldRepository(context.Background(), "repo-1", true, testRequester); err != nil {
		t.Fatalf("FoldRepository: %v", err)
	}

	// Assert
	if len(f.standDowns) != 0 {
		t.Fatalf("stand-downs = %v, want none outside the section", f.standDowns)
	}
}

// TestCollapsingARepositoryFoldsItsWorkspacesCrossRepositoryChildren pins the
// fold's membership to the tab bar's: a child of another repository is drawn
// under its parent, in the parent's section, so that section's collapse folds
// it and the child's own repository's does not.
func TestCollapsingARepositoryFoldsItsWorkspacesCrossRepositoryChildren(t *testing.T) {
	tests := []struct {
		name string
		repo ids.RepoID
		want []ids.WorkspaceID
	}{
		{name: "the parent's repository folds the child with its parent", repo: "repo-1", want: []ids.WorkspaceID{"c1", "p1"}},
		{name: "the child's own repository folds nothing", repo: "repo-2", want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.db.repositories = append(f.db.repositories, wsm.Repository{ID: "repo-1"}, wsm.Repository{ID: "repo-2"})
			f.workspace("p1", t.TempDir())
			inRepo(f, "p1", "repo-1", false)
			f.workspace("c1", t.TempDir())
			inRepo(f, "c1", "repo-2", false)
			child := f.db.workspaces["c1"]
			parent := ids.WorkspaceID("p1")
			child.Parent = &parent
			f.db.workspaces["c1"] = child

			// Act
			if err := f.verbs.FoldRepository(context.Background(), tt.repo, true, testRequester); err != nil {
				t.Fatalf("FoldRepository: %v", err)
			}

			// Assert
			got := slices.Clone(f.standDowns)
			slices.Sort(got)
			if !slices.Equal(got, tt.want) {
				t.Fatalf("stand-downs = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestCollapsingARepositoryLeavesItsClosedWorkspaces(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.db.repositories = append(f.db.repositories, wsm.Repository{ID: "repo-1"})
	f.workspace("w1", t.TempDir())
	inRepo(f, "w1", "repo-1", true)

	// Act
	if err := f.verbs.FoldRepository(context.Background(), "repo-1", true, testRequester); err != nil {
		t.Fatalf("FoldRepository: %v", err)
	}

	// Assert
	if len(f.standDowns) != 0 {
		t.Fatalf("stand-downs = %v, want none for a closed workspace", f.standDowns)
	}
}

func TestExpandingARepositoryStandsNothingDown(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.db.repositories = append(f.db.repositories, wsm.Repository{ID: "repo-1", Folded: true})
	f.workspace("w1", t.TempDir())
	inRepo(f, "w1", "repo-1", false)

	// Act
	if err := f.verbs.FoldRepository(context.Background(), "repo-1", false, testRequester); err != nil {
		t.Fatalf("FoldRepository: %v", err)
	}

	// Assert
	if len(f.standDowns) != 0 {
		t.Fatalf("stand-downs = %v, want none for an expand", f.standDowns)
	}
}

func TestCollapsingARepositoryWhoseWorkspacesCannotBeListedFailsLoudly(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.db.repositories = append(f.db.repositories, wsm.Repository{ID: "repo-1"})
	f.db.listWorkspacesErr = errors.New("disk I/O error")

	// Act
	err := f.verbs.FoldRepository(context.Background(), "repo-1", true, testRequester)

	// Assert
	if err == nil {
		t.Fatal("a collapse whose workspaces could not be listed was answered as success")
	}
	found := false
	for _, r := range f.log.logger.Records() {
		if r.Level == dlog.LevelError && r.Operation == opFoldRepository && strings.Contains(r.Message, "the collapse was not recorded") {
			found = true
		}
	}
	if !found {
		t.Fatal("the failed listing was not recorded at error")
	}
	if f.db.repositories[0].Folded || len(f.sidebar.registries) != 0 {
		t.Fatalf("folded = %v, republications = %d; want nothing recorded when the workspaces cannot be listed",
			f.db.repositories[0].Folded, len(f.sidebar.registries))
	}
}

// current marks a fixture workspace as the user's selection.
func current(f *fixture, ws ids.WorkspaceID) {
	record := f.db.workspaces[ws]
	record.Current = true
	f.db.workspaces[ws] = record
	f.db.current = &ws
}

func TestCollapsingARepositoryHoldingTheCurrentWorkspaceIsRefused(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.db.repositories = append(f.db.repositories, wsm.Repository{ID: "repo-1"})
	f.workspace("w1", t.TempDir())
	inRepo(f, "w1", "repo-1", false)
	current(f, "w1")

	// Act
	err := f.verbs.FoldRepository(context.Background(), "repo-1", true, testRequester)

	// Assert
	asRefusal(t, err, ArmHoldsCurrent)
	if f.db.repositories[0].Folded || len(f.sidebar.registries) != 0 || len(f.standDowns) != 0 {
		t.Fatalf("folded = %v, republications = %d, stand-downs = %v; want nothing done for a refused collapse",
			f.db.repositories[0].Folded, len(f.sidebar.registries), f.standDowns)
	}
}

func TestARefusedCollapseIsRecordedWithTheCurrentWorkspace(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.db.repositories = append(f.db.repositories, wsm.Repository{ID: "repo-1"})
	f.workspace("w1", t.TempDir())
	inRepo(f, "w1", "repo-1", false)
	current(f, "w1")

	// Act
	_ = f.verbs.FoldRepository(context.Background(), "repo-1", true, testRequester)

	// Assert
	found := false
	for _, r := range f.log.logger.Records() {
		if r.Level == dlog.LevelInfo && r.Operation == opRefusal && r.Context["arm"] == ArmHoldsCurrent &&
			strings.Contains(r.Message, `"w1"`) && r.Context["repo_id"] == "repo-1" {
			found = true
		}
	}
	if !found {
		t.Fatal("the refusal was not recorded at INFO naming the repository and the current workspace")
	}
}

func TestCollapsingASubsectionHoldingTheCurrentWorkspaceIsRefused(t *testing.T) {
	// Arrange
	f := newFixture(t)
	sub := ids.SubsectionID("sub-1")
	f.db.subsections = append(f.db.subsections, wsm.Subsection{ID: sub, Repo: "repo-1", Name: "later", Place: 1})
	inside := f.workspace("inside", t.TempDir())
	inside.Subsection = &sub
	inside.Current = true
	f.db.with(inside)

	// Act
	err := f.verbs.FoldSubsection(context.Background(), sub, true, testRequester)

	// Assert
	asRefusal(t, err, ArmHoldsCurrent)
	if f.db.subsections[0].Folded || len(f.standDowns) != 0 {
		t.Fatalf("folded = %v, stand-downs = %v; want nothing done for a refused collapse", f.db.subsections[0].Folded, f.standDowns)
	}
}

func TestCollapsingARepositoryWhoseCurrentWorkspaceIsElsewhereSucceeds(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.db.repositories = append(f.db.repositories, wsm.Repository{ID: "repo-1"})
	f.workspace("w1", t.TempDir())
	inRepo(f, "w1", "repo-1", false)
	f.workspace("w2", t.TempDir())
	inRepo(f, "w2", "repo-2", false)
	current(f, "w2")

	// Act
	err := f.verbs.FoldRepository(context.Background(), "repo-1", true, testRequester)

	// Assert
	if err != nil {
		t.Fatalf("FoldRepository: %v", err)
	}
	if !f.db.repositories[0].Folded || !slices.Equal(f.standDowns, []ids.WorkspaceID{"w1"}) {
		t.Fatalf("folded = %v, stand-downs = %v; want the collapse recorded and only w1 stood down", f.db.repositories[0].Folded, f.standDowns)
	}
}

func TestExpandingARepositoryHoldingTheCurrentWorkspaceSucceeds(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.db.repositories = append(f.db.repositories, wsm.Repository{ID: "repo-1", Folded: true})
	f.workspace("w1", t.TempDir())
	inRepo(f, "w1", "repo-1", false)
	current(f, "w1")

	// Act
	err := f.verbs.FoldRepository(context.Background(), "repo-1", false, testRequester)

	// Assert
	if err != nil || f.db.repositories[0].Folded {
		t.Fatalf("FoldRepository = %v, folded = %v; want the expand recorded", err, f.db.repositories[0].Folded)
	}
}

func TestFoldTabs(t *testing.T) {
	errWrite := errors.New("disk I/O error")
	tests := []struct {
		name          string
		folded        bool
		writeErr      error
		unknown       error
		wantErr       error
		wantRepublish int
		wantStandDown []ids.WorkspaceID
	}{
		{name: "a collapse stands down the open workspaces it holds", folded: true, wantRepublish: 1, wantStandDown: []ids.WorkspaceID{"held"}},
		{name: "an expand stands nothing down", folded: false, wantRepublish: 1},
		{name: "an unknown target answers its refusal", folded: true, writeErr: wsm.ErrNotFound, unknown: errUnknownTarget, wantErr: errUnknownTarget},
		{name: "a failed write is surfaced", folded: true, writeErr: errWrite, wantErr: errWrite},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.workspace("held", t.TempDir())
			closed := f.workspace("closed-held", t.TempDir())
			closed.Closed = true
			f.db.with(closed)
			f.workspace("other", t.TempDir())
			log := f.log.logger

			// Act
			err := f.verbs.(*verbs).foldTabs(context.Background(), log, foldTabsSpec{
				op: "daemon.workspace.test", what: "thing", target: "t1", folded: tt.folded,
				write: func() error { return tt.writeErr },
				unknown: func(error) error {
					if tt.unknown == nil {
						return nil
					}
					return tt.unknown
				},
				members: func([]wsm.Workspace) wsm.FoldMembers { return func(ws wsm.Workspace) bool { return ws.ID != "other" } },
			})

			// Assert
			if !errors.Is(err, tt.wantErr) || (tt.wantErr == nil && err != nil) {
				t.Fatalf("foldTabs = %v, want %v", err, tt.wantErr)
			}
			if len(f.sidebar.registries) != tt.wantRepublish {
				t.Fatalf("roster republications = %d, want %d", len(f.sidebar.registries), tt.wantRepublish)
			}
			if !slices.Equal(f.standDowns, tt.wantStandDown) {
				t.Fatalf("stand-downs = %v, want %v", f.standDowns, tt.wantStandDown)
			}
			if failed := loggedAtError(f, "daemon.workspace.test"); failed != (tt.wantErr == errWrite) {
				t.Fatalf("recorded at ERROR = %v, want it only for the failed write", failed)
			}
		})
	}
}

// errUnknownTarget is a fold's refusal of a target nothing carries.
var errUnknownTarget = errors.New("no such target")

func TestEveryFoldThatHidesTabsGoesThroughFoldTabs(t *testing.T) {
	// Act.
	specs := sourcescan.Count(t, "foldTabsSpec{")
	calls := sourcescan.Count(t, "v.foldTabs(ctx, log, ")
	byHand := sourcescan.Count(t, "v.deps.StandDowns.StandDownFolded(ws.ID)")

	// Assert.
	if specs != 3 || calls != 3 || byHand != 1 {
		t.Fatalf("fold specs = %d, foldTabs calls = %d, stand-down loops = %d; want the repository's, the subsection's and the children's spec, each fold through foldTabs",
			specs, calls, byHand)
	}
}

func TestFoldSubsectionRecordsTheFoldAndStandsDownOnlyItsOwnWorkspaces(t *testing.T) {
	// Arrange
	f := newFixture(t)
	sub := ids.SubsectionID("sub-1")
	f.db.subsections = append(f.db.subsections, wsm.Subsection{ID: sub, Repo: "repo-1", Name: "later", Place: 1})
	inside := f.workspace("inside", t.TempDir())
	inside.Subsection = &sub
	f.db.with(inside)
	f.workspace("outside", t.TempDir())

	// Act
	err := f.verbs.FoldSubsection(context.Background(), sub, true, testRequester)

	// Assert
	if err != nil {
		t.Fatalf("FoldSubsection: %v", err)
	}
	if !f.db.subsections[0].Folded {
		t.Fatal("the subsection's fold was not recorded")
	}
	if !slices.Equal(f.standDowns, []ids.WorkspaceID{"inside"}) {
		t.Fatalf("stand-downs = %v, want only the subsection's workspace", f.standDowns)
	}
}

func TestFoldSubsectionRefusesAnUnknownSubsection(t *testing.T) {
	// Arrange
	f := newFixture(t)

	// Act
	err := f.verbs.FoldSubsection(context.Background(), "absent", true, testRequester)

	// Assert
	refusal := asRefusal(t, err, ArmUnknownSubsection)
	if ref, ok := refusal.Fields["subsection"].(*agentreplv1.SubsectionRef); !ok || ref.GetId() != "absent" {
		t.Fatalf("refusal fields = %v, want the subsection named", refusal.Fields)
	}
	if len(f.sidebar.registries) != 0 {
		t.Fatalf("roster republications = %d, want none for a refused fold", len(f.sidebar.registries))
	}
}

// TestFoldTabsMembershipIsTheSharedOne pins every fold that hides tabs to
// wsm.FoldMembers: a hand-rolled membership closure would let the sessions a
// collapse stands down drift from the ones the roster and the refusal count.
func TestFoldTabsMembershipIsTheSharedOne(t *testing.T) {
	// Act
	shared := sourcescan.Count(t, "return wsm.RepositoryMembers(") +
		sourcescan.Count(t, "return wsm.SubsectionMembers(") + sourcescan.Count(t, "return wsm.DescendantMembers(")
	byHand := sourcescan.Count(t, "func(ws wsm.Workspace) bool { return ws.Repo ==") +
		sourcescan.Count(t, "func(ws wsm.Workspace) bool { return ws.Subsection")

	// Assert
	if shared != 3 || byHand != 0 {
		t.Fatalf("shared memberships = %d, hand-rolled = %d; want 3 and 0", shared, byHand)
	}
}

func TestACollapseIsWrittenUnderTheSelectionLock(t *testing.T) {
	tests := []struct {
		name       string
		folded     bool
		wantLocked bool
	}{
		{name: "a collapse holds the lock a select takes", folded: true, wantLocked: true},
		{name: "an expand does not", folded: false, wantLocked: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			v := f.verbs.(*verbs)
			locked := false

			// Act
			err := v.foldTabs(context.Background(), f.log.logger, foldTabsSpec{
				op: "daemon.workspace.test", what: "thing", target: "t1", folded: tt.folded,
				write: func() error {
					if v.selection.TryLock() {
						v.selection.Unlock()
					} else {
						locked = true
					}
					return nil
				},
				unknown: func(error) error { return nil },
				members: func(all []wsm.Workspace) wsm.FoldMembers {
					return wsm.RepositoryMembers(wsm.SectionRepos(all), "repo-1")
				},
			})

			// Assert
			if err != nil || locked != tt.wantLocked {
				t.Fatalf("foldTabs = %v, locked during the write = %v; want nil, %v", err, locked, tt.wantLocked)
			}
		})
	}
}

// testRequester is the requester every fold in these tests is asked by.
var testRequester = FoldRequester{RPC: "UpdateSidebarView", RequestID: "webhost-c1-7"}

func TestCollapsingARepositorySparesWhatTheRecordSpares(t *testing.T) {
	tests := []struct {
		name  string
		spare func(ws *wsm.Workspace)
	}{
		{"a one-shot", func(ws *wsm.Workspace) { ws.OneShot = true }},
		{"a merge pending", func(ws *wsm.Workspace) { ws.MergePending = true }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.db.repositories = append(f.db.repositories, wsm.Repository{ID: "repo-1"})
			spared := f.workspace("spared", t.TempDir())
			spared.Repo = "repo-1"
			tt.spare(&spared)
			f.db.with(spared)
			f.workspace("idle", t.TempDir())
			inRepo(f, "idle", "repo-1", false)

			// Act
			if err := f.verbs.FoldRepository(context.Background(), "repo-1", true, testRequester); err != nil {
				t.Fatalf("FoldRepository: %v", err)
			}

			// Assert
			if !slices.Equal(f.standDowns, []ids.WorkspaceID{"idle"}) {
				t.Fatalf("stand-downs = %v, want only the idle ordinary workspace", f.standDowns)
			}
		})
	}
}

func TestCollapsingARepositoryRecordsWhoAskedAndWhatItHid(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.db.repositories = append(f.db.repositories, wsm.Repository{ID: "repo-1"})
	oneShot := f.workspace("one-shot", t.TempDir())
	oneShot.Repo, oneShot.OneShot = "repo-1", true
	f.db.with(oneShot)
	queued := f.workspace("queued", t.TempDir())
	queued.Repo, queued.MergePending = "repo-1", true
	f.db.with(queued)
	f.workspace("idle", t.TempDir())
	inRepo(f, "idle", "repo-1", false)

	// Act
	if err := f.verbs.FoldRepository(context.Background(), "repo-1", true, testRequester); err != nil {
		t.Fatalf("FoldRepository: %v", err)
	}

	// Assert
	rec, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Operation: opFoldRepository,
		Message: "the collapsed repository's open workspaces are hidden; the spared keep running, the rest stand down once the fold's delay is over and they are free"})
	if !ok {
		t.Fatal("the collapse recorded nothing of what it hid")
	}
	if rec.Context["requested_by_rpc"] != testRequester.RPC || rec.Context["request_id"] != testRequester.RequestID {
		t.Fatalf("record names requester %v %v, want %+v", rec.Context["requested_by_rpc"], rec.Context["request_id"], testRequester)
	}
	hidden, _ := rec.Context["hidden"].([]string)
	slices.Sort(hidden)
	if !slices.Equal(hidden, []string{"idle", "one-shot", "queued"}) {
		t.Fatalf("hidden = %v, want every open workspace of the repository", rec.Context["hidden"])
	}
	spared, _ := rec.Context["spared"].(map[string]string)
	if spared["one-shot"] != string(wsm.FoldSpareOneShot) || spared["queued"] != string(wsm.FoldSpareMergePending) || len(spared) != 2 {
		t.Fatalf("spared = %v, want the one-shot and the queued workspace with their reasons", rec.Context["spared"])
	}
	if standing, _ := rec.Context["standing_down"].([]string); !slices.Equal(standing, []string{"idle"}) {
		t.Fatalf("standing_down = %v, want the idle workspace", rec.Context["standing_down"])
	}
}

// childFamily registers parent p1 with child c1 (whose own child is g1) and an
// unrelated workspace other.
func childFamily(t *testing.T, f *fixture) {
	f.workspace("p1", t.TempDir())
	f.workspace("c1", t.TempDir())
	f.workspace("g1", t.TempDir())
	f.workspace("other", t.TempDir())
	for child, parent := range map[ids.WorkspaceID]ids.WorkspaceID{"c1": "p1", "g1": "c1"} {
		record := f.db.workspaces[child]
		parentID := parent
		record.Parent = &parentID
		f.db.workspaces[child] = record
	}
}

func TestFoldChildrenCollapseRecordsTheFoldAndStandsDownOnlyTheDescendants(t *testing.T) {
	// Arrange
	f := newFixture(t)
	childFamily(t, f)

	// Act
	err := f.verbs.FoldChildren(context.Background(), "p1", true, testRequester)

	// Assert
	if err != nil {
		t.Fatalf("FoldChildren: %v", err)
	}
	if !f.db.workspaces["p1"].ChildrenFolded {
		t.Fatal("the children's fold was not recorded")
	}
	got := slices.Clone(f.standDowns)
	slices.Sort(got)
	if !slices.Equal(got, []ids.WorkspaceID{"c1", "g1"}) {
		t.Fatalf("stand-downs = %v, want the parent's descendants and neither the parent nor an unrelated workspace", got)
	}
	if len(f.sidebar.registries) != 1 {
		t.Fatalf("roster republications = %d, want 1", len(f.sidebar.registries))
	}
}

func TestFoldChildrenExpandRecordsTheFoldAndStandsNothingDown(t *testing.T) {
	// Arrange
	f := newFixture(t)
	childFamily(t, f)
	p := f.db.workspaces["p1"]
	p.ChildrenFolded = true
	f.db.workspaces["p1"] = p

	// Act
	err := f.verbs.FoldChildren(context.Background(), "p1", false, testRequester)

	// Assert
	if err != nil {
		t.Fatalf("FoldChildren: %v", err)
	}
	if f.db.workspaces["p1"].ChildrenFolded || len(f.standDowns) != 0 {
		t.Fatalf("folded = %v, stand-downs = %v; want the expand recorded and nothing stood down", f.db.workspaces["p1"].ChildrenFolded, f.standDowns)
	}
}

func TestFoldChildrenCollapseOverTheCurrentDescendantSelectsTheParentAndFolds(t *testing.T) {
	// Arrange
	f := newFixture(t)
	childFamily(t, f)
	current(f, "g1")

	// Act
	err := f.verbs.FoldChildren(context.Background(), "p1", true, testRequester)

	// Assert
	if err != nil {
		t.Fatalf("FoldChildren: %v", err)
	}
	if !f.db.workspaces["p1"].Current || f.db.workspaces["g1"].Current {
		t.Fatalf("p1 current = %v, g1 current = %v; want the parent selected", f.db.workspaces["p1"].Current, f.db.workspaces["g1"].Current)
	}
	if !f.db.workspaces["p1"].ChildrenFolded {
		t.Fatal("the children's fold was not recorded")
	}
}

func TestFoldChildrenCollapseOverAnUnrelatedCurrentWorkspaceSelectsNothing(t *testing.T) {
	// Arrange
	f := newFixture(t)
	childFamily(t, f)
	current(f, "other")

	// Act
	err := f.verbs.FoldChildren(context.Background(), "p1", true, testRequester)

	// Assert
	if err != nil || !f.db.workspaces["other"].Current || f.db.workspaces["p1"].Current {
		t.Fatalf("err = %v, other current = %v, p1 current = %v; want the selection untouched", err, f.db.workspaces["other"].Current, f.db.workspaces["p1"].Current)
	}
}

func TestFoldChildrenCollapseOfTheCurrentWorkspacesOwnChildrenSucceeds(t *testing.T) {
	// Arrange
	f := newFixture(t)
	childFamily(t, f)
	current(f, "p1")

	// Act
	err := f.verbs.FoldChildren(context.Background(), "p1", true, testRequester)

	// Assert
	if err != nil || !f.db.workspaces["p1"].ChildrenFolded {
		t.Fatalf("FoldChildren = %v, folded = %v; want the current workspace's own children folded", err, f.db.workspaces["p1"].ChildrenFolded)
	}
}

func TestFoldChildrenRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	f := newFixture(t)

	// Act
	err := f.verbs.FoldChildren(context.Background(), "absent", true, testRequester)

	// Assert
	asRefusal(t, err, ArmUnknownWorkspace)
	if len(f.sidebar.registries) != 0 {
		t.Fatalf("roster republications = %d, want none for a refused fold", len(f.sidebar.registries))
	}
}

// landingFixture is a fixture whose current workspace "inside" sits in the
// subsection "sub-1", beside "before" (selected at 10:00) and "never"
// outside it, in registry order before, inside, never.
func landingFixture(t *testing.T) (*fixture, ids.SubsectionID) {
	t.Helper()
	f := newFixture(t)
	sub := ids.SubsectionID("sub-1")
	for i, id := range []ids.WorkspaceID{"before", "inside", "never"} {
		ws := f.workspace(id, filepath.Join(t.TempDir(), string(id)))
		ws.Place = int64(i + 1)
		if id == "inside" {
			ws.Subsection = &sub
			f.db.subsections = append(f.db.subsections, wsm.Subsection{ID: sub, Repo: ws.Repo, Name: "later", Place: 1})
		}
		f.db.with(ws)
	}
	selected := time.Date(2026, 11, 9, 10, 0, 0, 0, time.UTC)
	before := f.db.workspaces["before"]
	before.LastSelectedAt = &selected
	f.db.workspaces["before"] = before
	current(f, "inside")
	return f, sub
}

func TestCollapsingASubsectionHoldingTheCurrentWorkspaceLandsOnTheWorkspaceSelectedBefore(t *testing.T) {
	// Arrange
	f, sub := landingFixture(t)

	// Act
	err := f.verbs.FoldSubsection(context.Background(), sub, true, testRequester)

	// Assert
	if err != nil {
		t.Fatalf("FoldSubsection: %v", err)
	}
	if f.db.current == nil || *f.db.current != "before" {
		t.Fatalf("current = %v, want before, the open workspace selected most recently", f.db.current)
	}
	if !f.db.subsections[0].Folded || !slices.Equal(f.standDowns, []ids.WorkspaceID{"inside"}) {
		t.Fatalf("folded = %v, stand-downs = %v; want the collapse recorded and the section's workspace stood down", f.db.subsections[0].Folded, f.standDowns)
	}
}

func TestACollapseThatLandsTheSelectionRecordsTheLandingAtInfo(t *testing.T) {
	// Arrange
	f, sub := landingFixture(t)

	// Act
	_ = f.verbs.FoldSubsection(context.Background(), sub, true, testRequester)

	// Assert
	record, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opFoldSubsection,
		Message: "the collapse moved the selection off the folded subsection to the workspace selected before"})
	if !ok || record.Context["from"] != "inside" || record.Context["by"] != string(sidebar.LandedByRecency) {
		t.Fatalf("no info record names the landing off the folded subsection; records = %+v", f.log.logger.Records())
	}
}

func TestACollapseWhoseLandingIsNotServedHereIsRefusedAndWritesNothing(t *testing.T) {
	// Arrange
	f, sub := landingFixture(t)
	f.db.current = nil
	current(f, "inside")
	f.db.current = nil

	// Act
	err := f.verbs.FoldSubsection(context.Background(), sub, true, testRequester)

	// Assert
	if err == nil {
		t.Fatal("a collapse whose selection moved under the lock was answered as success")
	}
	if f.db.subsections[0].Folded {
		t.Fatal("the collapse was recorded over a selection it could not move")
	}
}

func TestSelectingIntoAFoldLeavesItCollapsedAndStandsNothingDown(t *testing.T) {
	tests := []struct {
		name  string
		build func(*fixture)
		folds func(*fixture) bool
	}{
		{
			name: "a subsection",
			build: func(f *fixture) {
				sub := ids.SubsectionID("sub-1")
				f.db.subsections = append(f.db.subsections, wsm.Subsection{ID: sub, Name: "later", Place: 1, Folded: true})
				ws := f.db.workspaces["target"]
				ws.Subsection = &sub
				f.db.with(ws)
			},
			folds: func(f *fixture) bool { return f.db.subsections[0].Folded },
		},
		{
			name: "a repository",
			build: func(f *fixture) {
				inRepo(f, "target", "repo-1", false)
				for i := range f.db.repositories {
					if f.db.repositories[i].ID == "repo-1" {
						f.db.repositories[i].Folded = true
					}
				}
			},
			folds: func(f *fixture) bool {
				for _, repo := range f.db.repositories {
					if repo.ID == "repo-1" {
						return repo.Folded
					}
				}
				return false
			},
		},
		{
			name: "a parent's children",
			build: func(f *fixture) {
				parent := f.workspace("parent", t.TempDir())
				parent.ChildrenFolded = true
				f.db.with(parent)
				ws := f.db.workspaces["target"]
				ws.Parent = &parent.ID
				f.db.with(ws)
			},
			folds: func(f *fixture) bool { return f.db.workspaces["parent"].ChildrenFolded },
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.workspace("outside", t.TempDir())
			f.workspace("target", t.TempDir())
			tt.build(f)
			current(f, "outside")

			// Act
			err := f.verbs.Select(context.Background(), "target", Switch{History: RecordSelection})

			// Assert
			if err != nil {
				t.Fatalf("Select: %v", err)
			}
			if !tt.folds(f) {
				t.Error("the selection opened the fold")
			}
			if len(f.standDowns) != 0 {
				t.Errorf("stand-downs = %v, want none", f.standDowns)
			}
			if f.db.current == nil || *f.db.current != "target" {
				t.Errorf("current = %v, want target", f.db.current)
			}
		})
	}
}

// expandCase is one fold an expand of which the user made.
type expandCase struct {
	name   string
	kind   wsm.AttentionKind
	target string
	expand func(t *testing.T, f *fixture) error
}

func expandCases() []expandCase {
	ctx := context.Background()
	return []expandCase{
		{
			name: "a repository's section", kind: wsm.AttentionOpenedRepository, target: "repo-1",
			expand: func(t *testing.T, f *fixture) error {
				f.db.repositories = append(f.db.repositories, wsm.Repository{ID: "repo-1", Folded: true})
				return f.verbs.FoldRepository(ctx, "repo-1", false, testRequester)
			},
		},
		{
			name: "a subsection", kind: wsm.AttentionOpenedSubsection, target: "sub-1",
			expand: func(t *testing.T, f *fixture) error {
				f.db.subsections = append(f.db.subsections, wsm.Subsection{ID: "sub-1", Repo: "repo-1", Name: "later", Place: 1, Folded: true})
				return f.verbs.FoldSubsection(ctx, "sub-1", false, testRequester)
			},
		},
		{
			name: "a workspace's children", kind: wsm.AttentionOpenedChildren, target: "p1",
			expand: func(t *testing.T, f *fixture) error {
				childFamily(t, f)
				return f.verbs.FoldChildren(ctx, "p1", false, testRequester)
			},
		},
	}
}

func TestAnExpandStampsTheUsersOpenOfThatFold(t *testing.T) {
	for _, tt := range expandCases() {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)

			// Act
			err := tt.expand(t, f)

			// Assert
			if err != nil {
				t.Fatalf("expand: %v", err)
			}
			if len(f.db.userStamps) != 1 || f.db.userStamps[0].Kind != tt.kind || f.db.userStamps[0].Target != tt.target {
				t.Fatalf("stamps = %v, want one %s stamp on %s", f.db.userStamps, tt.kind, tt.target)
			}
		})
	}
}

func TestACollapseStampsNoOpen(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.db.repositories = append(f.db.repositories, wsm.Repository{ID: "repo-1"})

	// Act
	err := f.verbs.FoldRepository(context.Background(), "repo-1", true, testRequester)

	// Assert
	if err != nil {
		t.Fatalf("FoldRepository: %v", err)
	}
	if len(f.db.userStamps) != 0 {
		t.Fatalf("stamps = %v, want none for a collapse", f.db.userStamps)
	}
}

func TestAnExpandSurfacesAFailedOpenStamp(t *testing.T) {
	for _, tt := range expandCases() {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.db.stampErr = errors.New("disk I/O error")

			// Act
			err := tt.expand(t, f)

			// Assert
			if !errors.Is(err, f.db.stampErr) {
				t.Fatalf("err = %v, want the failed stamp surfaced", err)
			}
			if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError}); !ok {
				t.Fatal("the failed stamp was not recorded at error")
			}
		})
	}
}

// foldedAbove folds everything above g1 in childFamily's chain (g1 under c1
// under p1): both ancestors' children, the subsection sub-1 that p1's family
// sits in, and repository repo-1's section.
func foldedAbove(t *testing.T, f *fixture) {
	t.Helper()
	childFamily(t, f)
	for _, id := range []ids.WorkspaceID{"p1", "c1", "g1"} {
		record := f.db.workspaces[id]
		record.ChildrenFolded = id != "g1"
		if id == "p1" {
			sub := ids.SubsectionID("sub-1")
			record.Subsection = &sub
		}
		f.db.workspaces[id] = record
	}
	f.db.subsections = append(f.db.subsections, wsm.Subsection{ID: "sub-1", Repo: "repo-1", Name: "later", Place: 1, Folded: true})
	for i := range f.db.repositories {
		f.db.repositories[i].Folded = true
	}
}

func TestFoldChildrenExpandAlsoExpandsEveryFoldAboveInOneRepublish(t *testing.T) {
	// Arrange
	f := newFixture(t)
	foldedAbove(t, f)
	f.db.workspaces["g1"] = func() wsm.Workspace { g := f.db.workspaces["g1"]; g.ChildrenFolded = true; return g }()

	// Act
	err := f.verbs.FoldChildren(context.Background(), "g1", false, testRequester)

	// Assert
	if err != nil {
		t.Fatalf("FoldChildren: %v", err)
	}
	open := []bool{
		!f.db.workspaces["g1"].ChildrenFolded, !f.db.workspaces["c1"].ChildrenFolded, !f.db.workspaces["p1"].ChildrenFolded,
		!f.db.subsections[0].Folded, !f.db.repositories[0].Folded,
	}
	if !slices.Equal(open, []bool{true, true, true, true, true}) {
		t.Fatalf("expanded (g1, c1, p1, sub-1, repo-1) = %v, want all", open)
	}
	if len(f.sidebar.registries) != 1 {
		t.Fatalf("roster republications = %d, want one", len(f.sidebar.registries))
	}
}

func TestFoldChildrenExpandStampsTheUsersOpenOfEveryFoldItOpens(t *testing.T) {
	// Arrange
	f := newFixture(t)
	foldedAbove(t, f)

	// Act
	err := f.verbs.FoldChildren(context.Background(), "g1", false, testRequester)

	// Assert
	if err != nil {
		t.Fatalf("FoldChildren: %v", err)
	}
	var got []string
	for _, stamp := range f.db.userStamps {
		got = append(got, string(stamp.Kind)+":"+stamp.Target)
	}
	want := []string{"opened_children:g1", "opened_children:c1", "opened_children:p1", "opened_subsection:sub-1", "opened_repository:repo-1"}
	if !slices.Equal(got, want) {
		t.Fatalf("stamps = %v, want %v", got, want)
	}
}

func TestFoldChildrenExpandStampsNothingForAFoldAboveItThatWasOpen(t *testing.T) {
	// Arrange
	f := newFixture(t)
	childFamily(t, f)

	// Act
	err := f.verbs.FoldChildren(context.Background(), "g1", false, testRequester)

	// Assert
	if err != nil {
		t.Fatalf("FoldChildren: %v", err)
	}
	if len(f.db.userStamps) != 1 || f.db.userStamps[0].Target != "g1" {
		t.Fatalf("stamps = %v, want only g1's own", f.db.userStamps)
	}
}

func TestFoldChildrenCollapseLeavesEveryFoldAboveAsItWas(t *testing.T) {
	// Arrange
	f := newFixture(t)
	foldedAbove(t, f)
	for _, id := range []ids.WorkspaceID{"p1", "c1"} {
		record := f.db.workspaces[id]
		record.ChildrenFolded = false
		f.db.workspaces[id] = record
	}

	// Act
	err := f.verbs.FoldChildren(context.Background(), "c1", true, testRequester)

	// Assert
	if err != nil {
		t.Fatalf("FoldChildren: %v", err)
	}
	if !f.db.workspaces["c1"].ChildrenFolded || f.db.workspaces["p1"].ChildrenFolded || !f.db.subsections[0].Folded || !f.db.repositories[0].Folded {
		t.Fatalf("c1 folded = %v, p1 folded = %v, sub-1 folded = %v, repo-1 folded = %v; want only c1 collapsed",
			f.db.workspaces["c1"].ChildrenFolded, f.db.workspaces["p1"].ChildrenFolded, f.db.subsections[0].Folded, f.db.repositories[0].Folded)
	}
}

func TestFoldChildrenExpandRefusesAnUnknownWorkspaceAndWritesNothing(t *testing.T) {
	// Arrange
	f := newFixture(t)
	foldedAbove(t, f)

	// Act
	err := f.verbs.FoldChildren(context.Background(), "absent", false, testRequester)

	// Assert
	asRefusal(t, err, ArmUnknownWorkspace)
	if !f.db.workspaces["c1"].ChildrenFolded || !f.db.subsections[0].Folded || len(f.db.userStamps) != 0 || len(f.sidebar.registries) != 0 {
		t.Fatalf("an unknown workspace's expand changed state: stamps = %v, republications = %d", f.db.userStamps, len(f.sidebar.registries))
	}
}
