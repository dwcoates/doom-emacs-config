package workspace

import (
	"context"
	"errors"
	"slices"
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
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
				members: func(ws wsm.Workspace) bool { return ws.ID != "other" },
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
	shared := sourcescan.Count(t, "v.foldTabs(ctx, log, foldTabsSpec{")
	byHand := sourcescan.Count(t, "v.deps.StandDowns.StandDownWithheld(ws.ID)")

	// Assert.
	if shared != 2 || byHand != 1 {
		t.Fatalf("foldTabs calls = %d, stand-down loops = %d; want every tab-hiding fold through foldTabs", shared, byHand)
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
	shared := sourcescan.Count(t, "members: wsm.")
	byHand := sourcescan.Count(t, "func(ws wsm.Workspace) bool { return ws.Repo ==") +
		sourcescan.Count(t, "func(ws wsm.Workspace) bool { return ws.Subsection")

	// Assert
	if shared != 2 || byHand != 0 {
		t.Fatalf("shared memberships = %d, hand-rolled = %d; want 2 and 0", shared, byHand)
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
				members: wsm.RepositoryMembers("repo-1"),
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
