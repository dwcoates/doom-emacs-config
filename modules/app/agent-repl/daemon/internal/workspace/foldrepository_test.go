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
			err := f.verbs.FoldRepository(context.Background(), "repo-1", tt.folded)

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
	err := f.verbs.FoldRepository(context.Background(), "absent", true)

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
	err := f.verbs.FoldRepository(context.Background(), "repo-1", true)

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
	if err := f.verbs.FoldRepository(context.Background(), "repo-1", true); err != nil {
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
	if err := f.verbs.FoldRepository(context.Background(), "repo-1", true); err != nil {
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
	if err := f.verbs.FoldRepository(context.Background(), "repo-1", true); err != nil {
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
	if err := f.verbs.FoldRepository(context.Background(), "repo-1", false); err != nil {
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
	err := f.verbs.FoldRepository(context.Background(), "repo-1", true)

	// Assert
	if err == nil {
		t.Fatal("a collapse whose workspaces could not be listed was answered as success")
	}
	found := false
	for _, r := range f.log.logger.Records() {
		if r.Level == dlog.LevelError && r.Operation == opFoldRepository && strings.Contains(r.Message, "could not list the collapsed") {
			found = true
		}
	}
	if !found {
		t.Fatal("the failed listing was not recorded at error")
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
				holds: func(ws wsm.Workspace) bool { return ws.ID != "other" },
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
	err := f.verbs.FoldSubsection(context.Background(), sub, true)

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
	err := f.verbs.FoldSubsection(context.Background(), "absent", true)

	// Assert
	refusal := asRefusal(t, err, ArmUnknownSubsection)
	if ref, ok := refusal.Fields["subsection"].(*agentreplv1.SubsectionRef); !ok || ref.GetId() != "absent" {
		t.Fatalf("refusal fields = %v, want the subsection named", refusal.Fields)
	}
	if len(f.sidebar.registries) != 0 {
		t.Fatalf("roster republications = %d, want none for a refused fold", len(f.sidebar.registries))
	}
}
