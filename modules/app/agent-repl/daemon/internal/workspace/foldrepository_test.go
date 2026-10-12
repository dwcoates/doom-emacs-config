package workspace

import (
	"context"
	"errors"
	"slices"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
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
