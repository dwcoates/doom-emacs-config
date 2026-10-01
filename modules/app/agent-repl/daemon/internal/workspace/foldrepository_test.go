package workspace

import (
	"context"
	"errors"
	"testing"

	"claude-repld/internal/dlog"
	"claude-repld/internal/wsm"
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
