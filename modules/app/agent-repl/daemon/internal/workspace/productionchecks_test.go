package workspace

import (
	"context"
	"errors"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/resolve/topbar"
	"agent-repld/internal/wsm"
)

func TestProductionChecksOfResolvesTheRepositorysChip(t *testing.T) {
	cases := []struct {
		name     string
		standing merge.ProductionStanding
		want     func(dir string) topbar.ProductionChecks
	}{
		{name: "a repository that checks no production draws no chip",
			want: func(string) topbar.ProductionChecks { return topbar.ProductionChecks{} }},
		{name: "running checks with no mediator yet name none",
			standing: merge.ProductionStanding{On: true},
			want: func(dir string) topbar.ProductionChecks {
				return topbar.ProductionChecks{On: true, Repository: "repo-1", RepositoryDir: dir}
			}},
		{name: "paused checks name their mediator by its name",
			standing: merge.ProductionStanding{On: true, Paused: true, Mediator: "w1"},
			want: func(dir string) topbar.ProductionChecks {
				return topbar.ProductionChecks{On: true, Paused: true, Mediator: "sample", Repository: "repo-1", RepositoryDir: dir}
			}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			record := f.workspace("w1", t.TempDir())
			f.merge.productionStanding = tc.standing

			// Act
			got, err := f.verbs.(*verbs).productionChecksOf(context.Background(), record.Repo)

			// Assert
			if want := tc.want(f.db.repositories[0].Dir); err != nil || got != want {
				t.Fatalf("productionChecksOf = (%+v, %v), want %+v", got, err, want)
			}
		})
	}
}

func TestProductionChecksOfSurfacesWhatItCannotRead(t *testing.T) {
	cases := []struct {
		name     string
		standing merge.ProductionStanding
		err      error
		want     string
	}{
		{name: "an unreadable standing", err: errors.New("database is locked"), want: "database is locked"},
		{name: "a mediator the registry does not hold", standing: merge.ProductionStanding{On: true, Mediator: "gone"}, want: `"gone"`},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			record := f.workspace("w1", t.TempDir())
			f.merge.productionStanding, f.merge.productionStandingErr = tc.standing, tc.err

			// Act
			_, err := f.verbs.(*verbs).productionChecksOf(context.Background(), record.Repo)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("productionChecksOf = %v, want an error naming %s", err, tc.want)
			}
		})
	}
}

func TestRestateProductionChecksReachesEveryOpenWorkspaceOfTheRepository(t *testing.T) {
	// Arrange: two open workspaces of the repository, one closed, one in
	// another repository.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.workspace("w2", t.TempDir())
	closed := f.workspace("w3", t.TempDir())
	closed.Closed = true
	f.db.with(closed)
	other := f.workspace("w4", t.TempDir())
	other.Repo = "repo-2"
	f.db.with(other)
	f.merge.productionStanding = merge.ProductionStanding{On: true, Paused: true}

	// Act
	err := f.verbs.RestateProductionChecks(context.Background(), merge.RepositoryScope{ID: "repo-1"})

	// Assert
	if err != nil {
		t.Fatalf("RestateProductionChecks: %v", err)
	}
	var restated []ids.WorkspaceID
	for _, r := range f.topbarProductionChecks {
		if !r.chip.Paused {
			t.Fatalf("chip = %+v, want paused", r.chip)
		}
		restated = append(restated, r.ws)
	}
	if len(restated) != 2 || !containsAll(restated, "w1", "w2") {
		t.Fatalf("restated %v, want w1 and w2 only", restated)
	}
	if rec, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opProductionChecks}); !ok || rec.Context["workspaces"] != 2 {
		t.Fatalf("records = %v, want the restate at INFO counting two workspaces", f.log.logger.Records())
	}
}

// containsAll reports whether GOT holds every one of WANT.
func containsAll(got []ids.WorkspaceID, want ...ids.WorkspaceID) bool {
	for _, w := range want {
		found := false
		for _, g := range got {
			found = found || g == w
		}
		if !found {
			return false
		}
	}
	return true
}

func TestRestateProductionChecksOfAnUnknownRepositoryStatesNothing(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.merge.productionStanding = merge.ProductionStanding{On: true}

	// Act
	err := f.verbs.RestateProductionChecks(context.Background(), merge.RepositoryScope{ID: "repo-gone"})

	// Assert
	if err == nil || len(f.topbarProductionChecks) != 0 {
		t.Fatalf("RestateProductionChecks = %v, stated %v; want the failure and nothing stated", err, f.topbarProductionChecks)
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opProductionChecks}); !ok {
		t.Fatalf("records = %v, want the failure at ERROR", f.log.logger.Records())
	}
}

func TestRestateProductionChecksThatCannotResolveStatesNothing(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.merge.productionStandingErr = errors.New("database is locked")

	// Act
	err := f.verbs.RestateProductionChecks(context.Background(), merge.RepositoryScope{ID: "repo-1"})

	// Assert
	if err == nil || len(f.topbarProductionChecks) != 0 {
		t.Fatalf("RestateProductionChecks = %v, stated %v; want the failure and nothing stated", err, f.topbarProductionChecks)
	}
	if rec, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opProductionChecks}); !ok || rec.Context["repository"] != "repo-1" || !strings.Contains(rec.Context["cause"].(string), "database is locked") {
		t.Fatalf("records = %v, want the failure at ERROR naming the repository and the cause", f.log.logger.Records())
	}
}

func TestRegistrationStatesTheProductionChecksOnTheNaming(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.merge.productionStanding = merge.ProductionStanding{On: true}

	// Act
	if _, err := f.verbs.Register(context.Background(), f.git.mainWorktree, wsm.RegisterFacts{}); err != nil {
		t.Fatalf("Register: %v", err)
	}

	// Assert
	if n := len(f.topbarNamings); n == 0 || !f.topbarNamings[n-1].ProductionChecks.On {
		t.Fatalf("namings = %+v, want the last carrying the production checks", f.topbarNamings)
	}
}

func TestRegistrationFailsOnAnUnreadableProductionStanding(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.merge.productionStandingErr = errors.New("database is locked")

	// Act
	_, err := f.verbs.Register(context.Background(), f.git.mainWorktree, wsm.RegisterFacts{})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "database is locked") {
		t.Fatalf("Register = %v, want the read's failure", err)
	}
}

func TestAThreadsNamingDrawsNoProductionChecksAndAsksNoStanding(t *testing.T) {
	// Arrange: a thread, with a standing read that would fail if asked.
	f := newFixture(t)
	record := f.workspace("w1", t.TempDir())
	record.Kind = wsm.WorkspaceKindThread
	f.db.with(record)
	f.merge.productionStandingErr = errors.New("the zettel home is no registered repository")

	// Act
	err := f.verbs.(*verbs).publishNaming(context.Background(), f.log.logger, record)

	// Assert
	if err != nil {
		t.Fatalf("publishNaming = %v, want the thread named", err)
	}
	if n := len(f.topbarNamings); n == 0 || f.topbarNamings[n-1].ProductionChecks.On {
		t.Fatalf("namings = %+v, want the last drawing no production-checks chip", f.topbarNamings)
	}
}
