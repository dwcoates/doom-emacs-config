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
		want     topbar.ProductionChecks
	}{
		{name: "a repository that checks no production draws no chip"},
		{name: "running checks with no mediator yet name none",
			standing: merge.ProductionStanding{On: true, Dir: "/repos/one"},
			want:     topbar.ProductionChecks{On: true, Repository: "repo-1", RepositoryDir: "/repos/one"}},
		{name: "paused checks name their mediator by its name",
			standing: merge.ProductionStanding{On: true, Paused: true, Mediator: "w1", Dir: "/repos/one"},
			want:     topbar.ProductionChecks{On: true, Paused: true, Mediator: "sample", Repository: "repo-1", RepositoryDir: "/repos/one"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			record := f.workspace("w1", t.TempDir())

			// Act
			got, err := f.verbs.(*verbs).productionChecksOf(context.Background(), productionStandings{"repo-1": tc.standing}, record.Repo)

			// Assert
			if err != nil || got != tc.want {
				t.Fatalf("productionChecksOf = (%+v, %v), want %+v", got, err, tc.want)
			}
		})
	}
}

func TestProductionChecksOfSurfacesAMediatorItCannotName(t *testing.T) {
	// Arrange
	f := newFixture(t)
	record := f.workspace("w1", t.TempDir())

	// Act
	_, err := f.verbs.(*verbs).productionChecksOf(context.Background(), productionStandings{"repo-1": {On: true, Mediator: "gone"}}, record.Repo)

	// Assert
	if err == nil || !strings.Contains(err.Error(), `"gone"`) {
		t.Fatalf("productionChecksOf = %v, want an error naming the mediator", err)
	}
}

func TestTheOpeningRosterReadsTheProductionStandingsOnce(t *testing.T) {
	// Arrange: three workspaces of the repository.
	f := newFixture(t)
	for _, id := range []ids.WorkspaceID{"w1", "w2", "w3"} {
		f.workspace(id, t.TempDir())
	}
	f.merge.productionStanding = merge.ProductionStanding{On: true, Dir: "/repos/one"}

	// Act
	err := f.verbs.PublishRegistry(context.Background())

	// Assert
	if err != nil || f.merge.standingsReads != 1 {
		t.Fatalf("PublishRegistry = %v after %d standings reads, want one read for the whole walk", err, f.merge.standingsReads)
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
	f.merge.productionStanding = merge.ProductionStanding{On: true, Paused: true, Dir: "/repos/one"}

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
	// Arrange: the main worktree registered once, which mints its repository.
	f := newFixture(t)
	record, err := f.verbs.Register(context.Background(), f.git.mainWorktree, wsm.RegisterFacts{})
	if err != nil {
		t.Fatalf("Register: %v", err)
	}
	f.merge.productionStanding, f.merge.standingRepo = merge.ProductionStanding{On: true}, record.Repo

	// Act: Emacs announces it again.
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
	err := f.verbs.(*verbs).bringUpViewsAlone(context.Background(), f.log.logger, record)

	// Assert
	if err != nil {
		t.Fatalf("publishNaming = %v, want the thread named", err)
	}
	if n := len(f.topbarNamings); n == 0 || f.topbarNamings[n-1].ProductionChecks.On || f.merge.standingsReads != 0 {
		t.Fatalf("namings = %+v after %d standings reads, want the last drawing no chip and none read", f.topbarNamings, f.merge.standingsReads)
	}
}
