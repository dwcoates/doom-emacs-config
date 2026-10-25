package workspace

import (
	"context"
	"errors"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// loggedAtError reports whether the fixture's log carries an ERROR record
// under operation.
func loggedAtError(f *fixture, operation string) bool {
	for _, r := range f.log.logger.Records() {
		if r.Level == dlog.LevelError && r.Operation == operation {
			return true
		}
	}
	return false
}

func TestFoldMergedSectionRecordsTheFoldAndRepublishesTheRoster(t *testing.T) {
	tests := []struct {
		name   string
		folded bool
	}{{"collapse", true}, {"expand", false}}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			prior := wsm.DefaultSidebarView
			prior.MergedFolded = !tt.folded
			f.db.view = &prior

			// Act
			err := f.verbs.FoldMergedSection(context.Background(), tt.folded)

			// Assert
			if err != nil {
				t.Fatalf("FoldMergedSection: %v", err)
			}
			if len(f.sidebar.registries) != 1 {
				t.Fatalf("roster republications = %d, want exactly one", len(f.sidebar.registries))
			}
			if got := f.sidebar.registries[0].View.MergedFolded; got != tt.folded {
				t.Fatalf("republished band fold = %v, want %v", got, tt.folded)
			}
		})
	}
}

func TestFoldMergedSectionSurfacesAFailedWriteAtError(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.db.setMergedFoldedErr = errors.New("disk I/O error")

	// Act
	err := f.verbs.FoldMergedSection(context.Background(), false)

	// Assert
	if err == nil {
		t.Fatal("a failed fold write was answered as success")
	}
	if !loggedAtError(f, opFoldMergedSection) {
		t.Fatal("the failed write was not recorded at error")
	}
	if len(f.sidebar.registries) != 0 {
		t.Fatalf("roster republications = %d, want none for a failed fold", len(f.sidebar.registries))
	}
}

func TestFoldHiddenSectionRecordsTheFoldAndRepublishesTheRoster(t *testing.T) {
	tests := []struct {
		name   string
		folded bool
	}{{"collapse", true}, {"expand", false}}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			prior := wsm.DefaultSidebarView
			prior.HiddenFolded = !tt.folded
			f.db.view = &prior

			// Act
			err := f.verbs.FoldHiddenSection(context.Background(), tt.folded)

			// Assert
			if err != nil {
				t.Fatalf("FoldHiddenSection: %v", err)
			}
			if len(f.sidebar.registries) != 1 {
				t.Fatalf("roster republications = %d, want exactly one", len(f.sidebar.registries))
			}
			if got := f.sidebar.registries[0].View.HiddenFolded; got != tt.folded {
				t.Fatalf("republished hidden fold = %v, want %v", got, tt.folded)
			}
		})
	}
}

func TestFoldOpenSectionRecordsTheFoldAndRepublishesTheRoster(t *testing.T) {
	tests := []struct {
		name   string
		folded bool
	}{{"collapse", true}, {"expand", false}}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			prior := wsm.DefaultSidebarView
			prior.OpenFolded = !tt.folded
			f.db.view = &prior

			// Act
			err := f.verbs.FoldOpenSection(context.Background(), tt.folded)

			// Assert
			if err != nil {
				t.Fatalf("FoldOpenSection: %v", err)
			}
			if len(f.sidebar.registries) != 1 {
				t.Fatalf("roster republications = %d, want exactly one", len(f.sidebar.registries))
			}
			if got := f.sidebar.registries[0].View.OpenFolded; got != tt.folded {
				t.Fatalf("republished open fold = %v, want %v", got, tt.folded)
			}
		})
	}
}

func TestFoldOpenSectionSurfacesAFailedWriteAtError(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.db.setOpenFoldedErr = errors.New("disk I/O error")

	// Act
	err := f.verbs.FoldOpenSection(context.Background(), false)

	// Assert
	if err == nil {
		t.Fatal("a failed fold write was answered as success")
	}
	if !loggedAtError(f, opFoldOpenSection) {
		t.Fatal("the failed write was not recorded at error")
	}
	if len(f.sidebar.registries) != 0 {
		t.Fatalf("roster republications = %d, want none for a failed fold", len(f.sidebar.registries))
	}
}

func TestShowOpenMergeQueuesRecordsTheViewAndRepublishesTheRoster(t *testing.T) {
	tests := []struct {
		name        string
		mergeQueues bool
	}{{"merge queues", true}, {"workspaces", false}}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			prior := wsm.DefaultSidebarView
			prior.OpenShowsMergeQueues = !tt.mergeQueues
			f.db.view = &prior

			// Act
			err := f.verbs.ShowOpenMergeQueues(context.Background(), tt.mergeQueues)

			// Assert
			if err != nil {
				t.Fatalf("ShowOpenMergeQueues: %v", err)
			}
			if len(f.sidebar.registries) != 1 {
				t.Fatalf("roster republications = %d, want exactly one", len(f.sidebar.registries))
			}
			if got := f.sidebar.registries[0].View.OpenShowsMergeQueues; got != tt.mergeQueues {
				t.Fatalf("republished open shows merge queues = %v, want %v", got, tt.mergeQueues)
			}
		})
	}
}

func TestShowOpenMergeQueuesSurfacesAFailedWriteAtError(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.db.setOpenShowsErr = errors.New("disk I/O error")

	// Act
	err := f.verbs.ShowOpenMergeQueues(context.Background(), true)

	// Assert
	if err == nil {
		t.Fatal("a failed view write was answered as success")
	}
	if !loggedAtError(f, opShowOpenSection) {
		t.Fatal("the failed write was not recorded at error")
	}
	if len(f.sidebar.registries) != 0 {
		t.Fatalf("roster republications = %d, want none for a failed view change", len(f.sidebar.registries))
	}
}

func TestFoldHiddenSectionSurfacesAFailedWriteAtError(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.db.setHiddenFoldedErr = errors.New("disk I/O error")

	// Act
	err := f.verbs.FoldHiddenSection(context.Background(), false)

	// Assert
	if err == nil {
		t.Fatal("a failed fold write was answered as success")
	}
	if !loggedAtError(f, opFoldHiddenSection) {
		t.Fatal("the failed write was not recorded at error")
	}
	if len(f.sidebar.registries) != 0 {
		t.Fatalf("roster republications = %d, want none for a failed fold", len(f.sidebar.registries))
	}
}

func TestTheRepublishedRosterCarriesTheDefaultViewWhenNoneWasSet(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.db.repositories = append(f.db.repositories, wsm.Repository{ID: "repo-1"})

	// Act
	if err := f.verbs.FoldRepository(context.Background(), "repo-1", true); err != nil {
		t.Fatalf("FoldRepository: %v", err)
	}

	// Assert
	if got := f.sidebar.registries[0].View; got != wsm.DefaultSidebarView {
		t.Fatalf("republished view = %+v, want the default %+v", got, wsm.DefaultSidebarView)
	}
}

func TestARepublishThatCannotReadTheViewPublishesNothingAndSaysSoAtError(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.db.viewErr = errors.New("disk I/O error")

	// Act
	err := f.verbs.FoldMergedSection(context.Background(), false)

	// Assert
	if err != nil {
		t.Fatalf("FoldMergedSection: %v", err)
	}
	if len(f.sidebar.registries) != 0 {
		t.Fatalf("roster republications = %d, want none from an unreadable view", len(f.sidebar.registries))
	}
	if !loggedAtError(f, opFoldMergedSection) {
		t.Fatal("the unreadable view was not recorded at error")
	}
}

func TestPublishRegistryCarriesTheRecordedView(t *testing.T) {
	// Arrange
	f := newFixture(t)
	recorded := wsm.SidebarView{MergedFolded: true, HiddenFolded: true}
	f.db.view = &recorded

	// Act
	if err := f.verbs.PublishRegistry(context.Background()); err != nil {
		t.Fatalf("PublishRegistry: %v", err)
	}

	// Assert
	if len(f.sidebar.registries) != 1 || f.sidebar.registries[0].View != recorded {
		t.Fatalf("opening registries = %+v, want one carrying the recorded view", f.sidebar.registries)
	}
}

func TestPublishRegistryRefusesWhenTheViewCannotBeRead(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.db.viewErr = errors.New("the state client is closed")

	// Act
	err := f.verbs.PublishRegistry(context.Background())

	// Assert
	if err == nil {
		t.Fatal("PublishRegistry = nil, want the view read's failure surfaced")
	}
	if len(f.sidebar.registries) != 0 {
		t.Fatalf("roster publications = %d, want none from a view that could not be read", len(f.sidebar.registries))
	}
	if !loggedAtError(f, opRegister) {
		t.Fatal("the unreadable view was not recorded at error")
	}
}

func TestFoldSingletonSectionWritesLogsAndRepublishes(t *testing.T) {
	// Arrange
	f := newFixture(t)
	var wrote []bool
	write := func(_ context.Context, folded bool) error {
		wrote = append(wrote, folded)
		return nil
	}

	// Act
	err := f.verbs.(*verbs).foldSingletonSection(context.Background(), "daemon.workspace.fold_test_section", "test band", write, true)

	// Assert
	if err != nil {
		t.Fatalf("foldSingletonSection: %v", err)
	}
	if len(wrote) != 1 || !wrote[0] {
		t.Fatalf("writes = %v, want one collapse", wrote)
	}
	if len(f.sidebar.registries) != 1 {
		t.Fatalf("roster republications = %d, want exactly one", len(f.sidebar.registries))
	}
}

func TestFoldSingletonSectionSurfacesAFailedWriteWithItsContext(t *testing.T) {
	// Arrange
	f := newFixture(t)
	cause := errors.New("disk I/O error")
	write := func(context.Context, bool) error { return cause }

	// Act
	err := f.verbs.(*verbs).foldSingletonSection(context.Background(), "daemon.workspace.fold_test_section", "test band", write, false)

	// Assert
	if !errors.Is(err, cause) {
		t.Fatalf("err = %v, want the write's cause wrapped", err)
	}
	var record *dlog.Record
	for _, r := range f.log.logger.Records() {
		if r.Level == dlog.LevelError && r.Operation == "daemon.workspace.fold_test_section" {
			r := r
			record = &r
		}
	}
	if record == nil {
		t.Fatal("the failed write was not recorded at error")
	}
	if record.Context["section"] != "test band" || record.Context["cause"] != cause.Error() || record.Context["folded"] != false {
		t.Fatalf("error context = %v, want the section, the cause and the fold", record.Context)
	}
	if len(f.sidebar.registries) != 0 {
		t.Fatalf("roster republications = %d, want none for a failed fold", len(f.sidebar.registries))
	}
}

// TestEverySingletonSectionFoldSharesOneShape pins that each fold of a
// section the roster carries exactly one of goes through
// foldSingletonSection: its record names the section, which a hand-rolled
// fold would not.
func TestEverySingletonSectionFoldSharesOneShape(t *testing.T) {
	tests := []struct {
		name    string
		op      string
		section string
		fold    func(Verbs) error
	}{
		{"recently merged", opFoldMergedSection, "recently-merged band", func(v Verbs) error { return v.FoldMergedSection(context.Background(), true) }},
		{"hidden", opFoldHiddenSection, "hidden section", func(v Verbs) error { return v.FoldHiddenSection(context.Background(), true) }},
		{"open", opFoldOpenSection, "open section", func(v Verbs) error { return v.FoldOpenSection(context.Background(), true) }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)

			// Act
			if err := tt.fold(f.verbs); err != nil {
				t.Fatalf("fold: %v", err)
			}

			// Assert
			found := false
			for _, r := range f.log.logger.Records() {
				if r.Level == dlog.LevelInfo && r.Operation == tt.op && r.Context["section"] == tt.section {
					found = true
				}
			}
			if !found {
				t.Fatalf("no info record under %s naming section %q: %v", tt.op, tt.section, f.log.logger.Records())
			}
		})
	}
}
