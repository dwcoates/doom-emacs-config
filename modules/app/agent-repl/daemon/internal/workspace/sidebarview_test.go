package workspace

import (
	"context"
	"errors"
	"testing"

	"claude-repld/internal/dlog"
	"claude-repld/internal/wsm"
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

func TestFoldTaskSectionRecordsTheFoldAndRepublishesTheRoster(t *testing.T) {
	tests := []struct {
		name   string
		folded bool
	}{{"collapse", true}, {"expand", false}}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.db.tasks = append(f.db.tasks, wsm.Task{ID: "task-1", Title: "t", Folded: !tt.folded})

			// Act
			err := f.verbs.FoldTaskSection(context.Background(), "task-1", tt.folded)

			// Assert
			if err != nil {
				t.Fatalf("FoldTaskSection: %v", err)
			}
			if len(f.sidebar.registries) != 1 {
				t.Fatalf("roster republications = %d, want exactly one", len(f.sidebar.registries))
			}
			if got := f.sidebar.registries[0].Tasks[0].Folded; got != tt.folded {
				t.Fatalf("republished task fold = %v, want %v", got, tt.folded)
			}
		})
	}
}

func TestFoldTaskSectionAnswersAFoldAlreadyHeldAsSuccess(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.db.tasks = append(f.db.tasks, wsm.Task{ID: "task-1", Title: "t", Folded: true})

	// Act
	err := f.verbs.FoldTaskSection(context.Background(), "task-1", true)

	// Assert
	if err != nil {
		t.Fatalf("FoldTaskSection = %v, want success for a fold already held", err)
	}
	if len(f.sidebar.registries) != 1 {
		t.Fatalf("roster republications = %d, want exactly one", len(f.sidebar.registries))
	}
}

func TestFoldTaskSectionRefusesAnUnknownTask(t *testing.T) {
	// Arrange
	f := newFixture(t)

	// Act
	err := f.verbs.FoldTaskSection(context.Background(), "absent", true)

	// Assert
	asRefusal(t, err, ArmUnknownTask)
	if len(f.sidebar.registries) != 0 {
		t.Fatalf("roster republications = %d, want none for a refused fold", len(f.sidebar.registries))
	}
}

func TestFoldTaskSectionSurfacesAFailedWriteAtError(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.db.tasks = append(f.db.tasks, wsm.Task{ID: "task-1", Title: "t"})
	f.db.setTaskFoldedErr = errors.New("disk I/O error")

	// Act
	err := f.verbs.FoldTaskSection(context.Background(), "task-1", true)

	// Assert
	if err == nil {
		t.Fatal("a failed fold write was answered as success")
	}
	if _, refused := AsRefusal(err); refused {
		t.Fatalf("a failed write was answered as a refusal: %v", err)
	}
	if !loggedAtError(f, opFoldTaskSection) {
		t.Fatal("the failed write was not recorded at error")
	}
	if len(f.sidebar.registries) != 0 {
		t.Fatalf("roster republications = %d, want none for a failed fold", len(f.sidebar.registries))
	}
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

func TestTheRepublishedRosterCarriesTheDefaultViewWhenNoneWasSet(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.db.tasks = append(f.db.tasks, wsm.Task{ID: "task-1", Title: "t"})

	// Act
	if err := f.verbs.FoldTaskSection(context.Background(), "task-1", true); err != nil {
		t.Fatalf("FoldTaskSection: %v", err)
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
	recorded := wsm.SidebarView{MergedFolded: false, Grouping: wsm.GroupingTask}
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

func TestShowGroupingRecordsTheGroupingAndRepublishesTheRoster(t *testing.T) {
	tests := []struct {
		name     string
		grouping wsm.Grouping
	}{{"task", wsm.GroupingTask}, {"repository", wsm.GroupingRepository}}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)

			// Act
			err := f.verbs.ShowGrouping(context.Background(), tt.grouping)

			// Assert
			if err != nil {
				t.Fatalf("ShowGrouping: %v", err)
			}
			if len(f.sidebar.registries) != 1 || f.sidebar.registries[0].View.Grouping != tt.grouping {
				t.Fatalf("registries = %+v, want one showing %s", f.sidebar.registries, tt.grouping)
			}
		})
	}
}

func TestShowGroupingSurfacesAFailedWriteAtError(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.db.setGroupingErr = errors.New("disk I/O error")

	// Act
	err := f.verbs.ShowGrouping(context.Background(), wsm.GroupingTask)

	// Assert
	if err == nil {
		t.Fatal("a failed grouping write was answered as success")
	}
	if !loggedAtError(f, opShowGrouping) {
		t.Fatal("the failed write was not recorded at error")
	}
	if len(f.sidebar.registries) != 0 {
		t.Fatalf("roster republications = %d, want none for a failed write", len(f.sidebar.registries))
	}
}
