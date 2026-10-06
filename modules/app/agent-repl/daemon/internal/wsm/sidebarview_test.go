package wsm

import (
	"context"
	"errors"
	"path/filepath"
	"testing"
)

func TestSetTaskFoldedIsReadBack(t *testing.T) {
	tests := []struct {
		name   string
		folded bool
	}{{"collapsed", true}, {"expanded", false}}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			task, err := s.CreateTask(context.Background(), "write the report")
			if err != nil {
				t.Fatalf("CreateTask: %v", err)
			}
			if err := s.SetTaskFolded(context.Background(), task.ID, !tt.folded); err != nil {
				t.Fatalf("seed SetTaskFolded: %v", err)
			}

			// Act
			err = s.SetTaskFolded(context.Background(), task.ID, tt.folded)

			// Assert
			if err != nil {
				t.Fatalf("SetTaskFolded: %v", err)
			}
			tasks, _ := s.Tasks(context.Background())
			if len(tasks) != 1 || tasks[0].Folded != tt.folded {
				t.Fatalf("tasks = %+v, want folded=%v", tasks, tt.folded)
			}
		})
	}
}

func TestANewTaskIsExpanded(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	if _, err := s.CreateTask(context.Background(), "write the report"); err != nil {
		t.Fatalf("CreateTask: %v", err)
	}

	// Assert
	tasks, _ := s.Tasks(context.Background())
	if len(tasks) != 1 || tasks[0].Folded {
		t.Fatalf("tasks = %+v, want a new task expanded", tasks)
	}
}

func TestSetTaskFoldedRefusesAnUnknownTask(t *testing.T) {
	// Arrange
	s, log := testStore(t)

	// Act
	err := s.SetTaskFolded(context.Background(), TaskID("absent"), true)

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("SetTaskFolded = %v, want ErrNotFound", err)
	}
	if len(log.Records()) == 0 {
		t.Fatal("the refused write left no record")
	}
}

func TestSidebarViewDefaultsWhenUnset(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	got, err := s.SidebarView(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("SidebarView: %v", err)
	}
	if got != DefaultSidebarView {
		t.Fatalf("view = %+v, want the default %+v", got, DefaultSidebarView)
	}
}

func TestTheDefaultViewCollapsesTheBandAndShowsTheRepositoryGrouping(t *testing.T) {
	// Arrange, Act, Assert
	if !DefaultSidebarView.MergedFolded || DefaultSidebarView.Grouping != GroupingRepository {
		t.Fatalf("DefaultSidebarView = %+v, want the band collapsed and the repository grouping", DefaultSidebarView)
	}
}

func TestSidebarViewWritesAreReadBack(t *testing.T) {
	tests := []struct {
		name  string
		write func(*store) error
		want  SidebarView
	}{
		{
			name:  "unfolding the band keeps the default grouping",
			write: func(s *store) error { return s.SetMergedSectionFolded(context.Background(), false) },
			want:  SidebarView{MergedFolded: false, Grouping: GroupingRepository},
		},
		{
			name:  "showing the task grouping keeps the default band fold",
			write: func(s *store) error { return s.SetGrouping(context.Background(), GroupingTask) },
			want:  SidebarView{MergedFolded: true, Grouping: GroupingTask},
		},
		{
			name: "a second write keeps the first",
			write: func(s *store) error {
				if err := s.SetGrouping(context.Background(), GroupingTask); err != nil {
					return err
				}
				return s.SetMergedSectionFolded(context.Background(), false)
			},
			want: SidebarView{MergedFolded: false, Grouping: GroupingTask},
		},
		{
			name: "folding the band again replaces the unfold",
			write: func(s *store) error {
				if err := s.SetMergedSectionFolded(context.Background(), false); err != nil {
					return err
				}
				return s.SetMergedSectionFolded(context.Background(), true)
			},
			want: SidebarView{MergedFolded: true, Grouping: GroupingRepository},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)

			// Act
			err := tt.write(s)

			// Assert
			if err != nil {
				t.Fatalf("write: %v", err)
			}
			got, err := s.SidebarView(context.Background())
			if err != nil || got != tt.want {
				t.Fatalf("SidebarView = %+v, %v; want %+v", got, err, tt.want)
			}
			if rows := scalar[int](t, s, `SELECT count(*) FROM sidebar_view`); rows != 1 {
				t.Fatalf("sidebar_view rows = %d, want the one singleton", rows)
			}
		})
	}
}

func TestSetGroupingRefusesAGroupingThatIsNeither(t *testing.T) {
	// Arrange
	s, log := testStore(t)

	// Act
	err := s.SetGrouping(context.Background(), Grouping("sideways"))

	// Assert
	if err == nil {
		t.Fatal("SetGrouping(sideways) = nil, want a refusal")
	}
	if !loggedOperation(log, "daemon.wsm.set_grouping", "error") {
		t.Fatalf("the refusal was not recorded at ERROR: %v", log.Records())
	}
	if rows := scalar[int](t, s, `SELECT count(*) FROM sidebar_view`); rows != 0 {
		t.Fatalf("sidebar_view rows = %d, want nothing written", rows)
	}
}

func TestSidebarViewSurvivesAReopen(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "wsm.db")
	first, err := Open(context.Background(), path)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	task, err := first.CreateTask(context.Background(), "write the report")
	if err != nil {
		t.Fatalf("CreateTask: %v", err)
	}
	if err := first.SetTaskFolded(context.Background(), task.ID, true); err != nil {
		t.Fatalf("SetTaskFolded: %v", err)
	}
	if err := first.SetMergedSectionFolded(context.Background(), false); err != nil {
		t.Fatalf("SetMergedSectionFolded: %v", err)
	}
	if err := first.SetGrouping(context.Background(), GroupingTask); err != nil {
		t.Fatalf("SetGrouping: %v", err)
	}
	if err := first.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Act
	second, err := Open(context.Background(), path)
	if err != nil {
		t.Fatalf("reopen: %v", err)
	}
	defer second.Close()
	tasks, tasksErr := second.Tasks(context.Background())
	view, viewErr := second.SidebarView(context.Background())

	// Assert
	if tasksErr != nil || len(tasks) != 1 || !tasks[0].Folded {
		t.Fatalf("tasks after reopen = %+v, %v; want the one task folded", tasks, tasksErr)
	}
	want := SidebarView{MergedFolded: false, Grouping: GroupingTask}
	if viewErr != nil || view != want {
		t.Fatalf("view after reopen = %+v, %v; want %+v", view, viewErr, want)
	}
}

func TestSidebarViewRefusesAStoredGroupingThatIsNeither(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	corrupt(t, s, `INSERT INTO sidebar_view (id, merged_folded, grouping) VALUES (1, 1, 'sideways')`)

	// Act
	_, err := s.SidebarView(context.Background())

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Field != "grouping" {
		t.Fatalf("SidebarView = %v, want a DecodeError naming the grouping", err)
	}
	if !loggedOperation(log, "daemon.wsm.sidebar_view", "error") {
		t.Fatalf("the refusal was not recorded at ERROR: %v", log.Records())
	}
}

// TestTheMigrationAddsTheSidebarView pins the layout-23 step.
func TestTheMigrationAddsTheSidebarView(t *testing.T) {
	// Arrange
	path := fixtureAt(t, 22)

	// Act
	handle, err := Open(context.Background(), path)
	if err != nil {
		t.Fatalf("Open on a layout-22 database: %v", err)
	}
	defer handle.Close()

	// Assert
	s := handle.(*store)
	if got := scalar[int](t, s, `SELECT count(*) FROM pragma_table_info('tasks') WHERE name = 'folded'`); got != 1 {
		t.Fatalf("tasks.folded columns after the migration = %d, want 1", got)
	}
	if got := scalar[int](t, s, `SELECT count(*) FROM sqlite_master WHERE type = 'table' AND name = 'sidebar_view'`); got != 1 {
		t.Fatalf("sidebar_view tables after the migration = %d, want 1", got)
	}
}
