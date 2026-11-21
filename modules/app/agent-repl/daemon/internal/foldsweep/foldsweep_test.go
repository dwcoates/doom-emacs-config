package foldsweep

import (
	"context"
	"errors"
	"slices"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
)

var now = time.Date(2026, 11, 21, 12, 0, 0, 0, time.UTC)

type fakeDB struct {
	arr      wsm.Arrangement
	stamps   []wsm.UserAttention
	arrErr   error
	stampErr error
}

func (d *fakeDB) ReadArrangement(context.Context) (wsm.Arrangement, error) { return d.arr, d.arrErr }
func (d *fakeDB) ListUserAttention(context.Context) ([]wsm.UserAttention, error) {
	return d.stamps, d.stampErr
}

// fakeFolder records each fold as "kind:target" and fails the ones in failing.
type fakeFolder struct {
	folds     []string
	requester []workspace.FoldRequester
	failing   map[string]error
}

func (f *fakeFolder) record(kind, target string, folded bool, r workspace.FoldRequester) error {
	if !folded {
		panic("the sweep never expands")
	}
	key := kind + ":" + target
	if err := f.failing[key]; err != nil {
		return err
	}
	f.folds = append(f.folds, key)
	f.requester = append(f.requester, r)
	return nil
}
func (f *fakeFolder) FoldRepository(_ context.Context, id ids.RepoID, folded bool, r workspace.FoldRequester) error {
	return f.record("repository", string(id), folded, r)
}
func (f *fakeFolder) FoldSubsection(_ context.Context, id ids.SubsectionID, folded bool, r workspace.FoldRequester) error {
	return f.record("subsection", string(id), folded, r)
}
func (f *fakeFolder) FoldChildren(_ context.Context, id ids.WorkspaceID, folded bool, r workspace.FoldRequester) error {
	return f.record("children", string(id), folded, r)
}

func prompt(ws string, ago time.Duration) wsm.UserAttention {
	return wsm.UserAttention{Kind: wsm.AttentionPrompt, Target: ws, At: now.Add(-ago)}
}

func open(kind wsm.AttentionKind, target string, ago time.Duration) wsm.UserAttention {
	return wsm.UserAttention{Kind: kind, Target: target, At: now.Add(-ago)}
}

func sub(id string) *wsm.SubsectionID   { s := wsm.SubsectionID(id); return &s }
func parent(id string) *wsm.WorkspaceID { w := wsm.WorkspaceID(id); return &w }

// world is one repository with a subsection holding a family (root > child)
// and a loner in no subsection.
func world() wsm.Arrangement {
	return wsm.Arrangement{
		Repositories: []wsm.Repository{{ID: "repo"}},
		Subsections:  []wsm.Subsection{{ID: "sub", Repo: "repo"}},
		Workspaces: []wsm.Workspace{
			{ID: "root", Repo: "repo", Subsection: sub("sub")},
			{ID: "child", Repo: "repo", Parent: parent("root"), Subsection: sub("sub")},
			{ID: "loner", Repo: "repo"},
		},
	}
}

func run(t *testing.T, db *fakeDB, folder *fakeFolder) (*dlog.TestSurfaces, error) {
	t.Helper()
	log := dlog.NewTestSurfaces()
	sweeper, err := New(Deps{DB: db, Folder: folder, Now: func() time.Time { return now }, Log: log})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	return log, sweeper.Sweep(context.Background())
}

func TestSweepFoldsWhatNoMemberWasPromptedInSixHours(t *testing.T) {
	tests := []struct {
		name   string
		mutate func(*fakeDB)
		stamps []wsm.UserAttention
		want   []string
	}{
		{
			name: "no prompt ever folds every section",
			want: []string{"repository:repo", "subsection:sub", "children:root"},
		},
		{
			name:   "a prompt just under six hours old to the loner spares the repository section only",
			stamps: []wsm.UserAttention{prompt("loner", IdleAfter-time.Minute)},
			want:   []string{"subsection:sub", "children:root"},
		},
		{
			name:   "a prompt exactly six hours old no longer spares",
			stamps: []wsm.UserAttention{prompt("loner", IdleAfter)},
			want:   []string{"repository:repo", "subsection:sub", "children:root"},
		},
		{
			name:   "a recent prompt to the child spares its parent's children, the subsection and the repository",
			stamps: []wsm.UserAttention{prompt("child", time.Minute)},
		},
		{
			name:   "a recent prompt to the root spares the subsection and repository but not the root's own children",
			stamps: []wsm.UserAttention{prompt("root", time.Minute)},
			want:   []string{"children:root"},
		},
		{
			name:   "an open of the subsection under an hour ago spares it",
			stamps: []wsm.UserAttention{open(wsm.AttentionOpenedSubsection, "sub", 59*time.Minute)},
			want:   []string{"repository:repo", "children:root"},
		},
		{
			name:   "an open of the subsection an hour ago no longer spares it",
			stamps: []wsm.UserAttention{open(wsm.AttentionOpenedSubsection, "sub", time.Hour)},
			want:   []string{"repository:repo", "subsection:sub", "children:root"},
		},
		{
			name:   "an open of the repository under an hour ago spares it",
			stamps: []wsm.UserAttention{open(wsm.AttentionOpenedRepository, "repo", time.Minute)},
			want:   []string{"subsection:sub", "children:root"},
		},
		{
			name:   "an open of root's children under an hour ago spares them",
			stamps: []wsm.UserAttention{open(wsm.AttentionOpenedChildren, "root", time.Minute)},
			want:   []string{"repository:repo", "subsection:sub"},
		},
		{
			name:   "an open stamp of another target spares nothing",
			stamps: []wsm.UserAttention{open(wsm.AttentionOpenedSubsection, "elsewhere", time.Minute)},
			want:   []string{"repository:repo", "subsection:sub", "children:root"},
		},
		{
			name: "folds already folded are left alone",
			mutate: func(db *fakeDB) {
				db.arr.Repositories[0].Folded = true
				db.arr.Subsections[0].Folded = true
				db.arr.Workspaces[0].ChildrenFolded = true
			},
		},
		{
			name:   "the section holding the current workspace is spared",
			mutate: func(db *fakeDB) { db.arr.Workspaces[1].Current = true },
		},
		{
			name: "a section with only closed members is not folded",
			mutate: func(db *fakeDB) {
				for i := range db.arr.Workspaces {
					db.arr.Workspaces[i].Closed = true
				}
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			db := &fakeDB{arr: world(), stamps: tt.stamps}
			if tt.mutate != nil {
				tt.mutate(db)
			}
			folder := &fakeFolder{}

			// Act
			_, err := run(t, db, folder)

			// Assert
			if err != nil {
				t.Fatalf("Sweep: %v", err)
			}
			want := append([]string(nil), tt.want...)
			got := append([]string(nil), folder.folds...)
			slices.Sort(want)
			slices.Sort(got)
			if !slices.Equal(got, want) {
				t.Fatalf("folds = %v, want %v", got, want)
			}
		})
	}
}

func TestSweepFoldsCarryTheSweepAsRequester(t *testing.T) {
	// Arrange
	folder := &fakeFolder{}

	// Act
	_, err := run(t, &fakeDB{arr: world()}, folder)

	// Assert
	if err != nil {
		t.Fatalf("Sweep: %v", err)
	}
	for _, r := range folder.requester {
		if r.RPC != RequesterRPC {
			t.Fatalf("requester = %+v, want RPC %q", r, RequesterRPC)
		}
	}
}

func TestSweepRecordsAFailedFoldAndStillRunsTheRest(t *testing.T) {
	// Arrange
	boom := errors.New("holds_current")
	folder := &fakeFolder{failing: map[string]error{"subsection:sub": boom}}

	// Act
	log, err := run(t, &fakeDB{arr: world()}, folder)

	// Assert
	if !errors.Is(err, boom) {
		t.Fatalf("err = %v, want the failed fold surfaced", err)
	}
	if want := []string{"repository:repo", "children:root"}; !slices.Equal(folder.folds, want) {
		t.Fatalf("folds = %v, want the others still run: %v", folder.folds, want)
	}
	rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opSweep})
	if !ok || rec.Context["target"] != "sub" || rec.Context["cause"] != boom.Error() {
		t.Fatalf("error record = %+v (found %v), want one naming the subsection and its cause", rec, ok)
	}
}

func TestSweepFoldsNothingWhenAReadFails(t *testing.T) {
	tests := []struct {
		name string
		db   *fakeDB
	}{
		{name: "the arrangement", db: &fakeDB{arr: world(), arrErr: errors.New("arrangement read failed")}},
		{name: "the user's attention", db: &fakeDB{arr: world(), stampErr: errors.New("attention read failed")}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			folder := &fakeFolder{}

			// Act
			log, err := run(t, tt.db, folder)

			// Assert
			if err == nil {
				t.Fatal("Sweep answered success over a failed read")
			}
			if len(folder.folds) != 0 {
				t.Fatalf("folds = %v, want none", folder.folds)
			}
			if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opSweep}); !ok {
				t.Fatal("the failed read was not recorded at error")
			}
		})
	}
}

func TestNewRefusesEachMissingCollaborator(t *testing.T) {
	log := dlog.NewTestSurfaces()
	tests := []struct {
		name string
		deps Deps
	}{
		{name: "the state client", deps: Deps{Folder: &fakeFolder{}, Log: log}},
		{name: "the fold verbs", deps: Deps{DB: &fakeDB{}, Log: log}},
		{name: "the log surfaces", deps: Deps{DB: &fakeDB{}, Folder: &fakeFolder{}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			_, err := New(tt.deps)

			// Assert
			if err == nil {
				t.Fatal("New accepted a missing collaborator")
			}
		})
	}
}
