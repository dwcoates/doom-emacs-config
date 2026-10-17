package wsm

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"testing"
	"time"

	"agent-repld/internal/ids"
)

// parentFamily is one repository with a main checkout and the workspaces a
// retarget test arranges in it, each with a creation job targeting the main
// checkout unless the test says otherwise.
type parentFamily struct {
	repoDir string
	byName  map[string]Workspace
}

// newParentFamily registers names as workspaces of ONE repository, each with
// a creation job whose merge targets the repository's main checkout.
func newParentFamily(t *testing.T, s *store, names ...string) parentFamily {
	t.Helper()
	repoDir := canonicalTemp(t)
	family := parentFamily{repoDir: repoDir, byName: map[string]Workspace{}}
	for _, name := range names {
		family.byName[name] = registerInRepo(t, s, repoDir, name, true)
	}
	return family
}

// registerInRepo registers one workspace of the repository at repoDir, with a
// creation job targeting the main checkout when withJob is set.
func registerInRepo(t *testing.T, s *store, repoDir, name string, withJob bool) Workspace {
	t.Helper()
	ctx := context.Background()
	dir := filepath.Join(canonicalTemp(t), name)
	ws, _, err := s.RegisterWorkspace(ctx, mkdir(t, dir), RegisterFacts{
		Name: name, Branch: name, ParentBranch: "master", RepoDir: repoDir, DefaultBranch: "master",
	})
	if err != nil {
		t.Fatalf("RegisterWorkspace(%s): %v", name, err)
	}
	if withJob {
		job := CreationJob{Workspace: ws.ID, Layout: MergeLayout{SourceBranch: name, SourceDir: ws.Dir, TargetDir: repoDir, Origin: "create_standard"},
			Materialized: true, CreatedAt: time.Unix(1, 0)}
		if err := s.PutCreationJob(ctx, job); err != nil {
			t.Fatalf("PutCreationJob(%s): %v", name, err)
		}
	}
	return ws
}

// canonicalTemp is a fresh temp dir in its canonical spelling, which is the
// spelling the registry records and a target dir is compared in.
func canonicalTemp(t *testing.T) string {
	t.Helper()
	dir, err := filepath.EvalSymlinks(t.TempDir())
	if err != nil {
		t.Fatalf("EvalSymlinks: %v", err)
	}
	return dir
}

// mkdir creates dir and answers it.
func mkdir(t *testing.T, dir string) string {
	t.Helper()
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}
	return dir
}

// retarget sets name's parent to parentName ("" for top-level) or fails the
// test.
func (f parentFamily) retarget(t *testing.T, s *store, name, parentName string) ParentChange {
	t.Helper()
	change, err := s.SetParent(context.Background(), f.byName[name].ID, f.id(parentName))
	if err != nil {
		t.Fatalf("SetParent(%s -> %q): %v", name, parentName, err)
	}
	return change
}

// id answers a member's id, nil for "".
func (f parentFamily) id(name string) *WorkspaceID {
	if name == "" {
		return nil
	}
	id := f.byName[name].ID
	return &id
}

// targetOf reads a workspace's recorded parent and merge target back.
func targetOf(t *testing.T, s *store, id WorkspaceID) (*WorkspaceID, string) {
	t.Helper()
	ws, err := s.Workspace(context.Background(), id)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	job, found, err := s.CreationJob(context.Background(), id)
	if err != nil || !found {
		t.Fatalf("CreationJob: found=%v err=%v", found, err)
	}
	return ws.Parent, job.Layout.TargetDir
}

func TestSetParentMovesTheParentAndTheMergeTargetTogether(t *testing.T) {
	tests := []struct {
		name       string
		chain      [][2]string
		subject    string
		wantParent string
		// wantTargetOf names the workspace whose dir is the target, "" for
		// the repository's main checkout.
		wantTargetOf string
	}{
		{name: "a top-level workspace becomes a child", chain: [][2]string{{"child", "parent"}}, subject: "child", wantParent: "parent", wantTargetOf: "parent"},
		{name: "a child becomes top-level", chain: [][2]string{{"child", "parent"}, {"child", ""}}, subject: "child"},
		{name: "a child moves to another parent", chain: [][2]string{{"child", "parent"}, {"child", "other"}}, subject: "child", wantParent: "other", wantTargetOf: "other"},
		{name: "a retarget to the parent already held stands", chain: [][2]string{{"child", "parent"}, {"child", "parent"}}, subject: "child", wantParent: "parent", wantTargetOf: "parent"},
		{name: "a grandchild under a child", chain: [][2]string{{"child", "parent"}, {"other", "child"}}, subject: "other", wantParent: "child", wantTargetOf: "child"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			f := newParentFamily(t, s, "parent", "child", "other")

			// Act
			for _, step := range tt.chain {
				f.retarget(t, s, step[0], step[1])
			}

			// Assert
			parent, target := targetOf(t, s, f.byName[tt.subject].ID)
			if ids.Optional(parent) != ids.Optional(f.id(tt.wantParent)) {
				t.Fatalf("parent = %q, want %q", ids.Optional(parent), ids.Optional(f.id(tt.wantParent)))
			}
			want := f.repoDir
			if tt.wantTargetOf != "" {
				want = f.byName[tt.wantTargetOf].Dir
			}
			if target != want {
				t.Fatalf("target = %q, want %q", target, want)
			}
		})
	}
}

func TestSetParentReportsTheTargetBeforeAndAfter(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	f := newParentFamily(t, s, "parent", "child")
	f.retarget(t, s, "child", "parent")

	// Act
	change := f.retarget(t, s, "child", "")

	// Assert
	want := ParentChange{
		Workspace: f.byName["child"].ID,
		Before:    MergeTarget{Parent: f.id("parent"), ParentName: "parent", Dir: f.byName["parent"].Dir, DefaultBranch: "master"},
		After:     MergeTarget{Dir: f.repoDir, DefaultBranch: "master"},
	}
	if ids.Optional(change.Before.Parent) != ids.Optional(want.Before.Parent) || change.Before.ParentName != want.Before.ParentName ||
		change.Before.Dir != want.Before.Dir || change.Before.DefaultBranch != want.Before.DefaultBranch {
		t.Fatalf("before = %+v, want %+v", change.Before, want.Before)
	}
	if change.After.Parent != nil || change.After.Dir != want.After.Dir || change.After.DefaultBranch != want.After.DefaultBranch || change.After.ParentName != "" {
		t.Fatalf("after = %+v, want %+v", change.After, want.After)
	}
	if change.Workspace != want.Workspace {
		t.Fatalf("workspace = %q, want %q", change.Workspace, want.Workspace)
	}
}

func TestSetParentRefuses(t *testing.T) {
	tests := []struct {
		name string
		// arrange builds the family and answers the subject and the parent
		// asked for.
		arrange func(t *testing.T, s *store) (WorkspaceID, *WorkspaceID)
		want    ParentRefusal
	}{
		{
			name: "a parent the registry does not hold",
			arrange: func(t *testing.T, s *store) (WorkspaceID, *WorkspaceID) {
				f := newParentFamily(t, s, "child")
				missing := WorkspaceID("ws-missing")
				return f.byName["child"].ID, &missing
			},
			want: ParentUnknown,
		},
		{
			name: "a parent of another repository",
			arrange: func(t *testing.T, s *store) (WorkspaceID, *WorkspaceID) {
				f := newParentFamily(t, s, "child")
				g := newParentFamily(t, s, "foreign")
				return f.byName["child"].ID, g.id("foreign")
			},
			want: ParentOtherRepository,
		},
		{
			name: "the workspace as its own parent",
			arrange: func(t *testing.T, s *store) (WorkspaceID, *WorkspaceID) {
				f := newParentFamily(t, s, "child")
				return f.byName["child"].ID, f.id("child")
			},
			want: ParentCycle,
		},
		{
			name: "a parent among the workspace's descendants",
			arrange: func(t *testing.T, s *store) (WorkspaceID, *WorkspaceID) {
				f := newParentFamily(t, s, "top", "middle", "bottom")
				f.retarget(t, s, "middle", "top")
				f.retarget(t, s, "bottom", "middle")
				return f.byName["top"].ID, f.id("bottom")
			},
			want: ParentCycle,
		},
		{
			name: "a workspace with no creation job",
			arrange: func(t *testing.T, s *store) (WorkspaceID, *WorkspaceID) {
				f := newParentFamily(t, s, "parent")
				registered := registerInRepo(t, s, f.repoDir, "registered", false)
				return registered.ID, f.id("parent")
			},
			want: ParentNoMergeLayout,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			subject, parent := tt.arrange(t, s)
			before, err := s.Workspace(context.Background(), subject)
			if err != nil {
				t.Fatalf("Workspace: %v", err)
			}

			// Act
			_, err = s.SetParent(context.Background(), subject, parent)

			// Assert
			refused, ok := AsParentRefused(err)
			if !ok {
				t.Fatalf("SetParent err = %v, want a ParentRefusedError", err)
			}
			if refused.Refusal != tt.want {
				t.Fatalf("refusal = %s, want %s", refused.Refusal, tt.want)
			}
			after, err := s.Workspace(context.Background(), subject)
			if err != nil {
				t.Fatalf("Workspace: %v", err)
			}
			if ids.Optional(after.Parent) != ids.Optional(before.Parent) {
				t.Fatalf("parent changed on a refusal: %q -> %q", ids.Optional(before.Parent), ids.Optional(after.Parent))
			}
			for _, r := range log.Records() {
				if r.Level == "error" || r.Level == "warn" {
					t.Fatalf("a refused retarget logged %s %q; it is an answer", r.Level, r.Message)
				}
			}
		})
	}
}

func TestSetParentNamesTheForeignParentsRepository(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	f := newParentFamily(t, s, "child")
	g := newParentFamily(t, s, "foreign")

	// Act
	_, err := s.SetParent(context.Background(), f.byName["child"].ID, g.id("foreign"))

	// Assert
	refused, ok := AsParentRefused(err)
	if !ok || refused.ParentRepositoryDir != g.repoDir {
		t.Fatalf("refusal = %+v, want the foreign repository %q", refused, g.repoDir)
	}
}

func TestSetParentOfAnUnknownWorkspaceIsNotFound(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	_, err := s.SetParent(context.Background(), "ws-missing", nil)

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("err = %v, want ErrNotFound", err)
	}
}

func TestRestoreParent(t *testing.T) {
	tests := []struct {
		name string
		// supersede retargets the child again after the change under test.
		supersede  string
		wantStale  bool
		wantParent string
	}{
		{name: "puts the prior target back", wantParent: "parent"},
		{name: "leaves a superseding retarget alone", supersede: "other", wantStale: true, wantParent: "other"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			f := newParentFamily(t, s, "parent", "child", "other")
			f.retarget(t, s, "child", "parent")
			change := f.retarget(t, s, "child", "")
			if tt.supersede != "" {
				f.retarget(t, s, "child", tt.supersede)
			}

			// Act
			stale, err := s.RestoreParent(context.Background(), change)

			// Assert
			if err != nil {
				t.Fatalf("RestoreParent: %v", err)
			}
			if stale != tt.wantStale {
				t.Fatalf("stale = %v, want %v", stale, tt.wantStale)
			}
			parent, target := targetOf(t, s, f.byName["child"].ID)
			if ids.Optional(parent) != ids.Optional(f.id(tt.wantParent)) || target != f.byName[tt.wantParent].Dir {
				t.Fatalf("parent = %q at %q, want %q at %q", ids.Optional(parent), target, tt.wantParent, f.byName[tt.wantParent].Dir)
			}
		})
	}
}

func TestARetargetReDecidesWhoLandsTheWorkspace(t *testing.T) {
	tests := []struct {
		name        string
		contextFrom string
		chain       [][2]string
		want        bool
	}{
		{name: "a fresh child is its owner's to land", chain: [][2]string{{"child", "parent"}}, want: true},
		{name: "a child forked from its owner lands itself", contextFrom: "parent", chain: [][2]string{{"child", "parent"}}},
		{name: "a child moved to an owner it did not fork is that owner's to land", contextFrom: "parent", chain: [][2]string{{"child", "parent"}, {"child", "other"}}, want: true},
		{name: "a child moved to the top level lands itself", chain: [][2]string{{"child", "parent"}, {"child", ""}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			f := newParentFamily(t, s, "parent", "child", "other")
			if tt.contextFrom != "" {
				if _, err := s.db().Exec(`UPDATE workspaces SET context_from = ? WHERE id = ?`, f.byName[tt.contextFrom].ID, f.byName["child"].ID); err != nil {
					t.Fatalf("seed context_from: %v", err)
				}
			}

			// Act
			for _, step := range tt.chain {
				f.retarget(t, s, step[0], step[1])
			}

			// Assert
			got, err := s.Workspace(context.Background(), f.byName["child"].ID)
			if err != nil {
				t.Fatalf("Workspace: %v", err)
			}
			if got.OwnerLands != tt.want {
				t.Fatalf("owner lands = %v, want %v", got.OwnerLands, tt.want)
			}
		})
	}
}
