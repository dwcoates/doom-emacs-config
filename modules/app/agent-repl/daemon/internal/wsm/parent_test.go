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
	registerMain(t, s, repoDir)
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
	change, err := s.SetParent(context.Background(), f.byName[name].ID, ToParent(f.id(parentName)))
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
			_, err = s.SetParent(context.Background(), subject, ToParent(parent))

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

func TestSetParentOfAnotherRepositoryNestsAndOwnsButLandsInTheOwnRepository(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	f := newParentFamily(t, s, "child")
	g := newParentFamily(t, s, "foreign")

	// Act
	change, err := s.SetParent(context.Background(), f.byName["child"].ID, ToParent(g.id("foreign")))

	// Assert
	if err != nil {
		t.Fatalf("SetParent: %v", err)
	}
	if ids.Optional(change.After.Parent) != string(*g.id("foreign")) || !change.After.CrossRepository ||
		change.After.Dir != f.repoDir || change.After.ParentName != "foreign" {
		t.Fatalf("after = %+v, want parent foreign of another repository, landing in %q", change.After, f.repoDir)
	}
	child, err := s.Workspace(context.Background(), f.byName["child"].ID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if ids.Optional(child.Parent) != string(*g.id("foreign")) || !child.OwnerLands {
		t.Fatalf("child = parent %q owner_lands %v, want parent foreign, landed by its owner", ids.Optional(child.Parent), child.OwnerLands)
	}
	if _, got := targetOf(t, s, child.ID); got != f.repoDir {
		t.Fatalf("target_dir = %q, want the own repository's main checkout %q", got, f.repoDir)
	}
}

func TestSetParentReportsAForeignParentBefore(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	f := newParentFamily(t, s, "child")
	g := newParentFamily(t, s, "foreign")
	if _, err := s.SetParent(context.Background(), f.byName["child"].ID, ToParent(g.id("foreign"))); err != nil {
		t.Fatalf("SetParent: %v", err)
	}

	// Act
	change, err := s.SetParent(context.Background(), f.byName["child"].ID, ToParent(nil))

	// Assert
	if err != nil {
		t.Fatalf("SetParent: %v", err)
	}
	if !change.Before.CrossRepository || change.Before.ParentName != "foreign" || change.After.CrossRepository {
		t.Fatalf("change = %+v, want a foreign parent before and none after", change)
	}
}

func TestSetParentOfAnUnknownWorkspaceIsNotFound(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	_, err := s.SetParent(context.Background(), "ws-missing", ToParent(nil))

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

func TestSetParentLandsWhereTheRetargetSays(t *testing.T) {
	tests := []struct {
		name string
		// arrange runs before the retarget under test.
		arrange func(t *testing.T, s *store, f parentFamily)
		// retarget is the retarget under test, of "child".
		retarget      func(f parentFamily) Retarget
		wantParent    string
		wantLandsInto string
	}{
		{name: "a child lands into its parent by default",
			retarget:   func(f parentFamily) Retarget { return ToParent(f.id("parent")) },
			wantParent: "parent", wantLandsInto: "parent"},
		{name: "a child lands on the primary branch when asked",
			retarget:   func(f parentFamily) Retarget { return Retarget{Parent: f.id("parent"), Landing: &LandingChoice{}} },
			wantParent: "parent"},
		{name: "a kept parent with the landing moved into another workspace",
			arrange: func(t *testing.T, s *store, f parentFamily) { f.retarget(t, s, "child", "parent") },
			retarget: func(f parentFamily) Retarget {
				return Retarget{KeepParent: true, Landing: &LandingChoice{Into: f.id("other")}}
			},
			wantParent: "parent", wantLandsInto: "other"},
		{name: "a peer landing into a workspace",
			retarget:      func(f parentFamily) Retarget { return Retarget{Landing: &LandingChoice{Into: f.id("other")}} },
			wantLandsInto: "other"},
		{name: "a peer lands on the primary branch by default",
			arrange:  func(t *testing.T, s *store, f parentFamily) { f.retarget(t, s, "child", "parent") },
			retarget: func(f parentFamily) Retarget { return ToParent(nil) }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			f := newParentFamily(t, s, "child", "parent", "other")
			if tt.arrange != nil {
				tt.arrange(t, s, f)
			}

			// Act
			change, err := s.SetParent(context.Background(), f.byName["child"].ID, tt.retarget(f))

			// Assert
			if err != nil {
				t.Fatalf("SetParent: %v", err)
			}
			wantDir := f.repoDir
			if tt.wantLandsInto != "" {
				wantDir = f.byName[tt.wantLandsInto].Dir
			}
			if ids.Optional(change.After.Parent) != ids.Optional(f.id(tt.wantParent)) || ids.Optional(change.After.LandsInto) != ids.Optional(f.id(tt.wantLandsInto)) || change.After.Dir != wantDir {
				t.Fatalf("after = %+v, want parent %q landing into %q at %q", change.After, tt.wantParent, tt.wantLandsInto, wantDir)
			}
			child, err := s.Workspace(context.Background(), f.byName["child"].ID)
			if err != nil {
				t.Fatalf("Workspace: %v", err)
			}
			if ids.Optional(child.Parent) != ids.Optional(f.id(tt.wantParent)) || ids.Optional(child.LandsInto) != ids.Optional(f.id(tt.wantLandsInto)) {
				t.Fatalf("read back parent %q lands into %q, want %q and %q", ids.Optional(child.Parent), ids.Optional(child.LandsInto), tt.wantParent, tt.wantLandsInto)
			}
			if _, got := targetOf(t, s, child.ID); got != wantDir {
				t.Fatalf("target_dir = %q, want %q", got, wantDir)
			}
		})
	}
}

func TestSetParentRefusesALandingTargetItCannotLandIn(t *testing.T) {
	tests := []struct {
		name string
		// into names the workspace to land into, arranging what it needs.
		into        func(t *testing.T, s *store, f parentFamily) WorkspaceID
		wantRefusal ParentRefusal
		wantFamily  bool
	}{
		{name: "an unregistered workspace", into: func(*testing.T, *store, parentFamily) WorkspaceID { return "ws-missing" },
			wantRefusal: ParentUnknownLandingTarget},
		{name: "a workspace of another repository", into: func(t *testing.T, s *store, _ parentFamily) WorkspaceID {
			return *newParentFamily(t, s, "foreign").id("foreign")
		}, wantRefusal: ParentUnknownLandingTarget},
		{name: "the workspace itself", into: func(_ *testing.T, _ *store, f parentFamily) WorkspaceID { return *f.id("child") },
			wantRefusal: ParentUnknownLandingTarget},
		{name: "a workspace landing into it", into: func(t *testing.T, s *store, f parentFamily) WorkspaceID {
			if _, err := s.SetParent(context.Background(), *f.id("other"), Retarget{Landing: &LandingChoice{Into: f.id("child")}}); err != nil {
				t.Fatalf("SetParent: %v", err)
			}
			return *f.id("other")
		}, wantRefusal: ParentUnknownLandingTarget},
		{name: "the repository's main workspace", into: func(t *testing.T, s *store, f parentFamily) WorkspaceID {
			main, err := s.WorkspaceByDir(context.Background(), f.repoDir)
			if err != nil {
				t.Fatalf("WorkspaceByDir: %v", err)
			}
			return main.ID
		}, wantRefusal: ParentUnknownLandingTarget},
		{name: "a closed workspace", into: func(t *testing.T, s *store, f parentFamily) WorkspaceID {
			mustClose(t, s, *f.id("other"), CloseByUser)
			return *f.id("other")
		}, wantFamily: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			f := newParentFamily(t, s, "child", "other")
			into := tt.into(t, s, f)
			_, before := targetOf(t, s, *f.id("child"))

			// Act
			_, err := s.SetParent(context.Background(), *f.id("child"), Retarget{Landing: &LandingChoice{Into: &into}})

			// Assert
			if tt.wantFamily {
				var notOpen *ParentNotOpenError
				if !errors.As(err, &notOpen) {
					t.Fatalf("err = %v, want a ParentNotOpenError", err)
				}
			} else if refused, ok := AsParentRefused(err); !ok || refused.Refusal != tt.wantRefusal {
				t.Fatalf("err = %v, want refusal %s", err, tt.wantRefusal)
			}
			if _, after := targetOf(t, s, *f.id("child")); after != before {
				t.Fatalf("target_dir = %q, want %q kept by the refusal", after, before)
			}
		})
	}
}
