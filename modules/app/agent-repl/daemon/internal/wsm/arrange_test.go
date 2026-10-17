package wsm

import (
	"context"
	"errors"
	"path/filepath"
	"slices"
	"testing"

	"agent-repld/internal/dlog"
)

// hasOperation reports whether LOG holds a record under OPERATION.
func hasOperation(log *dlog.TestLogger, operation string) bool {
	_, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Operation: operation})
	return ok
}

// arrangeRepo names a repository directory whose base name is NAME, so the
// repository's drawn name is NAME.
func arrangeRepo(t *testing.T, name string) string {
	t.Helper()
	return filepath.Join(t.TempDir(), name)
}

// registerChild registers a workspace named NAME in repoDir whose recorded
// parent is PARENT.
func registerChild(t *testing.T, s *store, repoDir, name string, parent WorkspaceID) Workspace {
	t.Helper()
	ws, _, err := s.RegisterWorkspace(context.Background(), filepath.Join(t.TempDir(), name), RegisterFacts{
		Name: name, Branch: name, ParentBranch: "master", RepoDir: repoDir, Parent: &parent,
	})
	if err != nil {
		t.Fatalf("RegisterWorkspace(%s): %v", name, err)
	}
	return ws
}

// mustSubsection creates a subsection named NAME in REPO.
func mustSubsection(t *testing.T, s *store, repo RepoID, name string) Subsection {
	t.Helper()
	sub, err := s.CreateSubsection(context.Background(), repo, name)
	if err != nil {
		t.Fatalf("CreateSubsection(%s): %v", name, err)
	}
	return sub
}

// mustPlace puts WS into INTO before BEFORE.
func mustPlace(t *testing.T, s *store, ws WorkspaceID, into *SubsectionID, before *WorkspaceID) {
	t.Helper()
	if err := s.PlaceWorkspace(context.Background(), ws, into, before); err != nil {
		t.Fatalf("PlaceWorkspace(%s): %v", ws, err)
	}
}

// repositoryNames answers the repositories' names in their stored order.
func repositoryNames(t *testing.T, s *store) []string {
	t.Helper()
	repos, err := s.ListRepositories(context.Background())
	if err != nil {
		t.Fatalf("ListRepositories: %v", err)
	}
	var out []string
	for _, repo := range repos {
		out = append(out, repo.Name)
	}
	return out
}

// subsectionNames answers REPO's subsections' names in their stored order.
func subsectionNames(t *testing.T, s *store, repo RepoID) []string {
	t.Helper()
	subs, err := s.ListSubsections(context.Background())
	if err != nil {
		t.Fatalf("ListSubsections: %v", err)
	}
	var out []string
	for _, sub := range subs {
		if sub.Repo == repo {
			out = append(out, sub.Name)
		}
	}
	return out
}

// subsectionOf answers the subsection WS sits in, nil for none.
func subsectionOf(t *testing.T, s *store, ws WorkspaceID) *SubsectionID {
	t.Helper()
	got, err := s.Workspace(context.Background(), ws)
	if err != nil {
		t.Fatalf("Workspace(%s): %v", ws, err)
	}
	return got.Subsection
}

func sameSubsection(a, b *SubsectionID) bool {
	if a == nil || b == nil {
		return a == b
	}
	return *a == *b
}

func TestTheArrangementMigrationPlacesRepositoriesInTheirOldDrawnOrder(t *testing.T) {
	// Arrange -- the roster drew repositories by name before they had a place.
	path := layout3Fixture(t)
	execRaw(t, path, `INSERT INTO repositories (id, dir, name, default_branch) VALUES ('repo-z', '/tmp/z', 'zeta', 'master')`)
	execRaw(t, path, `INSERT INTO repositories (id, dir, name, default_branch) VALUES ('repo-a', '/tmp/a', 'alpha', 'master')`)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-3 database: %v", err)
	}
	defer handle.Close()

	// Assert
	if got, want := repositoryNames(t, handle.(*store)), []string{"alpha", "repo", "zeta"}; !slices.Equal(got, want) {
		t.Fatalf("repository order after the migration = %v, want %v", got, want)
	}
}

func TestTheArrangementMigrationAddsTheSubsectionsTable(t *testing.T) {
	// Arrange
	path := layout3Fixture(t)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-3 database: %v", err)
	}
	defer handle.Close()

	// Assert
	got := scalar[int](t, handle.(*store), `SELECT count(*) FROM sqlite_master WHERE type = 'table' AND name = 'subsections'`)
	if got != 1 {
		t.Fatalf("subsections exists %d times after the migration, want 1", got)
	}
}

func TestTheArrangementMigrationLeavesEveryWorkspaceInNoSubsection(t *testing.T) {
	// Arrange
	path := layout3Fixture(t)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-3 database: %v", err)
	}
	defer handle.Close()

	// Assert
	ws, err := handle.Workspace(context.Background(), fixtureWorkspaceID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if ws.Subsection != nil {
		t.Fatalf("the migrated workspace sits in subsection %q, want none", *ws.Subsection)
	}
}

func TestANewRepositoryLandsAfterEveryOther(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	registerIn(t, s, arrangeRepo(t, "zeta"), "z1", nil)

	// Act
	registerIn(t, s, arrangeRepo(t, "alpha"), "a1", nil)

	// Assert
	if got, want := repositoryNames(t, s), []string{"zeta", "alpha"}; !slices.Equal(got, want) {
		t.Fatalf("repository order = %v, want %v", got, want)
	}
}

func TestARepositoryInsertedWithNoPlaceLandsLast(t *testing.T) {
	// Arrange -- the build before places, still writing during a handover.
	s, _ := testStore(t)
	registerIn(t, s, arrangeRepo(t, "first"), "f1", nil)

	// Act
	if _, err := s.db().ExecContext(context.Background(),
		`INSERT INTO repositories (id, dir, name, default_branch) VALUES ('old-build', '/tmp/old', 'old', 'master')`); err != nil {
		t.Fatalf("insert an unplaced repository: %v", err)
	}

	// Assert
	if got, want := repositoryNames(t, s), []string{"first", "old"}; !slices.Equal(got, want) {
		t.Fatalf("repository order = %v, want %v", got, want)
	}
}

func TestCreateSubsectionLandsAfterTheRepositorysOthers(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := registerIn(t, s, arrangeRepo(t, "repo"), "w", nil)

	// Act
	mustSubsection(t, s, ws.Repo, "first")
	mustSubsection(t, s, ws.Repo, "second")

	// Assert
	if got, want := subsectionNames(t, s, ws.Repo), []string{"first", "second"}; !slices.Equal(got, want) {
		t.Fatalf("subsections = %v, want %v", got, want)
	}
}

func TestCreateSubsectionTrimsTheName(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := registerIn(t, s, arrangeRepo(t, "repo"), "w", nil)

	// Act
	sub := mustSubsection(t, s, ws.Repo, "  later  ")

	// Assert
	if sub.Name != "later" {
		t.Fatalf("name = %q, want %q", sub.Name, "later")
	}
}

func TestCreateSubsectionRefusals(t *testing.T) {
	tests := []struct {
		name    string
		repo    func(Workspace) RepoID
		subName string
		want    error
	}{
		{name: "a blank name", repo: func(ws Workspace) RepoID { return ws.Repo }, subName: "   ", want: ErrBlankSubsectionName},
		{name: "an unknown repository", repo: func(Workspace) RepoID { return "absent" }, subName: "later", want: ErrUnknownRepository},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			ws := registerIn(t, s, arrangeRepo(t, "repo"), "w", nil)

			// Act
			_, err := s.CreateSubsection(context.Background(), tt.repo(ws), tt.subName)

			// Assert
			if !errors.Is(err, tt.want) {
				t.Fatalf("CreateSubsection = %v, want %v", err, tt.want)
			}
			if got := scalar[int](t, s, `SELECT count(*) FROM subsections`); got != 0 {
				t.Fatalf("subsections after a refusal = %d, want 0", got)
			}
			if !hasOperation(log, "daemon.wsm.create_subsection") {
				t.Fatalf("no daemon.wsm.create_subsection record: %v", log.Records())
			}
		})
	}
}

func TestRenameSubsectionRelabelsIt(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := registerIn(t, s, arrangeRepo(t, "repo"), "w", nil)
	sub := mustSubsection(t, s, ws.Repo, "old")

	// Act
	err := s.RenameSubsection(context.Background(), sub.ID, " new ")

	// Assert
	if err != nil {
		t.Fatalf("RenameSubsection: %v", err)
	}
	if got, want := subsectionNames(t, s, ws.Repo), []string{"new"}; !slices.Equal(got, want) {
		t.Fatalf("subsections = %v, want %v", got, want)
	}
}

func TestRenameSubsectionRefusals(t *testing.T) {
	tests := []struct {
		name    string
		id      func(Subsection) SubsectionID
		subName string
		want    error
	}{
		{name: "a blank name", id: func(sub Subsection) SubsectionID { return sub.ID }, subName: "", want: ErrBlankSubsectionName},
		{name: "an unknown subsection", id: func(Subsection) SubsectionID { return "absent" }, subName: "new", want: ErrUnknownSubsection},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := registerIn(t, s, arrangeRepo(t, "repo"), "w", nil)
			sub := mustSubsection(t, s, ws.Repo, "old")

			// Act
			err := s.RenameSubsection(context.Background(), tt.id(sub), tt.subName)

			// Assert
			if !errors.Is(err, tt.want) {
				t.Fatalf("RenameSubsection = %v, want %v", err, tt.want)
			}
			if got, want := subsectionNames(t, s, ws.Repo), []string{"old"}; !slices.Equal(got, want) {
				t.Fatalf("subsections after a refusal = %v, want %v", got, want)
			}
		})
	}
}

func TestDeleteSubsectionReturnsItsWorkspacesAfterTheUnsectionedOnes(t *testing.T) {
	// Arrange -- a, b unsectioned; c, d moved into "later" in that order.
	s, _ := testStore(t)
	repoDir := arrangeRepo(t, "repo")
	a := registerIn(t, s, repoDir, "a", nil)
	registerIn(t, s, repoDir, "b", nil)
	c := registerIn(t, s, repoDir, "c", nil)
	d := registerIn(t, s, repoDir, "d", nil)
	later := mustSubsection(t, s, a.Repo, "later")
	mustPlace(t, s, c.ID, &later.ID, nil)
	mustPlace(t, s, d.ID, &later.ID, nil)
	mustPlace(t, s, a.ID, nil, nil)

	// Act
	err := s.DeleteSubsection(context.Background(), later.ID)

	// Assert
	if err != nil {
		t.Fatalf("DeleteSubsection: %v", err)
	}
	if got, want := orderedNames(t, s), []string{"b", "a", "c", "d"}; !slices.Equal(got, want) {
		t.Fatalf("place order = %v, want %v", got, want)
	}
	for _, ws := range []WorkspaceID{c.ID, d.ID} {
		if got := subsectionOf(t, s, ws); got != nil {
			t.Fatalf("workspace %s still sits in %q", ws, *got)
		}
	}
}

func TestDeleteSubsectionClosesTheGapInPlaces(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := registerIn(t, s, arrangeRepo(t, "repo"), "w", nil)
	first := mustSubsection(t, s, ws.Repo, "first")
	mustSubsection(t, s, ws.Repo, "second")

	// Act
	if err := s.DeleteSubsection(context.Background(), first.ID); err != nil {
		t.Fatalf("DeleteSubsection: %v", err)
	}
	third := mustSubsection(t, s, ws.Repo, "third")

	// Assert
	if third.Place != 2 {
		t.Fatalf("the next subsection's place = %d, want 2", third.Place)
	}
}

func TestDeleteSubsectionRefusesAnUnknownOne(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.DeleteSubsection(context.Background(), "absent")

	// Assert
	if !errors.Is(err, ErrUnknownSubsection) {
		t.Fatalf("DeleteSubsection = %v, want ErrUnknownSubsection", err)
	}
}

func TestMoveSubsection(t *testing.T) {
	tests := []struct {
		name   string
		move   string
		before string
		want   []string
	}{
		{name: "before an earlier one", move: "c", before: "a", want: []string{"c", "a", "b"}},
		{name: "before a later one", move: "a", before: "c", want: []string{"b", "a", "c"}},
		{name: "to the end", move: "a", want: []string{"b", "c", "a"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := registerIn(t, s, arrangeRepo(t, "repo"), "w", nil)
			byName := map[string]SubsectionID{}
			for _, name := range []string{"a", "b", "c"} {
				byName[name] = mustSubsection(t, s, ws.Repo, name).ID
			}
			var before *SubsectionID
			if tt.before != "" {
				id := byName[tt.before]
				before = &id
			}

			// Act
			err := s.MoveSubsection(context.Background(), byName[tt.move], before)

			// Assert
			if err != nil {
				t.Fatalf("MoveSubsection: %v", err)
			}
			if got := subsectionNames(t, s, ws.Repo); !slices.Equal(got, tt.want) {
				t.Fatalf("subsections = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestMoveSubsectionRefusals(t *testing.T) {
	tests := []struct {
		name   string
		id     func(own, other Subsection) SubsectionID
		before func(own, other Subsection) *SubsectionID
		want   error
	}{
		{
			name:   "before another repository's subsection",
			id:     func(own, _ Subsection) SubsectionID { return own.ID },
			before: func(_, other Subsection) *SubsectionID { return &other.ID },
			want:   ErrOtherRepository,
		},
		{
			name:   "an unknown subsection",
			id:     func(Subsection, Subsection) SubsectionID { return "absent" },
			before: func(Subsection, Subsection) *SubsectionID { return nil },
			want:   ErrUnknownSubsection,
		},
		{
			name:   "before an unknown subsection",
			id:     func(own, _ Subsection) SubsectionID { return own.ID },
			before: func(Subsection, Subsection) *SubsectionID { absent := SubsectionID("absent"); return &absent },
			want:   ErrUnknownSubsection,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			mine := registerIn(t, s, arrangeRepo(t, "mine"), "m", nil)
			theirs := registerIn(t, s, arrangeRepo(t, "theirs"), "t", nil)
			own := mustSubsection(t, s, mine.Repo, "own")
			mustSubsection(t, s, mine.Repo, "own-2")
			other := mustSubsection(t, s, theirs.Repo, "other")

			// Act
			err := s.MoveSubsection(context.Background(), tt.id(own, other), tt.before(own, other))

			// Assert
			if !errors.Is(err, tt.want) {
				t.Fatalf("MoveSubsection = %v, want %v", err, tt.want)
			}
			if got, want := subsectionNames(t, s, mine.Repo), []string{"own", "own-2"}; !slices.Equal(got, want) {
				t.Fatalf("subsections after a refusal = %v, want %v", got, want)
			}
		})
	}
}

func TestMoveSubsectionRefusesMovingBeforeItself(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := registerIn(t, s, arrangeRepo(t, "repo"), "w", nil)
	sub := mustSubsection(t, s, ws.Repo, "a")

	// Act
	err := s.MoveSubsection(context.Background(), sub.ID, &sub.ID)

	// Assert
	if err == nil {
		t.Fatalf("MoveSubsection before itself succeeded")
	}
	if !hasOperation(log, "daemon.wsm.move_subsection") {
		t.Fatalf("no daemon.wsm.move_subsection record: %v", log.Records())
	}
}

func TestMoveRepository(t *testing.T) {
	tests := []struct {
		name   string
		move   string
		before string
		want   []string
	}{
		{name: "before an earlier one", move: "c", before: "a", want: []string{"c", "a", "b"}},
		{name: "before a later one", move: "a", before: "c", want: []string{"b", "a", "c"}},
		{name: "to the end", move: "a", want: []string{"b", "c", "a"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			byName := map[string]RepoID{}
			for _, name := range []string{"a", "b", "c"} {
				byName[name] = registerIn(t, s, arrangeRepo(t, name), "w-"+name, nil).Repo
			}
			var before *RepoID
			if tt.before != "" {
				id := byName[tt.before]
				before = &id
			}

			// Act
			err := s.MoveRepository(context.Background(), byName[tt.move], before)

			// Assert
			if err != nil {
				t.Fatalf("MoveRepository: %v", err)
			}
			if got := repositoryNames(t, s); !slices.Equal(got, tt.want) {
				t.Fatalf("repositories = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestMoveRepositoryRefusesAnUnknownRepository(t *testing.T) {
	tests := []struct {
		name   string
		id     func(RepoID) RepoID
		before func(RepoID) *RepoID
	}{
		{name: "the moved one", id: func(RepoID) RepoID { return "absent" }, before: func(RepoID) *RepoID { return nil }},
		{name: "the anchor", id: func(own RepoID) RepoID { return own }, before: func(RepoID) *RepoID { absent := RepoID("absent"); return &absent }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			own := registerIn(t, s, arrangeRepo(t, "a"), "w", nil).Repo
			registerIn(t, s, arrangeRepo(t, "b"), "x", nil)

			// Act
			err := s.MoveRepository(context.Background(), tt.id(own), tt.before(own))

			// Assert
			if !errors.Is(err, ErrUnknownRepository) {
				t.Fatalf("MoveRepository = %v, want ErrUnknownRepository", err)
			}
			if got, want := repositoryNames(t, s), []string{"a", "b"}; !slices.Equal(got, want) {
				t.Fatalf("repositories after a refusal = %v, want %v", got, want)
			}
		})
	}
}

func TestPlaceWorkspaceIntoAnEmptySubsectionPutsItLast(t *testing.T) {
	// Arrange -- the owner's example: the FIRST workspace dragged into a new,
	// empty subsection at the bottom of its repository.
	s, _ := testStore(t)
	repoDir := arrangeRepo(t, "repo")
	first := registerIn(t, s, repoDir, "first", nil)
	registerIn(t, s, repoDir, "second", nil)
	later := mustSubsection(t, s, first.Repo, "later")

	// Act
	err := s.PlaceWorkspace(context.Background(), first.ID, &later.ID, nil)

	// Assert
	if err != nil {
		t.Fatalf("PlaceWorkspace: %v", err)
	}
	if got := subsectionOf(t, s, first.ID); !sameSubsection(got, &later.ID) {
		t.Fatalf("subsection = %v, want %q", got, later.ID)
	}
	if got, want := orderedNames(t, s), []string{"second", "first"}; !slices.Equal(got, want) {
		t.Fatalf("place order = %v, want %v", got, want)
	}
}

func TestPlaceWorkspaceBeforeAnAnchor(t *testing.T) {
	tests := []struct {
		name   string
		move   string
		before string
		want   []string
	}{
		{name: "before an earlier workspace", move: "c", before: "a", want: []string{"c", "a", "b"}},
		{name: "before a later workspace", move: "a", before: "c", want: []string{"b", "a", "c"}},
		{name: "after every workspace", move: "a", want: []string{"b", "c", "a"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repoDir := arrangeRepo(t, "repo")
			byName := map[string]WorkspaceID{}
			for _, name := range []string{"a", "b", "c"} {
				byName[name] = registerIn(t, s, repoDir, name, nil).ID
			}
			// Registration lands second-to-last; line them up a, b, c.
			mustPlace(t, s, byName["b"], nil, nil)
			mustPlace(t, s, byName["c"], nil, nil)
			var before *WorkspaceID
			if tt.before != "" {
				id := byName[tt.before]
				before = &id
			}

			// Act
			err := s.PlaceWorkspace(context.Background(), byName[tt.move], nil, before)

			// Assert
			if err != nil {
				t.Fatalf("PlaceWorkspace: %v", err)
			}
			if got := orderedNames(t, s); !slices.Equal(got, tt.want) {
				t.Fatalf("place order = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestPlaceWorkspaceMovesItsNestedFamilyWithIt(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repoDir := arrangeRepo(t, "repo")
	parent := registerIn(t, s, repoDir, "parent", nil)
	child := registerChild(t, s, repoDir, "child", parent.ID)
	grandchild := registerChild(t, s, repoDir, "grandchild", child.ID)
	bystander := registerIn(t, s, repoDir, "bystander", nil)
	later := mustSubsection(t, s, parent.Repo, "later")

	// Act
	mustPlace(t, s, parent.ID, &later.ID, nil)

	// Assert
	for _, ws := range []WorkspaceID{parent.ID, child.ID, grandchild.ID} {
		if got := subsectionOf(t, s, ws); !sameSubsection(got, &later.ID) {
			t.Fatalf("workspace %s sits in %v, want %q", ws, got, later.ID)
		}
	}
	if got := subsectionOf(t, s, bystander.ID); got != nil {
		t.Fatalf("the bystander moved into %q", *got)
	}
}

func TestPlaceWorkspaceLeavesAFamilyMemberInAnotherSubsectionWhereItIs(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repoDir := arrangeRepo(t, "repo")
	parent := registerIn(t, s, repoDir, "parent", nil)
	child := registerChild(t, s, repoDir, "child", parent.ID)
	aside := mustSubsection(t, s, parent.Repo, "aside")
	later := mustSubsection(t, s, parent.Repo, "later")
	mustPlace(t, s, child.ID, &aside.ID, nil)

	// Act
	mustPlace(t, s, parent.ID, &later.ID, nil)

	// Assert
	if got := subsectionOf(t, s, child.ID); !sameSubsection(got, &aside.ID) {
		t.Fatalf("the child sits in %v, want %q", got, aside.ID)
	}
}

func TestPlaceWorkspaceRefusals(t *testing.T) {
	type world struct {
		mover, anchor, foreignWs Workspace
		own, foreign             Subsection
	}
	tests := []struct {
		name   string
		ws     func(world) WorkspaceID
		into   func(world) *SubsectionID
		before func(world) *WorkspaceID
		want   error
	}{
		{
			name:   "into another repository's subsection",
			ws:     func(w world) WorkspaceID { return w.mover.ID },
			into:   func(w world) *SubsectionID { return &w.foreign.ID },
			before: func(world) *WorkspaceID { return nil },
			want:   ErrOtherRepository,
		},
		{
			name:   "into an unknown subsection",
			ws:     func(w world) WorkspaceID { return w.mover.ID },
			into:   func(world) *SubsectionID { absent := SubsectionID("absent"); return &absent },
			before: func(world) *WorkspaceID { return nil },
			want:   ErrUnknownSubsection,
		},
		{
			name:   "before a workspace outside the destination",
			ws:     func(w world) WorkspaceID { return w.mover.ID },
			into:   func(w world) *SubsectionID { return &w.own.ID },
			before: func(w world) *WorkspaceID { return &w.anchor.ID },
			want:   ErrAnchorElsewhere,
		},
		{
			name:   "before another repository's workspace",
			ws:     func(w world) WorkspaceID { return w.mover.ID },
			into:   func(world) *SubsectionID { return nil },
			before: func(w world) *WorkspaceID { return &w.foreignWs.ID },
			want:   ErrAnchorElsewhere,
		},
		{
			name:   "an unknown workspace",
			ws:     func(world) WorkspaceID { return "absent" },
			into:   func(world) *SubsectionID { return nil },
			before: func(world) *WorkspaceID { return nil },
			want:   ErrNotFound,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repoDir := arrangeRepo(t, "repo")
			w := world{mover: registerIn(t, s, repoDir, "mover", nil), anchor: registerIn(t, s, repoDir, "anchor", nil)}
			w.foreignWs = registerIn(t, s, arrangeRepo(t, "foreign"), "foreign-ws", nil)
			w.own = mustSubsection(t, s, w.mover.Repo, "own")
			w.foreign = mustSubsection(t, s, w.foreignWs.Repo, "foreign")
			order := orderedNames(t, s)

			// Act
			err := s.PlaceWorkspace(context.Background(), tt.ws(w), tt.into(w), tt.before(w))

			// Assert
			if !errors.Is(err, tt.want) {
				t.Fatalf("PlaceWorkspace = %v, want %v", err, tt.want)
			}
			if got := orderedNames(t, s); !slices.Equal(got, order) {
				t.Fatalf("place order after a refusal = %v, want %v", got, order)
			}
			if got := subsectionOf(t, s, w.mover.ID); got != nil {
				t.Fatalf("the refused workspace moved into %q", *got)
			}
		})
	}
}

func TestPlaceWorkspaceRecordsARefusal(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := registerIn(t, s, arrangeRepo(t, "repo"), "w", nil)
	other := registerIn(t, s, arrangeRepo(t, "other"), "o", nil)
	foreign := mustSubsection(t, s, other.Repo, "foreign")

	// Act
	err := s.PlaceWorkspace(context.Background(), ws.ID, &foreign.ID, nil)

	// Assert
	if !errors.Is(err, ErrOtherRepository) {
		t.Fatalf("PlaceWorkspace = %v, want ErrOtherRepository", err)
	}
	rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Operation: "daemon.wsm.place_workspace", Message: "the arrangement was refused; nothing was written"})
	if !ok {
		t.Fatalf("no daemon.wsm.place_workspace record: %v", log.Records())
	}
	if rec.Context["workspace"] != string(ws.ID) || rec.Context["into"] != string(foreign.ID) {
		t.Fatalf("the refusal's record context = %v", rec.Context)
	}
}

func TestTheStoreRefusesAWorkspaceInAnotherRepositorysSubsection(t *testing.T) {
	// Arrange -- a write past every verb, straight at SQLite.
	s, _ := testStore(t)
	ws := registerIn(t, s, arrangeRepo(t, "repo"), "w", nil)
	other := registerIn(t, s, arrangeRepo(t, "other"), "o", nil)
	foreign := mustSubsection(t, s, other.Repo, "foreign")

	// Act
	_, err := s.db().ExecContext(context.Background(), `UPDATE workspaces SET subsection_id = ? WHERE id = ?`, foreign.ID, ws.ID)

	// Assert
	if err == nil {
		t.Fatalf("SQLite accepted a workspace in another repository's subsection")
	}
}

func TestSwapPlacesAcrossRepositoriesExchangesPlacesAlone(t *testing.T) {
	// Arrange -- any two workspaces may swap (owner ruling, 2026-10-17).
	s, _ := testStore(t)
	ws := registerIn(t, s, arrangeRepo(t, "repo"), "w", nil)
	other := registerIn(t, s, arrangeRepo(t, "other"), "o", nil)
	later := mustSubsection(t, s, ws.Repo, "later")
	mustPlace(t, s, ws.ID, &later.ID, nil)
	before := placesByName(t, s)

	// Act
	err := s.SwapPlaces(context.Background(), ws.ID, other.ID)

	// Assert
	if err != nil {
		t.Fatalf("SwapPlaces: %v", err)
	}
	got := placesByName(t, s)
	if got["w"] != before["o"] || got["o"] != before["w"] {
		t.Fatalf("places = %v, want w and o exchanged from %v", got, before)
	}
	if sub := subsectionOf(t, s, ws.ID); !sameSubsection(sub, &later.ID) {
		t.Fatalf("w's subsection = %v, want it kept as %q", sub, later.ID)
	}
}

func TestSwapPlacesExchangesTheSubsectionsOfTwoRoots(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repoDir := arrangeRepo(t, "repo")
	ws := registerIn(t, s, repoDir, "w", nil)
	target := registerIn(t, s, repoDir, "target", nil)
	later := mustSubsection(t, s, ws.Repo, "later")
	mustPlace(t, s, target.ID, &later.ID, nil)

	// Act
	err := s.SwapPlaces(context.Background(), ws.ID, target.ID)

	// Assert
	if err != nil {
		t.Fatalf("SwapPlaces: %v", err)
	}
	if got := subsectionOf(t, s, ws.ID); !sameSubsection(got, &later.ID) {
		t.Fatalf("w's subsection = %v, want %q", got, later.ID)
	}
	if got := subsectionOf(t, s, target.ID); got != nil {
		t.Fatalf("target's subsection = %q, want none", *got)
	}
}

func TestAChildLandsInItsParentsSubsection(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repoDir := arrangeRepo(t, "repo")
	parent := registerIn(t, s, repoDir, "parent", nil)
	later := mustSubsection(t, s, parent.Repo, "later")
	mustPlace(t, s, parent.ID, &later.ID, nil)

	// Act
	child := registerChild(t, s, repoDir, "child", parent.ID)

	// Assert
	if !sameSubsection(child.Subsection, &later.ID) {
		t.Fatalf("the registered child reports subsection %v, want %q", child.Subsection, later.ID)
	}
	if got := subsectionOf(t, s, child.ID); !sameSubsection(got, &later.ID) {
		t.Fatalf("the stored child sits in %v, want %q", got, later.ID)
	}
}

func TestListSubsectionsOrdersEachRepositorysByPlace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := registerIn(t, s, arrangeRepo(t, "repo"), "w", nil)
	a := mustSubsection(t, s, ws.Repo, "a")
	mustSubsection(t, s, ws.Repo, "b")

	// Act
	if err := s.MoveSubsection(context.Background(), a.ID, nil); err != nil {
		t.Fatalf("MoveSubsection: %v", err)
	}

	// Assert
	if got, want := subsectionNames(t, s, ws.Repo), []string{"b", "a"}; !slices.Equal(got, want) {
		t.Fatalf("subsections = %v, want %v", got, want)
	}
}

func TestReadArrangementReadsTheWorkspacesRepositoriesAndSubsections(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := registerIn(t, s, arrangeRepo(t, "repo"), "w", nil)
	later := mustSubsection(t, s, ws.Repo, "later")
	mustPlace(t, s, ws.ID, &later.ID, nil)

	// Act
	got, err := s.ReadArrangement(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("ReadArrangement: %v", err)
	}
	if len(got.Workspaces) != 1 || !sameSubsection(got.Workspaces[0].Subsection, &later.ID) {
		t.Fatalf("workspaces = %+v, want the one workspace in %q", got.Workspaces, later.ID)
	}
	if len(got.Repositories) != 1 || got.Repositories[0].ID != ws.Repo {
		t.Fatalf("repositories = %+v, want the one repository %q", got.Repositories, ws.Repo)
	}
	if len(got.Subsections) != 1 || got.Subsections[0] != later {
		t.Fatalf("subsections = %+v, want [%+v]", got.Subsections, later)
	}
}

func TestAnUnknownSubsectionIsNamedByItsError(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.RenameSubsection(context.Background(), "absent", "new")

	// Assert
	var unknown *UnknownSubsectionError
	if !errors.As(err, &unknown) || unknown.ID != "absent" {
		t.Fatalf("RenameSubsection = %v, want an UnknownSubsectionError naming absent", err)
	}
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("RenameSubsection = %v, want it to be ErrNotFound too", err)
	}
}
