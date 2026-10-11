package wsm

import (
	"context"
	"errors"
	"maps"
	"path/filepath"
	"slices"
	"sort"
	"testing"
)

// placeRepo is one repository directory shared by every workspace a place
// test registers, so they land relative to each other.
func placeRepo(t *testing.T) string {
	t.Helper()
	return t.TempDir()
}

// registerIn registers a workspace named name in repoDir with priority p.
func registerIn(t *testing.T, s *store, repoDir, name string, p *Priority) Workspace {
	t.Helper()
	ws, created, err := s.RegisterWorkspace(context.Background(), filepath.Join(t.TempDir(), name), RegisterFacts{
		Name: name, Branch: name, ParentBranch: "master", RepoDir: repoDir, Priority: p,
	})
	if err != nil {
		t.Fatalf("RegisterWorkspace(%s): %v", name, err)
	}
	if !created {
		t.Fatalf("RegisterWorkspace(%s) reported an existing record", name)
	}
	return ws
}

// placesByName reads every workspace's stored place, keyed by name.
func placesByName(t *testing.T, s *store) map[string]int64 {
	t.Helper()
	all, err := s.ListWorkspaces(context.Background())
	if err != nil {
		t.Fatalf("ListWorkspaces: %v", err)
	}
	out := map[string]int64{}
	for _, ws := range all {
		out[ws.Name] = ws.Place
	}
	return out
}

// orderedNames answers the names in ascending stored place.
func orderedNames(t *testing.T, s *store) []string {
	t.Helper()
	places := placesByName(t, s)
	names := slices.Collect(maps.Keys(places))
	sort.Slice(names, func(i, j int) bool { return places[names[i]] < places[names[j]] })
	return names
}

func priority(p Priority) *Priority { return &p }

func TestRegisterLandsWorkspacesOnPlaces(t *testing.T) {
	cases := []struct {
		name  string
		given []struct {
			name     string
			priority *Priority
		}
		want []string
	}{
		{
			name: "an unprioritized workspace lands after every place",
			given: []struct {
				name     string
				priority *Priority
			}{{"first", nil}, {"second", nil}},
			want: []string{"first", "second"},
		},
		{
			name: "a prioritized workspace lands before the first weaker one",
			given: []struct {
				name     string
				priority *Priority
			}{{"none", nil}, {"p3", priority(PriorityP3)}, {"p1", priority(PriorityP1)}},
			want: []string{"p1", "p3", "none"},
		},
		{
			name: "a workspace joins the end of its own priority's run",
			given: []struct {
				name     string
				priority *Priority
			}{{"p1-a", priority(PriorityP1)}, {"none", nil}, {"p1-b", priority(PriorityP1)}},
			want: []string{"p1-a", "p1-b", "none"},
		},
		{
			name: "a workspace with nothing weaker lands last",
			given: []struct {
				name     string
				priority *Priority
			}{{"p05", priority(PriorityP05)}, {"p3", priority(PriorityP3)}},
			want: []string{"p05", "p3"},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			s, _ := testStore(t)
			repoDir := placeRepo(t)

			// Act.
			for _, ws := range tc.given {
				registerIn(t, s, repoDir, ws.name, ws.priority)
			}

			// Assert.
			if got := orderedNames(t, s); !slices.Equal(got, tc.want) {
				t.Fatalf("place order = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestRegisterRecordsTheCreationPriority(t *testing.T) {
	// Arrange.
	s, _ := testStore(t)

	// Act.
	ws := registerIn(t, s, placeRepo(t), "p2", priority(PriorityP2))

	// Assert.
	got, err := s.Workspace(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if got.Priority == nil || *got.Priority != PriorityP2 {
		t.Fatalf("priority = %v, want P2", got.Priority)
	}
}

func TestRegisterRefusesAnUndeclaredPriorityWritingNothing(t *testing.T) {
	// Arrange.
	s, log := testStore(t)
	bogus := Priority(99)

	// Act.
	_, _, err := s.RegisterWorkspace(context.Background(), t.TempDir(), RegisterFacts{
		Name: "bogus", Branch: "b", RepoDir: placeRepo(t), Priority: &bogus,
	})

	// Assert.
	if err == nil {
		t.Fatal("RegisterWorkspace with an undeclared priority succeeded")
	}
	if got := scalar[int](t, s, `SELECT count(*) FROM workspaces`); got != 0 {
		t.Fatalf("%d workspaces written, want none", got)
	}
	if !loggedOperation(log, "daemon.wsm.register_workspace", "error") {
		t.Fatalf("the refusal was not logged at error: %v", log.Records())
	}
}

func TestSetPriorityLeavesThePlaceAlone(t *testing.T) {
	// Arrange.
	s, _ := testStore(t)
	repoDir := placeRepo(t)
	registerIn(t, s, repoDir, "first", nil)
	second := registerIn(t, s, repoDir, "second", nil)

	// Act.
	if err := s.SetPriority(context.Background(), second.ID, priority(PriorityP05)); err != nil {
		t.Fatalf("SetPriority: %v", err)
	}

	// Assert.
	if got := orderedNames(t, s); !slices.Equal(got, []string{"first", "second"}) {
		t.Fatalf("place order = %v, want the priority change to move nothing", got)
	}
}

func TestSwapPlacesExchangesTheTwoPlaces(t *testing.T) {
	// Arrange.
	s, _ := testStore(t)
	repoDir := placeRepo(t)
	a := registerIn(t, s, repoDir, "a", nil)
	registerIn(t, s, repoDir, "b", nil)
	c := registerIn(t, s, repoDir, "c", nil)

	// Act.
	if err := s.SwapPlaces(context.Background(), a.ID, c.ID); err != nil {
		t.Fatalf("SwapPlaces: %v", err)
	}

	// Assert.
	if got := orderedNames(t, s); !slices.Equal(got, []string{"c", "b", "a"}) {
		t.Fatalf("place order = %v, want [c b a]", got)
	}
}

func TestSwapPlacesRefusesAnUnknownWorkspaceWritingNothing(t *testing.T) {
	// Arrange.
	s, log := testStore(t)
	repoDir := placeRepo(t)
	a := registerIn(t, s, repoDir, "a", nil)
	registerIn(t, s, repoDir, "b", nil)
	before := placesByName(t, s)

	// Act.
	err := s.SwapPlaces(context.Background(), a.ID, "ws-missing")

	// Assert.
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("SwapPlaces error = %v, want ErrNotFound", err)
	}
	after := placesByName(t, s)
	for name, place := range before {
		if after[name] != place {
			t.Fatalf("place of %s moved from %d to %d on a refused swap", name, place, after[name])
		}
	}
	if !loggedOperation(log, "daemon.wsm.swap_places", "debug") {
		t.Fatalf("the refused swap was not recorded at debug: %v", log.Records())
	}
}

func TestSwapPlacesRefusesAWorkspaceWithItself(t *testing.T) {
	// Arrange.
	s, log := testStore(t)
	a := registerIn(t, s, placeRepo(t), "a", nil)

	// Act.
	err := s.SwapPlaces(context.Background(), a.ID, a.ID)

	// Assert.
	if err == nil {
		t.Fatal("SwapPlaces of a workspace with itself succeeded")
	}
	if !loggedOperation(log, "daemon.wsm.swap_places", "error") {
		t.Fatalf("the refusal was not logged at error: %v", log.Records())
	}
}

func TestReopeningAClosedWorkspaceMovesItLast(t *testing.T) {
	// Arrange: a P1 workspace first, behind it an unprioritized one; the P1
	// workspace is closed.
	s, log := testStore(t)
	repoDir := placeRepo(t)
	p1 := registerIn(t, s, repoDir, "p1", priority(PriorityP1))
	registerIn(t, s, repoDir, "none", nil)
	if err := s.SetClosed(context.Background(), p1.ID, true); err != nil {
		t.Fatalf("SetClosed(true): %v", err)
	}

	// Act.
	if err := s.SetClosed(context.Background(), p1.ID, false); err != nil {
		t.Fatalf("SetClosed(false): %v", err)
	}

	// Assert: the re-opened workspace goes last, whatever its priority.
	if got := orderedNames(t, s); !slices.Equal(got, []string{"none", "p1"}) {
		t.Fatalf("place order = %v, want [none p1]", got)
	}
	if loggedOperation(log, "daemon.wsm.set_closed", "error") {
		t.Fatalf("the re-open logged an error: %v", log.Records())
	}
}

func TestMarkingAnOpenWorkspaceOpenMovesNothing(t *testing.T) {
	// Arrange: a P1 workspace the user swapped behind an unprioritized one.
	s, _ := testStore(t)
	repoDir := placeRepo(t)
	p1 := registerIn(t, s, repoDir, "p1", priority(PriorityP1))
	none := registerIn(t, s, repoDir, "none", nil)
	if err := s.SwapPlaces(context.Background(), p1.ID, none.ID); err != nil {
		t.Fatalf("SwapPlaces: %v", err)
	}

	// Act.
	if err := s.SetClosed(context.Background(), p1.ID, false); err != nil {
		t.Fatalf("SetClosed(false): %v", err)
	}

	// Assert.
	if got := orderedNames(t, s); !slices.Equal(got, []string{"none", "p1"}) {
		t.Fatalf("place order = %v, want the user's order kept", got)
	}
}

func TestReopeningAnUnknownWorkspaceIsNotFound(t *testing.T) {
	// Arrange.
	s, _ := testStore(t)

	// Act.
	err := s.SetClosed(context.Background(), "ws-missing", false)

	// Assert.
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("SetClosed error = %v, want ErrNotFound", err)
	}
}

func TestAnInsertStatingNoPlaceLandsAfterEveryPlace(t *testing.T) {
	// Arrange: the build before places writes rows without one.
	s, _ := testStore(t)
	ws := registerIn(t, s, placeRepo(t), "placed", nil)

	// Act.
	corrupt(t, s, `INSERT INTO workspaces (id, repo_id, dir, name, branch, parent_branch, closed, attention, is_current, created_at)
		VALUES ('ws-unplaced', ?, '/tmp/unplaced', 'unplaced', 'u', 'master', 0, 0, 0, 1)`, string(ws.Repo))

	// Assert.
	if got := orderedNames(t, s); !slices.Equal(got, []string{"placed", "unplaced"}) {
		t.Fatalf("place order = %v, want the unplaced row last", got)
	}
}

func TestTheSchemaRefusesTwoWorkspacesOnOnePlace(t *testing.T) {
	// Arrange.
	s, _ := testStore(t)
	repoDir := placeRepo(t)
	a := registerIn(t, s, repoDir, "a", nil)
	b := registerIn(t, s, repoDir, "b", nil)

	// Act.
	_, err := s.db().ExecContext(context.Background(), `UPDATE workspaces SET place = ? WHERE id = ?`, a.Place, b.ID)

	// Assert.
	if err == nil {
		t.Fatal("the schema let two workspaces share one place")
	}
}

func TestTheMigrationSeedsPlacesFromTheOrderBeforePlaces(t *testing.T) {
	// Arrange: a layout-3 file with workspaces of mixed priority.
	path := layout3Fixture(t)
	for _, row := range []struct {
		id, name string
		priority any
	}{
		{"ws-p1", "zeta", int(PriorityP1)},
		{"ws-p05", "omega", int(PriorityP05)},
		{"ws-none", "aaa", nil},
	} {
		execRaw(t, path, `INSERT INTO workspaces (id, repo_id, dir, name, branch, parent_branch, closed, attention, priority, is_current, created_at)
			VALUES (?, 'repo-1', ?, ?, 'b', 'master', 0, 0, ?, 0, 1)`, row.id, "/tmp/"+row.id, row.name, row.priority)
	}

	// Act.
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	defer handle.Close()

	// Assert: priority strongest first, then name.
	want := []string{"omega", "zeta", "aaa", fixtureWorkspaceName}
	if fixtureWorkspaceName < "aaa" {
		want = []string{"omega", "zeta", fixtureWorkspaceName, "aaa"}
	}
	if got := orderedNames(t, handle.(*store)); !slices.Equal(got, want) {
		t.Fatalf("seeded place order = %v, want %v", got, want)
	}
}
