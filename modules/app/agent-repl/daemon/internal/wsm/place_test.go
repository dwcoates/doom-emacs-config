package wsm

import (
	"context"
	"errors"
	"maps"
	"path/filepath"
	"slices"
	"sort"
	"testing"
	"time"
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
	registerMain(t, s, repoDir)
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

// placesByName reads every linked workspace's stored place, keyed by name.
func placesByName(t *testing.T, s *store) map[string]int64 {
	t.Helper()
	all, err := s.ListWorkspaces(context.Background())
	if err != nil {
		t.Fatalf("ListWorkspaces: %v", err)
	}
	out := map[string]int64{}
	for _, ws := range withoutMainWorktrees(t, s, all) {
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

// landing is one workspace a landing test registers, closed or not.
type landing struct {
	name     string
	priority *Priority
	closed   bool
}

func TestRegisterLandsWorkspacesOnPlaces(t *testing.T) {
	cases := []struct {
		name  string
		given []landing
		want  []string
	}{
		{
			name:  "the first workspace takes the first place",
			given: []landing{{name: "only"}},
			want:  []string{"only"},
		},
		{
			name:  "with one open workspace the new one lands after it",
			given: []landing{{name: "first"}, {name: "second"}},
			want:  []string{"first", "second"},
		},
		{
			name:  "with two or more open the new one lands second-to-last",
			given: []landing{{name: "a"}, {name: "b"}, {name: "new"}},
			want:  []string{"a", "new", "b"},
		},
		{
			name:  "a closed workspace does not count as last",
			given: []landing{{name: "a"}, {name: "closed", closed: true}, {name: "c"}, {name: "new"}},
			want:  []string{"a", "new", "c", "closed"},
		},
		{
			name: "a prioritized workspace lands second-to-last within its priority",
			given: []landing{
				{name: "p1-a", priority: priority(PriorityP1)}, {name: "none"},
				{name: "p1-b", priority: priority(PriorityP1)}, {name: "p1-new", priority: priority(PriorityP1)},
			},
			want: []string{"p1-a", "p1-new", "p1-b", "none"},
		},
		{
			name:  "a priority nobody open shares lands second-to-last overall",
			given: []landing{{name: "a"}, {name: "b"}, {name: "p2", priority: priority(PriorityP2)}},
			want:  []string{"a", "p2", "b"},
		},
		{
			name: "a priority one open workspace shares lands after it",
			given: []landing{
				{name: "p3", priority: priority(PriorityP3)}, {name: "none"},
				{name: "p3-new", priority: priority(PriorityP3)},
			},
			want: []string{"p3", "p3-new", "none"},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			s, _ := testStore(t)
			repoDir := placeRepo(t)

			// Act.
			for _, ws := range tc.given {
				registered := registerIn(t, s, repoDir, ws.name, ws.priority)
				if ws.closed {
					if err := s.MarkClosed(context.Background(), registered.ID, Closure{At: time.Now(), By: CloseByUser}); err != nil {
						t.Fatalf("MarkClosed: %v", err)
					}
				}
			}

			// Assert.
			if got := orderedNames(t, s); !slices.Equal(got, tc.want) {
				t.Fatalf("place order = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestLandingCountsOpenWorkspacesOfEveryRepository(t *testing.T) {
	// Arrange: the tab bar interleaves repositories, so landing does too.
	s, _ := testStore(t)
	registerIn(t, s, placeRepo(t), "a", nil)
	registerIn(t, s, placeRepo(t), "b", nil)

	// Act.
	registerIn(t, s, placeRepo(t), "new", nil)

	// Assert.
	if got := orderedNames(t, s); !slices.Equal(got, []string{"a", "new", "b"}) {
		t.Fatalf("place order = %v, want [a new b]", got)
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

func TestSettingAPriorityMovesTheWorkspaceSecondToLastWithinIt(t *testing.T) {
	// Arrange: two open P1 workspaces and two plain ones, the last plain one
	// about to take P1.
	s, _ := testStore(t)
	repoDir := placeRepo(t)
	registerIn(t, s, repoDir, "p1-a", priority(PriorityP1))
	registerIn(t, s, repoDir, "p1-b", priority(PriorityP1))
	registerIn(t, s, repoDir, "plain", nil)
	mover := registerIn(t, s, repoDir, "mover", nil)
	before := orderedNames(t, s)

	// Act.
	if err := s.SetPriority(context.Background(), mover.ID, priority(PriorityP1)); err != nil {
		t.Fatalf("SetPriority: %v", err)
	}

	// Assert: the mover sits just before the last P1 workspace.
	got := orderedNames(t, s)
	if i, j := slices.Index(got, "mover"), slices.Index(got, before[slices.Index(before, "p1-b")]); i != j-1 {
		t.Fatalf("place order = %v, want mover right before p1-b", got)
	}
}

func TestClearingAPriorityMovesNothing(t *testing.T) {
	// Arrange.
	s, _ := testStore(t)
	repoDir := placeRepo(t)
	p1 := registerIn(t, s, repoDir, "p1", priority(PriorityP1))
	registerIn(t, s, repoDir, "a", nil)
	registerIn(t, s, repoDir, "b", nil)
	before := orderedNames(t, s)

	// Act.
	if err := s.SetPriority(context.Background(), p1.ID, nil); err != nil {
		t.Fatalf("SetPriority(nil): %v", err)
	}

	// Assert.
	if got := orderedNames(t, s); !slices.Equal(got, before) {
		t.Fatalf("place order = %v, want %v unchanged", got, before)
	}
}

func TestSettingAPriorityOnAnUnknownWorkspaceIsNotFound(t *testing.T) {
	// Arrange.
	s, _ := testStore(t)
	registerIn(t, s, placeRepo(t), "a", nil)

	// Act.
	err := s.SetPriority(context.Background(), "ws-missing", priority(PriorityP1))

	// Assert.
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("SetPriority error = %v, want ErrNotFound", err)
	}
}

func TestSwapPlaces(t *testing.T) {
	cases := []struct {
		name       string
		ws, target string
		want       []string
	}{
		{name: "swapping rightward exchanges the two and leaves the ones between in place",
			ws: "a", target: "c", want: []string{"c", "b", "a", "d"}},
		{name: "swapping leftward exchanges the two and leaves the ones between in place",
			ws: "d", target: "b", want: []string{"a", "d", "c", "b"}},
		{name: "swapping neighbours exchanges the two",
			ws: "b", target: "c", want: []string{"a", "c", "b", "d"}},
		{name: "swapping the ends leaves the middle alone",
			ws: "a", target: "d", want: []string{"d", "b", "c", "a"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: places a, b, c, d in order, sorted into it by swaps.
			s, _ := testStore(t)
			repoDir := placeRepo(t)
			ids := map[string]WorkspaceID{}
			for _, name := range []string{"a", "b", "c", "d"} {
				ids[name] = registerIn(t, s, repoDir, name, nil).ID
			}
			for i, name := range []string{"a", "b", "c", "d"} {
				if at := orderedNames(t, s)[i]; at != name {
					if err := s.SwapPlaces(context.Background(), ids[name], ids[at]); err != nil {
						t.Fatalf("arrange SwapPlaces: %v", err)
					}
				}
			}
			if got := orderedNames(t, s); !slices.Equal(got, []string{"a", "b", "c", "d"}) {
				t.Fatalf("arranged order = %v, want [a b c d]", got)
			}

			// Act.
			if err := s.SwapPlaces(context.Background(), ids[tc.ws], ids[tc.target]); err != nil {
				t.Fatalf("SwapPlaces: %v", err)
			}

			// Assert.
			if got := orderedNames(t, s); !slices.Equal(got, tc.want) {
				t.Fatalf("place order = %v, want %v", got, tc.want)
			}
		})
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
	if after := placesByName(t, s); !maps.Equal(after, before) {
		t.Fatalf("places moved from %v to %v on a refused swap", before, after)
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

func TestReopeningAClosedWorkspaceRestoresItsPlace(t *testing.T) {
	// Arrange: three workspaces, the middle one closed while others land.
	s, log := testStore(t)
	repoDir := placeRepo(t)
	registerIn(t, s, repoDir, "a", nil)
	b := registerIn(t, s, repoDir, "b", nil)
	registerIn(t, s, repoDir, "c", nil)
	if err := s.MarkClosed(context.Background(), b.ID, Closure{At: time.Now(), By: CloseByUser}); err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}
	registerIn(t, s, repoDir, "d", nil)
	before := orderedNames(t, s)

	// Act.
	if err := s.Reopen(context.Background(), b.ID); err != nil {
		t.Fatalf("Reopen: %v", err)
	}

	// Assert: nothing moved; b is back where it was.
	if got := orderedNames(t, s); !slices.Equal(got, before) {
		t.Fatalf("place order = %v, want %v", got, before)
	}
	if loggedOperation(log, "daemon.wsm.set_closed", "error") {
		t.Fatalf("the re-open logged an error: %v", log.Records())
	}
}

func TestReopeningAnUnknownWorkspaceIsNotFound(t *testing.T) {
	// Arrange.
	s, _ := testStore(t)

	// Act.
	err := s.Reopen(context.Background(), "ws-missing")

	// Assert.
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("Reopen error = %v, want ErrNotFound", err)
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

func TestTheMigrationSeedsPlacesFromTheTabOrderBeforePlaces(t *testing.T) {
	// Arrange: a layout-3 file with workspaces of mixed priority, a child,
	// and a merged one. The fixture's own workspace is unprioritized too.
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

	// Assert: priority strongest first, then name, as the bar drew them.
	want := []string{"omega", "zeta", "aaa", fixtureWorkspaceName}
	if fixtureWorkspaceName < "aaa" {
		want = []string{"omega", "zeta", fixtureWorkspaceName, "aaa"}
	}
	if got := orderedNames(t, handle.(*store)); !slices.Equal(got, want) {
		t.Fatalf("seeded place order = %v, want %v", got, want)
	}
}

func TestTheMigrationSeedsAChildRightAfterItsParent(t *testing.T) {
	// Arrange: parent "b" with child "z", and a sibling "c" of the parent.
	// The fixture's own workspace shares the repository.
	// The file is a layout-34 one, the last before places, built forward.
	path := fixtureAt(t, 34)
	execRaw(t, path, `INSERT INTO workspaces (id, repo_id, dir, name, branch, parent_branch, closed, attention, is_current, created_at)
		VALUES ('ws-b', 'repo-1', '/tmp/ws-b', 'b', 'b', 'master', 0, 0, 0, 1)`)
	execRaw(t, path, `INSERT INTO workspaces (id, repo_id, dir, name, branch, parent_branch, closed, attention, is_current, created_at)
		VALUES ('ws-c', 'repo-1', '/tmp/ws-c', 'c', 'c', 'master', 0, 0, 0, 1)`)
	execRaw(t, path, `INSERT INTO workspaces (id, repo_id, dir, name, branch, parent_branch, parent_id, closed, attention, is_current, created_at)
		VALUES ('ws-z', 'repo-1', '/tmp/ws-z', 'z', 'z', 'b', 'ws-b', 0, 0, 0, 1)`)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	defer handle.Close()

	// Assert.
	got := orderedNames(t, handle.(*store))
	if i := slices.Index(got, "z"); i != slices.Index(got, "b")+1 {
		t.Fatalf("seeded place order = %v, want z right after its parent b", got)
	}
}
