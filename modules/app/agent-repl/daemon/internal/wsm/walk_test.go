package wsm

import (
	"context"
	"errors"
	"slices"
	"strings"
	"testing"
)

// slotted is a subsection named NAME at PLACE standing at SLOT (nil: after
// every root).
func slotted(name string, place int64, slot *int64) Subsection {
	return Subsection{ID: SubsectionID(name), Name: name, Place: place, Slot: slot}
}

// slotAt is a slot value.
func slotAt(v int64) *int64 { return &v }

// placed is one root of a Walk test: a name at a registry place.
type placed struct {
	name  string
	place int64
}

// walkEntryNames spells a Walk's entries: a root by its name, a subsection
// as [name].
func walkEntryNames(entries []WalkEntry[placed]) []string {
	var out []string
	for _, e := range entries {
		if e.Subsection != nil {
			out = append(out, "["+e.Subsection.Name+"]")
			continue
		}
		out = append(out, e.Root.name)
	}
	return out
}

func TestWalk(t *testing.T) {
	roots := []placed{{"b", 20}, {"a", 10}, {"c", 30}}
	tests := []struct {
		name  string
		roots []placed
		subs  []Subsection
		want  []string
	}{
		{name: "roots alone go in place order", roots: roots, want: []string{"a", "b", "c"}},
		{name: "a subsection with no slot follows every root", roots: roots, subs: []Subsection{slotted("s", 1, nil)},
			want: []string{"a", "b", "c", "[s]"}},
		{name: "a subsection stands before the first root at or past its slot", roots: roots, subs: []Subsection{slotted("s", 1, slotAt(15))},
			want: []string{"a", "[s]", "b", "c"}},
		{name: "a slot equal to a root's place stands before that root", roots: roots, subs: []Subsection{slotted("s", 1, slotAt(20))},
			want: []string{"a", "[s]", "b", "c"}},
		{name: "subsections at one point keep their place order", roots: roots,
			subs: []Subsection{slotted("y", 2, slotAt(20)), slotted("x", 1, slotAt(20))}, want: []string{"a", "[x]", "[y]", "b", "c"}},
		{name: "a slot before every root stands first", roots: roots, subs: []Subsection{slotted("s", 1, slotAt(1))},
			want: []string{"[s]", "a", "b", "c"}},
		{name: "a slot past every root stands last", roots: roots, subs: []Subsection{slotted("s", 1, slotAt(99))},
			want: []string{"a", "b", "c", "[s]"}},
		{name: "subsections alone go in place order", subs: []Subsection{slotted("y", 2, nil), slotted("x", 1, slotAt(5))},
			want: []string{"[x]", "[y]"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := Walk(tt.roots, func(r placed) int64 { return r.place }, tt.subs)

			// Assert
			if names := walkEntryNames(got); !slices.Equal(names, tt.want) {
				t.Fatalf("Walk = %v, want %v", names, tt.want)
			}
		})
	}
}

// walkShape spells REPO's stored walk: an unsectioned root by its name, a
// subsection as [name: member member].
func walkShape(t *testing.T, s *store, repo RepoID) []string {
	t.Helper()
	tx, err := s.db().BeginTx(context.Background(), nil)
	if err != nil {
		t.Fatalf("begin: %v", err)
	}
	walk, err := readWalk(context.Background(), tx, repo)
	if err != nil {
		t.Fatalf("readWalk: %v", err)
	}
	// The read ends before any name is looked up: the store holds one
	// connection.
	if err := tx.Rollback(); err != nil {
		t.Fatalf("rollback: %v", err)
	}
	name := func(id WorkspaceID) string {
		return scalar[string](t, s, `SELECT name FROM workspaces WHERE id = ?`, id)
	}
	var out []string
	for _, step := range walk {
		if step.sub == nil {
			out = append(out, name(step.root))
			continue
		}
		members := []string{step.sub.Name + ":"}
		for _, id := range step.members {
			members = append(members, name(id))
		}
		out = append(out, "["+strings.Join(members, " ")+"]")
	}
	return out
}

// walkNames answers REPO's roots in walk order, a subsection's where it stands.
func walkNames(t *testing.T, s *store, repo RepoID) []string {
	t.Helper()
	var out []string
	for _, entry := range walkShape(t, s, repo) {
		if !strings.HasPrefix(entry, "[") {
			out = append(out, entry)
			continue
		}
		out = append(out, strings.Fields(strings.Trim(entry, "[]"))[1:]...)
	}
	return out
}

// walkWorld is a repository a, b, c in that order, with subsection s holding
// x, standing at the bottom.
type walkWorld struct {
	a, b, c, x Workspace
	s          Subsection
}

func newWalkWorld(t *testing.T, s *store) walkWorld {
	t.Helper()
	repoDir := arrangeRepo(t, "repo")
	var w walkWorld
	w.a = registerIn(t, s, repoDir, "a", nil)
	w.b = registerIn(t, s, repoDir, "b", nil)
	w.c = registerIn(t, s, repoDir, "c", nil)
	w.x = registerIn(t, s, repoDir, "x", nil)
	w.s = mustSubsection(t, s, w.a.Repo, "s")
	mustPlace(t, s, w.x.ID, &w.s.ID, AtEnd{})
	for _, ws := range []Workspace{w.a, w.b, w.c} {
		mustPlace(t, s, ws.ID, nil, BeforeSubsection{ID: w.s.ID})
	}
	if got, want := walkShape(t, s, w.a.Repo), []string{"a", "b", "c", "[s: x]"}; !slices.Equal(got, want) {
		t.Fatalf("the walk world = %v, want %v", got, want)
	}
	return w
}

func TestMoveSubsectionBeforeAWorkspaceStandsItBetweenTwoRows(t *testing.T) {
	// Arrange -- the owner's example: drag the subsection onto b's line.
	s, _ := testStore(t)
	w := newWalkWorld(t, s)

	// Act
	err := s.MoveSubsection(context.Background(), w.s.ID, BeforeWorkspace{ID: w.b.ID})

	// Assert
	if err != nil {
		t.Fatalf("MoveSubsection: %v", err)
	}
	if got, want := walkShape(t, s, w.a.Repo), []string{"a", "[s: x]", "b", "c"}; !slices.Equal(got, want) {
		t.Fatalf("walk = %v, want %v", got, want)
	}
}

func TestMoveSubsectionToTheEndFollowsEveryRow(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	w := newWalkWorld(t, s)
	if err := s.MoveSubsection(context.Background(), w.s.ID, BeforeWorkspace{ID: w.a.ID}); err != nil {
		t.Fatalf("MoveSubsection: %v", err)
	}

	// Act
	err := s.MoveSubsection(context.Background(), w.s.ID, AtEnd{})

	// Assert
	if err != nil {
		t.Fatalf("MoveSubsection: %v", err)
	}
	if got, want := walkShape(t, s, w.a.Repo), []string{"a", "b", "c", "[s: x]"}; !slices.Equal(got, want) {
		t.Fatalf("walk = %v, want %v", got, want)
	}
}

func TestMoveSubsectionBeforeAWorkspaceRefusals(t *testing.T) {
	tests := []struct {
		name   string
		anchor func(t *testing.T, s *store, w walkWorld) WorkspaceID
		want   error
	}{
		{name: "a workspace in a subsection", want: ErrAnchorElsewhere,
			anchor: func(_ *testing.T, _ *store, w walkWorld) WorkspaceID { return w.x.ID }},
		{name: "a child", want: ErrAnchorElsewhere,
			anchor: func(t *testing.T, s *store, w walkWorld) WorkspaceID {
				return registerChild(t, s, arrangeRepoOf(t, s, w.a), "kid", w.a.ID).ID
			}},
		{name: "another repository's workspace", want: ErrOtherRepository,
			anchor: func(t *testing.T, s *store, _ walkWorld) WorkspaceID {
				return registerIn(t, s, arrangeRepo(t, "other"), "o", nil).ID
			}},
		{name: "an unknown workspace", want: ErrNotFound,
			anchor: func(*testing.T, *store, walkWorld) WorkspaceID { return "absent" }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			w := newWalkWorld(t, s)
			anchor := tt.anchor(t, s, w)
			before := walkShape(t, s, w.a.Repo)

			// Act
			err := s.MoveSubsection(context.Background(), w.s.ID, BeforeWorkspace{ID: anchor})

			// Assert
			if !errors.Is(err, tt.want) {
				t.Fatalf("MoveSubsection = %v, want %v", err, tt.want)
			}
			if got := walkShape(t, s, w.a.Repo); !slices.Equal(got, before) {
				t.Fatalf("walk after a refusal = %v, want %v", got, before)
			}
		})
	}
}

// arrangeRepoOf answers the directory of WS's repository.
func arrangeRepoOf(t *testing.T, s *store, ws Workspace) string {
	t.Helper()
	return scalar[string](t, s, `SELECT dir FROM repositories WHERE id = ?`, ws.Repo)
}

func TestPlaceWorkspaceBeforeASubsection(t *testing.T) {
	// Arrange -- a [s: x] b c.
	s, _ := testStore(t)
	w := newWalkWorld(t, s)
	if err := s.MoveSubsection(context.Background(), w.s.ID, BeforeWorkspace{ID: w.b.ID}); err != nil {
		t.Fatalf("MoveSubsection: %v", err)
	}

	// Act
	err := s.PlaceWorkspaces(context.Background(), []WorkspaceID{w.c.ID}, nil, BeforeSubsection{ID: w.s.ID})

	// Assert
	if err != nil {
		t.Fatalf("PlaceWorkspaces: %v", err)
	}
	if got, want := walkShape(t, s, w.a.Repo), []string{"a", "c", "[s: x]", "b"}; !slices.Equal(got, want) {
		t.Fatalf("walk = %v, want %v", got, want)
	}
}

func TestPlaceWorkspaceAtTheEndOfNoSubsectionFollowsEverySubsection(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	w := newWalkWorld(t, s)

	// Act
	err := s.PlaceWorkspaces(context.Background(), []WorkspaceID{w.a.ID}, nil, AtEnd{})

	// Assert
	if err != nil {
		t.Fatalf("PlaceWorkspaces: %v", err)
	}
	if got, want := walkShape(t, s, w.a.Repo), []string{"b", "c", "[s: x]", "a"}; !slices.Equal(got, want) {
		t.Fatalf("walk = %v, want %v", got, want)
	}
}

func TestPlaceWorkspaceOutOfASubsectionBeforeAnother(t *testing.T) {
	// Arrange -- a b c [s: x] [t].
	s, _ := testStore(t)
	w := newWalkWorld(t, s)
	other := mustSubsection(t, s, w.a.Repo, "t")

	// Act
	err := s.PlaceWorkspaces(context.Background(), []WorkspaceID{w.x.ID}, nil, BeforeSubsection{ID: other.ID})

	// Assert
	if err != nil {
		t.Fatalf("PlaceWorkspaces: %v", err)
	}
	if got, want := walkShape(t, s, w.a.Repo), []string{"a", "b", "c", "[s:]", "x", "[t:]"}; !slices.Equal(got, want) {
		t.Fatalf("walk = %v, want %v", got, want)
	}
}

func TestPlaceWorkspaceBeforeASubsectionRefusals(t *testing.T) {
	tests := []struct {
		name   string
		into   func(w walkWorld) *SubsectionID
		anchor func(t *testing.T, s *store, w walkWorld) SubsectionID
		want   error
	}{
		{name: "another repository's subsection", want: ErrOtherRepository,
			into: func(walkWorld) *SubsectionID { return nil },
			anchor: func(t *testing.T, s *store, _ walkWorld) SubsectionID {
				other := registerIn(t, s, arrangeRepo(t, "other"), "o", nil)
				return mustSubsection(t, s, other.Repo, "theirs").ID
			}},
		{name: "an unknown subsection", want: ErrUnknownSubsection,
			into:   func(walkWorld) *SubsectionID { return nil },
			anchor: func(*testing.T, *store, walkWorld) SubsectionID { return "absent" }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			w := newWalkWorld(t, s)
			anchor := tt.anchor(t, s, w)
			before := walkShape(t, s, w.a.Repo)

			// Act
			err := s.PlaceWorkspaces(context.Background(), []WorkspaceID{w.a.ID}, tt.into(w), BeforeSubsection{ID: anchor})

			// Assert
			if !errors.Is(err, tt.want) {
				t.Fatalf("PlaceWorkspaces = %v, want %v", err, tt.want)
			}
			if got := walkShape(t, s, w.a.Repo); !slices.Equal(got, before) {
				t.Fatalf("walk after a refusal = %v, want %v", got, before)
			}
		})
	}
}

func TestPlaceWorkspaceIntoASubsectionBeforeASubsectionIsMalformed(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	w := newWalkWorld(t, s)
	before := walkShape(t, s, w.a.Repo)

	// Act
	err := s.PlaceWorkspaces(context.Background(), []WorkspaceID{w.a.ID}, &w.s.ID, BeforeSubsection{ID: w.s.ID})

	// Assert
	if err == nil {
		t.Fatalf("PlaceWorkspaces into a subsection before a subsection succeeded")
	}
	if got := walkShape(t, s, w.a.Repo); !slices.Equal(got, before) {
		t.Fatalf("walk after a refusal = %v, want %v", got, before)
	}
	if !hasOperation(log, "daemon.wsm.place_workspaces") {
		t.Fatalf("no daemon.wsm.place_workspaces record: %v", log.Records())
	}
}

func TestARowLeavingItsPointLeavesTheSubsectionStanding(t *testing.T) {
	// Arrange -- a [s: x] b c.
	s, _ := testStore(t)
	w := newWalkWorld(t, s)
	if err := s.MoveSubsection(context.Background(), w.s.ID, BeforeWorkspace{ID: w.b.ID}); err != nil {
		t.Fatalf("MoveSubsection: %v", err)
	}

	// Act -- b, the row just after the subsection, goes to the bottom.
	err := s.PlaceWorkspaces(context.Background(), []WorkspaceID{w.b.ID}, nil, AtEnd{})

	// Assert
	if err != nil {
		t.Fatalf("PlaceWorkspaces: %v", err)
	}
	if got, want := walkShape(t, s, w.a.Repo), []string{"a", "[s: x]", "c", "b"}; !slices.Equal(got, want) {
		t.Fatalf("walk = %v, want %v", got, want)
	}
}

func TestShiftFromKeepsEveryRootOnItsSideOfASubsection(t *testing.T) {
	// Arrange -- a [s: x] b c, then every place from a's up shifts by one.
	s, _ := testStore(t)
	w := newWalkWorld(t, s)
	if err := s.MoveSubsection(context.Background(), w.s.ID, BeforeWorkspace{ID: w.b.ID}); err != nil {
		t.Fatalf("MoveSubsection: %v", err)
	}
	from := scalar[int64](t, s, `SELECT place FROM workspaces WHERE id = ?`, w.a.ID)
	tx, err := s.db().BeginTx(context.Background(), nil)
	if err != nil {
		t.Fatalf("begin: %v", err)
	}

	// Act
	if err := shiftFrom(context.Background(), tx, from, ""); err != nil {
		t.Fatalf("shiftFrom: %v", err)
	}
	if err := tx.Commit(); err != nil {
		t.Fatalf("commit: %v", err)
	}

	// Assert
	if got, want := walkShape(t, s, w.a.Repo), []string{"a", "[s: x]", "b", "c"}; !slices.Equal(got, want) {
		t.Fatalf("walk = %v, want %v", got, want)
	}
}

func TestANewWorkspaceLandsWithoutMovingASubsection(t *testing.T) {
	// Arrange -- a b [s: x] c: c is the last open workspace, so a new one
	// lands just before it (landingPlace), after the subsection.
	s, _ := testStore(t)
	w := newWalkWorld(t, s)
	if err := s.MoveSubsection(context.Background(), w.s.ID, BeforeWorkspace{ID: w.c.ID}); err != nil {
		t.Fatalf("MoveSubsection: %v", err)
	}
	if got := orderedNames(t, s); got[len(got)-1] != "c" {
		t.Fatalf("c is not the last workspace: %v", got)
	}

	// Act
	registerIn(t, s, arrangeRepoOf(t, s, w.a), "n", nil)

	// Assert
	if got, want := walkShape(t, s, w.a.Repo), []string{"a", "b", "[s: x]", "n", "c"}; !slices.Equal(got, want) {
		t.Fatalf("walk = %v, want %v", got, want)
	}
}

func TestMoveWorkspaceAcrossASubsectionJoinsItBeforeTheTarget(t *testing.T) {
	// Arrange -- a [s: x] b c.
	s, _ := testStore(t)
	w := newWalkWorld(t, s)
	if err := s.MoveSubsection(context.Background(), w.s.ID, BeforeWorkspace{ID: w.b.ID}); err != nil {
		t.Fatalf("MoveSubsection: %v", err)
	}

	// Act -- c, from a later tab, onto x's.
	err := s.MoveWorkspace(context.Background(), w.c.ID, w.x.ID)

	// Assert
	if err != nil {
		t.Fatalf("MoveWorkspace: %v", err)
	}
	if got, want := walkShape(t, s, w.a.Repo), []string{"a", "[s: c x]", "b"}; !slices.Equal(got, want) {
		t.Fatalf("walk = %v, want %v", got, want)
	}
}

func TestMoveWorkspaceOntoAnUnsectionedRowAfterASubsectionLeavesIt(t *testing.T) {
	// Arrange -- a [s: x] b c.
	s, _ := testStore(t)
	w := newWalkWorld(t, s)
	if err := s.MoveSubsection(context.Background(), w.s.ID, BeforeWorkspace{ID: w.b.ID}); err != nil {
		t.Fatalf("MoveSubsection: %v", err)
	}

	// Act -- x, from an earlier tab, onto b's.
	err := s.MoveWorkspace(context.Background(), w.x.ID, w.b.ID)

	// Assert
	if err != nil {
		t.Fatalf("MoveWorkspace: %v", err)
	}
	if got, want := walkShape(t, s, w.a.Repo), []string{"a", "[s:]", "b", "x", "c"}; !slices.Equal(got, want) {
		t.Fatalf("walk = %v, want %v", got, want)
	}
}

func TestAnArrangementMovesNoOtherRepositorysPlaces(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	w := newWalkWorld(t, s)
	other := registerIn(t, s, arrangeRepo(t, "other"), "o", nil)
	mustSubsection(t, s, other.Repo, "theirs")
	before := placesByName(t, s)["o"]

	// Act
	if err := s.MoveSubsection(context.Background(), w.s.ID, BeforeWorkspace{ID: w.a.ID}); err != nil {
		t.Fatalf("MoveSubsection: %v", err)
	}

	// Assert
	if got := placesByName(t, s)["o"]; got != before {
		t.Fatalf("the other repository's workspace moved from place %d to %d", before, got)
	}
	if got, want := walkShape(t, s, other.Repo), []string{"o", "[theirs:]"}; !slices.Equal(got, want) {
		t.Fatalf("the other repository's walk = %v, want %v", got, want)
	}
}

func TestTheSubsectionSlotsMigrationLeavesEverySubsectionAtTheBottom(t *testing.T) {
	// Arrange
	path := layout3Fixture(t)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-3 database: %v", err)
	}
	defer handle.Close()

	// Assert
	s := handle.(*store)
	if got := scalar[int](t, s, `SELECT count(*) FROM pragma_table_info('subsections') WHERE name = 'slot' AND "notnull" = 0`); got != 1 {
		t.Fatalf("subsections.slot (nullable) exists %d times after the migration, want 1", got)
	}
}
