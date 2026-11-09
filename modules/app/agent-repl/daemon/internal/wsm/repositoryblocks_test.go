package wsm

import (
	"context"
	"errors"
	"slices"
	"testing"

	"agent-repld/internal/dlog"
)

// blockFamily registers a parent in repository "a" with one child in each of
// repositories "b", "c" and "d" (in that order) and a second child in "b",
// answering the parent, its children by name, and the repositories by name.
func blockFamily(t *testing.T, s *store) (Workspace, map[string]Workspace, map[string]RepoID) {
	t.Helper()
	parent := registerIn(t, s, arrangeRepo(t, "a"), "parent", nil)
	repos := map[string]RepoID{"a": parent.Repo}
	children := map[string]Workspace{}
	dirs := map[string]string{}
	for _, name := range []string{"b", "c", "d"} {
		dirs[name] = arrangeRepo(t, name)
	}
	for _, kid := range []struct{ name, repo string }{{"b1", "b"}, {"c1", "c"}, {"d1", "d"}, {"b2", "b"}} {
		ws := registerChild(t, s, dirs[kid.repo], kid.name, parent.ID)
		children[kid.name] = ws
		repos[kid.repo] = ws.Repo
	}
	return parent, children, repos
}

// blockNames reads PARENT's block order from one arrangement read, spelled by
// repository name.
func blockNames(t *testing.T, s *store, parent Workspace) []string {
	t.Helper()
	arrangement, err := s.ReadArrangement(context.Background())
	if err != nil {
		t.Fatalf("ReadArrangement: %v", err)
	}
	var present []RepoID
	for _, ws := range arrangement.Workspaces {
		if ws.Parent != nil && *ws.Parent == parent.ID && ws.Repo != parent.Repo && !slices.Contains(present, ws.Repo) {
			present = append(present, ws.Repo)
		}
	}
	var stored []RepositoryBlock
	for _, b := range arrangement.RepositoryBlocks {
		if b.Parent == parent.ID {
			stored = append(stored, b)
		}
	}
	names := map[RepoID]string{}
	for _, repo := range arrangement.Repositories {
		names[repo.ID] = repo.Name
	}
	var out []string
	for _, repo := range OrderRepositoryBlocks(present, stored, RepositoryPlaces(arrangement.Repositories)) {
		out = append(out, names[repo])
	}
	return out
}

func TestOrderRepositoryBlocks(t *testing.T) {
	places := map[RepoID]int64{"b": 1, "c": 2, "d": 3}
	tests := []struct {
		name    string
		present []RepoID
		stored  []RepositoryBlock
		want    []RepoID
	}{
		{name: "unarranged blocks take the repositories' order", present: []RepoID{"d", "b", "c"}, want: []RepoID{"b", "c", "d"}},
		{name: "arranged blocks take their stored places", present: []RepoID{"b", "c", "d"},
			stored: []RepositoryBlock{{Repo: "d", Place: 1}, {Repo: "b", Place: 2}, {Repo: "c", Place: 3}}, want: []RepoID{"d", "b", "c"}},
		{name: "arranged blocks precede unarranged ones", present: []RepoID{"b", "c", "d"},
			stored: []RepositoryBlock{{Repo: "c", Place: 1}}, want: []RepoID{"c", "b", "d"}},
		{name: "a stored block with no child left is not answered", present: []RepoID{"b"},
			stored: []RepositoryBlock{{Repo: "c", Place: 1}, {Repo: "b", Place: 2}}, want: []RepoID{"b"}},
		{name: "no block answers nothing", present: nil, want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := OrderRepositoryBlocks(tt.present, tt.stored, places)

			// Assert
			if !slices.Equal(got, tt.want) {
				t.Fatalf("OrderRepositoryBlocks = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestOrderRepositoryBlocksPanicsOnAnUnregisteredRepository(t *testing.T) {
	// Assert
	defer func() {
		if recover() == nil {
			t.Fatal("OrderRepositoryBlocks ordered a repository the registry does not carry")
		}
	}()

	// Act
	OrderRepositoryBlocks([]RepoID{"gone"}, nil, map[RepoID]int64{"b": 1})
}

func TestMoveRepositoryBlock(t *testing.T) {
	tests := []struct {
		name   string
		move   string
		before string
		want   []string
	}{
		{name: "before an earlier block", move: "d", before: "b", want: []string{"d", "b", "c"}},
		{name: "before a later block", move: "b", before: "d", want: []string{"c", "b", "d"}},
		{name: "after every other block", move: "b", want: []string{"c", "d", "b"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			parent, _, repos := blockFamily(t, s)
			var before *RepoID
			if tt.before != "" {
				id := repos[tt.before]
				before = &id
			}

			// Act
			err := s.MoveRepositoryBlock(context.Background(), parent.ID, repos[tt.move], before)

			// Assert
			if err != nil {
				t.Fatalf("MoveRepositoryBlock: %v", err)
			}
			if got := blockNames(t, s, parent); !slices.Equal(got, tt.want) {
				t.Fatalf("blocks = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestTheBlockOrderPersistsAcrossMoves(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	parent, _, repos := blockFamily(t, s)
	ctx := context.Background()
	if err := s.MoveRepositoryBlock(ctx, parent.ID, repos["d"], nil); err != nil {
		t.Fatalf("MoveRepositoryBlock(d): %v", err)
	}

	// Act: a second move starts from the order the first one stored.
	b := repos["b"]
	err := s.MoveRepositoryBlock(ctx, parent.ID, repos["c"], &b)

	// Assert
	if err != nil {
		t.Fatalf("MoveRepositoryBlock(c): %v", err)
	}
	if got, want := blockNames(t, s, parent), []string{"c", "b", "d"}; !slices.Equal(got, want) {
		t.Fatalf("blocks = %v, want %v", got, want)
	}
}

func TestMoveRepositoryBlockRefusals(t *testing.T) {
	tests := []struct {
		name   string
		parent func(Workspace, map[string]Workspace) WorkspaceID
		repo   func(map[string]RepoID, RepoID) RepoID
		before func(map[string]RepoID) *RepoID
		want   error
	}{
		{name: "the parent's own repository is no block",
			parent: func(p Workspace, _ map[string]Workspace) WorkspaceID { return p.ID },
			repo:   func(r map[string]RepoID, _ RepoID) RepoID { return r["a"] },
			before: func(map[string]RepoID) *RepoID { return nil }, want: ErrNoRepositoryBlock},
		{name: "a repository with no child under the parent is no block",
			parent: func(p Workspace, _ map[string]Workspace) WorkspaceID { return p.ID },
			repo:   func(_ map[string]RepoID, lone RepoID) RepoID { return lone },
			before: func(map[string]RepoID) *RepoID { return nil }, want: ErrNoRepositoryBlock},
		{name: "an anchor that is no block of the parent",
			parent: func(p Workspace, _ map[string]Workspace) WorkspaceID { return p.ID },
			repo:   func(r map[string]RepoID, _ RepoID) RepoID { return r["b"] },
			before: func(r map[string]RepoID) *RepoID { own := r["a"]; return &own }, want: ErrNoRepositoryBlock},
		{name: "a child's own children are another parent's blocks",
			parent: func(_ Workspace, kids map[string]Workspace) WorkspaceID { return kids["b1"].ID },
			repo:   func(r map[string]RepoID, _ RepoID) RepoID { return r["c"] },
			before: func(map[string]RepoID) *RepoID { return nil }, want: ErrNoRepositoryBlock},
		{name: "an unknown parent",
			parent: func(Workspace, map[string]Workspace) WorkspaceID { return "w-absent" },
			repo:   func(r map[string]RepoID, _ RepoID) RepoID { return r["b"] },
			before: func(map[string]RepoID) *RepoID { return nil }, want: ErrNotFound},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			parent, kids, repos := blockFamily(t, s)
			lone := registerIn(t, s, arrangeRepo(t, "e"), "lone", nil).Repo

			// Act
			err := s.MoveRepositoryBlock(context.Background(), tt.parent(parent, kids), tt.repo(repos, lone), tt.before(repos))

			// Assert
			if !errors.Is(err, tt.want) {
				t.Fatalf("MoveRepositoryBlock = %v, want %v", err, tt.want)
			}
			arrangement, rerr := s.ReadArrangement(context.Background())
			if rerr != nil {
				t.Fatalf("ReadArrangement: %v", rerr)
			}
			if len(arrangement.RepositoryBlocks) != 0 {
				t.Fatalf("a refused move stored %v", arrangement.RepositoryBlocks)
			}
			if !hasOperation(log, "daemon.wsm.move_repository_block") {
				t.Fatalf("no daemon.wsm.move_repository_block record: %v", log.Records())
			}
		})
	}
}

func TestMoveRepositoryBlockRecordsARefusalWithItsContext(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	parent, _, repos := blockFamily(t, s)

	// Act
	err := s.MoveRepositoryBlock(context.Background(), parent.ID, repos["a"], nil)

	// Assert
	if !errors.Is(err, ErrNoRepositoryBlock) {
		t.Fatalf("MoveRepositoryBlock = %v, want ErrNoRepositoryBlock", err)
	}
	rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Operation: "daemon.wsm.move_repository_block", Message: "the arrangement was refused; nothing was written"})
	if !ok {
		t.Fatalf("no refusal record: %v", log.Records())
	}
	if rec.Context["parent"] != string(parent.ID) || rec.Context["repo_id"] != string(repos["a"]) {
		t.Fatalf("the refusal's record context = %v", rec.Context)
	}
	if blocks, _ := rec.Context["blocks"].([]RepoID); !slices.Equal(blocks, []RepoID{repos["b"], repos["c"], repos["d"]}) {
		t.Fatalf("the refusal's record names blocks %v, want the parent's three", rec.Context["blocks"])
	}
}

func TestMoveRepositoryBlockRefusesMovingBeforeItself(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	parent, _, repos := blockFamily(t, s)
	b := repos["b"]

	// Act
	err := s.MoveRepositoryBlock(context.Background(), parent.ID, b, &b)

	// Assert
	if err == nil {
		t.Fatal("MoveRepositoryBlock before itself succeeded")
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Operation: "daemon.wsm.move_repository_block", Level: dlog.LevelError}); !ok {
		t.Fatalf("no error record of the malformed move: %v", log.Records())
	}
}

func TestTheStoreRefusesABlockOfTheParentsOwnRepository(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	parent, _, _ := blockFamily(t, s)

	// Act
	_, err := s.db().ExecContext(context.Background(),
		`INSERT INTO repository_blocks (parent_id, repo_id, place) VALUES (?, ?, 1)`, parent.ID, parent.Repo)

	// Assert
	if err == nil {
		t.Fatal("the store stored a block of the parent's own repository")
	}
}

// blockRows reads repository REPO's children of PARENT in drawn order (by
// place), by name.
func blockRows(t *testing.T, s *store, parent Workspace, repo RepoID) []string {
	t.Helper()
	arrangement, err := s.ReadArrangement(context.Background())
	if err != nil {
		t.Fatalf("ReadArrangement: %v", err)
	}
	var in []Workspace
	for _, ws := range arrangement.Workspaces {
		if ws.Repo == repo && ws.Parent != nil && *ws.Parent == parent.ID {
			in = append(in, ws)
		}
	}
	slices.SortFunc(in, func(x, y Workspace) int { return int(x.Place - y.Place) })
	var out []string
	for _, ws := range in {
		out = append(out, ws.Name)
	}
	return out
}

func TestAMoveWithinABlockPersistsAndKeepsTheBlockOrder(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	parent, kids, repos := blockFamily(t, s)
	ctx := context.Background()
	if err := s.MoveRepositoryBlock(ctx, parent.ID, repos["b"], nil); err != nil {
		t.Fatalf("MoveRepositoryBlock: %v", err)
	}
	start := blockRows(t, s, parent, repos["b"])
	if len(start) != 2 {
		t.Fatalf("b's block = %v, want two rows", start)
	}

	// Act: the block's later row takes its earlier row's tab.
	err := s.MoveWorkspace(ctx, kids[start[1]].ID, kids[start[0]].ID)

	// Assert
	if err != nil {
		t.Fatalf("MoveWorkspace: %v", err)
	}
	if got, want := blockRows(t, s, parent, repos["b"]), []string{start[1], start[0]}; !slices.Equal(got, want) {
		t.Fatalf("b's block = %v, want %v", got, want)
	}
	if got, want := blockNames(t, s, parent), []string{"c", "d", "b"}; !slices.Equal(got, want) {
		t.Fatalf("blocks = %v, want %v", got, want)
	}
}

func TestADragOutOfABlockIsRefusedAndRecorded(t *testing.T) {
	tests := []struct {
		name string
		move func(*store, map[string]Workspace) error
	}{
		{name: "a move onto another block's row", move: func(s *store, kids map[string]Workspace) error {
			return s.MoveWorkspace(context.Background(), kids["b1"].ID, kids["c1"].ID)
		}},
		{name: "a swap with another block's row", move: func(s *store, kids map[string]Workspace) error {
			return s.SwapPlaces(context.Background(), kids["b1"].ID, kids["c1"].ID)
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			_, kids, _ := blockFamily(t, s)
			before, err := s.ReadArrangement(context.Background())
			if err != nil {
				t.Fatalf("ReadArrangement: %v", err)
			}

			// Act
			err = tt.move(s, kids)

			// Assert
			if !errors.Is(err, ErrOtherRepository) {
				t.Fatalf("move = %v, want ErrOtherRepository", err)
			}
			if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Message: "the arrangement was refused; nothing was written"}); !ok {
				t.Fatalf("no refusal record: %v", log.Records())
			}
			after, err := s.ReadArrangement(context.Background())
			if err != nil {
				t.Fatalf("ReadArrangement: %v", err)
			}
			for i := range before.Workspaces {
				if before.Workspaces[i].Place != after.Workspaces[i].Place {
					t.Fatalf("a refused move changed %s's place", before.Workspaces[i].Name)
				}
			}
		})
	}
}
