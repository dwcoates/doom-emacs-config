package workspacefind

import (
	"context"
	"errors"
	"reflect"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/wsm"
)

// locateRegistry is a registry of an open workspace, a hidden merge
// workspace, an open child nested inside the open one's worktree, a closed
// workspace the closed stream draws, a closed one it no longer draws, a
// merged workspace (in no stream), and a thread sharing the open workspace's
// directory.
func locateRegistry() []wsm.WorkspaceHead {
	landed := epoch()
	return heads(
		wsm.Workspace{ID: "ws-open", Dir: "/wt/open", Name: "open-one"},
		wsm.Workspace{ID: "ws-backlogged", Dir: "/wt/backlogged", Name: "backlogged-one", Backlogged: true},
		wsm.Workspace{ID: "ws-nested", Dir: "/wt/open/nested", Name: "nested-one"},
		wsm.Workspace{ID: "ws-drawn", Dir: "/wt/drawn", Name: "drawn-one", Closed: true},
		wsm.Workspace{ID: "ws-gone", Dir: "/wt/gone", Name: "gone-one", Closed: true},
		wsm.Workspace{ID: "ws-merged", Dir: "/wt/merged", Name: "merged-one", Closed: true, MergedAt: &landed},
		wsm.Workspace{ID: "ws-thread", Dir: "/wt/open", Name: "open-one", Kind: wsm.WorkspaceKindThread},
	)
}

// heads answers the heads of WORKSPACES, in order.
func heads(workspaces ...wsm.Workspace) []wsm.WorkspaceHead {
	out := make([]wsm.WorkspaceHead, 0, len(workspaces))
	for _, ws := range workspaces {
		out = append(out, ws.Head())
	}
	return out
}

// epoch is a fixed merge instant.
func epoch() time.Time { return time.UnixMilli(1_700_000_000_000) }

// locatePush draws the closed ws-drawn on the closed stream.
func locatePush() *frontendv1.RosterPush {
	row := &frontendv1.RosterRow{Workspace: &frontendv1.RosterRowWorkspace{Workspace: &workspacev1.WorkspaceRef{Id: "ws-drawn", Dir: "/wt/drawn"}}}
	return &frontendv1.RosterPush{Closed: &frontendv1.RosterClosedRows{Rows: []*frontendv1.RosterClosedRow{{Row: row}}}}
}

func TestLocate(t *testing.T) {
	tests := []struct {
		name string
		by   LocateBy
		want []wsm.WorkspaceID
	}{
		{name: "an open workspace by name, its thread left out", by: LocateBy{Name: "open-one"}, want: []wsm.WorkspaceID{"ws-open"}},
		{name: "a backlogged workspace by name", by: LocateBy{Name: "backlogged-one"}, want: []wsm.WorkspaceID{"ws-backlogged"}},
		{name: "a closed workspace the closed stream draws by name", by: LocateBy{Name: "drawn-one"}, want: []wsm.WorkspaceID{"ws-drawn"}},
		{name: "a merged workspace, in no stream, by name", by: LocateBy{Name: "merged-one"}, want: []wsm.WorkspaceID{"ws-merged"}},
		{name: "a merged workspace contains its worktree's subdirectory", by: LocateBy{Containing: "/wt/merged/src"}, want: []wsm.WorkspaceID{"ws-merged"}},
		{name: "a closed workspace the roster no longer draws is not located", by: LocateBy{Name: "gone-one"}},
		{name: "an unknown name", by: LocateBy{Name: "nobody"}},
		{name: "a backlogged workspace at its directory", by: LocateBy{At: "/wt/backlogged"}, want: []wsm.WorkspaceID{"ws-backlogged"}},
		{name: "an exact directory reads a trailing slash as the same", by: LocateBy{At: "/wt/open/"}, want: []wsm.WorkspaceID{"ws-open"}},
		{name: "a directory inside a worktree is not at it", by: LocateBy{At: "/wt/open/src"}},
		{name: "a backlogged workspace contains its worktree's subdirectory", by: LocateBy{Containing: "/wt/backlogged/modules/app"}, want: []wsm.WorkspaceID{"ws-backlogged"}},
		{name: "the deepest of nested worktrees contains", by: LocateBy{Containing: "/wt/open/nested/src"}, want: []wsm.WorkspaceID{"ws-nested"}},
		{name: "the outer worktree contains outside the nested one", by: LocateBy{Containing: "/wt/open/src"}, want: []wsm.WorkspaceID{"ws-open"}},
		{name: "a sibling sharing a prefix is not contained", by: LocateBy{Containing: "/wt/openly"}},
		{name: "a directory in no worktree", by: LocateBy{Containing: "/elsewhere"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			located := Locate(locateRegistry(), locatePush(), tt.by)

			// Assert
			var got []wsm.WorkspaceID
			for _, ws := range located {
				got = append(got, ws.ID)
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("Locate(%+v) = %v, want %v", tt.by, got, tt.want)
			}
		})
	}
}

func TestLocateAnswersEveryWorkspaceOfAName(t *testing.T) {
	// Arrange
	registry := heads(wsm.Workspace{ID: "ws-1", Dir: "/wt/one", Name: "twin"}, wsm.Workspace{ID: "ws-2", Dir: "/wt/two", Name: "twin"})

	// Act
	located := Locate(registry, &frontendv1.RosterPush{}, LocateBy{Name: "twin"})

	// Assert
	if len(located) != 2 || located[0].ID != "ws-1" || located[1].ID != "ws-2" {
		t.Fatalf("Locate = %v, want both twins in registry order", located)
	}
}

func TestLocateContainingPrefersTheOpenWorkspaceAtAReusedDirectory(t *testing.T) {
	// Arrange: a drawn closed workspace and an open one at the same worktree,
	// the closed one first in registry order.
	registry := heads(wsm.Workspace{ID: "ws-old", Dir: "/wt/reused", Closed: true}, wsm.Workspace{ID: "ws-new", Dir: "/wt/reused"})
	roster := &frontendv1.RosterPush{Closed: &frontendv1.RosterClosedRows{Rows: []*frontendv1.RosterClosedRow{{
		Row: &frontendv1.RosterRow{Workspace: &frontendv1.RosterRowWorkspace{Workspace: &workspacev1.WorkspaceRef{Id: "ws-old"}}}}}}}

	// Act
	located := Locate(registry, roster, LocateBy{Containing: "/wt/reused/src"})

	// Assert
	if len(located) != 1 || located[0].ID != "ws-new" {
		t.Fatalf("Locate = %v, want the open ws-new", located)
	}
}

func TestLocatePanicsOnALocateThatNamesNothing(t *testing.T) {
	// Assert
	defer func() {
		if recover() == nil {
			t.Fatal("Locate answered a locate naming nothing; want a panic")
		}
	}()

	// Act
	Locate(locateRegistry(), &frontendv1.RosterPush{}, LocateBy{})
}

// recordingReader answers the head reads with ANSWER (or ERR) and records
// what each was asked.
type recordingReader struct {
	answer []wsm.WorkspaceHead
	err    error
	dirs   []string
	name   string
	reads  int
}

func (r *recordingReader) WorkspaceHeadsAt(_ context.Context, dirs []string) ([]wsm.WorkspaceHead, error) {
	r.reads++
	r.dirs = dirs
	return r.answer, r.err
}

func (r *recordingReader) WorkspaceHeadsNamed(_ context.Context, name string) ([]wsm.WorkspaceHead, error) {
	r.reads++
	r.name = name
	return r.answer, r.err
}

func TestCandidatesReadsOnlyTheRowsALocateCouldMatch(t *testing.T) {
	tests := []struct {
		name     string
		by       LocateBy
		wantDirs []string
		wantName string
	}{
		{name: "a name reads the workspaces of that name", by: LocateBy{Name: "open-one"}, wantName: "open-one"},
		{name: "a directory reads the workspace at it, cleaned", by: LocateBy{At: "/wt/open/"}, wantDirs: []string{"/wt/open"}},
		{name: "a containing directory reads it and every ancestor", by: LocateBy{Containing: "/wt/open/src/"},
			wantDirs: []string{"/wt/open/src", "/wt/open", "/wt", "/"}},
		{name: "the root contains only itself", by: LocateBy{Containing: "/"}, wantDirs: []string{"/"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			want := locateRegistry()[:1]
			reader := &recordingReader{answer: want}

			// Act
			got, err := Candidates(context.Background(), reader, tt.by)

			// Assert
			if err != nil {
				t.Fatalf("Candidates = %v", err)
			}
			if reader.reads != 1 || !reflect.DeepEqual(reader.dirs, tt.wantDirs) || reader.name != tt.wantName {
				t.Fatalf("read %d times, dirs %v name %q; want once, dirs %v name %q", reader.reads, reader.dirs, reader.name, tt.wantDirs, tt.wantName)
			}
			if !reflect.DeepEqual(got, want) {
				t.Fatalf("Candidates = %v, want the reader's %v", got, want)
			}
		})
	}
}

func TestCandidatesFailsWithItsRead(t *testing.T) {
	tests := []struct {
		name string
		by   LocateBy
	}{
		{name: "by name", by: LocateBy{Name: "open-one"}},
		{name: "at a directory", by: LocateBy{At: "/wt/open"}},
		{name: "containing a directory", by: LocateBy{Containing: "/wt/open/src"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			cause := errors.New("disk gone")
			reader := &recordingReader{err: cause}

			// Act
			got, err := Candidates(context.Background(), reader, tt.by)

			// Assert
			if !errors.Is(err, cause) || got != nil {
				t.Fatalf("Candidates = %v, %v; want no heads and %v", got, err, cause)
			}
		})
	}
}

func TestCandidatesPanicsOnALocateThatNamesNothing(t *testing.T) {
	// Assert
	defer func() {
		if recover() == nil {
			t.Fatal("Candidates read for a locate naming nothing; want a panic")
		}
	}()

	// Act
	_, _ = Candidates(context.Background(), &recordingReader{}, LocateBy{})
}

// TestCandidatesThenLocateMatchesLocateOverTheWholeRegistry pins that reading
// the candidates loses no match: for every locate, Locate over the
// candidates a registry answers is Locate over the whole registry.
func TestCandidatesThenLocateMatchesLocateOverTheWholeRegistry(t *testing.T) {
	bys := []LocateBy{
		{Name: "open-one"}, {Name: "merged-one"}, {Name: "gone-one"},
		{At: "/wt/backlogged"}, {At: "/wt/open/"}, {At: "/wt/open/src"},
		{Containing: "/wt/open/nested/src"}, {Containing: "/wt/open/src"}, {Containing: "/wt/merged/src"}, {Containing: "/wt/openly"},
	}
	for _, by := range bys {
		t.Run(by.Name+by.At+by.Containing, func(t *testing.T) {
			// Arrange
			registry := registryReader(locateRegistry())

			// Act
			candidates, err := Candidates(context.Background(), registry, by)

			// Assert
			if err != nil {
				t.Fatalf("Candidates = %v", err)
			}
			got := Locate(candidates, locatePush(), by)
			want := Locate(locateRegistry(), locatePush(), by)
			if !reflect.DeepEqual(got, want) {
				t.Fatalf("Locate over the candidates = %v, want Locate over the registry's %v", got, want)
			}
		})
	}
}

// registryReader answers the head reads by filtering a whole registry, as
// the store's indexed reads do.
type registryReader []wsm.WorkspaceHead

func (r registryReader) WorkspaceHeadsAt(_ context.Context, dirs []string) ([]wsm.WorkspaceHead, error) {
	var out []wsm.WorkspaceHead
	for _, h := range r {
		for _, dir := range dirs {
			if h.Dir == dir {
				out = append(out, h)
			}
		}
	}
	return out, nil
}

func (r registryReader) WorkspaceHeadsNamed(_ context.Context, name string) ([]wsm.WorkspaceHead, error) {
	var out []wsm.WorkspaceHead
	for _, h := range r {
		if h.Name == name {
			out = append(out, h)
		}
	}
	return out, nil
}
