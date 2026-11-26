package wsm

import (
	"context"
	"errors"
	"path/filepath"
	"reflect"
	"testing"
	"time"
)

// TestAWorkspacesHeadIsItsRowsHead pins the narrow read against the whole
// row: every field the head reads is the one the full row holds, for a
// repository workspace, a closed one and a thread.
func TestAWorkspacesHeadIsItsRowsHead(t *testing.T) {
	tests := []struct {
		name string
		make func(t *testing.T, s *store) WorkspaceID
	}{
		{name: "a repository workspace", make: func(t *testing.T, s *store) WorkspaceID {
			return testWorkspace(t, s).ID
		}},
		{name: "a closed workspace", make: func(t *testing.T, s *store) WorkspaceID {
			id := testWorkspace(t, s).ID
			corrupt(t, s, `UPDATE workspaces SET closed = 1 WHERE id = ?`, id)
			return id
		}},
		{name: "a merged workspace", make: func(t *testing.T, s *store) WorkspaceID {
			id := testWorkspace(t, s).ID
			if err := s.SetMergedAt(context.Background(), id, time.Unix(1_700_000_000, 0), nil); err != nil {
				t.Fatalf("SetMergedAt: %v", err)
			}
			return id
		}},
		{name: "a thread", make: func(t *testing.T, s *store) WorkspaceID {
			ws, err := s.RegisterThread(context.Background(), filepath.Join(t.TempDir(), "hydrogen-ions"), "hydrogen-ions")
			if err != nil {
				t.Fatalf("RegisterThread: %v", err)
			}
			return ws.ID
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			id := tt.make(t, s)
			full, err := s.Workspace(context.Background(), id)
			if err != nil {
				t.Fatalf("Workspace: %v", err)
			}

			// Act
			head, err := s.WorkspaceHead(context.Background(), id)

			// Assert
			if err != nil {
				t.Fatalf("WorkspaceHead: %v", err)
			}
			if head != full.Head() {
				t.Fatalf("head = %+v, want the full row's %+v", head, full.Head())
			}
		})
	}
}

func TestAnUnknownWorkspacesHeadIsNotFound(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	_, err := s.WorkspaceHead(context.Background(), "no-such-workspace")

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("WorkspaceHead = %v, want ErrNotFound", err)
	}
}

func TestAHeadNamesItsWorkspaceAsTheFullRowDoes(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	full := testWorkspace(t, s)

	// Act
	head := full.Head()

	// Assert
	if head.IsThread() != full.IsThread() {
		t.Fatalf("head IsThread = %v, want %v", head.IsThread(), full.IsThread())
	}
	if got, want := head.Ref().String(), full.Ref().String(); got != want {
		t.Fatalf("head Ref = %s, want %s", got, want)
	}
}

func TestAnUnknownWorkspaceKindIsADecodeError(t *testing.T) {
	// Act
	_, err := decodeWorkspaceKind("w1", "spaceship")

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Field != "kind" || decode.Row != "w1" {
		t.Fatalf("decodeWorkspaceKind = %v, want a DecodeError on w1's kind", err)
	}
}

// reread answers WS as the store holds it now.
func reread(t *testing.T, s *store, ws Workspace) Workspace {
	t.Helper()
	full, err := s.Workspace(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	return full
}

func TestWorkspaceHeadsAtAnswersTheWorkspacesAtTheDirectories(t *testing.T) {
	tests := []struct {
		name string
		dirs func(first, second, third Workspace) []string
		want func(first, second, third Workspace) []Workspace
	}{
		{name: "one directory", dirs: func(_, second, _ Workspace) []string { return []string{second.Dir} },
			want: func(_, second, _ Workspace) []Workspace { return []Workspace{second} }},
		{name: "several directories, in registry order whatever order they are asked in",
			dirs: func(first, _, third Workspace) []string { return []string{third.Dir, first.Dir} },
			want: func(first, _, third Workspace) []Workspace { return []Workspace{first, third} }},
		{name: "a directory no workspace is at", dirs: func(_, _, _ Workspace) []string { return []string{"/no/such/worktree"} },
			want: func(_, _, _ Workspace) []Workspace { return nil }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			first := testWorkspaceNamed(t, s, "first")
			second := testWorkspaceNamed(t, s, "second")
			third := testWorkspaceNamed(t, s, "third")

			// Act
			got, err := s.WorkspaceHeadsAt(context.Background(), tt.dirs(first, second, third))

			// Assert
			if err != nil {
				t.Fatalf("WorkspaceHeadsAt: %v", err)
			}
			var want []WorkspaceHead
			for _, ws := range tt.want(first, second, third) {
				want = append(want, reread(t, s, ws).Head())
			}
			if !reflect.DeepEqual(got, want) {
				t.Fatalf("WorkspaceHeadsAt = %+v, want %+v", got, want)
			}
		})
	}
}

func TestWorkspaceHeadsAtNoDirectoriesReadsNothing(t *testing.T) {
	// Arrange: a closed handle fails any read, so an answer means none ran.
	s, _ := testStore(t)
	if err := s.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Act
	got, err := s.WorkspaceHeadsAt(context.Background(), nil)

	// Assert
	if err != nil || got != nil {
		t.Fatalf("WorkspaceHeadsAt(nil) = %v, %v; want no heads and no read", got, err)
	}
}

func TestWorkspaceHeadsNamedAnswersEveryWorkspaceOfTheName(t *testing.T) {
	tests := []struct {
		name  string
		asked string
		want  func(twinA, other, twinB Workspace) []Workspace
	}{
		{name: "two workspaces of one name, in registry order", asked: "twin",
			want: func(twinA, _, twinB Workspace) []Workspace { return []Workspace{twinA, twinB} }},
		{name: "one workspace of its name", asked: "other",
			want: func(_, other, _ Workspace) []Workspace { return []Workspace{other} }},
		{name: "a name no workspace has", asked: "nobody",
			want: func(_, _, _ Workspace) []Workspace { return nil }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			twinA := testWorkspaceNamed(t, s, "twin")
			other := testWorkspaceNamed(t, s, "other")
			twinB := testWorkspaceNamed(t, s, "twin")

			// Act
			got, err := s.WorkspaceHeadsNamed(context.Background(), tt.asked)

			// Assert
			if err != nil {
				t.Fatalf("WorkspaceHeadsNamed: %v", err)
			}
			var want []WorkspaceHead
			for _, ws := range tt.want(twinA, other, twinB) {
				want = append(want, reread(t, s, ws).Head())
			}
			if !reflect.DeepEqual(got, want) {
				t.Fatalf("WorkspaceHeadsNamed = %+v, want %+v", got, want)
			}
		})
	}
}

func TestAHeadsReadThatBreaksIsAnError(t *testing.T) {
	tests := []struct {
		name      string
		operation string
		read      func(s *store) ([]WorkspaceHead, error)
	}{
		{name: "at directories", operation: "daemon.wsm.workspace_heads_at",
			read: func(s *store) ([]WorkspaceHead, error) {
				return s.WorkspaceHeadsAt(context.Background(), []string{"/wt/one"})
			}},
		{name: "named", operation: "daemon.wsm.workspace_heads_named",
			read: func(s *store) ([]WorkspaceHead, error) { return s.WorkspaceHeadsNamed(context.Background(), "one") }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			if err := s.Close(); err != nil {
				t.Fatalf("Close: %v", err)
			}

			// Act
			got, err := tt.read(s)

			// Assert
			if err == nil || got != nil {
				t.Fatalf("read on a closed handle = %v, %v; want an error and no heads", got, err)
			}
			if !loggedOperation(log, tt.operation, "error") {
				t.Fatalf("a broken heads read was not recorded at error: %v", log.Records())
			}
		})
	}
}

// TestTheMergedSetIsSpelledOnceForTheHeadAndTheRow pins that a full row's
// MergeSettled and InMergedBand are its head's, for every combination the
// answer turns on.
func TestTheMergedSetIsSpelledOnceForTheHeadAndTheRow(t *testing.T) {
	landed := time.Unix(1_700_000_000, 0)
	tests := []struct {
		name        string
		ws          Workspace
		wantSettled bool
		wantBand    bool
	}{
		{name: "open and unmerged", ws: Workspace{Kind: WorkspaceKindRepository}},
		{name: "open and merged: open beats merged", ws: Workspace{Kind: WorkspaceKindRepository, MergedAt: &landed}},
		{name: "closed and unmerged", ws: Workspace{Kind: WorkspaceKindRepository, Closed: true}},
		{name: "closed and merged", ws: Workspace{Kind: WorkspaceKindRepository, Closed: true, MergedAt: &landed}, wantSettled: true, wantBand: true},
		{name: "a closed merged thread is in no merged band", ws: Workspace{Kind: WorkspaceKindThread, Closed: true, MergedAt: &landed}, wantSettled: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			head := tt.ws.Head()

			// Assert
			if head.MergeSettled() != tt.wantSettled || tt.ws.MergeSettled() != tt.wantSettled {
				t.Fatalf("MergeSettled head %v row %v, want %v", head.MergeSettled(), tt.ws.MergeSettled(), tt.wantSettled)
			}
			if head.InMergedBand() != tt.wantBand || tt.ws.InMergedBand() != tt.wantBand {
				t.Fatalf("InMergedBand head %v row %v, want %v", head.InMergedBand(), tt.ws.InMergedBand(), tt.wantBand)
			}
		})
	}
}
