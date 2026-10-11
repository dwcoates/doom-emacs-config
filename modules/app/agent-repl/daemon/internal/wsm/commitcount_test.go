package wsm

import (
	"context"
	"errors"
	"testing"
)

func countPtr(n uint32) *uint32 { return &n }

func commitCountOf(t *testing.T, s *store, id WorkspaceID) *uint32 {
	t.Helper()
	got, err := s.Workspace(context.Background(), id)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	return got.CommitCount
}

func TestANewWorkspaceHasNoCommitCount(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	ws := testWorkspace(t, s)

	// Assert
	if got := commitCountOf(t, s, ws.ID); got != nil {
		t.Fatalf("commit count = %d, want none", *got)
	}
}

func TestSetLiveCommitCount(t *testing.T) {
	tests := []struct {
		name        string
		mergedWith  *uint32 // nil leaves the workspace live
		mergedFirst bool
		write       uint32
		wantApplied bool
		want        *uint32
	}{
		{name: "stores a live workspace's count", write: 5, wantApplied: true, want: countPtr(5)},
		{name: "stores a zero count", write: 0, wantApplied: true, want: countPtr(0)},
		{name: "leaves a merged workspace's landed count", mergedFirst: true, mergedWith: countPtr(3), write: 0, wantApplied: false, want: countPtr(3)},
		{name: "writes nothing on a merged workspace with no landed count", mergedFirst: true, write: 7, wantApplied: false, want: nil},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			if tc.mergedFirst {
				if err := s.SetMergedAt(context.Background(), ws.ID, instant, tc.mergedWith); err != nil {
					t.Fatalf("SetMergedAt: %v", err)
				}
			}

			// Act
			applied, err := s.SetLiveCommitCount(context.Background(), ws.ID, tc.write)

			// Assert
			if err != nil {
				t.Fatalf("SetLiveCommitCount: %v", err)
			}
			if applied != tc.wantApplied {
				t.Fatalf("applied = %v, want %v", applied, tc.wantApplied)
			}
			got := commitCountOf(t, s, ws.ID)
			if (got == nil) != (tc.want == nil) || (got != nil && *got != *tc.want) {
				t.Fatalf("commit count = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestSetLiveCommitCountRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	s, log := testStore(t)

	// Act
	applied, err := s.SetLiveCommitCount(context.Background(), WorkspaceID("absent"), 4)

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("SetLiveCommitCount = %v, want ErrNotFound", err)
	}
	if applied {
		t.Fatal("applied = true for a workspace that is not there")
	}
	found := false
	for _, r := range log.Records() {
		if r.Operation == "daemon.wsm.set_live_commit_count" && r.Context["workspace"] == "absent" {
			found = true
		}
	}
	if !found {
		t.Fatalf("no daemon.wsm.set_live_commit_count record names the workspace: %v", log.Records())
	}
}

func TestSetMergedAtStampsTheLandedCount(t *testing.T) {
	tests := []struct {
		name   string
		before *uint32 // a live count recorded first
		landed *uint32
		want   *uint32
	}{
		{name: "stamps the landed count", before: countPtr(2), landed: countPtr(4), want: countPtr(4)},
		{name: "keeps the recorded count when none landed", before: countPtr(2), landed: nil, want: countPtr(2)},
		{name: "stays uncounted when nothing was ever counted", landed: nil, want: nil},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			if tc.before != nil {
				if _, err := s.SetLiveCommitCount(context.Background(), ws.ID, *tc.before); err != nil {
					t.Fatalf("SetLiveCommitCount: %v", err)
				}
			}

			// Act
			if err := s.SetMergedAt(context.Background(), ws.ID, instant, tc.landed); err != nil {
				t.Fatalf("SetMergedAt: %v", err)
			}

			// Assert
			got := commitCountOf(t, s, ws.ID)
			if (got == nil) != (tc.want == nil) || (got != nil && *got != *tc.want) {
				t.Fatalf("commit count = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestSetMergedAtWithACountRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.SetMergedAt(context.Background(), WorkspaceID("absent"), instant, countPtr(1))

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("SetMergedAt = %v, want ErrNotFound", err)
	}
}

func TestWorkspaceRefusesACorruptCommitCount(t *testing.T) {
	tests := []struct {
		name  string
		value int64
	}{
		{name: "negative", value: -1},
		{name: "past uint32", value: 1 << 32},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			corrupt(t, s, `UPDATE workspaces SET commit_count = ? WHERE id = ?`, tc.value, ws.ID)

			// Act
			_, err := s.Workspace(context.Background(), ws.ID)

			// Assert
			var decodeErr *DecodeError
			if !errors.As(err, &decodeErr) || decodeErr.Field != "commit_count" {
				t.Fatalf("Workspace = %v, want a DecodeError naming commit_count", err)
			}
		})
	}
}

// TestTheMigrationAddsTheCommitCountColumn pins the layout-34 step.
func TestTheMigrationAddsTheCommitCountColumn(t *testing.T) {
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
	if got := scalar[int](t, s, `SELECT count(*) FROM pragma_table_info('workspaces') WHERE name = 'commit_count'`); got != 1 {
		t.Fatalf("workspaces.commit_count exists %d times after the migration, want 1", got)
	}
	if got := scalar[int](t, s, `SELECT count(*) FROM workspaces WHERE commit_count IS NOT NULL`); got != 0 {
		t.Fatalf("%d migrated rows carry a commit count, want none", got)
	}
}
