package wsm

import (
	"context"
	"testing"
)

// derivedRead reads one workspace back through each of the shared select
// list's readers, so a derived column cannot be decoded by only some of them.
func derivedRead(t *testing.T, s *store, id WorkspaceID) []Workspace {
	t.Helper()
	ctx := context.Background()
	byID, err := s.Workspace(ctx, id)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	byDir, err := s.WorkspaceByDir(ctx, byID.Dir)
	if err != nil {
		t.Fatalf("WorkspaceByDir: %v", err)
	}
	all, err := s.ListWorkspaces(ctx)
	if err != nil {
		t.Fatalf("ListWorkspaces: %v", err)
	}
	out := []Workspace{byID, byDir}
	for _, ws := range all {
		if ws.ID == id {
			out = append(out, ws)
		}
	}
	if len(out) != 3 {
		t.Fatalf("ListWorkspaces did not list %s", id)
	}
	return out
}

func TestWorkspaceReadsOneShotOffTheCreationJob(t *testing.T) {
	cases := []struct {
		name string
		job  *CreationJob
		want bool
	}{
		{name: "a one-shot creation job", job: &CreationJob{OneShot: true}, want: true},
		{name: "an ordinary creation job", job: &CreationJob{OneShot: false}, want: false},
		{name: "no creation job", job: nil, want: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testLinkedWorkspace(t, s)
			if tc.job != nil {
				job := *tc.job
				job.Workspace = ws.ID
				job.CreatedAt = instant
				if err := s.PutCreationJob(context.Background(), job); err != nil {
					t.Fatalf("PutCreationJob: %v", err)
				}
			}

			// Act
			reads := derivedRead(t, s, ws.ID)

			// Assert
			for i, got := range reads {
				if got.OneShot != tc.want {
					t.Fatalf("read %d: OneShot = %v, want %v", i, got.OneShot, tc.want)
				}
			}
		})
	}
}

func TestWorkspaceReadsMergePendingOffTheMergeQueue(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(t *testing.T, s *store, repo RepoKey, id WorkspaceID)
		want    bool
	}{
		{
			name:    "no entry",
			arrange: func(*testing.T, *store, RepoKey, WorkspaceID) {},
			want:    false,
		},
		{
			name: "a recorded request",
			arrange: func(t *testing.T, s *store, repo RepoKey, id WorkspaceID) {
				if err := s.RequestMerge(context.Background(), repo, id, ownBranch, instant); err != nil {
					t.Fatalf("RequestMerge: %v", err)
				}
			},
			want: true,
		},
		{
			name: "an entry in line",
			arrange: func(t *testing.T, s *store, repo RepoKey, id WorkspaceID) {
				queued(t, s, repo, id, instant)
			},
			want: true,
		},
		{
			name: "an admitted entry",
			arrange: func(t *testing.T, s *store, repo RepoKey, id WorkspaceID) {
				queued(t, s, repo, id, instant)
				if err := s.AdmitMerge(context.Background(), repo, id); err != nil {
					t.Fatalf("AdmitMerge: %v", err)
				}
			},
			want: true,
		},
		{
			name: "a merge workspace's entry for its detached merge",
			arrange: func(t *testing.T, s *store, repo RepoKey, id WorkspaceID) {
				forked := registerMergeWorkspace(t, s, id, NewDetachedMergeID())
				queued(t, s, repo, forked.ID, instant)
			},
			want: true,
		},
		{
			name: "a merge workspace's removed entry",
			arrange: func(t *testing.T, s *store, repo RepoKey, id WorkspaceID) {
				forked := registerMergeWorkspace(t, s, id, NewDetachedMergeID())
				queued(t, s, repo, forked.ID, instant)
				if err := s.RemoveMergeQueueEntry(context.Background(), repo, forked.ID, "failed"); err != nil {
					t.Fatalf("RemoveMergeQueueEntry: %v", err)
				}
			},
			want: false,
		},
		{
			name: "another workspace's entry",
			arrange: func(t *testing.T, s *store, repo RepoKey, _ WorkspaceID) {
				other := testWorkspaceNamed(t, s, "other")
				queued(t, s, repo, other.ID, instant)
			},
			want: false,
		},
		{
			name: "a removed entry",
			arrange: func(t *testing.T, s *store, repo RepoKey, id WorkspaceID) {
				queued(t, s, repo, id, instant)
				if err := s.RemoveMergeQueueEntry(context.Background(), repo, id, "failed"); err != nil {
					t.Fatalf("RemoveMergeQueueEntry: %v", err)
				}
			},
			want: false,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			tc.arrange(t, s, RepoKey(t.TempDir()), ws.ID)

			// Act
			reads := derivedRead(t, s, ws.ID)

			// Assert
			for i, got := range reads {
				if got.MergePending != tc.want {
					t.Fatalf("read %d: MergePending = %v, want %v", i, got.MergePending, tc.want)
				}
			}
		})
	}
}
