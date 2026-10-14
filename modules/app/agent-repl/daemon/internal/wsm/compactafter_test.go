package wsm

import (
	"context"
	"errors"
	"reflect"
	"testing"
	"time"
)

func TestRegisterWorkspaceRecordsAMergeWorkspacesCompactAfterAsk(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	requester := testWorkspace(t, s)
	dir := t.TempDir()
	origin := MergeOrigin{Requester: requester.ID, Request: NewDetachedMergeID(), ForkHead: "f00d", CompactAfter: true}

	// Act
	ws, _, err := s.RegisterWorkspace(context.Background(), dir, RegisterFacts{
		Name: "sample-merge", Branch: "sample-merge", ParentBranch: "master", Parent: &requester.ID, RepoDir: dir,
		MergeOf: &origin,
	})
	if err != nil {
		t.Fatalf("RegisterWorkspace: %v", err)
	}
	loaded, err := s.Workspace(context.Background(), ws.ID)

	// Assert
	if err != nil || loaded.MergeOf == nil || !reflect.DeepEqual(*loaded.MergeOf, origin) {
		t.Fatalf("MergeOf = %+v, %v; want %+v", loaded.MergeOf, err, origin)
	}
}

func TestACompactAfterAskOnARowThatIsNoMergeWorkspaceIsRefused(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	corrupt(t, s, `UPDATE workspaces SET merge_compact_after = 1 WHERE id = ?`, ws.ID)

	// Act
	_, err := s.Workspace(context.Background(), ws.ID)

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Field != "merge_compact_after" {
		t.Fatalf("Workspace = %v, want a merge_compact_after DecodeError", err)
	}
}

func TestADetachedRequestKeepsItsCompactAfterAsk(t *testing.T) {
	tests := []struct {
		name         string
		compactAfter bool
	}{
		{name: "asked", compactAfter: true},
		{name: "not asked", compactAfter: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			repo := RepoKey(t.TempDir())
			source := MergeSource{Kind: MergeSourceOwnBranch, Detach: NewDetachedMergeID(), DetachName: "reconnect-fix", CompactAfter: tt.compactAfter}

			// Act
			if err := s.RequestMerge(context.Background(), repo, ws.ID, source, time.Unix(100, 0)); err != nil {
				t.Fatalf("RequestMerge: %v", err)
			}
			queue, err := s.MergeQueue(context.Background(), repo)

			// Assert
			if err != nil || len(queue) != 1 || queue[0].Source.CompactAfter != tt.compactAfter {
				t.Fatalf("queue = %+v, %v; want the request with compact_after %v", queue, err, tt.compactAfter)
			}
		})
	}
}

func TestACompactAfterAskOnARequestThatRunsInPlaceIsRefused(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	source := MergeSource{Kind: MergeSourceOwnBranch, CompactAfter: true}

	// Act
	err := s.RequestMerge(context.Background(), repo, ws.ID, source, time.Unix(100, 0))
	queue, qerr := s.MergeQueue(context.Background(), repo)

	// Assert
	if err == nil || qerr != nil || len(queue) != 0 {
		t.Fatalf("RequestMerge = %v with queue %+v (%v); want a refusal with nothing recorded", err, queue, qerr)
	}
}

func TestACompactAfterAskOnAnInPlaceRowIsRefusedOnRead(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	if err := s.RequestMerge(context.Background(), repo, ws.ID, MergeSource{Kind: MergeSourceOwnBranch}, time.Unix(100, 0)); err != nil {
		t.Fatalf("RequestMerge: %v", err)
	}
	corrupt(t, s, `UPDATE merge_queue SET source_compact_after = 1 WHERE workspace_id = ?`, ws.ID)

	// Act
	_, err := s.MergeQueue(context.Background(), repo)

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Field != "source" {
		t.Fatalf("MergeQueue = %v, want a source DecodeError", err)
	}
}
