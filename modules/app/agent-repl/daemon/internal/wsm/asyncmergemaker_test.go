package wsm

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"
)

func TestADetachedRequestKeepsItsMakerAndTarget(t *testing.T) {
	tests := []struct {
		name    string
		toMaker bool
	}{
		{name: "landing on the primary branch"},
		{name: "landing into the maker's branch", toMaker: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			requester := testWorkspace(t, s)
			maker := testWorkspaceNamed(t, s, "maker")
			repo := RepoKey(t.TempDir())
			source := MergeSource{Kind: MergeSourceBranch, Branch: "fix", Detach: NewDetachedMergeID(), DetachName: "fix-merge",
				DetachMaker: maker.ID, DetachTargetMaker: tc.toMaker}

			// Act
			err := s.RequestMerge(context.Background(), repo, requester.ID, source, time.Unix(100, 0))
			queue, qerr := s.MergeQueue(context.Background(), repo)

			// Assert
			if err != nil || qerr != nil || len(queue) != 1 {
				t.Fatalf("RequestMerge = %v, queue = %+v, %v", err, queue, qerr)
			}
			if got := queue[0].Source; got.DetachMaker != maker.ID || got.DetachTargetMaker != tc.toMaker {
				t.Fatalf("maker = %q, into maker = %v; want %q, %v", got.DetachMaker, got.DetachTargetMaker, maker.ID, tc.toMaker)
			}
		})
	}
}

func TestMergeSourceRefusesAMakerOutsideADetachedRequest(t *testing.T) {
	tests := []struct {
		name   string
		source MergeSource
	}{
		{name: "a detached request with no maker", source: MergeSource{Kind: MergeSourceBranch, Branch: "b", Detach: "d", DetachName: "n"}},
		{name: "a maker on an in-place merge", source: MergeSource{Kind: MergeSourceOwnBranch, DetachMaker: "m"}},
		{name: "landing into a maker on an in-place merge", source: MergeSource{Kind: MergeSourceOwnBranch, DetachTargetMaker: true}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			err := tc.source.validate()

			// Assert
			if err == nil || !strings.Contains(err.Error(), "names the workspace it forks") {
				t.Fatalf("validate = %v, want the maker refusal", err)
			}
		})
	}
}

func TestClearMergePickLetsAQueuedMergeWorkspaceBeAdmitted(t *testing.T) {
	// Arrange: the hand-off puts the merge workspace in line with its pick
	// pending, and its pick begins at the head.
	s, _ := testStore(t)
	repo, forked := handedOffPick(t, s)
	ctx := context.Background()
	if _, err := s.QueueMerge(ctx, repo, forked.ID, "ledger-1"); err != nil {
		t.Fatalf("QueueMerge: %v", err)
	}
	if err := s.StartMergePick(ctx, repo, forked.ID, "ledger-1", NewTurnID(), time.Unix(300, 0)); err != nil {
		t.Fatalf("StartMergePick on a queued row: %v", err)
	}

	// Act
	err := s.ClearMergePick(ctx, repo, forked.ID, time.Unix(400, 0))
	queue, qerr := s.MergeQueue(ctx, repo)

	// Assert
	if err != nil || qerr != nil || len(queue) != 1 {
		t.Fatalf("ClearMergePick = %v, queue = %+v, %v", err, queue, qerr)
	}
	if got := queue[0].Source; got.Pick != "" || got.PickStartedAt != nil || got.PickTurn != "" {
		t.Fatalf("source = %+v, want the pick gone", got)
	}
	if queue[0].State != MergeQueued || queue[0].Ledger != "ledger-1" {
		t.Fatalf("entry = %+v, want it still queued under its ledger", queue[0])
	}
}

func TestClearMergePickRefusesARowWhosePickNeverBegan(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo, forked := handedOffPick(t, s)
	ctx := context.Background()
	if _, err := s.QueueMerge(ctx, repo, forked.ID, "ledger-1"); err != nil {
		t.Fatalf("QueueMerge: %v", err)
	}

	// Act
	err := s.ClearMergePick(ctx, repo, forked.ID, time.Unix(400, 0))

	// Assert: the store answers not-found, which its caller records.
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("ClearMergePick = %v, want ErrNotFound for a pick that never began", err)
	}
	queue, _ := s.MergeQueue(ctx, repo)
	if len(queue) != 1 || queue[0].Source.Pick == "" {
		t.Fatalf("queue = %+v, want the pending pick untouched", queue)
	}
}
