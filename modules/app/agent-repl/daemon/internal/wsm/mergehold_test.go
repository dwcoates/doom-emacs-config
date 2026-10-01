package wsm

import (
	"context"
	"errors"
	"testing"
)

// mergeKeepOpen reads whether the workspace's queued merge keeps its requester
// open.
func mergeKeepOpen(t *testing.T, s *store, repo RepoKey, ws WorkspaceID) bool {
	t.Helper()
	entries, err := s.MergeQueue(context.Background(), repo)
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	for _, e := range entries {
		if e.Workspace == ws {
			return e.Source.KeepOpen
		}
	}
	t.Fatalf("no queue entry of %s stands", ws)
	return false
}

// queuedWith requests a workspace's merge of source and moves it into line.
func queuedWith(t *testing.T, s *store, repo RepoKey, ws WorkspaceID, source MergeSource) {
	t.Helper()
	if err := s.RequestMerge(context.Background(), repo, ws, source, instant); err != nil {
		t.Fatalf("RequestMerge: %v", err)
	}
	if _, err := s.QueueMerge(context.Background(), repo, ws); err != nil {
		t.Fatalf("QueueMerge: %v", err)
	}
}

// mergeHeld is a prompt held by the workspace's merge.
func mergeHeld(ws WorkspaceID) HeldPrompt {
	kind := HoldMerge
	return HeldPrompt{Workspace: ws, Turn: NewTurnID(), Said: said("held"), Origin: "webapp", QueuedAt: instant, Hold: &kind}
}

// TestPutHeldPromptBindsAMergeHoldToItsMerge covers the source arms: a merge
// hold keeps the requester open exactly where the merge would close it.
func TestPutHeldPromptBindsAMergeHoldToItsMerge(t *testing.T) {
	tests := []struct {
		name     string
		source   MergeSource
		wantOpen bool
	}{
		{"own branch", MergeSource{Kind: MergeSourceOwnBranch}, true},
		{"merged upstream", MergeSource{Kind: MergeSourceMergedUpstream}, true},
		{"a branch that is no workspace", MergeSource{Kind: MergeSourceBranch, Branch: "feature"}, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			repo := RepoKey(t.TempDir())
			queuedWith(t, s, repo, ws.ID, tt.source)
			if _, err := s.AcquireLease(context.Background(), ws.ID, HolderMerge, PolicyHold); err != nil {
				t.Fatalf("AcquireLease: %v", err)
			}

			// Act
			err := s.PutHeldPrompt(context.Background(), mergeHeld(ws.ID))

			// Assert
			if err != nil {
				t.Fatalf("PutHeldPrompt: %v", err)
			}
			if got := mergeKeepOpen(t, s, repo, ws.ID); got != tt.wantOpen {
				t.Fatalf("keep_open = %v, want %v", got, tt.wantOpen)
			}
		})
	}
}

// TestPutHeldPromptRefusesAMergeHoldWithNoMergeLease covers the release
// winning the race: no merge lease, so the hold is refused, nothing is
// recorded, and the refusal is the arbitration's DEBUG answer.
func TestPutHeldPromptRefusesAMergeHoldWithNoMergeLease(t *testing.T) {
	tests := []struct {
		name   string
		holder *LeaseHolder
	}{
		{"no lease", nil},
		{"another holder's lease", func() *LeaseHolder { h := HolderRestart; return &h }()},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			ws := testWorkspace(t, s)
			repo := RepoKey(t.TempDir())
			queuedWith(t, s, repo, ws.ID, ownBranch)
			if tt.holder != nil {
				if _, err := s.AcquireLease(context.Background(), ws.ID, *tt.holder, PolicyHold); err != nil {
					t.Fatalf("AcquireLease: %v", err)
				}
			}

			// Act
			err := s.PutHeldPrompt(context.Background(), mergeHeld(ws.ID))

			// Assert
			if !errors.Is(err, ErrMergeLeaseGone) {
				t.Fatalf("PutHeldPrompt = %v, want ErrMergeLeaseGone", err)
			}
			held, err := s.HeldPrompts(context.Background(), ws.ID)
			if err != nil {
				t.Fatalf("HeldPrompts: %v", err)
			}
			if len(held) != 0 {
				t.Fatalf("a refused merge hold left %d rows", len(held))
			}
			if mergeKeepOpen(t, s, repo, ws.ID) {
				t.Fatalf("a refused merge hold set keep_open")
			}
			if !loggedOperation(log, "daemon.wsm.put_held_prompt", "debug") || loggedOperation(log, "daemon.wsm.put_held_prompt", "error") {
				t.Fatalf("the refusal was not the arbitration's debug record: %v", log.Records())
			}
		})
	}
}

// TestPutHeldPromptRefusesAMergeLeaseWithNoQueueEntry covers the invariant: a
// merge lease is always taken for a queued merge, so one with no entry is a
// defect, refused loudly with nothing recorded.
func TestPutHeldPromptRefusesAMergeLeaseWithNoQueueEntry(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)
	if _, err := s.AcquireLease(context.Background(), ws.ID, HolderMerge, PolicyHold); err != nil {
		t.Fatalf("AcquireLease: %v", err)
	}

	// Act
	err := s.PutHeldPrompt(context.Background(), mergeHeld(ws.ID))

	// Assert
	if err == nil || errors.Is(err, ErrMergeLeaseGone) {
		t.Fatalf("PutHeldPrompt = %v, want the invariant's refusal", err)
	}
	held, readErr := s.HeldPrompts(context.Background(), ws.ID)
	if readErr != nil {
		t.Fatalf("HeldPrompts: %v", readErr)
	}
	if len(held) != 0 {
		t.Fatalf("a refused merge hold left %d rows", len(held))
	}
	if !loggedOperation(log, "daemon.wsm.put_held_prompt", "error") {
		t.Fatalf("the invariant violation was not logged at error: %v", log.Records())
	}
}

// TestUpdateHeldPromptHoldBindsAMergeStamp covers the restamp path: a hold the
// merge's lease stamps keeps the requester open, as a fresh one does.
func TestUpdateHeldPromptHoldBindsAMergeStamp(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	queuedWith(t, s, repo, ws.ID, ownBranch)
	turn := standingHold(t, s, ws.ID)
	if _, err := s.AcquireLease(context.Background(), ws.ID, HolderMerge, PolicyHold); err != nil {
		t.Fatalf("AcquireLease: %v", err)
	}
	kind := HoldMerge

	// Act
	err := s.UpdateHeldPromptHold(context.Background(), turn, &kind, "")

	// Assert
	if err != nil {
		t.Fatalf("UpdateHeldPromptHold: %v", err)
	}
	if got := heldRow(t, s, turn); got.Hold == nil || *got.Hold != HoldMerge {
		t.Fatalf("hold = %v, want merge", got.Hold)
	}
	if !mergeKeepOpen(t, s, repo, ws.ID) {
		t.Fatalf("keep_open = false after a merge stamp")
	}
}

// TestUpdateHeldPromptHoldRefusesAMergeStampAfterTheRelease covers the restamp
// losing the race: the stamp is refused and the hold stands as it was.
func TestUpdateHeldPromptHoldRefusesAMergeStampAfterTheRelease(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	queuedWith(t, s, repo, ws.ID, ownBranch)
	turn := standingHold(t, s, ws.ID)
	kind := HoldMerge

	// Act
	err := s.UpdateHeldPromptHold(context.Background(), turn, &kind, "")

	// Assert
	if !errors.Is(err, ErrMergeLeaseGone) {
		t.Fatalf("UpdateHeldPromptHold = %v, want ErrMergeLeaseGone", err)
	}
	if got := heldRow(t, s, turn); got.Hold != nil {
		t.Fatalf("hold = %v after a refused stamp, want none", *got.Hold)
	}
}

// TestClosesRequesterNamesTheSourcesThatCloseTheirRequester covers the one
// definition the keep-open binding and the source validation share.
func TestClosesRequesterNamesTheSourcesThatCloseTheirRequester(t *testing.T) {
	tests := []struct {
		kind MergeSourceKind
		want bool
	}{
		{MergeSourceOwnBranch, true},
		{MergeSourceWorkspace, false},
		{MergeSourceBranch, false},
		{MergeSourceMergedUpstream, true},
	}
	for _, tt := range tests {
		t.Run(tt.kind.String(), func(t *testing.T) {
			// Act
			got := tt.kind.ClosesRequester()

			// Assert
			if got != tt.want {
				t.Fatalf("ClosesRequester = %v, want %v", got, tt.want)
			}
		})
	}
}

// TestMergeSourceKeepOpenFollowsClosesRequester covers the validation sharing
// that definition: keep_open is valid exactly on a source that closes its
// requester.
func TestMergeSourceKeepOpenFollowsClosesRequester(t *testing.T) {
	tests := []struct {
		name    string
		source  MergeSource
		wantErr bool
	}{
		{"own branch", MergeSource{Kind: MergeSourceOwnBranch, KeepOpen: true}, false},
		{"merged upstream", MergeSource{Kind: MergeSourceMergedUpstream, KeepOpen: true}, false},
		{"another workspace", MergeSource{Kind: MergeSourceWorkspace, Workspace: "ws-other", KeepOpen: true}, true},
		{"a branch", MergeSource{Kind: MergeSourceBranch, Branch: "feature", KeepOpen: true}, true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			err := tt.source.validate()

			// Assert
			if (err != nil) != tt.wantErr {
				t.Fatalf("validate = %v, want error %v", err, tt.wantErr)
			}
		})
	}
}
