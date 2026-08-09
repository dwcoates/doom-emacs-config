package ssm

import (
	"errors"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// mergeGateRig arranges one operational workspace resting on a named merge
// phase, which is the shape every case below needs and the only thing they
// differ in.
func mergeGateRig(t *testing.T, phase string) *Manager {
	t.Helper()
	m, _, _ := openUnwiredTest(t, fakeResolver{"s1": "ws1"})
	connectOperational(t, m, "ws1", "s1", "g1")
	if err := m.ApplyMergeTransition("ws1", phase, "test arrangement"); err != nil {
		t.Fatalf("ApplyMergeTransition(%s): %v", phase, err)
	}
	return m
}

func TestIdleMachineryPromptIsAcceptedWhileMergeQueued(t *testing.T) {
	// Arrange: a workspace waiting its turn for the merge lock. No coordinator
	// owns its shim, so the daemon's own cache keep-alive ping is safe — and it
	// is the ONE thing keeping the prompt cache alive while the queue moves.
	m := mergeGateRig(t, sigMergeQueued)

	// Act.
	var published *frontendv1.WorkspaceState
	err := m.MarkPromptAccepted("ws1", "s1", "ka_1", PromptAdmissionIdleMachinery,
		func(state *frontendv1.WorkspaceState) { published = state })

	// Assert.
	if err != nil {
		t.Fatalf("MarkPromptAccepted(idle machinery, merge_queued) = %v, want the ping admitted", err)
	}
	if published.GetState() != frontendv1.RenderState_RENDER_STATE_MERGE_QUEUED || !published.GetTurnActive() {
		t.Fatalf("published = %s turn_active=%v, want MERGE_QUEUED/true: the badge is the queue's, and the turn claim is still required",
			published.GetState(), published.GetTurnActive())
	}
}

func TestIdleMachineryPromptIsAcceptedWhileMergeFailed(t *testing.T) {
	// Arrange: merge_failed is TERMINAL — the run is over and nobody owns the
	// shim — so a workspace evicted from the queue by a failed run is an
	// ordinary live session as far as the keep-alive cadence is concerned.
	m := mergeGateRig(t, sigMergeFailed)

	// Act.
	err := m.MarkPromptAccepted("ws1", "s1", "ka_1", PromptAdmissionIdleMachinery,
		func(*frontendv1.WorkspaceState) {})

	// Assert.
	if err != nil {
		t.Fatalf("MarkPromptAccepted(idle machinery, merge_failed) = %v, want the ping admitted", err)
	}
}

func TestIdleMachineryPromptIsRefusedWhileMerging(t *testing.T) {
	// Arrange: an ACTIVE merge phase. The coordinator holds the shim, its own
	// turns keep the cache warm, and a machinery turn racing them is exactly
	// what the exclusivity claim exists to prevent.
	m := mergeGateRig(t, sigMerging)

	// Act.
	err := m.MarkPromptAccepted("ws1", "s1", "ka_1", PromptAdmissionIdleMachinery,
		func(*frontendv1.WorkspaceState) {})

	// Assert.
	if !errors.Is(err, ErrPromptRefusedByMergeState) {
		t.Fatalf("MarkPromptAccepted(idle machinery, merging) = %v, want %v", err, ErrPromptRefusedByMergeState)
	}
}

func TestIdleMachineryPromptIsRefusedWhileMergeConflict(t *testing.T) {
	// Arrange: a conflicted workspace is parked awaiting a human, and the merge
	// machinery still owns its session until they act.
	m := mergeGateRig(t, sigMergeConflict)

	// Act.
	err := m.MarkPromptAccepted("ws1", "s1", "ka_1", PromptAdmissionIdleMachinery,
		func(*frontendv1.WorkspaceState) {})

	// Assert.
	if !errors.Is(err, ErrPromptRefusedByMergeState) {
		t.Fatalf("MarkPromptAccepted(idle machinery, merge_conflict) = %v, want %v", err, ErrPromptRefusedByMergeState)
	}
}

func TestUserPromptIsStillRefusedWhileMergeQueued(t *testing.T) {
	// Arrange: the exemption is by SUBMITTER. A user prompt aimed at a queued
	// workspace is refused exactly as it was before the exemption existed.
	m := mergeGateRig(t, sigMergeQueued)

	// Act.
	err := m.MarkPromptAccepted("ws1", "s1", "req-1", PromptAdmissionUser,
		func(*frontendv1.WorkspaceState) {})

	// Assert.
	if err == nil {
		t.Fatal("MarkPromptAccepted(user, merge_queued) = nil, want the queued workspace to refuse the user's prompt")
	}
}

func TestMergeQueuedUserRefusalIsClassifiedAsAMergeRefusal(t *testing.T) {
	// Arrange: the refusal used to be a bare fmt.Errorf, so it reached a human
	// as an unclassified daemon fault rather than as "a merge owns this
	// workspace". The sentinel is what a caller matches on.
	m := mergeGateRig(t, sigMergeQueued)

	// Act.
	err := m.MarkPromptAccepted("ws1", "s1", "req-1", PromptAdmissionUser,
		func(*frontendv1.WorkspaceState) {})

	// Assert.
	if !errors.Is(err, ErrPromptRefusedByMergeState) {
		t.Fatalf("err = %v, want it to carry %v", err, ErrPromptRefusedByMergeState)
	}
}

func TestMergeStateRefusalStillNamesTheStateItRefusedOn(t *testing.T) {
	// Arrange: the sentinel is ADDITIVE. Everything the old sentence carried —
	// which state refused, and whether a turn was claimed — must survive it.
	m := mergeGateRig(t, sigMergeConflict)

	// Act.
	err := m.MarkPromptAccepted("ws1", "s1", "req-1", PromptAdmissionUser,
		func(*frontendv1.WorkspaceState) {})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "state=RENDER_STATE_MERGE_CONFLICT") {
		t.Fatalf("err = %v, want the refused state named in the message", err)
	}
}

func TestNonMergeInvariantFailureDoesNotBorrowTheMergeSentinel(t *testing.T) {
	// Arrange: a hibernation lease refuses turn admission long before the merge
	// axis is consulted, so its failure must NOT classify as a merge refusal.
	m, _, _ := openUnwiredTest(t, fakeResolver{"s1": "ws1"})
	_, _, release, err := m.AcquireHibernationLease("ws1")
	if err != nil {
		t.Fatalf("AcquireHibernationLease: %v", err)
	}
	defer release()

	// Act.
	err = m.MarkPromptAccepted("ws1", "s1", "req-1", PromptAdmissionUser,
		func(*frontendv1.WorkspaceState) {})

	// Assert.
	if err == nil {
		t.Fatal("MarkPromptAccepted under a hibernation lease = nil, want a refusal")
	}
	if errors.Is(err, ErrPromptRefusedByMergeState) {
		t.Fatalf("err = %v, want a NON-merge refusal: no merge owns this workspace", err)
	}
}

func TestMergeStateLeavesSessionIdleNamesOnlyTheTwoWaitingStates(t *testing.T) {
	// Arrange: the membership is the whole exemption, so it is asserted
	// directly rather than only through the states that happen to be tested
	// above.
	tests := []struct {
		name  string
		state frontendv1.RenderState
		want  bool
	}{
		{"queued", frontendv1.RenderState_RENDER_STATE_MERGE_QUEUED, true},
		{"failed", frontendv1.RenderState_RENDER_STATE_MERGE_FAILED, true},
		{"enqueuing", frontendv1.RenderState_RENDER_STATE_MERGE_ENQUEUING, false},
		{"merging", frontendv1.RenderState_RENDER_STATE_MERGING, false},
		{"conflict", frontendv1.RenderState_RENDER_STATE_MERGE_CONFLICT, false},
		{"merged", frontendv1.RenderState_RENDER_STATE_MERGED, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := mergeStateLeavesSessionIdle(tt.state)
			// Assert.
			if got != tt.want {
				t.Fatalf("mergeStateLeavesSessionIdle(%s) = %v, want %v", tt.state, got, tt.want)
			}
		})
	}
}

func TestIdleMachineryAdmissionStillRequiresATurnClaim(t *testing.T) {
	// Arrange: the exemption relaxes which render state may HIDE the claim, not
	// whether a claim is required. A state with no turn active must refuse the
	// machinery prompt exactly as it refuses the user's.
	state := &frontendv1.WorkspaceState{
		State:      frontendv1.RenderState_RENDER_STATE_MERGE_QUEUED,
		TurnActive: false,
	}

	// Act.
	got := admitsIdleMachineryTurn(PromptAdmissionIdleMachinery, state)

	// Assert.
	if got {
		t.Fatal("admitsIdleMachineryTurn = true with no turn claimed, want the claim still required")
	}
}
