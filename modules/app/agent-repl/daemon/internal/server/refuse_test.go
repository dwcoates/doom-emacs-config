package server

import (
	"errors"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/drain"
	"claude-repld/internal/merge"
	"claude-repld/internal/prompthandler"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/resolve/feed"
	"claude-repld/internal/rollout"
	"claude-repld/internal/workspace"
)

// TestSetResponseErrorSetsTheNamedArm pins the mapping's core: an arm the rpc's
// error message carries is set by NAME.
func TestSetResponseErrorSetsTheNamedArm(t *testing.T) {
	// Arrange.
	resp := &agentreplv1.MergeWorkspaceResponse{}

	// Act.
	ok := setResponseError(resp, "already_queued", nil)

	// Assert.
	if !ok || resp.GetError().GetAlreadyQueued() == nil {
		t.Fatalf("already_queued was not set: ok=%v resp=%v", ok, resp)
	}
}

// TestSetResponseErrorRefusesAnUnlandedArm pins that an arm the contract does
// NOT carry falls through, which is what routes it to UnlandedArm.
func TestSetResponseErrorRefusesAnUnlandedArm(t *testing.T) {
	// Arrange.
	resp := &agentreplv1.MergeWorkspaceResponse{}

	// Act.
	ok := setResponseError(resp, "duplicate_submission", nil)

	// Assert.
	if ok {
		t.Fatal("an arm MergeWorkspaceError does not carry was reported as set")
	}
}

// TestSetResponseErrorFillsTheArmsOwnField pins that an arm's own field is
// filled from the refusal rather than left empty.
func TestSetResponseErrorFillsTheArmsOwnField(t *testing.T) {
	// Arrange.
	resp := &agentreplv1.MergeWorkspaceResponse{}

	// Act.
	setResponseError(resp, "transferring_away", map[string]any{"address": "127.0.0.1:1"})

	// Assert.
	if got := resp.GetError().GetTransferringAway().GetAddress(); got != "127.0.0.1:1" {
		t.Fatalf("address = %q, want the successor's", got)
	}
}

// TestSetResponseErrorFillsAnInt64Field pins the numeric arm field, which the
// confirm_required challenge carries.
func TestSetResponseErrorFillsAnInt64Field(t *testing.T) {
	// Arrange.
	resp := &agentreplv1.InterruptResponse{}

	// Act.
	setResponseError(resp, "confirm_required", map[string]any{"live_agent_count": int64(3)})

	// Assert.
	if got := resp.GetError().GetConfirmRequired().GetLiveAgentCount(); got != 3 {
		t.Fatalf("live_agent_count = %d, want 3", got)
	}
}

// TestAsRefusalNormalizesEveryComponentSentinel pins that every documented
// component refusal reaches the mapping as its arm NAME.
func TestAsRefusalNormalizesEveryComponentSentinel(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	surface := h.Server.(*server)
	tests := []struct {
		name string
		err  error
		arm  string
	}{
		{"workspace refusal", &workspace.Refusal{Arm: "no_session", Reason: "x"}, "no_session"},
		{"merge refusal", &merge.RefusalError{Arm: "already_merging", Reason: "x"}, "already_merging"},
		{"confirm challenge", &workspace.ConfirmRequired{LiveAgentCount: 2}, "confirm_required"},
		{"shim refusal", &workspace.ShimRefusal{Arm: "not_deliverable"}, "not_deliverable"},
		{"duplicate submission", prompthandler.ErrDuplicateSubmission, "duplicate_submission"},
		{"feed not in workspace", prompthandler.ErrFeedNotInWorkspace, "feed_not_in_workspace"},
		{"merging", promptqueue.ErrMerging, "merging"},
		{"no such hold", promptqueue.ErrNoSuchHold, "no_such_hold"},
		{"already delivered", promptqueue.ErrAlreadyDelivered, "already_delivered"},
		{"accept not applicable", promptqueue.ErrAcceptNotApplicable, "accept_not_applicable"},
		{"release refused", promptqueue.ErrReleaseRefused, "release_refused"},
		{"no walk", feed.ErrNoWalk, "no_walk_standing"},
		{"nothing scheduled", drain.ErrNothingScheduled, "nothing_scheduled"},
		{"no transfer announced", rollout.ErrNoTransferAnnounced, "no_transfer_announced"},
		{"not yet adopted", rollout.ErrNotYetAdopted, "not_yet_adopted"},
		{"participant not expected", rollout.ErrParticipantNotExpected, "participant_not_expected"},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Act.
			got, ok := surface.asRefusal(test.err)

			// Assert.
			if !ok || got.Arm != test.arm {
				t.Fatalf("arm = %q (ok=%v), want %q", got.Arm, ok, test.arm)
			}
		})
	}
}

// TestAsRefusalRejectsAnOrdinaryFailure pins that a plain error is NOT turned
// into a refusal answer: it must surface as an error.
func TestAsRefusalRejectsAnOrdinaryFailure(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	surface := h.Server.(*server)

	// Act.
	_, ok := surface.asRefusal(errors.New("the disk caught fire"))

	// Assert.
	if ok {
		t.Fatal("an ordinary failure was normalized into a typed refusal")
	}
}

// TestUnlandedArmSpellsTheLedgerMessage pins the EXACT message ERROR-ARMS.md
// prescribes, because the ledger reconciles against it.
func TestUnlandedArmSpellsTheLedgerMessage(t *testing.T) {
	// Arrange.
	const want = "intended arm: WatchFeedError.unknown_token: the token was never minted"

	// Act.
	cerr := UnlandedArm(fakeLogger{}, "WatchFeed", "unknown_token", "the token was never minted", false)

	// Assert.
	if got := cerr.Message(); got != want {
		t.Fatalf("message = %q, want %q", got, want)
	}
}

// TestUnlandedArmUsesNotFoundForAnUnknownID pins the code split: an unknown id
// answers NotFound, a state refusal answers FailedPrecondition.
func TestUnlandedArmUsesNotFoundForAnUnknownID(t *testing.T) {
	// Arrange, Act.
	cerr := UnlandedArm(fakeLogger{}, "WatchFeed", "unknown_token", "no such token", true)

	// Assert.
	if got := cerr.Code().String(); got != "not_found" {
		t.Fatalf("code = %q, want not_found", got)
	}
}

// TestAsRefusalCarriesTheVerbsArmFields pins that an arm's OWN evidence
// survives the normalization: base_ref_unresolved spells `ref`, and a client
// reading the arm must get the ref rather than an empty string beside prose.
func TestAsRefusalCarriesTheVerbsArmFields(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	surface := h.Server.(*server)
	err := &workspace.Refusal{
		Arm:    workspace.ArmBaseRefUnresolved,
		Reason: "the base ref does not resolve",
		Fields: map[string]any{"ref": "origin/nope"},
	}

	// Act.
	got, ok := surface.asRefusal(err)

	// Assert.
	if !ok {
		t.Fatal("the workspace refusal was not normalized")
	}
	if got.Fields["ref"] != "origin/nope" {
		t.Fatalf("Fields = %v, want the arm's ref value", got.Fields)
	}
}

// TestAsRefusalDoesNotMutateTheVerbsFieldMap pins that the transport's shared
// text/detail values are not written back into the component's own map.
func TestAsRefusalDoesNotMutateTheVerbsFieldMap(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	surface := h.Server.(*server)
	fields := map[string]any{"ref": "origin/nope"}
	err := &workspace.Refusal{Arm: workspace.ArmBaseRefUnresolved, Reason: "nope", Fields: fields}

	// Act.
	if _, ok := surface.asRefusal(err); !ok {
		t.Fatal("the workspace refusal was not normalized")
	}

	// Assert.
	if len(fields) != 1 {
		t.Fatalf("the verb's own field map = %v, want it untouched", fields)
	}
}

// TestSetArmFillsTheBaseRefUnresolvedRef pins the whole path end to end: the
// arm the wire carries names the ref.
func TestSetArmFillsTheBaseRefUnresolvedRef(t *testing.T) {
	// Arrange.
	resp := &agentreplv1.CreateWorkspaceResponse{}

	// Act.
	ok := setResponseError(resp, workspace.ArmBaseRefUnresolved, map[string]any{"ref": "origin/nope"})

	// Assert.
	if !ok {
		t.Fatal("setResponseError refused the landed base_ref_unresolved arm")
	}
	if got := resp.GetError().GetBaseRefUnresolved().GetRef(); got != "origin/nope" {
		t.Fatalf("base_ref_unresolved.ref = %q, want origin/nope", got)
	}
}

// TestSetArmSelectsANestedOneofArm pins the landing-7 shape: an arm message
// with a oneof of its OWN (SubmitPromptBubbleRefused.kind) has that arm
// selected by name.
func TestSetArmSelectsANestedOneofArm(t *testing.T) {
	// Arrange.
	resp := &agentreplv1.SubmitPromptResponse{}

	// Act.
	ok := setResponseError(resp, "bubble_refused", map[string]any{
		"detail": "the subagent's turn is running",
		"kind":   nestedArm("agent_busy"),
	})

	// Assert.
	if !ok || resp.GetError().GetBubbleRefused().GetAgentBusy() == nil {
		t.Fatalf("setResponseError = %v, %v, want bubble_refused{kind: agent_busy}", ok, resp)
	}
}

// TestSetArmCarriesTheDetailBesideTheNestedArm pins that the shim's own words
// ride the arm as `detail`, one edge case per test.
func TestSetArmCarriesTheDetailBesideTheNestedArm(t *testing.T) {
	// Arrange.
	resp := &agentreplv1.SubmitPromptResponse{}

	// Act.
	setResponseError(resp, "bubble_refused", map[string]any{
		"detail": "the subagent's turn is running",
		"kind":   nestedArm("agent_busy"),
	})

	// Assert.
	if got := resp.GetError().GetBubbleRefused().GetDetail(); got != "the subagent's turn is running" {
		t.Fatalf("bubble_refused.detail = %q, want the shim's sentence", got)
	}
}

// TestSetArmRefusesANestedArmTheMessageDoesNotCarry pins that an unknown nested
// arm is REFUSED rather than dropped: the refusal then answers as an unlanded
// arm instead of arriving with an empty kind.
func TestSetArmRefusesANestedArmTheMessageDoesNotCarry(t *testing.T) {
	// Arrange.
	resp := &agentreplv1.SubmitPromptResponse{}

	// Act.
	ok := setResponseError(resp, "bubble_refused", map[string]any{"kind": nestedArm("no_such_kind")})

	// Assert.
	if ok {
		t.Fatalf("setResponseError with an unknown nested arm = true, want a refusal")
	}
}
