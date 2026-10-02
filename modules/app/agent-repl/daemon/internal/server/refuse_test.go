package server

import (
	"context"
	"errors"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"google.golang.org/protobuf/proto"

	"claude-repld/internal/bounce"
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

// TestSetResponseErrorFillsTheLockHolderUnavailableArm pins that OpenWorkspace
// answers a failed lock holder on its TYPED arm, the shim's LockHolderFailure
// whole, so a client never has to read it out of an unlanded-arm sentence.
func TestSetResponseErrorFillsTheLockHolderUnavailableArm(t *testing.T) {
	// Arrange.
	resp := &agentreplv1.OpenWorkspaceResponse{}
	failure := &conversationv1.LockHolderFailure{
		Binary: "/bin/shim-lock",
		How:    &conversationv1.LockHolderFailure_Signaled{Signaled: &conversationv1.LockHolderSignaled{Signal: "SIGSEGV"}},
	}

	// Act.
	setResponseError(resp, workspace.ArmLockHolderUnavailable, map[string]any{"failure": failure})

	// Assert.
	if got := resp.GetError().GetLockHolderUnavailable().GetFailure(); !proto.Equal(got, failure) {
		t.Fatalf("failure = %v, want %v", got, failure)
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
		{"being delivered", promptqueue.ErrBeingDelivered, "being_delivered"},
		{"accept not applicable", promptqueue.ErrAcceptNotApplicable, "accept_not_applicable"},
		{"release refused", promptqueue.ErrReleaseRefused, "release_refused"},
		{"not held", promptqueue.ErrNotHeld, "not_held"},
		{"not editing", promptqueue.ErrNotEditing, "not_editing"},
		{"no editor", promptqueue.ErrNoEditor, "no_editor"},
		{"being edited", &promptqueue.BeingEditedError{Turn: "turn-0"}, "being_edited"},
		{"no walk", feed.ErrNoWalk, "no_walk_standing"},
		{"nothing scheduled", drain.ErrNothingScheduled, "nothing_scheduled"},
		{"no transfer announced", rollout.ErrNoTransferAnnounced, "no_transfer_announced"},
		{"not yet adopted", rollout.ErrNotYetAdopted, "not_yet_adopted"},
		{"participant not expected", rollout.ErrParticipantNotExpected, "participant_not_expected"},
		{"moved away by a sealed handover move", fmt.Errorf("restart %q: %w", "w1", bounce.ErrMovedAway), "transferring_away"},
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

// TestAsRefusalUnwrapsAWrappedWorkspaceRefusal pins that the normalization goes
// through workspace.AsRefusal, the package's canonical extractor, so a refusal
// a verb wrapped on its way out still names its arm.
func TestAsRefusalUnwrapsAWrappedWorkspaceRefusal(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	surface := h.Server.(*server)
	err := fmt.Errorf("CreateWorkspace: %w",
		&workspace.Refusal{Arm: workspace.ArmBaseRefUnresolved, Reason: "the base ref does not resolve"})

	// Act.
	got, ok := surface.asRefusal(err)

	// Assert.
	if !ok || got.Arm != workspace.ArmBaseRefUnresolved {
		t.Fatalf("asRefusal = %+v, %t, want the wrapped arm", got, ok)
	}
}

// TestAsRefusalCarriesTheMergeRefusalsReason pins that the merge extractor
// hands back the refusal's SENTENCE as well as its arm: the reason becomes the
// arm's text and detail fields, so an extractor that dropped it would answer
// every merge refusal with an empty message.
func TestAsRefusalCarriesTheMergeRefusalsReason(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	surface := h.Server.(*server)
	err := fmt.Errorf("MergeWorkspace: %w",
		&merge.RefusalError{Arm: merge.ArmAlreadyQueued, Reason: "a merge is already queued"})

	// Act.
	got, ok := surface.asRefusal(err)

	// Assert.
	if !ok || got.Arm != merge.ArmAlreadyQueued || got.Reason != "a merge is already queued" {
		t.Fatalf("asRefusal = %+v, %t, want the merge arm and its reason", got, ok)
	}
}

// TestAsRefusalPutsTheMergeReasonOnTheArmsTextField pins the one consequence
// that made the arm-only extractor unusable here: the reason must reach the
// arm's own text field, not only the refusal's prose.
func TestAsRefusalPutsTheMergeReasonOnTheArmsTextField(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	surface := h.Server.(*server)
	err := &merge.RefusalError{Arm: merge.ArmAlreadyQueued, Reason: "a merge is already queued"}

	// Act.
	got, _ := surface.asRefusal(err)

	// Assert.
	if got.Fields["text"] != "a merge is already queued" {
		t.Fatalf("Fields[text] = %v, want the merge refusal's reason", got.Fields["text"])
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

// TestEndStreamIsQuietOnACancelledClient pins the standing-stream ending: a
// cancelled or expired request context is the CLIENT LEAVING and must record
// no ERROR and return no error, while every other failure keeps fail's ERROR
// record and its internal Connect error.
func TestEndStreamIsQuietOnACancelledClient(t *testing.T) {
	tests := []struct {
		name      string
		err       error
		wantQuiet bool
	}{
		{name: "cancelled", err: context.Canceled, wantQuiet: true},
		{name: "wrapped cancelled", err: fmt.Errorf("read: %w", context.Canceled), wantQuiet: true},
		{name: "deadline exceeded", err: context.DeadlineExceeded, wantQuiet: true},
		{name: "a genuine read failure", err: errors.New("the disk went away"), wantQuiet: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			log := &recordingLogger{}

			// Act.
			err := endStream(log, "WatchFooter", tc.err)

			// Assert.
			if tc.wantQuiet {
				if err != nil {
					t.Fatalf("endStream returned %v, want no error", err)
				}
				if got := log.at("ERROR"); len(got) != 0 {
					t.Fatalf("recorded %v at ERROR, want none", got)
				}
				info := log.at("INFO")
				if len(info) != 1 || info[0].Context["stream"] != "WatchFooter" {
					t.Fatalf("INFO records = %v, want exactly one naming the stream", info)
				}
				return
			}
			if err == nil {
				t.Fatal("endStream swallowed a genuine failure")
			}
			if got := log.at("ERROR"); len(got) != 1 {
				t.Fatalf("ERROR records = %v, want exactly one", got)
			}
		})
	}
}

// TestStreamVerbsEndQuietlyWhenTheClientsContextIsCancelled pins the ending on
// every verb family that opens a standing stream: one cancelled resolve, no
// ERROR record, no error returned.
func TestStreamVerbsEndQuietlyWhenTheClientsContextIsCancelled(t *testing.T) {
	tests := []string{
		"WatchFooter",
		"WatchTopbar",
		"WatchDaemonHolds",
		"WatchHostWorkspace",
		"WatchWebWorkspace",
		"WatchFeed",
		"WatchLoginTerminal",
	}
	for _, rpc := range tests {
		t.Run(rpc, func(t *testing.T) {
			// Arrange.
			log := &recordingLogger{}

			// Act.
			err := endStream(log, rpc, fmt.Errorf("%s: workspace %q: %w",
				rpc, "ws-1", context.Canceled))

			// Assert.
			if err != nil {
				t.Fatalf("%s returned %v, want no error", rpc, err)
			}
			if got := log.at("ERROR"); len(got) != 0 {
				t.Fatalf("%s recorded %v at ERROR, want none", rpc, got)
			}
			info := log.at("INFO")
			if len(info) != 1 || info[0].Context["stream"] != rpc {
				t.Fatalf("%s INFO records = %v, want exactly one naming the stream", rpc, info)
			}
		})
	}
}

// TestResolveRefDecidesTheStandingBeforeReadingState pins the ORDERING the
// contract owes: the serving standing is answered before any state is touched,
// so the outgoing daemon of a handover — whose state client is already closed
// when the late rpc lands — still answers the typed arm rather than the state
// client's failure.
func TestResolveRefDecidesTheStandingBeforeReadingState(t *testing.T) {
	closedDB := errors.New("sql: database is closed")
	tests := []struct {
		name     string
		standing workspace.Standing
		want     func(*agentreplv1.SubmitPromptError) bool
	}{
		{
			name:     "a transferred workspace answers transferring_away",
			standing: workspace.StandingTransferringAway,
			want: func(e *agentreplv1.SubmitPromptError) bool {
				return e.GetTransferringAway().GetAddress() == "127.0.0.1:9999"
			},
		},
		{
			name:     "an unadopted workspace answers not_yet_adopted",
			standing: workspace.StandingNotYetAdopted,
			want: func(e *agentreplv1.SubmitPromptError) bool {
				return e.GetNotYetAdopted() != nil
			},
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Ownership.standing = test.standing
			h.DB.workspaceErr = closedDB

			// Act.
			resp, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(submitRequest()))

			// Assert.
			if err != nil {
				t.Fatalf("SubmitPrompt answered an error rather than a typed arm: %v", err)
			}
			if !test.want(resp.Msg.GetError()) {
				t.Fatalf("SubmitPrompt answered %v, want the standing's own arm", resp.Msg.GetError())
			}
		})
	}
}

// TestResolveRefFailsAnOwnedWorkspaceOnAClosedState pins that the reordering
// does NOT swallow a genuine state failure: a workspace this daemon still
// serves surfaces the read's error.
func TestResolveRefFailsAnOwnedWorkspaceOnAClosedState(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Ownership.standing = workspace.StandingOwned
	h.DB.workspaceErr = errors.New("sql: database is closed")

	// Act.
	_, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(submitRequest()))

	// Assert.
	if err == nil || connect.CodeOf(err) != connect.CodeInternal {
		t.Fatalf("SubmitPrompt = %v, want an internal error for an owned workspace", err)
	}
}

// TestResolveRefRefusesOnceTheDaemonIsShuttingDown pins that an rpc arriving
// after the surface's lifetime ended — an h2c connection the client still holds
// through the shutdown grace — is answered UNAVAILABLE, never with whatever the
// state client being torn down underneath it returns.
func TestResolveRefRefusesOnceTheDaemonIsShuttingDown(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.DB.workspaceErr = errors.New("sql: database is closed")
	if err := h.Server.Close(); err != nil {
		t.Fatalf("close the surface: %v", err)
	}

	// Act.
	_, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(submitRequest()))

	// Assert.
	if err == nil || connect.CodeOf(err) != connect.CodeUnavailable {
		t.Fatalf("SubmitPrompt = %v, want CodeUnavailable while the daemon is shutting down", err)
	}
}

// TestSetArmFillsARepeatedStringField pins that an arm carrying a LIST states
// its evidence. A repeated string's Kind() is StringKind, so before the list
// branch existed the copy tried a `value.(string)` on a slice, failed it, and
// left ForgetWorkspaceHasChildren.children empty beside a sentence saying the
// workspace has children.
func TestSetArmFillsARepeatedStringField(t *testing.T) {
	// Arrange.
	resp := &agentreplv1.ForgetWorkspaceResponse{}

	// Act.
	ok := setResponseError(resp, workspace.ArmHasChildren,
		map[string]any{"children": []string{"ws-2", "ws-3"}})

	// Assert.
	got := resp.GetError().GetHasChildren().GetChildren()
	if !ok || len(got) != 2 || got[0] != "ws-2" || got[1] != "ws-3" {
		t.Fatalf("setResponseError = %v, children = %v, want the two spawned ids", ok, got)
	}
}

// TestSetArmLeavesARepeatedFieldUnsetForAWrongValueType pins that a value the
// list branch cannot convert leaves the field UNSET rather than writing a
// wrong one; the refusal's prose still states the evidence.
func TestSetArmLeavesARepeatedFieldUnsetForAWrongValueType(t *testing.T) {
	// Arrange.
	resp := &agentreplv1.ForgetWorkspaceResponse{}

	// Act.
	ok := setResponseError(resp, workspace.ArmHasChildren, map[string]any{"children": "ws-2"})

	// Assert: the ARM is still answered — only its evidence is missing.
	if !ok || len(resp.GetError().GetHasChildren().GetChildren()) != 0 {
		t.Fatalf("setResponseError = %v, children = %v, want the arm answered with no children",
			ok, resp.GetError().GetHasChildren().GetChildren())
	}
}

// TestSetResponseErrorFillsTheCreateSpawnFailedDetail pins the arm that made a
// create answer out of band while an open answered in band: ONE daemon refusal
// (workspace.ArmSpawnFailed) is raised for both rpcs, and `fill` already
// supplies `detail`, so the handler switches onto the arm by its existing.
func TestSetResponseErrorFillsTheCreateSpawnFailedDetail(t *testing.T) {
	// Arrange.
	resp := &agentreplv1.CreateWorkspaceResponse{}

	// Act.
	ok := setResponseError(resp, workspace.ArmSpawnFailed,
		map[string]any{"detail": "the shim would not come up"})

	// Assert.
	if !ok || resp.GetError().GetSpawnFailed().GetDetail() != "the shim would not come up" {
		t.Fatalf("setResponseError = %v, resp = %v, want spawn_failed carrying the spawn's account", ok, resp)
	}
}

// TestAResolutionReadInFlightHoldsCloseOff is the forced interleaving the
// webapp layer's roster area lost at random: a ClientLog's registry read is in
// flight when the daemon's exit closes the surface. Close must not be able to
// complete — and so the state client must not be closed — until that read has.
func TestAResolutionReadInFlightHoldsCloseOff(t *testing.T) {
	// Arrange: the read parks inside the registry.
	h := newHarness(t)
	h.DB.workspaceEntered = make(chan struct{})
	h.DB.workspaceRelease = make(chan struct{})
	entered := h.DB.workspaceEntered
	answered := make(chan error, 1)
	go func() { answered <- clientLogOnce(h) }()
	<-entered

	// Act: the exit tries to take the gate Close takes.
	gate := &h.Server.(*server).registry
	closable := gate.TryLock()
	if closable {
		gate.Unlock()
	}
	close(h.DB.workspaceRelease)

	// Assert.
	if closable {
		t.Fatal("Close could have completed while a resolution read was in flight; the state client would close beneath it")
	}
	if err := <-answered; err != nil {
		t.Fatalf("the in-flight ClientLog = %v, want it persisted", err)
	}
}

// TestReadRegistryAfterCloseMakesNoRead: once Close has begun, no read reaches
// a state client the exit may already have closed.
func TestReadRegistryAfterCloseMakesNoRead(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	s := h.Server.(*server)
	if err := s.Close(); err != nil {
		t.Fatalf("close the surface: %v", err)
	}
	read := false

	// Act.
	ended, err := s.readRegistry(func() error { read = true; return nil })

	// Assert.
	if !ended || err != nil || read {
		t.Fatalf("readRegistry after Close = (ended %v, err %v, read %v), want ended with no read", ended, err, read)
	}
}

// TestFailResolutionRecordsNoErrorForTheShutdownRefusal: the refusal was
// already stated at INFO where it was decided.
func TestFailResolutionRecordsNoErrorForTheShutdownRefusal(t *testing.T) {
	// Arrange.
	log := &recordingLogger{}
	refusal := connect.NewError(connect.CodeUnavailable, fmt.Errorf("ClientLog: %w", errServingEnded))

	// Act.
	got := failResolution(log, "ClientLog", refusal)

	// Assert.
	if got.Code() != connect.CodeUnavailable {
		t.Fatalf("failResolution code = %v, want unavailable", got.Code())
	}
	if len(log.at("ERROR")) != 0 {
		t.Fatalf("the shutdown refusal was recorded at ERROR: %+v", log.at("ERROR"))
	}
}

// TestFailResolutionRecordsNoErrorForAnEndedRequest: the caller leaving is not
// a failure of the daemon's.
func TestFailResolutionRecordsNoErrorForAnEndedRequest(t *testing.T) {
	// Arrange.
	log := &recordingLogger{}

	// Act.
	got := failResolution(log, "ClientLog", fmt.Errorf("ClientLog: %w", context.Canceled))

	// Assert.
	if got.Code() != connect.CodeCanceled {
		t.Fatalf("failResolution code = %v, want canceled", got.Code())
	}
	if len(log.at("ERROR")) != 0 {
		t.Fatalf("an ended request was recorded at ERROR: %+v", log.at("ERROR"))
	}
}

// TestFailResolutionRecordsAGenuineFailureAtError: everything else still fails
// loudly.
func TestFailResolutionRecordsAGenuineFailureAtError(t *testing.T) {
	// Arrange.
	log := &recordingLogger{}

	// Act.
	got := failResolution(log, "ClientLog", errors.New("sql: database is closed"))

	// Assert.
	if got.Code() != connect.CodeInternal {
		t.Fatalf("failResolution code = %v, want internal", got.Code())
	}
	if len(log.at("ERROR")) != 1 {
		t.Fatalf("a genuine failure was recorded %d time(s) at ERROR, want once", len(log.at("ERROR")))
	}
}

// TestRegistryWorkspaceAnswersTheRecord pins the shared read's ordinary answer.
func TestRegistryWorkspaceAnswersTheRecord(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	s := h.Server.(*server)

	// Act.
	record, ended, err := s.registryWorkspace(context.Background(), testWorkspaceID)

	// Assert.
	if ended || err != nil || record.Dir != testWorkspaceDir {
		t.Fatalf("registryWorkspace = (%+v, ended %v, %v), want the registered record", record, ended, err)
	}
}

// TestEveryResolutionReadGoesThroughRegistryWorkspace fails a resolution that
// reads the registry on its own, outside the gate Close takes.
func TestEveryResolutionReadGoesThroughRegistryWorkspace(t *testing.T) {
	cases := []struct{ file, function string }{
		{"refuse.go", "resolveRegistered"},
		{"requestlog.go", "beginRequest"},
	}
	for _, tc := range cases {
		t.Run(tc.function, func(t *testing.T) {
			// Arrange.
			fset := token.NewFileSet()
			parsed, err := parser.ParseFile(fset, tc.file, nil, 0)
			if err != nil {
				t.Fatalf("parse %s: %v", tc.file, err)
			}
			var body string
			for _, decl := range parsed.Decls {
				if fn, ok := decl.(*ast.FuncDecl); ok && fn.Name.Name == tc.function {
					raw, readErr := os.ReadFile(tc.file)
					if readErr != nil {
						t.Fatalf("read %s: %v", tc.file, readErr)
					}
					body = string(raw[fset.Position(fn.Body.Pos()).Offset:fset.Position(fn.Body.End()).Offset])
				}
			}
			if body == "" {
				t.Fatalf("%s declares no %s", tc.file, tc.function)
			}

			// Act.
			direct := strings.Contains(body, "s.deps.DB.Workspace(")
			shared := strings.Contains(body, "s.registryWorkspace(")

			// Assert.
			if direct || !shared {
				t.Fatalf("%s: direct registry read %v, shared helper %v; its registry read must go through registryWorkspace", tc.function, direct, shared)
			}
		})
	}
}

func TestRefuseOntoSetsTheTypedRefusalTheRPCWouldAnswer(t *testing.T) {
	tests := []struct {
		name    string
		err     error
		wantSet bool
	}{
		{name: "a typed refusal the response declares", err: &workspace.Refusal{Arm: workspace.ArmGitFailed, Reason: "locked"}, wantSet: true},
		{name: "a typed refusal the response does not declare", err: &workspace.Refusal{Arm: "no_such_arm", Reason: "x"}, wantSet: false},
		{name: "an error that is no refusal", err: errors.New("forget failed"), wantSet: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			s := h.Server.(*server)
			resp := &agentreplv1.NukeWorkspaceResponse{}

			// Act.
			got := s.refuseOnto(s.log, "NukeWorkspace", resp, tt.err)

			// Assert.
			if got != tt.wantSet {
				t.Fatalf("refuseOnto = %v, want %v", got, tt.wantSet)
			}
			if tt.wantSet && resp.GetError().GetGitFailed() == nil {
				t.Fatalf("response = %v, want git_failed set", resp.GetResult())
			}
			if !tt.wantSet && resp.GetError() != nil {
				t.Fatalf("response = %v, want nothing set", resp.GetResult())
			}
		})
	}
}

// TestDetachedFailuresMapTheirRefusalOnlyThroughRefuseOnto pins that every
// detached mutation maps its typed refusal through the one helper, so no site
// hand-rolls `asRefusal` then `refuse(...) == nil` and drifts from the others.
func TestDetachedFailuresMapTheirRefusalOnlyThroughRefuseOnto(t *testing.T) {
	// Arrange.
	entries, err := os.ReadDir(".")
	if err != nil {
		t.Fatalf("read the package: %v", err)
	}

	// Act.
	var offenders []string
	for _, entry := range entries {
		name := entry.Name()
		if !strings.HasSuffix(name, ".go") || strings.HasSuffix(name, "_test.go") {
			continue
		}
		src, err := os.ReadFile(name)
		if err != nil {
			t.Fatalf("read %s: %v", name, err)
		}
		for _, line := range strings.Split(string(src), "\n") {
			if strings.Contains(line, "s.refuse(") && strings.Contains(line, "== nil") && !strings.Contains(line, "return s.refuse(") {
				offenders = append(offenders, name+": "+strings.TrimSpace(line))
			}
		}
	}

	// Assert.
	if len(offenders) > 0 {
		t.Fatalf("hand-rolled typed-refusal mapping, use refuseOnto: %v", offenders)
	}
}
