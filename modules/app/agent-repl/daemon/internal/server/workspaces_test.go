package server

import (
	"context"
	"errors"
	"fmt"
	"reflect"
	"slices"
	"strings"
	"testing"
	"time"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/lifecycleconfig"
	"agent-repld/internal/merge"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/refusalarm"
	"agent-repld/internal/semver"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
)

// TestUnknownWorkspaceIsRefusedByArm pins that a ref naming an id the registry
// does not hold answers the unknown_workspace arm.
func TestUnknownWorkspaceIsRefusedByArm(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.SelectWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{
			Workspace: &workspacev1.WorkspaceRef{Id: "ws-nope"},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("SelectWorkspace: %v", err)
	}
	if resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("result = %v, want unknown_workspace", resp.Msg.GetResult())
	}
}

// TestWorkspaceRefMismatchCarriesTheRegistryDir pins the ruling: the daemon keys
// on `id` and REFUSES a ref whose dir disagrees, telling the caller what the
// registry actually holds.
func TestWorkspaceRefMismatchCarriesTheRegistryDir(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.SelectWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{
			Workspace: &workspacev1.WorkspaceRef{Id: string(testWorkspaceID), Dir: "/elsewhere"},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("SelectWorkspace: %v", err)
	}
	if got := resp.Msg.GetError().GetWorkspaceRefMismatch().GetRegistryDir(); got != testWorkspaceDir {
		t.Fatalf("registry_dir = %q, want %q", got, testWorkspaceDir)
	}
}

// TestTransferringAwayCarriesTheSuccessorAddress pins the OLD daemon's refusal
// after the transfer notice: the caller is told where to go.
func TestTransferringAwayCarriesTheSuccessorAddress(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Ownership.standing = workspace.StandingTransferringAway

	// Act.
	resp, err := h.Client.SelectWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("SelectWorkspace: %v", err)
	}
	if got := resp.Msg.GetError().GetTransferringAway().GetAddress(); got != "127.0.0.1:9999" {
		t.Fatalf("address = %q, want the successor's", got)
	}
}

// TestNotYetAdoptedIsRefusedOnAJoiningDaemon pins the JOINING daemon's refusal
// before adoption.
func TestNotYetAdoptedIsRefusedOnAJoiningDaemon(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Ownership.standing = workspace.StandingNotYetAdopted

	// Act.
	resp, err := h.Client.SelectWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("SelectWorkspace: %v", err)
	}
	if resp.Msg.GetError().GetNotYetAdopted() == nil {
		t.Fatalf("result = %v, want not_yet_adopted", resp.Msg.GetResult())
	}
}

// TestNotYetAdoptedNamesTheServingDaemon pins where the caller views the
// workspace meanwhile: the arm names the incumbent that still serves it, or
// names none when no other daemon serves it.
func TestNotYetAdoptedNamesTheServingDaemon(t *testing.T) {
	cases := []struct {
		name      string
		incumbent string
		wantSet   bool
	}{
		{name: "a live incumbent is named", incumbent: "127.0.0.1:8888", wantSet: true},
		{name: "no live incumbent leaves the address unset", incumbent: "", wantSet: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Ownership.standing = workspace.StandingNotYetAdopted
			h.IncumbentAddress = tc.incumbent

			// Act.
			resp, err := h.Client.SelectWorkspace(context.Background(),
				connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{Workspace: ref()}))

			// Assert.
			if err != nil {
				t.Fatalf("SelectWorkspace: %v", err)
			}
			arm := resp.Msg.GetError().GetNotYetAdopted()
			if arm == nil {
				t.Fatalf("result = %v, want not_yet_adopted", resp.Msg.GetResult())
			}
			if arm.Address != nil != tc.wantSet {
				t.Fatalf("address set = %v, want %v", arm.Address != nil, tc.wantSet)
			}
			if tc.wantSet && arm.GetAddress() != tc.incumbent {
				t.Fatalf("address = %q, want the incumbent's %q", arm.GetAddress(), tc.incumbent)
			}
		})
	}
}

// TestOwnedWorkspaceIsServed pins that a served workspace reaches the verb.
func TestOwnedWorkspaceIsServed(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.SelectWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("SelectWorkspace: %v", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("result = %v, want success", resp.Msg.GetResult())
	}
}

// TestCloseWorkspaceMapsTheBlockedArm pins that the verbs' close refusal reaches
// the caller as CloseWorkspaceError.blocked.
func TestCloseWorkspaceMapsTheBlockedArm(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.closeErr = &workspace.Refusal{Arm: "blocked", Reason: "a turn is in flight"}

	// Act.
	resp, err := h.Client.CloseWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.CloseWorkspaceRequest{Workspace: ref(), Requester: userRequester()}))

	// Assert.
	if err != nil {
		t.Fatalf("CloseWorkspace: %v", err)
	}
	if resp.Msg.GetError().GetBlocked() == nil {
		t.Fatalf("result = %v, want blocked", resp.Msg.GetResult())
	}
}

// TestCloseWorkspaceAnswersAnUnresolvableRefInBand pins that a close naming
// no registered workspace, or naming one by a dir the registry disagrees
// with, is answered on CloseWorkspaceError's own arm and closes nothing.
func TestCloseWorkspaceAnswersAnUnresolvableRefInBand(t *testing.T) {
	tests := []struct {
		name string
		ref  *workspacev1.WorkspaceRef
		got  func(*agentreplv1.CloseWorkspaceError) bool
	}{
		{name: "an unknown workspace", ref: &workspacev1.WorkspaceRef{Id: "ws-nope", Dir: "/tmp/ws-nope"},
			got: func(e *agentreplv1.CloseWorkspaceError) bool { return e.GetUnknownWorkspace() != nil }},
		{name: "a ref whose dir disagrees", ref: &workspacev1.WorkspaceRef{Id: string(testWorkspaceID), Dir: "/elsewhere"},
			got: func(e *agentreplv1.CloseWorkspaceError) bool {
				return e.GetWorkspaceRefMismatch().GetRegistryDir() == testWorkspaceDir
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			resp, err := h.Client.CloseWorkspace(context.Background(),
				connect.NewRequest(&agentreplv1.CloseWorkspaceRequest{Workspace: tt.ref, Requester: userRequester()}))

			// Assert.
			if err != nil {
				t.Fatalf("CloseWorkspace = transport error %v, want the in-band arm", err)
			}
			if !tt.got(resp.Msg.GetError()) {
				t.Fatalf("result = %v, want the %s arm", resp.Msg.GetResult(), tt.name)
			}
			if len(h.Verbs.closedBy) != 0 {
				t.Fatalf("close causes = %v, want nothing closed", h.Verbs.closedBy)
			}
		})
	}
}

// TestCloseWorkspaceIsTheUsersClose pins that the rpc closes on the user's
// behalf, which the close record keeps.
func TestCloseWorkspaceIsTheUsersClose(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.CloseWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.CloseWorkspaceRequest{Workspace: ref(), Requester: userRequester()}))

	// Assert.
	if err != nil {
		t.Fatalf("CloseWorkspace: %v", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("result = %v, want success", resp.Msg.GetResult())
	}
	if len(h.Verbs.closedBy) != 1 || h.Verbs.closedBy[0] != wsm.CloseByUser {
		t.Fatalf("close causes = %v, want one by the user", h.Verbs.closedBy)
	}
}

// TestCreateWorkspaceRefusesAnUnknownRepository pins that a repository ref
// matching nothing registered is refused rather than materialized somewhere.
func TestCreateWorkspaceRefusesAnUnknownRepository(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.CreateWorkspace(context.Background(),
		connect.NewRequest(standardCreate(&workspacev1.RepositoryRef{Id: "repo-nope"}, "")))

	// Assert.
	if err != nil {
		t.Fatalf("CreateWorkspace: %v", err)
	}
	if resp.Msg.GetError().GetUnknownRepository() == nil {
		t.Fatalf("result = %v, want unknown_repository", resp.Msg.GetResult())
	}
}

// ---------------------------------------------------------------------------
// ForgetWorkspace. The verb, its three refusals and its command-file route all
// existed before the endpoint did; these pin that the wire now carries them.
// ---------------------------------------------------------------------------

// TestForgetWorkspaceAnswersSuccessWhenTheVerbForgets pins the ordinary answer:
// the record is gone and the caller is told so.
func TestForgetWorkspaceAnswersSuccessWhenTheVerbForgets(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.ForgetWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.ForgetWorkspaceRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("ForgetWorkspace: %v", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("result = %v, want success", resp.Msg.GetResult())
	}
}

// TestForgetWorkspaceMapsTheNotClosedArm pins the refusal that keeps the close
// verb the one owner of the quiet requirement.
func TestForgetWorkspaceMapsTheNotClosedArm(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.forgetErr = &workspace.Refusal{
		Rpc: "ForgetWorkspace", Arm: workspace.ArmNotClosed,
		Reason: `workspace "ws-1" is open; close it before forgetting it`,
	}

	// Act.
	resp, err := h.Client.ForgetWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.ForgetWorkspaceRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("ForgetWorkspace: %v", err)
	}
	if resp.Msg.GetError().GetNotClosed() == nil {
		t.Fatalf("result = %v, want not_closed", resp.Msg.GetResult())
	}
}

// TestForgetWorkspaceBlockedCarriesTheSameFiveFieldsACloseDoes pins that the
// re-run quiet check states its evidence, not only a sentence: a hold outlives
// a close when the close raced it, and the caller must be able to see it.
func TestForgetWorkspaceBlockedCarriesTheSameFiveFieldsACloseDoes(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.forgetErr = &workspace.Refusal{
		Rpc: "ForgetWorkspace", Arm: "blocked", Reason: "2 held prompts are undelivered",
		Fields: map[string]any{
			"turn_in_flight": true,
			"live_work":      uint32(3),
			"held_prompts":   uint32(2),
			"merge_queued":   true,
			"summary":        "2 held prompts are undelivered",
		},
	}

	// Act.
	resp, err := h.Client.ForgetWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.ForgetWorkspaceRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("ForgetWorkspace: %v", err)
	}
	blocked := resp.Msg.GetError().GetBlocked()
	if !blocked.GetTurnInFlight() || blocked.GetLiveWork() != 3 || blocked.GetHeldPrompts() != 2 ||
		!blocked.GetMergeQueued() || blocked.GetSummary() != "2 held prompts are undelivered" {
		t.Fatalf("blocked = %v, want all five fields as the composer stated them", blocked)
	}
}

// TestForgetWorkspaceHasChildrenNamesTheChildren pins the REPEATED arm field:
// the schema's parent_id is ON DELETE SET NULL, so the caller is owed the ids
// it must deal with first rather than a count in a sentence.
func TestForgetWorkspaceHasChildrenNamesTheChildren(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.forgetErr = &workspace.Refusal{
		Rpc: "ForgetWorkspace", Arm: workspace.ArmHasChildren,
		Reason: `2 workspaces were spawned from "ws-1"; forget them first`,
		Fields: map[string]any{"children": []string{"ws-2", "ws-3"}},
	}

	// Act.
	resp, err := h.Client.ForgetWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.ForgetWorkspaceRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("ForgetWorkspace: %v", err)
	}
	got := resp.Msg.GetError().GetHasChildren().GetChildren()
	if len(got) != 2 || got[0] != "ws-2" || got[1] != "ws-3" {
		t.Fatalf("children = %v, want the two spawned ids", got)
	}
}

// TestForgetWorkspaceAnswersTheMainWorktreeArm pins the main_worktree arm and
// its count: a repository's main worktree stands while other workspaces are
// registered under it.
func TestForgetWorkspaceAnswersTheMainWorktreeArm(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.forgetErr = &workspace.Refusal{
		Rpc: "ForgetWorkspace", Arm: workspace.ArmMainWorktree,
		Reason: `workspace "ws-1" is its repository's main worktree, and 2 other workspaces are registered under the repository`,
		Fields: map[string]any{"other_workspaces": uint32(2)},
	}

	// Act.
	resp, err := h.Client.ForgetWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.ForgetWorkspaceRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("ForgetWorkspace: %v", err)
	}
	if got := resp.Msg.GetError().GetMainWorktree(); got == nil || got.GetOtherWorkspaces() != 2 {
		t.Fatalf("error = %v, want main_worktree with other_workspaces 2", resp.Msg.GetError())
	}
}

// TestForgetWorkspaceRefusesAnUnknownWorkspace pins that the shared per-verb
// resolution refuses before the verb is reached, as it does for every other
// per-workspace rpc.
func TestForgetWorkspaceRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.ForgetWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.ForgetWorkspaceRequest{
			Workspace: &workspacev1.WorkspaceRef{Id: "ws-nope"},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("ForgetWorkspace: %v", err)
	}
	if resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("result = %v, want unknown_workspace", resp.Msg.GetResult())
	}
}

// ---------------------------------------------------------------------------
// CreateWorkspace option B: instant ack, background execution detached from the
// request context, and staged progress on the WatchDaemon channel.
// ---------------------------------------------------------------------------

// registeredRepoRef registers a repository in the fixture and returns a ref to
// it, so a create reaches the verb instead of being refused on unknown_repository.
func registeredRepoRef(h *harness) *workspacev1.RepositoryRef {
	repo := wsm.Repository{
		ID: wsm.RepoID("repo-1"), Dir: "/tmp/agent-repl-test-repo",
		Name: "repo", DefaultBranch: "master",
	}
	h.DB.repositories = append(h.DB.repositories, repo)
	return &workspacev1.RepositoryRef{Id: string(repo.ID)}
}

// standardCreate builds a standard create request for repoRef, with opID set
// when non-empty.
func standardCreate(repoRef *workspacev1.RepositoryRef, opID string) *agentreplv1.CreateWorkspaceRequest {
	req := &agentreplv1.CreateWorkspaceRequest{
		Repository: repoRef,
		Form: &agentreplv1.CreateWorkspaceRequest_Standard{
			Standard: &agentreplv1.CreateWorkspaceStandard{Relationship: peerRelationship()},
		},
		Requester:     userRequester(),
		ContextOrigin: freshContext(),
		Originator:    userOrWorkspaceOriginator(),
	}
	if opID != "" {
		req.OpId = &opID
	}
	return req
}

// proveDaemonSubscription opens a WatchDaemon stream and proves it is live by
// pushing a drain schedule and receiving it, so a later progress event cannot
// be lost to a subscription that had not finished attaching.
func proveDaemonSubscription(t *testing.T, h *harness) *connect.ServerStreamForClient[agentreplv1.WatchDaemonResponse] {
	t.Helper()
	ctx, cancel := context.WithCancel(context.Background())
	t.Cleanup(cancel)
	stream := dialEmacsDaemon(t, h, ctx, emacsDaemonRequest)
	h.Server.DrainScheduled(&agentreplv1.DaemonDrainScheduled{AtMs: 1})
	if !stream.Receive() {
		t.Fatalf("prove the subscription: %v", stream.Err())
	}
	return stream
}

// TestCreateWorkspaceAcksImmediatelyWhenGivenAnOpId pins that a create carrying
// an op_id is answered with the accepted arm the instant it is accepted, before
// its work has finished.
func TestCreateWorkspaceAcksImmediatelyWhenGivenAnOpId(t *testing.T) {
	// Arrange: a create whose work is held open, so an ack that waited for it
	// could never return.
	h := newHarness(t)
	repoRef := registeredRepoRef(h)
	h.Verbs.createRec = wsm.Workspace{ID: "ws-created", Dir: "/tmp/ws", Name: "minted"}
	h.Verbs.createRelease = make(chan struct{})

	// Act.
	resp, err := h.Client.CreateWorkspace(context.Background(),
		connect.NewRequest(standardCreate(repoRef, "op-ack")))

	// Assert.
	if err != nil {
		t.Fatalf("CreateWorkspace: %v", err)
	}
	if got := resp.Msg.GetAccepted().GetOpId(); got != "op-ack" {
		t.Fatalf("accepted op_id = %q, want %q (result=%v)", got, "op-ack", resp.Msg.GetResult())
	}
	close(h.Verbs.createRelease)
}

// TestCreateWorkspaceStaysSynchronousWithoutAnOpId pins the backward-compatible
// form: a create with no op_id blocks and answers success itself, unchanged.
func TestCreateWorkspaceStaysSynchronousWithoutAnOpId(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	repoRef := registeredRepoRef(h)
	h.Verbs.createRec = wsm.Workspace{ID: "ws-created", Dir: "/tmp/ws", Name: "minted"}

	// Act.
	resp, err := h.Client.CreateWorkspace(context.Background(),
		connect.NewRequest(standardCreate(repoRef, "")))

	// Assert.
	if err != nil {
		t.Fatalf("CreateWorkspace: %v", err)
	}
	if got := resp.Msg.GetSuccess().GetWorkspace().GetId(); got != "ws-created" {
		t.Fatalf("result = %v, want success ws-created", resp.Msg.GetResult())
	}
}

func TestACreatesSuccessSaysWhenItsPromptIsHeldAtTheColdGate(t *testing.T) {
	cases := []struct {
		name       string
		cold       *conversationv1.SessionCold
		wantHeld   bool
		wantTokens uint64
	}{
		{name: "a session parked at its cold gate holds the prompt there", cold: &conversationv1.SessionCold{ContextTokens: 194149}, wantHeld: true, wantTokens: 194149},
		{name: "a session at no gate names no hold"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			repoRef := registeredRepoRef(h)
			h.Verbs.createRec = wsm.Workspace{ID: "ws-created", Dir: "/tmp/ws", Name: "minted"}
			if tc.cold != nil {
				h.ColdGates = map[ids.WorkspaceID]*conversationv1.SessionCold{"ws-created": tc.cold}
			}

			// Act.
			resp, err := h.Client.CreateWorkspace(context.Background(),
				connect.NewRequest(standardCreate(repoRef, "")))

			// Assert.
			if err != nil {
				t.Fatalf("CreateWorkspace: %v", err)
			}
			held := resp.Msg.GetSuccess().GetHeldAtColdGate()
			if resp.Msg.GetSuccess().GetWorkspace().GetId() != "ws-created" || (held != nil) != tc.wantHeld || held.GetContextTokens() != tc.wantTokens {
				t.Fatalf("result = %v, want success held %v at %d tokens", resp.Msg.GetResult(), tc.wantHeld, tc.wantTokens)
			}
		})
	}
}

// TestCreateWorkspaceRunsToCompletionUnderACancelledRequestContext is the
// cancellation fix: the accepting request is cancelled WHILE the create is
// mid-flight, and the create must still run to completion — a succeeded event,
// never a failure — because its work runs on a context detached from the
// request. Called through the server directly so the request context is the
// test's to cancel.
func TestCreateWorkspaceRunsToCompletionUnderACancelledRequestContext(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	s := h.Server.(*server)
	repoRef := registeredRepoRef(h)
	h.Verbs.createRec = wsm.Workspace{ID: "ws-created", Dir: "/tmp/ws", Name: "minted"}
	h.Verbs.createEntered = make(chan struct{})
	h.Verbs.createRelease = make(chan struct{})
	stream := proveDaemonSubscription(t, h)

	// Act: accept the create under a cancellable request context, cancel that
	// context while the create is blocked mid-flight, then let it proceed.
	reqCtx, reqCancel := context.WithCancel(context.Background())
	defer reqCancel()
	resp, err := s.CreateWorkspace(reqCtx, connect.NewRequest(standardCreate(repoRef, "op-cancel")))
	if err != nil {
		t.Fatalf("CreateWorkspace: %v", err)
	}
	if resp.Msg.GetAccepted() == nil {
		t.Fatalf("result = %v, want accepted", resp.Msg.GetResult())
	}
	<-h.Verbs.createEntered
	reqCancel()
	close(h.Verbs.createRelease)

	// Assert: the terminal outcome is a success, proving the work was not
	// cancelled with the request.
	if !stream.Receive() {
		t.Fatalf("receive the terminal outcome: %v", stream.Err())
	}
	prog := stream.Msg().GetMutationProgress()
	if prog.GetOpId() != "op-cancel" {
		t.Fatalf("op_id = %q, want op-cancel", prog.GetOpId())
	}
	if prog.GetCreate().GetSucceeded() == nil {
		t.Fatalf("terminal step = %v, want succeeded (the create must survive the cancellation)", prog.GetCreate().GetStep())
	}
	if got := prog.GetCreate().GetSucceeded().GetName(); got != "minted" {
		t.Fatalf("succeeded name = %q, want minted", got)
	}
}

// TestCreateWorkspaceRelaysProgressStagesInOrder pins that the stages the verb
// reports reach the client on the WatchDaemon channel, in order, keyed on the
// op_id, followed by the terminal succeeded step.
func TestCreateWorkspaceRelaysProgressStagesInOrder(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	repoRef := registeredRepoRef(h)
	h.Verbs.createRec = wsm.Workspace{ID: "ws-created", Dir: "/tmp/ws", Name: "minted"}
	h.Verbs.createStages = []workspace.CreateStage{
		workspace.CreateStageDerivingName,
		workspace.CreateStageCreatingWorktree,
		workspace.CreateStageStartingSession,
	}
	stream := proveDaemonSubscription(t, h)

	// Act.
	resp, err := h.Client.CreateWorkspace(context.Background(),
		connect.NewRequest(standardCreate(repoRef, "op-order")))
	if err != nil {
		t.Fatalf("CreateWorkspace: %v", err)
	}
	if resp.Msg.GetAccepted() == nil {
		t.Fatalf("result = %v, want accepted", resp.Msg.GetResult())
	}

	// Assert: the three stages arrive in order, then the terminal success.
	wantStages := []string{"deriving_name", "creating_worktree", "starting_session"}
	for i, want := range wantStages {
		if !stream.Receive() {
			t.Fatalf("receive stage %d: %v", i, stream.Err())
		}
		prog := stream.Msg().GetMutationProgress()
		if prog.GetOpId() != "op-order" {
			t.Fatalf("stage %d op_id = %q, want op-order", i, prog.GetOpId())
		}
		if got := createStageArm(prog.GetCreate().GetEnteredStage()); got != want {
			t.Fatalf("stage %d = %q, want %q", i, got, want)
		}
	}
	if !stream.Receive() {
		t.Fatalf("receive the terminal outcome: %v", stream.Err())
	}
	if stream.Msg().GetMutationProgress().GetCreate().GetSucceeded() == nil {
		t.Fatalf("terminal step = %v, want succeeded",
			stream.Msg().GetMutationProgress().GetCreate().GetStep())
	}
}

// createStageArm names the arm set on an entered_stage, so a test compares the
// stage a push carried by name. An unset or unknown arm names itself as such.
func createStageArm(stage *agentreplv1.WorkspaceCreateStage) string {
	switch stage.GetStage().(type) {
	case *agentreplv1.WorkspaceCreateStage_DerivingName:
		return "deriving_name"
	case *agentreplv1.WorkspaceCreateStage_CreatingWorktree:
		return "creating_worktree"
	case *agentreplv1.WorkspaceCreateStage_StartingSession:
		return "starting_session"
	case nil:
		return "<unset>"
	default:
		return "<unknown>"
	}
}

// TestCreateProgressReporterMapsEachStageToItsArm pins that every stage the
// verb reports reaches the wire on entered_stage with its own oneof arm set.
func TestCreateProgressReporterMapsEachStageToItsArm(t *testing.T) {
	cases := []struct {
		name  string
		stage workspace.CreateStage
		want  string
	}{
		{"deriving name", workspace.CreateStageDerivingName, "deriving_name"},
		{"creating worktree", workspace.CreateStageCreatingWorktree, "creating_worktree"},
		{"starting session", workspace.CreateStageStartingSession, "starting_session"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			stream := proveDaemonSubscription(t, h)
			reporter := createProgressReporter{server: h.Server.(*server), opID: "op-arm"}

			// Act.
			reporter.Stage(tc.stage)

			// Assert.
			if !stream.Receive() {
				t.Fatalf("receive the stage: %v", stream.Err())
			}
			prog := stream.Msg().GetMutationProgress()
			if prog.GetOpId() != "op-arm" {
				t.Fatalf("op_id = %q, want op-arm", prog.GetOpId())
			}
			if got := createStageArm(prog.GetCreate().GetEnteredStage()); got != tc.want {
				t.Fatalf("arm = %q, want %q", got, tc.want)
			}
		})
	}
}

// TestCreateWorkspaceEndsInFailedWhenTheSessionStageFails pins that a create
// failing during its session bring-up, after it entered starting_session,
// still reaches the client as the terminal failed step.
func TestCreateWorkspaceEndsInFailedWhenTheSessionStageFails(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	repoRef := registeredRepoRef(h)
	h.Verbs.createStages = []workspace.CreateStage{
		workspace.CreateStageCreatingWorktree,
		workspace.CreateStageStartingSession,
	}
	h.Verbs.createErr = errors.New("start the session: the shim never answered")
	stream := proveDaemonSubscription(t, h)

	// Act.
	resp, err := h.Client.CreateWorkspace(context.Background(),
		connect.NewRequest(standardCreate(repoRef, "op-bringup")))
	if err != nil {
		t.Fatalf("CreateWorkspace: %v", err)
	}
	if resp.Msg.GetAccepted() == nil {
		t.Fatalf("result = %v, want accepted", resp.Msg.GetResult())
	}

	// Assert: both stages, then the terminal failure carrying the cause.
	for i, want := range []string{"creating_worktree", "starting_session"} {
		if !stream.Receive() {
			t.Fatalf("receive stage %d: %v", i, stream.Err())
		}
		if got := createStageArm(stream.Msg().GetMutationProgress().GetCreate().GetEnteredStage()); got != want {
			t.Fatalf("stage %d = %q, want %q", i, got, want)
		}
	}
	if !stream.Receive() {
		t.Fatalf("receive the terminal outcome: %v", stream.Err())
	}
	failed := stream.Msg().GetMutationProgress().GetCreate().GetFailed()
	if failed == nil {
		t.Fatalf("terminal step = %v, want failed",
			stream.Msg().GetMutationProgress().GetCreate().GetStep())
	}
	if got := failed.GetInternal(); !strings.Contains(got, "the shim never answered") {
		t.Fatalf("internal failure = %q, want it to carry the cause", got)
	}
}

// TestCreateWorkspaceSurfacesAnInternalFailureAsAFailureEvent pins that a
// background create that fails with a non-refusal error (a git worktree add
// failure among them) is surfaced loudly on the progress channel, not swallowed.
func TestCreateWorkspaceSurfacesAnInternalFailureAsAFailureEvent(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	repoRef := registeredRepoRef(h)
	h.Verbs.createErr = errors.New("materialize worktree: fatal: boom")
	stream := proveDaemonSubscription(t, h)

	// Act.
	resp, err := h.Client.CreateWorkspace(context.Background(),
		connect.NewRequest(standardCreate(repoRef, "op-fail")))
	if err != nil {
		t.Fatalf("CreateWorkspace: %v", err)
	}
	if resp.Msg.GetAccepted() == nil {
		t.Fatalf("result = %v, want accepted", resp.Msg.GetResult())
	}

	// Assert.
	if !stream.Receive() {
		t.Fatalf("receive the failure: %v", stream.Err())
	}
	failed := stream.Msg().GetMutationProgress().GetCreate().GetFailed()
	if failed == nil {
		t.Fatalf("terminal step = %v, want failed",
			stream.Msg().GetMutationProgress().GetCreate().GetStep())
	}
	if got := failed.GetInternal(); !strings.Contains(got, "boom") {
		t.Fatalf("internal failure = %q, want it to carry the cause", got)
	}
}

// TestCreateWorkspaceSurfacesARefusalOnTheFailureEvent pins that a background
// create refused by the verb (naming_failed here) rides the failure event as
// the SAME typed refusal the synchronous form answers, so a client words it
// identically.
func TestCreateWorkspaceSurfacesARefusalOnTheFailureEvent(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	repoRef := registeredRepoRef(h)
	h.Verbs.createErr = &workspace.Refusal{
		Rpc: "CreateWorkspace", Arm: workspace.ArmNamingFailed,
		Reason: "no name was supplied and the naming call could not mint one",
		Fields: map[string]any{
			"model": "haiku", "cause": "timeout", "attempts": uint32(2), "answer": "",
		},
	}
	stream := proveDaemonSubscription(t, h)

	// Act.
	resp, err := h.Client.CreateWorkspace(context.Background(),
		connect.NewRequest(standardCreate(repoRef, "op-refuse")))
	if err != nil {
		t.Fatalf("CreateWorkspace: %v", err)
	}
	if resp.Msg.GetAccepted() == nil {
		t.Fatalf("result = %v, want accepted", resp.Msg.GetResult())
	}

	// Assert.
	if !stream.Receive() {
		t.Fatalf("receive the failure: %v", stream.Err())
	}
	failed := stream.Msg().GetMutationProgress().GetCreate().GetFailed()
	if failed.GetRefusal().GetNamingFailed() == nil {
		t.Fatalf("failure cause = %v, want naming_failed refusal", failed.GetCause())
	}
	if got := failed.GetRefusal().GetNamingFailed().GetCause(); got != "timeout" {
		t.Fatalf("naming_failed cause = %q, want timeout", got)
	}
}

// TestOpenWorkspaceRelaysProgressStagesInOrder pins that an open carrying an
// op_id has its stages pushed onto WatchDaemon, in order, keyed on that id.
func TestOpenWorkspaceRelaysProgressStagesInOrder(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.openStages = []workspace.OpenStage{
		workspace.OpenStageCheckingWorktree,
		workspace.OpenStageRestoringWorktree,
		workspace.OpenStageClearingClosed,
		workspace.OpenStageCheckingBuild,
	}
	stream := proveDaemonSubscription(t, h)

	// Act.
	resp, err := h.Client.OpenWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: ref(), OpId: "op-open"}))
	if err != nil {
		t.Fatalf("OpenWorkspace: %v", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("result = %v, want success", resp.Msg.GetResult())
	}

	// Assert.
	wantStages := []string{"checking_worktree", "restoring_worktree", "clearing_closed", "checking_build"}
	for i, want := range wantStages {
		if !stream.Receive() {
			t.Fatalf("receive stage %d: %v", i, stream.Err())
		}
		prog := stream.Msg().GetMutationProgress()
		if prog.GetOpId() != "op-open" {
			t.Fatalf("stage %d op_id = %q, want op-open", i, prog.GetOpId())
		}
		if got := openStageArm(prog.GetOpen().GetEnteredStage()); got != want {
			t.Fatalf("stage %d = %q, want %q", i, got, want)
		}
	}
}

// openStageArm names the arm set on an open's entered_stage, so a test compares
// the stage a push carried by name. An unset or unknown arm names itself as
// such.
func openStageArm(stage *agentreplv1.WorkspaceOpenStage) string {
	switch stage.GetStage().(type) {
	case *agentreplv1.WorkspaceOpenStage_CheckingWorktree:
		return "checking_worktree"
	case *agentreplv1.WorkspaceOpenStage_StartingSession:
		return "starting_session"
	case *agentreplv1.WorkspaceOpenStage_ClearingClosed:
		return "clearing_closed"
	case *agentreplv1.WorkspaceOpenStage_CheckingBuild:
		return "checking_build"
	case *agentreplv1.WorkspaceOpenStage_RestoringWorktree:
		return "restoring_worktree"
	case nil:
		return "<unset>"
	default:
		return "<unknown>"
	}
}

// TestOpenWorkspaceArmsNoReporterWithoutAnOpID pins that an open that minted no
// correlation id gets exactly the behavior it always had: no reporter is armed,
// so nothing is pushed to a stream that could not correlate it anyway.
func TestOpenWorkspaceArmsNoReporterWithoutAnOpID(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	if _, err := h.Client.OpenWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: ref()})); err != nil {
		t.Fatalf("OpenWorkspace: %v", err)
	}

	// Assert.
	if h.Verbs.openProgress != nil {
		t.Fatalf("open progress reporter = %v, want none for an open with no op_id", h.Verbs.openProgress)
	}
}

// TestOpenWorkspaceRelaysNoUnmappedStage pins that a stage this server's switch
// does not know is refused loudly rather than relayed as UNSPECIFIED, which a
// client would have to guess at.
func TestOpenWorkspaceRelaysNoUnmappedStage(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.openStages = []workspace.OpenStage{workspace.OpenStage(9999)}
	stream := proveDaemonSubscription(t, h)

	// Act.
	if _, err := h.Client.OpenWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: ref(), OpId: "op-bad"})); err != nil {
		t.Fatalf("OpenWorkspace: %v", err)
	}
	// A second, MAPPED open proves the stream is live and carried nothing for
	// the unmapped stage — the only way to assert an absence on a stream.
	h.Verbs.openStages = []workspace.OpenStage{workspace.OpenStageCheckingWorktree}
	if _, err := h.Client.OpenWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: ref(), OpId: "op-good"})); err != nil {
		t.Fatalf("OpenWorkspace: %v", err)
	}

	// Assert: the FIRST thing on the stream is the second open's stage.
	if !stream.Receive() {
		t.Fatalf("receive: %v", stream.Err())
	}
	if got := stream.Msg().GetMutationProgress().GetOpId(); got != "op-good" {
		t.Fatalf("op_id = %q, want op-good (the unmapped stage must not have been relayed)", got)
	}
}

// ---- SetWorkspaceBacklogged ----------------------------------------------------

func hideArm() *agentreplv1.SetWorkspaceBackloggedRequest_Hide {
	return &agentreplv1.SetWorkspaceBackloggedRequest_Hide{Hide: &agentreplv1.SetWorkspaceBackloggedHide{}}
}

func showArm() *agentreplv1.SetWorkspaceBackloggedRequest_Show {
	return &agentreplv1.SetWorkspaceBackloggedRequest_Show{Show: &agentreplv1.SetWorkspaceBackloggedShow{}}
}

// TestSetWorkspaceBackloggedHandsThePlaceToTheVerb pins each visibility arm
// reaching the verb with the id the registry resolved.
func TestSetWorkspaceBackloggedHandsThePlaceToTheVerb(t *testing.T) {
	tests := []struct {
		name string
		req  *agentreplv1.SetWorkspaceBackloggedRequest
		want bool
	}{
		{"hide", &agentreplv1.SetWorkspaceBackloggedRequest{Workspace: ref(), Visibility: hideArm()}, true},
		{"show", &agentreplv1.SetWorkspaceBackloggedRequest{Workspace: ref(), Visibility: showArm()}, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			resp, err := h.Client.SetWorkspaceBacklogged(context.Background(),
				connect.NewRequest(tt.req))

			// Assert
			if err != nil {
				t.Fatalf("SetWorkspaceBacklogged: %v", err)
			}
			if resp.Msg.GetSuccess() == nil {
				t.Fatalf("result = %v, want success", resp.Msg.GetResult())
			}
			want := backloggedSet{ws: testWorkspaceID, backlogged: tt.want}
			if len(h.Verbs.backloggedSets) != 1 || h.Verbs.backloggedSets[0] != want {
				t.Fatalf("backlogged sets = %+v, want %+v", h.Verbs.backloggedSets, want)
			}
		})
	}
}

// TestSetWorkspaceBackloggedRefusesAnUnknownWorkspace pins that an id the
// registry does not hold answers the unknown_workspace arm and reaches no verb.
func TestSetWorkspaceBackloggedRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	resp, err := h.Client.SetWorkspaceBacklogged(context.Background(),
		connect.NewRequest(&agentreplv1.SetWorkspaceBackloggedRequest{
			Workspace: &workspacev1.WorkspaceRef{Id: "ws-nope"}, Visibility: hideArm(),
		}))

	// Assert
	if err != nil {
		t.Fatalf("SetWorkspaceBacklogged: %v", err)
	}
	if resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("result = %v, want unknown_workspace", resp.Msg.GetResult())
	}
	if len(h.Verbs.backloggedSets) != 0 {
		t.Fatalf("backlogged sets = %+v, want none for an unknown workspace", h.Verbs.backloggedSets)
	}
}

// TestSetWorkspaceBackloggedMapsTheVerbsRefusal pins a refusal the verb raises
// answering its own arm.
func TestSetWorkspaceBackloggedMapsTheVerbsRefusal(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Verbs.setBackloggedErr = &workspace.Refusal{Arm: workspace.ArmNotYetAdopted, Reason: "still adopting"}

	// Act
	resp, err := h.Client.SetWorkspaceBacklogged(context.Background(),
		connect.NewRequest(&agentreplv1.SetWorkspaceBackloggedRequest{Workspace: ref(), Visibility: hideArm()}))

	// Assert
	if err != nil {
		t.Fatalf("SetWorkspaceBacklogged: %v", err)
	}
	if resp.Msg.GetError().GetNotYetAdopted() == nil {
		t.Fatalf("result = %v, want not_yet_adopted", resp.Msg.GetResult())
	}
}

// ---- ChangeWorkspaceLinks --------------------------------------------------

// TestChangeWorkspaceLinksHandsTheChangeToTheVerb pins each change arm reaching
// its verb, and the success arm answering the links the verb answered.
func TestChangeWorkspaceLinksHandsTheChangeToTheVerb(t *testing.T) {
	tests := []struct {
		name string
		req  *agentreplv1.ChangeWorkspaceLinksRequest
		want string
	}{
		{"set", &agentreplv1.ChangeWorkspaceLinksRequest{Workspace: ref(), Change: &agentreplv1.ChangeWorkspaceLinksRequest_Set{Set: &agentreplv1.ChangeWorkspaceLinksSet{Url: "https://example.com/pr/1", Label: "The pull request", Ownership: workspace.LinkOwnershipView(true)}}},
			"set https://example.com/pr/1 The pull request owned"},
		{"set of an announcement", &agentreplv1.ChangeWorkspaceLinksRequest{Workspace: ref(), Change: &agentreplv1.ChangeWorkspaceLinksRequest_Set{Set: &agentreplv1.ChangeWorkspaceLinksSet{Url: "https://w.slack.com/archives/C1/p1", Label: "The pull request's announcement",
			Ownership: &agentreplv1.WorkspaceLinkOwnership{Ownership: &agentreplv1.WorkspaceLinkOwnership_Owned{Owned: &agentreplv1.WorkspaceLinkOwned{Announces: &agentreplv1.WorkspaceLinkAnnouncement{Url: "https://example.com/pr/1"}}}}}}},
			"set https://w.slack.com/archives/C1/p1 The pull request's announcement owned announcing https://example.com/pr/1"},
		{"set of a reference", &agentreplv1.ChangeWorkspaceLinksRequest{Workspace: ref(), Change: &agentreplv1.ChangeWorkspaceLinksRequest_Set{Set: &agentreplv1.ChangeWorkspaceLinksSet{Url: "https://example.com/t/1", Label: "The tracking ticket", Ownership: workspace.LinkOwnershipView(false)}}},
			"set https://example.com/t/1 The tracking ticket reference"},
		{"remove", &agentreplv1.ChangeWorkspaceLinksRequest{Workspace: ref(), Change: &agentreplv1.ChangeWorkspaceLinksRequest_Remove{Remove: &agentreplv1.ChangeWorkspaceLinksRemove{Url: "https://example.com/pr/1"}}},
			"remove https://example.com/pr/1"},
		{"show", &agentreplv1.ChangeWorkspaceLinksRequest{Workspace: ref(), Change: &agentreplv1.ChangeWorkspaceLinksRequest_Show{Show: &agentreplv1.ChangeWorkspaceLinksShow{Urls: []string{"https://a", "https://b"}}}},
			"show https://a,https://b"},
		{"clear", &agentreplv1.ChangeWorkspaceLinksRequest{Workspace: ref(), Change: &agentreplv1.ChangeWorkspaceLinksRequest_Clear{Clear: &agentreplv1.ChangeWorkspaceLinksClear{}}}, "clear"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.Verbs.links = []wsm.WorkspaceLink{{URL: "https://example.com/pr/1", Label: "The pull request", Shown: true}}

			// Act
			resp, err := h.Client.ChangeWorkspaceLinks(context.Background(),
				connect.NewRequest(tt.req))

			// Assert
			if err != nil {
				t.Fatalf("ChangeWorkspaceLinks: %v", err)
			}
			if !proto.Equal(resp.Msg.GetSuccess().GetLinks(), workspace.LinksView(h.Verbs.links)) {
				t.Fatalf("result = %v, want success answering the verb's links", resp.Msg.GetResult())
			}
			if len(h.Verbs.linkChanges) != 1 || h.Verbs.linkChanges[0] != tt.want {
				t.Fatalf("changes = %q, want [%q]", h.Verbs.linkChanges, tt.want)
			}
		})
	}
}

// TestChangeWorkspaceLinksRefusesAnUnknownWorkspace pins the
// unknown_workspace arm, with no verb reached.
func TestChangeWorkspaceLinksRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	resp, err := h.Client.ChangeWorkspaceLinks(context.Background(),
		connect.NewRequest(&agentreplv1.ChangeWorkspaceLinksRequest{
			Workspace: &workspacev1.WorkspaceRef{Id: "ws-nope"},
			Change:    &agentreplv1.ChangeWorkspaceLinksRequest_Clear{Clear: &agentreplv1.ChangeWorkspaceLinksClear{}},
		}))

	// Assert
	if err != nil {
		t.Fatalf("ChangeWorkspaceLinks: %v", err)
	}
	if resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("result = %v, want unknown_workspace", resp.Msg.GetResult())
	}
	if len(h.Verbs.linkChanges) != 0 {
		t.Fatalf("changes = %q, want none for an unknown workspace", h.Verbs.linkChanges)
	}
}

// TestChangeWorkspaceLinksMapsTheVerbsRefusals pins each of the verb's own
// refusals answering its arm with its fields.
func TestChangeWorkspaceLinksMapsTheVerbsRefusals(t *testing.T) {
	tests := []struct {
		name    string
		refusal *workspace.Refusal
		check   func(e *agentreplv1.ChangeWorkspaceLinksError) bool
	}{
		{name: "invalid_link", refusal: &workspace.Refusal{Arm: workspace.ArmInvalidLink, Fields: map[string]any{"url": "ftp://x", "reason": "url must be http or https"}},
			check: func(e *agentreplv1.ChangeWorkspaceLinksError) bool {
				return e.GetInvalidLink().GetUrl() == "ftp://x" && e.GetInvalidLink().GetReason() == "url must be http or https"
			}},
		{name: "invalid_label", refusal: &workspace.Refusal{Arm: workspace.ArmInvalidLabel, Fields: map[string]any{"label": "Doc", "reason": "a label is 2 to 8 words, not 1"}},
			check: func(e *agentreplv1.ChangeWorkspaceLinksError) bool {
				return e.GetInvalidLabel().GetLabel() == "Doc" && e.GetInvalidLabel().GetReason() == "a label is 2 to 8 words, not 1"
			}},
		{name: "unknown_link", refusal: &workspace.Refusal{Arm: workspace.ArmUnknownLink, Fields: map[string]any{"url": "https://absent"}},
			check: func(e *agentreplv1.ChangeWorkspaceLinksError) bool {
				return e.GetUnknownLink().GetUrl() == "https://absent"
			}},
		{name: "unknown_announced_link", refusal: &workspace.Refusal{Arm: workspace.ArmUnknownAnnouncedLink, Fields: map[string]any{"url": "https://absent"}},
			check: func(e *agentreplv1.ChangeWorkspaceLinksError) bool {
				return e.GetUnknownAnnouncedLink().GetUrl() == "https://absent"
			}},
		{name: "too_many_shown", refusal: &workspace.Refusal{Arm: workspace.ArmTooManyShown, Fields: map[string]any{"named": uint32(6), "limit": uint32(5)}},
			check: func(e *agentreplv1.ChangeWorkspaceLinksError) bool {
				return e.GetTooManyShown().GetNamed() == 6 && e.GetTooManyShown().GetLimit() == 5
			}},
		{name: "pull_request_owned_elsewhere", refusal: &workspace.Refusal{Arm: workspace.ArmPullRequestOwnedElsewhere,
			Fields: map[string]any{"url": "https://github.com/o/r/pull/7", "owner": &workspacev1.WorkspaceRef{Id: "w2", Dir: "/w2"}, "owner_name": "owner"}},
			check: func(e *agentreplv1.ChangeWorkspaceLinksError) bool {
				got := e.GetPullRequestOwnedElsewhere()
				return got.GetUrl() == "https://github.com/o/r/pull/7" && got.GetOwner().GetId() == "w2" && got.GetOwnerName() == "owner"
			}},
		{name: "subpage_of_owned_link", refusal: &workspace.Refusal{Arm: workspace.ArmSubpageOfOwnedLink,
			Fields: map[string]any{"url": "https://gns.example.com/key/a.b", "owned_page_url": "https://gns.example.com/key/a"}},
			check: func(e *agentreplv1.ChangeWorkspaceLinksError) bool {
				got := e.GetSubpageOfOwnedLink()
				return got.GetUrl() == "https://gns.example.com/key/a.b" && got.GetOwnedPageUrl() == "https://gns.example.com/key/a"
			}},
		{name: "owned_link_unshown", refusal: &workspace.Refusal{Arm: workspace.ArmOwnedLinkUnshown,
			Fields: map[string]any{"owned_url": "https://example.com/owned", "reference_url": "https://example.com/reference"}},
			check: func(e *agentreplv1.ChangeWorkspaceLinksError) bool {
				got := e.GetOwnedLinkUnshown()
				return got.GetOwnedUrl() == "https://example.com/owned" && got.GetReferenceUrl() == "https://example.com/reference"
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.Verbs.linksErr = tt.refusal

			// Act
			resp, err := h.Client.ChangeWorkspaceLinks(context.Background(),
				connect.NewRequest(&agentreplv1.ChangeWorkspaceLinksRequest{Workspace: ref(),
					Change: &agentreplv1.ChangeWorkspaceLinksRequest_Set{Set: &agentreplv1.ChangeWorkspaceLinksSet{Url: "ftp://x", Label: "Doc", Ownership: workspace.LinkOwnershipView(false)}}}))

			// Assert
			if err != nil {
				t.Fatalf("ChangeWorkspaceLinks: %v", err)
			}
			if !tt.check(resp.Msg.GetError()) {
				t.Fatalf("result = %v, want %s carrying its fields", resp.Msg.GetResult(), tt.name)
			}
		})
	}
}

// TestFamilyRefusalsReachEveryRpcThatCarriesThem pins that a family rule's
// refusal, wrapped by whichever verb carried it up, answers on the rpc's own
// shared arm.
func TestFamilyRefusalsReachEveryRpcThatCarriesThem(t *testing.T) {
	parentNotOpen := &wsm.ParentNotOpenError{Parent: wsm.FamilyRef{ID: "ws-parent", Dir: "/w/parent"}, State: wsm.ParentBacklogged}
	tests := []struct {
		name  string
		setUp func(h *harness)
		call  func(h *harness) (bool, error)
	}{
		{
			name:  "CloseWorkspace has_open_children",
			setUp: func(h *harness) { h.Verbs.closeErr = fmt.Errorf("close: %w", familyChildren) },
			call: func(h *harness) (bool, error) {
				resp, err := h.Client.CloseWorkspace(context.Background(), connect.NewRequest(&agentreplv1.CloseWorkspaceRequest{Workspace: ref(), Requester: userRequester()}))
				return err == nil && resp.Msg.GetError().GetHasOpenChildren() != nil, err
			},
		},
		{
			name:  "SetWorkspaceBacklogged has_open_children",
			setUp: func(h *harness) { h.Verbs.setBackloggedErr = fmt.Errorf("hide: %w", familyChildren) },
			call: func(h *harness) (bool, error) {
				resp, err := h.Client.SetWorkspaceBacklogged(context.Background(), connect.NewRequest(&agentreplv1.SetWorkspaceBackloggedRequest{Workspace: ref(), Visibility: hideArm()}))
				return err == nil && resp.Msg.GetError().GetHasOpenChildren() != nil, err
			},
		},
		{
			name:  "SetWorkspaceBacklogged parent_not_open",
			setUp: func(h *harness) { h.Verbs.setBackloggedErr = fmt.Errorf("show: %w", parentNotOpen) },
			call: func(h *harness) (bool, error) {
				resp, err := h.Client.SetWorkspaceBacklogged(context.Background(), connect.NewRequest(&agentreplv1.SetWorkspaceBackloggedRequest{Workspace: ref(), Visibility: hideArm()}))
				return err == nil && resp.Msg.GetError().GetParentNotOpen().GetBacklogged() != nil, err
			},
		},
		{
			name:  "OpenWorkspace parent_not_open",
			setUp: func(h *harness) { h.Verbs.openErr = fmt.Errorf("open: %w", parentNotOpen) },
			call: func(h *harness) (bool, error) {
				resp, err := h.Client.OpenWorkspace(context.Background(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: ref()}))
				return err == nil && resp.Msg.GetError().GetParentNotOpen() != nil, err
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			tt.setUp(h)

			// Act.
			ok, err := tt.call(h)

			// Assert.
			if err != nil || !ok {
				t.Fatalf("answered %v (err %v), want the family arm", ok, err)
			}
		})
	}
}

// TestSetWorkspaceBackloggedRefusesAMissingVisibility pins an unset arm as a
// malformed request that reaches no verb.
func TestSetWorkspaceBackloggedRefusesAMissingVisibility(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	_, err := h.Client.SetWorkspaceBacklogged(context.Background(),
		connect.NewRequest(&agentreplv1.SetWorkspaceBackloggedRequest{Workspace: ref()}))

	// Assert
	if connect.CodeOf(err) != connect.CodeInvalidArgument {
		t.Fatalf("code = %v (%v), want InvalidArgument", connect.CodeOf(err), err)
	}
	if len(h.Verbs.backloggedSets) != 0 {
		t.Fatalf("backlogged sets = %+v, want none for a malformed request", h.Verbs.backloggedSets)
	}
}

// TestRegisterRepositoryRefusesABlankPathAsMalformed pins a blank path as a
// malformed request that reaches no verb, rather than a refusal arm.
func TestRegisterRepositoryRefusesABlankPathAsMalformed(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	_, err := h.Client.RegisterRepository(context.Background(),
		connect.NewRequest(&agentreplv1.RegisterRepositoryRequest{}))

	// Assert
	if connect.CodeOf(err) != connect.CodeInvalidArgument {
		t.Fatalf("code = %v (%v), want InvalidArgument", connect.CodeOf(err), err)
	}
	if len(h.Verbs.registeredPaths) != 0 {
		t.Fatalf("registered paths = %v, want none for a malformed request", h.Verbs.registeredPaths)
	}
}

// TestRegisterRepositoryAnswersTheRegisteredRepositoryAndWorkspace pins the
// success arm's projection: the repository's ref and whether it was known,
// and the main worktree's workspace ref and whether it was known.
func TestRegisterRepositoryAnswersTheRegisteredRepositoryAndWorkspace(t *testing.T) {
	tests := []struct {
		name       string
		registered workspace.RegisteredRepository
	}{
		{name: "a freshly minted repository and workspace", registered: workspace.RegisteredRepository{
			Repository: wsm.Repository{ID: "repo-1", Dir: "/repos/one"},
			Workspace:  wsm.Workspace{ID: "ws-main", Dir: "/repos/one"},
		}},
		{name: "a repository and workspace already known", registered: workspace.RegisteredRepository{
			Repository:             wsm.Repository{ID: "repo-1", Dir: "/repos/one"},
			RepositoryAlreadyKnown: true,
			Workspace:              wsm.Workspace{ID: "ws-main", Dir: "/repos/one"},
			WorkspaceAlreadyKnown:  true,
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Verbs.registerRepositoryAnswer = tt.registered

			// Act.
			resp, err := h.Client.RegisterRepository(context.Background(),
				connect.NewRequest(&agentreplv1.RegisterRepositoryRequest{Path: "/repos/one/README.md"}))

			// Assert.
			if err != nil {
				t.Fatalf("RegisterRepository: %v", err)
			}
			success := resp.Msg.GetSuccess()
			if got := success.GetRepository(); got.GetId() != "repo-1" || got.GetDir() != "/repos/one" {
				t.Fatalf("repository = %v, want repo-1 at /repos/one", got)
			}
			if success.GetAlreadyKnown() != tt.registered.RepositoryAlreadyKnown {
				t.Fatalf("already_known = %v, want %v", success.GetAlreadyKnown(), tt.registered.RepositoryAlreadyKnown)
			}
			if !proto.Equal(success.GetWorkspace(), tt.registered.Workspace.Ref()) {
				t.Fatalf("workspace = %v, want %v", success.GetWorkspace(), tt.registered.Workspace.Ref())
			}
			if success.GetWorkspaceAlreadyKnown() != tt.registered.WorkspaceAlreadyKnown {
				t.Fatalf("workspace_already_known = %v, want %v", success.GetWorkspaceAlreadyKnown(), tt.registered.WorkspaceAlreadyKnown)
			}
		})
	}
}

// TestRegisterRepositoryAnswersTheVerbsRefusalsOnTheirArms pins each refusal
// the verb raises reaching RegisterRepositoryError by name, with its fields.
func TestRegisterRepositoryAnswersTheVerbsRefusalsOnTheirArms(t *testing.T) {
	tests := []struct {
		name    string
		refusal *workspace.Refusal
		got     func(*agentreplv1.RegisterRepositoryError) bool
	}{
		{name: "not in a repository",
			refusal: &workspace.Refusal{Rpc: "RegisterRepository", Arm: workspace.ArmNotInARepository, Reason: "no repository"},
			got:     func(e *agentreplv1.RegisterRepositoryError) bool { return e.GetNotInARepository() != nil }},
		{name: "an unreadable path",
			refusal: &workspace.Refusal{Rpc: "RegisterRepository", Arm: workspace.ArmUnreadablePath, Reason: "no such file"},
			got:     func(e *agentreplv1.RegisterRepositoryError) bool { return e.GetUnreadablePath() != nil }},
		{name: "inside a temporary directory",
			refusal: &workspace.Refusal{Rpc: "RegisterRepository", Arm: workspace.ArmInsideTemporaryDirectory, Reason: "scratch",
				Fields: map[string]any{"dir": "/tmp/scratch/cwd", "temporary_root": "/tmp"}},
			got: func(e *agentreplv1.RegisterRepositoryError) bool {
				arm := e.GetInsideTemporaryDirectory()
				return arm.GetDir() == "/tmp/scratch/cwd" && arm.GetTemporaryRoot() == "/tmp"
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Verbs.registerRepositoryErr = tt.refusal

			// Act.
			resp, err := h.Client.RegisterRepository(context.Background(),
				connect.NewRequest(&agentreplv1.RegisterRepositoryRequest{Path: "/somewhere"}))

			// Assert.
			if err != nil {
				t.Fatalf("RegisterRepository = transport error %v, want the in-band arm", err)
			}
			if !tt.got(resp.Msg.GetError()) {
				t.Fatalf("result = %v, want the %s arm", resp.Msg.GetResult(), tt.refusal.Arm)
			}
		})
	}
}

// ---- SetWorkspaceParent ----------------------------------------------------

// parentWorkspaceID is the registered workspace a retarget names as the new
// parent.
const parentWorkspaceID = ids.WorkspaceID("ws-parent")

// withParent registers the parent workspace beside the test workspace.
func withParent(h *harness) {
	h.DB.workspaces[parentWorkspaceID] = wsm.Workspace{ID: parentWorkspaceID, Dir: "/tmp/agent-repl-parent", Name: "parent"}
}

func childOf(ref *workspacev1.WorkspaceRef) *agentreplv1.SetWorkspaceParentRequest_ChildOf {
	return &agentreplv1.SetWorkspaceParentRequest_ChildOf{ChildOf: &agentreplv1.SetWorkspaceParentChild{Parent: ref}}
}

func topLevel() *agentreplv1.SetWorkspaceParentRequest_TopLevel {
	return &agentreplv1.SetWorkspaceParentRequest_TopLevel{TopLevel: &agentreplv1.SetWorkspaceParentTopLevel{}}
}

// TestSetWorkspaceParentHandsTheTargetToTheRetarget pins each target arm
// reaching the retarget operation with the ids the registry resolved.
func TestSetWorkspaceParentHandsTheTargetToTheRetarget(t *testing.T) {
	tests := []struct {
		name string
		req  *agentreplv1.SetWorkspaceParentRequest
		want retargetCall
	}{
		{"child of a workspace", &agentreplv1.SetWorkspaceParentRequest{
			Workspace: ref(), Parent: childOf(&workspacev1.WorkspaceRef{Id: string(parentWorkspaceID), Dir: "/tmp/agent-repl-parent"}),
		}, retargetCall{ws: testWorkspaceID, parent: string(parentWorkspaceID)}},
		{"top-level", &agentreplv1.SetWorkspaceParentRequest{Workspace: ref(), Parent: topLevel()},
			retargetCall{ws: testWorkspaceID}},
		{"a child landing on the primary branch", &agentreplv1.SetWorkspaceParentRequest{
			Workspace: ref(), Parent: childOf(&workspacev1.WorkspaceRef{Id: string(parentWorkspaceID), Dir: "/tmp/agent-repl-parent"}),
			LandingTarget: &agentreplv1.WorkspaceLandingTarget{Target: &agentreplv1.WorkspaceLandingTarget_Primary{Primary: &agentreplv1.WorkspaceLandingTargetPrimary{}}},
		}, retargetCall{ws: testWorkspaceID, parent: string(parentWorkspaceID), landing: "primary"}},
		{"the relationship kept and the landing moved into a workspace", &agentreplv1.SetWorkspaceParentRequest{
			Workspace: ref(), Parent: &agentreplv1.SetWorkspaceParentRequest_Unchanged{Unchanged: &agentreplv1.SetWorkspaceParentUnchanged{}},
			LandingTarget: &agentreplv1.WorkspaceLandingTarget{Target: &agentreplv1.WorkspaceLandingTarget_Workspace{Workspace: &agentreplv1.WorkspaceLandingTargetWorkspace{
				Workspace: &workspacev1.WorkspaceRef{Id: "ws-into"}}}},
		}, retargetCall{ws: testWorkspaceID, keep: true, landing: "ws-into"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			withParent(h)

			// Act
			resp, err := h.Client.SetWorkspaceParent(context.Background(), connect.NewRequest(tt.req))

			// Assert
			if err != nil {
				t.Fatalf("SetWorkspaceParent: %v", err)
			}
			if resp.Msg.GetSuccess() == nil {
				t.Fatalf("result = %v, want success", resp.Msg.GetResult())
			}
			if len(h.Retarget.calls) != 1 || h.Retarget.calls[0] != tt.want {
				t.Fatalf("retargets = %+v, want %+v", h.Retarget.calls, tt.want)
			}
		})
	}
}

// TestSetWorkspaceParentAnswersTheRosterRevisionThatCarriesTheNesting pins
// the revision a sidebar that dropped a row onto its new parent holds its
// drawing until.
func TestSetWorkspaceParentAnswersTheRosterRevisionThatCarriesTheNesting(t *testing.T) {
	// Arrange
	h := newHarness(t)
	withParent(h)
	h.Deps.Sidebar.(*fakeSidebar).revision = 42

	// Act
	resp, err := h.Client.SetWorkspaceParent(context.Background(), connect.NewRequest(&agentreplv1.SetWorkspaceParentRequest{
		Workspace: ref(), Parent: childOf(&workspacev1.WorkspaceRef{Id: string(parentWorkspaceID), Dir: "/tmp/agent-repl-parent"}),
	}))

	// Assert
	if err != nil || resp.Msg.GetSuccess().GetRosterRevision() != 42 {
		t.Fatalf("SetWorkspaceParent = %v, %v; want success at the sidebar's revision 42", resp, err)
	}
}

// TestSetWorkspaceParentRefusesBeforeRetargeting pins the refusals the
// handler answers itself — the subject's and the parent ref's — each on its
// own arm and none reaching the retarget.
func TestSetWorkspaceParentRefusesBeforeRetargeting(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		req     *agentreplv1.SetWorkspaceParentRequest
		check   func(e *agentreplv1.SetWorkspaceParentError) bool
	}{
		{
			name:  "an unknown workspace",
			req:   &agentreplv1.SetWorkspaceParentRequest{Workspace: &workspacev1.WorkspaceRef{Id: "ws-nope"}, Parent: topLevel()},
			check: func(e *agentreplv1.SetWorkspaceParentError) bool { return e.GetUnknownWorkspace() != nil },
		},
		{
			name: "a workspace ref whose dir disagrees",
			req:  &agentreplv1.SetWorkspaceParentRequest{Workspace: &workspacev1.WorkspaceRef{Id: string(testWorkspaceID), Dir: "/elsewhere"}, Parent: topLevel()},
			check: func(e *agentreplv1.SetWorkspaceParentError) bool {
				return e.GetWorkspaceRefMismatch().GetRegistryDir() == testWorkspaceDir
			},
		},
		{
			name:    "a workspace handed to a successor",
			arrange: func(h *harness) { h.Ownership.standing = workspace.StandingTransferringAway },
			req:     &agentreplv1.SetWorkspaceParentRequest{Workspace: ref(), Parent: topLevel()},
			check:   func(e *agentreplv1.SetWorkspaceParentError) bool { return e.GetTransferringAway() != nil },
		},
		{
			name:    "a workspace not yet adopted",
			arrange: func(h *harness) { h.Ownership.standing = workspace.StandingNotYetAdopted },
			req:     &agentreplv1.SetWorkspaceParentRequest{Workspace: ref(), Parent: topLevel()},
			check:   func(e *agentreplv1.SetWorkspaceParentError) bool { return e.GetNotYetAdopted() != nil },
		},
		{
			name:  "an unknown parent",
			req:   &agentreplv1.SetWorkspaceParentRequest{Workspace: ref(), Parent: childOf(&workspacev1.WorkspaceRef{Id: "ws-gone"})},
			check: func(e *agentreplv1.SetWorkspaceParentError) bool { return e.GetUnknownParent() != nil },
		},
		{
			name: "a parent ref whose dir disagrees",
			req: &agentreplv1.SetWorkspaceParentRequest{Workspace: ref(),
				Parent: childOf(&workspacev1.WorkspaceRef{Id: string(parentWorkspaceID), Dir: "/elsewhere"})},
			check: func(e *agentreplv1.SetWorkspaceParentError) bool {
				return e.GetParentRefMismatch().GetRegistryDir() == "/tmp/agent-repl-parent"
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			withParent(h)
			if tt.arrange != nil {
				tt.arrange(h)
			}

			// Act
			resp, err := h.Client.SetWorkspaceParent(context.Background(), connect.NewRequest(tt.req))

			// Assert
			if err != nil {
				t.Fatalf("SetWorkspaceParent: %v", err)
			}
			if !tt.check(resp.Msg.GetError()) {
				t.Fatalf("result = %v, want the %s arm", resp.Msg.GetResult(), tt.name)
			}
			if len(h.Retarget.calls) != 0 {
				t.Fatalf("retargets = %+v, want none for a refusal", h.Retarget.calls)
			}
		})
	}
}

// TestSetWorkspaceParentMapsTheRetargetsRefusals pins each refusal the
// retarget operation raises answering its own arm.
func TestSetWorkspaceParentMapsTheRetargetsRefusals(t *testing.T) {
	tests := []struct {
		name  string
		err   error
		check func(e *agentreplv1.SetWorkspaceParentError) bool
	}{
		{"unknown parent", &workspace.Refusal{Arm: workspace.ArmUnknownParent, Reason: "gone", NotFound: true},
			func(e *agentreplv1.SetWorkspaceParentError) bool { return e.GetUnknownParent() != nil }},
		{"cycle", &workspace.Refusal{Arm: workspace.ArmCycle, Reason: "loop"},
			func(e *agentreplv1.SetWorkspaceParentError) bool { return e.GetCycle() != nil }},
		{"no merge layout", &workspace.Refusal{Arm: workspace.ArmNoMergeLayout, Reason: "registered"},
			func(e *agentreplv1.SetWorkspaceParentError) bool { return e.GetNoMergeLayout() != nil }},
		{"cold gate", fmt.Errorf("retarget ws-1: notify: submit: %w", &promptqueue.ColdGateRefusal{Detail: "resumed cold"}),
			func(e *agentreplv1.SetWorkspaceParentError) bool { return e.GetColdGate() != nil }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.Retarget.err = tt.err

			// Act
			resp, err := h.Client.SetWorkspaceParent(context.Background(),
				connect.NewRequest(&agentreplv1.SetWorkspaceParentRequest{Workspace: ref(), Parent: topLevel()}))

			// Assert
			if err != nil {
				t.Fatalf("SetWorkspaceParent: %v", err)
			}
			if !tt.check(resp.Msg.GetError()) {
				t.Fatalf("result = %v, want the %s arm", resp.Msg.GetResult(), tt.name)
			}
		})
	}
}

// TestSetWorkspaceParentFailsAFailedRetargetAsInternal pins a retarget that
// broke, rather than refused, answering Internal and recorded at error.
func TestSetWorkspaceParentFailsAFailedRetargetAsInternal(t *testing.T) {
	// Arrange
	h := newHarness(t)
	log := dlog.NewTestLogger()
	h.Surfaces.workspace = log
	h.Retarget.err = errors.New("disk I/O error")

	// Act
	_, err := h.Client.SetWorkspaceParent(context.Background(),
		connect.NewRequest(&agentreplv1.SetWorkspaceParentRequest{Workspace: ref(), Parent: topLevel()}))

	// Assert
	if connect.CodeOf(err) != connect.CodeInternal {
		t.Fatalf("code = %v (%v), want Internal", connect.CodeOf(err), err)
	}
	recorded := false
	for _, r := range log.Records() {
		recorded = recorded || (r.Level == "error" && r.Operation == "SetWorkspaceParent")
	}
	if !recorded {
		t.Fatalf("records = %+v, want the failed retarget recorded at error", log.Records())
	}
}

// TestSetWorkspaceParentRefusesAMalformedRequest pins an unset target arm, and
// a child_of arm naming no parent, as malformed requests reaching no retarget.
func TestSetWorkspaceParentRefusesAMalformedRequest(t *testing.T) {
	tests := []struct {
		name string
		req  *agentreplv1.SetWorkspaceParentRequest
	}{
		{"no target arm", &agentreplv1.SetWorkspaceParentRequest{Workspace: ref()}},
		{"a child_of naming no parent", &agentreplv1.SetWorkspaceParentRequest{Workspace: ref(), Parent: childOf(nil)}},
		{"the relationship kept with no landing target", &agentreplv1.SetWorkspaceParentRequest{Workspace: ref(),
			Parent: &agentreplv1.SetWorkspaceParentRequest_Unchanged{Unchanged: &agentreplv1.SetWorkspaceParentUnchanged{}}}},
		{"a landing target cut from origin", &agentreplv1.SetWorkspaceParentRequest{Workspace: ref(), Parent: topLevel(),
			LandingTarget: &agentreplv1.WorkspaceLandingTarget{Target: &agentreplv1.WorkspaceLandingTarget_Primary{Primary: &agentreplv1.WorkspaceLandingTargetPrimary{
				FromOrigin: &agentreplv1.WorkspaceLandingTargetPrimaryFromOrigin{}}}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			_, err := h.Client.SetWorkspaceParent(context.Background(), connect.NewRequest(tt.req))

			// Assert
			if connect.CodeOf(err) != connect.CodeInvalidArgument {
				t.Fatalf("code = %v (%v), want InvalidArgument", connect.CodeOf(err), err)
			}
			if len(h.Retarget.calls) != 0 {
				t.Fatalf("retargets = %+v, want none for a malformed request", h.Retarget.calls)
			}
		})
	}
}

// ---- ReportUserInteraction -------------------------------------------------

// TestReportUserInteractionHandsEachKindToTheVerb pins every interaction kind
// reaching the verb, named, with the id the registry resolved.
func TestReportUserInteractionHandsEachKindToTheVerb(t *testing.T) {
	cases := []struct {
		name string
		kind *agentreplv1.UserInteraction
		want workspace.Interaction
	}{
		{name: "feed scrolled", want: workspace.InteractionFeedScrolled, kind: &agentreplv1.UserInteraction{
			Kind: &agentreplv1.UserInteraction_FeedScrolled{FeedScrolled: &agentreplv1.UserInteractionFeedScrolled{}}}},
		{name: "composer typed", want: workspace.InteractionComposerTyped, kind: &agentreplv1.UserInteraction{
			Kind: &agentreplv1.UserInteraction_ComposerTyped{ComposerTyped: &agentreplv1.UserInteractionComposerTyped{}}}},
		{name: "feed navigated", want: workspace.InteractionFeedNavigated, kind: &agentreplv1.UserInteraction{
			Kind: &agentreplv1.UserInteraction_FeedNavigated{FeedNavigated: &agentreplv1.UserInteractionFeedNavigated{}}}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			resp, err := h.Client.ReportUserInteraction(context.Background(), connect.NewRequest(
				&agentreplv1.ReportUserInteractionRequest{Workspace: ref(), Interaction: tc.kind}))

			// Assert.
			if err != nil {
				t.Fatalf("ReportUserInteraction: %v", err)
			}
			if resp.Msg.GetSuccess() == nil {
				t.Fatalf("result = %v, want success", resp.Msg.GetResult())
			}
			want := []interactionCall{{ws: testWorkspaceID, kind: tc.want}}
			if !slices.Equal(h.Verbs.interactions, want) {
				t.Fatalf("interactions = %v, want %v", h.Verbs.interactions, want)
			}
		})
	}
}

// TestReportUserInteractionRefusesAnUnsetKind pins that a request naming no
// interaction is InvalidArgument and reaches no verb.
func TestReportUserInteractionRefusesAnUnsetKind(t *testing.T) {
	cases := []struct {
		name        string
		interaction *agentreplv1.UserInteraction
	}{
		{name: "no interaction", interaction: nil},
		{name: "an interaction with no kind", interaction: &agentreplv1.UserInteraction{}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			_, err := h.Client.ReportUserInteraction(context.Background(), connect.NewRequest(
				&agentreplv1.ReportUserInteractionRequest{Workspace: ref(), Interaction: tc.interaction}))

			// Assert.
			if connect.CodeOf(err) != connect.CodeInvalidArgument {
				t.Fatalf("error = %v, want InvalidArgument", err)
			}
			if len(h.Verbs.interactions) != 0 {
				t.Fatalf("interactions = %v, want none for a malformed request", h.Verbs.interactions)
			}
		})
	}
}

// TestReportUserInteractionRefusesAnUnknownWorkspace pins that an id the
// registry does not hold answers the unknown_workspace arm rather than a
// transport error.
func TestReportUserInteractionRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.ReportUserInteraction(context.Background(), connect.NewRequest(&agentreplv1.ReportUserInteractionRequest{
		Workspace:   &workspacev1.WorkspaceRef{Id: "ws-nope"},
		Interaction: &agentreplv1.UserInteraction{Kind: &agentreplv1.UserInteraction_FeedScrolled{FeedScrolled: &agentreplv1.UserInteractionFeedScrolled{}}},
	}))

	// Assert.
	if err != nil {
		t.Fatalf("ReportUserInteraction: %v", err)
	}
	if resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("result = %v, want unknown_workspace", resp.Msg.GetResult())
	}
	if len(h.Verbs.interactions) != 0 {
		t.Fatalf("interactions = %v, want none for an unknown workspace", h.Verbs.interactions)
	}
}

// TestReportUserInteractionAnswersTheVerbsRefusal pins a refused interaction
// answering its refusal rather than success.
func TestReportUserInteractionAnswersTheVerbsRefusal(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.interactionErr = errors.New("the store is gone")

	// Act.
	resp, err := h.Client.ReportUserInteraction(context.Background(), connect.NewRequest(&agentreplv1.ReportUserInteractionRequest{
		Workspace:   ref(),
		Interaction: &agentreplv1.UserInteraction{Kind: &agentreplv1.UserInteraction_ComposerTyped{ComposerTyped: &agentreplv1.UserInteractionComposerTyped{}}},
	}))

	// Assert.
	if err == nil && resp.Msg.GetSuccess() != nil {
		t.Fatalf("result = %v, want the refusal, not success", resp.Msg.GetResult())
	}
}

// TestASelectWhoseCallerLeftRecordsNoError is the 18:28:56 switch at the rpc:
// the caller cancelled mid-revival, and "the rpc failed" was recorded twice
// at ERROR for a select nobody was still waiting on.
func TestASelectWhoseCallerLeftRecordsNoError(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	log := dlog.NewTestLogger()
	h.Surfaces.workspace = log
	h.Verbs.selectEntered, h.Verbs.selectLeft = make(chan struct{}), make(chan struct{})
	ctx, cancel := context.WithCancel(context.Background())
	answered := make(chan error, 1)
	go func() {
		_, err := h.Client.SelectWorkspace(ctx, connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{Workspace: ref()}))
		answered <- err
	}()

	// Act.
	awaitClosed(t, h.Verbs.selectEntered, "the select")
	cancel()
	awaitClosed(t, h.Verbs.selectLeft, "the select's return")
	// Close waits for the handler to finish, so its records are all in.
	h.HTTP.Close()

	// Assert.
	infos := 0
	for _, r := range log.Records() {
		if r.Level == dlog.LevelError {
			t.Fatalf("records = %+v, want no ERROR for a caller that left", log.Records())
		}
		if r.Level == dlog.LevelInfo && r.Message == "the caller left before the select finished" {
			infos++
		}
	}
	if infos != 1 {
		t.Fatalf("records = %+v, want the departure at INFO once", log.Records())
	}
}

// awaitClosed waits for ch to close, failing the test past a bound.
func awaitClosed(t *testing.T, ch <-chan struct{}, what string) {
	t.Helper()
	select {
	case <-ch:
	case <-time.After(5 * time.Second):
		t.Fatalf("%s never happened", what)
	}
}

// ownFinish is the workspace's deploy of its own work, asked by the user,
// at the back of the queue.
func ownFinish() *agentreplv1.FinishWorkspaceRequest {
	return &agentreplv1.FinishWorkspaceRequest{Workspace: ref(), Requester: userRequester(), Placement: backPlacement()}
}

func strPtr(s string) *string { return &s }

func TestFinishWorkspaceMapsTheQueuesRefusalsOntoItsArms(t *testing.T) {
	tests := []struct {
		name    string
		refusal *merge.RefusalError
		got     func(*agentreplv1.FinishWorkspaceError) bool
	}{
		{name: "already queued", refusal: &merge.RefusalError{Arm: merge.ArmAlreadyQueued, Reason: "already waiting"},
			got: func(e *agentreplv1.FinishWorkspaceError) bool { return e.GetAlreadyQueued() != nil }},
		{name: "already deploying", refusal: &merge.RefusalError{Arm: merge.ArmAlreadyMerging, Reason: "already running"},
			got: func(e *agentreplv1.FinishWorkspaceError) bool { return e.GetAlreadyDeploying() != nil }},
		{name: "no deploy config", refusal: &merge.RefusalError{Arm: merge.ArmNoDeployConfig, Reason: "none",
			Fields: map[string]any{"path": "/repo/.agent-repl/lifecycle.yaml"}},
			got: func(e *agentreplv1.FinishWorkspaceError) bool {
				return e.GetNoDeployConfig().GetPath() == "/repo/.agent-repl/lifecycle.yaml"
			}},
		{name: "an unusable deploy config", refusal: &merge.RefusalError{Arm: merge.ArmDeployConfigInvalid, Reason: "bad",
			Fields: map[string]any{"path": "/repo/.agent-repl/lifecycle.yaml", "detail": "pre_deploy is required"}},
			got: func(e *agentreplv1.FinishWorkspaceError) bool {
				return e.GetDeployConfigInvalid().GetPath() == "/repo/.agent-repl/lifecycle.yaml" && e.GetDeployConfigInvalid().GetDetail() == "pre_deploy is required"
			}},
		{name: "placed behind a merge not in line", refusal: &merge.RefusalError{Arm: merge.ArmPlacementBehindNotInLine, Reason: "none",
			Fields: map[string]any{"behind": "two"}},
			got: func(e *agentreplv1.FinishWorkspaceError) bool {
				return e.GetPlacementBehindNotInLine().GetBehind() == "two"
			}},
		{name: "its merge stands off its queue", refusal: &merge.RefusalError{Arm: merge.ArmMergeOffQueue, Reason: "left",
			Fields: map[string]any{"cause": merge.OffQueueCause("failed")}},
			got: func(e *agentreplv1.FinishWorkspaceError) bool {
				return e.GetMergeOffQueue().GetCause().GetFailed() != nil
			}},
		{name: "its branch failed its rule suites", refusal: &merge.RefusalError{Arm: merge.ArmRuleChecksFailed, Reason: "failed",
			Fields: map[string]any{"worktree": "/w/a", "suites": []string{"repo-scans", "shellcheck"}, "failing_tests": "- TestX", "log": "/s/rule_check-1-0.log"}},
			got: func(e *agentreplv1.FinishWorkspaceError) bool {
				r := e.GetRuleChecksFailed()
				return r.GetWorktree() == "/w/a" && slices.Equal(r.GetSuites(), []string{"repo-scans", "shellcheck"}) &&
					r.GetFailingTests() == "- TestX" && r.GetLog() == "/s/rule_check-1-0.log"
			}},
		{name: "a workspace with no layout facts", refusal: &merge.RefusalError{Arm: merge.ArmNoLayoutFacts, Reason: "no recorded geometry"},
			got: func(e *agentreplv1.FinishWorkspaceError) bool { return e.GetNoLayoutFacts() != nil }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Merge.enqueueErr = tt.refusal

			// Act.
			resp, err := h.Client.FinishWorkspace(context.Background(), connect.NewRequest(ownFinish()))

			// Assert.
			if err != nil || !tt.got(resp.Msg.GetError()) {
				t.Fatalf("FinishWorkspace = %v, %v; want the %s arm", resp, err, tt.refusal.Arm)
			}
		})
	}
}

func TestFinishWorkspaceMapsTheRequest(t *testing.T) {
	front := ownFinish()
	front.Placement = frontPlacement()
	asked := ownFinish()
	asked.Requester = workspaceRequester(ref())
	tests := []struct {
		name      string
		req       *agentreplv1.FinishWorkspaceRequest
		want      wsm.MergeSource
		wantBy    merge.Requester
		wantAsker ids.WorkspaceID
	}{
		{name: "asked by the user", req: ownFinish(), want: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch}, wantBy: merge.RequestedByUser},
		{name: "at the front", req: front, want: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch, Front: true}, wantBy: merge.RequestedByUser},
		{name: "asked by a workspace's agent", req: asked, want: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch}, wantBy: merge.RequestedByAgent, wantAsker: testWorkspaceID},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			if _, err := h.Client.FinishWorkspace(context.Background(), connect.NewRequest(tt.req)); err != nil {
				t.Fatalf("FinishWorkspace: %v", err)
			}

			// Assert.
			if len(h.Merge.requests) != 1 || !reflect.DeepEqual(h.Merge.requests[0].Source, tt.want) {
				t.Fatalf("requests = %+v, want source %+v", h.Merge.requests, tt.want)
			}
			got := h.Merge.requests[0]
			if got.By != tt.wantBy || got.Asker != tt.wantAsker || got.Detached != nil {
				t.Fatalf("request = %+v, want asked by %v (%q) in place", got, tt.wantBy, tt.wantAsker)
			}
		})
	}
}

func TestFinishWorkspaceJoinsWhereItsCreateFixed(t *testing.T) {
	none := ownFinish()
	none.Placement = nil
	front := ownFinish()
	front.Placement = frontPlacement()
	back := ownFinish()
	tests := []struct {
		name string
		// stored is the placement the workspace's create fixed.
		stored wsm.FinishPlacement
		req    *agentreplv1.FinishWorkspaceRequest
		// wantFront is whether the finish joined at the front; wantRefused
		// whether it was refused against the stored placement.
		wantFront, wantRefused bool
	}{
		{name: "a finish naming none joins where its create fixed", stored: wsm.FinishPlacementFront, req: none, wantFront: true},
		{name: "a finish naming the fixed placement joins there", stored: wsm.FinishPlacementFront, req: front, wantFront: true},
		{name: "a finish naming another placement is refused", stored: wsm.FinishPlacementFront, req: back, wantRefused: true},
		{name: "a fixed back placement joins at the back", stored: wsm.FinishPlacementBack, req: none},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ws := h.DB.workspaces[testWorkspaceID]
			ws.FinishPlacement = tt.stored
			h.DB.workspaces[testWorkspaceID] = ws

			// Act
			resp, err := h.Client.FinishWorkspace(context.Background(), connect.NewRequest(tt.req))

			// Assert
			if err != nil {
				t.Fatalf("FinishWorkspace: %v", err)
			}
			if conflict := resp.Msg.GetError().GetPlacementConflictsWithStored(); (conflict != nil) != tt.wantRefused {
				t.Fatalf("result = %v, want refused %v", resp.Msg.GetResult(), tt.wantRefused)
			}
			if tt.wantRefused {
				if len(h.Merge.requests) != 0 {
					t.Fatalf("requests = %+v, want nothing enqueued", h.Merge.requests)
				}
				return
			}
			if len(h.Merge.requests) != 1 || h.Merge.requests[0].Source.Front != tt.wantFront {
				t.Fatalf("requests = %+v, want one at the front %v", h.Merge.requests, tt.wantFront)
			}
		})
	}
}

// TestFinishWorkspaceFailsWhenItsStoredPlacementCannotBeRead: the stored
// placement is read from the workspace's whole row, and a failed read is
// answered internal, recorded at ERROR with its cause, and enqueues nothing.
func TestFinishWorkspaceFailsWhenItsStoredPlacementCannotBeRead(t *testing.T) {
	// Arrange
	h, log := newLoggedHarness(t)
	h.DB.rowErr = errors.New("database is locked")

	// Act
	_, err := h.Client.FinishWorkspace(context.Background(), connect.NewRequest(ownFinish()))

	// Assert
	if connect.CodeOf(err) != connect.CodeInternal {
		t.Fatalf("FinishWorkspace = %v, want internal", err)
	}
	if len(h.Merge.requests) != 0 {
		t.Fatalf("requests = %+v, want nothing enqueued", h.Merge.requests)
	}
	for _, record := range log.at("ERROR") {
		if cause, _ := record.Context["cause"].(string); record.Operation == "FinishWorkspace" && strings.Contains(cause, "database is locked") {
			return
		}
	}
	t.Fatalf("no error record named the failed read: %+v", log.at("ERROR"))
}

func TestFinishWorkspaceAnswersWhoMayLandIt(t *testing.T) {
	tests := []struct {
		name string
		arm  string
		got  func(*agentreplv1.FinishWorkspaceError) bool
	}{
		{name: "a stranger", arm: merge.ArmRequesterMayNotLand, got: func(e *agentreplv1.FinishWorkspaceError) bool { return e.GetRequesterMayNotLand() != nil }},
		{name: "a detached head", arm: merge.ArmNoBranchCheckedOut, got: func(e *agentreplv1.FinishWorkspaceError) bool { return e.GetNoBranchCheckedOut().GetDir() == "/wt" }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Merge.enqueueErr = &merge.RefusalError{Arm: tt.arm, Reason: "refused", Fields: map[string]any{
				"dir": "/wt"}}

			// Act.
			resp, err := h.Client.FinishWorkspace(context.Background(), connect.NewRequest(ownFinish()))

			// Assert.
			if err != nil || !tt.got(resp.Msg.GetError()) {
				t.Fatalf("FinishWorkspace = %v, %v; want the %s arm", resp, err, tt.arm)
			}
		})
	}
}

func TestFinishWorkspaceRefusesARequestMissingAChoiceAsInvalid(t *testing.T) {
	noRequester := ownFinish()
	noRequester.Requester = nil
	noPlacement := ownFinish()
	noPlacement.Placement = nil
	tests := []struct {
		name string
		req  *agentreplv1.FinishWorkspaceRequest
	}{
		{name: "no requester", req: noRequester},
		{name: "no placement", req: noPlacement},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			_, err := h.Client.FinishWorkspace(context.Background(), connect.NewRequest(tt.req))

			// Assert.
			if connect.CodeOf(err) != connect.CodeInvalidArgument || len(h.Merge.requests) != 0 {
				t.Fatalf("FinishWorkspace = %v, requests %+v; want invalid_argument and none", err, h.Merge.requests)
			}
		})
	}
}

// ---------------------------------------------------------------------------
// FoldRepository.
// ---------------------------------------------------------------------------

func TestFoldRepositoryHandsTheResolvedRepositoryAndFoldToTheVerb(t *testing.T) {
	tests := []struct {
		name string
		req  *agentreplv1.FoldRepositoryRequest
		want repositoryFold
	}{
		{
			name: "collapse by id",
			req: &agentreplv1.FoldRepositoryRequest{
				Repository: &workspacev1.RepositoryRef{Id: "repo-1"},
				Fold:       &agentreplv1.FoldRepositoryRequest_Collapse{Collapse: &agentreplv1.FoldRepositoryCollapse{}},
			},
			want: repositoryFold{repo: "repo-1", folded: true, requester: workspace.FoldRequester{RPC: "FoldRepository"}},
		},
		{
			name: "expand by dir",
			req: &agentreplv1.FoldRepositoryRequest{
				Repository: &workspacev1.RepositoryRef{Dir: "/repos/one"},
				Fold:       &agentreplv1.FoldRepositoryRequest_Expand{Expand: &agentreplv1.FoldRepositoryExpand{}},
			},
			want: repositoryFold{repo: "repo-1", folded: false, requester: workspace.FoldRequester{RPC: "FoldRepository"}},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.DB.repositories = append(h.DB.repositories, wsm.Repository{ID: "repo-1", Dir: "/repos/one"})

			// Act
			resp, err := h.Client.FoldRepository(context.Background(), connect.NewRequest(tt.req))

			// Assert
			if err != nil {
				t.Fatalf("FoldRepository: %v", err)
			}
			if resp.Msg.GetSuccess() == nil {
				t.Fatalf("result = %v, want success", resp.Msg.GetResult())
			}
			if len(h.Verbs.folds) != 1 || h.Verbs.folds[0] != tt.want {
				t.Fatalf("folds = %+v, want %+v", h.Verbs.folds, tt.want)
			}
		})
	}
}

func TestFoldRepositoryRefusesAnUnknownRepository(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	resp, err := h.Client.FoldRepository(context.Background(), connect.NewRequest(&agentreplv1.FoldRepositoryRequest{
		Repository: &workspacev1.RepositoryRef{Id: "repo-nope"},
		Fold:       &agentreplv1.FoldRepositoryRequest_Collapse{Collapse: &agentreplv1.FoldRepositoryCollapse{}},
	}))

	// Assert
	if err != nil {
		t.Fatalf("FoldRepository: %v", err)
	}
	if resp.Msg.GetError().GetUnknownRepository() == nil {
		t.Fatalf("result = %v, want unknown_repository", resp.Msg.GetResult())
	}
	if len(h.Verbs.folds) != 0 {
		t.Fatalf("folds = %+v, want none for an unknown repository", h.Verbs.folds)
	}
}

func TestFoldRepositoryMapsTheVerbsRefusal(t *testing.T) {
	// Arrange: the repository was forgotten between the resolution and the write.
	h := newHarness(t)
	h.DB.repositories = append(h.DB.repositories, wsm.Repository{ID: "repo-1", Dir: "/repos/one"})
	h.Verbs.foldErr = &workspace.Refusal{Arm: "unknown_repository", Reason: "gone", NotFound: true}

	// Act
	resp, err := h.Client.FoldRepository(context.Background(), connect.NewRequest(&agentreplv1.FoldRepositoryRequest{
		Repository: &workspacev1.RepositoryRef{Id: "repo-1"},
		Fold:       &agentreplv1.FoldRepositoryRequest_Collapse{Collapse: &agentreplv1.FoldRepositoryCollapse{}},
	}))

	// Assert
	if err != nil {
		t.Fatalf("FoldRepository: %v", err)
	}
	if resp.Msg.GetError().GetUnknownRepository() == nil {
		t.Fatalf("result = %v, want unknown_repository", resp.Msg.GetResult())
	}
}

func TestFoldRepositoryAnswersACollapseThatHoldsTheCurrentWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.DB.repositories = append(h.DB.repositories, wsm.Repository{ID: "repo-1", Dir: "/repos/one"})
	h.Verbs.foldErr = &workspace.Refusal{Arm: workspace.ArmHoldsCurrent, Reason: "holds the current workspace"}

	// Act
	resp, err := h.Client.FoldRepository(context.Background(), connect.NewRequest(&agentreplv1.FoldRepositoryRequest{
		Repository: &workspacev1.RepositoryRef{Id: "repo-1"},
		Fold:       &agentreplv1.FoldRepositoryRequest_Collapse{Collapse: &agentreplv1.FoldRepositoryCollapse{}},
	}))

	// Assert
	if err != nil {
		t.Fatalf("FoldRepository: %v", err)
	}
	if resp.Msg.GetError().GetHoldsCurrent() == nil {
		t.Fatalf("result = %v, want holds_current", resp.Msg.GetResult())
	}
}

func TestFoldRepositoryFailsAVerbError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.DB.repositories = append(h.DB.repositories, wsm.Repository{ID: "repo-1", Dir: "/repos/one"})
	h.Verbs.foldErr = errors.New("disk I/O error")

	// Act
	_, err := h.Client.FoldRepository(context.Background(), connect.NewRequest(&agentreplv1.FoldRepositoryRequest{
		Repository: &workspacev1.RepositoryRef{Id: "repo-1"},
		Fold:       &agentreplv1.FoldRepositoryRequest_Collapse{Collapse: &agentreplv1.FoldRepositoryCollapse{}},
	}))

	// Assert
	if connect.CodeOf(err) != connect.CodeInternal {
		t.Fatalf("FoldRepository = %v, want an internal failure", err)
	}
}

func TestValidateFoldRepositoryRequestRefusesAnIncompleteRequest(t *testing.T) {
	tests := []struct {
		name string
		req  *agentreplv1.FoldRepositoryRequest
	}{
		{"no repository", &agentreplv1.FoldRepositoryRequest{
			Fold: &agentreplv1.FoldRepositoryRequest_Expand{Expand: &agentreplv1.FoldRepositoryExpand{}}}},
		{"no fold arm", &agentreplv1.FoldRepositoryRequest{Repository: &workspacev1.RepositoryRef{Id: "repo-1"}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			err := validateFoldRepositoryRequest(tt.req)

			// Assert
			if err == nil || err.Code() != connect.CodeInvalidArgument {
				t.Fatalf("validate = %v, want InvalidArgument", err)
			}
		})
	}
}

// repositoryRPCs are every rpc that names a repository through
// resolveRepository, each answering its unknown_repository arm or nil.
var repositoryRPCs = []struct {
	name string
	call func(*harness, *workspacev1.RepositoryRef) (bool, error)
}{
	{"CreateWorkspace", func(h *harness, ref *workspacev1.RepositoryRef) (bool, error) {
		resp, err := h.Client.CreateWorkspace(context.Background(), connect.NewRequest(standardCreate(ref, "")))
		return err == nil && resp.Msg.GetError().GetUnknownRepository() != nil, err
	}},
	{"FoldRepository", func(h *harness, ref *workspacev1.RepositoryRef) (bool, error) {
		resp, err := h.Client.FoldRepository(context.Background(), connect.NewRequest(&agentreplv1.FoldRepositoryRequest{
			Repository: ref,
			Fold:       &agentreplv1.FoldRepositoryRequest_Collapse{Collapse: &agentreplv1.FoldRepositoryCollapse{}},
		}))
		return err == nil && resp.Msg.GetError().GetUnknownRepository() != nil, err
	}},
	{"UpdateSidebarView", func(h *harness, ref *workspacev1.RepositoryRef) (bool, error) {
		resp, err := h.Client.UpdateSidebarView(context.Background(), connect.NewRequest(foldChange(&agentreplv1.SidebarViewFoldSection{
			Section: &agentreplv1.SidebarViewFoldSection_Repository{Repository: ref},
			Fold:    collapseFold(),
		})))
		return err == nil && resp.Msg.GetError().GetUnknownRepository() != nil, err
	}},
}

func TestEveryRepositoryRPCRefusesAnUnknownRepositoryTheSameWay(t *testing.T) {
	for _, tt := range repositoryRPCs {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			refused, err := tt.call(h, &workspacev1.RepositoryRef{Id: "repo-nope"})

			// Assert
			if err != nil || !refused {
				t.Fatalf("%s = refused %v, error %v; want the unknown_repository arm", tt.name, refused, err)
			}
		})
	}
}

func TestEveryRepositoryRPCFailsAnUnreadableRegistryAsInternal(t *testing.T) {
	for _, tt := range repositoryRPCs {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.DB.listRepositoriesErr = errors.New("database is closed")

			// Act
			_, err := tt.call(h, &workspacev1.RepositoryRef{Id: "repo-1"})

			// Assert
			if connect.CodeOf(err) != connect.CodeInternal {
				t.Fatalf("%s = %v, want an internal failure", tt.name, err)
			}
		})
	}
}

// ---- NukeWorkspace ---------------------------------------------------------

// TestANukeWhoseTeardownRefusesAnswersTheRefusalInBand pins a nuke with no
// op_id: its teardown runs inside the request, so the teardown's refusal is
// the request's answer, as its typed arm, and recorded.
func TestANukeWhoseTeardownRefusesAnswersTheRefusalInBand(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	log := dlog.NewTestLogger()
	h.Surfaces.workspace = log
	h.Verbs.teardownErr = &workspace.Refusal{Rpc: "NukeWorkspace", Arm: workspace.ArmGitFailed, Reason: "the worktree is locked"}

	// Act.
	resp, err := h.Client.NukeWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.NukeWorkspaceRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("NukeWorkspace: %v", err)
	}
	if resp.Msg.GetError().GetGitFailed() == nil {
		t.Fatalf("result = %v, want git_failed", resp.Msg.GetResult())
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Operation: "NukeWorkspace", Message: "answered a typed refusal",
		Match: func(r dlog.Record) bool { return r.Context["arm"] == workspace.ArmGitFailed }}); !ok {
		t.Fatalf("records = %+v, want the git_failed refusal recorded", log.Records())
	}
}

// ---- SwapWorkspaces --------------------------------------------------------

// swapHarness is a harness whose registry holds a second workspace, ws-2.
func swapHarness(t *testing.T) (*harness, *workspacev1.WorkspaceRef) {
	t.Helper()
	h := newHarness(t)
	other := wsm.Workspace{ID: "ws-2", Dir: "/tmp/agent-repl-test-workspace-2"}
	h.DB.workspaces[other.ID] = other
	return h, &workspacev1.WorkspaceRef{Id: string(other.ID), Dir: other.Dir}
}

func TestSwapWorkspacesHandsBothResolvedIdsToTheVerb(t *testing.T) {
	// Arrange.
	h, target := swapHarness(t)

	// Act.
	resp, err := h.Client.SwapWorkspaces(context.Background(),
		connect.NewRequest(&agentreplv1.SwapWorkspacesRequest{Workspace: ref(), Target: target}))

	// Assert.
	if err != nil {
		t.Fatalf("SwapWorkspaces: %v", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("result = %v, want success", resp.Msg.GetResult())
	}
	if want := [][2]ids.WorkspaceID{{testWorkspaceID, "ws-2"}}; !slices.Equal(h.Verbs.swaps, want) {
		t.Fatalf("swaps = %v, want %v", h.Verbs.swaps, want)
	}
}

func TestSwapWorkspacesNamesTheUnknownRef(t *testing.T) {
	// Arrange.
	h, _ := swapHarness(t)
	missing := &workspacev1.WorkspaceRef{Id: "ws-nope"}

	// Act.
	resp, err := h.Client.SwapWorkspaces(context.Background(),
		connect.NewRequest(&agentreplv1.SwapWorkspacesRequest{Workspace: ref(), Target: missing}))

	// Assert.
	if err != nil {
		t.Fatalf("SwapWorkspaces: %v", err)
	}
	got := resp.Msg.GetError().GetUnknownWorkspace()
	if got == nil || got.GetWorkspace().GetId() != "ws-nope" {
		t.Fatalf("result = %v, want unknown_workspace naming ws-nope", resp.Msg.GetResult())
	}
	if len(h.Verbs.swaps) != 0 {
		t.Fatalf("swaps = %v, want none for an unknown workspace", h.Verbs.swaps)
	}
}

func TestSwapWorkspacesNamesTheMismatchedRef(t *testing.T) {
	// Arrange.
	h, target := swapHarness(t)
	stale := &workspacev1.WorkspaceRef{Id: target.GetId(), Dir: "/somewhere/else"}

	// Act.
	resp, err := h.Client.SwapWorkspaces(context.Background(),
		connect.NewRequest(&agentreplv1.SwapWorkspacesRequest{Workspace: ref(), Target: stale}))

	// Assert.
	if err != nil {
		t.Fatalf("SwapWorkspaces: %v", err)
	}
	got := resp.Msg.GetError().GetWorkspaceRefMismatch()
	if got == nil || got.GetWorkspace().GetId() != target.GetId() || got.GetRegistryDir() != target.GetDir() {
		t.Fatalf("result = %v, want workspace_ref_mismatch naming ws-2 and its registry dir", resp.Msg.GetResult())
	}
}

func TestSwapWorkspacesAnswersOtherRepository(t *testing.T) {
	// Arrange.
	h, target := swapHarness(t)
	h.Verbs.swapErr = &workspace.Refusal{Rpc: "SwapWorkspaces", Arm: workspace.ArmOtherRepository, Reason: "two repositories"}

	// Act.
	resp, err := h.Client.SwapWorkspaces(context.Background(),
		connect.NewRequest(&agentreplv1.SwapWorkspacesRequest{Workspace: ref(), Target: target}))

	// Assert.
	if err != nil {
		t.Fatalf("SwapWorkspaces: %v", err)
	}
	if resp.Msg.GetError().GetOtherRepository() == nil {
		t.Fatalf("result = %v, want other_repository", resp.Msg.GetResult())
	}
}

func TestSwapWorkspacesAnswersNotSiblings(t *testing.T) {
	// Arrange.
	h, target := swapHarness(t)
	h.Verbs.swapErr = &workspace.Refusal{Rpc: "SwapWorkspaces", Arm: workspace.ArmNotSiblings, Reason: "a child and a root"}

	// Act.
	resp, err := h.Client.SwapWorkspaces(context.Background(),
		connect.NewRequest(&agentreplv1.SwapWorkspacesRequest{Workspace: ref(), Target: target}))

	// Assert.
	if err != nil {
		t.Fatalf("SwapWorkspaces: %v", err)
	}
	if resp.Msg.GetError().GetNotSiblings() == nil {
		t.Fatalf("result = %v, want not_siblings", resp.Msg.GetResult())
	}
}

func TestSwapWorkspacesFailsAVerbError(t *testing.T) {
	// Arrange.
	h, target := swapHarness(t)
	h.Verbs.swapErr = errors.New("disk I/O error")

	// Act.
	_, err := h.Client.SwapWorkspaces(context.Background(),
		connect.NewRequest(&agentreplv1.SwapWorkspacesRequest{Workspace: ref(), Target: target}))

	// Assert.
	if connect.CodeOf(err) != connect.CodeInternal {
		t.Fatalf("SwapWorkspaces = %v, want an internal failure", err)
	}
}

// ---- MoveWorkspace ---------------------------------------------------------

func TestMoveWorkspaceHandsBothResolvedIdsToTheVerb(t *testing.T) {
	// Arrange.
	h, target := swapHarness(t)

	// Act.
	resp, err := h.Client.MoveWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.MoveWorkspaceRequest{Workspace: ref(), Target: target}))

	// Assert.
	if err != nil {
		t.Fatalf("MoveWorkspace: %v", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("result = %v, want success", resp.Msg.GetResult())
	}
	if want := [][2]ids.WorkspaceID{{testWorkspaceID, "ws-2"}}; !slices.Equal(h.Verbs.moves, want) {
		t.Fatalf("moves = %v, want %v", h.Verbs.moves, want)
	}
}

func TestMoveWorkspaceNamesTheUnknownRef(t *testing.T) {
	// Arrange.
	h, _ := swapHarness(t)
	missing := &workspacev1.WorkspaceRef{Id: "ws-nope"}

	// Act.
	resp, err := h.Client.MoveWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.MoveWorkspaceRequest{Workspace: ref(), Target: missing}))

	// Assert.
	if err != nil {
		t.Fatalf("MoveWorkspace: %v", err)
	}
	got := resp.Msg.GetError().GetUnknownWorkspace()
	if got == nil || got.GetWorkspace().GetId() != "ws-nope" {
		t.Fatalf("result = %v, want unknown_workspace naming ws-nope", resp.Msg.GetResult())
	}
	if len(h.Verbs.moves) != 0 {
		t.Fatalf("moves = %v, want none for an unknown workspace", h.Verbs.moves)
	}
}

func TestMoveWorkspaceAnswersTheVerbsRefusals(t *testing.T) {
	tests := []struct {
		name string
		arm  string
		set  func(*agentreplv1.MoveWorkspaceError) bool
	}{
		{name: "other repository", arm: workspace.ArmOtherRepository,
			set: func(e *agentreplv1.MoveWorkspaceError) bool { return e.GetOtherRepository() != nil }},
		{name: "not siblings", arm: workspace.ArmNotSiblings,
			set: func(e *agentreplv1.MoveWorkspaceError) bool { return e.GetNotSiblings() != nil }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h, target := swapHarness(t)
			h.Verbs.moveErr = &workspace.Refusal{Rpc: "MoveWorkspace", Arm: tt.arm, Reason: "no tab to take"}

			// Act.
			resp, err := h.Client.MoveWorkspace(context.Background(),
				connect.NewRequest(&agentreplv1.MoveWorkspaceRequest{Workspace: ref(), Target: target}))

			// Assert.
			if err != nil {
				t.Fatalf("MoveWorkspace: %v", err)
			}
			if !tt.set(resp.Msg.GetError()) {
				t.Fatalf("result = %v, want the %s arm", resp.Msg.GetResult(), tt.arm)
			}
		})
	}
}

func TestMoveWorkspaceFailsAVerbError(t *testing.T) {
	// Arrange.
	h, target := swapHarness(t)
	h.Verbs.moveErr = errors.New("disk I/O error")

	// Act.
	_, err := h.Client.MoveWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.MoveWorkspaceRequest{Workspace: ref(), Target: target}))

	// Assert.
	if connect.CodeOf(err) != connect.CodeInternal {
		t.Fatalf("MoveWorkspace = %v, want an internal failure", err)
	}
}

// ---- CreateWorkspace: a one-shot's context workspace -------------------------

// oneShotCreate builds a one-shot create request for repoRef naming the
// context workspace about, when it is non-nil.
func oneShotCreate(repoRef *workspacev1.RepositoryRef, about *workspacev1.WorkspaceRef) *agentreplv1.CreateWorkspaceRequest {
	oneShot := &agentreplv1.CreateWorkspaceOneShot{Prompt: workspace.SaidText("fix the feed"), Relationship: peerRelationship()}
	if about != nil {
		oneShot.Context = &agentreplv1.CreateWorkspaceOneShotContext{Workspace: about}
	}
	return &agentreplv1.CreateWorkspaceRequest{
		Repository:    repoRef,
		Form:          &agentreplv1.CreateWorkspaceRequest_OneShot{OneShot: oneShot},
		Requester:     userRequester(),
		ContextOrigin: freshContext(),
		Originator:    userOrWorkspaceOriginator(),
	}
}

func TestCreateWorkspaceMapsAOneShotContextToTheSpec(t *testing.T) {
	tests := []struct {
		name  string
		about *workspacev1.WorkspaceRef
		want  *ids.WorkspaceID
	}{
		{name: "a context names the resolved workspace", about: ref(), want: func() *ids.WorkspaceID { id := testWorkspaceID; return &id }()},
		{name: "no context names none", about: nil, want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			repoRef := registeredRepoRef(h)
			h.Verbs.createRec = wsm.Workspace{ID: "ws-created", Dir: "/tmp/ws", Name: "minted"}

			// Act.
			resp, err := h.Client.CreateWorkspace(context.Background(), connect.NewRequest(oneShotCreate(repoRef, tt.about)))

			// Assert.
			if err != nil || resp.Msg.GetSuccess() == nil {
				t.Fatalf("CreateWorkspace = (%v, %v), want success", resp, err)
			}
			spec := h.Verbs.createSpecs[0]
			if !spec.OneShot || spec.Parent != nil || spec.ForkFrom != nil {
				t.Fatalf("spec = one-shot %v, parent %v, fork from %v; want a top-level one-shot", spec.OneShot, spec.Parent, spec.ForkFrom)
			}
			if (spec.ContextWorkspace == nil) != (tt.want == nil) || (tt.want != nil && *spec.ContextWorkspace != *tt.want) {
				t.Fatalf("context workspace = %v, want %v", spec.ContextWorkspace, tt.want)
			}
		})
	}
}

func TestCreateWorkspaceFixesAOneShotsFinishPlacement(t *testing.T) {
	tests := []struct {
		name      string
		placement *agentreplv1.MergeQueuePlacement
		want      wsm.FinishPlacement
		// wantInvalid is whether the create is refused as invalid.
		wantInvalid bool
	}{
		{name: "none fixed lets the finish name its own", want: wsm.FinishPlacementAsked},
		{name: "the front is fixed", placement: frontPlacement(), want: wsm.FinishPlacementFront},
		{name: "the back is fixed", placement: backPlacement(), want: wsm.FinishPlacementBack},
		{name: "a placement behind a merge cannot be fixed", wantInvalid: true,
			placement: &agentreplv1.MergeQueuePlacement{Place: &agentreplv1.MergeQueuePlacement_Behind{Behind: &agentreplv1.MergeQueuePlacementBehind{Merge: "other"}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.Verbs.createRec = wsm.Workspace{ID: "ws-created", Dir: "/tmp/ws", Name: "minted"}
			req := oneShotCreate(registeredRepoRef(h), nil)
			req.GetOneShot().FinishPlacement = tt.placement

			// Act
			resp, err := h.Client.CreateWorkspace(context.Background(), connect.NewRequest(req))

			// Assert
			if tt.wantInvalid {
				if connect.CodeOf(err) != connect.CodeInvalidArgument || len(h.Verbs.createSpecs) != 0 {
					t.Fatalf("CreateWorkspace = (%v, %v), specs %d; want InvalidArgument and nothing created", resp, err, len(h.Verbs.createSpecs))
				}
				return
			}
			if err != nil || resp.Msg.GetSuccess() == nil || h.Verbs.createSpecs[0].FinishPlacement != tt.want {
				t.Fatalf("CreateWorkspace = (%v, %v), spec %+v; want placement %q", resp, err, h.Verbs.createSpecs, tt.want)
			}
		})
	}
}

func TestCreateWorkspaceMapsASlackOriginToTheSpec(t *testing.T) {
	const thread = "https://w.slack.com/archives/C1/p1730000000123456"
	tests := []struct {
		name   string
		origin *agentreplv1.CreateWorkspaceSlackOrigin
		want   string
	}{
		{name: "an origin names its thread", origin: &agentreplv1.CreateWorkspaceSlackOrigin{ThreadUrl: thread}, want: thread},
		{name: "no origin names none", origin: nil, want: ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Verbs.createRec = wsm.Workspace{ID: "ws-created", Dir: "/tmp/ws", Name: "minted"}
			req := oneShotCreate(registeredRepoRef(h), nil)
			req.SlackOrigin = tt.origin

			// Act.
			resp, err := h.Client.CreateWorkspace(context.Background(), connect.NewRequest(req))

			// Assert.
			if err != nil || resp.Msg.GetSuccess() == nil {
				t.Fatalf("CreateWorkspace = (%v, %v), want success", resp, err)
			}
			if got := h.Verbs.createSpecs[0].SlackThread; got != tt.want {
				t.Fatalf("spec slack thread = %q, want %q", got, tt.want)
			}
		})
	}
}

// TestCreateWorkspaceCarriesTheContextWorkspacesSelection pins that a
// one-shot naming a context workspace carries the bubble that workspace's
// feed has selected as the request arrives: its kind, row and text, or none
// when nothing is selected.
func TestCreateWorkspaceCarriesTheContextWorkspacesSelection(t *testing.T) {
	tests := []struct {
		name  string
		click string
		want  *workspace.ContextSelection
	}{
		{name: "nothing selected carries none", click: "", want: nil},
		{name: "a final response", click: "final-1",
			want: &workspace.ContextSelection{Kind: workspace.ContextSelectionResponse, Row: "final-1", Markdown: "the answer"}},
		{name: "a rollback prompt", click: "prompt-1",
			want: &workspace.ContextSelection{Kind: workspace.ContextSelectionPrompt, Row: "prompt-1", Markdown: "the ask"}},
		{name: "any other bubble", click: "interim-1",
			want: &workspace.ContextSelection{Kind: workspace.ContextSelectionBubble, Row: "interim-1", Markdown: "meanwhile"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			repoRef := registeredRepoRef(h)
			h.Verbs.createRec = wsm.Workspace{ID: "ws-created", Dir: "/tmp/ws", Name: "minted"}
			h.Feed.finals = feedIDs("final-1")
			h.Feed.prompts = feedIDs("prompt-1")
			h.Feed.markdown = map[string]string{"final-1": "the answer", "prompt-1": "the ask", "interim-1": "meanwhile"}
			if tt.click != "" {
				clickRow(t, h, tt.click)
			}

			// Act.
			resp, err := h.Client.CreateWorkspace(context.Background(), connect.NewRequest(oneShotCreate(repoRef, ref())))

			// Assert.
			if err != nil || resp.Msg.GetSuccess() == nil {
				t.Fatalf("CreateWorkspace = (%v, %v), want success", resp, err)
			}
			got := h.Verbs.createSpecs[0].ContextSelection
			if (got == nil) != (tt.want == nil) || (got != nil && *got != *tt.want) {
				t.Fatalf("context selection = %+v, want %+v", got, tt.want)
			}
		})
	}
}

func TestCreateWorkspaceCarriesNoSelectionWithoutAContextWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	repoRef := registeredRepoRef(h)
	h.Verbs.createRec = wsm.Workspace{ID: "ws-created", Dir: "/tmp/ws", Name: "minted"}
	h.Feed.finals = feedIDs("final-1")
	h.Feed.markdown = map[string]string{"final-1": "the answer"}
	clickRow(t, h, "final-1")

	// Act.
	resp, err := h.Client.CreateWorkspace(context.Background(), connect.NewRequest(oneShotCreate(repoRef, nil)))

	// Assert.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace = (%v, %v), want success", resp, err)
	}
	if got := h.Verbs.createSpecs[0].ContextSelection; got != nil {
		t.Fatalf("context selection = %+v, want none", got)
	}
}

func TestCreateWorkspaceNamesAnUnknownContextWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	repoRef := registeredRepoRef(h)

	// Act.
	resp, err := h.Client.CreateWorkspace(context.Background(),
		connect.NewRequest(oneShotCreate(repoRef, &workspacev1.WorkspaceRef{Id: "ws-nope"})))

	// Assert.
	if err != nil {
		t.Fatalf("CreateWorkspace: %v", err)
	}
	got := resp.Msg.GetError().GetUnknownContextWorkspace()
	if got == nil || got.GetWorkspace().GetId() != "ws-nope" {
		t.Fatalf("result = %v, want unknown_context_workspace naming ws-nope", resp.Msg.GetResult())
	}
	if len(h.Verbs.createSpecs) != 0 {
		t.Fatalf("Create ran %d times, want none", len(h.Verbs.createSpecs))
	}
}

// versioned is a finish request carrying INCREMENT.
func versioned(increment *agentreplv1.MergeWorkspaceVersionIncrement) *agentreplv1.FinishWorkspaceRequest {
	req := ownFinish()
	req.VersionIncrement = increment
	return req
}

func TestFinishWorkspaceMapsTheVersionIncrement(t *testing.T) {
	tests := []struct {
		name      string
		increment *agentreplv1.MergeWorkspaceVersionIncrement
		want      *wsm.VersionIncrement
	}{
		{name: "a minor", increment: &agentreplv1.MergeWorkspaceVersionIncrement{
			Increment: &agentreplv1.MergeWorkspaceVersionIncrement_Minor{Minor: &agentreplv1.MergeWorkspaceVersionIncrementMinor{}}, ChangelogLine: "Merges record a version"},
			want: &wsm.VersionIncrement{Part: semver.Minor, ChangelogLine: "Merges record a version"}},
		{name: "a patch", increment: &agentreplv1.MergeWorkspaceVersionIncrement{
			Increment: &agentreplv1.MergeWorkspaceVersionIncrement_Patch{Patch: &agentreplv1.MergeWorkspaceVersionIncrementPatch{}}, ChangelogLine: "Fix the footer"},
			want: &wsm.VersionIncrement{Part: semver.Patch, ChangelogLine: "Fix the footer"}},
		{name: "none", increment: nil, want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			if _, err := h.Client.FinishWorkspace(context.Background(), connect.NewRequest(versioned(tt.increment))); err != nil {
				t.Fatalf("FinishWorkspace: %v", err)
			}

			// Assert
			if len(h.Merge.requests) != 1 {
				t.Fatalf("requests = %+v, want one", h.Merge.requests)
			}
			got := h.Merge.requests[0].Source.Version
			if (got == nil) != (tt.want == nil) || (got != nil && *got != *tt.want) {
				t.Fatalf("version = %+v, want %+v", got, tt.want)
			}
		})
	}
}

func TestFinishWorkspaceRefusesAMalformedVersionIncrementAsInvalid(t *testing.T) {
	patch := func(line string) *agentreplv1.MergeWorkspaceVersionIncrement {
		return &agentreplv1.MergeWorkspaceVersionIncrement{
			Increment: &agentreplv1.MergeWorkspaceVersionIncrement_Patch{Patch: &agentreplv1.MergeWorkspaceVersionIncrementPatch{}}, ChangelogLine: line}
	}
	tests := []struct {
		name      string
		increment *agentreplv1.MergeWorkspaceVersionIncrement
	}{
		{name: "no increment arm", increment: &agentreplv1.MergeWorkspaceVersionIncrement{ChangelogLine: "x"}},
		{name: "a blank changelog line", increment: patch(" ")},
		{name: "a changelog line of two lines", increment: patch("a\nb")},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			_, err := h.Client.FinishWorkspace(context.Background(), connect.NewRequest(versioned(tt.increment)))

			// Assert
			if connect.CodeOf(err) != connect.CodeInvalidArgument {
				t.Fatalf("FinishWorkspace = %v, want invalid_argument", err)
			}
			if len(h.Merge.requests) != 0 {
				t.Fatalf("requests = %+v, want none", h.Merge.requests)
			}
		})
	}
}

func TestFinishWorkspaceAnswersTheVersionRefusalsOnTheirArms(t *testing.T) {
	tests := []struct {
		name  string
		err   *merge.RefusalError
		check func(*agentreplv1.FinishWorkspaceError) bool
	}{
		{name: "a missing increment", err: &merge.RefusalError{Arm: merge.ArmVersionIncrementRequired, Workspace: testWorkspaceID, Reason: "required"},
			check: func(e *agentreplv1.FinishWorkspaceError) bool { return e.GetVersionIncrementRequired() != nil }},
		{name: "an increment in an unversioned repository", err: &merge.RefusalError{Arm: merge.ArmVersionIncrementUnexpected, Workspace: testWorkspaceID, Reason: "unversioned",
			Fields: map[string]any{"reason": refusalarm.Nested("unversioned_repository")}},
			check: func(e *agentreplv1.FinishWorkspaceError) bool {
				return e.GetVersionIncrementUnexpected().GetUnversionedRepository() != nil
			}},
		{name: "an increment off the default branch", err: &merge.RefusalError{Arm: merge.ArmVersionIncrementUnexpected, Workspace: testWorkspaceID, Reason: "parent",
			Fields: map[string]any{"reason": refusalarm.Nested("not_default_branch")}},
			check: func(e *agentreplv1.FinishWorkspaceError) bool {
				return e.GetVersionIncrementUnexpected().GetNotDefaultBranch() != nil
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.Merge.enqueueErr = tt.err

			// Act
			resp, err := h.Client.FinishWorkspace(context.Background(), connect.NewRequest(ownFinish()))

			// Assert
			if err != nil || !tt.check(resp.Msg.GetError()) {
				t.Fatalf("FinishWorkspace = (%v, %v), want the %s arm", resp.Msg.GetResult(), err, tt.err.Arm)
			}
		})
	}
}

// TestOpenWorkspaceRoutesTheOpenItAsks pins which verb an open reaches: a
// request carrying revert_merge reaches OpenToRevertMerge alone, and one
// without it reaches Open alone.
func TestOpenWorkspaceRoutesTheOpenItAsks(t *testing.T) {
	tests := []struct {
		name        string
		revert      *agentreplv1.OpenWorkspaceRevertMerge
		wantOpens   int32
		wantReverts int32
	}{
		{name: "an ordinary open", revert: nil, wantOpens: 1, wantReverts: 0},
		{name: "an open that reverts the merge", revert: &agentreplv1.OpenWorkspaceRevertMerge{}, wantOpens: 0, wantReverts: 1},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			resp, err := h.Client.OpenWorkspace(context.Background(),
				connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: ref(), RevertMerge: tc.revert}))

			// Assert.
			if err != nil || resp.Msg.GetSuccess() == nil {
				t.Fatalf("OpenWorkspace = (%v, %v), want success", resp, err)
			}
			if got := h.Verbs.opens.Load(); got != tc.wantOpens {
				t.Fatalf("opens = %d, want %d", got, tc.wantOpens)
			}
			if got := h.Verbs.revertOpens.Load(); got != tc.wantReverts {
				t.Fatalf("revert opens = %d, want %d", got, tc.wantReverts)
			}
		})
	}
}

// TestOpenWorkspaceAnswersTheRevertRefusalsByTheirArms pins that the revert's
// own refusals reach the client as their typed arms.
func TestOpenWorkspaceAnswersTheRevertRefusalsByTheirArms(t *testing.T) {
	tests := []struct {
		name  string
		arm   string
		check func(*agentreplv1.OpenWorkspaceError) bool
	}{
		{name: "not merged", arm: workspace.ArmNotMerged, check: func(e *agentreplv1.OpenWorkspaceError) bool {
			return e.GetNotMerged() != nil
		}},
		{name: "brief missing", arm: workspace.ArmBriefMissing, check: func(e *agentreplv1.OpenWorkspaceError) bool {
			return e.GetBriefMissing().GetDetail() == "the brief is gone"
		}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Verbs.revertErr = &workspace.Refusal{Arm: tc.arm, Reason: "the brief is gone"}

			// Act.
			resp, err := h.Client.OpenWorkspace(context.Background(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{
				Workspace: ref(), RevertMerge: &agentreplv1.OpenWorkspaceRevertMerge{}}))

			// Assert.
			if err != nil || !tc.check(resp.Msg.GetError()) {
				t.Fatalf("OpenWorkspace = (%v, %v), want the %s arm", resp, err, tc.arm)
			}
		})
	}
}

func TestOpenWorkspaceIsTimedOnlyWhenTheSessionComesUp(t *testing.T) {
	tests := []struct {
		name    string
		openErr error
		want    int
	}{
		{name: "a successful open is one workspace_open", want: 1},
		{name: "a refused open is none", openErr: errors.New("the worktree is gone"), want: 0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Verbs.openErr = tt.openErr

			// Act.
			_, _ = h.Client.OpenWorkspace(context.Background(),
				connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: ref()}))

			// Assert.
			opens := 0
			for _, l := range h.Latencies.all() {
				if l.kind == "workspace_open" && l.workspace == testWorkspaceID {
					opens++
				}
			}
			if opens != tt.want {
				t.Fatalf("workspace_open latencies = %d (%+v), want %d", opens, h.Latencies.all(), tt.want)
			}
		})
	}
}

// A CREATE THE GATE WILL NOT ADMIT IS REFUSED BEFORE ANYTHING RUNS, with the
// arm the stand-down's kind calls for.

func TestCreateWorkspaceRefusesWhileTheGateIsClosed(t *testing.T) {
	cases := []struct {
		name      string
		successor string
		check     func(t *testing.T, e *agentreplv1.CreateWorkspaceError)
	}{
		{name: "a handover names its successor", successor: "127.0.0.1:4242", check: func(t *testing.T, e *agentreplv1.CreateWorkspaceError) {
			if got := e.GetTransferringAway().GetAddress(); got != "127.0.0.1:4242" {
				t.Fatalf("error = %v, want transferring_away to 127.0.0.1:4242", e)
			}
		}},
		{name: "a stand-down with no successor names none", check: func(t *testing.T, e *agentreplv1.CreateWorkspaceError) {
			if e.GetStandingDown() == nil {
				t.Fatalf("error = %v, want standing_down", e)
			}
		}},
	}
	for _, tc := range cases {
		for _, opID := range []string{"", "op-refused"} {
			t.Run(fmt.Sprintf("%s/op_id=%q", tc.name, opID), func(t *testing.T) {
				// Arrange.
				h := newHarness(t)
				repoRef := registeredRepoRef(h)
				h.Creates.Close(tc.successor)

				// Act.
				resp, err := h.Client.CreateWorkspace(context.Background(), connect.NewRequest(standardCreate(repoRef, opID)))

				// Assert.
				if err != nil {
					t.Fatalf("CreateWorkspace: %v", err)
				}
				tc.check(t, resp.Msg.GetError())
				if len(h.Verbs.createSpecs) != 0 {
					t.Fatalf("the verb ran %d creates, want none", len(h.Verbs.createSpecs))
				}
			})
		}
	}
}

func TestCreateWorkspaceHoldsItsTicketUntilTheOutcomeIsPublished(t *testing.T) {
	// Arrange: a create held open after its ack.
	h := newHarness(t)
	repoRef := registeredRepoRef(h)
	h.Verbs.createRec = wsm.Workspace{ID: "ws-created", Dir: "/tmp/ws", Name: "minted"}
	h.Verbs.createRelease = make(chan struct{})
	if _, err := h.Client.CreateWorkspace(context.Background(), connect.NewRequest(standardCreate(repoRef, "op-held"))); err != nil {
		t.Fatalf("CreateWorkspace: %v", err)
	}
	idle := h.Creates.Idle()
	if h.Creates.InFlight() != 1 {
		t.Fatalf("in flight after the ack = %d, want 1", h.Creates.InFlight())
	}

	// Act.
	close(h.Verbs.createRelease)
	<-idle

	// Assert.
	if h.Creates.InFlight() != 0 {
		t.Fatalf("in flight after the create = %d, want 0", h.Creates.InFlight())
	}
}

func TestCreateWorkspaceReleasesASynchronousTicketWithItsAnswer(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	repoRef := registeredRepoRef(h)
	h.Verbs.createRec = wsm.Workspace{ID: "ws-created", Dir: "/tmp/ws", Name: "minted"}

	// Act.
	if _, err := h.Client.CreateWorkspace(context.Background(), connect.NewRequest(standardCreate(repoRef, ""))); err != nil {
		t.Fatalf("CreateWorkspace: %v", err)
	}

	// Assert.
	if h.Creates.InFlight() != 0 {
		t.Fatalf("in flight after the answer = %d, want 0", h.Creates.InFlight())
	}
}

// behindPlacement places a merge directly behind MERGE.
func behindPlacement(merge string) *agentreplv1.MergeQueuePlacement {
	return &agentreplv1.MergeQueuePlacement{Place: &agentreplv1.MergeQueuePlacement_Behind{Behind: &agentreplv1.MergeQueuePlacementBehind{Merge: merge}}}
}

func TestFinishWorkspaceCarriesABehindPlacement(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	req := ownFinish()
	req.Placement = behindPlacement(" two ")

	// Act.
	if _, err := h.Client.FinishWorkspace(context.Background(), connect.NewRequest(req)); err != nil {
		t.Fatalf("FinishWorkspace: %v", err)
	}

	// Assert.
	if len(h.Merge.requests) != 1 || h.Merge.requests[0].PlaceBehind != "two" || h.Merge.requests[0].Source.Front {
		t.Fatalf("requests = %+v, want one placed behind two", h.Merge.requests)
	}
}

func TestFinishWorkspaceCarriesWhetherItsGateProfiles(t *testing.T) {
	for _, asked := range []bool{true, false} {
		t.Run(map[bool]string{true: "asked", false: "not asked"}[asked], func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			req := ownFinish()
			req.ProfileGate = asked

			// Act.
			if _, err := h.Client.FinishWorkspace(context.Background(), connect.NewRequest(req)); err != nil {
				t.Fatalf("FinishWorkspace: %v", err)
			}

			// Assert.
			if len(h.Merge.requests) != 1 || h.Merge.requests[0].Source.ProfileGate != asked {
				t.Fatalf("requests = %+v, want one with ProfileGate %v", h.Merge.requests, asked)
			}
		})
	}
}

func TestFinishWorkspaceRefusesABehindPlacementNamingNoMerge(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	req := ownFinish()
	req.Placement = behindPlacement(" ")

	// Act.
	_, err := h.Client.FinishWorkspace(context.Background(), connect.NewRequest(req))

	// Assert.
	if connect.CodeOf(err) != connect.CodeInvalidArgument || !strings.Contains(err.Error(), "placement.behind.merge") || len(h.Merge.requests) != 0 {
		t.Fatalf("FinishWorkspace = %v, requests %+v; want the empty merge refused as invalid", err, h.Merge.requests)
	}
}

// ---- ValidateWorkspaceName -------------------------------------------------

// TestValidateWorkspaceNameJudgesByTheNamingRule pins each verdict: a legal
// name answers legal, and a breach answers the create's invalid_name shape,
// naming the component and its rule, each recorded at DEBUG.
func TestValidateWorkspaceNameJudgesByTheNamingRule(t *testing.T) {
	tests := []struct {
		name      string
		supplied  string
		legal     bool
		trimmed   string
		component string
		rule      func(*agentreplv1.CreateWorkspaceInvalidName) bool
		message   string
	}{
		{name: "a legal slug", supplied: " fix-login ", legal: true, trimmed: "fix-login", message: "the supplied name keeps the naming rule"},
		{name: "a legal prefixed slug", supplied: "ABC/fix-login", legal: true, trimmed: "ABC/fix-login", message: "the supplied name keeps the naming rule"},
		{
			name: "a paragraph breaks the slug rule", supplied: "Please fix the login page.", trimmed: "Please fix the login page.",
			component: "Please fix the login page.", rule: func(m *agentreplv1.CreateWorkspaceInvalidName) bool { return m.GetSlug() != nil },
			message: "the supplied name breaks the naming rule",
		},
		{
			name: "a bad prefix breaks the prefix rule", supplied: "A B/fix-login", trimmed: "A B/fix-login",
			component: "A B", rule: func(m *agentreplv1.CreateWorkspaceInvalidName) bool { return m.GetPrefix() != nil },
			message: "the supplied name breaks the naming rule",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			log := dlog.NewTestLogger()
			h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })

			// Act
			resp, err := h.Client.ValidateWorkspaceName(context.Background(),
				connect.NewRequest(&agentreplv1.ValidateWorkspaceNameRequest{Name: tt.supplied}))

			// Assert
			if err != nil {
				t.Fatalf("ValidateWorkspaceName: %v", err)
			}
			success := resp.Msg.GetSuccess()
			if tt.legal {
				if success.GetLegal() == nil {
					t.Fatalf("result = %v, want legal", resp.Msg.GetResult())
				}
			} else {
				invalid := success.GetInvalid()
				if invalid.GetName() != tt.trimmed || invalid.GetComponent() != tt.component || !tt.rule(invalid) {
					t.Fatalf("result = %v, want invalid %q at %q", resp.Msg.GetResult(), tt.trimmed, tt.component)
				}
			}
			r, found := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelDebug, Operation: "daemon.server.validate_workspace_name", Message: tt.message})
			if !found || r.Context["name"] != tt.trimmed {
				t.Fatalf("record = %+v, want DEBUG %q naming %q", r, tt.message, tt.trimmed)
			}
			if !tt.legal && r.Context["component"] != tt.component {
				t.Fatalf("record context = %v, want component %q", r.Context, tt.component)
			}
		})
	}
}

// TestValidateWorkspaceNameRefusesABlankName pins a blank name as a malformed
// request, never a verdict.
func TestValidateWorkspaceNameRefusesABlankName(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	_, err := h.Client.ValidateWorkspaceName(context.Background(),
		connect.NewRequest(&agentreplv1.ValidateWorkspaceNameRequest{Name: "  "}))

	// Assert
	if connect.CodeOf(err) != connect.CodeInvalidArgument {
		t.Fatalf("ValidateWorkspaceName err = %v, want InvalidArgument", err)
	}
}

// ---- ListCreationModels ----------------------------------------------------

// modelOption is a served catalog option naming NAME.
func modelOption(name string) *conversationv1.ModelOption {
	return &conversationv1.ModelOption{Model: &conversationv1.AgentModel{Name: name}, DisplayName: name}
}

func TestListCreationModelsAnswersTheCreatingLoginsCatalog(t *testing.T) {
	tests := []struct {
		name      string
		catalog   workspace.CreationCatalog
		wantKnown []string
	}{
		{name: "a known catalog is served in order", catalog: workspace.CreationCatalog{ConfigDir: "/home/.claude", Known: true, Options: []*conversationv1.ModelOption{modelOption("opus"), modelOption("sonnet")}}, wantKnown: []string{"opus", "sonnet"}},
		{name: "a login with no catalog is answered as unknown", catalog: workspace.CreationCatalog{ConfigDir: "/home/.claude"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			ref := registeredRepoRef(h)
			h.Verbs.creationCatalog = tt.catalog

			// Act.
			resp, err := h.Client.ListCreationModels(context.Background(),
				connect.NewRequest(&agentreplv1.ListCreationModelsRequest{Repository: ref}))

			// Assert.
			if err != nil {
				t.Fatalf("ListCreationModels: %v", err)
			}
			success := resp.Msg.GetSuccess()
			if success.GetConfigDir() != "/home/.claude" {
				t.Fatalf("config_dir = %q, want the login", success.GetConfigDir())
			}
			if len(h.Verbs.creationCatalogDirs) != 1 || h.Verbs.creationCatalogDirs[0] != "/tmp/agent-repl-test-repo" {
				t.Fatalf("asked about %v, want the registered repository's dir", h.Verbs.creationCatalogDirs)
			}
			if tt.wantKnown == nil {
				if success.GetUnknown() == nil {
					t.Fatalf("catalog = %v, want unknown", success.GetCatalog())
				}
				return
			}
			var names []string
			for _, option := range success.GetKnown().GetOptions() {
				names = append(names, option.GetModel().GetName())
			}
			if !slices.Equal(names, tt.wantKnown) {
				t.Fatalf("options = %v, want %v", names, tt.wantKnown)
			}
		})
	}
}

func TestListCreationModelsRefusesAnUnknownRepository(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.ListCreationModels(context.Background(),
		connect.NewRequest(&agentreplv1.ListCreationModelsRequest{Repository: &workspacev1.RepositoryRef{Id: "repo-nope"}}))

	// Assert.
	if err != nil {
		t.Fatalf("ListCreationModels: %v", err)
	}
	if resp.Msg.GetError().GetUnknownRepository() == nil {
		t.Fatalf("result = %v, want unknown_repository", resp.Msg.GetResult())
	}
	if len(h.Verbs.creationCatalogDirs) != 0 {
		t.Fatalf("asked about %v, want no catalog read", h.Verbs.creationCatalogDirs)
	}
}

func TestListCreationModelsRefusesAMissingRepository(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.ListCreationModels(context.Background(),
		connect.NewRequest(&agentreplv1.ListCreationModelsRequest{}))

	// Assert.
	if connect.CodeOf(err) != connect.CodeInvalidArgument {
		t.Fatalf("ListCreationModels err = %v, want InvalidArgument", err)
	}
}

func TestListCreationModelsFailsWhenTheCatalogCannotBeRead(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })
	ref := registeredRepoRef(h)
	h.Verbs.creationCatalogErr = errors.New("disk on fire")

	// Act.
	_, err := h.Client.ListCreationModels(context.Background(),
		connect.NewRequest(&agentreplv1.ListCreationModelsRequest{Repository: ref}))

	// Assert.
	if connect.CodeOf(err) != connect.CodeInternal {
		t.Fatalf("ListCreationModels err = %v, want Internal", err)
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelError}); !ok {
		t.Fatalf("records = %+v, want the failure at ERROR", log.Records())
	}
}

func TestCreateWorkspaceCarriesTheModelRefusalsOnTheirArms(t *testing.T) {
	tests := []struct {
		name   string
		refuse *workspace.Refusal
		check  func(*agentreplv1.CreateWorkspaceError) bool
	}{
		{
			name: "model_not_in_catalog carries the model, the login and the catalog",
			refuse: &workspace.Refusal{Rpc: "CreateWorkspace", Arm: workspace.ArmModelNotInCatalog, Reason: "not offered",
				Fields: map[string]any{"model": "gpt-9", "config_dir": "/home/.claude", "options": []proto.Message{modelOption("opus")}}},
			check: func(e *agentreplv1.CreateWorkspaceError) bool {
				arm := e.GetModelNotInCatalog()
				return arm.GetModel() == "gpt-9" && arm.GetConfigDir() == "/home/.claude" &&
					len(arm.GetOptions()) == 1 && arm.GetOptions()[0].GetModel().GetName() == "opus"
			},
		},
		{
			name: "model_catalog_unknown carries the model and the login",
			refuse: &workspace.Refusal{Rpc: "CreateWorkspace", Arm: workspace.ArmModelCatalogUnknown, Reason: "no catalog",
				Fields: map[string]any{"model": "opus", "config_dir": "/home/.claude"}},
			check: func(e *agentreplv1.CreateWorkspaceError) bool {
				arm := e.GetModelCatalogUnknown()
				return arm.GetModel() == "opus" && arm.GetConfigDir() == "/home/.claude"
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			ref := registeredRepoRef(h)
			h.Verbs.createErr = tt.refuse

			// Act.
			resp, err := h.Client.CreateWorkspace(context.Background(), connect.NewRequest(standardCreate(ref, "")))

			// Assert.
			if err != nil {
				t.Fatalf("CreateWorkspace: %v", err)
			}
			if !tt.check(resp.Msg.GetError()) {
				t.Fatalf("result = %v, want the %s arm with its fields", resp.Msg.GetResult(), tt.refuse.Arm)
			}
		})
	}
}

// ---- SetWorkspaceSummary ---------------------------------------------------

// footerLong is a long summary every handler test sends.
const footerLong = "The footer stalls at a cold gate. Fix its detail."

// TestSetWorkspaceSummaryHandsTheSummaryToTheVerb pins both parts reaching
// the verb, and the verb's answer coming back as the success.
func TestSetWorkspaceSummaryHandsTheSummaryToTheVerb(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	resp, err := h.Client.SetWorkspaceSummary(context.Background(),
		connect.NewRequest(&agentreplv1.SetWorkspaceSummaryRequest{Workspace: ref(), LongSummary: footerLong, ShortSummary: "Fix the footer"}))

	// Assert
	if err != nil {
		t.Fatalf("SetWorkspaceSummary: %v", err)
	}
	if got := resp.Msg.GetSuccess(); got.GetShortSummary() != "Fix the footer" || got.GetLongSummary() != footerLong {
		t.Fatalf("result = %v, want success naming both parts", resp.Msg.GetResult())
	}
	want := []wsm.WorkspaceSummary{{Long: footerLong, Short: "Fix the footer"}}
	if !slices.Equal(h.Verbs.summaries, want) {
		t.Fatalf("summaries = %+v, want %+v", h.Verbs.summaries, want)
	}
}

// TestSetWorkspaceSummaryRefusesAnUnknownWorkspace pins the unknown_workspace
// arm, with no verb reached.
func TestSetWorkspaceSummaryRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	resp, err := h.Client.SetWorkspaceSummary(context.Background(),
		connect.NewRequest(&agentreplv1.SetWorkspaceSummaryRequest{Workspace: &workspacev1.WorkspaceRef{Id: "ws-nope"}, LongSummary: footerLong, ShortSummary: "Fix the footer"}))

	// Assert
	if err != nil {
		t.Fatalf("SetWorkspaceSummary: %v", err)
	}
	if resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("result = %v, want unknown_workspace", resp.Msg.GetResult())
	}
	if len(h.Verbs.summaries) != 0 {
		t.Fatalf("summaries = %+v, want none for an unknown workspace", h.Verbs.summaries)
	}
}

// TestSetWorkspaceSummaryMapsAnInvalidSummary pins the verb's invalid_summary
// refusal reaching its arm, with the refused part, its text and the reason.
func TestSetWorkspaceSummaryMapsAnInvalidSummary(t *testing.T) {
	tests := []struct {
		name    string
		refusal *workspace.Refusal
		check   func(*agentreplv1.SetWorkspaceSummaryInvalidSummary) bool
	}{
		{name: "the long summary", refusal: &workspace.Refusal{Arm: workspace.ArmInvalidSummary, Reason: "one sentence",
			Fields: map[string]any{"part": refusalarm.Nested("long_summary"), "long_summary": "Fix it.", "reason": "a long summary is 2 to 4 sentences, not 1"}},
			check: func(e *agentreplv1.SetWorkspaceSummaryInvalidSummary) bool {
				got := e.GetLongSummary()
				return got.GetLongSummary() == "Fix it." && got.GetReason() == "a long summary is 2 to 4 sentences, not 1"
			}},
		{name: "the short summary", refusal: &workspace.Refusal{Arm: workspace.ArmInvalidSummary, Reason: "two clauses",
			Fields: map[string]any{"part": refusalarm.Nested("short_summary"), "short_summary": "Fix it, then that", "reason": "a short summary carries no comma"}},
			check: func(e *agentreplv1.SetWorkspaceSummaryInvalidSummary) bool {
				got := e.GetShortSummary()
				return got.GetShortSummary() == "Fix it, then that" && got.GetReason() == "a short summary carries no comma"
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.Verbs.setSummaryErr = tt.refusal

			// Act
			resp, err := h.Client.SetWorkspaceSummary(context.Background(),
				connect.NewRequest(&agentreplv1.SetWorkspaceSummaryRequest{Workspace: ref(), LongSummary: footerLong, ShortSummary: "Fix it, then that"}))

			// Assert
			if err != nil {
				t.Fatalf("SetWorkspaceSummary: %v", err)
			}
			if !tt.check(resp.Msg.GetError().GetInvalidSummary()) {
				t.Fatalf("result = %v, want invalid_summary naming the part with its evidence", resp.Msg.GetResult())
			}
		})
	}
}

// ---- SetWorkspaceName ------------------------------------------------------

// TestSetWorkspaceNameHandsTheNameToTheVerb pins the requested name reaching
// the verb, and the verb's answered name coming back as the success.
func TestSetWorkspaceNameHandsTheNameToTheVerb(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	resp, err := h.Client.SetWorkspaceName(context.Background(),
		connect.NewRequest(&agentreplv1.SetWorkspaceNameRequest{Workspace: ref(), Name: "better-name"}))

	// Assert
	if err != nil {
		t.Fatalf("SetWorkspaceName: %v", err)
	}
	if got := resp.Msg.GetSuccess().GetName(); got != "better-name" {
		t.Fatalf("result = %v, want success naming better-name", resp.Msg.GetResult())
	}
	if len(h.Verbs.names) != 1 || h.Verbs.names[0] != "better-name" {
		t.Fatalf("names = %q, want [better-name]", h.Verbs.names)
	}
}

// TestSetWorkspaceNameAnswersWhereTheMoveStands pins each of the verb's move
// states reaching its arm of SetWorkspaceNameSuccess.move.
func TestSetWorkspaceNameAnswersWhereTheMoveStands(t *testing.T) {
	to := wsm.MoveTarget{Branch: "better-name", Dir: "/trees/better-name"}
	tests := []struct {
		name    string
		renamed workspace.Renamed
		check   func(*agentreplv1.SetWorkspaceNameMove) bool
	}{
		{name: "nothing moves", renamed: workspace.Renamed{Move: workspace.MoveNone},
			check: func(m *agentreplv1.SetWorkspaceNameMove) bool { return m.GetNone() != nil }},
		{name: "the move is owed", renamed: workspace.Renamed{Move: workspace.MoveOwed, To: to},
			check: func(m *agentreplv1.SetWorkspaceNameMove) bool {
				return m.GetOwed().GetBranch() == to.Branch && m.GetOwed().GetDir() == to.Dir
			}},
		{name: "the move ran", renamed: workspace.Renamed{Move: workspace.MoveMoved, To: to},
			check: func(m *agentreplv1.SetWorkspaceNameMove) bool {
				return m.GetMoved().GetBranch() == to.Branch && m.GetMoved().GetDir() == to.Dir
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.Verbs.renamed = tt.renamed

			// Act
			resp, err := h.Client.SetWorkspaceName(context.Background(),
				connect.NewRequest(&agentreplv1.SetWorkspaceNameRequest{Workspace: ref(), Name: "better-name"}))

			// Assert
			if err != nil {
				t.Fatalf("SetWorkspaceName: %v", err)
			}
			if !tt.check(resp.Msg.GetSuccess().GetMove()) {
				t.Fatalf("move = %v, want the verb's %v", resp.Msg.GetSuccess().GetMove(), tt.renamed)
			}
		})
	}
}

// TestSetWorkspaceNameRefusesAnUnknownWorkspace pins the unknown_workspace
// arm, with no verb reached.
func TestSetWorkspaceNameRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	resp, err := h.Client.SetWorkspaceName(context.Background(),
		connect.NewRequest(&agentreplv1.SetWorkspaceNameRequest{Workspace: &workspacev1.WorkspaceRef{Id: "ws-nope"}, Name: "better-name"}))

	// Assert
	if err != nil {
		t.Fatalf("SetWorkspaceName: %v", err)
	}
	if resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("result = %v, want unknown_workspace", resp.Msg.GetResult())
	}
	if len(h.Verbs.names) != 0 {
		t.Fatalf("names = %q, want none for an unknown workspace", h.Verbs.names)
	}
}

// TestSetWorkspaceNameMapsTheVerbsRefusals pins each of the verb's own
// refusals reaching its arm with its evidence, nested arms included.
func TestSetWorkspaceNameMapsTheVerbsRefusals(t *testing.T) {
	tests := []struct {
		name    string
		refusal *workspace.Refusal
		check   func(*agentreplv1.SetWorkspaceNameError) bool
	}{
		{name: "invalid_name, with its rule", refusal: &workspace.Refusal{Arm: workspace.ArmInvalidName, Reason: "bad slug",
			Fields: map[string]any{"name": "Bad Name", "component": "Bad Name", "rule": workspace.NameRuleSlug}},
			check: func(e *agentreplv1.SetWorkspaceNameError) bool {
				got := e.GetInvalidName()
				return got.GetName() == "Bad Name" && got.GetComponent() == "Bad Name" && got.GetSlug() != nil
			}},
		{name: "name_taken by a workspace", refusal: &workspace.Refusal{Arm: workspace.ArmNameTaken, Reason: "held",
			Fields: map[string]any{"name": "better-name", "workspace": &workspacev1.WorkspaceRef{Id: "ws-holder", Dir: "/tmp/holder"}}},
			check: func(e *agentreplv1.SetWorkspaceNameError) bool {
				got := e.GetNameTaken()
				return got.GetName() == "better-name" && got.GetWorkspace().GetId() == "ws-holder" && got.GetWorkspace().GetDir() == "/tmp/holder"
			}},
		{name: "name_taken by a pending reservation", refusal: &workspace.Refusal{Arm: workspace.ArmNameTaken, Reason: "reserved",
			Fields: map[string]any{"name": "better-name", "holder": refusalarm.Nested("pending")}},
			check: func(e *agentreplv1.SetWorkspaceNameError) bool {
				got := e.GetNameTaken()
				return got.GetName() == "better-name" && got.GetPending() != nil
			}},
		{name: "merging", refusal: &workspace.Refusal{Arm: workspace.ArmMerging, Reason: "merging"},
			check: func(e *agentreplv1.SetWorkspaceNameError) bool { return e.GetMerging() != nil }},
		{name: "branch_published, with its upstream", refusal: &workspace.Refusal{Arm: workspace.ArmBranchPublished, Reason: "tracks",
			Fields: map[string]any{"upstream": "origin/feature"}},
			check: func(e *agentreplv1.SetWorkspaceNameError) bool {
				return e.GetBranchPublished().GetUpstream() == "origin/feature"
			}},
		{name: "branch_exists, with its branch", refusal: &workspace.Refusal{Arm: workspace.ArmBranchExists, Reason: "exists",
			Fields: map[string]any{"branch": "better-name"}},
			check: func(e *agentreplv1.SetWorkspaceNameError) bool {
				return e.GetBranchExists().GetBranch() == "better-name"
			}},
		{name: "directory_exists, with its dir", refusal: &workspace.Refusal{Arm: workspace.ArmDirectoryExists, Reason: "exists",
			Fields: map[string]any{"dir": "/trees/better-name"}},
			check: func(e *agentreplv1.SetWorkspaceNameError) bool {
				return e.GetDirectoryExists().GetDir() == "/trees/better-name"
			}},
		{name: "moving", refusal: &workspace.Refusal{Arm: workspace.ArmMoving, Reason: "moving"},
			check: func(e *agentreplv1.SetWorkspaceNameError) bool { return e.GetMoving() != nil }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.Verbs.setNameErr = tt.refusal

			// Act
			resp, err := h.Client.SetWorkspaceName(context.Background(),
				connect.NewRequest(&agentreplv1.SetWorkspaceNameRequest{Workspace: ref(), Name: "better-name"}))

			// Assert
			if err != nil {
				t.Fatalf("SetWorkspaceName: %v", err)
			}
			if !tt.check(resp.Msg.GetError()) {
				t.Fatalf("result = %v, want the %s arm with its evidence", resp.Msg.GetResult(), tt.refusal.Arm)
			}
		})
	}
}

// TestSetWorkspaceNameAnswersTheRosterRevisionThatCarriesTheName pins the
// revision a sidebar that renamed a row holds its drawing until.
func TestSetWorkspaceNameAnswersTheRosterRevisionThatCarriesTheName(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Deps.Sidebar.(*fakeSidebar).revision = 42

	// Act
	resp, err := h.Client.SetWorkspaceName(context.Background(),
		connect.NewRequest(&agentreplv1.SetWorkspaceNameRequest{Workspace: ref(), Name: "better-name"}))

	// Assert
	if err != nil || resp.Msg.GetSuccess().GetRosterRevision() != 42 {
		t.Fatalf("SetWorkspaceName = %v, %v; want success at the sidebar's revision 42", resp, err)
	}
}

// TestCreateWorkspaceAnswersAHeldCreateAsWaitingForTheNetwork pins the
// synchronous form's answer for a create held for the network: the
// waiting_for_network arm naming the pending create, never an error.
func TestCreateWorkspaceAnswersAHeldCreateAsWaitingForTheNetwork(t *testing.T) {
	// Arrange
	h := newHarness(t)
	repoRef := registeredRepoRef(h)
	h.Verbs.createHeld = "create-held"

	// Act
	resp, err := h.Client.CreateWorkspace(context.Background(), connect.NewRequest(standardCreate(repoRef, "")))

	// Assert
	if err != nil {
		t.Fatalf("CreateWorkspace: %v", err)
	}
	if got := resp.Msg.GetWaitingForNetwork().GetCreate().GetId(); got != "create-held" {
		t.Fatalf("result = %v, want waiting_for_network naming create-held", resp.Msg.GetResult())
	}
}

// TestABackgroundCreateHeldForTheNetworkSaysSoOnItsOp pins the option-B
// form: the op is told the create waits, and no terminal step follows it.
func TestABackgroundCreateHeldForTheNetworkSaysSoOnItsOp(t *testing.T) {
	// Arrange
	h := newHarness(t)
	repoRef := registeredRepoRef(h)
	h.Verbs.createHeld = "create-held"
	stream := proveDaemonSubscription(t, h)

	// Act
	resp, err := h.Client.CreateWorkspace(context.Background(), connect.NewRequest(standardCreate(repoRef, "op-held")))

	// Assert
	if err != nil || resp.Msg.GetAccepted() == nil || resp.Msg.GetAccepted().Joined != nil {
		t.Fatalf("CreateWorkspace = %v, %v; want an ack of its own create", resp.Msg.GetResult(), err)
	}
	if !stream.Receive() {
		t.Fatalf("receive the wait: %v", stream.Err())
	}
	prog := stream.Msg().GetMutationProgress()
	if prog.GetOpId() != "op-held" || prog.GetCreate().GetWaitingForNetwork().GetCreate().GetId() != "create-held" {
		t.Fatalf("progress = %v, want op-held waiting for the network as create-held", prog)
	}
}

// TestCreateWorkspaceAnswersACreateHeldForItsRequestersTurn pins the
// synchronous form's answer for a workspace forking itself in its first turn:
// the waiting_for_requester_turn arm naming the pending create, never an
// error.
func TestCreateWorkspaceAnswersACreateHeldForItsRequestersTurn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	repoRef := registeredRepoRef(h)
	h.Verbs.createTurnHeld = "create-turn"
	log := dlog.NewTestLogger()
	h.Server.(*server).log = log

	// Act
	resp, err := h.Client.CreateWorkspace(context.Background(), connect.NewRequest(standardCreate(repoRef, "")))

	// Assert
	if err != nil {
		t.Fatalf("CreateWorkspace: %v", err)
	}
	if got := resp.Msg.GetWaitingForRequesterTurn().GetCreate().GetId(); got != "create-turn" {
		t.Fatalf("result = %v, want waiting_for_requester_turn naming create-turn", resp.Msg.GetResult())
	}
	record, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: "daemon.server.create_workspace",
		Message: "the create waits for its requester's turn to end; it completes once that turn has ended"})
	if !ok || record.Context["requester"] != "ws-requester" {
		t.Fatalf("record = %+v, want the wait recorded naming the requester", record)
	}
}

// TestABackgroundCreateHeldForItsRequestersTurnSaysSoOnItsOp pins the
// option-B form: the op is told the create waits for the requester's turn,
// and no terminal step follows it.
func TestABackgroundCreateHeldForItsRequestersTurnSaysSoOnItsOp(t *testing.T) {
	// Arrange
	h := newHarness(t)
	repoRef := registeredRepoRef(h)
	h.Verbs.createTurnHeld = "create-turn"
	stream := proveDaemonSubscription(t, h)

	// Act
	resp, err := h.Client.CreateWorkspace(context.Background(), connect.NewRequest(standardCreate(repoRef, "op-turn")))

	// Assert
	if err != nil || resp.Msg.GetAccepted() == nil {
		t.Fatalf("CreateWorkspace = %v, %v; want an ack", resp.Msg.GetResult(), err)
	}
	if !stream.Receive() {
		t.Fatalf("receive the wait: %v", stream.Err())
	}
	prog := stream.Msg().GetMutationProgress()
	if prog.GetOpId() != "op-turn" || prog.GetCreate().GetWaitingForRequesterTurn().GetCreate().GetId() != "create-turn" {
		t.Fatalf("progress = %v, want op-turn waiting for its requester's turn as create-turn", prog)
	}
}

// TestCreateWorkspaceAcksAJoinedRequestWithTheCreateItJoined pins that an
// identical request is acked naming the pending create it joined.
func TestCreateWorkspaceAcksAJoinedRequestWithTheCreateItJoined(t *testing.T) {
	// Arrange
	h := newHarness(t)
	repoRef := registeredRepoRef(h)
	h.Verbs.createJoined = "create-first"
	h.Verbs.createRec = wsm.Workspace{ID: "ws-created", Dir: "/tmp/ws", Name: "minted"}

	// Act
	resp, err := h.Client.CreateWorkspace(context.Background(), connect.NewRequest(standardCreate(repoRef, "op-second")))

	// Assert
	if err != nil {
		t.Fatalf("CreateWorkspace: %v", err)
	}
	accepted := resp.Msg.GetAccepted()
	if accepted.GetOpId() != "op-second" || accepted.GetJoined().GetCreate().GetId() != "create-first" {
		t.Fatalf("accepted = %v, want op-second joined to create-first", accepted)
	}
}

// TestACreatesReporterPublishesEveryStepOnItsOp pins the reporter a held
// create's op is answered through, from this daemon or a successor.
func TestACreatesReporterPublishesEveryStepOnItsOp(t *testing.T) {
	tests := []struct {
		name   string
		report func(r workspace.CreateProgress)
		check  func(p *agentreplv1.WorkspaceCreateProgress) bool
	}{
		{name: "waiting for the network",
			report: func(r workspace.CreateProgress) { r.WaitingForNetwork("create-1") },
			check: func(p *agentreplv1.WorkspaceCreateProgress) bool {
				return p.GetWaitingForNetwork().GetCreate().GetId() == "create-1"
			}},
		{name: "waiting for the requester's turn",
			report: func(r workspace.CreateProgress) { r.WaitingForRequesterTurn("create-1") },
			check: func(p *agentreplv1.WorkspaceCreateProgress) bool {
				return p.GetWaitingForRequesterTurn().GetCreate().GetId() == "create-1"
			}},
		{name: "created",
			report: func(r workspace.CreateProgress) { r.Created(wsm.Workspace{ID: "ws-1", Dir: "/d", Name: "minted"}) },
			check: func(p *agentreplv1.WorkspaceCreateProgress) bool {
				return p.GetSucceeded().GetName() == "minted" && p.GetSucceeded().GetWorkspace().GetId() == "ws-1"
			}},
		{name: "failed",
			report: func(r workspace.CreateProgress) { r.Failed(errors.New("disk full")) },
			check: func(p *agentreplv1.WorkspaceCreateProgress) bool {
				return p.GetFailed().GetInternal() == "disk full"
			}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			s := h.Server.(*server)
			log := dlog.NewTestLogger()
			s.log = log
			stream := proveDaemonSubscription(t, h)
			reporter := s.CreateProgress("op-r")

			// Act
			tc.report(reporter)

			// Assert
			if reporter.OpID() != "op-r" {
				t.Fatalf("OpID = %q, want op-r", reporter.OpID())
			}
			if !stream.Receive() {
				t.Fatalf("receive: %v", stream.Err())
			}
			prog := stream.Msg().GetMutationProgress()
			if prog.GetOpId() != "op-r" || !tc.check(prog.GetCreate()) {
				t.Fatalf("progress = %v", prog)
			}
			if tc.name == "failed" {
				if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "daemon.server.create_workspace",
					Message: "a background create failed"}); !ok {
					t.Fatalf("no error record for the internal failure: %+v", log.Records())
				}
			}
		})
	}
}

// ---- RegisterWorkspaces ----------------------------------------------------

// TestRegisterWorkspacesAnswersEachDirsOutcome pins one outcome per dir, in
// the order the verb answered them, each in the arm its answer calls for: a
// record is `registered`, a typed refusal is `refused` with RegisterWorkspace's
// own arm, and any other error is `failed`.
func TestRegisterWorkspacesAnswersEachDirsOutcome(t *testing.T) {
	tests := []struct {
		name   string
		answer workspace.Registered
		check  func(t *testing.T, outcome *agentreplv1.RegisterWorkspacesOutcome)
	}{
		{"a registered dir answers its ref",
			workspace.Registered{Dir: "/w/a", Record: wsm.Workspace{ID: "ws-a", Dir: "/w/a"}},
			func(t *testing.T, outcome *agentreplv1.RegisterWorkspacesOutcome) {
				if got := outcome.GetRegistered().GetWorkspace().GetId(); got != "ws-a" {
					t.Fatalf("registered id = %q (outcome %v), want ws-a", got, outcome)
				}
			}},
		{"a refused dir answers RegisterWorkspace's refusal arm",
			workspace.Registered{Dir: "/w/b", Err: &workspace.Refusal{
				Arm: workspace.ArmNotAWorktree, Reason: "not a worktree", Rpc: "RegisterWorkspaces",
			}},
			func(t *testing.T, outcome *agentreplv1.RegisterWorkspacesOutcome) {
				if outcome.GetRefused().GetNotAWorktree() == nil {
					t.Fatalf("outcome = %v, want refused not_a_worktree", outcome)
				}
			}},
		{"a handed-away dir answers transferring_away naming the successor",
			workspace.Registered{Dir: "/w/d", Err: &workspace.Refusal{
				Arm: workspace.ArmTransferringAway, Reason: "handed to the successor", Rpc: "RegisterWorkspaces",
			}},
			func(t *testing.T, outcome *agentreplv1.RegisterWorkspacesOutcome) {
				if got := outcome.GetRefused().GetTransferringAway().GetAddress(); got != "127.0.0.1:9999" {
					t.Fatalf("transferring_away address = %q (outcome %v), want the successor's", got, outcome)
				}
			}},
		{"a faulted dir answers failed with its cause",
			workspace.Registered{Dir: "/w/c", Err: errors.New("git broke")},
			func(t *testing.T, outcome *agentreplv1.RegisterWorkspacesOutcome) {
				if got := outcome.GetFailed().GetCause(); got != "git broke" {
					t.Fatalf("failed cause = %q (outcome %v), want git broke", got, outcome)
				}
			}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Verbs.registerAllAnswers = []workspace.Registered{tc.answer}

			// Act.
			resp, err := h.Client.RegisterWorkspaces(context.Background(),
				connect.NewRequest(&agentreplv1.RegisterWorkspacesRequest{Dirs: []string{tc.answer.Dir}}))

			// Assert.
			if err != nil {
				t.Fatalf("RegisterWorkspaces: %v", err)
			}
			if len(resp.Msg.GetOutcomes()) != 1 || resp.Msg.GetOutcomes()[0].GetDir() != tc.answer.Dir {
				t.Fatalf("outcomes = %v, want one naming %q", resp.Msg.GetOutcomes(), tc.answer.Dir)
			}
			tc.check(t, resp.Msg.GetOutcomes()[0])
		})
	}
}

// TestRegisterWorkspaceRefusesAHandedAwayWorkspaceWithItsSuccessor pins the
// lone registration's typed transferring_away: the arm carries the successor
// the workspace registers at, and the refusal is recorded once.
func TestRegisterWorkspaceRefusesAHandedAwayWorkspaceWithItsSuccessor(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })
	h.Verbs.registerErr = &workspace.Refusal{
		Arm: workspace.ArmTransferringAway, Reason: "handed to the successor", Rpc: "RegisterWorkspace",
	}

	// Act.
	resp, err := h.Client.RegisterWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.RegisterWorkspaceRequest{Dir: "/w/a"}))

	// Assert.
	if err != nil {
		t.Fatalf("RegisterWorkspace: %v, want the typed refusal", err)
	}
	if got := resp.Msg.GetError().GetTransferringAway().GetAddress(); got != "127.0.0.1:9999" {
		t.Fatalf("transferring_away address = %q (response %v), want the successor's", got, resp.Msg)
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelDebug, Operation: "RegisterWorkspace"}); !ok {
		t.Fatalf("records = %+v, want the typed refusal recorded", log.Records())
	}
}

// TestRegisterWorkspaceAnswersTheVerbsRefusalsOnTheirArms pins a lone
// registration's typed refusals reaching RegisterWorkspaceError by name, with
// their fields.
func TestRegisterWorkspaceAnswersTheVerbsRefusalsOnTheirArms(t *testing.T) {
	tests := []struct {
		name    string
		refusal *workspace.Refusal
		got     func(*agentreplv1.RegisterWorkspaceError) bool
	}{
		{name: "not a worktree",
			refusal: &workspace.Refusal{Rpc: "RegisterWorkspace", Arm: workspace.ArmNotAWorktree, Reason: "not a worktree"},
			got:     func(e *agentreplv1.RegisterWorkspaceError) bool { return e.GetNotAWorktree() != nil }},
		{name: "inside a temporary directory",
			refusal: &workspace.Refusal{Rpc: "RegisterWorkspace", Arm: workspace.ArmInsideTemporaryDirectory, Reason: "scratch",
				Fields: map[string]any{"dir": "/tmp/scratch/cwd", "temporary_root": "/tmp"}},
			got: func(e *agentreplv1.RegisterWorkspaceError) bool {
				arm := e.GetInsideTemporaryDirectory()
				return arm.GetDir() == "/tmp/scratch/cwd" && arm.GetTemporaryRoot() == "/tmp"
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Verbs.registerErr = tt.refusal

			// Act.
			resp, err := h.Client.RegisterWorkspace(context.Background(),
				connect.NewRequest(&agentreplv1.RegisterWorkspaceRequest{Dir: "/w/a"}))

			// Assert.
			if err != nil {
				t.Fatalf("RegisterWorkspace = transport error %v, want the in-band arm", err)
			}
			if !tt.got(resp.Msg.GetError()) {
				t.Fatalf("result = %v, want the %s arm", resp.Msg.GetResult(), tt.refusal.Arm)
			}
		})
	}
}

// TestRegisterWorkspacesHandsEveryDirToOneBatch pins that the request's dirs
// reach the verb as ONE batch, in order: the whole point of the rpc.
func TestRegisterWorkspacesHandsEveryDirToOneBatch(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	dirs := []string{"/w/a", "/w/b", "/w/c"}
	for _, dir := range dirs {
		h.Verbs.registerAllAnswers = append(h.Verbs.registerAllAnswers,
			workspace.Registered{Dir: dir, Record: wsm.Workspace{ID: ids.WorkspaceID("id" + dir), Dir: dir}})
	}

	// Act.
	if _, err := h.Client.RegisterWorkspaces(context.Background(),
		connect.NewRequest(&agentreplv1.RegisterWorkspacesRequest{Dirs: dirs})); err != nil {
		t.Fatalf("RegisterWorkspaces: %v", err)
	}

	// Assert.
	if len(h.Verbs.registerAllDirs) != 1 || !slices.Equal(h.Verbs.registerAllDirs[0], dirs) {
		t.Fatalf("batches = %v, want one carrying %v", h.Verbs.registerAllDirs, dirs)
	}
}

// TestRegisterWorkspacesRecordsAFaultedDirAtError pins the fault's one ERROR
// record, naming the dir and its cause.
func TestRegisterWorkspacesRecordsAFaultedDirAtError(t *testing.T) {
	// Arrange.
	log := &recordingLogger{}
	h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })
	h.Verbs.registerAllAnswers = []workspace.Registered{{Dir: "/w/c", Err: errors.New("git broke")}}

	// Act.
	if _, err := h.Client.RegisterWorkspaces(context.Background(),
		connect.NewRequest(&agentreplv1.RegisterWorkspacesRequest{Dirs: []string{"/w/c"}})); err != nil {
		t.Fatalf("RegisterWorkspaces: %v", err)
	}

	// Assert.
	for _, rec := range log.at("ERROR") {
		if rec.Operation == "RegisterWorkspaces" && rec.Context["cause"] == "git broke" {
			return
		}
	}
	t.Fatalf("ERROR records = %+v, want the faulted dir's cause", log.at("ERROR"))
}

// TestRegisterWorkspacesRefusesAMalformedRequest pins no dirs, or a blank
// one, as a malformed request that reaches no verb.
func TestRegisterWorkspacesRefusesAMalformedRequest(t *testing.T) {
	tests := []struct {
		name string
		dirs []string
	}{
		{"no dirs", nil},
		{"a blank dir", []string{"/w/a", ""}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			_, err := h.Client.RegisterWorkspaces(context.Background(),
				connect.NewRequest(&agentreplv1.RegisterWorkspacesRequest{Dirs: tc.dirs}))

			// Assert.
			if connect.CodeOf(err) != connect.CodeInvalidArgument {
				t.Fatalf("code = %v (%v), want InvalidArgument", connect.CodeOf(err), err)
			}
			if len(h.Verbs.registerAllDirs) != 0 {
				t.Fatalf("batches = %v, want none for a malformed request", h.Verbs.registerAllDirs)
			}
		})
	}
}

func TestCreateWorkspaceMapsTheOriginatorToTheSpec(t *testing.T) {
	const thread = "https://w.slack.com/archives/C1/p1730000000123456"
	tests := []struct {
		name       string
		originator *agentreplv1.WorkspaceOriginator
		want       lifecycleconfig.Originator
	}{
		{name: "the emoji-ack system", originator: emojiAckOriginator(), want: lifecycleconfig.OriginatorEmojiAck},
		{name: "the user or a workspace", originator: userOrWorkspaceOriginator(), want: lifecycleconfig.OriginatorUserOrWorkspace},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Verbs.createRec = wsm.Workspace{ID: "ws-created", Dir: "/tmp/ws", Name: "minted"}
			req := oneShotCreate(registeredRepoRef(h), nil)
			req.Originator = tt.originator
			req.SlackOrigin = &agentreplv1.CreateWorkspaceSlackOrigin{ThreadUrl: thread}

			// Act.
			resp, err := h.Client.CreateWorkspace(context.Background(), connect.NewRequest(req))

			// Assert.
			if err != nil || resp.Msg.GetSuccess() == nil {
				t.Fatalf("CreateWorkspace = (%v, %v), want success", resp, err)
			}
			if got := h.Verbs.createSpecs[0].Originator(); got != tt.want {
				t.Fatalf("spec originator = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestCreateWorkspaceAnswersTheLifecycleRefusals(t *testing.T) {
	tests := []struct {
		name    string
		refusal *workspace.Refusal
		check   func(*agentreplv1.CreateWorkspaceError) bool
	}{
		{name: "a failed creation hook", refusal: &workspace.Refusal{Rpc: "CreateWorkspace", Arm: workspace.ArmCreationHookFailed,
			Fields: map[string]any{"path": "bin/setup.sh", "detail": "it exited 2", "log_path": "/state/merge-logs/creation-1-0.log"}},
			check: func(e *agentreplv1.CreateWorkspaceError) bool {
				f := e.GetCreationHookFailed()
				return f.GetPath() == "bin/setup.sh" && f.GetDetail() == "it exited 2" && f.GetLogPath() == "/state/merge-logs/creation-1-0.log"
			}},
		{name: "an unusable lifecycle config", refusal: &workspace.Refusal{Rpc: "CreateWorkspace", Arm: workspace.ArmLifecycleConfigInvalid,
			Fields: map[string]any{"path": "/r/.agent-repl/lifecycle.yaml", "detail": "unknown field"}},
			check: func(e *agentreplv1.CreateWorkspaceError) bool {
				return e.GetLifecycleConfigInvalid().GetPath() == "/r/.agent-repl/lifecycle.yaml" && e.GetLifecycleConfigInvalid().GetDetail() == "unknown field"
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Verbs.createErr = tt.refusal

			// Act.
			resp, err := h.Client.CreateWorkspace(context.Background(), connect.NewRequest(standardCreate(registeredRepoRef(h), "")))

			// Assert.
			if err != nil || !tt.check(resp.Msg.GetError()) {
				t.Fatalf("CreateWorkspace = (%v, %v), want the refusal's arm with its fields", resp, err)
			}
		})
	}
}

func TestCreateWorkspaceMapsBackloggedAndPlacementToTheSpec(t *testing.T) {
	tests := []struct {
		name           string
		backlogged     *bool
		placement      *agentreplv1.CreateWorkspacePlacement
		wantBacklogged bool
		wantPlacement  *workspace.CreatePlacement
	}{
		{name: "neither", wantBacklogged: false},
		{name: "born backlogged", backlogged: boolPtr(true), wantBacklogged: true},
		{name: "an existing subsection", placement: &agentreplv1.CreateWorkspacePlacement{
			Target: &agentreplv1.CreateWorkspacePlacement_Subsection{Subsection: "auth"}},
			wantPlacement: &workspace.CreatePlacement{Subsection: "auth"}},
		{name: "a new subsection with a gather", placement: &agentreplv1.CreateWorkspacePlacement{
			Target: &agentreplv1.CreateWorkspacePlacement_NewSubsection{NewSubsection: &agentreplv1.CreateWorkspaceNewSubsection{Name: "auth", Gather: []string{"a", "b"}}}},
			wantPlacement: &workspace.CreatePlacement{NewSubsection: &wsm.NewSubsectionPlacement{Name: "auth", Gather: []string{"a", "b"}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.Verbs.createRec = wsm.Workspace{ID: "ws-created", Dir: "/tmp/ws", Name: "minted"}
			req := oneShotCreate(registeredRepoRef(h), nil)
			req.Backlogged, req.Placement = tt.backlogged, tt.placement

			// Act
			resp, err := h.Client.CreateWorkspace(context.Background(), connect.NewRequest(req))

			// Assert
			if err != nil || resp.Msg.GetSuccess() == nil {
				t.Fatalf("CreateWorkspace = (%v, %v), want success", resp, err)
			}
			spec := h.Verbs.createSpecs[0]
			if spec.Backlogged != tt.wantBacklogged || !reflect.DeepEqual(spec.Placement, tt.wantPlacement) {
				t.Fatalf("spec backlogged %v placement %+v, want %v and %+v", spec.Backlogged, spec.Placement, tt.wantBacklogged, tt.wantPlacement)
			}
		})
	}
}

func TestCreateWorkspaceRefusesAPlacementOnAnAsyncMergeOnItsArm(t *testing.T) {
	// Arrange
	h, req := asyncHarness(t)
	req.Placement = &agentreplv1.CreateWorkspacePlacement{Target: &agentreplv1.CreateWorkspacePlacement_Subsection{Subsection: "auth"}}

	// Act
	resp, err := h.Client.CreateWorkspace(context.Background(), connect.NewRequest(req))

	// Assert
	if err != nil || resp.Msg.GetError().GetPlacementOnMergeWorkspace() == nil {
		t.Fatalf("CreateWorkspace = (%v, %v), want the placement_on_merge_workspace arm", resp, err)
	}
	if len(h.Merge.requests) != 0 {
		t.Fatalf("merge requests = %+v, want none enqueued", h.Merge.requests)
	}
}

// TestSaidPartsSplitsWordsFromImages pins how a creation prompt reaches the
// creation spec: every text block joined by a newline, and every image kept
// in composed order, wherever it stood among the words.
func TestSaidPartsSplitsWordsFromImages(t *testing.T) {
	tests := []struct {
		name       string
		said       *conversationv1.UserSaid
		wantText   string
		wantImages []string
	}{
		{name: "no prompt", said: nil, wantText: ""},
		{name: "words alone", said: saidBlocks(textBlock("fix it")), wantText: "fix it"},
		{
			name:       "images among the words",
			said:       saidBlocks(textBlock("look at"), imageBlock("/a.png"), textBlock("and"), imageBlock("/b.png")),
			wantText:   "look at\nand",
			wantImages: []string{"/a.png", "/b.png"},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			text, images := saidParts(tt.said)

			// Assert
			var paths []string
			for _, image := range images {
				paths = append(paths, image.GetPath().GetPath())
			}
			if text != tt.wantText || !slices.Equal(paths, tt.wantImages) {
				t.Fatalf("saidParts = %q %v, want %q %v", text, paths, tt.wantText, tt.wantImages)
			}
		})
	}
}

func TestFinishWorkspaceAnswersHowTheWorkLands(t *testing.T) {
	tests := []struct {
		name            string
		remediation     bool
		noPlacement     bool
		wantDirectly    bool
		wantRemediation int
		wantQueued      int
	}{
		{name: "an ordinary workspace's finish is queued", wantQueued: 1},
		{name: "a deploy remediation's finish lands directly", remediation: true, wantDirectly: true, wantRemediation: 1},
		{name: "a deploy remediation's finish naming no placement lands directly", remediation: true, noPlacement: true, wantDirectly: true, wantRemediation: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Merge.remediations = map[ids.WorkspaceID]bool{testWorkspaceID: tt.remediation}
			req := ownFinish()
			if tt.noPlacement {
				req.Placement = nil
			}

			// Act.
			resp, err := h.Client.FinishWorkspace(context.Background(), connect.NewRequest(req))

			// Assert.
			if err != nil || resp.Msg.GetSuccess() == nil {
				t.Fatalf("FinishWorkspace = (%v, %v), want success", resp, err)
			}
			success := resp.Msg.GetSuccess()
			if (success.GetLandingDirectly() != nil) != tt.wantDirectly || (success.GetQueued() != nil) == tt.wantDirectly {
				t.Fatalf("landing = %v, want directly %v", success, tt.wantDirectly)
			}
			if len(h.Merge.remediationFinishes) != tt.wantRemediation || len(h.Merge.requests) != tt.wantQueued {
				t.Fatalf("remediation finishes %v, queued %v; want %d and %d", h.Merge.remediationFinishes, h.Merge.requests, tt.wantRemediation, tt.wantQueued)
			}
		})
	}
}

func TestFinishWorkspaceMapsARemediationsRefusalOntoItsArm(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Merge.remediations = map[ids.WorkspaceID]bool{testWorkspaceID: true}
	h.Merge.remediationFinishErr = &merge.RefusalError{Arm: merge.ArmAlreadyMerging, Reason: "already running"}

	// Act.
	resp, err := h.Client.FinishWorkspace(context.Background(), connect.NewRequest(ownFinish()))

	// Assert.
	if err != nil || resp.Msg.GetError().GetAlreadyDeploying() == nil {
		t.Fatalf("FinishWorkspace = (%v, %v), want the already_deploying arm", resp, err)
	}
}

func TestFinishWorkspaceWhoseRoleCannotBeReadFinishesNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	log := dlog.NewTestLogger()
	h.Surfaces.workspace = log
	h.Merge.remediationErr = errors.New("store gone")

	// Act.
	_, err := h.Client.FinishWorkspace(context.Background(), connect.NewRequest(ownFinish()))

	// Assert.
	if got := connectCode(t, err); got != connect.CodeInternal {
		t.Fatalf("code = %v, want Internal", got)
	}
	if len(h.Merge.requests) != 0 || len(h.Merge.remediationFinishes) != 0 {
		t.Fatalf("queued %v, remediation finishes %v; want nothing finished", h.Merge.requests, h.Merge.remediationFinishes)
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.server.finish_workspace"}); !ok {
		t.Fatalf("records = %+v, want the unreadable role at ERROR", log.Records())
	}
}

// mediatorHarness is a harness whose workspace is repo-1's deploy mediator.
func mediatorHarness(t *testing.T) *harness {
	t.Helper()
	h := newHarness(t)
	ws := h.DB.workspaces[testWorkspaceID]
	ws.Repo = "repo-1"
	ws.Mediator = wsm.MediatorDeploy
	h.DB.workspaces[testWorkspaceID] = ws
	return h
}

func TestCreateWorkspaceRefusesEveryFormTheDeployMediatorRequests(t *testing.T) {
	tests := []struct {
		name string
		req  func(h *harness) *agentreplv1.CreateWorkspaceRequest
	}{
		{name: "a standard create", req: func(h *harness) *agentreplv1.CreateWorkspaceRequest {
			req := standardCreate(registeredRepoRef(h), "")
			req.Requester = workspaceRequester(&workspacev1.WorkspaceRef{Id: string(testWorkspaceID)})
			return req
		}},
		{name: "an async merge", req: func(h *harness) *agentreplv1.CreateWorkspaceRequest {
			req := validAsyncMerge(nil)
			req.Repository = registeredRepoRef(h)
			return req
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := mediatorHarness(t)

			// Act
			resp, err := h.Client.CreateWorkspace(context.Background(), connect.NewRequest(tt.req(h)))

			// Assert
			if err != nil {
				t.Fatalf("CreateWorkspace: %v", err)
			}
			refused := resp.Msg.GetError().GetRequestedByDeployMediator()
			if refused == nil || refused.GetWorkspace().GetId() != string(testWorkspaceID) {
				t.Fatalf("result = %v, want requested_by_deploy_mediator naming the mediator", resp.Msg.GetResult())
			}
			if !strings.Contains(refused.GetDetail(), "in its own session") {
				t.Fatalf("detail = %q, want it to say the work belongs in the mediator's own session", refused.GetDetail())
			}
		})
	}
}

func TestCreateWorkspaceRefusesTheProductionMediatorsDirectCreate(t *testing.T) {
	// Arrange
	h := mediatorHarness(t)
	ws := h.DB.workspaces[testWorkspaceID]
	ws.Mediator = wsm.MediatorProduction
	h.DB.workspaces[testWorkspaceID] = ws
	req := standardCreate(registeredRepoRef(h), "")
	req.Requester = workspaceRequester(&workspacev1.WorkspaceRef{Id: string(testWorkspaceID)})

	// Act
	resp, err := h.Client.CreateWorkspace(context.Background(), connect.NewRequest(req))

	// Assert
	if err != nil {
		t.Fatalf("CreateWorkspace: %v", err)
	}
	refused := resp.Msg.GetError().GetRequestedByProductionMediator()
	if refused == nil || refused.GetWorkspace().GetId() != string(testWorkspaceID) || !strings.Contains(refused.GetDetail(), "production-mediation dispatch") {
		t.Fatalf("result = %v, want requested_by_production_mediator naming the mediator and the dispatch", resp.Msg.GetResult())
	}
}

func TestCreateWorkspaceDoesNotRefuseAnOrdinaryRequesterAsTheDeployMediator(t *testing.T) {
	// Arrange
	h := newHarness(t)
	req := standardCreate(registeredRepoRef(h), "")
	req.Requester = workspaceRequester(&workspacev1.WorkspaceRef{Id: string(testWorkspaceID)})

	// Act
	resp, _ := h.Client.CreateWorkspace(context.Background(), connect.NewRequest(req))

	// Assert
	if resp != nil && resp.Msg.GetError().GetRequestedByDeployMediator() != nil {
		t.Fatalf("result = %v, want no deploy-mediator refusal for an ordinary workspace", resp.Msg.GetResult())
	}
}

func TestCreateWorkspaceFailsWhenTheRequestersStandingCannotBeRead(t *testing.T) {
	// Arrange
	log := &recordingLogger{}
	h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })
	ws := h.DB.workspaces[testWorkspaceID]
	ws.Mediator = wsm.MediatorDeploy
	h.DB.workspaces[testWorkspaceID] = ws
	h.DB.workspaceErrs = map[ids.WorkspaceID]error{testWorkspaceID: errors.New("database is locked")}
	req := standardCreate(registeredRepoRef(h), "")
	req.Requester = workspaceRequester(&workspacev1.WorkspaceRef{Id: string(testWorkspaceID)})

	// Act
	_, err := h.Client.CreateWorkspace(context.Background(), connect.NewRequest(req))

	// Assert
	if connect.CodeOf(err) != connect.CodeInternal {
		t.Fatalf("err = %v, want an internal failure for an unreadable requester", err)
	}
	logged := false
	for _, rec := range log.at("ERROR") {
		if strings.Contains(fmt.Sprint(rec.Context), "mediator standing") {
			logged = true
		}
	}
	if !logged {
		t.Fatalf("ERROR records = %+v, want the failed standing read recorded", log.at("ERROR"))
	}
}
