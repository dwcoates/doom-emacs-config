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
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
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
			Standard: &agentreplv1.CreateWorkspaceStandard{Topology: primaryTopology()},
		},
		Requester:     userRequester(),
		ContextOrigin: freshContext(),
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
	stream, err := h.Client.WatchDaemon(ctx, connect.NewRequest(&agentreplv1.WatchDaemonRequest{Client: &agentreplv1.WatchDaemonRequest_Emacs{Emacs: &agentreplv1.WatchDaemonEmacs{Focus: unfocusedEditor(), ElispBuild: "elisp-test", Instance: testEditorInstance()}}}))
	if err != nil {
		t.Fatalf("open the daemon stream: %v", err)
	}
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

// ---- SetWorkspaceHidden ----------------------------------------------------

func hideArm() *agentreplv1.SetWorkspaceHiddenRequest_Hide {
	return &agentreplv1.SetWorkspaceHiddenRequest_Hide{Hide: &agentreplv1.SetWorkspaceHiddenHide{}}
}

func showArm() *agentreplv1.SetWorkspaceHiddenRequest_Show {
	return &agentreplv1.SetWorkspaceHiddenRequest_Show{Show: &agentreplv1.SetWorkspaceHiddenShow{}}
}

// TestSetWorkspaceHiddenHandsThePlaceToTheVerb pins each visibility arm
// reaching the verb with the id the registry resolved.
func TestSetWorkspaceHiddenHandsThePlaceToTheVerb(t *testing.T) {
	tests := []struct {
		name string
		req  *agentreplv1.SetWorkspaceHiddenRequest
		want bool
	}{
		{"hide", &agentreplv1.SetWorkspaceHiddenRequest{Workspace: ref(), Visibility: hideArm()}, true},
		{"show", &agentreplv1.SetWorkspaceHiddenRequest{Workspace: ref(), Visibility: showArm()}, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			resp, err := h.Client.SetWorkspaceHidden(context.Background(),
				connect.NewRequest(tt.req))

			// Assert
			if err != nil {
				t.Fatalf("SetWorkspaceHidden: %v", err)
			}
			if resp.Msg.GetSuccess() == nil {
				t.Fatalf("result = %v, want success", resp.Msg.GetResult())
			}
			want := hiddenSet{ws: testWorkspaceID, hidden: tt.want}
			if len(h.Verbs.hiddenSets) != 1 || h.Verbs.hiddenSets[0] != want {
				t.Fatalf("hidden sets = %+v, want %+v", h.Verbs.hiddenSets, want)
			}
		})
	}
}

// TestSetWorkspaceHiddenRefusesAnUnknownWorkspace pins that an id the
// registry does not hold answers the unknown_workspace arm and reaches no verb.
func TestSetWorkspaceHiddenRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	resp, err := h.Client.SetWorkspaceHidden(context.Background(),
		connect.NewRequest(&agentreplv1.SetWorkspaceHiddenRequest{
			Workspace: &workspacev1.WorkspaceRef{Id: "ws-nope"}, Visibility: hideArm(),
		}))

	// Assert
	if err != nil {
		t.Fatalf("SetWorkspaceHidden: %v", err)
	}
	if resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("result = %v, want unknown_workspace", resp.Msg.GetResult())
	}
	if len(h.Verbs.hiddenSets) != 0 {
		t.Fatalf("hidden sets = %+v, want none for an unknown workspace", h.Verbs.hiddenSets)
	}
}

// TestSetWorkspaceHiddenMapsTheVerbsRefusal pins a refusal the verb raises
// answering its own arm.
func TestSetWorkspaceHiddenMapsTheVerbsRefusal(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Verbs.setHiddenErr = &workspace.Refusal{Arm: workspace.ArmNotYetAdopted, Reason: "still adopting"}

	// Act
	resp, err := h.Client.SetWorkspaceHidden(context.Background(),
		connect.NewRequest(&agentreplv1.SetWorkspaceHiddenRequest{Workspace: ref(), Visibility: hideArm()}))

	// Assert
	if err != nil {
		t.Fatalf("SetWorkspaceHidden: %v", err)
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
		{"set", &agentreplv1.ChangeWorkspaceLinksRequest{Workspace: ref(), Change: &agentreplv1.ChangeWorkspaceLinksRequest_Set{Set: &agentreplv1.ChangeWorkspaceLinksSet{Url: "https://example.com/pr/1", Label: "The pull request"}}},
			"set https://example.com/pr/1 The pull request"},
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
		{name: "too_many_shown", refusal: &workspace.Refusal{Arm: workspace.ArmTooManyShown, Fields: map[string]any{"named": uint32(6), "limit": uint32(5)}},
			check: func(e *agentreplv1.ChangeWorkspaceLinksError) bool {
				return e.GetTooManyShown().GetNamed() == 6 && e.GetTooManyShown().GetLimit() == 5
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
					Change: &agentreplv1.ChangeWorkspaceLinksRequest_Set{Set: &agentreplv1.ChangeWorkspaceLinksSet{Url: "ftp://x", Label: "Doc"}}}))

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
	parentNotOpen := &wsm.ParentNotOpenError{Parent: wsm.FamilyRef{ID: "ws-parent", Dir: "/w/parent"}, State: wsm.ParentHidden}
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
			name:  "SetWorkspaceHidden has_open_children",
			setUp: func(h *harness) { h.Verbs.setHiddenErr = fmt.Errorf("hide: %w", familyChildren) },
			call: func(h *harness) (bool, error) {
				resp, err := h.Client.SetWorkspaceHidden(context.Background(), connect.NewRequest(&agentreplv1.SetWorkspaceHiddenRequest{Workspace: ref(), Visibility: hideArm()}))
				return err == nil && resp.Msg.GetError().GetHasOpenChildren() != nil, err
			},
		},
		{
			name:  "SetWorkspaceHidden parent_not_open",
			setUp: func(h *harness) { h.Verbs.setHiddenErr = fmt.Errorf("show: %w", parentNotOpen) },
			call: func(h *harness) (bool, error) {
				resp, err := h.Client.SetWorkspaceHidden(context.Background(), connect.NewRequest(&agentreplv1.SetWorkspaceHiddenRequest{Workspace: ref(), Visibility: hideArm()}))
				return err == nil && resp.Msg.GetError().GetParentNotOpen().GetHidden() != nil, err
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

// TestSetWorkspaceHiddenRefusesAMissingVisibility pins an unset arm as a
// malformed request that reaches no verb.
func TestSetWorkspaceHiddenRefusesAMissingVisibility(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	_, err := h.Client.SetWorkspaceHidden(context.Background(),
		connect.NewRequest(&agentreplv1.SetWorkspaceHiddenRequest{Workspace: ref()}))

	// Assert
	if connect.CodeOf(err) != connect.CodeInvalidArgument {
		t.Fatalf("code = %v (%v), want InvalidArgument", connect.CodeOf(err), err)
	}
	if len(h.Verbs.hiddenSets) != 0 {
		t.Fatalf("hidden sets = %+v, want none for a malformed request", h.Verbs.hiddenSets)
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
		{"parent in another repository", &workspace.Refusal{Arm: workspace.ArmParentInOtherRepository, Reason: "foreign",
			Fields: map[string]any{"parent_repository_dir": "/other"}},
			func(e *agentreplv1.SetWorkspaceParentError) bool {
				return e.GetParentInOtherRepository().GetParentRepositoryDir() == "/other"
			}},
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

// ---- MarkWorkspaceViewed ---------------------------------------------------
// TestMarkWorkspaceViewedAnswersSuccess pins the editor's viewed report
// reaching the verb with the id the registry resolved.
func TestMarkWorkspaceViewedAnswersSuccess(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	// Act.
	resp, err := h.Client.MarkWorkspaceViewed(context.Background(),
		connect.NewRequest(&agentreplv1.MarkWorkspaceViewedRequest{Workspace: ref()}))
	// Assert.
	if err != nil {
		t.Fatalf("MarkWorkspaceViewed: %v", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("result = %v, want success", resp.Msg.GetResult())
	}
	if len(h.Verbs.markViewed) != 1 || h.Verbs.markViewed[0] != testWorkspaceID {
		t.Fatalf("marked = %v, want the resolved workspace once", h.Verbs.markViewed)
	}
}

// TestMarkWorkspaceViewedRefusesAnUnknownWorkspace pins that an id the registry
// does not hold answers the unknown_workspace arm rather than a transport error.
func TestMarkWorkspaceViewedRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	// Act.
	resp, err := h.Client.MarkWorkspaceViewed(context.Background(),
		connect.NewRequest(&agentreplv1.MarkWorkspaceViewedRequest{
			Workspace: &workspacev1.WorkspaceRef{Id: "ws-nope"},
		}))
	// Assert.
	if err != nil {
		t.Fatalf("MarkWorkspaceViewed: %v", err)
	}
	if resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("result = %v, want unknown_workspace", resp.Msg.GetResult())
	}
	if len(h.Verbs.markViewed) != 0 {
		t.Fatalf("marked = %v, want nothing marked for an unknown workspace", h.Verbs.markViewed)
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
			Fields: map[string]any{"path": "/repo/.agent-repl/deploy.yaml"}},
			got: func(e *agentreplv1.FinishWorkspaceError) bool {
				return e.GetNoDeployConfig().GetPath() == "/repo/.agent-repl/deploy.yaml"
			}},
		{name: "an unusable deploy config", refusal: &merge.RefusalError{Arm: merge.ArmDeployConfigInvalid, Reason: "bad",
			Fields: map[string]any{"path": "/repo/.agent-repl/deploy.yaml", "detail": "pre_deploy is required"}},
			got: func(e *agentreplv1.FinishWorkspaceError) bool {
				return e.GetDeployConfigInvalid().GetPath() == "/repo/.agent-repl/deploy.yaml" && e.GetDeployConfigInvalid().GetDetail() == "pre_deploy is required"
			}},
		{name: "placed behind a merge not in line", refusal: &merge.RefusalError{Arm: merge.ArmPlacementBehindNotInLine, Reason: "none",
			Fields: map[string]any{"behind": "two"}},
			got: func(e *agentreplv1.FinishWorkspaceError) bool {
				return e.GetPlacementBehindNotInLine().GetBehind() == "two"
			}},
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

func TestFinishWorkspaceAnswersWhoMayLandIt(t *testing.T) {
	tests := []struct {
		name string
		arm  string
		got  func(*agentreplv1.FinishWorkspaceError) bool
	}{
		{name: "a child its owner lands", arm: merge.ArmOwnerLands, got: func(e *agentreplv1.FinishWorkspaceError) bool {
			return e.GetOwnerLands().GetOwner().GetId() == "ws-owner"
		}},
		{name: "a stranger", arm: merge.ArmRequesterMayNotLand, got: func(e *agentreplv1.FinishWorkspaceError) bool { return e.GetRequesterMayNotLand() != nil }},
		{name: "a detached head", arm: merge.ArmNoBranchCheckedOut, got: func(e *agentreplv1.FinishWorkspaceError) bool { return e.GetNoBranchCheckedOut().GetDir() == "/wt" }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Merge.enqueueErr = &merge.RefusalError{Arm: tt.arm, Reason: "refused", Fields: map[string]any{
				"owner": &workspacev1.WorkspaceRef{Id: "ws-owner"}, "dir": "/wt"}}

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
	oneShot := &agentreplv1.CreateWorkspaceOneShot{Prompt: workspace.SaidText("fix the feed"), Topology: primaryTopology()}
	if about != nil {
		oneShot.Context = &agentreplv1.CreateWorkspaceOneShotContext{Workspace: about}
	}
	return &agentreplv1.CreateWorkspaceRequest{
		Repository:    repoRef,
		Form:          &agentreplv1.CreateWorkspaceRequest_OneShot{OneShot: oneShot},
		Requester:     userRequester(),
		ContextOrigin: freshContext(),
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
