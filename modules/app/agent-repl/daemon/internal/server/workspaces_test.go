package server

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/merge"
	"claude-repld/internal/workspace"
	"claude-repld/internal/wsm"
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
		connect.NewRequest(&agentreplv1.CloseWorkspaceRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("CloseWorkspace: %v", err)
	}
	if resp.Msg.GetError().GetBlocked() == nil {
		t.Fatalf("result = %v, want blocked", resp.Msg.GetResult())
	}
}

// TestCreateWorkspaceRefusesAnUnknownRepository pins that a repository ref
// matching nothing registered is refused rather than materialized somewhere.
func TestCreateWorkspaceRefusesAnUnknownRepository(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.CreateWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
			Repository: &workspacev1.RepositoryRef{Id: "repo-nope"},
			Form: &agentreplv1.CreateWorkspaceRequest_Standard{
				Standard: &agentreplv1.CreateWorkspaceStandard{},
			},
		}))

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
			Standard: &agentreplv1.CreateWorkspaceStandard{},
		},
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
	stream, err := h.Client.WatchDaemon(ctx, connect.NewRequest(&agentreplv1.WatchDaemonRequest{Client: &agentreplv1.WatchDaemonRequest_Emacs{Emacs: &agentreplv1.WatchDaemonEmacs{Focus: unfocusedEditor(), ElispBuild: "elisp-test"}}}))
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
		workspace.OpenStageStartingSession,
		workspace.OpenStageReviving,
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
	wantStages := []string{"checking_worktree", "starting_session", "reviving", "clearing_closed", "checking_build"}
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
	case *agentreplv1.WorkspaceOpenStage_Reviving:
		return "reviving"
	case *agentreplv1.WorkspaceOpenStage_ClearingClosed:
		return "clearing_closed"
	case *agentreplv1.WorkspaceOpenStage_CheckingBuild:
		return "checking_build"
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
	h.Verbs.openStages = []workspace.OpenStage{workspace.OpenStageStartingSession}
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

// ownBranchSource is the requester's own branch, closed on landing.
func ownBranchSource() *agentreplv1.MergeWorkspaceSource {
	return &agentreplv1.MergeWorkspaceSource{Source: &agentreplv1.MergeWorkspaceSource_OwnBranch{OwnBranch: &agentreplv1.MergeWorkspaceSourceOwnBranch{}}}
}

func TestMergeWorkspaceMapsAPreStateRefusalOntoItsArm(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Merge.enqueueErr = &merge.RefusalError{Arm: merge.ArmAlreadyQueued, Reason: "already waiting"}

	// Act.
	resp, err := h.Client.MergeWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: ref(), Source: ownBranchSource()}))

	// Assert.
	if err != nil {
		t.Fatalf("MergeWorkspace: %v", err)
	}
	if resp.Msg.GetError().GetAlreadyQueued() == nil {
		t.Fatalf("result = %v, want already_queued", resp.Msg.GetResult())
	}
}

func TestMergeWorkspaceAsksAsTheUser(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	if _, err := h.Client.MergeWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: ref(), Source: ownBranchSource()})); err != nil {
		t.Fatalf("MergeWorkspace: %v", err)
	}

	// Assert.
	if len(h.Merge.enqueuedBy) != 1 || h.Merge.enqueuedBy[0] != merge.RequestedByUser {
		t.Fatalf("the merge was enqueued as %v, want the user's ask", h.Merge.enqueuedBy)
	}
}

func TestMergeWorkspaceMapsEachSourceArm(t *testing.T) {
	tests := []struct {
		name   string
		source *agentreplv1.MergeWorkspaceSource
		want   wsm.MergeSource
	}{
		{name: "own branch kept open", source: &agentreplv1.MergeWorkspaceSource{Source: &agentreplv1.MergeWorkspaceSource_OwnBranch{
			OwnBranch: &agentreplv1.MergeWorkspaceSourceOwnBranch{KeepOpen: true}}},
			want: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch, KeepOpen: true}},
		{name: "a branch", source: &agentreplv1.MergeWorkspaceSource{Source: &agentreplv1.MergeWorkspaceSource_Branch{
			Branch: &agentreplv1.MergeWorkspaceSourceBranch{Name: "agent-1/fix"}}},
			want: wsm.MergeSource{Kind: wsm.MergeSourceBranch, Branch: "agent-1/fix"}},
		{name: "another workspace", source: &agentreplv1.MergeWorkspaceSource{Source: &agentreplv1.MergeWorkspaceSource_Workspace{
			Workspace: &agentreplv1.MergeWorkspaceSourceWorkspace{Ref: ref()}}},
			want: wsm.MergeSource{Kind: wsm.MergeSourceWorkspace, Workspace: testWorkspaceID}},
		{name: "merged upstream", source: &agentreplv1.MergeWorkspaceSource{Source: &agentreplv1.MergeWorkspaceSource_MergedUpstream{
			MergedUpstream: &agentreplv1.MergeWorkspaceSourceMergedUpstream{}}},
			want: wsm.MergeSource{Kind: wsm.MergeSourceMergedUpstream}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			if _, err := h.Client.MergeWorkspace(context.Background(),
				connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: ref(), Source: tt.source})); err != nil {
				t.Fatalf("MergeWorkspace: %v", err)
			}

			// Assert.
			if len(h.Merge.requests) != 1 || h.Merge.requests[0].Source != tt.want {
				t.Fatalf("requests = %+v, want source %+v", h.Merge.requests, tt.want)
			}
		})
	}
}

func TestMergeWorkspaceRefusesARequestWithNoSourceAsInvalid(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.MergeWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: ref()}))

	// Assert.
	if connect.CodeOf(err) != connect.CodeInvalidArgument {
		t.Fatalf("MergeWorkspace = %v, want invalid_argument", err)
	}
	if len(h.Merge.requests) != 0 {
		t.Fatalf("an invalid request reached the queue: %+v", h.Merge.requests)
	}
}

func TestMergeWorkspaceAnswersAnUnknownSourceWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	source := &agentreplv1.MergeWorkspaceSource{Source: &agentreplv1.MergeWorkspaceSource_Workspace{
		Workspace: &agentreplv1.MergeWorkspaceSourceWorkspace{Ref: &workspacev1.WorkspaceRef{Id: "no-such-workspace"}}}}

	// Act.
	resp, err := h.Client.MergeWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: ref(), Source: source}))

	// Assert.
	if err != nil {
		t.Fatalf("MergeWorkspace: %v", err)
	}
	if resp.Msg.GetError().GetUnknownSourceWorkspace() == nil {
		t.Fatalf("result = %v, want unknown_source_workspace", resp.Msg.GetResult())
	}
}
