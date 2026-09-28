package server

import (
	"context"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/internal/workspace"
)

func TestListWorkspaceTranscriptsAnswersWhatTheVerbListed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.listTranscripts = []*agentreplv1.WorkspaceTranscript{
		{VendorSessionId: "a"}, {VendorSessionId: "b"},
	}

	// Act.
	resp, err := h.Client.ListWorkspaceTranscripts(context.Background(),
		connect.NewRequest(&agentreplv1.ListWorkspaceTranscriptsRequest{Workspace: ref()}))
	if err != nil {
		t.Fatalf("ListWorkspaceTranscripts: %v", err)
	}

	// Assert.
	got := resp.Msg.GetSuccess().GetTranscripts()
	if len(got) != 2 || got[0].GetVendorSessionId() != "a" || got[1].GetVendorSessionId() != "b" {
		t.Fatalf("transcripts = %+v, want a then b", got)
	}
}

func TestListWorkspaceTranscriptsAnswersAnEmptyListAsASuccess(t *testing.T) {
	// Arrange: a directory with no transcripts is an answer, not a failure.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.ListWorkspaceTranscripts(context.Background(),
		connect.NewRequest(&agentreplv1.ListWorkspaceTranscriptsRequest{Workspace: ref()}))
	if err != nil {
		t.Fatalf("ListWorkspaceTranscripts: %v", err)
	}

	// Assert.
	if resp.Msg.GetSuccess() == nil || len(resp.Msg.GetSuccess().GetTranscripts()) != 0 {
		t.Fatalf("result = %v, want an empty success", resp.Msg.GetResult())
	}
}

func TestListWorkspaceTranscriptsAnswersTheNoSessionArm(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.listTranscriptsErr = &workspace.Refusal{
		Rpc: "ListWorkspaceTranscripts", Arm: workspace.ArmNoSession, Reason: "no live session",
	}

	// Act.
	resp, err := h.Client.ListWorkspaceTranscripts(context.Background(),
		connect.NewRequest(&agentreplv1.ListWorkspaceTranscriptsRequest{Workspace: ref()}))
	if err != nil {
		t.Fatalf("ListWorkspaceTranscripts: %v", err)
	}

	// Assert.
	if resp.Msg.GetError().GetNoSession() == nil {
		t.Fatalf("error = %v, want the no_session arm", resp.Msg.GetError())
	}
}

func TestListWorkspaceTranscriptsAnswersTheUnreadableArmWithItsEvidence(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.listTranscriptsErr = &workspace.Refusal{
		Rpc: "ListWorkspaceTranscripts", Arm: workspace.ArmUnreadable, Reason: "EACCES",
		Fields: map[string]any{"searched_path": "/p", "detail": "EACCES"},
	}

	// Act.
	resp, err := h.Client.ListWorkspaceTranscripts(context.Background(),
		connect.NewRequest(&agentreplv1.ListWorkspaceTranscriptsRequest{Workspace: ref()}))
	if err != nil {
		t.Fatalf("ListWorkspaceTranscripts: %v", err)
	}

	// Assert: the arm carries the path and the read's own account, not only prose.
	unreadable := resp.Msg.GetError().GetUnreadable()
	if unreadable.GetSearchedPath() != "/p" || unreadable.GetDetail() != "EACCES" {
		t.Fatalf("unreadable arm = %+v", unreadable)
	}
}

func TestListWorkspaceTranscriptsRefusesARefThatNamesNoWorkspace(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	_, err := h.Client.ListWorkspaceTranscripts(context.Background(),
		connect.NewRequest(&agentreplv1.ListWorkspaceTranscriptsRequest{}))

	// Assert.
	if got := connectCode(t, err); got != connect.CodeInvalidArgument {
		t.Fatalf("code = %v, want InvalidArgument", got)
	}
}

func TestBindWorkspaceSessionResolvesTheChosenConversationOntoTheVerb(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.BindWorkspaceSession(context.Background(),
		connect.NewRequest(&agentreplv1.BindWorkspaceSessionRequest{Workspace: ref(), VendorSessionId: "b"}))
	if err != nil {
		t.Fatalf("BindWorkspaceSession: %v", err)
	}

	// Assert.
	if resp.Msg.GetSuccess() == nil || h.Verbs.bindID != "b" {
		t.Fatalf("result = %v, bound id = %q", resp.Msg.GetResult(), h.Verbs.bindID)
	}
}

func TestBindWorkspaceSessionRefusesABlankVendorSessionID(t *testing.T) {
	// Arrange: the id is an ECHO of a served value; a client may not invent
	// one, and it certainly may not invent nothing.
	h := newHarness(t)

	// Act.
	_, err := h.Client.BindWorkspaceSession(context.Background(),
		connect.NewRequest(&agentreplv1.BindWorkspaceSessionRequest{Workspace: ref()}))

	// Assert.
	if got := connectCode(t, err); got != connect.CodeInvalidArgument {
		t.Fatalf("code = %v, want InvalidArgument", got)
	}
}

func TestBindWorkspaceSessionAnswersTheUnknownTranscriptArmNamingTheID(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.bindErr = &workspace.Refusal{
		Rpc: "BindWorkspaceSession", Arm: workspace.ArmUnknownTranscript, Reason: "no such transcript",
		NotFound: true, Fields: map[string]any{"vendor_session_id": "invented"},
	}

	// Act.
	resp, err := h.Client.BindWorkspaceSession(context.Background(),
		connect.NewRequest(&agentreplv1.BindWorkspaceSessionRequest{Workspace: ref(), VendorSessionId: "invented"}))
	if err != nil {
		t.Fatalf("BindWorkspaceSession: %v", err)
	}

	// Assert.
	if resp.Msg.GetError().GetUnknownTranscript().GetVendorSessionId() != "invented" {
		t.Fatalf("error = %v, want unknown_transcript naming the id", resp.Msg.GetError())
	}
}

func TestBindWorkspaceSessionAnswersTheAlreadyBoundArm(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.bindErr = &workspace.Refusal{
		Rpc: "BindWorkspaceSession", Arm: workspace.ArmAlreadyBound, Reason: "already running it",
	}

	// Act.
	resp, err := h.Client.BindWorkspaceSession(context.Background(),
		connect.NewRequest(&agentreplv1.BindWorkspaceSessionRequest{Workspace: ref(), VendorSessionId: "a"}))
	if err != nil {
		t.Fatalf("BindWorkspaceSession: %v", err)
	}

	// Assert.
	if resp.Msg.GetError().GetAlreadyBound() == nil {
		t.Fatalf("error = %v, want the already_bound arm", resp.Msg.GetError())
	}
}

func TestBindWorkspaceSessionAnswersTheTranscriptActiveArmWithTheWriteInstant(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.bindErr = &workspace.Refusal{
		Rpc: "BindWorkspaceSession", Arm: workspace.ArmTranscriptActive, Reason: "something is writing to it",
		Fields: map[string]any{"at_ms": int64(1_700_000_000_000)},
	}

	// Act.
	resp, err := h.Client.BindWorkspaceSession(context.Background(),
		connect.NewRequest(&agentreplv1.BindWorkspaceSessionRequest{Workspace: ref(), VendorSessionId: "b"}))
	if err != nil {
		t.Fatalf("BindWorkspaceSession: %v", err)
	}

	// Assert.
	if resp.Msg.GetError().GetTranscriptActive().GetAtMs() != 1_700_000_000_000 {
		t.Fatalf("error = %v, want transcript_active carrying the instant", resp.Msg.GetError())
	}
}

func TestBindWorkspaceSessionAnswersTheTranscriptHeldArmNamingTheHolder(t *testing.T) {
	// Arrange: an arm whose evidence is itself a message.
	h := newHarness(t)
	h.Verbs.bindErr = &workspace.Refusal{
		Rpc: "BindWorkspaceSession", Arm: workspace.ArmTranscriptHeld, Reason: "w2 holds it",
		Fields: map[string]any{"workspace": &workspacev1.WorkspaceRef{Id: "w2", Dir: "/w2"}},
	}

	// Act.
	resp, err := h.Client.BindWorkspaceSession(context.Background(),
		connect.NewRequest(&agentreplv1.BindWorkspaceSessionRequest{Workspace: ref(), VendorSessionId: "b"}))
	if err != nil {
		t.Fatalf("BindWorkspaceSession: %v", err)
	}

	// Assert: the client names the holder rather than saying only that something does.
	if resp.Msg.GetError().GetTranscriptHeld().GetWorkspace().GetId() != "w2" {
		t.Fatalf("error = %v, want transcript_held naming w2", resp.Msg.GetError())
	}
}

func TestBindWorkspaceSessionAnswersTheTurnInFlightArm(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.bindErr = &workspace.Refusal{
		Rpc: "BindWorkspaceSession", Arm: workspace.ArmTurnInFlight, Reason: "a turn is running",
	}

	// Act.
	resp, err := h.Client.BindWorkspaceSession(context.Background(),
		connect.NewRequest(&agentreplv1.BindWorkspaceSessionRequest{Workspace: ref(), VendorSessionId: "b"}))
	if err != nil {
		t.Fatalf("BindWorkspaceSession: %v", err)
	}

	// Assert.
	if resp.Msg.GetError().GetTurnInFlight() == nil {
		t.Fatalf("error = %v, want the turn_in_flight arm", resp.Msg.GetError())
	}
}

func TestBindWorkspaceSessionAnswersTheStopFailedArmWithItsDetail(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.bindErr = &workspace.Refusal{
		Rpc: "BindWorkspaceSession", Arm: workspace.ArmStopFailed, Reason: "it would not die",
		Fields: map[string]any{"detail": "it would not die"},
	}

	// Act.
	resp, err := h.Client.BindWorkspaceSession(context.Background(),
		connect.NewRequest(&agentreplv1.BindWorkspaceSessionRequest{Workspace: ref(), VendorSessionId: "b"}))
	if err != nil {
		t.Fatalf("BindWorkspaceSession: %v", err)
	}

	// Assert.
	if resp.Msg.GetError().GetStopFailed().GetDetail() != "it would not die" {
		t.Fatalf("error = %v, want stop_failed carrying its detail", resp.Msg.GetError())
	}
}

func TestBindWorkspaceSessionAnswersTheStartFailedArmWithItsDetail(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.bindErr = &workspace.Refusal{
		Rpc: "BindWorkspaceSession", Arm: workspace.ArmStartFailed, Reason: "it would not come up",
		Fields: map[string]any{"detail": "it would not come up"},
	}

	// Act.
	resp, err := h.Client.BindWorkspaceSession(context.Background(),
		connect.NewRequest(&agentreplv1.BindWorkspaceSessionRequest{Workspace: ref(), VendorSessionId: "b"}))
	if err != nil {
		t.Fatalf("BindWorkspaceSession: %v", err)
	}

	// Assert.
	if resp.Msg.GetError().GetStartFailed().GetDetail() != "it would not come up" {
		t.Fatalf("error = %v, want start_failed carrying its detail", resp.Msg.GetError())
	}
}

func TestBindWorkspaceSessionRelaysTheStartingSessionStage(t *testing.T) {
	// Arrange: the one stage a bind shares with an open, and the slow one.
	h := newHarness(t)
	h.Verbs.bindStages = []workspace.BindStage{workspace.BindStageStartingSession}
	stream := proveDaemonSubscription(t, h)

	// Act.
	if _, err := h.Client.BindWorkspaceSession(context.Background(),
		connect.NewRequest(&agentreplv1.BindWorkspaceSessionRequest{
			Workspace: ref(), VendorSessionId: "b", OpId: "op-bind",
		})); err != nil {
		t.Fatalf("BindWorkspaceSession: %v", err)
	}

	// Assert.
	if !stream.Receive() {
		t.Fatalf("receive the stage: %v", stream.Err())
	}
	prog := stream.Msg().GetMutationProgress()
	if prog.GetOpId() != "op-bind" ||
		openStageArm(prog.GetOpen().GetEnteredStage()) != "starting_session" {
		t.Fatalf("progress = %v, want the starting-session stage on op-bind", prog)
	}
}

func TestBindWorkspaceSessionArmsNoReporterWithoutAnOpID(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	if _, err := h.Client.BindWorkspaceSession(context.Background(),
		connect.NewRequest(&agentreplv1.BindWorkspaceSessionRequest{Workspace: ref(), VendorSessionId: "b"})); err != nil {
		t.Fatalf("BindWorkspaceSession: %v", err)
	}

	// Assert: a bind with no op_id emits no stages.
	if h.Verbs.bindProgress != nil {
		t.Fatalf("bind progress reporter = %v, want none for a bind with no op_id", h.Verbs.bindProgress)
	}
}
