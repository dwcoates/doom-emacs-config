package server

import (
	"context"
	"errors"
	"reflect"
	"strings"
	"testing"
	"time"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
)

// ---- CreateWorkspace: topology, context and requester -------------------------

func TestCreateWorkspaceMapsItsChoicesOntoTheSpec(t *testing.T) {
	owner := ids.WorkspaceID("ws-owner")
	other := ids.WorkspaceID("ws-other")
	tests := []struct {
		name          string
		edit          func(*agentreplv1.CreateWorkspaceRequest)
		wantParent    *ids.WorkspaceID
		wantFork      *ids.WorkspaceID
		wantRequester *ids.WorkspaceID
		wantNotice    string
	}{
		{name: "a fresh workspace on the primary branch, asked by the user"},
		{name: "a fork of another workspace onto the primary branch", edit: func(r *agentreplv1.CreateWorkspaceRequest) {
			r.ContextOrigin = forkContext(&workspacev1.WorkspaceRef{Id: string(other)})
		}, wantFork: &other},
		{name: "a child of an owner that lands it, asked by a workspace", edit: func(r *agentreplv1.CreateWorkspaceRequest) {
			r.Form = &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{
				Topology: childTopology(&workspacev1.WorkspaceRef{Id: string(owner)}, "the parser fix")}}
			r.Requester = workspaceRequester(ref())
		}, wantParent: &owner, wantNotice: "the parser fix", wantRequester: func() *ids.WorkspaceID { id := testWorkspaceID; return &id }()},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			repoRef := registeredRepoRef(h)
			h.Verbs.createRec = wsm.Workspace{ID: "ws-created", Dir: "/tmp/ws", Name: "minted"}
			req := validCreate(tt.edit)
			req.Repository = repoRef

			// Act
			resp, err := h.Client.CreateWorkspace(context.Background(), connect.NewRequest(req))

			// Assert
			if err != nil || resp.Msg.GetSuccess() == nil {
				t.Fatalf("CreateWorkspace = %v, %v; want success", resp, err)
			}
			spec := h.Verbs.createSpecs[0]
			if !reflect.DeepEqual(spec.Parent, tt.wantParent) || !reflect.DeepEqual(spec.ForkFrom, tt.wantFork) ||
				!reflect.DeepEqual(spec.Requester, tt.wantRequester) || spec.OwnerNotice != tt.wantNotice {
				t.Fatalf("spec = parent %v fork %v requester %v notice %q; want %v %v %v %q",
					spec.Parent, spec.ForkFrom, spec.Requester, spec.OwnerNotice, tt.wantParent, tt.wantFork, tt.wantRequester, tt.wantNotice)
			}
		})
	}
}

// ---- CreateWorkspace: the async merge form -----------------------------------

// asyncHarness is a harness whose workspace is a workspace of repo-1, and
// whose repository is registered.
func asyncHarness(t *testing.T) (*harness, *agentreplv1.CreateWorkspaceRequest) {
	t.Helper()
	h := newHarness(t)
	repoRef := registeredRepoRef(h)
	ws := h.DB.workspaces[testWorkspaceID]
	ws.Repo = "repo-1"
	h.DB.workspaces[testWorkspaceID] = ws
	req := validAsyncMerge(nil)
	req.Repository = repoRef
	return h, req
}

func TestAnAsyncMergeIsEnqueuedWithItsMakerAndAnsweredWithItsBranch(t *testing.T) {
	// Arrange
	h, req := asyncHarness(t)
	async := req.GetAsyncMerge()
	async.Target = &agentreplv1.CreateWorkspaceAsyncMergeTarget{Target: &agentreplv1.CreateWorkspaceAsyncMergeTarget_Maker{Maker: &agentreplv1.CreateWorkspaceAsyncMergeTargetMaker{}}}
	async.Placement = frontPlacement()
	async.Instruction = strPtr(" rebase it ")
	async.Source.Branch = " agent-1/fix "

	// Act
	resp, err := h.Client.CreateWorkspace(context.Background(), connect.NewRequest(req))

	// Assert
	if err != nil || resp.Msg.GetAsyncMergeAccepted().GetMergeWorkspaceBranch() != "agent/reconnect-fix" {
		t.Fatalf("CreateWorkspace = %v, %v; want the async merge accepted under its branch", resp, err)
	}
	got := h.Merge.requests[0]
	want := merge.Request{Workspace: testWorkspaceID, By: merge.RequestedByAgent, Asker: testWorkspaceID,
		Source:   wsm.MergeSource{Kind: wsm.MergeSourceBranch, Branch: "agent-1/fix", Front: true},
		Detached: merge.NewDetachedRun("reconnect-fix", "rebase it", false, "", false, wsm.DeployHooks{}, testWorkspaceID, true)}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("request = %+v, want %+v", got, want)
	}
}

func TestAnAsyncMergeOfASubagentForksItsSpawner(t *testing.T) {
	// Arrange
	h, req := asyncHarness(t)
	h.Makers.spawners = map[string]wsm.Workspace{"a6e95de7": {ID: testWorkspaceID}}
	req.GetAsyncMerge().Source.Maker = &agentreplv1.CreateWorkspaceAsyncMergeMaker{Maker: &agentreplv1.CreateWorkspaceAsyncMergeMaker_Subagent{
		Subagent: &agentreplv1.CreateWorkspaceAsyncMergeMakerSubagent{AgentId: "a6e95de7"}}}

	// Act
	resp, err := h.Client.CreateWorkspace(context.Background(), connect.NewRequest(req))

	// Assert
	if err != nil || resp.Msg.GetAsyncMergeAccepted() == nil {
		t.Fatalf("CreateWorkspace = %v, %v; want accepted", resp, err)
	}
	if got := h.Merge.requests[0].Detached.Maker; got != testWorkspaceID {
		t.Fatalf("maker = %q, want the spawning workspace", got)
	}
}

func TestAnAsyncMergeIsRefused(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness, req *agentreplv1.CreateWorkspaceRequest)
		got     func(*agentreplv1.CreateWorkspaceError) bool
	}{
		{name: "an op id beside it", arrange: func(_ *harness, req *agentreplv1.CreateWorkspaceRequest) { req.OpId = strPtr("op-1") },
			got: func(e *agentreplv1.CreateWorkspaceError) bool { return e.GetOpIdWithAsyncMerge() != nil }},
		{name: "a requester the registry does not hold", arrange: func(_ *harness, req *agentreplv1.CreateWorkspaceRequest) {
			req.Requester = workspaceRequester(&workspacev1.WorkspaceRef{Id: "ws-none"})
		}, got: func(e *agentreplv1.CreateWorkspaceError) bool {
			return e.GetUnknownRequester().GetWorkspace().GetId() == "ws-none"
		}},
		{name: "a requester of another repository", arrange: func(h *harness, _ *agentreplv1.CreateWorkspaceRequest) {
			ws := h.DB.workspaces[testWorkspaceID]
			ws.Repo = "repo-9"
			h.DB.workspaces[testWorkspaceID] = ws
		}, got: func(e *agentreplv1.CreateWorkspaceError) bool { return e.GetUnknownRequester() != nil }},
		{name: "a subagent no session holds", arrange: func(_ *harness, req *agentreplv1.CreateWorkspaceRequest) {
			req.GetAsyncMerge().Source.Maker = &agentreplv1.CreateWorkspaceAsyncMergeMaker{Maker: &agentreplv1.CreateWorkspaceAsyncMergeMaker_Subagent{
				Subagent: &agentreplv1.CreateWorkspaceAsyncMergeMakerSubagent{AgentId: "ghost"}}}
		}, got: func(e *agentreplv1.CreateWorkspaceError) bool {
			return e.GetAsyncMergeUnknownSubagent().GetAgentId() == "ghost"
		}},
		{name: "a context that is not the maker's", arrange: func(_ *harness, req *agentreplv1.CreateWorkspaceRequest) {
			req.ContextOrigin = freshContext()
		}, got: func(e *agentreplv1.CreateWorkspaceError) bool {
			return e.GetAsyncMergeContextNotMaker().GetMaker().GetId() == string(testWorkspaceID)
		}},
		{name: "a requester whose queue row is taken", arrange: func(h *harness, _ *agentreplv1.CreateWorkspaceRequest) {
			h.Merge.enqueueErr = &merge.RefusalError{Arm: merge.ArmAlreadyQueued, Reason: "queued"}
		}, got: func(e *agentreplv1.CreateWorkspaceError) bool { return e.GetAsyncMergeRequesterQueued() != nil }},
		{name: "a requester whose own merge runs", arrange: func(h *harness, _ *agentreplv1.CreateWorkspaceRequest) {
			h.Merge.enqueueErr = &merge.RefusalError{Arm: merge.ArmAlreadyMerging, Reason: "merging"}
		}, got: func(e *agentreplv1.CreateWorkspaceError) bool { return e.GetAsyncMergeRequesterMerging() != nil }},
		{name: "a branch a workspace owns", arrange: func(h *harness, _ *agentreplv1.CreateWorkspaceRequest) {
			h.Merge.enqueueErr = &merge.RefusalError{Arm: merge.ArmBranchOwned, Reason: "owned",
				Fields: map[string]any{"branch": "agent-1/fix", "owner": &workspacev1.WorkspaceRef{Id: "ws-owner"}}}
		}, got: func(e *agentreplv1.CreateWorkspaceError) bool {
			return e.GetAsyncMergeBranchOwned().GetOwner().GetId() == "ws-owner"
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h, req := asyncHarness(t)
			tt.arrange(h, req)

			// Act
			resp, err := h.Client.CreateWorkspace(context.Background(), connect.NewRequest(req))

			// Assert
			if err != nil || !tt.got(resp.Msg.GetError()) {
				t.Fatalf("CreateWorkspace = %v, %v; want the refusal", resp, err)
			}
		})
	}
}

func TestAnAsyncMergeWhoseSpawnerCannotBeReadFails(t *testing.T) {
	// Arrange
	h, req := asyncHarness(t)
	h.Makers.err = errors.New("disk gone")
	req.GetAsyncMerge().Source.Maker = &agentreplv1.CreateWorkspaceAsyncMergeMaker{Maker: &agentreplv1.CreateWorkspaceAsyncMergeMaker_Subagent{
		Subagent: &agentreplv1.CreateWorkspaceAsyncMergeMakerSubagent{AgentId: "a6e95de7"}}}

	// Act
	_, err := h.Client.CreateWorkspace(context.Background(), connect.NewRequest(req))

	// Assert
	if err == nil || len(h.Merge.requests) != 0 {
		t.Fatalf("CreateWorkspace = %v, requests %+v; want a failure and nothing enqueued", err, h.Merge.requests)
	}
}

// ---- CloseWorkspace: who asked -----------------------------------------------

func TestCloseWorkspaceClosesAsItsRequesterAsks(t *testing.T) {
	tests := []struct {
		name      string
		requester *agentreplv1.WorkspaceRequester
		later     bool
		wantBy    []wsm.CloseCause
		wantSelf  int
		wantLater bool
	}{
		{name: "the user's close", requester: userRequester(), wantBy: []wsm.CloseCause{wsm.CloseByUser}},
		{name: "another workspace's close", requester: workspaceRequester(&workspacev1.WorkspaceRef{Id: "ws-other"}), wantBy: []wsm.CloseCause{wsm.CloseByAgent}},
		{name: "a self-close with no turn running", requester: workspaceRequester(ref()), wantSelf: 1},
		{name: "a self-close inside its turn", requester: workspaceRequester(ref()), later: true, wantSelf: 1, wantLater: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.Verbs.api.closesLater = tt.later

			// Act
			resp, err := h.Client.CloseWorkspace(context.Background(), connect.NewRequest(&agentreplv1.CloseWorkspaceRequest{Workspace: ref(), Requester: tt.requester}))

			// Assert
			if err != nil {
				t.Fatalf("CloseWorkspace: %v", err)
			}
			success := resp.Msg.GetSuccess()
			if (success.GetClosesWhenTurnEnds() != nil) != tt.wantLater || (success.GetClosed() != nil) == tt.wantLater {
				t.Fatalf("success = %v, want closes-when-turn-ends %v", success, tt.wantLater)
			}
			if !reflect.DeepEqual(h.Verbs.closedBy, tt.wantBy) || len(h.Verbs.api.selfCloses) != tt.wantSelf {
				t.Fatalf("closes %v, self-closes %v; want %v and %d", h.Verbs.closedBy, h.Verbs.api.selfCloses, tt.wantBy, tt.wantSelf)
			}
		})
	}
}

func TestCloseWorkspaceWithNoRequesterIsInvalid(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	_, err := h.Client.CloseWorkspace(context.Background(), connect.NewRequest(&agentreplv1.CloseWorkspaceRequest{Workspace: ref()}))

	// Assert
	if connect.CodeOf(err) != connect.CodeInvalidArgument || len(h.Verbs.closedBy) != 0 {
		t.Fatalf("CloseWorkspace = %v, closes %v; want invalid_argument and none", err, h.Verbs.closedBy)
	}
}

// ---- GetWorkspaceStatus and workspace subscriptions -----------------------------

func TestGetWorkspaceStatusAnswersTheVerbsStatus(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Verbs.api.status = &agentreplv1.WorkspaceStatus{Workspace: ref(), Name: "test", OneShot: true}

	// Act
	resp, err := h.Client.GetWorkspaceStatus(context.Background(), connect.NewRequest(&agentreplv1.GetWorkspaceStatusRequest{Workspace: ref()}))

	// Assert
	if err != nil || resp.Msg.GetSuccess().GetStatus().GetName() != "test" || !resp.Msg.GetSuccess().GetStatus().GetOneShot() {
		t.Fatalf("GetWorkspaceStatus = %v, %v; want the verb's status", resp, err)
	}
}

func TestGetWorkspaceStatusOfAnUnknownWorkspaceIsAnAnswer(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	resp, err := h.Client.GetWorkspaceStatus(context.Background(), connect.NewRequest(&agentreplv1.GetWorkspaceStatusRequest{
		Workspace: &workspacev1.WorkspaceRef{Id: "ws-none"}}))

	// Assert
	if err != nil || resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("GetWorkspaceStatus = %v, %v; want unknown_workspace", resp, err)
	}
}

// followHarness is a harness holding a second workspace, ws-2, to follow.
func followHarness(t *testing.T) *harness {
	t.Helper()
	h := newHarness(t)
	h.DB.workspaces["ws-2"] = wsm.Workspace{ID: "ws-2", Dir: "/tmp/ws-2", Name: "two"}
	h.Verbs.api.status = &agentreplv1.WorkspaceStatus{Workspace: &workspacev1.WorkspaceRef{Id: "ws-2"}, Name: "two"}
	return h
}

func followRequest(events ...agentreplv1.WorkspaceNoticeEvent) *connect.Request[agentreplv1.SubscribeWorkspaceRequest] {
	return connect.NewRequest(&agentreplv1.SubscribeWorkspaceRequest{Subscriber: ref(), Followed: &workspacev1.WorkspaceRef{Id: "ws-2"}, Events: events})
}

func TestSubscribeWorkspaceRecordsTheEventsAndAnswersTheStatus(t *testing.T) {
	tests := []struct {
		name      string
		replaces  bool
		wantTaken string
	}{
		{name: "a new subscription", wantTaken: "subscribed"},
		{name: "a replaced subscription", replaces: true, wantTaken: "resubscribed"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := followHarness(t)
			h.DB.replacesSub = tt.replaces

			// Act
			resp, err := h.Client.SubscribeWorkspace(context.Background(), followRequest(
				agentreplv1.WorkspaceNoticeEvent_WORKSPACE_NOTICE_EVENT_LANDED, agentreplv1.WorkspaceNoticeEvent_WORKSPACE_NOTICE_EVENT_CLOSED,
				agentreplv1.WorkspaceNoticeEvent_WORKSPACE_NOTICE_EVENT_LANDED))

			// Assert
			if err != nil {
				t.Fatalf("SubscribeWorkspace: %v", err)
			}
			success := resp.Msg.GetSuccess()
			taken := "subscribed"
			if success.GetTaken().GetResubscribed() != nil {
				taken = "resubscribed"
			}
			if taken != tt.wantTaken || success.GetStatus().GetName() != "two" {
				t.Fatalf("success = %v, want %s with the followed status", success, tt.wantTaken)
			}
			want := wsm.NewWorkspaceEvents(wsm.WorkspaceEventLanded, wsm.WorkspaceEventClosed)
			if len(h.DB.subscribed) != 1 || h.DB.subscribed[0].Events != want || h.DB.subscribed[0].Followed != "ws-2" {
				t.Fatalf("subscribed = %+v, want landed and closed of ws-2", h.DB.subscribed)
			}
		})
	}
}

func TestSubscribeWorkspaceIsRefused(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness) *connect.Request[agentreplv1.SubscribeWorkspaceRequest]
		got     func(*agentreplv1.SubscribeWorkspaceError) bool
	}{
		{name: "following itself", arrange: func(*harness) *connect.Request[agentreplv1.SubscribeWorkspaceRequest] {
			return connect.NewRequest(&agentreplv1.SubscribeWorkspaceRequest{Subscriber: ref(), Followed: ref(),
				Events: []agentreplv1.WorkspaceNoticeEvent{agentreplv1.WorkspaceNoticeEvent_WORKSPACE_NOTICE_EVENT_LANDED}})
		}, got: func(e *agentreplv1.SubscribeWorkspaceError) bool { return e.GetFollowsItself() != nil }},
		{name: "an unknown followed workspace", arrange: func(*harness) *connect.Request[agentreplv1.SubscribeWorkspaceRequest] {
			return connect.NewRequest(&agentreplv1.SubscribeWorkspaceRequest{Subscriber: ref(), Followed: &workspacev1.WorkspaceRef{Id: "ws-none"},
				Events: []agentreplv1.WorkspaceNoticeEvent{agentreplv1.WorkspaceNoticeEvent_WORKSPACE_NOTICE_EVENT_LANDED}})
		}, got: func(e *agentreplv1.SubscribeWorkspaceError) bool { return e.GetUnknownFollowed() != nil }},
		{name: "a closed subscriber", arrange: func(h *harness) *connect.Request[agentreplv1.SubscribeWorkspaceRequest] {
			h.DB.subscribeErr = wsm.ErrSubscriberClosed
			return followRequest(agentreplv1.WorkspaceNoticeEvent_WORKSPACE_NOTICE_EVENT_LANDED)
		}, got: func(e *agentreplv1.SubscribeWorkspaceError) bool { return e.GetSubscriberClosed() != nil }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := followHarness(t)
			req := tt.arrange(h)

			// Act
			resp, err := h.Client.SubscribeWorkspace(context.Background(), req)

			// Assert
			if err != nil || !tt.got(resp.Msg.GetError()) {
				t.Fatalf("SubscribeWorkspace = %v, %v; want the refusal", resp, err)
			}
		})
	}
}

func TestSubscribeWorkspaceWithAnUnspecifiedEventIsInvalid(t *testing.T) {
	// Arrange
	h := followHarness(t)

	// Act
	_, err := h.Client.SubscribeWorkspace(context.Background(), followRequest(agentreplv1.WorkspaceNoticeEvent_WORKSPACE_NOTICE_EVENT_UNSPECIFIED))

	// Assert
	if connect.CodeOf(err) != connect.CodeInvalidArgument || len(h.DB.subscribed) != 0 {
		t.Fatalf("SubscribeWorkspace = %v, subscribed %+v; want invalid_argument and nothing", err, h.DB.subscribed)
	}
}

func TestUnsubscribeWorkspaceAnswersWhetherOneStood(t *testing.T) {
	for _, ends := range []bool{true, false} {
		t.Run(map[bool]string{true: "a subscription ended", false: "none held"}[ends], func(t *testing.T) {
			// Arrange
			h := followHarness(t)
			h.DB.endsSub = ends

			// Act
			resp, err := h.Client.UnsubscribeWorkspace(context.Background(), connect.NewRequest(&agentreplv1.UnsubscribeWorkspaceRequest{
				Subscriber: ref(), Followed: &workspacev1.WorkspaceRef{Id: "ws-2"}}))

			// Assert
			if err != nil || (resp.Msg.GetSuccess().GetUnsubscribed() != nil) != ends {
				t.Fatalf("UnsubscribeWorkspace = %v, %v; want unsubscribed %v", resp, err, ends)
			}
		})
	}
}

// ---- CompactWorkspace and agent-sent slash commands -----------------------------

func TestCompactWorkspaceSubmitsTheCompactionDeferred(t *testing.T) {
	tests := []struct {
		name         string
		instructions *agentreplv1.CompactWorkspaceInstructions
		want         string
	}{
		{name: "with no instructions", want: "/compact"},
		{name: "with instructions", instructions: &agentreplv1.CompactWorkspaceInstructions{Text: " keep the plan "}, want: "/compact keep the plan"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			resp, err := h.Client.CompactWorkspace(context.Background(), connect.NewRequest(&agentreplv1.CompactWorkspaceRequest{
				Workspace: ref(), Requester: workspaceRequester(ref()), Instructions: tt.instructions}))

			// Assert
			if err != nil || resp.Msg.GetSuccess() == nil {
				t.Fatalf("CompactWorkspace = %v, %v; want success", resp, err)
			}
			text := h.Prompts.lastSaid.GetContent().GetBlocks()[0].GetText().GetText()
			if text != tt.want || h.Prompts.lastOrigin != conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_COMPACT || h.Prompts.lastDelivery != wsm.DeliveryDeferred {
				t.Fatalf("submitted %q as %s %v; want %q deferred as a workspace compaction", text, h.Prompts.lastOrigin, h.Prompts.lastDelivery, tt.want)
			}
		})
	}
}

func TestCompactWorkspaceIsRefused(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		got     func(*agentreplv1.CompactWorkspaceError) bool
	}{
		{name: "a closed workspace", arrange: func(h *harness) {
			ws := h.DB.workspaces[testWorkspaceID]
			ws.Closed = true
			h.DB.workspaces[testWorkspaceID] = ws
		}, got: func(e *agentreplv1.CompactWorkspaceError) bool { return e.GetClosed() != nil }},
		{name: "a session at its cold gate", arrange: func(h *harness) {
			h.Prompts.err = &workspace.Refusal{Arm: "cold_gate", Reason: "cold"}
		}, got: func(e *agentreplv1.CompactWorkspaceError) bool { return e.GetColdGate() != nil }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			tt.arrange(h)

			// Act
			resp, err := h.Client.CompactWorkspace(context.Background(), connect.NewRequest(&agentreplv1.CompactWorkspaceRequest{
				Workspace: ref(), Requester: userRequester()}))

			// Assert
			if err != nil || !tt.got(resp.Msg.GetError()) {
				t.Fatalf("CompactWorkspace = %v, %v; want the refusal", resp, err)
			}
		})
	}
}

func TestAnAgentSentSlashCommandIsRefused(t *testing.T) {
	tests := []struct {
		name    string
		origin  conversationv1.PromptOrigin
		text    string
		refused string
	}{
		{name: "an agent's /clear", origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT, text: "  /clear now", refused: "/clear"},
		{name: "an agent's ordinary prompt", origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT, text: "please look at /clear"},
		{name: "the user's /clear", origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT, text: "/clear"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			sentBy := registerSender(h)
			if tt.origin != conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT {
				sentBy = nil
			}

			// Act
			resp, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(&agentreplv1.SubmitPromptRequest{
				Workspace: ref(), Said: workspace.SaidText(tt.text), IdempotencyKey: "k-1", Origin: tt.origin, SentBy: sentBy}))

			// Assert
			if err != nil {
				t.Fatalf("SubmitPrompt: %v", err)
			}
			got := resp.Msg.GetError().GetSlashCommandFromAgent().GetCommand()
			if got != tt.refused || (tt.refused != "") == (h.Prompts.submits == 1) {
				t.Fatalf("refused %q, submits %d; want refused %q", got, h.Prompts.submits, tt.refused)
			}
		})
	}
}

// senderWorkspaceID is the workspace registerSender registers as the sender of
// agent-sent prompts.
const senderWorkspaceID ids.WorkspaceID = "ws-sender"

// registerSender registers the sending workspace in H's store and answers its
// ref.
func registerSender(h *harness) *workspacev1.WorkspaceRef {
	h.DB.workspaces[senderWorkspaceID] = wsm.Workspace{ID: senderWorkspaceID, Dir: "/tmp/sender", Name: "sender"}
	return &workspacev1.WorkspaceRef{Id: string(senderWorkspaceID), Dir: "/tmp/sender"}
}

// submitAgentPrompt submits an agent-sent prompt naming SENTBY to the harness
// workspace.
func submitAgentPrompt(h *harness, origin conversationv1.PromptOrigin, sentBy *workspacev1.WorkspaceRef) (*connect.Response[agentreplv1.SubmitPromptResponse], error) {
	return h.Client.SubmitPrompt(context.Background(), connect.NewRequest(&agentreplv1.SubmitPromptRequest{
		Workspace: ref(), Said: workspace.SaidText("hello"), IdempotencyKey: "k-1", Origin: origin, SentBy: sentBy}))
}

func TestSubmitPromptNamesItsSenderExactlyWhenAnAgentSentIt(t *testing.T) {
	agentSent := conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT
	userSent := conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT
	tests := []struct {
		name   string
		origin conversationv1.PromptOrigin
		named  bool
	}{
		{name: "an agent-sent prompt naming no sender", origin: agentSent, named: false},
		{name: "a user's prompt naming a sender", origin: userSent, named: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			var sentBy *workspacev1.WorkspaceRef
			if tt.named {
				sentBy = registerSender(h)
			}

			// Act
			_, err := submitAgentPrompt(h, tt.origin, sentBy)

			// Assert
			if code := connectCode(t, err); code != connect.CodeInvalidArgument || h.Prompts.submits != 0 {
				t.Fatalf("code %v, submits %d; want InvalidArgument and nothing submitted", code, h.Prompts.submits)
			}
		})
	}
}

func TestSubmitPromptHandsTheSenderToTheHandler(t *testing.T) {
	// Arrange
	h := newHarness(t)
	sentBy := registerSender(h)

	// Act
	resp, err := submitAgentPrompt(h, conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT, sentBy)

	// Assert
	if err != nil || resp.Msg.GetError() != nil {
		t.Fatalf("SubmitPrompt: %v / %v", err, resp.Msg.GetError())
	}
	if got := h.Prompts.lastSentBy; got == nil || *got != senderWorkspaceID {
		t.Fatalf("the handler was handed sender %v, want %s", got, senderWorkspaceID)
	}
}

func TestSubmitPromptDrawsTheSendersRowWithHowTheSendFared(t *testing.T) {
	refusedWhy := func(fate feed.SentFate) string {
		if fate.Refused == nil {
			return ""
		}
		return *fate.Refused
	}
	tests := []struct {
		name        string
		text        string
		outcome     prompthandler.Outcome
		err         error
		wantQueued  bool
		wantRefused string
	}{
		{name: "delivered at once", text: "hello", outcome: prompthandler.Outcome{Turn: "t1", Disposition: promptqueue.Disposition{Delivered: true}}},
		{name: "held behind the recipient's work", text: "hello", outcome: prompthandler.Outcome{Turn: "t1", Disposition: promptqueue.Disposition{Held: func() *wsm.HoldKind { k := wsm.HoldMerge; return &k }()}}, wantQueued: true},
		{name: "refused by the queue", text: "hello", err: &promptqueue.ColdGateRefusal{Detail: "the conversation is cold"}, wantRefused: "cold"},
		{name: "refused as an agent's slash command", text: "/clear", wantRefused: "/clear"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			sentBy := registerSender(h)
			h.Prompts.outcome, h.Prompts.err = tt.outcome, tt.err

			// Act
			_, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(&agentreplv1.SubmitPromptRequest{
				Workspace: ref(), Said: workspace.SaidText(tt.text), IdempotencyKey: "k-1",
				Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT, SentBy: sentBy}))

			// Assert
			if err != nil {
				t.Fatalf("SubmitPrompt: %v", err)
			}
			rows := h.Feed.sentRows()
			if len(rows) != 1 || rows[0].sender != senderWorkspaceID || rows[0].recipient != "test" || rows[0].key != "k-1" {
				t.Fatalf("sent rows = %+v, want one in the sender's feed naming the recipient", rows)
			}
			if rows[0].fate.Queued != tt.wantQueued || !strings.Contains(refusedWhy(rows[0].fate), tt.wantRefused) || (tt.wantRefused == "") != (rows[0].fate.Refused == nil) {
				t.Fatalf("fate = %+v, want queued %v and refused %q", rows[0].fate, tt.wantQueued, tt.wantRefused)
			}
		})
	}
}

func TestSubmitPromptDrawsNoSentRowForAUsersPrompt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Prompts.outcome = prompthandler.Outcome{Turn: "t1", Disposition: promptqueue.Disposition{Delivered: true}}

	// Act
	if _, err := submitAgentPrompt(h, conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT, nil); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Assert
	if rows := h.Feed.sentRows(); len(rows) != 0 {
		t.Fatalf("sent rows = %+v, want none", rows)
	}
}

func TestSubmitPromptRefusesASenderThatIsNoOtherWorkspace(t *testing.T) {
	tests := []struct {
		name   string
		sentBy *workspacev1.WorkspaceRef
	}{
		{name: "a sender no workspace is registered as", sentBy: &workspacev1.WorkspaceRef{Id: "ws-gone", Dir: "/tmp/gone"}},
		{name: "the recipient itself", sentBy: ref()},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			resp, err := submitAgentPrompt(h, conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT, tt.sentBy)

			// Assert
			if err != nil {
				t.Fatalf("SubmitPrompt: %v", err)
			}
			if got := resp.Msg.GetError().GetUnknownSender().GetSentBy().GetId(); got != tt.sentBy.GetId() || h.Prompts.submits != 0 {
				t.Fatalf("unknown_sender names %q, submits %d; want %q and nothing submitted", got, h.Prompts.submits, tt.sentBy.GetId())
			}
		})
	}
}

func TestSubmitPromptSurfacesAnUnreadableSender(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	h := newHarness(t)
	h.Surfaces.workspace = log
	sentBy := registerSender(h)
	h.DB.workspaceErrs = map[ids.WorkspaceID]error{senderWorkspaceID: errors.New("database is locked")}

	// Act
	_, err := submitAgentPrompt(h, conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT, sentBy)

	// Assert
	if err == nil || h.Prompts.submits != 0 {
		t.Fatalf("err %v, submits %d; want a failure and nothing submitted", err, h.Prompts.submits)
	}
	for _, record := range log.Records() {
		if record.Level == "error" && record.Operation == opSubmit && record.Context["sent_by"] == string(senderWorkspaceID) && record.Context["cause"] == "database is locked" {
			return
		}
	}
	t.Fatalf("no error record named the unreadable sender: %+v", log.Records())
}

// ---- EvaluateInEditor ----------------------------------------------------------

// evaluating starts an evaluation of TEXT for the harness workspace, with its
// editor's host stream held, and answers the push the editor received and
// the channel the rpc's answer arrives on.
func evaluating(t *testing.T, h *harness, text string) (*agentreplv1.HostEvaluate, <-chan *connect.Response[agentreplv1.EvaluateInEditorResponse]) {
	t.Helper()
	s := h.Server.(*server)
	ctx, cancel := context.WithCancel(context.Background())
	t.Cleanup(cancel)
	pushes := s.hostEventStream(testWorkspaceID).Subscribe(ctx)
	s.holdParticipant(testWorkspaceID, true, 1)
	answers := make(chan *connect.Response[agentreplv1.EvaluateInEditorResponse], 1)
	go func() {
		resp, err := h.Client.EvaluateInEditor(context.Background(), connect.NewRequest(&agentreplv1.EvaluateInEditorRequest{Workspace: ref(), Text: text}))
		if err != nil {
			t.Errorf("EvaluateInEditor: %v", err)
		}
		answers <- resp
	}()
	push := <-pushes
	return push.GetEvaluate(), answers
}

func TestAnEvaluationIsAnsweredWithTheEditorsOutcome(t *testing.T) {
	// Arrange
	h := newHarness(t)
	push, answers := evaluating(t, h, "(+ 1 2)")
	outcome := &agentreplv1.EditorEvaluationOutcome{Outcome: &agentreplv1.EditorEvaluationOutcome_Value{Value: &agentreplv1.EditorEvaluationValue{Printed: "3", Readable: true}},
		Messages: []string{"done"}}

	// Act
	answer, err := h.Client.AnswerEditorEvaluation(context.Background(), connect.NewRequest(&agentreplv1.AnswerEditorEvaluationRequest{
		Workspace: ref(), Token: push.GetToken(), Outcome: outcome}))

	// Assert
	if err != nil || answer.Msg.GetSuccess() == nil {
		t.Fatalf("AnswerEditorEvaluation = %v, %v; want success", answer, err)
	}
	if push.GetText() != "(+ 1 2)" {
		t.Errorf("pushed text = %q, want the request's", push.GetText())
	}
	resp := <-answers
	if got := resp.Msg.GetSuccess().GetOutcome(); got.GetValue().GetPrinted() != "3" || !reflect.DeepEqual(got.GetMessages(), []string{"done"}) {
		t.Fatalf("outcome = %v, want the editor's", got)
	}
}

func TestAnEvaluationWithNoEditorIsAnAnswer(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	resp, err := h.Client.EvaluateInEditor(context.Background(), connect.NewRequest(&agentreplv1.EvaluateInEditorRequest{Workspace: ref(), Text: "(+ 1 2)"}))

	// Assert
	if err != nil || resp.Msg.GetError().GetNoEditor() == nil {
		t.Fatalf("EvaluateInEditor = %v, %v; want no_editor", resp, err)
	}
}

func TestAnEvaluationWhoseEditorLeavesIsAnswered(t *testing.T) {
	// Arrange
	h := newHarness(t)
	_, answers := evaluating(t, h, "(sleep-for 60)")

	// Act
	h.Server.(*server).holdParticipant(testWorkspaceID, true, -1)

	// Assert
	if resp := <-answers; resp.Msg.GetError().GetNoEditor() == nil {
		t.Fatalf("EvaluateInEditor = %v, want no_editor", resp)
	}
}

func TestAnEvaluationTheEditorNeverAnswersTimesOut(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Server.(*server).evalDeadline = time.Millisecond

	// Act
	_, answers := evaluating(t, h, "(sleep-for 60)")

	// Assert
	if resp := <-answers; resp.Msg.GetError().GetTimedOut() == nil || resp.Msg.GetError().GetTimedOut().GetDeadlineMs() != 1 {
		t.Fatalf("EvaluateInEditor = %v, want timed_out at the deadline", resp)
	}
}

func TestAnAnswerNobodyWaitsOnIsTakenOrRefused(t *testing.T) {
	tests := []struct {
		name    string
		token   func(h *harness) string
		refused bool
	}{
		{name: "a late answer to a token this daemon minted", token: func(h *harness) string {
			token, _ := h.Server.(*server).evals.open(testWorkspaceID)
			h.Server.(*server).evals.end(token)
			return token
		}},
		{name: "a token this daemon never minted", token: func(*harness) string { return "eval-elsewhere-1" }, refused: true},
		{name: "a token of this daemon not yet minted", token: func(h *harness) string {
			return h.Server.(*server).evals.prefix + "-99"
		}, refused: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			token := tt.token(h)

			// Act
			resp, err := h.Client.AnswerEditorEvaluation(context.Background(), connect.NewRequest(&agentreplv1.AnswerEditorEvaluationRequest{
				Workspace: ref(), Token: &agentreplv1.EditorEvaluationToken{Value: token},
				Outcome: &agentreplv1.EditorEvaluationOutcome{Outcome: &agentreplv1.EditorEvaluationOutcome_ReadError{ReadError: &agentreplv1.EditorEvaluationReadError{Message: "eof"}}}}))

			// Assert
			if err != nil || (resp.Msg.GetError().GetUnknownToken() != nil) != tt.refused {
				t.Fatalf("AnswerEditorEvaluation = %v, %v; want refused %v", resp, err, tt.refused)
			}
		})
	}
}

// ---- The standings -------------------------------------------------------------

func TestSetWorkspaceOneShotHandsTheVerbItsAskAndAnswersTheDirective(t *testing.T) {
	tests := []struct {
		name          string
		oneShot       bool
		wantDirective bool
	}{
		{name: "a mark", oneShot: true, wantDirective: true},
		{name: "an unmark"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.Verbs.api.directive = "when you're all done, land it"

			// Act
			resp, err := h.Client.SetWorkspaceOneShot(context.Background(), connect.NewRequest(&agentreplv1.SetWorkspaceOneShotRequest{
				Workspace: ref(), Requester: workspaceRequester(ref()), OneShot: tt.oneShot}))

			// Assert
			if err != nil || (resp.Msg.GetSuccess().CompletionDirective != nil) != tt.wantDirective {
				t.Fatalf("SetWorkspaceOneShot = %v, %v; want a directive %v", resp, err, tt.wantDirective)
			}
			got := h.Verbs.api.oneShots
			if len(got) != 1 || got[0].oneShot != tt.oneShot || got[0].asker == nil || *got[0].asker != testWorkspaceID {
				t.Fatalf("one-shots = %+v, want one asked by the workspace", got)
			}
		})
	}
}

func TestSetWorkspaceOneShotAnswersTheRosterRevisionThatCarriesTheChange(t *testing.T) {
	// Arrange -- a sidebar that drew a drag into the one-shot subsection
	// holds its drawing until a push at this revision.
	h := newHarness(t)
	h.Deps.Sidebar.(*fakeSidebar).revision = 42

	// Act
	resp, err := h.Client.SetWorkspaceOneShot(context.Background(), connect.NewRequest(&agentreplv1.SetWorkspaceOneShotRequest{
		Workspace: ref(), Requester: userRequester(), OneShot: true}))

	// Assert
	if err != nil || resp.Msg.GetSuccess().GetRosterRevision() != 42 {
		t.Fatalf("SetWorkspaceOneShot = %v, %v; want success at the sidebar's revision 42", resp, err)
	}
}

func TestSetWorkspaceOneShotAnswersTheVerbsRefusal(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Verbs.api.oneShotErr = &workspace.Refusal{Arm: workspace.ArmOwnerLands, Reason: "owner lands", Fields: map[string]any{"owner": &workspacev1.WorkspaceRef{Id: "ws-owner"}}}

	// Act
	resp, err := h.Client.SetWorkspaceOneShot(context.Background(), connect.NewRequest(&agentreplv1.SetWorkspaceOneShotRequest{
		Workspace: ref(), Requester: userRequester(), OneShot: true}))

	// Assert
	if err != nil || resp.Msg.GetError().GetOwnerLands().GetOwner().GetId() != "ws-owner" {
		t.Fatalf("SetWorkspaceOneShot = %v, %v; want owner_lands naming the owner", resp, err)
	}
}

func TestSetWorkspaceCorrespondenceHandsTheVerbItsMark(t *testing.T) {
	awaited := ids.WorkspaceID("ws-planner")
	tests := []struct {
		name string
		req  *agentreplv1.SetWorkspaceCorrespondenceRequest
		want *ids.WorkspaceID
	}{
		{name: "awaiting a workspace", req: &agentreplv1.SetWorkspaceCorrespondenceRequest{Workspace: ref(), Requester: workspaceRequester(ref()),
			Mark: &agentreplv1.SetWorkspaceCorrespondenceRequest_Awaiting{Awaiting: &agentreplv1.SetWorkspaceCorrespondenceAwaiting{Awaited: &workspacev1.WorkspaceRef{Id: string(awaited)}}}}},
		{name: "clearing", req: &agentreplv1.SetWorkspaceCorrespondenceRequest{Workspace: ref(), Requester: workspaceRequester(ref()),
			Mark: &agentreplv1.SetWorkspaceCorrespondenceRequest_Clear{Clear: &agentreplv1.SetWorkspaceCorrespondenceClear{}}}},
	}
	tests[0].want = &awaited
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			resp, err := h.Client.SetWorkspaceCorrespondence(context.Background(), connect.NewRequest(tt.req))

			// Assert
			if err != nil || resp.Msg.GetSuccess() == nil {
				t.Fatalf("SetWorkspaceCorrespondence = %v, %v; want success", resp, err)
			}
			got := h.Verbs.api.awaits
			if len(got) != 1 || !reflect.DeepEqual(got[0].awaited, tt.want) {
				t.Fatalf("awaits = %+v, want awaiting %v", got, tt.want)
			}
		})
	}
}

func TestSetWorkspaceCorrespondenceWithNoMarkIsInvalid(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	_, err := h.Client.SetWorkspaceCorrespondence(context.Background(), connect.NewRequest(&agentreplv1.SetWorkspaceCorrespondenceRequest{
		Workspace: ref(), Requester: userRequester()}))

	// Assert
	if connect.CodeOf(err) != connect.CodeInvalidArgument || len(h.Verbs.api.awaits) != 0 {
		t.Fatalf("SetWorkspaceCorrespondence = %v, awaits %+v; want invalid_argument and none", err, h.Verbs.api.awaits)
	}
}

func TestAnAgentSentPromptNeitherQuotesNorEndsTheUsersSelection(t *testing.T) {
	// Arrange: the user has a response selected in the recipient's feed.
	h := newHarness(t)
	sentBy := registerSender(h)
	h.Feed.finals = feedIDs("resp-1-old", "resp-2-new")
	h.Feed.markdown = map[string]string{"resp-2-new": "prior answer"}
	h.Prompts.outcome = prompthandler.Outcome{Turn: "t1", Disposition: promptqueue.Disposition{Delivered: true}}
	if _, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(boundaryStep(older))); err != nil {
		t.Fatalf("seed selection: %v", err)
	}

	// Act
	if _, err := submitAgentPrompt(h, conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT, sentBy); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Assert: the prompt went as the agent wrote it, and the selection stands,
	// so an OLDER steps on from it to the older response.
	if got := promptText(h.Prompts.lastSaid); got != "hello" {
		t.Fatalf("submitted = %q, want the agent's text with no quote", got)
	}
	resp, err := h.Client.SelectFeedRow(context.Background(), connect.NewRequest(boundaryStep(older)))
	if err != nil {
		t.Fatalf("SelectFeedRow: %v", err)
	}
	if got := resp.Msg.GetSuccess().GetSelected().GetSelection().GetResponse().GetRow().GetValue(); got != "resp-1-old" {
		t.Fatalf("selected = %q, want resp-1-old (the user's selection survived)", got)
	}
}

func TestAnAsyncMergeCarriesABehindPlacement(t *testing.T) {
	// Arrange
	h, req := asyncHarness(t)
	req.GetAsyncMerge().Placement = behindPlacement("two")

	// Act
	_, err := h.Client.CreateWorkspace(context.Background(), connect.NewRequest(req))

	// Assert
	if err != nil || len(h.Merge.requests) != 1 || h.Merge.requests[0].PlaceBehind != "two" {
		t.Fatalf("CreateWorkspace = %v, requests %+v; want the async merge placed behind two", err, h.Merge.requests)
	}
}

func TestAnAsyncMergePlacedBehindAMergeNotInLineIsRefused(t *testing.T) {
	// Arrange
	h, req := asyncHarness(t)
	req.GetAsyncMerge().Placement = behindPlacement("two")
	h.Merge.enqueueErr = &merge.RefusalError{Arm: merge.ArmPlacementBehindNotInLine, Reason: "none", Fields: map[string]any{"behind": "two"}}

	// Act
	resp, err := h.Client.CreateWorkspace(context.Background(), connect.NewRequest(req))

	// Assert
	if err != nil || resp.Msg.GetError().GetPlacementBehindNotInLine().GetBehind() != "two" {
		t.Fatalf("CreateWorkspace = %v, %v; want the placement_behind_not_in_line arm", resp, err)
	}
}
