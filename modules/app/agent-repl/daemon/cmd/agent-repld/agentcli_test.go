package main

import (
	"bytes"
	"context"
	"errors"
	"os"
	"regexp"
	"strings"
	"testing"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/sourcescan"
)

// cliClient is a daemon client that records every request and answers the
// scripted responses.
type cliClient struct {
	agentreplv1connect.AgentReplClient
	requests []proto.Message
	err      error
	status   *agentreplv1.GetWorkspaceStatusResponse
	create   *agentreplv1.CreateWorkspaceResponse
	finish   *agentreplv1.FinishWorkspaceResponse
	queues   *agentreplv1.GetMergeQueueStatusResponse
}

func (c *cliClient) record(req proto.Message) error {
	c.requests = append(c.requests, req)
	return c.err
}

func (c *cliClient) CreateWorkspace(_ context.Context, req *connect.Request[agentreplv1.CreateWorkspaceRequest]) (*connect.Response[agentreplv1.CreateWorkspaceResponse], error) {
	if err := c.record(req.Msg); err != nil {
		return nil, err
	}
	if c.create != nil {
		return connect.NewResponse(c.create), nil
	}
	return connect.NewResponse(&agentreplv1.CreateWorkspaceResponse{Result: &agentreplv1.CreateWorkspaceResponse_Success{Success: &agentreplv1.CreateWorkspaceSuccess{}}}), nil
}

func (c *cliClient) FinishWorkspace(_ context.Context, req *connect.Request[agentreplv1.FinishWorkspaceRequest]) (*connect.Response[agentreplv1.FinishWorkspaceResponse], error) {
	if err := c.record(req.Msg); err != nil {
		return nil, err
	}
	if c.finish != nil {
		return connect.NewResponse(c.finish), nil
	}
	return connect.NewResponse(&agentreplv1.FinishWorkspaceResponse{Result: &agentreplv1.FinishWorkspaceResponse_Success{Success: &agentreplv1.FinishWorkspaceSuccess{}}}), nil
}

func (c *cliClient) GetWorkspaceStatus(_ context.Context, req *connect.Request[agentreplv1.GetWorkspaceStatusRequest]) (*connect.Response[agentreplv1.GetWorkspaceStatusResponse], error) {
	if err := c.record(req.Msg); err != nil {
		return nil, err
	}
	if c.status != nil {
		return connect.NewResponse(c.status), nil
	}
	return connect.NewResponse(&agentreplv1.GetWorkspaceStatusResponse{Result: &agentreplv1.GetWorkspaceStatusResponse_Success{Success: &agentreplv1.GetWorkspaceStatusSuccess{
		Status: &agentreplv1.WorkspaceStatus{Repository: &workspacev1.RepositoryRef{Id: "repo-1"}}}}}), nil
}

func (c *cliClient) SetWorkspaceHidden(_ context.Context, req *connect.Request[agentreplv1.SetWorkspaceHiddenRequest]) (*connect.Response[agentreplv1.SetWorkspaceHiddenResponse], error) {
	return connect.NewResponse(&agentreplv1.SetWorkspaceHiddenResponse{}), c.record(req.Msg)
}

func (c *cliClient) SetWorkspaceOneShot(_ context.Context, req *connect.Request[agentreplv1.SetWorkspaceOneShotRequest]) (*connect.Response[agentreplv1.SetWorkspaceOneShotResponse], error) {
	return connect.NewResponse(&agentreplv1.SetWorkspaceOneShotResponse{}), c.record(req.Msg)
}

func (c *cliClient) SetWorkspaceCorrespondence(_ context.Context, req *connect.Request[agentreplv1.SetWorkspaceCorrespondenceRequest]) (*connect.Response[agentreplv1.SetWorkspaceCorrespondenceResponse], error) {
	return connect.NewResponse(&agentreplv1.SetWorkspaceCorrespondenceResponse{}), c.record(req.Msg)
}

func (c *cliClient) SubmitPrompt(_ context.Context, req *connect.Request[agentreplv1.SubmitPromptRequest]) (*connect.Response[agentreplv1.SubmitPromptResponse], error) {
	return connect.NewResponse(&agentreplv1.SubmitPromptResponse{}), c.record(req.Msg)
}

func (c *cliClient) KillWorkspace(_ context.Context, req *connect.Request[agentreplv1.KillWorkspaceRequest]) (*connect.Response[agentreplv1.KillWorkspaceResponse], error) {
	return connect.NewResponse(&agentreplv1.KillWorkspaceResponse{}), c.record(req.Msg)
}

func (c *cliClient) SelectWorkspace(_ context.Context, req *connect.Request[agentreplv1.SelectWorkspaceRequest]) (*connect.Response[agentreplv1.SelectWorkspaceResponse], error) {
	return connect.NewResponse(&agentreplv1.SelectWorkspaceResponse{}), c.record(req.Msg)
}

func (c *cliClient) RestartWorkspace(_ context.Context, req *connect.Request[agentreplv1.RestartWorkspaceRequest]) (*connect.Response[agentreplv1.RestartWorkspaceResponse], error) {
	return connect.NewResponse(&agentreplv1.RestartWorkspaceResponse{}), c.record(req.Msg)
}

func (c *cliClient) Interrupt(_ context.Context, req *connect.Request[agentreplv1.InterruptRequest]) (*connect.Response[agentreplv1.InterruptResponse], error) {
	return connect.NewResponse(&agentreplv1.InterruptResponse{}), c.record(req.Msg)
}

func (c *cliClient) SubscribeWorkspace(_ context.Context, req *connect.Request[agentreplv1.SubscribeWorkspaceRequest]) (*connect.Response[agentreplv1.SubscribeWorkspaceResponse], error) {
	return connect.NewResponse(&agentreplv1.SubscribeWorkspaceResponse{}), c.record(req.Msg)
}

func (c *cliClient) AnswerColdGate(_ context.Context, req *connect.Request[agentreplv1.AnswerColdGateRequest]) (*connect.Response[agentreplv1.AnswerColdGateResponse], error) {
	return connect.NewResponse(&agentreplv1.AnswerColdGateResponse{}), c.record(req.Msg)
}

func (c *cliClient) EvaluateInEditor(_ context.Context, req *connect.Request[agentreplv1.EvaluateInEditorRequest]) (*connect.Response[agentreplv1.EvaluateInEditorResponse], error) {
	return connect.NewResponse(&agentreplv1.EvaluateInEditorResponse{}), c.record(req.Msg)
}

func (c *cliClient) UpdateDeployHold(_ context.Context, req *connect.Request[agentreplv1.UpdateDeployHoldRequest]) (*connect.Response[agentreplv1.UpdateDeployHoldResponse], error) {
	return connect.NewResponse(&agentreplv1.UpdateDeployHoldResponse{}), c.record(req.Msg)
}

func (c *cliClient) RollOutLanding(_ context.Context, req *connect.Request[agentreplv1.RollOutLandingRequest]) (*connect.Response[agentreplv1.RollOutLandingResponse], error) {
	return connect.NewResponse(&agentreplv1.RollOutLandingResponse{}), c.record(req.Msg)
}

func (c *cliClient) GetMergeQueueStatus(_ context.Context, req *connect.Request[agentreplv1.GetMergeQueueStatusRequest]) (*connect.Response[agentreplv1.GetMergeQueueStatusResponse], error) {
	if err := c.record(req.Msg); err != nil {
		return nil, err
	}
	if c.queues != nil {
		return connect.NewResponse(c.queues), nil
	}
	return connect.NewResponse(&agentreplv1.GetMergeQueueStatusResponse{Result: &agentreplv1.GetMergeQueueStatusResponse_Success{Success: &agentreplv1.GetMergeQueueStatusSuccess{}}}), nil
}

func (c *cliClient) UpdateMergeQueue(_ context.Context, req *connect.Request[agentreplv1.UpdateMergeQueueRequest]) (*connect.Response[agentreplv1.UpdateMergeQueueResponse], error) {
	return connect.NewResponse(&agentreplv1.UpdateMergeQueueResponse{}), c.record(req.Msg)
}

// cliFixture is a daemon serving a roster of the calling workspace "caller"
// at /wt/caller, its peer "peer" at /wt/peer, and two workspaces named "twin".
type cliFixture struct {
	client *cliClient
	cwd    string
	out    bytes.Buffer
	errOut bytes.Buffer
	// dialOptions counts the client options the command dialed with.
	dialOptions int
}

func newCLIFixture() *cliFixture {
	return &cliFixture{client: &cliClient{}, cwd: "/wt/caller/src"}
}

func cliRow(id, dir, name string) *frontendv1.RosterRow {
	return &frontendv1.RosterRow{Workspace: &frontendv1.RosterRowWorkspace{Workspace: &workspacev1.WorkspaceRef{Id: id, Dir: dir}},
		Name: &frontendv1.RosterRowName{Text: name}}
}

func (f *cliFixture) run(noun string, args ...string) int {
	deps := cliDeps{
		address: func(string) (string, error) { return "127.0.0.1:1", nil },
		client: func(_ string, options ...connect.ClientOption) agentreplv1connect.AgentReplClient {
			f.dialOptions = len(options)
			return f.client
		},
		roster: func(context.Context, agentreplv1connect.AgentReplClient) (*frontendv1.WorkspaceRoster, error) {
			return roster(cliRow("ws-caller", "/wt/caller", "caller"), cliRow("ws-peer", "/wt/peer", "peer"),
				cliRow("ws-twin-1", "/wt/twin-1", "twin"), cliRow("ws-twin-2", "/wt/twin-2", "twin")), nil
		},
		getwd:   func() (string, error) { return f.cwd, nil },
		mintKey: func(prefix string) (string, error) { return prefix + "-1", nil },
	}
	return runNoun(context.Background(), noun, args, deps, &f.out, &f.errOut)
}

// only answers the one request the client was sent, of type T.
func only[T proto.Message](t *testing.T, f *cliFixture) T {
	t.Helper()
	var found []T
	for _, req := range f.client.requests {
		if typed, ok := req.(T); ok {
			found = append(found, typed)
		}
	}
	if len(found) != 1 {
		t.Fatalf("requests = %v, want exactly one %T", f.client.requests, *new(T))
	}
	return found[0]
}

func TestANounWithNoCommandPrintsItsCommands(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	code := f.run(workspaceNoun, "teleport")

	// Assert
	if code != exitFailure || !strings.Contains(f.errOut.String(), "agent-repld workspace create") {
		t.Fatalf("exit %d, stderr %q; want the usage of every command", code, f.errOut.String())
	}
}

func TestCreateSendsEveryChoiceItNames(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	code := f.run(workspaceNoun, "create", "-repository", "here", "-topology", "child", "-owner", "peer", "-owner-notice", "the fix",
		"-context", "fork", "-fork", "here", "-prompt", "fix the parser")

	// Assert
	if code != exitSuccess {
		t.Fatalf("exit %d, stderr %q", code, f.errOut.String())
	}
	req := only[*agentreplv1.CreateWorkspaceRequest](t, f)
	if req.GetRepository().GetId() != "repo-1" || req.GetRequester().GetWorkspace().GetWorkspace().GetId() != "ws-caller" {
		t.Errorf("repository %v requester %v, want the caller's", req.GetRepository(), req.GetRequester())
	}
	child := req.GetStandard().GetTopology().GetChild()
	if child.GetOwner().GetId() != "ws-peer" || child.GetOwnerNotice().GetText() != "the fix" || req.GetContextOrigin().GetFork().GetWorkspace().GetId() != "ws-caller" {
		t.Errorf("create = %v, want a child of peer forking the caller", req)
	}
}

func TestCreateSendsAPriorityItNames(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	code := f.run(workspaceNoun, "create", "-repository", "here", "-topology", "primary", "-context", "fresh", "-priority", "p1")

	// Assert
	if code != exitSuccess {
		t.Fatalf("exit %d, stderr %q", code, f.errOut.String())
	}
	if req := only[*agentreplv1.CreateWorkspaceRequest](t, f); req.GetPriority().GetP1() == nil {
		t.Fatalf("priority = %v, want p1", req.GetPriority())
	}
}

func TestCreateSendsNoPriorityItWasNotGiven(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	code := f.run(workspaceNoun, "create", "-repository", "here", "-topology", "primary", "-context", "fresh")

	// Assert
	if code != exitSuccess {
		t.Fatalf("exit %d, stderr %q", code, f.errOut.String())
	}
	if req := only[*agentreplv1.CreateWorkspaceRequest](t, f); req.Priority != nil {
		t.Fatalf("priority = %v, want unset", req.GetPriority())
	}
}

func TestCreateRefusesAnUnnamedChoice(t *testing.T) {
	tests := []struct {
		name string
		args []string
	}{
		{name: "no topology", args: []string{"-repository", "here", "-context", "fresh"}},
		{name: "no context", args: []string{"-repository", "here", "-topology", "primary"}},
		{name: "a child with no owner", args: []string{"-repository", "here", "-topology", "child", "-context", "fresh"}},
		{name: "a one-shot with no prompt", args: []string{"-repository", "here", "-topology", "primary", "-context", "fresh", "-one-shot"}},
		{name: "an unknown priority", args: []string{"-repository", "here", "-topology", "primary", "-context", "fresh", "-priority", "urgent"}},
		{name: "a cleared priority, which a create has none to clear", args: []string{"-repository", "here", "-topology", "primary", "-context", "fresh", "-priority", "none"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(workspaceNoun, append([]string{"create"}, tt.args...)...)

			// Assert
			if code != exitFailure || len(f.client.requests) > 1 {
				t.Fatalf("exit %d, requests %v; want a usage failure sending no create", code, f.client.requests)
			}
		})
	}
}

func TestACommandOutsideAnyWorkspaceIsRefusedUnlessTheUserAsks(t *testing.T) {
	tests := []struct {
		name   string
		asUser bool
		want   int
	}{
		{name: "an agent outside any workspace", want: exitFailure},
		{name: "the user", asUser: true, want: exitSuccess},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()
			f.cwd = "/elsewhere"
			args := []string{"finished", "peer", "-placement", "back"}
			if tt.asUser {
				args = append(args, "-as-user")
			}

			// Act
			code := f.run(workspaceNoun, args...)

			// Assert
			if code != tt.want {
				t.Fatalf("exit %d, stderr %q; want %d", code, f.errOut.String(), tt.want)
			}
			if tt.asUser && only[*agentreplv1.FinishWorkspaceRequest](t, f).GetRequester().GetUser() == nil {
				t.Error("the deploy was not asked as the user")
			}
		})
	}
}

func TestAWorkspaceIsNamedByNameDirectoryOrHere(t *testing.T) {
	tests := []struct {
		name string
		arg  string
		want string
		code int
	}{
		{name: "by name", arg: "peer", want: "ws-peer", code: exitSuccess},
		{name: "by directory", arg: "/wt/peer", want: "ws-peer", code: exitSuccess},
		{name: "here", arg: here, want: "ws-caller", code: exitSuccess},
		{name: "a name two workspaces share", arg: "twin", code: exitFailure},
		{name: "a name nobody has", arg: "ghost", code: exitFailure},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(workspaceNoun, "status", tt.arg)

			// Assert
			if code != tt.code {
				t.Fatalf("exit %d, stderr %q; want %d", code, f.errOut.String(), tt.code)
			}
			if tt.want != "" && only[*agentreplv1.GetWorkspaceStatusRequest](t, f).GetWorkspace().GetId() != tt.want {
				t.Errorf("status of %v, want %s", f.client.requests, tt.want)
			}
		})
	}
}

func TestFinishedNamesItsPlacement(t *testing.T) {
	tests := []struct {
		name  string
		args  []string
		code  int
		front bool
	}{
		{name: "at the front", args: []string{"here", "-placement", "front"}, code: exitSuccess, front: true},
		{name: "with no placement", args: []string{"here"}, code: exitFailure},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(workspaceNoun, append([]string{"finished"}, tt.args...)...)

			// Assert
			if code != tt.code {
				t.Fatalf("exit %d, stderr %q; want %d", code, f.errOut.String(), tt.code)
			}
			if tt.front && only[*agentreplv1.FinishWorkspaceRequest](t, f).GetPlacement().GetFront() == nil {
				t.Error("the deploy was not placed at the front")
			}
		})
	}
}

func TestARefusedAnswerExitsRefusedAndAFailedCallFails(t *testing.T) {
	tests := []struct {
		name string
		edit func(c *cliClient)
		want int
	}{
		{name: "a refusal", edit: func(c *cliClient) {
			c.finish = &agentreplv1.FinishWorkspaceResponse{Result: &agentreplv1.FinishWorkspaceResponse_Error{Error: &agentreplv1.FinishWorkspaceError{}}}
		}, want: exitRefused},
		{name: "a failed call", edit: func(c *cliClient) { c.err = errors.New("unavailable") }, want: exitFailure},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()
			tt.edit(f.client)

			// Act
			code := f.run(workspaceNoun, "finished", "here", "-placement", "back")

			// Assert
			if code != tt.want {
				t.Fatalf("exit %d, want %d", code, tt.want)
			}
		})
	}
}

func TestSetSendsOneCallPerSettingInOrder(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	code := f.run(workspaceNoun, "set", "here", "-hidden", "-one-shot", "-await", "peer")

	// Assert
	if code != exitSuccess {
		t.Fatalf("exit %d, stderr %q", code, f.errOut.String())
	}
	var kinds []string
	for _, req := range f.client.requests {
		kinds = append(kinds, string(req.ProtoReflect().Descriptor().Name()))
	}
	want := "SetWorkspaceHiddenRequest SetWorkspaceOneShotRequest SetWorkspaceCorrespondenceRequest"
	if got := strings.Join(kinds, " "); !strings.HasSuffix(got, want) {
		t.Fatalf("requests = %s, want ...%s", got, want)
	}
	if only[*agentreplv1.SetWorkspaceCorrespondenceRequest](t, f).GetAwaiting().GetAwaited().GetId() != "ws-peer" {
		t.Error("the workspace does not await peer")
	}
}

func TestSetRefusesASettingSetBothWays(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	code := f.run(workspaceNoun, "set", "here", "-one-shot", "-not-one-shot")

	// Assert
	if code != exitFailure || len(f.client.requests) != 0 {
		t.Fatalf("exit %d, requests %v; want a usage failure and nothing sent", code, f.client.requests)
	}
}

func TestPromptIsSentAsTheAgents(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	code := f.run(workspaceNoun, "prompt", "peer", "please rebase")

	// Assert
	req := only[*agentreplv1.SubmitPromptRequest](t, f)
	if code != exitSuccess || req.GetWorkspace().GetId() != "ws-peer" || req.GetOrigin().String() != "PROMPT_ORIGIN_AGENT_SENT" || req.GetIdempotencyKey() == "" {
		t.Fatalf("exit %d, prompt %v; want peer prompted as the agent", code, req)
	}
}

func TestPromptNamesTheCallingWorkspaceAsItsSender(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	code := f.run(workspaceNoun, "prompt", "peer", "please rebase")

	// Assert
	req := only[*agentreplv1.SubmitPromptRequest](t, f)
	if code != exitSuccess || req.GetSentBy().GetId() != "ws-caller" {
		t.Fatalf("exit %d, sent_by %v; want the calling workspace", code, req.GetSentBy())
	}
}

func TestPromptFromNoWorkspaceSendsNothing(t *testing.T) {
	// Arrange
	f := newCLIFixture()
	f.cwd = "/elsewhere"

	// Act
	code := f.run(workspaceNoun, "prompt", "peer", "please rebase")

	// Assert
	for _, req := range f.client.requests {
		if _, sent := req.(*agentreplv1.SubmitPromptRequest); sent {
			t.Fatalf("a prompt was sent from no workspace: %v", req)
		}
	}
	if code != exitFailure || !strings.Contains(f.errOut.String(), "inside no workspace") {
		t.Fatalf("exit %d, stderr %q; want the calling directory refused", code, f.errOut.String())
	}
}

func TestSubscribeReadsItsEvents(t *testing.T) {
	tests := []struct {
		name   string
		events string
		code   int
	}{
		{name: "known events", events: "landed, closed", code: exitSuccess},
		{name: "an unknown event", events: "landed,exploded", code: exitFailure},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(workspaceNoun, "subscribe", "peer", "-events", tt.events)

			// Assert
			if code != tt.code {
				t.Fatalf("exit %d, stderr %q; want %d", code, f.errOut.String(), tt.code)
			}
			if tt.code == exitSuccess {
				req := only[*agentreplv1.SubscribeWorkspaceRequest](t, f)
				if len(req.GetEvents()) != 2 || req.GetSubscriber().GetId() != "ws-caller" || req.GetFollowed().GetId() != "ws-peer" {
					t.Errorf("subscription = %v, want the caller following peer's two events", req)
				}
			}
		})
	}
}

func TestGateAnswersTheStandingGate(t *testing.T) {
	gate := &frontendv1.FeedId{Value: "gate-1"}
	standing := &agentreplv1.GetWorkspaceStatusResponse{Result: &agentreplv1.GetWorkspaceStatusResponse_Success{Success: &agentreplv1.GetWorkspaceStatusSuccess{
		Status: &agentreplv1.WorkspaceStatus{Standing: &agentreplv1.WorkspaceStatusStanding{Standing: &agentreplv1.WorkspaceStatusStanding_Open{Open: &agentreplv1.WorkspaceStatusOpen{
			Activity: &agentreplv1.WorkspaceStatusActivity{Activity: &agentreplv1.WorkspaceStatusActivity_ColdGate{ColdGate: &agentreplv1.WorkspaceStatusActivityColdGate{Gate: gate}}}}}}}}}}
	tests := []struct {
		name   string
		status *agentreplv1.GetWorkspaceStatusResponse
		args   []string
		code   int
	}{
		{name: "pay", status: standing, args: []string{"peer", "pay"}, code: exitSuccess},
		{name: "compact with its options", status: standing, args: []string{"peer", "compact", "-model", "haiku", "-scope", "all"}, code: exitSuccess},
		{name: "compact with no options", status: standing, args: []string{"peer", "compact"}, code: exitFailure},
		{name: "no gate standing", args: []string{"peer", "clear"}, code: exitRefused},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()
			f.client.status = tt.status

			// Act
			code := f.run(workspaceNoun, append([]string{"gate"}, tt.args...)...)

			// Assert
			if code != tt.code {
				t.Fatalf("exit %d, stderr %q; want %d", code, f.errOut.String(), tt.code)
			}
			if tt.code == exitSuccess && !proto.Equal(only[*agentreplv1.AnswerColdGateRequest](t, f).GetGate(), gate) {
				t.Error("the answer does not name the standing gate")
			}
		})
	}
}

func TestEvalDefaultsToTheCallingWorkspacesBuffer(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	code := f.run(editorNoun, "eval", "(buffer-name)")

	// Assert
	req := only[*agentreplv1.EvaluateInEditorRequest](t, f)
	if code != exitSuccess || req.GetWorkspace().GetId() != "ws-caller" || req.GetText() != "(buffer-name)" {
		t.Fatalf("exit %d, eval %v; want the caller's buffer", code, req)
	}
}

func TestDeployHoldHoldsOrReleasesTheNamedRepository(t *testing.T) {
	tests := []struct {
		name    string
		args    []string
		code    int
		release bool
	}{
		{name: "hold", args: []string{"hold", "-repository", "here"}, code: exitSuccess},
		{name: "release", args: []string{"release", "-repository", "here"}, code: exitSuccess, release: true},
		{name: "an unknown action", args: []string{"pause", "-repository", "here"}, code: exitFailure},
		{name: "no repository", args: []string{"hold"}, code: exitFailure},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(runtimeNoun, append([]string{"deploy-hold"}, tt.args...)...)

			// Assert
			if code != tt.code {
				t.Fatalf("exit %d, stderr %q; want %d", code, f.errOut.String(), tt.code)
			}
			if tt.code != exitSuccess {
				return
			}
			req := only[*agentreplv1.UpdateDeployHoldRequest](t, f)
			if req.GetRepository().GetId() != "repo-1" || (req.GetRelease() != nil) != tt.release {
				t.Fatalf("request = %v, want the caller's repository, release %v", req, tt.release)
			}
		})
	}
}

func TestRollOutLandingNamesItsDeployWhenGivenOne(t *testing.T) {
	tests := []struct {
		name string
		args []string
		want string
	}{
		{name: "a deploy's ledger", args: []string{"-deploy", "0011aabb"}, want: "0011aabb"},
		{name: "no deploy", args: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(runtimeNoun, append([]string{"roll-out-landing"}, tt.args...)...)

			// Assert
			if code != exitSuccess {
				t.Fatalf("exit %d, stderr %q", code, f.errOut.String())
			}
			req := only[*agentreplv1.RollOutLandingRequest](t, f)
			if (req.GetDeploy() != nil) != (tt.want != "") || req.GetDeploy().GetLedger() != tt.want {
				t.Fatalf("request = %v, want deploy %q", req, tt.want)
			}
		})
	}
}

func TestRollOutLandingRefusesAPositional(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	code := f.run(runtimeNoun, "roll-out-landing", "extra")

	// Assert
	if code != exitFailure || len(f.client.requests) != 0 {
		t.Fatalf("exit %d with %d request(s), want a usage failure and none", code, len(f.client.requests))
	}
}

func TestCreateSendsItsDeployHooksInOrder(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	code := f.run(workspaceNoun, "create", "-repository", "here", "-topology", "primary", "-context", "fresh",
		"-pre-deploy-prompt", "lint it", "-pre-deploy-script", "bin/check.sh:30", "-post-deploy-prompt", "announce it")

	// Assert
	if code != exitSuccess {
		t.Fatalf("exit %d, stderr %q", code, f.errOut.String())
	}
	hooks := only[*agentreplv1.CreateWorkspaceRequest](t, f).GetStandard().GetDeployHooks()
	pre, post := hooks.GetPreDeploy(), hooks.GetPostDeploy()
	if len(pre) != 2 || pre[0].GetPrompt().GetText() != "lint it" || pre[1].GetScript().GetPath() != "bin/check.sh" || pre[1].GetScript().GetTimeoutSeconds() != 30 {
		t.Fatalf("pre-deploy hooks = %v, want the prompt then the script", pre)
	}
	if len(post) != 1 || post[0].GetPrompt().GetText() != "announce it" {
		t.Fatalf("post-deploy hooks = %v, want the prompt", post)
	}
}

func TestAMalformedHookFlagIsAUsageFailure(t *testing.T) {
	tests := []struct {
		name string
		flag []string
	}{
		{name: "a script with no timeout", flag: []string{"-pre-deploy-script", "bin/check.sh"}},
		{name: "a script with a zero timeout", flag: []string{"-post-deploy-script", "bin/check.sh:0"}},
		{name: "a script with no path", flag: []string{"-pre-deploy-script", ":30"}},
		{name: "a blank prompt", flag: []string{"-post-deploy-prompt", "  "}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()
			args := append([]string{"create", "-repository", "here", "-topology", "primary", "-context", "fresh"}, tt.flag...)

			// Act
			code := f.run(workspaceNoun, args...)

			// Assert
			if code != exitFailure || len(f.client.requests) != 0 {
				t.Fatalf("exit %d with %d request(s), want a usage failure and none", code, len(f.client.requests))
			}
		})
	}
}

func TestAsyncMergeSendsItsMakerTargetAndPlacement(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	code := f.run(workspaceNoun, "async-merge", "-name", "fix", "-branch", "agent/fix", "-maker-subagent", "a6e95de7",
		"-context-fork", "here", "-target", "maker", "-placement", "front")

	// Assert
	if code != exitSuccess {
		t.Fatalf("exit %d, stderr %q", code, f.errOut.String())
	}
	async := only[*agentreplv1.CreateWorkspaceRequest](t, f).GetAsyncMerge()
	if async.GetSource().GetMaker().GetSubagent().GetAgentId() != "a6e95de7" || async.GetTarget().GetMaker() == nil || async.GetPlacement().GetFront() == nil {
		t.Fatalf("async merge = %v, want the subagent's branch into its maker at the front", async)
	}
}

func TestListPrintsEveryWorkspaceOneALine(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	code := f.run(workspaceNoun, "list")

	// Assert
	lines := strings.Split(strings.TrimSpace(f.out.String()), "\n")
	if code != exitSuccess || len(lines) != 4 {
		t.Fatalf("exit %d, lines %q; want four workspaces", code, lines)
	}
	if !strings.Contains(lines[0], `"name":"caller"`) || !strings.Contains(lines[0], `"calling":true`) || strings.Contains(lines[1], `"calling"`) {
		t.Fatalf("lines = %q, want the caller first and marked", lines)
	}
}

func TestOnWorkspaceSendsTheNamedWorkspace(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	code := f.run(workspaceNoun, "kill", "peer")

	// Assert
	req := only[*agentreplv1.KillWorkspaceRequest](t, f)
	if code != exitSuccess || req.GetWorkspace().GetId() != "ws-peer" {
		t.Fatalf("exit %d, request %v; want peer named", code, req)
	}
}

func TestOnWorkspaceRefusesAnythingAfterTheWorkspace(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	code := f.run(workspaceNoun, "kill", "peer", "extra")

	// Assert
	if code != exitFailure || len(f.client.requests) != 0 {
		t.Fatalf("exit %d, requests %v; want a failure and nothing sent", code, f.client.requests)
	}
}

// EVERY ONE-WORKSPACE VERB GOES THROUGH onWorkspace: a verb that parses its
// one workspace by hand and then makes a single call fails here.
func TestEveryOneWorkspaceVerbIsOnWorkspace(t *testing.T) {
	// Arrange
	var raw []byte
	for _, file := range sourcescan.Production(t) {
		if file.Name == "agentcli.go" {
			raw = file.Source
		}
	}
	verb := regexp.MustCompile(`(?s)func workspace(Kill|Select|Restart)\(c \*cliCall, args \[\]string\) int \{\n(.*?)\n}\n`)

	// Act
	matches := verb.FindAllSubmatch(raw, -1)

	// Assert
	if len(matches) == 0 {
		t.Fatal("found no one-workspace verb to check")
	}
	for _, m := range matches {
		if !strings.HasPrefix(strings.TrimSpace(string(m[2])), "return onWorkspace(") {
			t.Errorf("workspace%s parses its workspace by hand instead of onWorkspace", m[1])
		}
	}
}

func TestSelectAndRestartNameTheirWorkspace(t *testing.T) {
	tests := []struct {
		verb string
		got  func(*testing.T, *cliFixture) string
	}{
		{verb: "select", got: func(t *testing.T, f *cliFixture) string {
			return only[*agentreplv1.SelectWorkspaceRequest](t, f).GetWorkspace().GetId()
		}},
		{verb: "restart", got: func(t *testing.T, f *cliFixture) string {
			return only[*agentreplv1.RestartWorkspaceRequest](t, f).GetWorkspace().GetId()
		}},
	}
	for _, tt := range tests {
		t.Run(tt.verb, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(workspaceNoun, tt.verb, "peer")

			// Assert
			if got := tt.got(t, f); code != exitSuccess || got != "ws-peer" {
				t.Fatalf("exit %d, workspace %q; want ws-peer", code, got)
			}
		})
	}
}

func TestOnWorkspaceSaysWhyItRefusesAnythingAfterTheWorkspace(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	f.run(workspaceNoun, "restart", "peer", "extra")

	// Assert
	if !strings.Contains(f.errOut.String(), "nothing after it") {
		t.Fatalf("stderr %q, want the refusal explained", f.errOut.String())
	}
}

func TestInterruptChoosesItsTarget(t *testing.T) {
	tests := []struct {
		name      string
		flags     []string
		allAgents bool
		confirm   bool
	}{
		{name: "the running turn"},
		{name: "the running turn and its live agents", flags: []string{"-confirm-agents"}, confirm: true},
		{name: "every live agent", flags: []string{"-all-agents"}, allAgents: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(workspaceNoun, append([]string{"interrupt", "peer"}, tt.flags...)...)

			// Assert
			req := only[*agentreplv1.InterruptRequest](t, f)
			if code != exitSuccess || (req.GetAllAgents() != nil) != tt.allAgents || (req.GetTurn() != nil) == tt.allAgents || req.GetConfirmAgents() != tt.confirm {
				t.Fatalf("exit %d, request %v; want all-agents %v, confirm %v", code, req, tt.allAgents, tt.confirm)
			}
		})
	}
}

func TestInterruptRefusesBothFlags(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	code := f.run(workspaceNoun, "interrupt", "peer", "-all-agents", "-confirm-agents")

	// Assert
	if code != exitFailure || len(f.client.requests) != 0 {
		t.Fatalf("exit %d, requests %v; want a usage failure and nothing sent", code, f.client.requests)
	}
}

func TestCreateOriginMasterCutsFromOrigin(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	code := f.run(workspaceNoun, "create", "-repository", "/repo", "-topology", "primary", "-origin-master", "-context", "fresh", "-prompt", "fix it")

	// Assert
	req := only[*agentreplv1.CreateWorkspaceRequest](t, f)
	if code != exitSuccess || req.GetStandard().GetTopology().GetPrimary().GetFromOrigin() == nil {
		t.Fatalf("exit %d, request %v; want a primary cut from origin", code, req)
	}
}

func TestCreateOriginMasterRefusesAChild(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	code := f.run(workspaceNoun, "create", "-repository", "/repo", "-topology", "child", "-owner", "peer", "-origin-master", "-context", "fresh")

	// Assert
	if code != exitFailure || len(f.client.requests) != 0 || !strings.Contains(f.errOut.String(), "-origin-master") {
		t.Fatalf("exit %d, requests %v, stderr %q; want the flag refused for a child", code, f.client.requests, f.errOut.String())
	}
}

// queuesListing answers the merge queues listing ENTRIES, each a merge's name
// and its workspace, spread across two repositories' queues.
func queuesListing(entries ...[2]string) *agentreplv1.GetMergeQueueStatusResponse {
	queues := []*agentreplv1.MergeQueueStatus{{}, {}}
	for i, e := range entries {
		queues[i%2].Entries = append(queues[i%2].Entries, &agentreplv1.MergeQueueStatusEntry{Name: e[0], Workspace: &workspacev1.WorkspaceRef{Id: e[1]}})
	}
	return &agentreplv1.GetMergeQueueStatusResponse{Result: &agentreplv1.GetMergeQueueStatusResponse_Success{Success: &agentreplv1.GetMergeQueueStatusSuccess{Queues: queues}}}
}

// evicted answers the workspace one queue update names, whichever verb it was.
func evicted(req *agentreplv1.UpdateMergeQueueRequest) string {
	switch action := req.GetAction().(type) {
	case *agentreplv1.UpdateMergeQueueRequest_Evict:
		return "dequeue " + action.Evict.GetWorkspace().GetId()
	case *agentreplv1.UpdateMergeQueueRequest_MoveUp:
		return "move-up " + action.MoveUp.GetWorkspace().GetId()
	case *agentreplv1.UpdateMergeQueueRequest_MoveDown:
		return "move-down " + action.MoveDown.GetWorkspace().GetId()
	}
	return ""
}

func TestAQueueVerbAddressesAMergeByTheNameTheQueueListsItUnder(t *testing.T) {
	for _, verb := range []string{"dequeue", "move-up", "move-down"} {
		t.Run(verb, func(t *testing.T) {
			// Arrange: the async merge's workspace is hidden, so no roster row names it.
			f := newCLIFixture()
			f.client.queues = queuesListing([2]string{"other", "ws-other"}, [2]string{"pt3-sentences", "ws-hidden-merge"})

			// Act
			code := f.run(mergeQueueNoun, verb, "pt3-sentences")

			// Assert
			if code != exitSuccess {
				t.Fatalf("exit %d, stderr %q", code, f.errOut.String())
			}
			if got := evicted(only[*agentreplv1.UpdateMergeQueueRequest](t, f)); got != verb+" ws-hidden-merge" {
				t.Fatalf("update = %q, want %s of the listed merge's workspace", got, verb)
			}
		})
	}
}

func TestAQueueVerbAddressesHereAsTheCallingWorkspacesOwnMerge(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	code := f.run(mergeQueueNoun, "dequeue", "here")

	// Assert
	if code != exitSuccess {
		t.Fatalf("exit %d, stderr %q", code, f.errOut.String())
	}
	if got := evicted(only[*agentreplv1.UpdateMergeQueueRequest](t, f)); got != "dequeue ws-caller" {
		t.Fatalf("update = %q, want the caller's own merge", got)
	}
}

func TestAQueueVerbRefusesANameTheQueuesDoNotListOnce(t *testing.T) {
	tests := []struct {
		name   string
		queues *agentreplv1.GetMergeQueueStatusResponse
		stderr string
	}{
		{name: "no merge has the name, though a workspace does", queues: queuesListing([2]string{"other", "ws-other"}), stderr: `no merge in any queue is named "peer"`},
		{name: "two merges share the name", queues: queuesListing([2]string{"peer", "ws-a"}, [2]string{"peer", "ws-b"}), stderr: `2 merges in the queues are named "peer"`},
		{
			name:   "the daemon refuses the read",
			queues: &agentreplv1.GetMergeQueueStatusResponse{Result: &agentreplv1.GetMergeQueueStatusResponse_Error{Error: &agentreplv1.GetMergeQueueStatusError{}}},
			stderr: "the daemon refused to read the merge queues",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()
			f.client.queues = tt.queues

			// Act
			code := f.run(mergeQueueNoun, "dequeue", "peer")

			// Assert
			if code != exitFailure || !strings.Contains(f.errOut.String(), tt.stderr) {
				t.Fatalf("exit %d, stderr %q, want a failure saying %q", code, f.errOut.String(), tt.stderr)
			}
			for _, req := range f.client.requests {
				if _, updated := req.(*agentreplv1.UpdateMergeQueueRequest); updated {
					t.Fatalf("requests = %v, want no queue update", f.client.requests)
				}
			}
		})
	}
}

func TestAQueueVerbRefusesAnUnreadableQueue(t *testing.T) {
	// Arrange
	f := newCLIFixture()
	f.client.err = errors.New("connection refused")

	// Act
	code := f.run(mergeQueueNoun, "move-up", "pt3-sentences")

	// Assert
	if code != exitFailure || !strings.Contains(f.errOut.String(), "read the merge queues: connection refused") {
		t.Fatalf("exit %d, stderr %q, want the unread queues named", code, f.errOut.String())
	}
}

func TestEveryTargetedVerbRefusesAMissingTargetAndDialsNothing(t *testing.T) {
	tests := []struct {
		noun, verb, want string
	}{
		{noun: workspaceNoun, verb: "status", want: "the command names its workspace"},
		{noun: mergeQueueNoun, verb: "dequeue", want: "the command names its merge"},
		{noun: mergeQueueNoun, verb: "move-up", want: "the command names its merge"},
	}
	for _, tt := range tests {
		t.Run(tt.noun+" "+tt.verb, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(tt.noun, tt.verb)

			// Assert
			if code != exitFailure || !strings.Contains(f.errOut.String(), tt.want) || len(f.client.requests) != 0 {
				t.Fatalf("exit %d, stderr %q, requests %v; want %q and nothing asked", code, f.errOut.String(), f.client.requests, tt.want)
			}
		})
	}
}

func TestTheTargetedVerbsShareOneArgumentPreamble(t *testing.T) {
	// Arrange: the parse-then-dial preamble lives in targetArgs alone.
	src, err := os.ReadFile("agentcli.go")
	if err != nil {
		t.Fatalf("read agentcli.go: %v", err)
	}

	// Act
	handRolled := strings.Count(string(src), "c.usageFailed(\"the command names its workspace\")") +
		strings.Count(string(src), "c.usageFailed(\"the command names its merge\")")

	// Assert
	if handRolled != 0 || strings.Count(string(src), "c.targetArgs(fs, stateDir, args, ") != 2 {
		t.Fatalf("usage refusals %d, targetArgs calls %d; want the one preamble shared by workspaceArg and queuedArg",
			handRolled, strings.Count(string(src), "c.targetArgs(fs, stateDir, args, "))
	}
}

func TestAGateWithNoColdGateNamesTheWorkspacesStanding(t *testing.T) {
	// Arrange: the default status stands idle, with no gate.
	f := newCLIFixture()
	f.client.status = &agentreplv1.GetWorkspaceStatusResponse{Result: &agentreplv1.GetWorkspaceStatusResponse_Success{Success: &agentreplv1.GetWorkspaceStatusSuccess{
		Status: &agentreplv1.WorkspaceStatus{Standing: &agentreplv1.WorkspaceStatusStanding{}}}}}

	// Act
	code := f.run(workspaceNoun, "gate", "peer", "pay")

	// Assert
	stderr := f.errOut.String()
	if code != exitRefused || !strings.Contains(stderr, "no cold gate stands on the workspace; it stands {") || strings.Contains(stderr, "&{") {
		t.Fatalf("exit %d, stderr %q; want the refusal naming the standing as JSON, never a Go struct", code, stderr)
	}
}
