package main

import (
	"bytes"
	"context"
	"errors"
	"strings"
	"testing"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
)

// cliClient is a daemon client that records every request and answers the
// scripted responses.
type cliClient struct {
	agentreplv1connect.AgentReplClient
	requests []proto.Message
	err      error
	status   *agentreplv1.GetWorkspaceStatusResponse
	create   *agentreplv1.CreateWorkspaceResponse
	merge    *agentreplv1.MergeWorkspaceResponse
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

func (c *cliClient) MergeWorkspace(_ context.Context, req *connect.Request[agentreplv1.MergeWorkspaceRequest]) (*connect.Response[agentreplv1.MergeWorkspaceResponse], error) {
	if err := c.record(req.Msg); err != nil {
		return nil, err
	}
	if c.merge != nil {
		return connect.NewResponse(c.merge), nil
	}
	return connect.NewResponse(&agentreplv1.MergeWorkspaceResponse{Result: &agentreplv1.MergeWorkspaceResponse_Success{Success: &agentreplv1.MergeWorkspaceSuccess{}}}), nil
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

func (c *cliClient) SubscribeWorkspace(_ context.Context, req *connect.Request[agentreplv1.SubscribeWorkspaceRequest]) (*connect.Response[agentreplv1.SubscribeWorkspaceResponse], error) {
	return connect.NewResponse(&agentreplv1.SubscribeWorkspaceResponse{}), c.record(req.Msg)
}

func (c *cliClient) AnswerColdGate(_ context.Context, req *connect.Request[agentreplv1.AnswerColdGateRequest]) (*connect.Response[agentreplv1.AnswerColdGateResponse], error) {
	return connect.NewResponse(&agentreplv1.AnswerColdGateResponse{}), c.record(req.Msg)
}

func (c *cliClient) EvaluateInEditor(_ context.Context, req *connect.Request[agentreplv1.EvaluateInEditorRequest]) (*connect.Response[agentreplv1.EvaluateInEditorResponse], error) {
	return connect.NewResponse(&agentreplv1.EvaluateInEditorResponse{}), c.record(req.Msg)
}

func (c *cliClient) UpdateRollouts(_ context.Context, req *connect.Request[agentreplv1.UpdateRolloutsRequest]) (*connect.Response[agentreplv1.UpdateRolloutsResponse], error) {
	return connect.NewResponse(&agentreplv1.UpdateRolloutsResponse{}), c.record(req.Msg)
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
		client:  func(string) agentreplv1connect.AgentReplClient { return f.client },
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
			args := []string{"merge", "peer", "-placement", "back"}
			if tt.asUser {
				args = append(args, "-as-user")
			}

			// Act
			code := f.run(workspaceNoun, args...)

			// Assert
			if code != tt.want {
				t.Fatalf("exit %d, stderr %q; want %d", code, f.errOut.String(), tt.want)
			}
			if tt.asUser && only[*agentreplv1.MergeWorkspaceRequest](t, f).GetRequester().GetUser() == nil {
				t.Error("the merge was not asked as the user")
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

func TestMergeNamesItsPlacement(t *testing.T) {
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
			code := f.run(workspaceNoun, append([]string{"merge"}, tt.args...)...)

			// Assert
			if code != tt.code {
				t.Fatalf("exit %d, stderr %q; want %d", code, f.errOut.String(), tt.code)
			}
			if tt.front && only[*agentreplv1.MergeWorkspaceRequest](t, f).GetPlacement().GetFront() == nil {
				t.Error("the merge was not placed at the front")
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
			c.merge = &agentreplv1.MergeWorkspaceResponse{Result: &agentreplv1.MergeWorkspaceResponse_Error{Error: &agentreplv1.MergeWorkspaceError{}}}
		}, want: exitRefused},
		{name: "a failed call", edit: func(c *cliClient) { c.err = errors.New("unavailable") }, want: exitFailure},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()
			tt.edit(f.client)

			// Act
			code := f.run(workspaceNoun, "merge", "here", "-placement", "back")

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

func TestRolloutsHoldOrRelease(t *testing.T) {
	tests := []struct {
		arg  string
		code int
	}{
		{arg: "hold", code: exitSuccess},
		{arg: "release", code: exitSuccess},
		{arg: "pause", code: exitFailure},
	}
	for _, tt := range tests {
		t.Run(tt.arg, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(runtimeNoun, "rollouts", tt.arg)

			// Assert
			if code != tt.code {
				t.Fatalf("exit %d, stderr %q; want %d", code, f.errOut.String(), tt.code)
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
