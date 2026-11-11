package main

import (
	"bytes"
	"context"
	"errors"
	"os"
	"regexp"
	"slices"
	"strings"
	"testing"
	"time"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/encoding/protojson"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
	"agentrepl/protohelpers/rosterwalk"

	"agent-repld/internal/prwatch"
	"agent-repld/internal/sourcescan"
	"agent-repld/internal/workspace"
	"agent-repld/internal/workspacesubs"
	"agent-repld/internal/wsm"
)

// cliClient is a daemon client that records every request and answers the
// scripted responses.
type cliClient struct {
	agentreplv1connect.AgentReplClient
	requests []proto.Message
	err      error
	status   *agentreplv1.GetWorkspaceStatusResponse
	// gathered and gatherStatus answer GatherWorkspaces and GetGatherStatus,
	// an empty answer when nil.
	gathered     *agentreplv1.GatherWorkspacesResponse
	gatherStatus *agentreplv1.GetGatherStatusResponse
	create       *agentreplv1.CreateWorkspaceResponse
	// creates, when set, answers each CreateWorkspace in turn, before create.
	creates []*agentreplv1.CreateWorkspaceResponse
	// createErr, when set, fails CreateWorkspace alone, so the reads a
	// create makes first still answer.
	createErr error
	finish    *agentreplv1.FinishWorkspaceResponse
	// retargeted and oneShot script SetWorkspaceParent's and
	// SetWorkspaceOneShot's answers; nil answers an empty success.
	retargeted *agentreplv1.SetWorkspaceParentResponse
	oneShot    *agentreplv1.SetWorkspaceOneShotResponse
	queues     *agentreplv1.GetMergeQueueStatusResponse
	// found answers FindWorkspaces, an empty complete find when nil.
	found *agentreplv1.FindWorkspacesResponse
	// arrangement answers GetSidebarArrangement; arrange answers
	// ArrangeSidebar, a success when nil.
	arrangement *agentreplv1.GetSidebarArrangementResponse
	arrange     *agentreplv1.ArrangeSidebarResponse
	// registered answers RegisterRepository, a new repository's success when nil.
	registered *agentreplv1.RegisterRepositoryResponse
	// remote answers UpdateRemoteControlMode, a success when nil.
	remote *agentreplv1.UpdateRemoteControlModeResponse
	// prompted answers SubmitPrompt, an empty answer when nil.
	prompted *agentreplv1.SubmitPromptResponse
}

func (c *cliClient) RegisterRepository(_ context.Context, req *connect.Request[agentreplv1.RegisterRepositoryRequest]) (*connect.Response[agentreplv1.RegisterRepositoryResponse], error) {
	if err := c.record(req.Msg); err != nil {
		return nil, err
	}
	if c.registered != nil {
		return connect.NewResponse(c.registered), nil
	}
	return connect.NewResponse(registerSuccess(false)), nil
}

func registerSuccess(alreadyKnown bool) *agentreplv1.RegisterRepositoryResponse {
	return &agentreplv1.RegisterRepositoryResponse{Result: &agentreplv1.RegisterRepositoryResponse_Success{Success: &agentreplv1.RegisterRepositorySuccess{
		Repository: &workspacev1.RepositoryRef{Id: "repo-9", Dir: "/repos/new"}, AlreadyKnown: alreadyKnown}}}
}

func (c *cliClient) GetSidebarArrangement(_ context.Context, req *connect.Request[agentreplv1.GetSidebarArrangementRequest]) (*connect.Response[agentreplv1.GetSidebarArrangementResponse], error) {
	if err := c.record(req.Msg); err != nil {
		return nil, err
	}
	return connect.NewResponse(c.arrangement), nil
}

func (c *cliClient) ArrangeSidebar(_ context.Context, req *connect.Request[agentreplv1.ArrangeSidebarRequest]) (*connect.Response[agentreplv1.ArrangeSidebarResponse], error) {
	if err := c.record(req.Msg); err != nil {
		return nil, err
	}
	if c.arrange != nil {
		return connect.NewResponse(c.arrange), nil
	}
	return connect.NewResponse(&agentreplv1.ArrangeSidebarResponse{Result: &agentreplv1.ArrangeSidebarResponse_Success{Success: &agentreplv1.ArrangeSidebarSuccess{}}}), nil
}

func (c *cliClient) record(req proto.Message) error {
	c.requests = append(c.requests, req)
	return c.err
}

func (c *cliClient) CreateWorkspace(_ context.Context, req *connect.Request[agentreplv1.CreateWorkspaceRequest]) (*connect.Response[agentreplv1.CreateWorkspaceResponse], error) {
	if err := c.record(req.Msg); err != nil {
		return nil, err
	}
	if c.createErr != nil {
		return nil, c.createErr
	}
	if len(c.creates) > 0 {
		next := c.creates[0]
		c.creates = c.creates[1:]
		return connect.NewResponse(next), nil
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

func (c *cliClient) SetWorkspaceParent(_ context.Context, req *connect.Request[agentreplv1.SetWorkspaceParentRequest]) (*connect.Response[agentreplv1.SetWorkspaceParentResponse], error) {
	if c.retargeted != nil {
		return connect.NewResponse(c.retargeted), c.record(req.Msg)
	}
	return connect.NewResponse(&agentreplv1.SetWorkspaceParentResponse{}), c.record(req.Msg)
}

func (c *cliClient) SetWorkspaceOneShot(_ context.Context, req *connect.Request[agentreplv1.SetWorkspaceOneShotRequest]) (*connect.Response[agentreplv1.SetWorkspaceOneShotResponse], error) {
	if c.oneShot != nil {
		return connect.NewResponse(c.oneShot), c.record(req.Msg)
	}
	return connect.NewResponse(&agentreplv1.SetWorkspaceOneShotResponse{}), c.record(req.Msg)
}

func (c *cliClient) SetWorkspaceCorrespondence(_ context.Context, req *connect.Request[agentreplv1.SetWorkspaceCorrespondenceRequest]) (*connect.Response[agentreplv1.SetWorkspaceCorrespondenceResponse], error) {
	return connect.NewResponse(&agentreplv1.SetWorkspaceCorrespondenceResponse{}), c.record(req.Msg)
}

func (c *cliClient) ChangeWorkspaceLinks(_ context.Context, req *connect.Request[agentreplv1.ChangeWorkspaceLinksRequest]) (*connect.Response[agentreplv1.ChangeWorkspaceLinksResponse], error) {
	return connect.NewResponse(&agentreplv1.ChangeWorkspaceLinksResponse{}), c.record(req.Msg)
}

func (c *cliClient) SubmitPrompt(_ context.Context, req *connect.Request[agentreplv1.SubmitPromptRequest]) (*connect.Response[agentreplv1.SubmitPromptResponse], error) {
	if c.prompted != nil {
		return connect.NewResponse(c.prompted), c.record(req.Msg)
	}
	return connect.NewResponse(&agentreplv1.SubmitPromptResponse{}), c.record(req.Msg)
}

func (c *cliClient) GatherWorkspaces(_ context.Context, req *connect.Request[agentreplv1.GatherWorkspacesRequest]) (*connect.Response[agentreplv1.GatherWorkspacesResponse], error) {
	if c.gathered != nil {
		return connect.NewResponse(c.gathered), c.record(req.Msg)
	}
	return connect.NewResponse(&agentreplv1.GatherWorkspacesResponse{}), c.record(req.Msg)
}

func (c *cliClient) GetGatherStatus(_ context.Context, req *connect.Request[agentreplv1.GetGatherStatusRequest]) (*connect.Response[agentreplv1.GetGatherStatusResponse], error) {
	if c.gatherStatus != nil {
		return connect.NewResponse(c.gatherStatus), c.record(req.Msg)
	}
	return connect.NewResponse(&agentreplv1.GetGatherStatusResponse{}), c.record(req.Msg)
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

func (c *cliClient) ParkWorkspace(_ context.Context, req *connect.Request[agentreplv1.ParkWorkspaceRequest]) (*connect.Response[agentreplv1.ParkWorkspaceResponse], error) {
	return connect.NewResponse(&agentreplv1.ParkWorkspaceResponse{}), c.record(req.Msg)
}

func (c *cliClient) Interrupt(_ context.Context, req *connect.Request[agentreplv1.InterruptRequest]) (*connect.Response[agentreplv1.InterruptResponse], error) {
	return connect.NewResponse(&agentreplv1.InterruptResponse{}), c.record(req.Msg)
}

func (c *cliClient) SubscribeWorkspace(_ context.Context, req *connect.Request[agentreplv1.SubscribeWorkspaceRequest]) (*connect.Response[agentreplv1.SubscribeWorkspaceResponse], error) {
	return connect.NewResponse(&agentreplv1.SubscribeWorkspaceResponse{}), c.record(req.Msg)
}

func (c *cliClient) SubscribePullRequest(_ context.Context, req *connect.Request[agentreplv1.SubscribePullRequestRequest]) (*connect.Response[agentreplv1.SubscribePullRequestResponse], error) {
	return connect.NewResponse(&agentreplv1.SubscribePullRequestResponse{}), c.record(req.Msg)
}

func (c *cliClient) UnsubscribePullRequest(_ context.Context, req *connect.Request[agentreplv1.UnsubscribePullRequestRequest]) (*connect.Response[agentreplv1.UnsubscribePullRequestResponse], error) {
	return connect.NewResponse(&agentreplv1.UnsubscribePullRequestResponse{}), c.record(req.Msg)
}

func (c *cliClient) SubscribeGithubMergeQueue(_ context.Context, req *connect.Request[agentreplv1.SubscribeGithubMergeQueueRequest]) (*connect.Response[agentreplv1.SubscribeGithubMergeQueueResponse], error) {
	return connect.NewResponse(&agentreplv1.SubscribeGithubMergeQueueResponse{}), c.record(req.Msg)
}

func (c *cliClient) UnsubscribeGithubMergeQueue(_ context.Context, req *connect.Request[agentreplv1.UnsubscribeGithubMergeQueueRequest]) (*connect.Response[agentreplv1.UnsubscribeGithubMergeQueueResponse], error) {
	return connect.NewResponse(&agentreplv1.UnsubscribeGithubMergeQueueResponse{}), c.record(req.Msg)
}

func (c *cliClient) ListGithubMergeQueueSubscriptions(_ context.Context, req *connect.Request[agentreplv1.ListGithubMergeQueueSubscriptionsRequest]) (*connect.Response[agentreplv1.ListGithubMergeQueueSubscriptionsResponse], error) {
	return connect.NewResponse(&agentreplv1.ListGithubMergeQueueSubscriptionsResponse{}), c.record(req.Msg)
}

func (c *cliClient) ListPullRequestSubscriptions(_ context.Context, req *connect.Request[agentreplv1.ListPullRequestSubscriptionsRequest]) (*connect.Response[agentreplv1.ListPullRequestSubscriptionsResponse], error) {
	return connect.NewResponse(&agentreplv1.ListPullRequestSubscriptionsResponse{}), c.record(req.Msg)
}

func (c *cliClient) CommentOnPullRequest(_ context.Context, req *connect.Request[agentreplv1.CommentOnPullRequestRequest]) (*connect.Response[agentreplv1.CommentOnPullRequestResponse], error) {
	return connect.NewResponse(&agentreplv1.CommentOnPullRequestResponse{}), c.record(req.Msg)
}

func (c *cliClient) NotifyWorkspace(_ context.Context, req *connect.Request[agentreplv1.NotifyWorkspaceRequest]) (*connect.Response[agentreplv1.NotifyWorkspaceResponse], error) {
	return connect.NewResponse(&agentreplv1.NotifyWorkspaceResponse{}), c.record(req.Msg)
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

func (c *cliClient) PruneStorage(_ context.Context, req *connect.Request[agentreplv1.PruneStorageRequest]) (*connect.Response[agentreplv1.PruneStorageResponse], error) {
	return connect.NewResponse(&agentreplv1.PruneStorageResponse{}), c.record(req.Msg)
}

func (c *cliClient) ReapWorktrees(_ context.Context, req *connect.Request[agentreplv1.ReapWorktreesRequest]) (*connect.Response[agentreplv1.ReapWorktreesResponse], error) {
	return connect.NewResponse(&agentreplv1.ReapWorktreesResponse{}), c.record(req.Msg)
}

func (c *cliClient) RollOutLanding(_ context.Context, req *connect.Request[agentreplv1.RollOutLandingRequest]) (*connect.Response[agentreplv1.RollOutLandingResponse], error) {
	return connect.NewResponse(&agentreplv1.RollOutLandingResponse{}), c.record(req.Msg)
}

func (c *cliClient) FindWorkspaces(_ context.Context, req *connect.Request[agentreplv1.FindWorkspacesRequest]) (*connect.Response[agentreplv1.FindWorkspacesResponse], error) {
	if err := c.record(req.Msg); err != nil {
		return nil, err
	}
	if c.found != nil {
		return connect.NewResponse(c.found), nil
	}
	return connect.NewResponse(&agentreplv1.FindWorkspacesResponse{Result: &agentreplv1.FindWorkspacesResponse_Success{Success: &agentreplv1.FindWorkspacesSuccess{
		Extent: &agentreplv1.FindWorkspacesSuccess_Complete{Complete: &agentreplv1.FindWorkspacesSuccessComplete{}}}}}), nil
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
	// dialed is every address the command dialed, in order.
	dialed []string
}

// cliNow is the instant the fixture's commands read as now.
var cliNow = time.Date(2026, 10, 26, 12, 0, 0, 0, time.UTC)

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
		client: func(address string, options ...connect.ClientOption) agentreplv1connect.AgentReplClient {
			f.dialed = append(f.dialed, address)
			f.dialOptions = len(options)
			return f.client
		},
		roster: func(context.Context, agentreplv1connect.AgentReplClient) (*frontendv1.WorkspaceRoster, error) {
			return roster(cliRow("ws-caller", "/wt/caller", "caller"), cliRow("ws-peer", "/wt/peer", "peer"),
				cliRow("ws-twin-1", "/wt/twin-1", "twin"), cliRow("ws-twin-2", "/wt/twin-2", "twin")), nil
		},
		getwd:   func() (string, error) { return f.cwd, nil },
		mintKey: func(prefix string) (string, error) { return prefix + "-1", nil },
		now:     func() time.Time { return cliNow },
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
	code := f.run(workspaceNoun, "create", "-repository", "here", "-relationship", "child", "-parent", "peer",
		"-context", "fork", "-fork", "here", "-prompt", "fix the parser")

	// Assert
	if code != exitSuccess {
		t.Fatalf("exit %d, stderr %q", code, f.errOut.String())
	}
	req := only[*agentreplv1.CreateWorkspaceRequest](t, f)
	if req.GetRepository().GetId() != "repo-1" || req.GetRequester().GetWorkspace().GetWorkspace().GetId() != "ws-caller" {
		t.Errorf("repository %v requester %v, want the caller's", req.GetRepository(), req.GetRequester())
	}
	child := req.GetStandard().GetRelationship().GetChild()
	if child.GetParent().GetId() != "ws-peer" || req.GetContextOrigin().GetFork().GetWorkspace().GetId() != "ws-caller" {
		t.Errorf("create = %v, want a child of peer forking the caller", req)
	}
	if req.GetStandard().LandingTarget != nil {
		t.Errorf("landing target = %v, want unset for the relationship's default", req.GetStandard().GetLandingTarget())
	}
}

func TestCreateSendsTheNameItNamesInEitherForm(t *testing.T) {
	tests := []struct {
		name    string
		args    []string
		formGot func(*agentreplv1.CreateWorkspaceRequest) *string
	}{
		{name: "a standard create",
			args:    []string{"-prompt", "fix the parser"},
			formGot: func(r *agentreplv1.CreateWorkspaceRequest) *string { return r.GetStandard().Name }},
		{name: "a one-shot",
			args:    []string{"-one-shot", "-prompt", "fix the parser"},
			formGot: func(r *agentreplv1.CreateWorkspaceRequest) *string { return r.GetOneShot().Name }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()
			args := append([]string{"create", "-repository", "here", "-relationship", "peer", "-context", "fresh", "-name", "parser-fix"}, tt.args...)

			// Act
			code := f.run(workspaceNoun, args...)

			// Assert
			if code != exitSuccess {
				t.Fatalf("exit %d, stderr %q", code, f.errOut.String())
			}
			if got := tt.formGot(only[*agentreplv1.CreateWorkspaceRequest](t, f)); got == nil || *got != "parser-fix" {
				t.Fatalf("name = %v, want parser-fix carried by the form", got)
			}
		})
	}
}

func TestCreateSendsNoNameItWasNotGiven(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	code := f.run(workspaceNoun, "create", "-repository", "here", "-relationship", "peer", "-context", "fresh", "-one-shot", "-prompt", "fix the parser")

	// Assert
	if code != exitSuccess {
		t.Fatalf("exit %d, stderr %q", code, f.errOut.String())
	}
	if req := only[*agentreplv1.CreateWorkspaceRequest](t, f); req.GetOneShot().Name != nil {
		t.Fatalf("name = %q, want unset so the daemon mints one", req.GetOneShot().GetName())
	}
}

func TestCreateSendsAPriorityItNames(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	code := f.run(workspaceNoun, "create", "-repository", "here", "-relationship", "peer", "-context", "fresh", "-priority", "p1")

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
	code := f.run(workspaceNoun, "create", "-repository", "here", "-relationship", "peer", "-context", "fresh")

	// Assert
	if code != exitSuccess {
		t.Fatalf("exit %d, stderr %q", code, f.errOut.String())
	}
	if req := only[*agentreplv1.CreateWorkspaceRequest](t, f); req.Priority != nil {
		t.Fatalf("priority = %v, want unset", req.GetPriority())
	}
}

func TestCreateSendsTheSlackOriginItNames(t *testing.T) {
	tests := []struct {
		name string
		args []string
		want *agentreplv1.CreateWorkspaceSlackOrigin
	}{
		{name: "a slack thread is the origin", args: []string{"-slack-thread", "https://w.slack.com/archives/C1/p1730000000123456"},
			want: &agentreplv1.CreateWorkspaceSlackOrigin{ThreadUrl: "https://w.slack.com/archives/C1/p1730000000123456"}},
		{name: "no slack thread names no origin", args: nil, want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(workspaceNoun, append([]string{"create", "-repository", "here", "-relationship", "peer", "-context", "fresh"}, tt.args...)...)

			// Assert
			if code != exitSuccess {
				t.Fatalf("exit %d, stderr %q", code, f.errOut.String())
			}
			if req := only[*agentreplv1.CreateWorkspaceRequest](t, f); !proto.Equal(req.GetSlackOrigin(), tt.want) {
				t.Fatalf("slack origin = %v, want %v", req.GetSlackOrigin(), tt.want)
			}
		})
	}
}

func TestCreateStatesItsOriginator(t *testing.T) {
	tests := []struct {
		name string
		args []string
		want *agentreplv1.WorkspaceOriginator
	}{
		{name: "the emoji-ack system says so", args: []string{"-originator", "emoji-ack", "-slack-thread", "https://w.slack.com/archives/C1/p1730000000123456"},
			want: &agentreplv1.WorkspaceOriginator{Originator: &agentreplv1.WorkspaceOriginator_EmojiAck{EmojiAck: &agentreplv1.WorkspaceOriginatorEmojiAck{}}}},
		{name: "unnamed is the user or a workspace", args: nil,
			want: &agentreplv1.WorkspaceOriginator{Originator: &agentreplv1.WorkspaceOriginator_UserOrWorkspace{UserOrWorkspace: &agentreplv1.WorkspaceOriginatorUserOrWorkspace{}}}},
		{name: "a slack thread alone is still the user or a workspace", args: []string{"-slack-thread", "https://w.slack.com/archives/C1/p1730000000123456"},
			want: &agentreplv1.WorkspaceOriginator{Originator: &agentreplv1.WorkspaceOriginator_UserOrWorkspace{UserOrWorkspace: &agentreplv1.WorkspaceOriginatorUserOrWorkspace{}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(workspaceNoun, append([]string{"create", "-repository", "here", "-relationship", "peer", "-context", "fresh"}, tt.args...)...)

			// Assert
			if code != exitSuccess {
				t.Fatalf("exit %d, stderr %q", code, f.errOut.String())
			}
			if req := only[*agentreplv1.CreateWorkspaceRequest](t, f); !proto.Equal(req.GetOriginator(), tt.want) {
				t.Fatalf("originator = %v, want %v", req.GetOriginator(), tt.want)
			}
		})
	}
}

func TestCreateRefusesAnUnknownOriginator(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	code := f.run(workspaceNoun, "create", "-repository", "here", "-relationship", "peer", "-context", "fresh", "-originator", "emoji")

	// Assert
	if code != exitFailure || len(f.client.requests) != 0 || !strings.Contains(f.errOut.String(), "-originator is emoji-ack or user-or-workspace") {
		t.Fatalf("exit %d, requests %v, stderr %q; want a usage failure naming the spellings", code, f.client.requests, f.errOut.String())
	}
}

func TestCreateSendsBornHiddenOnlyWhenAsked(t *testing.T) {
	tests := []struct {
		name string
		args []string
		want *bool
	}{
		{name: "-hidden is born hidden", args: []string{"-hidden"}, want: boolPtr(true)},
		{name: "unnamed is unset", args: nil, want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(workspaceNoun, append([]string{"create", "-repository", "here", "-relationship", "peer", "-context", "fresh"}, tt.args...)...)

			// Assert
			if code != exitSuccess {
				t.Fatalf("exit %d, stderr %q", code, f.errOut.String())
			}
			if got := only[*agentreplv1.CreateWorkspaceRequest](t, f).Hidden; (got == nil) != (tt.want == nil) || (got != nil && *got != *tt.want) {
				t.Fatalf("hidden = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestCreateSendsThePlacementItNames(t *testing.T) {
	tests := []struct {
		name string
		args []string
		want *agentreplv1.CreateWorkspacePlacement
	}{
		{name: "an existing subsection", args: []string{"-subsection", "auth"},
			want: &agentreplv1.CreateWorkspacePlacement{Target: &agentreplv1.CreateWorkspacePlacement_Subsection{Subsection: "auth"}}},
		{name: "a new subsection of its own", args: []string{"-new-subsection", "auth"},
			want: &agentreplv1.CreateWorkspacePlacement{Target: &agentreplv1.CreateWorkspacePlacement_NewSubsection{NewSubsection: &agentreplv1.CreateWorkspaceNewSubsection{Name: "auth"}}}},
		{name: "a new subsection gathering workspaces", args: []string{"-new-subsection", "auth", "-gather", "a, b,c"},
			want: &agentreplv1.CreateWorkspacePlacement{Target: &agentreplv1.CreateWorkspacePlacement_NewSubsection{NewSubsection: &agentreplv1.CreateWorkspaceNewSubsection{Name: "auth", Gather: []string{"a", "b", "c"}}}}},
		{name: "no placement", args: nil, want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(workspaceNoun, append([]string{"create", "-repository", "here", "-relationship", "peer", "-context", "fresh"}, tt.args...)...)

			// Assert
			if code != exitSuccess {
				t.Fatalf("exit %d, stderr %q", code, f.errOut.String())
			}
			if got := only[*agentreplv1.CreateWorkspaceRequest](t, f).GetPlacement(); !proto.Equal(got, tt.want) {
				t.Fatalf("placement = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestCreateRefusesAPlacementFlagMisuse(t *testing.T) {
	tests := []struct {
		name    string
		args    []string
		wantErr string
	}{
		{name: "both subsection flags", args: []string{"-subsection", "a", "-new-subsection", "b"}, wantErr: "-subsection and -new-subsection are alternatives"},
		{name: "gather beside an existing subsection", args: []string{"-subsection", "a", "-gather", "x"}, wantErr: "-gather goes with -new-subsection"},
		{name: "gather alone", args: []string{"-gather", "x"}, wantErr: "-gather goes with -new-subsection"},
		{name: "an empty gathered name", args: []string{"-new-subsection", "a", "-gather", "x,,y"}, wantErr: "-gather names workspaces separated by commas"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(workspaceNoun, append([]string{"create", "-repository", "here", "-relationship", "peer", "-context", "fresh"}, tt.args...)...)

			// Assert
			if code != exitFailure || len(f.client.requests) != 0 || !strings.Contains(f.errOut.String(), tt.wantErr) {
				t.Fatalf("exit %d, requests %v, stderr %q; want a usage failure saying %q", code, f.client.requests, f.errOut.String(), tt.wantErr)
			}
		})
	}
}

func TestCreateRefusesAnUnnamedChoice(t *testing.T) {
	tests := []struct {
		name string
		args []string
	}{
		{name: "no relationship", args: []string{"-repository", "here", "-context", "fresh"}},
		{name: "an unknown relationship", args: []string{"-repository", "here", "-relationship", "sibling", "-context", "fresh"}},
		{name: "a peer with a parent", args: []string{"-repository", "here", "-relationship", "peer", "-parent", "peer", "-context", "fresh"}},
		{name: "the retired owner notice", args: []string{"-repository", "here", "-relationship", "child", "-parent", "peer", "-context", "fresh", "-owner-notice", "the fix"}},
		{name: "no context", args: []string{"-repository", "here", "-relationship", "peer"}},
		{name: "a child with no parent", args: []string{"-repository", "here", "-relationship", "child", "-context", "fresh"}},
		{name: "a one-shot with no prompt", args: []string{"-repository", "here", "-relationship", "peer", "-context", "fresh", "-one-shot"}},
		{name: "an unknown priority", args: []string{"-repository", "here", "-relationship", "peer", "-context", "fresh", "-priority", "urgent"}},
		{name: "a cleared priority, which a create has none to clear", args: []string{"-repository", "here", "-relationship", "peer", "-context", "fresh", "-priority", "none"}},
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
		name   string
		args   []string
		code   int
		front  bool
		behind string
	}{
		{name: "at the front", args: []string{"here", "-placement", "front"}, code: exitSuccess, front: true},
		{name: "behind a named merge", args: []string{"here", "-placement", "behind=two"}, code: exitSuccess, behind: "two"},
		{name: "behind no merge", args: []string{"here", "-placement", "behind="}, code: exitFailure},
		{name: "with an unknown placement", args: []string{"here", "-placement", "middle"}, code: exitFailure},
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
			if tt.behind != "" && only[*agentreplv1.FinishWorkspaceRequest](t, f).GetPlacement().GetBehind().GetMerge() != tt.behind {
				t.Errorf("the deploy was not placed behind %s", tt.behind)
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

func (c *cliClient) SetWorkspaceName(_ context.Context, req *connect.Request[agentreplv1.SetWorkspaceNameRequest]) (*connect.Response[agentreplv1.SetWorkspaceNameResponse], error) {
	return connect.NewResponse(&agentreplv1.SetWorkspaceNameResponse{}), c.record(req.Msg)
}

func (c *cliClient) SetWorkspaceSummary(_ context.Context, req *connect.Request[agentreplv1.SetWorkspaceSummaryRequest]) (*connect.Response[agentreplv1.SetWorkspaceSummaryResponse], error) {
	return connect.NewResponse(&agentreplv1.SetWorkspaceSummaryResponse{}), c.record(req.Msg)
}

func TestSummarySendsTheSummaryForTheNamedWorkspace(t *testing.T) {
	tests := []struct {
		name   string
		ws     string
		wantID string
	}{
		{name: "the calling workspace, as here", ws: "here", wantID: "ws-caller"},
		{name: "a workspace named by its roster name", ws: "peer", wantID: "ws-peer"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(workspaceNoun, "summary", tt.ws, "-text", "Fix the footer")

			// Assert
			if code != exitSuccess {
				t.Fatalf("exit %d, stderr %q", code, f.errOut.String())
			}
			req := only[*agentreplv1.SetWorkspaceSummaryRequest](t, f)
			if req.GetWorkspace().GetId() != tt.wantID || req.GetSummary() != "Fix the footer" {
				t.Errorf("request = %v, want %s summarized as Fix the footer", req, tt.wantID)
			}
		})
	}
}

func TestSummaryRefusesAMisspelledCall(t *testing.T) {
	tests := []struct {
		name string
		args []string
	}{
		{name: "no text", args: []string{"summary", "here"}},
		{name: "a positional word", args: []string{"summary", "here", "-text", "Fix the footer", "extra"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(workspaceNoun, tt.args...)

			// Assert
			if code != exitFailure || len(f.client.requests) != 0 {
				t.Fatalf("exit %d, requests %v; want a usage failure and nothing sent", code, f.client.requests)
			}
		})
	}
}

func TestSetNameSendsTheRename(t *testing.T) {
	tests := []struct {
		name   string
		ws     string
		wantID string
	}{
		{name: "the calling workspace, as here", ws: "here", wantID: "ws-caller"},
		{name: "a workspace named by its roster name", ws: "peer", wantID: "ws-peer"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(workspaceNoun, "set", tt.ws, "-name", "better-name")

			// Assert
			if code != exitSuccess {
				t.Fatalf("exit %d, stderr %q", code, f.errOut.String())
			}
			req := only[*agentreplv1.SetWorkspaceNameRequest](t, f)
			if req.GetWorkspace().GetId() != tt.wantID || req.GetName() != "better-name" {
				t.Errorf("request = %v, want %s renamed better-name", req, tt.wantID)
			}
		})
	}
}

func (c *cliClient) UpdateRemoteControlMode(_ context.Context, req *connect.Request[agentreplv1.UpdateRemoteControlModeRequest]) (*connect.Response[agentreplv1.UpdateRemoteControlModeResponse], error) {
	if err := c.record(req.Msg); err != nil {
		return nil, err
	}
	if c.remote != nil {
		return connect.NewResponse(c.remote), nil
	}
	return connect.NewResponse(&agentreplv1.UpdateRemoteControlModeResponse{Result: &agentreplv1.UpdateRemoteControlModeResponse_Success{
		Success: &agentreplv1.UpdateRemoteControlModeSuccess{On: req.Msg.GetOn() != nil}}}), nil
}

func (c *cliClient) UpdateQueueBypassMode(_ context.Context, req *connect.Request[agentreplv1.UpdateQueueBypassModeRequest]) (*connect.Response[agentreplv1.UpdateQueueBypassModeResponse], error) {
	if err := c.record(req.Msg); err != nil {
		return nil, err
	}
	return connect.NewResponse(&agentreplv1.UpdateQueueBypassModeResponse{Result: &agentreplv1.UpdateQueueBypassModeResponse_Success{
		Success: &agentreplv1.UpdateQueueBypassModeSuccess{On: req.Msg.GetOn() != nil}}}), nil
}

func TestSetQueueBypassSendsTheModeAndTheNamedContext(t *testing.T) {
	tests := []struct {
		name string
		args []string
		want *agentreplv1.UpdateQueueBypassModeRequest
	}{
		{name: "on naming no context", args: []string{"-queue-bypass", "on"},
			want: &agentreplv1.UpdateQueueBypassModeRequest{Action: &agentreplv1.UpdateQueueBypassModeRequest_On{On: &agentreplv1.UpdateQueueBypassModeOn{}}}},
		{name: "off", args: []string{"-queue-bypass", "off"},
			want: &agentreplv1.UpdateQueueBypassModeRequest{Action: &agentreplv1.UpdateQueueBypassModeRequest_Off{Off: &agentreplv1.UpdateQueueBypassModeOff{}}}},
		{name: "on with full main context", args: []string{"-queue-bypass", "on", "-context", "full"},
			want: bypassOnWith(&frontendv1.TopbarQueueBypassContext{Context: &frontendv1.TopbarQueueBypassContext_FullMain{FullMain: &frontendv1.TopbarQueueBypassContextFullMain{}}})},
		{name: "on with compacted main context", args: []string{"-queue-bypass", "on", "-context", "compacted"},
			want: bypassOnWith(&frontendv1.TopbarQueueBypassContext{Context: &frontendv1.TopbarQueueBypassContext_CompactedMain{CompactedMain: &frontendv1.TopbarQueueBypassContextCompactedMain{}}})},
		{name: "on with existing bypass context", args: []string{"-queue-bypass", "on", "-context", "existing"},
			want: bypassOnWith(&frontendv1.TopbarQueueBypassContext{Context: &frontendv1.TopbarQueueBypassContext_Existing{Existing: &frontendv1.TopbarQueueBypassContextExisting{}}})},
		{name: "on with existing plus compacted main context", args: []string{"-queue-bypass", "on", "-context", "existing-plus-compacted"},
			want: bypassOnWith(&frontendv1.TopbarQueueBypassContext{Context: &frontendv1.TopbarQueueBypassContext_ExistingPlusCompactedMain{ExistingPlusCompactedMain: &frontendv1.TopbarQueueBypassContextExistingPlusCompactedMain{}}})},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(workspaceNoun, append([]string{"set", "peer"}, tt.args...)...)

			// Assert
			if code != exitSuccess {
				t.Fatalf("exit %d, stderr %q", code, f.errOut.String())
			}
			req := only[*agentreplv1.UpdateQueueBypassModeRequest](t, f)
			tt.want.Workspace = req.GetWorkspace()
			if req.GetWorkspace().GetId() != "ws-peer" || !proto.Equal(req, tt.want) {
				t.Errorf("request = %v, want %v for ws-peer", req, tt.want)
			}
		})
	}
}

// bypassOnWith is an on request naming CONTEXT.
func bypassOnWith(named *frontendv1.TopbarQueueBypassContext) *agentreplv1.UpdateQueueBypassModeRequest {
	return &agentreplv1.UpdateQueueBypassModeRequest{Action: &agentreplv1.UpdateQueueBypassModeRequest_On{On: &agentreplv1.UpdateQueueBypassModeOn{Context: named}}}
}

func TestSetQueueBypassRefusesAMisusedFlag(t *testing.T) {
	tests := []struct {
		name string
		args []string
		want string
	}{
		{name: "a toggle, which is the editor's", args: []string{"-queue-bypass", "toggle"}, want: "-queue-bypass is on or off"},
		{name: "an unknown context", args: []string{"-queue-bypass", "on", "-context", "half"}, want: "-context is full, compacted, existing or existing-plus-compacted"},
		{name: "a context with off", args: []string{"-queue-bypass", "off", "-context", "full"}, want: "-context is named only with -queue-bypass on"},
		{name: "a context with no mode", args: []string{"-context", "full"}, want: "-context is named only with -queue-bypass on"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(workspaceNoun, append([]string{"set", "peer"}, tt.args...)...)

			// Assert
			if code != exitFailure || len(f.client.requests) != 0 || !strings.Contains(f.errOut.String(), tt.want) {
				t.Fatalf("exit %d, requests %v, stderr %q; want a usage failure naming %q and nothing sent", code, f.client.requests, f.errOut.String(), tt.want)
			}
		})
	}
}

func TestSetRemoteControlSendsTheModeForTheNamedWorkspace(t *testing.T) {
	tests := []struct {
		name   string
		ws     string
		mode   string
		wantID string
		wantOn bool
	}{
		{name: "another workspace by its name, on", ws: "peer", mode: "on", wantID: "ws-peer", wantOn: true},
		{name: "another workspace by its directory, off", ws: "/wt/peer", mode: "off", wantID: "ws-peer"},
		{name: "the calling workspace, as here", ws: "here", mode: "on", wantID: "ws-caller", wantOn: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(workspaceNoun, "set", tt.ws, "-remote-control", tt.mode)

			// Assert
			if code != exitSuccess {
				t.Fatalf("exit %d, stderr %q", code, f.errOut.String())
			}
			req := only[*agentreplv1.UpdateRemoteControlModeRequest](t, f)
			if req.GetWorkspace().GetId() != tt.wantID || (req.GetOn() != nil) != tt.wantOn || (req.GetOff() != nil) == tt.wantOn {
				t.Errorf("request = %v, want %s turned %s", req, tt.wantID, tt.mode)
			}
			if !strings.Contains(f.out.String(), `"success"`) {
				t.Errorf("printed %q, want the daemon's success", f.out.String())
			}
		})
	}
}

func TestSetRemoteControlRefusesAModeItDoesNotKnow(t *testing.T) {
	tests := []struct {
		name string
		mode string
	}{
		{name: "a toggle, which is the editor's", mode: "toggle"},
		{name: "a misspelling", mode: "yes"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(workspaceNoun, "set", "peer", "-remote-control", tt.mode)

			// Assert
			if code != exitFailure || len(f.client.requests) != 0 || !strings.Contains(f.errOut.String(), "-remote-control is on or off") {
				t.Fatalf("exit %d, requests %v, stderr %q; want a usage failure and nothing sent", code, f.client.requests, f.errOut.String())
			}
		})
	}
}

func TestSetRemoteControlFailsOnAWorkspaceTheRosterLacks(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	code := f.run(workspaceNoun, "set", "nobody", "-remote-control", "on")

	// Assert
	if code != exitFailure || len(f.client.requests) != 0 || !strings.Contains(f.errOut.String(), `no workspace named "nobody"`) {
		t.Fatalf("exit %d, requests %v, stderr %q; want the unknown name and nothing sent", code, f.client.requests, f.errOut.String())
	}
}

func TestSetRemoteControlPrintsTheDaemonsTypedRefusal(t *testing.T) {
	tests := []struct {
		name  string
		cause *agentreplv1.UpdateRemoteControlModeError
		want  string
	}{
		{name: "a workspace the daemon does not know", cause: &agentreplv1.UpdateRemoteControlModeError{
			Cause: &agentreplv1.UpdateRemoteControlModeError_UnknownWorkspace{UnknownWorkspace: &agentreplv1.UpdateRemoteControlModeUnknownWorkspace{}}}, want: `"unknownWorkspace"`},
		{name: "the vendor's refusal with its detail", cause: &agentreplv1.UpdateRemoteControlModeError{
			Cause: &agentreplv1.UpdateRemoteControlModeError_VendorRefused{VendorRefused: &agentreplv1.UpdateRemoteControlModeVendorRefused{Detail: "disabled by policy"}}}, want: "disabled by policy"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()
			f.client.remote = &agentreplv1.UpdateRemoteControlModeResponse{Result: &agentreplv1.UpdateRemoteControlModeResponse_Error{Error: tt.cause}}

			// Act
			code := f.run(workspaceNoun, "set", "peer", "-remote-control", "on")

			// Assert
			if code != exitRefused || !strings.Contains(f.out.String(), tt.want) {
				t.Fatalf("exit %d, printed %q; want exit %d and %s printed", code, f.out.String(), exitRefused, tt.want)
			}
		})
	}
}

func TestSetRemoteControlDryRunSendsNothing(t *testing.T) {
	// Arrange: the dry run's interceptor stops the change as errDryRun.
	f := newCLIFixture()
	f.client.err = errDryRun

	// Act
	code := f.run(workspaceNoun, "set", "peer", "-remote-control", "on", "-dry-run")

	// Assert
	if code != exitSuccess || f.dialOptions != 1 {
		t.Fatalf("exit %d, dial options %d; want success with the dry run's interceptor installed", code, f.dialOptions)
	}
	if f.out.Len() != 0 {
		t.Errorf("printed %q, want no answer from a request that was never sent", f.out.String())
	}
}

func TestLinkSendsTheChange(t *testing.T) {
	tests := []struct {
		name string
		args []string
		want *agentreplv1.ChangeWorkspaceLinksRequest
	}{
		{name: "-url with -label and -owned sets an owned link", args: []string{"-url", "https://example.com/pr/3", "-label", "The pull request", "-owned"},
			want: &agentreplv1.ChangeWorkspaceLinksRequest{Change: &agentreplv1.ChangeWorkspaceLinksRequest_Set{
				Set: &agentreplv1.ChangeWorkspaceLinksSet{Url: "https://example.com/pr/3", Label: "The pull request", Ownership: workspace.LinkOwnershipView(true)}}}},
		{name: "-url with -label and -reference sets a reference", args: []string{"-url", "https://example.com/t/3", "-label", "The tracking ticket", "-reference"},
			want: &agentreplv1.ChangeWorkspaceLinksRequest{Change: &agentreplv1.ChangeWorkspaceLinksRequest_Set{
				Set: &agentreplv1.ChangeWorkspaceLinksSet{Url: "https://example.com/t/3", Label: "The tracking ticket", Ownership: workspace.LinkOwnershipView(false)}}}},
		{name: "-announces with -owned names the link it announces", args: []string{"-url", "https://w.slack.com/archives/C1/p1", "-label", "The pull request's announcement", "-owned", "-announces", "https://example.com/pr/3"},
			want: &agentreplv1.ChangeWorkspaceLinksRequest{Change: &agentreplv1.ChangeWorkspaceLinksRequest_Set{
				Set: &agentreplv1.ChangeWorkspaceLinksSet{Url: "https://w.slack.com/archives/C1/p1", Label: "The pull request's announcement", Ownership: &agentreplv1.WorkspaceLinkOwnership{
					Ownership: &agentreplv1.WorkspaceLinkOwnership_Owned{Owned: &agentreplv1.WorkspaceLinkOwned{Announces: &agentreplv1.WorkspaceLinkAnnouncement{Url: "https://example.com/pr/3"}}}}}}}},
		{name: "-remove removes one", args: []string{"-remove", "https://example.com/pr/3"},
			want: &agentreplv1.ChangeWorkspaceLinksRequest{Change: &agentreplv1.ChangeWorkspaceLinksRequest_Remove{
				Remove: &agentreplv1.ChangeWorkspaceLinksRemove{Url: "https://example.com/pr/3"}}}},
		{name: "-show repeated names each shown link in order", args: []string{"-show", "https://a", "-show", "https://b"},
			want: &agentreplv1.ChangeWorkspaceLinksRequest{Change: &agentreplv1.ChangeWorkspaceLinksRequest_Show{
				Show: &agentreplv1.ChangeWorkspaceLinksShow{Urls: []string{"https://a", "https://b"}}}}},
		{name: "-clear clears them", args: []string{"-clear"},
			want: &agentreplv1.ChangeWorkspaceLinksRequest{Change: &agentreplv1.ChangeWorkspaceLinksRequest_Clear{
				Clear: &agentreplv1.ChangeWorkspaceLinksClear{}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(workspaceNoun, append([]string{"link", "here"}, tt.args...)...)

			// Assert
			if code != exitSuccess {
				t.Fatalf("exit %d, stderr %q", code, f.errOut.String())
			}
			req := only[*agentreplv1.ChangeWorkspaceLinksRequest](t, f)
			if req.GetWorkspace() == nil {
				t.Error("the request names no workspace")
			}
			req.Workspace = nil
			if !proto.Equal(req, tt.want) {
				t.Errorf("request = %v, want %v", req, tt.want)
			}
		})
	}
}

func TestLinkRefusesAMalformedChange(t *testing.T) {
	tests := []struct {
		name string
		args []string
	}{
		{name: "no change", args: nil},
		{name: "a url with no label", args: []string{"-url", "https://example.com", "-owned"}},
		{name: "a set naming no ownership", args: []string{"-url", "https://example.com", "-label", "The design doc"}},
		{name: "a set naming both ownerships", args: []string{"-url", "https://example.com", "-label", "The design doc", "-owned", "-reference"}},
		{name: "an ownership beside a removal", args: []string{"-remove", "https://a", "-owned"}},
		{name: "a label with no url", args: []string{"-label", "The design doc"}},
		{name: "two changes at once", args: []string{"-remove", "https://a", "-clear"}},
		{name: "a reference that announces", args: []string{"-url", "https://w.slack.com/archives/C1/p1", "-label", "The announcement", "-reference", "-announces", "https://example.com/pr/3"}},
		{name: "an announcement beside a removal", args: []string{"-remove", "https://a", "-announces", "https://b"}},
		{name: "a positional argument", args: []string{"-clear", "extra"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(workspaceNoun, append([]string{"link", "here"}, tt.args...)...)

			// Assert
			if code != exitFailure || len(f.client.requests) != 0 {
				t.Fatalf("exit %d, requests %v; want a usage failure and nothing sent", code, f.client.requests)
			}
			if f.errOut.Len() == 0 {
				t.Error("stderr is empty, want the usage problem named")
			}
		})
	}
}

func TestStatusPrintsTheLinks(t *testing.T) {
	// Arrange
	f := newCLIFixture()
	link := "https://example.com/pr/4"
	f.client.status = &agentreplv1.GetWorkspaceStatusResponse{Result: &agentreplv1.GetWorkspaceStatusResponse_Success{Success: &agentreplv1.GetWorkspaceStatusSuccess{
		Status: &agentreplv1.WorkspaceStatus{Repository: &workspacev1.RepositoryRef{Id: "repo-1"},
			Links: &agentreplv1.WorkspaceLinks{Links: []*agentreplv1.WorkspaceLink{{Url: link, Label: "The pull request"}}}}}}}

	// Act
	code := f.run(workspaceNoun, "status", "here")

	// Assert
	if code != exitSuccess {
		t.Fatalf("exit %d, stderr %q", code, f.errOut.String())
	}
	if !strings.Contains(f.out.String(), link) || !strings.Contains(f.out.String(), "The pull request") {
		t.Fatalf("stdout = %q, want it to carry the link and its label", f.out.String())
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

func TestPromptAsUserSaysTheUserAsked(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	code := f.run(workspaceNoun, "prompt", "-as-user", "peer", "please rebase")

	// Assert
	req := only[*agentreplv1.SubmitPromptRequest](t, f)
	if code != exitSuccess || !req.GetAsUser() {
		t.Fatalf("exit %d, prompt %v; want the send marked as the user's", code, req)
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
		{name: "the pull request event", events: "pull_request,landed", code: exitSuccess},
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

func TestSubscribeSpellsEveryStoreEventByItsStoreName(t *testing.T) {
	for _, e := range wsm.AllWorkspaceEvents().Members() {
		t.Run(e.String(), func(t *testing.T) {
			// Act
			got, ok := noticeEvents[e.String()]

			// Assert
			if !ok || got != workspacesubs.EventProto(e) {
				t.Errorf("-events %q = %v (%v), want %v", e, got, ok, workspacesubs.EventProto(e))
			}
		})
	}
	if len(noticeEvents) != len(wsm.AllWorkspaceEvents().Members()) {
		t.Errorf("noticeEvents spells %d events, want exactly the store's %d", len(noticeEvents), len(wsm.AllWorkspaceEvents().Members()))
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

func TestPruneStorageAsksForASweepOrAPreview(t *testing.T) {
	tests := []struct {
		name    string
		args    []string
		code    int
		preview bool
	}{
		{name: "a sweep", args: nil, code: exitSuccess},
		{name: "a preview", args: []string{"-preview"}, code: exitSuccess, preview: true},
		{name: "a positional", args: []string{"now"}, code: exitFailure},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(runtimeNoun, append([]string{"prune-storage"}, tt.args...)...)

			// Assert
			if code != tt.code {
				t.Fatalf("exit %d, stderr %q; want %d", code, f.errOut.String(), tt.code)
			}
			if tt.code != exitSuccess {
				return
			}
			if req := only[*agentreplv1.PruneStorageRequest](t, f); req.GetDryRun() != tt.preview {
				t.Fatalf("request = %v, want dry run %v", req, tt.preview)
			}
		})
	}
}

func TestReapWorktreesAsksForASweepAPreviewOrAForce(t *testing.T) {
	tests := []struct {
		name    string
		args    []string
		code    int
		preview bool
		force   bool
	}{
		{name: "a sweep", args: nil, code: exitSuccess},
		{name: "a preview", args: []string{"-preview"}, code: exitSuccess, preview: true},
		{name: "a forced sweep", args: []string{"-force"}, code: exitSuccess, force: true},
		{name: "a forced preview", args: []string{"-preview", "-force"}, code: exitSuccess, preview: true, force: true},
		{name: "a positional", args: []string{"now"}, code: exitFailure},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(runtimeNoun, append([]string{"reap-worktrees"}, tt.args...)...)

			// Assert
			if code != tt.code {
				t.Fatalf("exit %d, stderr %q; want %d", code, f.errOut.String(), tt.code)
			}
			if tt.code != exitSuccess {
				return
			}
			if req := only[*agentreplv1.ReapWorktreesRequest](t, f); req.GetDryRun() != tt.preview || req.GetForce() != tt.force {
				t.Fatalf("request = %v, want dry run %v and force %v", req, tt.preview, tt.force)
			}
		})
	}
}

func TestRollOutLandingDialsTheDaemonItIsNamed(t *testing.T) {
	tests := []struct {
		name string
		args []string
		want string
	}{
		{name: "the deploy's daemon", args: []string{"-deploy", "0011aabb", "-daemon", "127.0.0.1:4242"}, want: "127.0.0.1:4242"},
		{name: "the serving daemon when none is named", args: []string{"-deploy", "0011aabb"}, want: "127.0.0.1:1"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(runtimeNoun, append([]string{"roll-out-landing"}, tt.args...)...)

			// Assert
			if code != exitSuccess || !slices.Equal(f.dialed, []string{tt.want}) {
				t.Fatalf("exit %d dialed %v, want %s", code, f.dialed, tt.want)
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
	code := f.run(workspaceNoun, "create", "-repository", "here", "-relationship", "peer", "-context", "fresh",
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
			args := append([]string{"create", "-repository", "here", "-relationship", "peer", "-context", "fresh"}, tt.flag...)

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

// listedRecords is a complete find answering two records: the caller's own,
// drawn thinking in the open section with its summary, and a peer's drawn
// done in the hidden section.
func listedRecords() *agentreplv1.FindWorkspacesResponse {
	summary := "Fix the footer"
	return &agentreplv1.FindWorkspacesResponse{Result: &agentreplv1.FindWorkspacesResponse_Success{Success: &agentreplv1.FindWorkspacesSuccess{
		Extent: &agentreplv1.FindWorkspacesSuccess_Complete{Complete: &agentreplv1.FindWorkspacesSuccessComplete{Workspaces: []*agentreplv1.FoundWorkspace{
			{Workspace: &workspacev1.WorkspaceRef{Id: "ws-caller", Dir: "/wt/caller"}, Name: "caller", Summary: &summary, Calling: true,
				Roster: &agentreplv1.FoundWorkspaceRoster{
					Section: &agentreplv1.FoundWorkspaceRosterSection{Section: &agentreplv1.FoundWorkspaceRosterSection_Open{Open: &agentreplv1.FoundWorkspaceRosterSectionOpen{}}},
					Status:  &agentreplv1.FoundWorkspaceRosterStatus{Arm: "thinking"}}},
			{Workspace: &workspacev1.WorkspaceRef{Id: "ws-peer", Dir: "/wt/peer"}, Name: "peer",
				Roster: &agentreplv1.FoundWorkspaceRoster{
					Section: &agentreplv1.FoundWorkspaceRosterSection{Section: &agentreplv1.FoundWorkspaceRosterSection_Hidden{Hidden: &agentreplv1.FoundWorkspaceRosterSectionHidden{}}},
					Status:  &agentreplv1.FoundWorkspaceRosterStatus{Arm: "done"}}},
		}}}}}}
}

// printedRecords decodes OUT as one agentrepl.v1.FoundWorkspace a line.
func printedRecords(t *testing.T, out string) []*agentreplv1.FoundWorkspace {
	t.Helper()
	var records []*agentreplv1.FoundWorkspace
	for _, line := range strings.Split(strings.TrimSpace(out), "\n") {
		record := &agentreplv1.FoundWorkspace{}
		if err := protojson.Unmarshal([]byte(line), record); err != nil {
			t.Fatalf("line %q is no FoundWorkspace: %v", line, err)
		}
		records = append(records, record)
	}
	return records
}

func TestListAsksForTheRosterAsTheCaller(t *testing.T) {
	tests := []struct {
		name       string
		cwd        string
		wantCaller string
	}{
		{name: "inside a workspace names it the caller", cwd: "/wt/caller/src", wantCaller: "ws-caller"},
		{name: "outside every workspace names no caller", cwd: "/elsewhere", wantCaller: ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()
			f.cwd = tt.cwd

			// Act
			code := f.run(workspaceNoun, "list")

			// Assert
			req := only[*agentreplv1.FindWorkspacesRequest](t, f)
			if code != exitSuccess || req.OnRoster == nil || req.GetCaller().GetId() != tt.wantCaller ||
				req.Limit != nil || req.Repository != nil || req.Standing != nil {
				t.Fatalf("exit %d, sent %v; want on_roster, caller %q and nothing else", code, req, tt.wantCaller)
			}
		})
	}
}

func TestListPrintsEveryRecordOneALine(t *testing.T) {
	// Arrange
	f := newCLIFixture()
	f.client.found = listedRecords()

	// Act
	code := f.run(workspaceNoun, "list")

	// Assert
	if code != exitSuccess {
		t.Fatalf("exit %d, stderr %q", code, f.errOut.String())
	}
	got := printedRecords(t, f.out.String())
	want := f.client.found.GetSuccess().GetComplete().GetWorkspaces()
	if len(got) != len(want) {
		t.Fatalf("printed %d records, want %d", len(got), len(want))
	}
	for i := range want {
		if !proto.Equal(got[i], want[i]) {
			t.Fatalf("line %d = %v, want %v", i, got[i], want[i])
		}
	}
}

// EVERY FACT A LIST LINE CARRIED BEFORE IT ANSWERED THE FIND RECORD is still
// on the line, under the record's JSON names: name, id, dir, live status,
// section and calling, and the summary it gained.
func TestListLinesCarryEveryLegacyFactAndTheSummary(t *testing.T) {
	// Arrange
	f := newCLIFixture()
	f.client.found = listedRecords()

	// Act
	f.run(workspaceNoun, "list")

	// Assert
	lines := strings.Split(strings.TrimSpace(f.out.String()), "\n")
	for i, want := range [][]string{
		{`"name":"caller"`, `"id":"ws-caller"`, `"dir":"/wt/caller"`, `"arm":"thinking"`, `"section":{"open":{}}`, `"calling":true`, `"summary":"Fix the footer"`},
		{`"name":"peer"`, `"id":"ws-peer"`, `"dir":"/wt/peer"`, `"arm":"done"`, `"section":{"hidden":{}}`},
	} {
		for _, fact := range want {
			if !strings.Contains(lines[i], fact) {
				t.Fatalf("line %d %q lacks %s", i, lines[i], fact)
			}
		}
	}
	if strings.Contains(lines[1], `"calling"`) {
		t.Fatalf("line 1 %q marks a workspace that is not the caller", lines[1])
	}
}

// LIST AND FIND ANSWER ONE RECORD SHAPE: over the same daemon answer, every
// line `list` prints is exactly a record `find -all` prints.
func TestListAndFindPrintTheSameRecords(t *testing.T) {
	// Arrange
	listed, found := newCLIFixture(), newCLIFixture()
	listed.client.found, found.client.found = listedRecords(), listedRecords()

	// Act
	listCode := listed.run(workspaceNoun, "list")
	findCode := found.run(workspaceNoun, "find", "-all")

	// Assert
	if listCode != exitSuccess || findCode != exitSuccess {
		t.Fatalf("list exit %d, find exit %d", listCode, findCode)
	}
	var printed agentreplv1.FindWorkspacesResponse
	if err := protojson.Unmarshal(found.out.Bytes(), &printed); err != nil {
		t.Fatalf("find printed no FindWorkspacesResponse: %v", err)
	}
	fromFind := printed.GetSuccess().GetComplete().GetWorkspaces()
	fromList := printedRecords(t, listed.out.String())
	if len(fromList) != len(fromFind) {
		t.Fatalf("list printed %d records, find %d", len(fromList), len(fromFind))
	}
	for i := range fromFind {
		if !proto.Equal(fromList[i], fromFind[i]) {
			t.Fatalf("record %d: list %v, find %v", i, fromList[i], fromFind[i])
		}
	}
}

func TestListExitsRefusedOnARefusedFind(t *testing.T) {
	// Arrange
	f := newCLIFixture()
	f.client.found = &agentreplv1.FindWorkspacesResponse{Result: &agentreplv1.FindWorkspacesResponse_Error{Error: &agentreplv1.FindWorkspacesError{
		Cause: &agentreplv1.FindWorkspacesError_UnknownCaller{UnknownCaller: &agentreplv1.FindWorkspacesUnknownCaller{}}}}}

	// Act
	code := f.run(workspaceNoun, "list")

	// Assert
	if code != exitRefused || !strings.Contains(f.errOut.String(), "the daemon refused") {
		t.Fatalf("exit %d, stderr %q; want the refusal", code, f.errOut.String())
	}
}

func TestListFailsOnAPartialAnswer(t *testing.T) {
	// Arrange
	f := newCLIFixture()
	f.client.found = &agentreplv1.FindWorkspacesResponse{Result: &agentreplv1.FindWorkspacesResponse_Success{Success: &agentreplv1.FindWorkspacesSuccess{
		Extent: &agentreplv1.FindWorkspacesSuccess_Partial{Partial: &agentreplv1.FindWorkspacesSuccessPartial{Omitted: 1}}}}}

	// Act
	code := f.run(workspaceNoun, "list")

	// Assert
	if code != exitFailure || !strings.Contains(f.errOut.String(), "not a complete extent") || f.out.Len() != 0 {
		t.Fatalf("exit %d, stdout %q, stderr %q; want a failure printing nothing", code, f.out.String(), f.errOut.String())
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
	verb := regexp.MustCompile(`(?s)func workspace(Kill|Select)\(c \*cliCall, args \[\]string\) int \{\n(.*?)\n}\n`)

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

// A ONE-WORKSPACE VERB WITH FLAGS OF ITS OWN PARSES THROUGH onlyWorkspace, the
// parse onWorkspace runs, so its refusal of anything after the workspace
// cannot drift from onWorkspace's.
func TestEveryFlaggedOneWorkspaceVerbParsesThroughOnlyWorkspace(t *testing.T) {
	// Arrange
	var raw []byte
	for _, file := range sourcescan.Production(t) {
		if file.Name == "agentcli.go" {
			raw = file.Source
		}
	}
	body := regexp.MustCompile(`(?s)func (onWorkspace\[Res any\]|workspaceRestart)\(.*?\) int \{\n(.*?)\n}\n`)

	// Act
	matches := body.FindAllSubmatch(raw, -1)

	// Assert
	if len(matches) != 2 {
		t.Fatalf("found %d of onWorkspace and workspaceRestart, want both", len(matches))
	}
	for _, m := range matches {
		if !strings.Contains(string(m[2]), "c.onlyWorkspace(") {
			t.Errorf("%s parses its workspace without onlyWorkspace", m[1])
		}
	}
}

func TestRestartAsksForItsMode(t *testing.T) {
	tests := []struct {
		name     string
		flags    []string
		whenIdle bool
	}{
		{name: "at once by default"},
		{name: "once idle with -when-idle", flags: []string{"-when-idle"}, whenIdle: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(workspaceNoun, append([]string{"restart"}, append(tt.flags, "peer")...)...)

			// Assert
			req := only[*agentreplv1.RestartWorkspaceRequest](t, f)
			if code != exitSuccess || req.GetWhenIdle() != tt.whenIdle || req.GetWorkspace().GetId() != "ws-peer" {
				t.Fatalf("exit %d, request %v; want ws-peer with when_idle %v", code, req, tt.whenIdle)
			}
		})
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

func TestParkNamesItsWorkspace(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	code := f.run(workspaceNoun, "park", "peer")

	// Assert
	if got := only[*agentreplv1.ParkWorkspaceRequest](t, f).GetWorkspace().GetId(); code != exitSuccess || got != "ws-peer" {
		t.Fatalf("exit %d, workspace %q; want ws-peer", code, got)
	}
}

func TestParkAllAsksEveryOpenWorkspace(t *testing.T) {
	// Arrange
	f := newCLIFixture()
	var open []string
	for _, row := range rosterwalk.OpenRows(roster(cliRow("ws-caller", "/wt/caller", "caller"), cliRow("ws-peer", "/wt/peer", "peer"),
		cliRow("ws-twin-1", "/wt/twin-1", "twin"), cliRow("ws-twin-2", "/wt/twin-2", "twin"))) {
		open = append(open, row.GetRow().GetWorkspace().GetWorkspace().GetId())
	}

	// Act
	code := f.run(workspaceNoun, "park", "-all")

	// Assert
	var asked []string
	for _, req := range f.client.requests {
		asked = append(asked, req.(*agentreplv1.ParkWorkspaceRequest).GetWorkspace().GetId())
	}
	if code != exitSuccess || len(open) == 0 || !slices.Equal(asked, open) {
		t.Fatalf("exit %d, asked %v; want every open workspace %v", code, asked, open)
	}
}

func TestParkAllNamesNoWorkspace(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	code := f.run(workspaceNoun, "park", "-all", "peer")

	// Assert
	if code != exitFailure || len(f.client.requests) != 0 {
		t.Fatalf("exit %d, requests %v; want a failure and nothing sent", code, f.client.requests)
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
	code := f.run(workspaceNoun, "create", "-repository", "/repo", "-relationship", "peer", "-lands-into", "main", "-origin-master", "-context", "fresh", "-prompt", "fix it")

	// Assert
	req := only[*agentreplv1.CreateWorkspaceRequest](t, f)
	if code != exitSuccess || req.GetStandard().GetLandingTarget().GetPrimary().GetFromOrigin() == nil {
		t.Fatalf("exit %d, request %v; want a landing on main cut from origin", code, req)
	}
}

func TestCreateOriginMasterRefusesALandingNotOnMain(t *testing.T) {
	tests := []struct {
		name string
		args []string
	}{
		{name: "the relationship's default landing", args: []string{"-relationship", "child", "-parent", "peer"}},
		{name: "a landing into a workspace", args: []string{"-relationship", "peer", "-lands-into", "peer"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			args := append([]string{"create", "-repository", "/repo", "-origin-master", "-context", "fresh"}, tt.args...)
			code := f.run(workspaceNoun, args...)

			// Assert
			if code != exitFailure || len(f.client.requests) != 0 || !strings.Contains(f.errOut.String(), "-origin-master") {
				t.Fatalf("exit %d, requests %v, stderr %q; want the flag refused", code, f.client.requests, f.errOut.String())
			}
		})
	}
}

func TestCreateSendsTheLandingTargetItNames(t *testing.T) {
	tests := []struct {
		name string
		args []string
		want func(*agentreplv1.WorkspaceLandingTarget) bool
	}{
		{name: "a child landing on main", args: []string{"-relationship", "child", "-parent", "peer", "-lands-into", "main"},
			want: func(l *agentreplv1.WorkspaceLandingTarget) bool {
				return l.GetPrimary() != nil && l.GetPrimary().GetFromOrigin() == nil
			}},
		{name: "a peer landing into a workspace", args: []string{"-relationship", "peer", "-lands-into", "peer"},
			want: func(l *agentreplv1.WorkspaceLandingTarget) bool {
				return l.GetWorkspace().GetWorkspace().GetId() == "ws-peer"
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(workspaceNoun, append([]string{"create", "-repository", "here", "-context", "fresh"}, tt.args...)...)

			// Assert
			if code != exitSuccess {
				t.Fatalf("exit %d, stderr %q", code, f.errOut.String())
			}
			if req := only[*agentreplv1.CreateWorkspaceRequest](t, f); !tt.want(req.GetStandard().GetLandingTarget()) {
				t.Fatalf("landing target = %v, want %s", req.GetStandard().GetLandingTarget(), tt.name)
			}
		})
	}
}

func TestSetSendsTheRetargetItNames(t *testing.T) {
	tests := []struct {
		name string
		args []string
		want func(*agentreplv1.SetWorkspaceParentRequest) bool
	}{
		{name: "a new parent with the default landing", args: []string{"-parent", "peer"},
			want: func(r *agentreplv1.SetWorkspaceParentRequest) bool {
				return r.GetChildOf().GetParent().GetId() == "ws-peer" && r.LandingTarget == nil
			}},
		{name: "a top-level peer landing into a workspace", args: []string{"-top-level", "-lands-into", "peer"},
			want: func(r *agentreplv1.SetWorkspaceParentRequest) bool {
				return r.GetTopLevel() != nil && r.GetLandingTarget().GetWorkspace().GetWorkspace().GetId() == "ws-peer"
			}},
		{name: "the landing alone moved onto main", args: []string{"-lands-into", "main"},
			want: func(r *agentreplv1.SetWorkspaceParentRequest) bool {
				return r.GetUnchanged() != nil && r.GetLandingTarget().GetPrimary() != nil
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(workspaceNoun, append([]string{"set", "here"}, tt.args...)...)

			// Assert
			if code != exitSuccess {
				t.Fatalf("exit %d, stderr %q", code, f.errOut.String())
			}
			if req := only[*agentreplv1.SetWorkspaceParentRequest](t, f); !tt.want(req) {
				t.Fatalf("retarget = %v, want %s", req, tt.name)
			}
		})
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

func transferringAway(address string) *agentreplv1.CreateWorkspaceResponse {
	return &agentreplv1.CreateWorkspaceResponse{Result: &agentreplv1.CreateWorkspaceResponse_Error{Error: &agentreplv1.CreateWorkspaceError{
		Cause: &agentreplv1.CreateWorkspaceError_TransferringAway{TransferringAway: &agentreplv1.CreateWorkspaceTransferringAway{Address: address}},
	}}}
}

// TestCreateWhoseCallFailsAnswersTheErrorWithNoAnswerRead pins the create's
// error path: a call that failed carries no answer, so the handover check
// must not read one (a nil response panicked here on 2026-10-30).
func TestCreateWhoseCallFailsAnswersTheErrorWithNoAnswerRead(t *testing.T) {
	tests := []struct {
		name       string
		err        error
		wantCode   int
		wantStderr string
	}{
		{name: "a failed call is surfaced", err: errors.New("connection refused"), wantCode: exitFailure, wantStderr: "the daemon failed the call: connection refused"},
		{name: "a dry run is answered as success", err: errDryRun, wantCode: exitSuccess},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()
			f.client.createErr = tt.err

			// Act
			code := f.run(workspaceNoun, "create", "-repository", "here", "-relationship", "peer", "-context", "fresh", "-name", "named")

			// Assert: the error answered, never resent to a successor.
			if code != tt.wantCode {
				t.Fatalf("exit %d, stderr %q, want %d", code, f.errOut.String(), tt.wantCode)
			}
			if !strings.Contains(f.errOut.String(), tt.wantStderr) {
				t.Fatalf("stderr %q, want %q", f.errOut.String(), tt.wantStderr)
			}
			if len(f.dialed) != 1 {
				t.Fatalf("dialed %v, want only the daemon", f.dialed)
			}
		})
	}
}

func TestCreateRefusedStandingDownIsNotResent(t *testing.T) {
	// Arrange
	f := newCLIFixture()
	f.client.create = &agentreplv1.CreateWorkspaceResponse{Result: &agentreplv1.CreateWorkspaceResponse_Error{Error: &agentreplv1.CreateWorkspaceError{
		Cause: &agentreplv1.CreateWorkspaceError_StandingDown{StandingDown: &agentreplv1.CreateWorkspaceStandingDown{}},
	}}}

	// Act
	code := f.run(workspaceNoun, "create", "-repository", "here", "-relationship", "peer", "-context", "fresh")

	// Assert
	if code != exitRefused || len(f.dialed) != 1 {
		t.Fatalf("exit %d after dialing %v, want refused with no resend", code, f.dialed)
	}
}

// TestEveryQueueVerbNamesTheCallingWorkspaceAsWhoAsked pins the one sender
// (cliCall.updateMergeQueue): every queue action the command line sends names
// the calling workspace, so the daemon records who acted on the queue.
func TestEveryQueueVerbNamesTheCallingWorkspaceAsWhoAsked(t *testing.T) {
	tests := []struct {
		verb string
		args []string
	}{
		{verb: "dequeue", args: []string{"here"}},
		{verb: "move-up", args: []string{"here"}},
		{verb: "move-down", args: []string{"here"}},
		{verb: "pause", args: []string{"-repository", "here"}},
		{verb: "resume", args: []string{"-repository", "here"}},
	}
	for _, tt := range tests {
		t.Run(tt.verb, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(mergeQueueNoun, append([]string{tt.verb}, tt.args...)...)

			// Assert
			if code != exitSuccess {
				t.Fatalf("exit %d, stderr %q", code, f.errOut.String())
			}
			req := only[*agentreplv1.UpdateMergeQueueRequest](t, f)
			if got := req.GetRequester().GetWorkspace().GetWorkspace().GetId(); got != "ws-caller" {
				t.Fatalf("requester = %v, want the calling workspace", req.GetRequester())
			}
		})
	}
}

func TestPauseSendsHardOnlyWhenAsked(t *testing.T) {
	tests := []struct {
		name string
		args []string
		want bool
	}{
		{name: "a hard stop", args: []string{"pause", "-repository", "here", "-hard"}, want: true},
		{name: "an ordinary pause", args: []string{"pause", "-repository", "here"}, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(mergeQueueNoun, tt.args...)

			// Assert
			if code != exitSuccess {
				t.Fatalf("exit %d, stderr %q", code, f.errOut.String())
			}
			if got := only[*agentreplv1.UpdateMergeQueueRequest](t, f).GetPause().GetHard(); got != tt.want {
				t.Fatalf("hard = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestResumeTakesNoHardFlag(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	code := f.run(mergeQueueNoun, "resume", "-repository", "here", "-hard")

	// Assert
	if code == exitSuccess {
		t.Fatal("resume accepted -hard")
	}
}

func (c *cliClient) SubscribeMergeQueue(_ context.Context, req *connect.Request[agentreplv1.SubscribeMergeQueueRequest]) (*connect.Response[agentreplv1.SubscribeMergeQueueResponse], error) {
	if err := c.record(req.Msg); err != nil {
		return nil, err
	}
	return connect.NewResponse(&agentreplv1.SubscribeMergeQueueResponse{Result: &agentreplv1.SubscribeMergeQueueResponse_Success{Success: &agentreplv1.SubscribeMergeQueueSuccess{
		Taken: &agentreplv1.SubscribeMergeQueueSuccessTaken{Taken: &agentreplv1.SubscribeMergeQueueSuccessTaken_Subscribed{Subscribed: &agentreplv1.SubscribeMergeQueueSuccessTakenSubscribed{}}}}}}), nil
}

func TestSubscribeSubscribesTheCallerToEveryQueue(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	code := f.run(mergeQueueNoun, "subscribe")

	// Assert
	if code != exitSuccess {
		t.Fatalf("exit %d, stderr %q; want success", code, f.errOut.String())
	}
	req := only[*agentreplv1.SubscribeMergeQueueRequest](t, f)
	if req.GetSubscriber().GetId() != "ws-caller" || req.Repository != nil {
		t.Fatalf("request = %v, want the caller subscribed to every repository's queue", req)
	}
}

// A RETIRED -detail IS REFUSED, NOT IGNORED: an agent following an older
// skill learns at once that every subscription is now told the same changes.
func TestSubscribeRefusesTheRetiredDetailFlag(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	code := f.run(mergeQueueNoun, "subscribe", "-detail", "advances")

	// Assert
	if code == exitSuccess || len(f.client.requests) != 0 {
		t.Fatalf("exit %d, requests %v; want a usage failure sending nothing", code, f.client.requests)
	}
}

// arrangedSubsection is an arrangement entry: subsection ID named NAME.
func arrangedSubsection(id, name string) *agentreplv1.ArrangedEntry {
	return &agentreplv1.ArrangedEntry{Entry: &agentreplv1.ArrangedEntry_Subsection{Subsection: &agentreplv1.ArrangedSubsection{
		Subsection: &agentreplv1.SubsectionRef{Id: id}, Name: name}}}
}

// subsectionsArrangement is the caller's repository "repo-1" with subsections
// "later" and "twin" twice, after another repository with its own "later".
func subsectionsArrangement() *agentreplv1.GetSidebarArrangementResponse {
	return &agentreplv1.GetSidebarArrangementResponse{Repositories: []*agentreplv1.ArrangedRepository{
		{Repository: &workspacev1.RepositoryRef{Id: "repo-other"}, Name: "other",
			Entries: []*agentreplv1.ArrangedEntry{arrangedSubsection("sub-elsewhere", "later")}},
		{Repository: &workspacev1.RepositoryRef{Id: "repo-1"}, Name: "doom", Entries: []*agentreplv1.ArrangedEntry{
			{Entry: &agentreplv1.ArrangedEntry_Workspace{Workspace: &agentreplv1.ArrangedWorkspace{
				Workspace: &workspacev1.WorkspaceRef{Id: "ws-caller"}, Name: "caller"}}},
			arrangedSubsection("sub-later", "later"),
			arrangedSubsection("sub-twin-1", "twin"),
			arrangedSubsection("sub-twin-2", "twin"),
		}},
	}}
}

func TestSubsectionsPrintsTheArrangementOfTheWorkspacesRepository(t *testing.T) {
	// Arrange
	f := newCLIFixture()
	f.client.arrangement = subsectionsArrangement()

	// Act
	code := f.run(workspaceNoun, "subsections", "here")

	// Assert
	if code != exitSuccess {
		t.Fatalf("exit %d, stderr %q; want success", code, f.errOut.String())
	}
	var printed agentreplv1.ArrangedRepository
	if err := protojson.Unmarshal(f.out.Bytes(), &printed); err != nil {
		t.Fatalf("the output is no ArrangedRepository: %v\n%s", err, f.out.String())
	}
	if !proto.Equal(&printed, f.client.arrangement.GetRepositories()[1]) {
		t.Fatalf("printed %v, want the caller's repository", &printed)
	}
	if status := only[*agentreplv1.GetWorkspaceStatusRequest](t, f); status.GetWorkspace().GetId() != "ws-caller" {
		t.Fatalf("status read of %v, want the caller's", status.GetWorkspace())
	}
}

func TestSubsectionsFailsARepositoryTheArrangementDoesNotHold(t *testing.T) {
	// Arrange
	f := newCLIFixture()
	f.client.arrangement = &agentreplv1.GetSidebarArrangementResponse{}

	// Act
	code := f.run(workspaceNoun, "subsections", "here")

	// Assert
	if code != exitFailure || !strings.Contains(f.errOut.String(), "holds no repository repo-1") {
		t.Fatalf("exit %d, stderr %q; want the missing repository named", code, f.errOut.String())
	}
}

func TestSubsectionsReadsANamedWorkspacesRepository(t *testing.T) {
	// Arrange
	f := newCLIFixture()
	f.client.arrangement = subsectionsArrangement()

	// Act
	code := f.run(workspaceNoun, "subsections", "peer")

	// Assert
	if code != exitSuccess {
		t.Fatalf("exit %d, stderr %q; want success", code, f.errOut.String())
	}
	if status := only[*agentreplv1.GetWorkspaceStatusRequest](t, f); status.GetWorkspace().GetId() != "ws-peer" {
		t.Fatalf("status read of %v, want the peer's", status.GetWorkspace())
	}
}

func TestPlaceFilesTheWorkspacesAtTheSubsectionsEnd(t *testing.T) {
	// Arrange
	f := newCLIFixture()
	f.client.arrangement = subsectionsArrangement()

	// Act
	code := f.run(workspaceNoun, "place", "here", "peer", "-subsection", " later ")

	// Assert
	if code != exitSuccess {
		t.Fatalf("exit %d, stderr %q; want success", code, f.errOut.String())
	}
	place := only[*agentreplv1.ArrangeSidebarRequest](t, f).GetPlaceWorkspaces()
	want := &agentreplv1.ArrangePlaceWorkspaces{
		Workspaces: []*workspacev1.WorkspaceRef{{Id: "ws-caller", Dir: "/wt/caller"}, {Id: "ws-peer", Dir: "/wt/peer"}},
		Into:       &agentreplv1.ArrangePlaceWorkspaces_Subsection{Subsection: &agentreplv1.SubsectionRef{Id: "sub-later"}},
		At:         &agentreplv1.ArrangePlaceWorkspaces_End{End: &agentreplv1.ArrangeEnd{}},
	}
	if !proto.Equal(place, want) {
		t.Fatalf("placement = %v, want %v", place, want)
	}
}

func TestPlaceIntoANewSubsectionCreatesItAsTheCallingWorkspacesAgent(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	code := f.run(workspaceNoun, "place", "here", "peer", "-new-subsection", "auth work")

	// Assert
	if code != exitSuccess {
		t.Fatalf("exit %d, stderr %q; want success", code, f.errOut.String())
	}
	create := only[*agentreplv1.ArrangeSidebarRequest](t, f).GetCreateAgentSubsection()
	want := &agentreplv1.ArrangeCreateAgentSubsection{
		CreatedBy:  &workspacev1.WorkspaceRef{Id: "ws-caller", Dir: "/wt/caller"},
		Name:       "auth work",
		Workspaces: []*workspacev1.WorkspaceRef{{Id: "ws-caller", Dir: "/wt/caller"}, {Id: "ws-peer", Dir: "/wt/peer"}},
	}
	if !proto.Equal(create, want) {
		t.Fatalf("creation = %v, want %v", create, want)
	}
}

func TestPlaceIntoANewSubsectionFromNoWorkspaceSendsNothing(t *testing.T) {
	// Arrange
	f := newCLIFixture()
	f.cwd = "/elsewhere"

	// Act
	code := f.run(workspaceNoun, "place", "peer", "-new-subsection", "auth work")

	// Assert
	if code != exitFailure || !strings.Contains(f.errOut.String(), "inside no workspace") {
		t.Fatalf("exit %d, stderr %q; want the missing calling workspace named", code, f.errOut.String())
	}
	for _, req := range f.client.requests {
		if _, ok := req.(*agentreplv1.ArrangeSidebarRequest); ok {
			t.Fatalf("requests = %v, want no change sent", f.client.requests)
		}
	}
}

func TestPlaceRefusesASubsectionItCannotPick(t *testing.T) {
	tests := []struct {
		name    string
		args    []string
		wantErr string
	}{
		{name: "no subsection named", args: []string{"place", "here"}, wantErr: "-subsection NAME or -new-subsection NAME"},
		{name: "a blank subsection", args: []string{"place", "here", "-subsection", "  "}, wantErr: "-subsection NAME or -new-subsection NAME"},
		{name: "both destinations", args: []string{"place", "here", "-subsection", "later", "-new-subsection", "auth"},
			wantErr: "-subsection NAME or -new-subsection NAME"},
		{name: "no workspace", args: []string{"place", "-subsection", "later"}, wantErr: "names its workspace"},
		{name: "an unknown workspace", args: []string{"place", "here", "nobody", "-subsection", "later"}, wantErr: `no workspace named "nobody"`},
		{name: "an unknown subsection", args: []string{"place", "here", "-subsection", "sooner"},
			wantErr: `no subsection named "sooner"; its subsections are ["later", "twin", "twin"]`},
		{name: "another repository's subsection", args: []string{"place", "here", "-subsection", "sub-elsewhere"},
			wantErr: `no subsection named "sub-elsewhere"`},
		{name: "an ambiguous subsection", args: []string{"place", "here", "-subsection", "twin"},
			wantErr: `2 subsections named "twin"`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()
			f.client.arrangement = subsectionsArrangement()

			// Act
			code := f.run(workspaceNoun, tt.args...)

			// Assert
			if code != exitFailure || !strings.Contains(f.errOut.String(), tt.wantErr) {
				t.Fatalf("exit %d, stderr %q; want a failure saying %q", code, f.errOut.String(), tt.wantErr)
			}
			for _, req := range f.client.requests {
				if _, ok := req.(*agentreplv1.ArrangeSidebarRequest); ok {
					t.Fatalf("requests = %v, want no placement sent", f.client.requests)
				}
			}
		})
	}
}

func TestPlaceExitsRefusedOnTheDaemonsRefusal(t *testing.T) {
	// Arrange
	f := newCLIFixture()
	f.client.arrangement = subsectionsArrangement()
	f.client.arrange = &agentreplv1.ArrangeSidebarResponse{Result: &agentreplv1.ArrangeSidebarResponse_Error{Error: &agentreplv1.ArrangeSidebarError{
		Cause: &agentreplv1.ArrangeSidebarError_UnknownSubsection{UnknownSubsection: &agentreplv1.ArrangeSidebarUnknownSubsection{}}}}}

	// Act
	code := f.run(workspaceNoun, "place", "here", "-subsection", "later")

	// Assert
	if code != exitRefused {
		t.Fatalf("exit %d, stderr %q; want refused", code, f.errOut.String())
	}
}

// ---- workspace find --------------------------------------------------------

func TestFindSendsEveryNarrowingItNames(t *testing.T) {
	tests := []struct {
		name  string
		args  []string
		check func(*agentreplv1.FindWorkspacesRequest) bool
	}{
		{name: "nothing named sends the default limit and nothing else", args: nil, check: func(r *agentreplv1.FindWorkspacesRequest) bool {
			return r.GetLimit() == defaultFindLimit && r.Repository == nil && r.Window == nil && r.Standing == nil && r.Text == nil && r.CreatedBy == nil
		}},
		{name: "-all sends no limit", args: []string{"-all"}, check: func(r *agentreplv1.FindWorkspacesRequest) bool { return r.Limit == nil }},
		{name: "-limit", args: []string{"-limit", "3"}, check: func(r *agentreplv1.FindWorkspacesRequest) bool { return r.GetLimit() == 3 }},
		{name: "-repository here is the caller's repository", args: []string{"-repository", "here"}, check: func(r *agentreplv1.FindWorkspacesRequest) bool {
			return r.GetRepository().GetId() == "repo-1"
		}},
		{name: "-repository DIR", args: []string{"-repository", "/src/doom"}, check: func(r *agentreplv1.FindWorkspacesRequest) bool {
			return r.GetRepository().GetDir() == "/src/doom"
		}},
		{name: "-since an age in days", args: []string{"-since", "7d"}, check: func(r *agentreplv1.FindWorkspacesRequest) bool {
			return r.GetWindow().GetSinceMs() == cliNow.AddDate(0, 0, -7).UnixMilli() && r.GetWindow().UntilMs == nil
		}},
		{name: "-until an RFC 3339 instant", args: []string{"-until", "2026-10-20T08:00:00Z"}, check: func(r *agentreplv1.FindWorkspacesRequest) bool {
			return r.GetWindow().GetUntilMs() == time.Date(2026, 10, 20, 8, 0, 0, 0, time.UTC).UnixMilli() && r.GetWindow().SinceMs == nil
		}},
		{name: "-standing open", args: []string{"-standing", "open"}, check: func(r *agentreplv1.FindWorkspacesRequest) bool { return r.GetStanding().GetOpen() != nil }},
		{name: "-standing closed", args: []string{"-standing", "closed"}, check: func(r *agentreplv1.FindWorkspacesRequest) bool { return r.GetStanding().GetClosed() != nil }},
		{name: "-standing landed", args: []string{"-standing", "landed"}, check: func(r *agentreplv1.FindWorkspacesRequest) bool { return r.GetStanding().GetLanded() != nil }},
		{name: "-text", args: []string{"-text", "footer"}, check: func(r *agentreplv1.FindWorkspacesRequest) bool { return r.GetText() == "footer" }},
		{name: "-created-by here is the caller", args: []string{"-created-by", "here"}, check: func(r *agentreplv1.FindWorkspacesRequest) bool {
			return r.GetCreatedBy().GetId() == "ws-caller"
		}},
		{name: "the calling workspace is named the caller", args: nil, check: func(r *agentreplv1.FindWorkspacesRequest) bool {
			return r.GetCaller().GetId() == "ws-caller" && r.OnRoster == nil
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(workspaceNoun, append([]string{"find"}, tt.args...)...)

			// Assert
			if code != exitSuccess {
				t.Fatalf("exit %d, stderr %q; want success", code, f.errOut.String())
			}
			if req := only[*agentreplv1.FindWorkspacesRequest](t, f); !tt.check(req) {
				t.Fatalf("sent %v", req)
			}
		})
	}
}

func TestFindPrintsTheDaemonsAnswer(t *testing.T) {
	// Arrange
	f := newCLIFixture()
	summary := "Fix the footer"
	f.client.found = &agentreplv1.FindWorkspacesResponse{Result: &agentreplv1.FindWorkspacesResponse_Success{Success: &agentreplv1.FindWorkspacesSuccess{
		Extent: &agentreplv1.FindWorkspacesSuccess_Partial{Partial: &agentreplv1.FindWorkspacesSuccessPartial{
			Workspaces: []*agentreplv1.FoundWorkspace{{Name: "footer-fix", Summary: &summary}}, Omitted: 4}}}}}

	// Act
	code := f.run(workspaceNoun, "find")

	// Assert
	if code != exitSuccess {
		t.Fatalf("exit %d, stderr %q; want success", code, f.errOut.String())
	}
	var printed agentreplv1.FindWorkspacesResponse
	if err := protojson.Unmarshal(f.out.Bytes(), &printed); err != nil {
		t.Fatalf("the output is no FindWorkspacesResponse: %v\n%s", err, f.out.String())
	}
	if !proto.Equal(&printed, f.client.found) {
		t.Fatalf("printed %v, want the daemon's answer", &printed)
	}
}

func TestFindExitsRefusedOnAnUnknownRepository(t *testing.T) {
	// Arrange
	f := newCLIFixture()
	f.client.found = &agentreplv1.FindWorkspacesResponse{Result: &agentreplv1.FindWorkspacesResponse_Error{Error: &agentreplv1.FindWorkspacesError{
		Cause: &agentreplv1.FindWorkspacesError_UnknownRepository{UnknownRepository: &agentreplv1.FindWorkspacesUnknownRepository{}}}}}

	// Act
	code := f.run(workspaceNoun, "find", "-repository", "/src/nowhere")

	// Assert
	if code != exitRefused || !strings.Contains(f.errOut.String(), "the daemon refused") {
		t.Fatalf("exit %d, stderr %q; want the refusal", code, f.errOut.String())
	}
}

func TestFindRefusesAMalformedCommandBeforeCallingTheDaemon(t *testing.T) {
	tests := []struct {
		name string
		args []string
		want string
	}{
		{name: "a positional", args: []string{"footer"}, want: "nothing is positional"},
		{name: "-limit with -all", args: []string{"-limit", "3", "-all"}, want: "-limit and -all are exclusive"},
		{name: "a zero limit", args: []string{"-limit", "0"}, want: "-limit is at least 1"},
		{name: "an unknown standing", args: []string{"-standing", "merged"}, want: `-standing is open, closed or landed, not "merged"`},
		{name: "an unreadable since", args: []string{"-since", "last week"}, want: `-since: "last week" is no age`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(workspaceNoun, append([]string{"find"}, tt.args...)...)

			// Assert
			if code != exitFailure || !strings.Contains(f.errOut.String(), tt.want) {
				t.Fatalf("exit %d, stderr %q; want %q", code, f.errOut.String(), tt.want)
			}
			if len(f.client.requests) != 0 {
				t.Fatalf("requests = %v, want none sent", f.client.requests)
			}
		})
	}
}

func TestParseWhen(t *testing.T) {
	tests := []struct {
		name  string
		value string
		want  time.Time
	}{
		{name: "days", value: "7d", want: cliNow.AddDate(0, 0, -7)},
		{name: "zero days", value: "0d", want: cliNow},
		{name: "a duration", value: "36h", want: cliNow.Add(-36 * time.Hour)},
		{name: "a date at local midnight", value: "2026-10-20", want: time.Date(2026, 10, 20, 0, 0, 0, 0, time.Local)},
		{name: "an RFC 3339 instant", value: "2026-10-20T08:30:00Z", want: time.Date(2026, 10, 20, 8, 30, 0, 0, time.UTC)},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := parseWhen(tt.value, cliNow)

			// Assert
			if err != nil || !got.Equal(tt.want) {
				t.Fatalf("parseWhen(%q) = %v, %v; want %v", tt.value, got, err, tt.want)
			}
		})
	}
}

func TestParseWhenRefusesWhatItCannotRead(t *testing.T) {
	for _, value := range []string{"", "yesterday", "-3d", "-2h", "2026-13-01"} {
		t.Run(value, func(t *testing.T) {
			// Act
			_, err := parseWhen(value, cliNow)

			// Assert
			if err == nil || !strings.Contains(err.Error(), "is no age") {
				t.Fatalf("parseWhen(%q) = %v, want a refusal", value, err)
			}
		})
	}
}

func TestRepositoryRegisterSendsTheResolvedPathAndPrintsTheAnswer(t *testing.T) {
	tests := []struct {
		name        string
		args        []string
		wantPath    string
		registered  *agentreplv1.RegisterRepositoryResponse
		wantAlready bool
	}{
		{name: "a new repository by an absolute path", args: []string{"/repos/new/src"}, wantPath: "/repos/new/src"},
		{name: "here is the working directory", args: []string{"here"}, wantPath: "/wt/caller/src"},
		{name: "a relative path counts from the working directory", args: []string{"../lib"}, wantPath: "/wt/caller/lib"},
		{name: "an already known repository", args: []string{"/repos/new"}, wantPath: "/repos/new", registered: registerSuccess(true), wantAlready: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()
			f.client.registered = tt.registered

			// Act
			code := f.run(repositoryNoun, append([]string{"register"}, tt.args...)...)

			// Assert
			if code != exitSuccess {
				t.Fatalf("exit %d, stderr %q; want success", code, f.errOut.String())
			}
			if req := only[*agentreplv1.RegisterRepositoryRequest](t, f); req.GetPath() != tt.wantPath {
				t.Fatalf("path %q, want %q", req.GetPath(), tt.wantPath)
			}
			var printed agentreplv1.RegisterRepositoryResponse
			if err := protojson.Unmarshal(f.out.Bytes(), &printed); err != nil {
				t.Fatalf("stdout %q is no protojson answer: %v", f.out.String(), err)
			}
			if printed.GetSuccess().GetRepository().GetId() != "repo-9" || printed.GetSuccess().GetAlreadyKnown() != tt.wantAlready {
				t.Fatalf("printed %v, want repo-9 with already_known %v", &printed, tt.wantAlready)
			}
		})
	}
}

func TestRepositoryRegisterSurfacesTheDaemonsErrorArms(t *testing.T) {
	tests := []struct {
		name  string
		cause *agentreplv1.RegisterRepositoryError
		want  string
	}{
		{name: "not in a repository", cause: &agentreplv1.RegisterRepositoryError{Cause: &agentreplv1.RegisterRepositoryError_NotInARepository{NotInARepository: &agentreplv1.RegisterRepositoryNotInARepository{}}}, want: "notInARepository"},
		{name: "an unreadable path", cause: &agentreplv1.RegisterRepositoryError{Cause: &agentreplv1.RegisterRepositoryError_UnreadablePath{UnreadablePath: &agentreplv1.RegisterRepositoryUnreadablePath{}}}, want: "unreadablePath"},
		{name: "inside a temporary directory", cause: &agentreplv1.RegisterRepositoryError{Cause: &agentreplv1.RegisterRepositoryError_InsideTemporaryDirectory{InsideTemporaryDirectory: &agentreplv1.RegisterRepositoryInsideTemporaryDirectory{Dir: "/tmp/x", TemporaryRoot: "/tmp"}}}, want: "insideTemporaryDirectory"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()
			f.client.registered = &agentreplv1.RegisterRepositoryResponse{Result: &agentreplv1.RegisterRepositoryResponse_Error{Error: tt.cause}}

			// Act
			code := f.run(repositoryNoun, "register", "/nowhere")

			// Assert
			if code != exitRefused || !strings.Contains(f.out.String(), tt.want) || !strings.Contains(f.errOut.String(), "the daemon refused") {
				t.Fatalf("exit %d, stdout %q, stderr %q; want refused naming %s", code, f.out.String(), f.errOut.String(), tt.want)
			}
		})
	}
}

func TestRepositoryRegisterNeedsExactlyOnePath(t *testing.T) {
	tests := []struct {
		name string
		args []string
	}{
		{name: "no path", args: nil},
		{name: "two paths", args: []string{"/a", "/b"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(repositoryNoun, append([]string{"register"}, tt.args...)...)

			// Assert
			if code != exitFailure || len(f.client.requests) != 0 {
				t.Fatalf("exit %d with %d request(s), want a usage failure and none", code, len(f.client.requests))
			}
		})
	}
}

func TestTheRepositoryNounListsRegisterInItsUsage(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	code := f.run(repositoryNoun, "teleport")

	// Assert
	if code != exitFailure || !strings.Contains(f.errOut.String(), "agent-repld repository register here|PATH") {
		t.Fatalf("exit %d, stderr %q; want the usage listing register", code, f.errOut.String())
	}
}

func TestPullRequestVerbsSendTheirRequests(t *testing.T) {
	ref := &agentreplv1.PullRequestRef{Owner: "o", Repo: "r", Number: 7}
	tests := []struct {
		name  string
		args  []string
		check func(t *testing.T, f *cliFixture)
	}{
		{name: "pr-subscribe sends #N for the daemon to resolve, asking every event by default", args: []string{"pr-subscribe", "here", "-pr", "#7"},
			check: func(t *testing.T, f *cliFixture) {
				req := only[*agentreplv1.SubscribePullRequestRequest](t, f)
				if req.GetText() != "#7" || len(req.GetEvents()) != 6 {
					t.Errorf("request = %v, want #7 with every event", req)
				}
			}},
		{name: "pr-subscribe sends OWNER/REPO#N as a ref", args: []string{"pr-subscribe", "here", "-pr", "o/r#7", "-events", "checks"},
			check: func(t *testing.T, f *cliFixture) {
				req := only[*agentreplv1.SubscribePullRequestRequest](t, f)
				if !proto.Equal(req.GetRef(), ref) || len(req.GetEvents()) != 1 {
					t.Errorf("request = %v, want the ref and checks", req)
				}
			}},
		{name: "pr-unsubscribe sends a url as a ref", args: []string{"pr-unsubscribe", "here", "-pr", "https://github.com/o/r/pull/7"},
			check: func(t *testing.T, f *cliFixture) {
				if req := only[*agentreplv1.UnsubscribePullRequestRequest](t, f); !proto.Equal(req.GetPullRequest(), ref) {
					t.Errorf("request = %v, want the ref", req)
				}
			}},
		{name: "pr-subscriptions lists", args: []string{"pr-subscriptions", "here"},
			check: func(t *testing.T, f *cliFixture) {
				if req := only[*agentreplv1.ListPullRequestSubscriptionsRequest](t, f); req.GetSubscriber() == nil {
					t.Errorf("request = %v, want the subscriber", req)
				}
			}},
		{name: "pr-subscribe asks for the merge queue", args: []string{"pr-subscribe", "here", "-pr", "o/r#7", "-events", "merge_queue,state"},
			check: func(t *testing.T, f *cliFixture) {
				req := only[*agentreplv1.SubscribePullRequestRequest](t, f)
				want := []agentreplv1.PullRequestNoticeEvent{agentreplv1.PullRequestNoticeEvent_PULL_REQUEST_NOTICE_EVENT_MERGE_QUEUE, agentreplv1.PullRequestNoticeEvent_PULL_REQUEST_NOTICE_EVENT_STATE}
				if !slices.Equal(req.GetEvents(), want) {
					t.Errorf("events = %v, want %v", req.GetEvents(), want)
				}
			}},
		{name: "gh-queue-subscribe follows the workspace's own repository's default branch", args: []string{"gh-queue-subscribe", "here"},
			check: func(t *testing.T, f *cliFixture) {
				req := only[*agentreplv1.SubscribeGithubMergeQueueRequest](t, f)
				if req.GetSubscribers() == nil || req.Branch != nil {
					t.Errorf("request = %v, want the subscriber's repository and no branch", req)
				}
			}},
		{name: "gh-queue-subscribe follows a named repository's branch", args: []string{"gh-queue-subscribe", "here", "-repository", "o/r", "-branch", "release"},
			check: func(t *testing.T, f *cliFixture) {
				req := only[*agentreplv1.SubscribeGithubMergeQueueRequest](t, f)
				if req.GetNamed().GetOwner() != "o" || req.GetNamed().GetRepo() != "r" || req.GetBranch() != "release" {
					t.Errorf("request = %v, want o/r on release", req)
				}
			}},
		{name: "gh-queue-unsubscribe sends the queue", args: []string{"gh-queue-unsubscribe", "here", "-queue", "o/r@main"},
			check: func(t *testing.T, f *cliFixture) {
				req := only[*agentreplv1.UnsubscribeGithubMergeQueueRequest](t, f)
				if req.GetMergeQueue().GetRepository().GetOwner() != "o" || req.GetMergeQueue().GetRepository().GetRepo() != "r" || req.GetMergeQueue().GetBranch() != "main" {
					t.Errorf("request = %v, want o/r@main", req)
				}
			}},
		{name: "gh-queue-subscriptions lists", args: []string{"gh-queue-subscriptions", "here"},
			check: func(t *testing.T, f *cliFixture) {
				if req := only[*agentreplv1.ListGithubMergeQueueSubscriptionsRequest](t, f); req.GetSubscriber() == nil {
					t.Errorf("request = %v, want the subscriber", req)
				}
			}},
		{name: "pr-comment replies in a thread", args: []string{"pr-comment", "here", "-pr", "o/r#7", "-body", "done", "-in-reply-to", "5"},
			check: func(t *testing.T, f *cliFixture) {
				req := only[*agentreplv1.CommentOnPullRequestRequest](t, f)
				if req.GetReply().GetInReplyToCommentId() != 5 || req.GetReply().GetMarkdown() != "done" || !proto.Equal(req.GetPullRequest(), ref) {
					t.Errorf("request = %v, want a reply to 5", req)
				}
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(workspaceNoun, tt.args...)

			// Assert
			if code != exitSuccess {
				t.Fatalf("exit %d, stderr %q", code, f.errOut.String())
			}
			tt.check(t, f)
		})
	}
}

func TestPullRequestEventSpellingsAreTheStoresEvents(t *testing.T) {
	// Assert
	members := wsm.AllPullRequestEvents().Members()
	if len(pullRequestEvents) != len(members) || allPullRequestEvents != wsm.AllPullRequestEvents().String() {
		t.Fatalf("spellings %v and default %q; want one per store event, %v", pullRequestEvents, allPullRequestEvents, members)
	}
	for _, e := range members {
		if pullRequestEvents[e.String()] != prwatch.EventProto(e) {
			t.Fatalf("%s spells %v, want %v", e, pullRequestEvents[e.String()], prwatch.EventProto(e))
		}
	}
}

func TestPullRequestVerbsRefuseAMalformedCall(t *testing.T) {
	tests := []struct {
		name string
		args []string
	}{
		{name: "pr-subscribe naming no pull request", args: []string{"pr-subscribe", "here"}},
		{name: "pr-subscribe naming an unknown event", args: []string{"pr-subscribe", "here", "-pr", "#7", "-events", "pushes"}},
		{name: "gh-queue-subscribe naming a malformed repository", args: []string{"gh-queue-subscribe", "here", "-repository", "o"}},
		{name: "gh-queue-unsubscribe naming no branch", args: []string{"gh-queue-unsubscribe", "here", "-queue", "o/r"}},
		{name: "pr-unsubscribe naming #N", args: []string{"pr-unsubscribe", "here", "-pr", "#7"}},
		{name: "pr-comment with no body", args: []string{"pr-comment", "here", "-pr", "o/r#7"}},
		{name: "pr-comment with both bodies", args: []string{"pr-comment", "here", "-pr", "o/r#7", "-body", "x", "-body-file", "/f"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(workspaceNoun, tt.args...)

			// Assert
			if code == exitSuccess {
				t.Fatalf("exit %d, want a usage failure", code)
			}
		})
	}
}

func TestNotifyPostsTheBannerForTheNamedWorkspace(t *testing.T) {
	tests := []struct {
		name   string
		target string
		wantID string
	}{
		{name: "another workspace, by name", target: "peer", wantID: "ws-peer"},
		{name: "the calling workspace", target: "here", wantID: "ws-caller"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(workspaceNoun, "notify", tt.target, "The build is green")

			// Assert
			req := only[*agentreplv1.NotifyWorkspaceRequest](t, f)
			if code != exitSuccess || req.GetWorkspace().GetId() != tt.wantID || req.GetMessage() != "The build is green" {
				t.Fatalf("exit %d, request %v; want %s notified with the text", code, req, tt.wantID)
			}
		})
	}
}

func TestNotifyRefusesAMalformedCall(t *testing.T) {
	tests := []struct {
		name string
		args []string
	}{
		{name: "no text", args: []string{"notify", "peer"}},
		{name: "blank text", args: []string{"notify", "peer", "  "}},
		{name: "two texts", args: []string{"notify", "peer", "one", "two"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(workspaceNoun, tt.args...)

			// Assert
			if code == exitSuccess || len(f.client.requests) != 0 {
				t.Fatalf("exit %d, sent %v; want a usage failure and nothing sent", code, f.client.requests)
			}
		})
	}
}

func TestWorkspaceVerbsSayTheMissingGeometrysRefusalInPlainEnglish(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(c *cliClient)
		args    []string
		want    string
	}{
		{name: "finished", arrange: func(c *cliClient) {
			c.finish = &agentreplv1.FinishWorkspaceResponse{Result: &agentreplv1.FinishWorkspaceResponse_Error{Error: &agentreplv1.FinishWorkspaceError{
				Cause: &agentreplv1.FinishWorkspaceError_NoLayoutFacts{NoLayoutFacts: &agentreplv1.FinishWorkspaceNoLayoutFacts{}}}}}
		}, args: []string{"finished", "peer", "-placement", "back"},
			want: "peer records no merge geometry, so nothing says where it lands; record it by restating where the workspace lands: `agent-repld workspace set peer -lands-into main`"},
		{name: "set -one-shot", arrange: func(c *cliClient) {
			c.oneShot = &agentreplv1.SetWorkspaceOneShotResponse{Result: &agentreplv1.SetWorkspaceOneShotResponse_Error{Error: &agentreplv1.SetWorkspaceOneShotError{
				Cause: &agentreplv1.SetWorkspaceOneShotError_NoCreationJob{NoCreationJob: &agentreplv1.SetWorkspaceOneShotNoCreationJob{}}}}}
		}, args: []string{"set", "peer", "-one-shot"},
			want: "peer records no creation job to mark; record it by restating where the workspace lands"},
		{name: "set -top-level", arrange: func(c *cliClient) {
			c.retargeted = &agentreplv1.SetWorkspaceParentResponse{Result: &agentreplv1.SetWorkspaceParentResponse_Error{Error: &agentreplv1.SetWorkspaceParentError{
				Cause: &agentreplv1.SetWorkspaceParentError_NoMergeLayout{NoMergeLayout: &agentreplv1.SetWorkspaceParentNoMergeLayout{}}}}}
		}, args: []string{"set", "peer", "-top-level"},
			want: "peer records no merge geometry to keep; restate where it lands with -lands-into main"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()
			tt.arrange(f.client)

			// Act
			code := f.run(workspaceNoun, tt.args...)

			// Assert
			if code != exitRefused || !strings.Contains(f.errOut.String(), tt.want) {
				t.Fatalf("exit %d, stderr %q; want refused saying %q", code, f.errOut.String(), tt.want)
			}
		})
	}
}

func TestPlaceMarksEveryArrangementItSendsAsTheAgents(t *testing.T) {
	tests := []struct {
		name string
		args []string
	}{
		{name: "into an existing subsection", args: []string{"-subsection", "later"}},
		{name: "into a new subsection", args: []string{"-new-subsection", "auth work"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()
			f.client.arrangement = subsectionsArrangement()

			// Act
			code := f.run(workspaceNoun, append([]string{"place", "here", "peer"}, tt.args...)...)

			// Assert
			if code != exitSuccess {
				t.Fatalf("exit %d, stderr %q; want success", code, f.errOut.String())
			}
			if !only[*agentreplv1.ArrangeSidebarRequest](t, f).GetByAgent() {
				t.Fatal("the request did not say it came from an agent, so the daemon cannot refuse a main workspace")
			}
		})
	}
}

func boolPtr(b bool) *bool { return &b }
