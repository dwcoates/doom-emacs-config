package server

import (
	"context"
	"fmt"
	"sync"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/publish"
	"agent-repld/internal/replyquote"
	"agent-repld/internal/workspace"
)

const opResponseQuestion = "daemon.server.response_question"

// A QUESTION ABOUT A FINAL RESPONSE. The daemon is the only holder of each
// workspace's question (endpoint_ask_about_response.proto): the webapp's
// question-mark control toggles it (AskAboutResponse), the root feed's watch
// draws it (frontend.v1.FeedResponseQuestion), Emacs edits its composer on
// each transition (HostWorkspaceResponseQuestion), and the prompt the user
// sends while it stands is taken by it: forked into a new child workspace
// whose first prompt quotes the response (submitQuestion).

// responseQuestions is every workspace's held question: the asked final
// response's row, ABSENT for none.
type responseQuestions struct {
	mu   sync.Mutex
	held map[ids.WorkspaceID]*frontendv1.FeedId
}

// questionToggle is which transition a toggle made.
type questionToggle int

const (
	// questionAsked: none stood, and one about the row stands now.
	questionAsked questionToggle = iota
	// questionMoved: one about another row stood, and it moved to the row.
	questionMoved
	// questionWithdrawn: one about the row stood, and none stands now.
	questionWithdrawn
)

// toggle applies a click on ROW to WS's question: the asked row withdraws it,
// any other row asks about that one. Answers the transition made.
func (q *responseQuestions) toggle(ws ids.WorkspaceID, row *frontendv1.FeedId) questionToggle {
	q.mu.Lock()
	defer q.mu.Unlock()
	held, ok := q.held[ws]
	switch {
	case ok && held.GetValue() == row.GetValue():
		delete(q.held, ws)
		return questionWithdrawn
	case ok:
		q.held[ws] = row
		return questionMoved
	default:
		q.held[ws] = row
		return questionAsked
	}
}

// take answers WS's question and ends it, false when none stands.
func (q *responseQuestions) take(ws ids.WorkspaceID) (*frontendv1.FeedId, bool) {
	q.mu.Lock()
	defer q.mu.Unlock()
	held, ok := q.held[ws]
	delete(q.held, ws)
	return held, ok
}

// AskAboutResponse toggles the workspace's question about one final response.
// See endpoint_ask_about_response.proto.
func (s *server) AskAboutResponse(
	ctx context.Context,
	req *connect.Request[agentreplv1.AskAboutResponseRequest],
) (*connect.Response[agentreplv1.AskAboutResponseResponse], error) {
	const rpc = "AskAboutResponse"
	if err := validateAskAboutResponseRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.AskAboutResponseResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	ws := subject.Record.ID
	row := req.Msg.GetRow()
	if _, ok := s.deps.Feed.RevisableFinalResponse(ws, row); !ok {
		return answer(resp, s.refuse(subject.Log, rpc, resp, refusal{
			Arm:    "not_a_final_response",
			Reason: fmt.Sprintf("the row %q is not a settled final response of this workspace's root feed", row.GetValue()),
		}))
	}
	change := s.questions.toggle(ws, row)
	success := &agentreplv1.AskAboutResponseSuccess{}
	switch change {
	case questionAsked, questionMoved:
		success.State = &agentreplv1.AskAboutResponseSuccess_Asking{Asking: &agentreplv1.AskAboutResponseAsking{Row: row}}
		s.publishQuestion(ws, row)
	case questionWithdrawn:
		success.State = &agentreplv1.AskAboutResponseSuccess_Withdrawn{Withdrawn: &agentreplv1.AskAboutResponseWithdrawn{}}
		s.publishQuestion(ws, nil)
	}
	s.questionChanged(subject.Log, ws, hostQuestionChangeOf(change))
	subject.Log.Info(opResponseQuestion, "the question about a final response was toggled", dlog.Context{
		"row": row.GetValue(), "change": hostQuestionChangeName(hostQuestionChangeOf(change)),
	})
	resp.Result = &agentreplv1.AskAboutResponseResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}

// validateAskAboutResponseRequest requires the workspace and the row.
func validateAskAboutResponseRequest(req *agentreplv1.AskAboutResponseRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	return validateFeedID("row", req.GetRow())
}

// hostQuestionChange is one HostWorkspaceResponseQuestion arm.
type hostQuestionChange int

const (
	hostQuestionAsked hostQuestionChange = iota
	hostQuestionMoved
	hostQuestionWithdrawn
	hostQuestionTaken
)

// hostQuestionChangeOf is the host arm a toggle's transition pushes.
func hostQuestionChangeOf(t questionToggle) hostQuestionChange {
	switch t {
	case questionAsked:
		return hostQuestionAsked
	case questionMoved:
		return hostQuestionMoved
	case questionWithdrawn:
		return hostQuestionWithdrawn
	}
	panic(fmt.Sprintf("hostQuestionChangeOf: questionToggle %d is no transition", t))
}

// hostQuestionChangeName is CHANGE's arm name, for the log.
func hostQuestionChangeName(change hostQuestionChange) string {
	return [...]string{"asked", "moved", "withdrawn", "taken"}[change]
}

// hostQuestionPush is CHANGE as the host watch's push.
func hostQuestionPush(change hostQuestionChange) *agentreplv1.HostWorkspaceResponseQuestion {
	push := &agentreplv1.HostWorkspaceResponseQuestion{}
	switch change {
	case hostQuestionAsked:
		push.Change = &agentreplv1.HostWorkspaceResponseQuestion_Asked{Asked: &agentreplv1.HostWorkspaceResponseQuestionAsked{}}
	case hostQuestionMoved:
		push.Change = &agentreplv1.HostWorkspaceResponseQuestion_Moved{Moved: &agentreplv1.HostWorkspaceResponseQuestionMoved{}}
	case hostQuestionWithdrawn:
		push.Change = &agentreplv1.HostWorkspaceResponseQuestion_Withdrawn{Withdrawn: &agentreplv1.HostWorkspaceResponseQuestionWithdrawn{}}
	case hostQuestionTaken:
		push.Change = &agentreplv1.HostWorkspaceResponseQuestion_Taken{Taken: &agentreplv1.HostWorkspaceResponseQuestionTaken{}}
	}
	return push
}

// questionChanged pushes CHANGE onto the workspace's host EVENT stream, never
// a topic: each arm asks Emacs for an edit of the composer made now, which a
// reconnect must not repeat.
func (s *server) questionChanged(log dlog.Logger, ws ids.WorkspaceID, change hostQuestionChange) {
	log.Debug(opResponseQuestion, "told the host how the question changed", dlog.Context{"change": hostQuestionChangeName(change)})
	s.hostEventStream(ws).Emit(&agentreplv1.WatchHostWorkspaceResponse{
		Push: &agentreplv1.WatchHostWorkspaceResponse_ResponseQuestion{ResponseQuestion: hostQuestionPush(change)},
	})
}

// feedQuestion is the root feed's FeedResponseQuestion for ROW, none for nil.
func feedQuestion(row *frontendv1.FeedId) *frontendv1.FeedResponseQuestion {
	if row == nil {
		return &frontendv1.FeedResponseQuestion{State: &frontendv1.FeedResponseQuestion_None{None: &frontendv1.FeedResponseQuestionNone{}}}
	}
	return &frontendv1.FeedResponseQuestion{State: &frontendv1.FeedResponseQuestion_Asking{Asking: &frontendv1.FeedResponseQuestionAsking{Row: row}}}
}

// publishQuestion publishes WS's question, about ROW or none for nil, on its
// root feed's watch.
func (s *server) publishQuestion(ws ids.WorkspaceID, row *frontendv1.FeedId) {
	s.responseQuestionTopic(ws).Publish(feedQuestion(row))
}

// responseQuestionTopic answers a workspace's response-question topic,
// creating it on first use. It replays its latest value, so a root feed watch
// accepted while a question stands draws it at once.
func (s *server) responseQuestionTopic(ws ids.WorkspaceID) *publish.Topic[*frontendv1.FeedResponseQuestion] {
	return perWorkspace(s, s.responseQuestionTopics, ws)
}

// submitQuestion is SubmitPrompt's body for a user prompt sent while a
// question about ROW stood: the question has been taken, so it ends here
// whatever happens, and the prompt is forked into a new child workspace whose
// first prompt quotes the response. No turn is minted in this workspace.
//
// THE CHILD IS CREATED IN THE BACKGROUND under the prompt's idempotency key as
// its op id (SubmitPromptQuestionForked), exactly as an op-carrying
// CreateWorkspace is: the client that sent the prompt registered for that op
// before sending, so the create's stages and outcome reach it.
func (s *server) submitQuestion(
	ctx context.Context,
	subject resolved,
	req *agentreplv1.SubmitPromptRequest,
	resp *agentreplv1.SubmitPromptResponse,
	row *frontendv1.FeedId,
) (*connect.Response[agentreplv1.SubmitPromptResponse], error) {
	const rpc = "SubmitPrompt"
	ws := subject.Record.ID
	s.publishQuestion(ws, nil)
	s.questionChanged(subject.Log, ws, hostQuestionTaken)
	quoted, ok := s.deps.Feed.BubbleText(ws, row)
	if !ok {
		// THE ASKED RESPONSE IS GONE (a rollback took it): the prompt is
		// refused rather than asked unquoted, which would answer a question
		// about nothing the person can see.
		return answer(resp, s.refuse(subject.Log, rpc, resp, submitRefusal(s.fill(refusal{
			Arm:      "reference_response_unresolvable",
			Reason:   fmt.Sprintf("the asked row %q is no longer a landed bubble of this workspace's root feed", row.GetValue()),
			NotFound: true,
		}))))
	}
	repository, err, known := s.repositoryFor(ctx, &workspacev1.RepositoryRef{Id: string(subject.Record.Repo)})
	if err != nil {
		return nil, fail(subject.Log, rpc, err)
	}
	if !known {
		subject.Log.Error(opResponseQuestion, "the asking workspace's repository is not registered", dlog.Context{"repository": string(subject.Record.Repo)})
		return nil, fail(subject.Log, rpc, fmt.Errorf("the asking workspace's repository %q is not registered", subject.Record.Repo))
	}
	admitted, release, err := s.deps.Creates.Admit(ctx)
	if err != nil {
		return answer(resp, s.refuseUnadmitted(rpc, resp, err))
	}
	opID := req.GetIdempotencyKey()
	spec := workspace.CreateSpec{
		RepoDir:       repository.Dir,
		ForkFrom:      &ws,
		Parent:        &ws,
		InitialPrompt: replyquote.QuestionText(quoted.Markdown, s.saidText(subject.Log, "said", req.GetSaid())),
	}
	bgCtx, cancel := s.backgroundContext(admitted)
	ticket := s.startBackgroundCreate(bgCtx, cancel, release, spec, opID)
	subject.Log.Info(opResponseQuestion, "the prompt asks about a final response; forking it into a child workspace", dlog.Context{
		"row": row.GetValue(), "op_id": opID, "create_id": string(ticket.ID()),
	})
	resp.Result = &agentreplv1.SubmitPromptResponse_Success{Success: &agentreplv1.SubmitPromptSuccess{
		Outcome: &agentreplv1.SubmitPromptSuccess_QuestionForked{QuestionForked: &agentreplv1.SubmitPromptQuestionForked{Row: row, OpId: opID}},
	}}
	return connect.NewResponse(resp), nil
}
