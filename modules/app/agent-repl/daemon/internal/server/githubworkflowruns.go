package server

import (
	"context"
	"errors"
	"fmt"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/prwatch"
	"agent-repld/internal/wsm"
)

// The agent API's GitHub workflow-run follows: a workspace's subscription to
// one GitHub Actions run, told once when it finishes, and the list of them.

// opGithubWorkflowRuns is the operation every record of these rpcs carries.
const opGithubWorkflowRuns = "daemon.server.github_workflow_runs"

// SubscribeGithubWorkflowRun subscribes one workspace's agent to a workflow
// run's completion, answering the run as it was read. A run already finished
// owes its notice at once and holds nothing.
func (s *server) SubscribeGithubWorkflowRun(
	ctx context.Context,
	req *connect.Request[agentreplv1.SubscribeGithubWorkflowRunRequest],
) (*connect.Response[agentreplv1.SubscribeGithubWorkflowRunResponse], error) {
	const rpc = "SubscribeGithubWorkflowRun"
	if err := validateSubscribeGithubWorkflowRunRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.SubscribeGithubWorkflowRunResponse{}
	subscriber, refused, done := s.resolveRegisteredNaming(ctx, rpc, resp, req.Msg.GetSubscriber(), subscriberRenames)
	if done {
		return answer(resp, refused)
	}
	log := subscriber.Log
	refuse := func(cause *agentreplv1.SubscribeGithubWorkflowRunError, reason string) (*connect.Response[agentreplv1.SubscribeGithubWorkflowRunResponse], error) {
		log.Info(opGithubWorkflowRuns, "refused a GitHub workflow-run subscription", dlog.Context{"rpc": rpc, "reason": reason})
		resp.Result = &agentreplv1.SubscribeGithubWorkflowRunResponse_Error{Error: cause}
		return connect.NewResponse(resp), nil
	}
	closed := func(reason string) (*connect.Response[agentreplv1.SubscribeGithubWorkflowRunResponse], error) {
		return refuse(&agentreplv1.SubscribeGithubWorkflowRunError{Cause: &agentreplv1.SubscribeGithubWorkflowRunError_SubscriberClosed{SubscriberClosed: &agentreplv1.SubscribeGithubWorkflowRunSubscriberClosed{}}}, reason)
	}
	unknown := func() (*connect.Response[agentreplv1.SubscribeGithubWorkflowRunResponse], error) {
		return refuse(&agentreplv1.SubscribeGithubWorkflowRunError{Cause: &agentreplv1.SubscribeGithubWorkflowRunError_UnknownSubscriber{UnknownSubscriber: &agentreplv1.SubscribeGithubWorkflowRunUnknownSubscriber{}}},
			"the workspace was forgotten while the subscription was taken")
	}
	if subscriber.Record.Closed {
		return closed("a closed workspace runs no agent to tell")
	}
	var key wsm.GithubWorkflowRunKey
	switch run := req.Msg.GetRun().(type) {
	case *agentreplv1.SubscribeGithubWorkflowRunRequest_Url:
		parsed, err := prwatch.ParseRunURL(run.Url)
		var unparsable *prwatch.UnparsableError
		if errors.As(err, &unparsable) {
			return refuse(&agentreplv1.SubscribeGithubWorkflowRunError{Cause: &agentreplv1.SubscribeGithubWorkflowRunError_Unparsable{Unparsable: &agentreplv1.SubscribeGithubWorkflowRunUnparsable{
				Text: unparsable.Text, Reason: unparsable.Reason}}}, unparsable.Error())
		}
		key = parsed
	case *agentreplv1.SubscribeGithubWorkflowRunRequest_Named:
		key = prwatch.RunKeyOf(run.Named)
	case *agentreplv1.SubscribeGithubWorkflowRunRequest_InSubscribersRepository:
		owner, repo, remote, ok, err := prwatch.OriginRepository(ctx, s.deps.Remotes, subscriber.Record.Dir)
		if err != nil {
			return nil, fail(log, rpc, err)
		}
		if !ok {
			return refuse(&agentreplv1.SubscribeGithubWorkflowRunError{Cause: &agentreplv1.SubscribeGithubWorkflowRunError_NotOnGithub{NotOnGithub: &agentreplv1.SubscribeGithubWorkflowRunNotOnGithub{Origin: remote}}},
				fmt.Sprintf("the repository's origin %q is not on github.com", remote))
		}
		key = wsm.GithubWorkflowRunKey{Owner: owner, Repo: repo, RunID: run.InSubscribersRepository.GetRunId()}
	}
	read, err := s.deps.PullRequests.ReadRun(ctx, key)
	if err != nil {
		return refuse(&agentreplv1.SubscribeGithubWorkflowRunError{Cause: &agentreplv1.SubscribeGithubWorkflowRunError_Unreadable{Unreadable: &agentreplv1.SubscribeGithubWorkflowRunUnreadable{
			Cause: prwatch.FailureProto(err)}}}, err.Error())
	}
	success := &agentreplv1.SubscribeGithubWorkflowRunSuccess{Run: prwatch.RunRefProto(key), Status: read.Proto()}
	fields := dlog.Context{"subscriber": string(subscriber.Record.ID), "workflow_run": key.String(), "status": read.Status, "conclusion": read.Conclusion}
	if read.Completed() {
		// A RUN ALREADY FINISHED IS TOLD AT ONCE: its notice is owed (ending
		// any subscription that stands) and the watcher's own loop is woken to
		// deliver it.
		_, err := s.deps.DB.OweGithubWorkflowRunCompletion(ctx, prwatch.RunCompletionOf(subscriber.Record.ID, key, read, false))
		switch {
		case errors.Is(err, wsm.ErrSubscriberClosed):
			return closed("the workspace closed while the subscription was taken")
		case errors.Is(err, wsm.ErrNotFound):
			return unknown()
		case err != nil:
			return nil, fail(log, rpc, fmt.Errorf("owe %s the completion of %s: %w", subscriber.Record.ID, key, err))
		}
		s.deps.PullRequests.Wake()
		log.Info(opGithubWorkflowRuns, "the run had already finished; its notice is owed at once and nothing was subscribed", fields)
		success.Taken = &agentreplv1.SubscribeGithubWorkflowRunSuccess_AlreadyCompleted{AlreadyCompleted: &agentreplv1.SubscribeGithubWorkflowRunSuccessAlreadyCompleted{}}
		resp.Result = &agentreplv1.SubscribeGithubWorkflowRunResponse_Success{Success: success}
		return connect.NewResponse(resp), nil
	}
	held, err := s.deps.DB.SubscribeGithubWorkflowRun(ctx, wsm.GithubWorkflowRunSubscription{Subscriber: subscriber.Record.ID, Run: key, SubscribedAt: read.ReadAt, LastRead: read.ReadAt})
	switch {
	case errors.Is(err, wsm.ErrSubscriberClosed):
		return closed("the workspace closed while the subscription was taken")
	case errors.Is(err, wsm.ErrNotFound):
		return unknown()
	case err != nil:
		return nil, fail(log, rpc, fmt.Errorf("record the subscription of %s to %s: %w", subscriber.Record.ID, key, err))
	}
	stored, err := s.runSubscriptionOf(ctx, subscriber.Record.ID, subscriber.Record.Dir, key)
	if err != nil {
		return nil, fail(log, rpc, err)
	}
	if held {
		success.Taken = &agentreplv1.SubscribeGithubWorkflowRunSuccess_AlreadySubscribed{AlreadySubscribed: &agentreplv1.SubscribeGithubWorkflowRunSuccessAlreadySubscribed{Subscription: stored}}
	} else {
		success.Taken = &agentreplv1.SubscribeGithubWorkflowRunSuccess_Subscribed{Subscribed: &agentreplv1.SubscribeGithubWorkflowRunSuccessSubscribed{Subscription: stored}}
	}
	fields["already"] = held
	log.Info(opGithubWorkflowRuns, "subscribed the workspace's agent to a GitHub workflow run's completion", fields)
	resp.Result = &agentreplv1.SubscribeGithubWorkflowRunResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}

// UnsubscribeGithubWorkflowRun ends one workspace's subscription to a
// workflow run. Holding none is an answer, not a refusal.
func (s *server) UnsubscribeGithubWorkflowRun(
	ctx context.Context,
	req *connect.Request[agentreplv1.UnsubscribeGithubWorkflowRunRequest],
) (*connect.Response[agentreplv1.UnsubscribeGithubWorkflowRunResponse], error) {
	const rpc = "UnsubscribeGithubWorkflowRun"
	if err := validateWorkspaceRef("subscriber", req.Msg.GetSubscriber()); err != nil {
		return nil, err
	}
	if err := validateGithubWorkflowRunRef("run", req.Msg.GetRun()); err != nil {
		return nil, err
	}
	resp := &agentreplv1.UnsubscribeGithubWorkflowRunResponse{}
	subscriber, refused, done := s.resolveRegisteredNaming(ctx, rpc, resp, req.Msg.GetSubscriber(), subscriberRenames)
	if done {
		return answer(resp, refused)
	}
	key := prwatch.RunKeyOf(req.Msg.GetRun())
	ended, err := s.deps.DB.UnsubscribeGithubWorkflowRun(ctx, subscriber.Record.ID, key)
	if err != nil {
		return nil, fail(subscriber.Log, rpc, fmt.Errorf("end the subscription of %s to %s: %w", subscriber.Record.ID, key, err))
	}
	success := &agentreplv1.UnsubscribeGithubWorkflowRunSuccess{Ended: &agentreplv1.UnsubscribeGithubWorkflowRunSuccess_Unsubscribed{Unsubscribed: &agentreplv1.UnsubscribeGithubWorkflowRunSuccessUnsubscribed{}}}
	if !ended {
		success.Ended = &agentreplv1.UnsubscribeGithubWorkflowRunSuccess_NotSubscribed{NotSubscribed: &agentreplv1.UnsubscribeGithubWorkflowRunSuccessNotSubscribed{}}
	}
	subscriber.Log.Info(opGithubWorkflowRuns, "ended a GitHub workflow-run subscription", dlog.Context{"subscriber": string(subscriber.Record.ID), "workflow_run": key.String(), "ended": ended})
	resp.Result = &agentreplv1.UnsubscribeGithubWorkflowRunResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}

// ListGithubWorkflowRunSubscriptions answers the workflow runs one workspace
// waits on.
func (s *server) ListGithubWorkflowRunSubscriptions(
	ctx context.Context,
	req *connect.Request[agentreplv1.ListGithubWorkflowRunSubscriptionsRequest],
) (*connect.Response[agentreplv1.ListGithubWorkflowRunSubscriptionsResponse], error) {
	const rpc = "ListGithubWorkflowRunSubscriptions"
	if err := validateWorkspaceRef("subscriber", req.Msg.GetSubscriber()); err != nil {
		return nil, err
	}
	resp := &agentreplv1.ListGithubWorkflowRunSubscriptionsResponse{}
	subscriber, refused, done := s.resolveRegisteredNaming(ctx, rpc, resp, req.Msg.GetSubscriber(), subscriberRenames)
	if done {
		return answer(resp, refused)
	}
	subs, err := s.deps.DB.WorkspaceGithubWorkflowRunSubscriptions(ctx, subscriber.Record.ID)
	if err != nil {
		return nil, fail(subscriber.Log, rpc, err)
	}
	success := &agentreplv1.ListGithubWorkflowRunSubscriptionsSuccess{}
	for _, sub := range subs {
		success.Subscriptions = append(success.Subscriptions, withRunDir(prwatch.RunSubscriptionProto(sub), subscriber.Record.Dir))
	}
	resp.Result = &agentreplv1.ListGithubWorkflowRunSubscriptionsResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}

// runSubscriptionOf reads back the subscription WS holds to KEY, as the
// contract spells it.
func (s *server) runSubscriptionOf(ctx context.Context, ws ids.WorkspaceID, dir string, key wsm.GithubWorkflowRunKey) (*agentreplv1.GithubWorkflowRunSubscription, error) {
	subs, err := s.deps.DB.WorkspaceGithubWorkflowRunSubscriptions(ctx, ws)
	if err != nil {
		return nil, err
	}
	for _, sub := range subs {
		if sub.Run == key {
			return withRunDir(prwatch.RunSubscriptionProto(sub), dir), nil
		}
	}
	return nil, fmt.Errorf("the subscription of %s to %s was recorded and is not there to read back", ws, key)
}

// withRunDir fills the subscriber's directory, which the watcher does not
// hold.
func withRunDir(sub *agentreplv1.GithubWorkflowRunSubscription, dir string) *agentreplv1.GithubWorkflowRunSubscription {
	sub.Subscriber = &workspacev1.WorkspaceRef{Id: sub.GetSubscriber().GetId(), Dir: dir}
	return sub
}

// validateSubscribeGithubWorkflowRunRequest is
// SubscribeGithubWorkflowRunRequest's base function.
func validateSubscribeGithubWorkflowRunRequest(req *agentreplv1.SubscribeGithubWorkflowRunRequest) *connect.Error {
	if err := validateWorkspaceRef("subscriber", req.GetSubscriber()); err != nil {
		return err
	}
	switch run := req.GetRun().(type) {
	case *agentreplv1.SubscribeGithubWorkflowRunRequest_Url:
		if run.Url == "" {
			return invalid("url", "a run's address is not empty")
		}
	case *agentreplv1.SubscribeGithubWorkflowRunRequest_Named:
		return validateGithubWorkflowRunRef("named", run.Named)
	case *agentreplv1.SubscribeGithubWorkflowRunRequest_InSubscribersRepository:
		if run.InSubscribersRepository.GetRunId() == 0 {
			return invalid("in_subscribers_repository", "a run names its id")
		}
	default:
		return invalid("run", "a subscription names its run")
	}
	return nil
}

// validateGithubWorkflowRunRef refuses a ref missing any part.
func validateGithubWorkflowRunRef(field string, ref *agentreplv1.GithubWorkflowRunRef) *connect.Error {
	if ref.GetRepository().GetOwner() == "" || ref.GetRepository().GetRepo() == "" || ref.GetRunId() == 0 {
		return invalid(field, "a workflow run names its owner, repository and run id")
	}
	return nil
}
