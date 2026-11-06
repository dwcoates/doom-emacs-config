package server

import (
	"context"
	"errors"
	"fmt"
	"strings"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/prwatch"
	"agent-repld/internal/wsm"
)

// The agent API's GitHub merge-queue follows: a workspace's subscription to
// a repository branch's GITHUB merge queue, and the list of them. Agent-repl's
// own landing queue is SubscribeMergeQueue's (mergesubscriptions.go).

// opGithubMergeQueues is the operation every record of these rpcs carries.
const opGithubMergeQueues = "daemon.server.github_merge_queues"

// SubscribeGithubMergeQueue subscribes one workspace's agent to a
// repository branch's GitHub merge queue, answering the queue as the
// subscription is anchored on it.
func (s *server) SubscribeGithubMergeQueue(
	ctx context.Context,
	req *connect.Request[agentreplv1.SubscribeGithubMergeQueueRequest],
) (*connect.Response[agentreplv1.SubscribeGithubMergeQueueResponse], error) {
	const rpc = "SubscribeGithubMergeQueue"
	if err := validateSubscribeGithubMergeQueueRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.SubscribeGithubMergeQueueResponse{}
	subscriber, refused, done := s.resolveRegisteredNaming(ctx, rpc, resp, req.Msg.GetSubscriber(), subscriberRenames)
	if done {
		return answer(resp, refused)
	}
	log := subscriber.Log
	refuse := func(cause *agentreplv1.SubscribeGithubMergeQueueError, reason string) (*connect.Response[agentreplv1.SubscribeGithubMergeQueueResponse], error) {
		log.Info(opGithubMergeQueues, "refused a GitHub merge-queue subscription", dlog.Context{"rpc": rpc, "reason": reason})
		resp.Result = &agentreplv1.SubscribeGithubMergeQueueResponse_Error{Error: cause}
		return connect.NewResponse(resp), nil
	}
	closed := func(reason string) (*connect.Response[agentreplv1.SubscribeGithubMergeQueueResponse], error) {
		return refuse(&agentreplv1.SubscribeGithubMergeQueueError{Cause: &agentreplv1.SubscribeGithubMergeQueueError_SubscriberClosed{SubscriberClosed: &agentreplv1.SubscribeGithubMergeQueueSubscriberClosed{}}}, reason)
	}
	if subscriber.Record.Closed {
		return closed("a closed workspace runs no agent to tell")
	}
	owner, repo := req.Msg.GetNamed().GetOwner(), req.Msg.GetNamed().GetRepo()
	if req.Msg.GetSubscribers() != nil {
		remote, err := s.deps.Remotes.RemoteURL(ctx, subscriber.Record.Dir, "origin")
		if err != nil {
			return nil, fail(log, rpc, fmt.Errorf("read the origin remote of %s: %w", subscriber.Record.Dir, err))
		}
		var ok bool
		if owner, repo, ok = prwatch.RepositoryOfRemote(remote); !ok {
			return refuse(&agentreplv1.SubscribeGithubMergeQueueError{Cause: &agentreplv1.SubscribeGithubMergeQueueError_NotOnGithub{NotOnGithub: &agentreplv1.SubscribeGithubMergeQueueNotOnGithub{Origin: remote}}},
				fmt.Sprintf("the repository's origin %q is not on github.com", remote))
		}
	}
	sub, status, key, err := s.deps.PullRequests.AnchorQueue(ctx, subscriber.Record.ID, owner, repo, req.Msg.GetBranch())
	switch {
	case errors.Is(err, prwatch.ErrNoMergeQueue):
		return refuse(&agentreplv1.SubscribeGithubMergeQueueError{Cause: &agentreplv1.SubscribeGithubMergeQueueError_NoMergeQueue{NoMergeQueue: &agentreplv1.SubscribeGithubMergeQueueNoMergeQueue{
			MergeQueue: prwatch.QueueRefProto(key)}}}, "GitHub runs no merge queue for "+key.String())
	case err != nil:
		return refuse(&agentreplv1.SubscribeGithubMergeQueueError{Cause: &agentreplv1.SubscribeGithubMergeQueueError_Unreadable{Unreadable: &agentreplv1.SubscribeGithubMergeQueueUnreadable{
			Cause: prwatch.FailureProto(err)}}}, err.Error())
	}
	held, err := s.deps.DB.SubscribeGithubMergeQueue(ctx, sub)
	switch {
	case errors.Is(err, wsm.ErrSubscriberClosed):
		return closed("the workspace closed while the subscription was taken")
	case errors.Is(err, wsm.ErrNotFound):
		return refuse(&agentreplv1.SubscribeGithubMergeQueueError{Cause: &agentreplv1.SubscribeGithubMergeQueueError_UnknownSubscriber{UnknownSubscriber: &agentreplv1.SubscribeGithubMergeQueueUnknownSubscriber{}}},
			"the workspace was forgotten while the subscription was taken")
	case err != nil:
		return nil, fail(log, rpc, fmt.Errorf("record the subscription of %s to %s: %w", subscriber.Record.ID, key, err))
	}
	stored, err := s.queueSubscriptionOf(ctx, subscriber.Record.ID, subscriber.Record.Dir, key)
	if err != nil {
		return nil, fail(log, rpc, err)
	}
	success := &agentreplv1.SubscribeGithubMergeQueueSuccess{MergeQueue: prwatch.QueueRefProto(key), Status: status}
	if held {
		success.Taken = &agentreplv1.SubscribeGithubMergeQueueSuccess_AlreadySubscribed{AlreadySubscribed: &agentreplv1.SubscribeGithubMergeQueueSuccessAlreadySubscribed{Subscription: stored}}
	} else {
		success.Taken = &agentreplv1.SubscribeGithubMergeQueueSuccess_Subscribed{Subscribed: &agentreplv1.SubscribeGithubMergeQueueSuccessSubscribed{Subscription: stored}}
	}
	log.Info(opGithubMergeQueues, "subscribed the workspace's agent to a GitHub merge queue", dlog.Context{"subscriber": string(subscriber.Record.ID), "merge_queue": key.String(), "already": held})
	resp.Result = &agentreplv1.SubscribeGithubMergeQueueResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}

// UnsubscribeGithubMergeQueue ends one workspace's subscription to a GitHub
// merge queue. Holding none is an answer, not a refusal.
func (s *server) UnsubscribeGithubMergeQueue(
	ctx context.Context,
	req *connect.Request[agentreplv1.UnsubscribeGithubMergeQueueRequest],
) (*connect.Response[agentreplv1.UnsubscribeGithubMergeQueueResponse], error) {
	const rpc = "UnsubscribeGithubMergeQueue"
	if err := validateWorkspaceRef("subscriber", req.Msg.GetSubscriber()); err != nil {
		return nil, err
	}
	if err := validateGithubMergeQueueRef("merge_queue", req.Msg.GetMergeQueue()); err != nil {
		return nil, err
	}
	resp := &agentreplv1.UnsubscribeGithubMergeQueueResponse{}
	subscriber, refused, done := s.resolveRegisteredNaming(ctx, rpc, resp, req.Msg.GetSubscriber(), subscriberRenames)
	if done {
		return answer(resp, refused)
	}
	key := prwatch.QueueKeyOf(req.Msg.GetMergeQueue())
	ended, err := s.deps.DB.UnsubscribeGithubMergeQueue(ctx, subscriber.Record.ID, key)
	if err != nil {
		return nil, fail(subscriber.Log, rpc, fmt.Errorf("end the subscription of %s to %s: %w", subscriber.Record.ID, key, err))
	}
	success := &agentreplv1.UnsubscribeGithubMergeQueueSuccess{Ended: &agentreplv1.UnsubscribeGithubMergeQueueSuccess_Unsubscribed{Unsubscribed: &agentreplv1.UnsubscribeGithubMergeQueueSuccessUnsubscribed{}}}
	if !ended {
		success.Ended = &agentreplv1.UnsubscribeGithubMergeQueueSuccess_NotSubscribed{NotSubscribed: &agentreplv1.UnsubscribeGithubMergeQueueSuccessNotSubscribed{}}
	}
	subscriber.Log.Info(opGithubMergeQueues, "ended a GitHub merge-queue subscription", dlog.Context{"subscriber": string(subscriber.Record.ID), "merge_queue": key.String(), "ended": ended})
	resp.Result = &agentreplv1.UnsubscribeGithubMergeQueueResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}

// ListGithubMergeQueueSubscriptions answers the GitHub merge queues one
// workspace follows.
func (s *server) ListGithubMergeQueueSubscriptions(
	ctx context.Context,
	req *connect.Request[agentreplv1.ListGithubMergeQueueSubscriptionsRequest],
) (*connect.Response[agentreplv1.ListGithubMergeQueueSubscriptionsResponse], error) {
	const rpc = "ListGithubMergeQueueSubscriptions"
	if err := validateWorkspaceRef("subscriber", req.Msg.GetSubscriber()); err != nil {
		return nil, err
	}
	resp := &agentreplv1.ListGithubMergeQueueSubscriptionsResponse{}
	subscriber, refused, done := s.resolveRegisteredNaming(ctx, rpc, resp, req.Msg.GetSubscriber(), subscriberRenames)
	if done {
		return answer(resp, refused)
	}
	subs, err := s.deps.DB.WorkspaceGithubMergeQueueSubscriptions(ctx, subscriber.Record.ID)
	if err != nil {
		return nil, fail(subscriber.Log, rpc, err)
	}
	success := &agentreplv1.ListGithubMergeQueueSubscriptionsSuccess{}
	for _, sub := range subs {
		success.Subscriptions = append(success.Subscriptions, withQueueDir(prwatch.QueueSubscriptionProto(sub), subscriber.Record.Dir))
	}
	resp.Result = &agentreplv1.ListGithubMergeQueueSubscriptionsResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}

// queueSubscriptionOf reads back the subscription WS holds to KEY, as the
// contract spells it.
func (s *server) queueSubscriptionOf(ctx context.Context, ws ids.WorkspaceID, dir string, key wsm.GithubMergeQueueKey) (*agentreplv1.GithubMergeQueueSubscription, error) {
	subs, err := s.deps.DB.WorkspaceGithubMergeQueueSubscriptions(ctx, ws)
	if err != nil {
		return nil, err
	}
	for _, sub := range subs {
		if sub.Queue == key {
			return withQueueDir(prwatch.QueueSubscriptionProto(sub), dir), nil
		}
	}
	return nil, fmt.Errorf("the subscription of %s to %s was recorded and is not there to read back", ws, key)
}

// withQueueDir fills the subscriber's directory, which the watcher does not
// hold.
func withQueueDir(sub *agentreplv1.GithubMergeQueueSubscription, dir string) *agentreplv1.GithubMergeQueueSubscription {
	sub.Subscriber = &workspacev1.WorkspaceRef{Id: sub.GetSubscriber().GetId(), Dir: dir}
	return sub
}

// validateSubscribeGithubMergeQueueRequest is
// SubscribeGithubMergeQueueRequest's base function.
func validateSubscribeGithubMergeQueueRequest(req *agentreplv1.SubscribeGithubMergeQueueRequest) *connect.Error {
	if err := validateWorkspaceRef("subscriber", req.GetSubscriber()); err != nil {
		return err
	}
	switch repo := req.GetRepository().(type) {
	case *agentreplv1.SubscribeGithubMergeQueueRequest_Named:
		if repo.Named.GetOwner() == "" || repo.Named.GetRepo() == "" {
			return invalid("named", "a repository names its owner and name")
		}
	case *agentreplv1.SubscribeGithubMergeQueueRequest_Subscribers:
	default:
		return invalid("repository", "a subscription names its repository")
	}
	if req.Branch != nil && strings.TrimSpace(req.GetBranch()) == "" {
		return invalid("branch", "a named branch is not empty; leave it unset for the default branch")
	}
	return nil
}

// validateGithubMergeQueueRef refuses a ref missing any part.
func validateGithubMergeQueueRef(field string, ref *agentreplv1.GithubMergeQueueRef) *connect.Error {
	if ref.GetRepository().GetOwner() == "" || ref.GetRepository().GetRepo() == "" || ref.GetBranch() == "" {
		return invalid(field, "a merge queue names its owner, repository and branch")
	}
	return nil
}
