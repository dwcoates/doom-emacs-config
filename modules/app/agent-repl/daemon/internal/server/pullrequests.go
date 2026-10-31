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

// The agent API's pull-request follows: a workspace's subscription to a
// GitHub pull request, the list of them, and a comment it posts there.

// PullRequests is the part of the pull-request watcher the server uses.
type PullRequests interface {
	Anchor(ctx context.Context, subscriber ids.WorkspaceID, pr wsm.PullRequestKey, events wsm.PullRequestEvents, origin wsm.PullRequestOrigin) (wsm.PullRequestSubscription, *agentreplv1.PullRequestStatus, error)
	Comment(ctx context.Context, ws ids.WorkspaceID, pr wsm.PullRequestKey, inReplyTo *uint64, markdown string) (prwatch.Posted, error)
}

// opPullRequests is the operation every record of these rpcs carries.
const opPullRequests = "daemon.server.pull_requests"

// SubscribePullRequest subscribes one workspace's agent to a pull request's
// activity, answering the pull request as the subscription is anchored on it.
func (s *server) SubscribePullRequest(
	ctx context.Context,
	req *connect.Request[agentreplv1.SubscribePullRequestRequest],
) (*connect.Response[agentreplv1.SubscribePullRequestResponse], error) {
	const rpc = "SubscribePullRequest"
	if err := validateSubscribePullRequestRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.SubscribePullRequestResponse{}
	subscriber, refused, done := s.resolveRegisteredNaming(ctx, rpc, resp, req.Msg.GetSubscriber(), subscriberRenames)
	if done {
		return answer(resp, refused)
	}
	log := subscriber.Log
	refuse := func(cause *agentreplv1.SubscribePullRequestError, reason string) (*connect.Response[agentreplv1.SubscribePullRequestResponse], error) {
		log.Info(opPullRequests, "refused a pull-request subscription", dlog.Context{"rpc": rpc, "reason": reason})
		resp.Result = &agentreplv1.SubscribePullRequestResponse_Error{Error: cause}
		return connect.NewResponse(resp), nil
	}
	if subscriber.Record.Closed {
		return refuse(&agentreplv1.SubscribePullRequestError{Cause: &agentreplv1.SubscribePullRequestError_SubscriberClosed{SubscriberClosed: &agentreplv1.SubscribePullRequestSubscriberClosed{}}},
			"a closed workspace runs no agent to tell")
	}
	pr, unparsable, err := s.pullRequestOf(ctx, subscriber.Record.Dir, req.Msg)
	switch {
	case unparsable != nil:
		return refuse(&agentreplv1.SubscribePullRequestError{Cause: &agentreplv1.SubscribePullRequestError_Unparsable{Unparsable: &agentreplv1.SubscribePullRequestUnparsable{
			Text: unparsable.Text, Reason: unparsable.Reason}}}, unparsable.Error())
	case err != nil:
		return nil, fail(log, rpc, err)
	}
	events := prwatch.EventsOf(req.Msg.GetEvents())
	sub, status, err := s.deps.PullRequests.Anchor(ctx, subscriber.Record.ID, pr, events, wsm.PullRequestOriginRequested)
	if err != nil {
		return refuse(&agentreplv1.SubscribePullRequestError{Cause: &agentreplv1.SubscribePullRequestError_Unreadable{Unreadable: &agentreplv1.SubscribePullRequestUnreadable{
			Cause: prwatch.FailureProto(err)}}}, err.Error())
	}
	success := &agentreplv1.SubscribePullRequestSuccess{PullRequest: prwatch.RefProto(pr), Status: status}
	fields := dlog.Context{"subscriber": string(subscriber.Record.ID), "pull_request": pr.String(), "events": events.String()}
	if sub.Ended != nil {
		log.Info(opPullRequests, "the pull request already ended; nothing was subscribed", fields)
		success.Taken = &agentreplv1.SubscribePullRequestSuccess_AlreadyEnded{AlreadyEnded: &agentreplv1.SubscribePullRequestSuccessAlreadyEnded{}}
		resp.Result = &agentreplv1.SubscribePullRequestResponse_Success{Success: success}
		return connect.NewResponse(resp), nil
	}
	replaced, err := s.deps.DB.SubscribePullRequest(ctx, sub)
	switch {
	case errors.Is(err, wsm.ErrSubscriberClosed):
		return refuse(&agentreplv1.SubscribePullRequestError{Cause: &agentreplv1.SubscribePullRequestError_SubscriberClosed{SubscriberClosed: &agentreplv1.SubscribePullRequestSubscriberClosed{}}},
			"the workspace closed while the subscription was taken")
	case errors.Is(err, wsm.ErrNotFound):
		return refuse(&agentreplv1.SubscribePullRequestError{Cause: &agentreplv1.SubscribePullRequestError_UnknownSubscriber{UnknownSubscriber: &agentreplv1.SubscribePullRequestUnknownSubscriber{}}},
			"the workspace was forgotten while the subscription was taken")
	case errors.Is(err, wsm.ErrSubscriptionEnded):
		success.Taken = &agentreplv1.SubscribePullRequestSuccess_AlreadyEnded{AlreadyEnded: &agentreplv1.SubscribePullRequestSuccessAlreadyEnded{}}
		resp.Result = &agentreplv1.SubscribePullRequestResponse_Success{Success: success}
		return connect.NewResponse(resp), nil
	case err != nil:
		return nil, fail(log, rpc, fmt.Errorf("record the subscription of %s to %s: %w", subscriber.Record.ID, pr, err))
	}
	stored, err := s.subscriptionOf(ctx, subscriber.Record.ID, subscriber.Record.Dir, pr)
	if err != nil {
		return nil, fail(log, rpc, err)
	}
	if replaced {
		success.Taken = &agentreplv1.SubscribePullRequestSuccess_Resubscribed{Resubscribed: &agentreplv1.SubscribePullRequestSuccessResubscribed{Subscription: stored}}
	} else {
		success.Taken = &agentreplv1.SubscribePullRequestSuccess_Subscribed{Subscribed: &agentreplv1.SubscribePullRequestSuccessSubscribed{Subscription: stored}}
	}
	fields["replaced"] = replaced
	log.Info(opPullRequests, "subscribed the workspace's agent to a pull request", fields)
	resp.Result = &agentreplv1.SubscribePullRequestResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}

// UnsubscribePullRequest ends one workspace's subscription to a pull request.
// Holding none is an answer, not a refusal.
func (s *server) UnsubscribePullRequest(
	ctx context.Context,
	req *connect.Request[agentreplv1.UnsubscribePullRequestRequest],
) (*connect.Response[agentreplv1.UnsubscribePullRequestResponse], error) {
	const rpc = "UnsubscribePullRequest"
	if err := validateUnsubscribePullRequestRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.UnsubscribePullRequestResponse{}
	subscriber, refused, done := s.resolveRegisteredNaming(ctx, rpc, resp, req.Msg.GetSubscriber(), subscriberRenames)
	if done {
		return answer(resp, refused)
	}
	pr := prwatch.KeyOf(req.Msg.GetPullRequest())
	ended, err := s.deps.DB.UnsubscribePullRequest(ctx, subscriber.Record.ID, pr)
	if err != nil {
		return nil, fail(subscriber.Log, rpc, fmt.Errorf("end the subscription of %s to %s: %w", subscriber.Record.ID, pr, err))
	}
	success := &agentreplv1.UnsubscribePullRequestSuccess{Ended: &agentreplv1.UnsubscribePullRequestSuccess_Unsubscribed{Unsubscribed: &agentreplv1.UnsubscribePullRequestSuccessUnsubscribed{}}}
	if !ended {
		success.Ended = &agentreplv1.UnsubscribePullRequestSuccess_NotSubscribed{NotSubscribed: &agentreplv1.UnsubscribePullRequestSuccessNotSubscribed{}}
	}
	subscriber.Log.Info(opPullRequests, "ended a pull-request subscription", dlog.Context{"subscriber": string(subscriber.Record.ID), "pull_request": pr.String(), "ended": ended})
	resp.Result = &agentreplv1.UnsubscribePullRequestResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}

// ListPullRequestSubscriptions answers the pull requests one workspace
// follows, ended ones left out.
func (s *server) ListPullRequestSubscriptions(
	ctx context.Context,
	req *connect.Request[agentreplv1.ListPullRequestSubscriptionsRequest],
) (*connect.Response[agentreplv1.ListPullRequestSubscriptionsResponse], error) {
	const rpc = "ListPullRequestSubscriptions"
	if err := validateWorkspaceRef("subscriber", req.Msg.GetSubscriber()); err != nil {
		return nil, err
	}
	resp := &agentreplv1.ListPullRequestSubscriptionsResponse{}
	subscriber, refused, done := s.resolveRegisteredNaming(ctx, rpc, resp, req.Msg.GetSubscriber(), subscriberRenames)
	if done {
		return answer(resp, refused)
	}
	subs, err := s.deps.DB.WorkspacePullRequestSubscriptions(ctx, subscriber.Record.ID)
	if err != nil {
		return nil, fail(subscriber.Log, rpc, err)
	}
	success := &agentreplv1.ListPullRequestSubscriptionsSuccess{}
	for _, sub := range subs {
		if sub.Ended != nil {
			continue
		}
		success.Subscriptions = append(success.Subscriptions, &agentreplv1.ListPullRequestSubscriptionsEntry{Subscription: withDir(prwatch.SubscriptionProto(sub), subscriber.Record.Dir)})
	}
	resp.Result = &agentreplv1.ListPullRequestSubscriptionsResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}

// CommentOnPullRequest posts a comment on a pull request for a workspace,
// recorded as its own so it is never told back to it.
func (s *server) CommentOnPullRequest(
	ctx context.Context,
	req *connect.Request[agentreplv1.CommentOnPullRequestRequest],
) (*connect.Response[agentreplv1.CommentOnPullRequestResponse], error) {
	const rpc = "CommentOnPullRequest"
	if err := validateCommentOnPullRequestRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.CommentOnPullRequestResponse{}
	subject, refused, done := s.resolveRegisteredNaming(ctx, rpc, resp, req.Msg.GetWorkspace(), nil)
	if done {
		return answer(resp, refused)
	}
	pr := prwatch.KeyOf(req.Msg.GetPullRequest())
	var inReplyTo *uint64
	markdown := req.Msg.GetConversation().GetMarkdown()
	if reply := req.Msg.GetReply(); reply != nil {
		id := reply.GetInReplyToCommentId()
		inReplyTo, markdown = &id, reply.GetMarkdown()
	}
	posted, err := s.deps.PullRequests.Comment(ctx, subject.Record.ID, pr, inReplyTo, markdown)
	switch {
	case errors.Is(err, prwatch.ErrUnknownThread):
		resp.Result = &agentreplv1.CommentOnPullRequestResponse_Error{Error: &agentreplv1.CommentOnPullRequestError{Cause: &agentreplv1.CommentOnPullRequestError_UnknownThread{UnknownThread: &agentreplv1.CommentOnPullRequestUnknownThread{}}}}
		return connect.NewResponse(resp), nil
	case err != nil:
		subject.Log.Info(opPullRequests, "the comment was not posted", dlog.WithCause(dlog.Context{"rpc": rpc, "pull_request": pr.String()}, err))
		resp.Result = &agentreplv1.CommentOnPullRequestResponse_Error{Error: &agentreplv1.CommentOnPullRequestError{Cause: &agentreplv1.CommentOnPullRequestError_NotPosted{NotPosted: &agentreplv1.CommentOnPullRequestNotPosted{Cause: prwatch.FailureProto(err)}}}}
		return connect.NewResponse(resp), nil
	}
	resp.Result = &agentreplv1.CommentOnPullRequestResponse_Success{Success: &agentreplv1.CommentOnPullRequestSuccess{CommentId: posted.ID, Url: posted.URL}}
	return connect.NewResponse(resp), nil
}

// pullRequestOf resolves the pull request a subscribe names, against the
// subscriber's worktree DIR for `#<number>`.
func (s *server) pullRequestOf(ctx context.Context, dir string, req *agentreplv1.SubscribePullRequestRequest) (wsm.PullRequestKey, *prwatch.UnparsableError, error) {
	if ref := req.GetRef(); ref != nil {
		return prwatch.KeyOf(ref), nil, nil
	}
	pr, err := prwatch.Resolve(ctx, s.deps.Remotes, dir, req.GetText())
	var unparsable *prwatch.UnparsableError
	if errors.As(err, &unparsable) {
		return pr, unparsable, nil
	}
	return pr, nil, err
}

// subscriptionOf reads back the subscription WS holds to PR, as the contract
// spells it.
func (s *server) subscriptionOf(ctx context.Context, ws ids.WorkspaceID, dir string, pr wsm.PullRequestKey) (*agentreplv1.PullRequestSubscription, error) {
	subs, err := s.deps.DB.WorkspacePullRequestSubscriptions(ctx, ws)
	if err != nil {
		return nil, err
	}
	for _, sub := range subs {
		if sub.PullRequest == pr {
			return withDir(prwatch.SubscriptionProto(sub), dir), nil
		}
	}
	return nil, fmt.Errorf("the subscription of %s to %s was recorded and is not there to read back", ws, pr)
}

// withDir fills the subscriber's directory, which the watcher does not hold.
func withDir(sub *agentreplv1.PullRequestSubscription, dir string) *agentreplv1.PullRequestSubscription {
	sub.Subscriber = &workspacev1.WorkspaceRef{Id: sub.GetSubscriber().GetId(), Dir: dir}
	return sub
}

// validateSubscribePullRequestRequest is SubscribePullRequestRequest's base
// function.
func validateSubscribePullRequestRequest(req *agentreplv1.SubscribePullRequestRequest) *connect.Error {
	if err := validateWorkspaceRef("subscriber", req.GetSubscriber()); err != nil {
		return err
	}
	switch pr := req.GetPullRequest().(type) {
	case *agentreplv1.SubscribePullRequestRequest_Ref:
		if err := validatePullRequestRef("ref", pr.Ref); err != nil {
			return err
		}
	case *agentreplv1.SubscribePullRequestRequest_Text:
		if strings.TrimSpace(pr.Text) == "" {
			return invalid("text", "a subscription names its pull request")
		}
	default:
		return invalid("pull_request", "a subscription names its pull request")
	}
	if len(req.GetEvents()) == 0 {
		return invalid("events", "a subscription names at least one event")
	}
	for _, e := range req.GetEvents() {
		if e == agentreplv1.PullRequestNoticeEvent_PULL_REQUEST_NOTICE_EVENT_UNSPECIFIED {
			return invalid("events", "an unspecified event is no event")
		}
		if _, declared := agentreplv1.PullRequestNoticeEvent_name[int32(e)]; !declared {
			return invalid("events", fmt.Sprintf("event %d is not declared", e))
		}
	}
	return nil
}

// validateUnsubscribePullRequestRequest is UnsubscribePullRequestRequest's
// base function.
func validateUnsubscribePullRequestRequest(req *agentreplv1.UnsubscribePullRequestRequest) *connect.Error {
	if err := validateWorkspaceRef("subscriber", req.GetSubscriber()); err != nil {
		return err
	}
	return validatePullRequestRef("pull_request", req.GetPullRequest())
}

// validateCommentOnPullRequestRequest is CommentOnPullRequestRequest's base
// function.
func validateCommentOnPullRequestRequest(req *agentreplv1.CommentOnPullRequestRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	if err := validatePullRequestRef("pull_request", req.GetPullRequest()); err != nil {
		return err
	}
	switch placement := req.GetPlacement().(type) {
	case *agentreplv1.CommentOnPullRequestRequest_Conversation:
		if strings.TrimSpace(placement.Conversation.GetMarkdown()) == "" {
			return invalid("conversation.markdown", "a comment says something")
		}
	case *agentreplv1.CommentOnPullRequestRequest_Reply:
		if placement.Reply.GetInReplyToCommentId() == 0 {
			return invalid("reply.in_reply_to_comment_id", "a reply names the comment it answers")
		}
		if strings.TrimSpace(placement.Reply.GetMarkdown()) == "" {
			return invalid("reply.markdown", "a reply says something")
		}
	default:
		return invalid("placement", "a comment names where it goes")
	}
	return nil
}

// validatePullRequestRef refuses a ref missing any part.
func validatePullRequestRef(field string, ref *agentreplv1.PullRequestRef) *connect.Error {
	if ref.GetOwner() == "" || ref.GetRepo() == "" || ref.GetNumber() == 0 {
		return invalid(field, "a pull request names its owner, repository and number")
	}
	return nil
}
