package server

import (
	"context"
	"errors"
	"fmt"
	"time"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
)

// The agent API's workspace reads and follows: a workspace's status, and one
// workspace's subscription to another's edges.

// followedRenames spells an unknown followed workspace as SubscribeWorkspace's
// own arm.
var followedRenames = map[string]string{workspace.ArmUnknownWorkspace: "unknown_followed"}

// GetWorkspaceStatus answers one workspace's status, open or closed.
func (s *server) GetWorkspaceStatus(
	ctx context.Context,
	req *connect.Request[agentreplv1.GetWorkspaceStatusRequest],
) (*connect.Response[agentreplv1.GetWorkspaceStatusResponse], error) {
	const rpc = "GetWorkspaceStatus"
	resp := &agentreplv1.GetWorkspaceStatusResponse{}
	if err := validateWorkspaceRef("workspace", req.Msg.GetWorkspace()); err != nil {
		return nil, err
	}
	subject, refused, done := s.resolveRegisteredNaming(ctx, rpc, resp, req.Msg.GetWorkspace(), nil)
	if done {
		return answer(resp, refused)
	}
	status, err := s.deps.Verbs.Status(ctx, subject.Record.ID)
	if err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	resp.Result = &agentreplv1.GetWorkspaceStatusResponse_Success{Success: &agentreplv1.GetWorkspaceStatusSuccess{Status: status}}
	return connect.NewResponse(resp), nil
}

// SubscribeWorkspace subscribes one workspace's agent to another's edges and
// answers the followed workspace's status as the subscription stands taken.
func (s *server) SubscribeWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.SubscribeWorkspaceRequest],
) (*connect.Response[agentreplv1.SubscribeWorkspaceResponse], error) {
	const rpc = "SubscribeWorkspace"
	if err := validateSubscribeWorkspaceRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.SubscribeWorkspaceResponse{}
	subscriber, refused, done := s.resolveRegisteredNaming(ctx, rpc, resp, req.Msg.GetSubscriber(), subscriberRenames)
	if done {
		return answer(resp, refused)
	}
	followed, refused, done := s.resolveRegisteredNaming(ctx, rpc, resp, req.Msg.GetFollowed(), followedRenames)
	if done {
		return answer(resp, refused)
	}
	log := subscriber.Log
	if subscriber.Record.ID == followed.Record.ID {
		return answer(resp, s.refuse(log, rpc, resp, s.fill(refusal{Arm: "follows_itself", Reason: "a workspace does not subscribe to itself"})))
	}
	events := workspaceEventsOf(req.Msg.GetEvents())
	replaced, err := s.deps.DB.SubscribeWorkspace(ctx, wsm.WorkspaceSubscription{
		Subscriber: subscriber.Record.ID, Followed: followed.Record.ID, Events: events, SubscribedAt: time.Now(),
	})
	switch {
	case errors.Is(err, wsm.ErrSubscriberClosed):
		return answer(resp, s.refuse(log, rpc, resp, s.fill(refusal{Arm: "subscriber_closed", Reason: "a closed workspace runs no agent to tell", Info: true})))
	case errors.Is(err, wsm.ErrUnknownFollowed), errors.Is(err, wsm.ErrNotFound):
		// The two were resolved above; one forgotten since is an answer.
		arm := "unknown_followed"
		if errors.Is(err, wsm.ErrNotFound) {
			arm = "unknown_subscriber"
		}
		return answer(resp, s.refuse(log, rpc, resp, s.fill(refusal{Arm: arm, Reason: err.Error(), NotFound: true})))
	case err != nil:
		return nil, fail(log, rpc, fmt.Errorf("record the subscription of %s to %s: %w", subscriber.Record.ID, followed.Record.ID, err))
	}
	status, err := s.deps.Verbs.Status(ctx, followed.Record.ID)
	if err != nil {
		return answer(resp, s.answerRefusal(log, rpc, resp, err, followedRenames))
	}
	log.Info("daemon.server.subscribe_workspace", "subscribed the workspace's agent to another workspace's edges", dlog.Context{
		"subscriber": string(subscriber.Record.ID), "followed": string(followed.Record.ID), "events": events.String(), "replaced": replaced})
	taken := &agentreplv1.SubscribeWorkspaceSuccessTaken{Taken: &agentreplv1.SubscribeWorkspaceSuccessTaken_Subscribed{Subscribed: &agentreplv1.SubscribeWorkspaceSuccessTakenSubscribed{}}}
	if replaced {
		taken.Taken = &agentreplv1.SubscribeWorkspaceSuccessTaken_Resubscribed{Resubscribed: &agentreplv1.SubscribeWorkspaceSuccessTakenResubscribed{}}
	}
	resp.Result = &agentreplv1.SubscribeWorkspaceResponse_Success{Success: &agentreplv1.SubscribeWorkspaceSuccess{Taken: taken, Status: status}}
	return connect.NewResponse(resp), nil
}

// UnsubscribeWorkspace ends one workspace's subscription to another. Holding
// none is an answer, not a refusal.
func (s *server) UnsubscribeWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.UnsubscribeWorkspaceRequest],
) (*connect.Response[agentreplv1.UnsubscribeWorkspaceResponse], error) {
	const rpc = "UnsubscribeWorkspace"
	if err := validateUnsubscribeWorkspaceRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.UnsubscribeWorkspaceResponse{}
	subscriber, refused, done := s.resolveRegisteredNaming(ctx, rpc, resp, req.Msg.GetSubscriber(), subscriberRenames)
	if done {
		return answer(resp, refused)
	}
	followed := wsm.WorkspaceID(req.Msg.GetFollowed().GetId())
	ended, err := s.deps.DB.UnsubscribeWorkspace(ctx, subscriber.Record.ID, followed)
	if err != nil {
		return nil, fail(subscriber.Log, rpc, fmt.Errorf("end the subscription of %s to %s: %w", subscriber.Record.ID, followed, err))
	}
	success := &agentreplv1.UnsubscribeWorkspaceSuccess{Ended: &agentreplv1.UnsubscribeWorkspaceSuccess_Unsubscribed{Unsubscribed: &agentreplv1.UnsubscribeWorkspaceSuccessUnsubscribed{}}}
	if !ended {
		success.Ended = &agentreplv1.UnsubscribeWorkspaceSuccess_NotSubscribed{NotSubscribed: &agentreplv1.UnsubscribeWorkspaceSuccessNotSubscribed{}}
	}
	subscriber.Log.Info("daemon.server.unsubscribe_workspace", "ended a workspace subscription", dlog.Context{
		"subscriber": string(subscriber.Record.ID), "followed": string(followed), "ended": ended})
	resp.Result = &agentreplv1.UnsubscribeWorkspaceResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}

// workspaceEventsOf reads a subscription's events. The request's validation
// refused an unspecified one, so an event this switch does not know is a
// contract arm added without its mapping, and fails hard.
func workspaceEventsOf(events []agentreplv1.WorkspaceNoticeEvent) wsm.WorkspaceEvents {
	var out []wsm.WorkspaceEvent
	for _, e := range events {
		switch e {
		case agentreplv1.WorkspaceNoticeEvent_WORKSPACE_NOTICE_EVENT_TURN_STARTED:
			out = append(out, wsm.WorkspaceEventTurnStarted)
		case agentreplv1.WorkspaceNoticeEvent_WORKSPACE_NOTICE_EVENT_TURN_ENDED:
			out = append(out, wsm.WorkspaceEventTurnEnded)
		case agentreplv1.WorkspaceNoticeEvent_WORKSPACE_NOTICE_EVENT_NEEDS_INPUT:
			out = append(out, wsm.WorkspaceEventNeedsInput)
		case agentreplv1.WorkspaceNoticeEvent_WORKSPACE_NOTICE_EVENT_LANDED:
			out = append(out, wsm.WorkspaceEventLanded)
		case agentreplv1.WorkspaceNoticeEvent_WORKSPACE_NOTICE_EVENT_CLOSED:
			out = append(out, wsm.WorkspaceEventClosed)
		default:
			panic(fmt.Sprintf("server: workspace notice event %s has no store event", e))
		}
	}
	return wsm.NewWorkspaceEvents(out...)
}
