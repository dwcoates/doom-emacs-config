package server

import (
	"context"
	"errors"
	"fmt"
	"regexp"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/slacklink"
	"agent-repld/internal/slackwatch"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
)

// The agent API's Slack follows: a workspace's subscription to a Slack
// thread, its mode, its posts, a read of a thread, the daemon's Slack status,
// and the reaction initiator's settings.

// SlackThreads is the part of the Slack watcher the server uses
// (slackwatch.Watcher).
type SlackThreads interface {
	Subscribe(ctx context.Context, ws slackwatch.Workspace, url string, mode wsm.SlackThreadMode, origin wsm.SlackThreadOrigin) (wsm.SlackThreadSubscription, bool, error)
	Unsubscribe(ctx context.Context, ws slackwatch.Workspace, url string) (bool, bool, error)
	SetMode(ctx context.Context, ws slackwatch.Workspace, url string, mode wsm.SlackThreadMode) (wsm.SlackThreadSubscription, error)
	Post(ctx context.Context, ws slackwatch.Workspace, url, text string, force bool) (string, slackwatch.Posts, error)
	Read(ctx context.Context, ws ids.WorkspaceID, url string, limit int) (wsm.SlackThreadKey, []slackwatch.Message, error)
	Status(ctx context.Context) (slackwatch.Login, []wsm.SlackThreadSubscription, wsm.SlackReactionSettings, slackwatch.PollState, error)
}

// opSlack is the operation every record of these rpcs carries.
const opSlack = "daemon.server.slack"

// The read's bounds (agentrepl.v1.ReadSlackThreadRequest.limit).
const (
	defaultSlackReadLimit = 25
	maxSlackReadLimit     = 200
)

// slackWorkspace is RECORD as the watcher names it.
func slackWorkspace(record wsm.Workspace) slackwatch.Workspace {
	return slackwatch.Workspace{ID: record.ID, Name: record.Name}
}

// validateSlackThreadURL is a thread link's base function.
func validateSlackThreadURL(field, url string) *connect.Error {
	if _, err := slacklink.ParseThread(url); err != nil {
		return invalid(field, err.Error())
	}
	return nil
}

// validateSlackThreadMode is a mode's base function.
func validateSlackThreadMode(field string, mode *agentreplv1.SlackThreadMode) *connect.Error {
	if _, ok := slackwatch.ModeOf(mode); !ok {
		return invalid(field, "a mode is listen, respond or interact")
	}
	return nil
}

// slackRefusal is a watcher's refusal as the error arm every Slack rpc spells
// it with, and that arm's fields. An rpc whose contract has no such arm
// answers it as a failure (refuse's UnlandedArm), which is a defect.
func slackRefusal(r *slackwatch.Refusal) refusal {
	out := refusal{Reason: r.Error(), Info: true}
	switch r.Kind {
	case slackwatch.RefusedNotSubscribed:
		out.Arm = "not_subscribed"
	case slackwatch.RefusedNoEmojiPrefix:
		out.Arm = "no_emoji_prefix"
	case slackwatch.RefusedReservedEmoji:
		out.Arm, out.Fields = "reserved_emoji", map[string]any{"emoji": r.Emoji}
	case slackwatch.RefusedListenMode:
		out.Arm = "listen_mode"
	case slackwatch.RefusedPostLimit:
		out.Arm, out.Fields = "post_limit_reached", map[string]any{"posts": slackwatch.PostsProto(r.Posts)}
	case slackwatch.RefusedSecret:
		out.Arm, out.Fields = "secret", map[string]any{"kinds": r.Secrets}
	case slackwatch.RefusedUnreachable:
		out.Arm, out.Fields = "unreachable", map[string]any{"detail": r.Detail}
	}
	return out
}

// SubscribeSlackThread subscribes one workspace's agent to a Slack thread.
func (s *server) SubscribeSlackThread(
	ctx context.Context,
	req *connect.Request[agentreplv1.SubscribeSlackThreadRequest],
) (*connect.Response[agentreplv1.SubscribeSlackThreadResponse], error) {
	const rpc = "SubscribeSlackThread"
	if err := validateWorkspaceRef("subscriber", req.Msg.GetSubscriber()); err != nil {
		return nil, err
	}
	if err := validateSlackThreadURL("thread_url", req.Msg.GetThreadUrl()); err != nil {
		return nil, err
	}
	if err := validateSlackThreadMode("mode", req.Msg.GetMode()); err != nil {
		return nil, err
	}
	resp := &agentreplv1.SubscribeSlackThreadResponse{}
	subscriber, refused, done := s.resolveRegisteredNaming(ctx, rpc, resp, req.Msg.GetSubscriber(), subscriberRenames)
	if done {
		return answer(resp, refused)
	}
	if subscriber.Record.Closed {
		return answer(resp, s.refuse(subscriber.Log, rpc, resp, refusal{Arm: "subscriber_closed", Reason: "a closed workspace runs no agent to tell", Info: true}))
	}
	mode, _ := slackwatch.ModeOf(req.Msg.GetMode())
	sub, held, err := s.deps.Slack.Subscribe(ctx, slackWorkspace(subscriber.Record), req.Msg.GetThreadUrl(), mode, wsm.SlackThreadOriginRequested)
	switch r := slackwatch.RefusalOf(err); {
	case r != nil:
		return answer(resp, s.refuse(subscriber.Log, rpc, resp, slackRefusal(r)))
	case errors.Is(err, wsm.ErrSubscriberClosed):
		return answer(resp, s.refuse(subscriber.Log, rpc, resp, refusal{Arm: "subscriber_closed", Reason: "the workspace closed while the subscription was taken", Info: true}))
	case errors.Is(err, wsm.ErrNotFound):
		return answer(resp, s.refuse(subscriber.Log, rpc, resp, refusal{Arm: "unknown_subscriber", Reason: "the workspace was forgotten while the subscription was taken", Info: true}))
	case err != nil:
		return nil, fail(subscriber.Log, rpc, fmt.Errorf("subscribe %s to %s: %w", subscriber.Record.ID, req.Msg.GetThreadUrl(), err))
	}
	stored := slackwatch.SubscriptionProto(sub, subscriber.Record.Dir)
	success := &agentreplv1.SubscribeSlackThreadSuccess{Taken: &agentreplv1.SubscribeSlackThreadSuccess_Subscribed{Subscribed: &agentreplv1.SubscribeSlackThreadSuccessSubscribed{Subscription: stored}}}
	if held {
		success.Taken = &agentreplv1.SubscribeSlackThreadSuccess_Resubscribed{Resubscribed: &agentreplv1.SubscribeSlackThreadSuccessResubscribed{Subscription: stored}}
	}
	resp.Result = &agentreplv1.SubscribeSlackThreadResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}

// UnsubscribeSlackThread ends one workspace's subscription to a Slack thread.
// Holding none is an answer, not a refusal.
func (s *server) UnsubscribeSlackThread(
	ctx context.Context,
	req *connect.Request[agentreplv1.UnsubscribeSlackThreadRequest],
) (*connect.Response[agentreplv1.UnsubscribeSlackThreadResponse], error) {
	const rpc = "UnsubscribeSlackThread"
	if err := validateWorkspaceRef("subscriber", req.Msg.GetSubscriber()); err != nil {
		return nil, err
	}
	if err := validateSlackThreadURL("thread_url", req.Msg.GetThreadUrl()); err != nil {
		return nil, err
	}
	resp := &agentreplv1.UnsubscribeSlackThreadResponse{}
	subscriber, refused, done := s.resolveRegisteredNaming(ctx, rpc, resp, req.Msg.GetSubscriber(), subscriberRenames)
	if done {
		return answer(resp, refused)
	}
	ended, announced, err := s.deps.Slack.Unsubscribe(ctx, slackWorkspace(subscriber.Record), req.Msg.GetThreadUrl())
	if err != nil {
		return nil, fail(subscriber.Log, rpc, fmt.Errorf("end the subscription of %s to %s: %w", subscriber.Record.ID, req.Msg.GetThreadUrl(), err))
	}
	success := &agentreplv1.UnsubscribeSlackThreadSuccess{Ended: &agentreplv1.UnsubscribeSlackThreadSuccess_NotSubscribed{NotSubscribed: &agentreplv1.UnsubscribeSlackThreadSuccessNotSubscribed{}}}
	if ended {
		success.Ended = &agentreplv1.UnsubscribeSlackThreadSuccess_Unsubscribed{Unsubscribed: &agentreplv1.UnsubscribeSlackThreadSuccessUnsubscribed{Announced: announced}}
	}
	resp.Result = &agentreplv1.UnsubscribeSlackThreadResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}

// SetSlackThreadMode changes what a subscribed workspace may do in its
// thread.
func (s *server) SetSlackThreadMode(
	ctx context.Context,
	req *connect.Request[agentreplv1.SetSlackThreadModeRequest],
) (*connect.Response[agentreplv1.SetSlackThreadModeResponse], error) {
	const rpc = "SetSlackThreadMode"
	if err := validateWorkspaceRef("subscriber", req.Msg.GetSubscriber()); err != nil {
		return nil, err
	}
	if err := validateSlackThreadURL("thread_url", req.Msg.GetThreadUrl()); err != nil {
		return nil, err
	}
	if err := validateSlackThreadMode("mode", req.Msg.GetMode()); err != nil {
		return nil, err
	}
	resp := &agentreplv1.SetSlackThreadModeResponse{}
	subscriber, refused, done := s.resolveRegisteredNaming(ctx, rpc, resp, req.Msg.GetSubscriber(), subscriberRenames)
	if done {
		return answer(resp, refused)
	}
	mode, _ := slackwatch.ModeOf(req.Msg.GetMode())
	sub, err := s.deps.Slack.SetMode(ctx, slackWorkspace(subscriber.Record), req.Msg.GetThreadUrl(), mode)
	if r := slackwatch.RefusalOf(err); r != nil {
		return answer(resp, s.refuse(subscriber.Log, rpc, resp, slackRefusal(r)))
	}
	if err != nil {
		return nil, fail(subscriber.Log, rpc, fmt.Errorf("set the mode of %s's subscription to %s: %w", subscriber.Record.ID, req.Msg.GetThreadUrl(), err))
	}
	resp.Result = &agentreplv1.SetSlackThreadModeResponse_Success{Success: &agentreplv1.SetSlackThreadModeSuccess{Subscription: slackwatch.SubscriptionProto(sub, subscriber.Record.Dir)}}
	return connect.NewResponse(resp), nil
}

// ListSlackThreadSubscriptions answers the threads one workspace, or every
// open workspace, follows.
func (s *server) ListSlackThreadSubscriptions(
	ctx context.Context,
	req *connect.Request[agentreplv1.ListSlackThreadSubscriptionsRequest],
) (*connect.Response[agentreplv1.ListSlackThreadSubscriptionsResponse], error) {
	const rpc = "ListSlackThreadSubscriptions"
	resp := &agentreplv1.ListSlackThreadSubscriptionsResponse{}
	success := &agentreplv1.ListSlackThreadSubscriptionsSuccess{}
	if req.Msg.Subscriber != nil {
		if err := validateWorkspaceRef("subscriber", req.Msg.GetSubscriber()); err != nil {
			return nil, err
		}
		subscriber, refused, done := s.resolveRegisteredNaming(ctx, rpc, resp, req.Msg.GetSubscriber(), subscriberRenames)
		if done {
			return answer(resp, refused)
		}
		subs, err := s.deps.DB.WorkspaceSlackThreadSubscriptions(ctx, subscriber.Record.ID)
		if err != nil {
			return nil, fail(subscriber.Log, rpc, err)
		}
		for _, sub := range subs {
			success.Subscriptions = append(success.Subscriptions, slackwatch.SubscriptionProto(sub, subscriber.Record.Dir))
		}
	} else {
		subs, err := s.slackSubscriptionsProto(ctx, nil)
		if err != nil {
			return nil, fail(s.log, rpc, err)
		}
		success.Subscriptions = subs
	}
	resp.Result = &agentreplv1.ListSlackThreadSubscriptionsResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}

// slackSubscriptionsProto spells SUBS, or every subscription when SUBS is
// nil, each subscriber at its directory.
func (s *server) slackSubscriptionsProto(ctx context.Context, subs []wsm.SlackThreadSubscription) ([]*agentreplv1.SlackThreadSubscription, error) {
	if subs == nil {
		var err error
		if subs, err = s.deps.DB.SlackThreadSubscriptions(ctx); err != nil {
			return nil, err
		}
	}
	workspaces, err := s.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		return nil, err
	}
	dirs := map[ids.WorkspaceID]string{}
	for _, ws := range workspaces {
		dirs[ws.ID] = ws.Dir
	}
	out := make([]*agentreplv1.SlackThreadSubscription, 0, len(subs))
	for _, sub := range subs {
		out = append(out, slackwatch.SubscriptionProto(sub, dirs[sub.Subscriber]))
	}
	return out, nil
}

// ReadSlackThread answers a thread's newest messages, read at once.
func (s *server) ReadSlackThread(
	ctx context.Context,
	req *connect.Request[agentreplv1.ReadSlackThreadRequest],
) (*connect.Response[agentreplv1.ReadSlackThreadResponse], error) {
	const rpc = "ReadSlackThread"
	if err := validateWorkspaceRef("reader", req.Msg.GetReader()); err != nil {
		return nil, err
	}
	if err := validateSlackThreadURL("thread_url", req.Msg.GetThreadUrl()); err != nil {
		return nil, err
	}
	limit := defaultSlackReadLimit
	if req.Msg.Limit != nil {
		if req.Msg.GetLimit() < 1 || req.Msg.GetLimit() > maxSlackReadLimit {
			return nil, invalid("limit", fmt.Sprintf("a read is of 1 to %d messages", maxSlackReadLimit))
		}
		limit = int(req.Msg.GetLimit())
	}
	resp := &agentreplv1.ReadSlackThreadResponse{}
	reader, refused, done := s.resolveRegisteredNaming(ctx, rpc, resp, req.Msg.GetReader(), map[string]string{workspace.ArmUnknownWorkspace: "unknown_reader"})
	if done {
		return answer(resp, refused)
	}
	key, msgs, err := s.deps.Slack.Read(ctx, reader.Record.ID, req.Msg.GetThreadUrl(), limit)
	if r := slackwatch.RefusalOf(err); r != nil {
		return answer(resp, s.refuse(reader.Log, rpc, resp, slackRefusal(r)))
	}
	if err != nil {
		return nil, fail(reader.Log, rpc, fmt.Errorf("read %s for %s: %w", req.Msg.GetThreadUrl(), reader.Record.ID, err))
	}
	success := &agentreplv1.ReadSlackThreadSuccess{Thread: slackwatch.ThreadProto(key, req.Msg.GetThreadUrl())}
	for _, m := range msgs {
		success.Messages = append(success.Messages, slackwatch.MessageProto(m))
	}
	resp.Result = &agentreplv1.ReadSlackThreadResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}

// PostToSlackThread posts a reply into a subscribed thread for a workspace.
func (s *server) PostToSlackThread(
	ctx context.Context,
	req *connect.Request[agentreplv1.PostToSlackThreadRequest],
) (*connect.Response[agentreplv1.PostToSlackThreadResponse], error) {
	const rpc = "PostToSlackThread"
	if err := validateWorkspaceRef("poster", req.Msg.GetPoster()); err != nil {
		return nil, err
	}
	if err := validateSlackThreadURL("thread_url", req.Msg.GetThreadUrl()); err != nil {
		return nil, err
	}
	if req.Msg.GetText() == "" {
		return nil, invalid("text", "a post says something")
	}
	resp := &agentreplv1.PostToSlackThreadResponse{}
	poster, refused, done := s.resolveRegisteredNaming(ctx, rpc, resp, req.Msg.GetPoster(), map[string]string{workspace.ArmUnknownWorkspace: "unknown_poster"})
	if done {
		return answer(resp, refused)
	}
	ts, posts, err := s.deps.Slack.Post(ctx, slackWorkspace(poster.Record), req.Msg.GetThreadUrl(), req.Msg.GetText(), req.Msg.GetForce())
	if r := slackwatch.RefusalOf(err); r != nil {
		return answer(resp, s.refuse(poster.Log, rpc, resp, slackRefusal(r)))
	}
	if err != nil {
		return nil, fail(poster.Log, rpc, fmt.Errorf("post into %s for %s: %w", req.Msg.GetThreadUrl(), poster.Record.ID, err))
	}
	resp.Result = &agentreplv1.PostToSlackThreadResponse_Success{Success: &agentreplv1.PostToSlackThreadSuccess{Ts: ts, Posts: slackwatch.PostsProto(posts)}}
	return connect.NewResponse(resp), nil
}

// GetSlackStatus answers how the daemon's Slack follows stand.
func (s *server) GetSlackStatus(
	ctx context.Context,
	_ *connect.Request[agentreplv1.GetSlackStatusRequest],
) (*connect.Response[agentreplv1.GetSlackStatusResponse], error) {
	const rpc = "GetSlackStatus"
	login, subs, settings, polls, err := s.deps.Slack.Status(ctx)
	if err != nil {
		return nil, fail(s.log, rpc, err)
	}
	spelled, err := s.slackSubscriptionsProto(ctx, subs)
	if err != nil {
		return nil, fail(s.log, rpc, err)
	}
	settingsProto, err := s.slackSettingsProto(ctx, settings)
	if err != nil {
		return nil, fail(s.log, rpc, err)
	}
	return connect.NewResponse(&agentreplv1.GetSlackStatusResponse{Result: &agentreplv1.GetSlackStatusResponse_Success{Success: &agentreplv1.GetSlackStatusSuccess{
		Login: slackwatch.LoginProto(login), Subscriptions: spelled, ReactionSettings: settingsProto, ReactionPolls: slackwatch.PollsProto(polls)}}}), nil
}

// slackSettingsProto spells SETTINGS, its repository at its directory.
func (s *server) slackSettingsProto(ctx context.Context, settings wsm.SlackReactionSettings) (*agentreplv1.SlackReactionSettings, error) {
	dir := ""
	if settings.RepoID != "" {
		repository, err, known := s.repositoryFor(ctx, &workspacev1.RepositoryRef{Id: settings.RepoID})
		if err != nil {
			return nil, err
		}
		if known {
			dir = repository.Dir
		}
	}
	return slackwatch.SettingsProto(settings, dir), nil
}

// slackEmoji is a binding's emoji name: Slack's shortcode alphabet.
var slackEmoji = regexp.MustCompile(`^[a-z0-9_+'-]+$`)

// slackUserID is a Slack member id.
var slackUserID = regexp.MustCompile(`^[UW][A-Z0-9]+$`)

// validateUpdateSlackReactionSettingsRequest is its request's base function.
func validateUpdateSlackReactionSettingsRequest(req *agentreplv1.UpdateSlackReactionSettingsRequest) *connect.Error {
	switch change := req.GetChange().(type) {
	case *agentreplv1.UpdateSlackReactionSettingsRequest_WatchUser:
		if !slackUserID.MatchString(change.WatchUser.GetUserId()) {
			return invalid("watch_user.user_id", "a Slack member id is U or W, then capitals and digits")
		}
	case *agentreplv1.UpdateSlackReactionSettingsRequest_Repository:
		if change.Repository.GetId() == "" && change.Repository.GetDir() == "" {
			return invalid("repository", "a repository ref names an id or a directory")
		}
	case *agentreplv1.UpdateSlackReactionSettingsRequest_Bind:
		if !slackEmoji.MatchString(change.Bind.GetEmoji()) {
			return invalid("bind.emoji", "an emoji name is lowercase letters, digits, -, _ and +, without colons")
		}
		if _, ok := slackwatch.BindingOf(change.Bind); !ok {
			return invalid("bind.action", "an action names its thread arm, and a subscribed one its mode")
		}
	case *agentreplv1.UpdateSlackReactionSettingsRequest_Unbind:
		if !slackEmoji.MatchString(change.Unbind) {
			return invalid("unbind", "an emoji name is lowercase letters, digits, -, _ and +, without colons")
		}
	default:
		return invalid("change", "an update names one change")
	}
	return nil
}

// UpdateSlackReactionSettings changes one of the reaction initiator's
// settings.
func (s *server) UpdateSlackReactionSettings(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdateSlackReactionSettingsRequest],
) (*connect.Response[agentreplv1.UpdateSlackReactionSettingsResponse], error) {
	const rpc = "UpdateSlackReactionSettings"
	if err := validateUpdateSlackReactionSettingsRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.UpdateSlackReactionSettingsResponse{}
	var (
		settings wsm.SlackReactionSettings
		err      error
	)
	switch change := req.Msg.GetChange().(type) {
	case *agentreplv1.UpdateSlackReactionSettingsRequest_WatchUser:
		settings, err = s.deps.DB.SetSlackReactionWatchUser(ctx, change.WatchUser.GetUserId())
	case *agentreplv1.UpdateSlackReactionSettingsRequest_Repository:
		repository, cerr, done := s.resolveRepository(ctx, rpc, resp, change.Repository)
		if done {
			return answer(resp, cerr)
		}
		settings, err = s.deps.DB.SetSlackReactionRepository(ctx, string(repository.ID))
		if errors.Is(err, wsm.ErrNotFound) {
			return answer(resp, s.refuse(s.log, rpc, resp, refusal{Arm: "unknown_repository", Reason: "the repository was forgotten while it was set", Info: true}))
		}
	case *agentreplv1.UpdateSlackReactionSettingsRequest_Bind:
		binding, _ := slackwatch.BindingOf(change.Bind)
		settings, err = s.deps.DB.BindSlackReaction(ctx, binding)
	case *agentreplv1.UpdateSlackReactionSettingsRequest_Unbind:
		settings, err = s.deps.DB.UnbindSlackReaction(ctx, change.Unbind)
		if errors.Is(err, wsm.ErrNotFound) {
			return answer(resp, s.refuse(s.log, rpc, resp, refusal{Arm: "not_bound", Reason: fmt.Sprintf("the emoji %q has no binding", change.Unbind),
				Fields: map[string]any{"emoji": change.Unbind}, Info: true}))
		}
	}
	if err != nil {
		return nil, fail(s.log, rpc, err)
	}
	spelled, err := s.slackSettingsProto(ctx, settings)
	if err != nil {
		return nil, fail(s.log, rpc, err)
	}
	s.log.Info(opSlack, "changed a Slack reaction setting", dlog.Context{"missing": settings.Missing(), "bindings": len(settings.Bindings)})
	resp.Result = &agentreplv1.UpdateSlackReactionSettingsResponse_Success{Success: &agentreplv1.UpdateSlackReactionSettingsSuccess{Settings: spelled}}
	return connect.NewResponse(resp), nil
}
