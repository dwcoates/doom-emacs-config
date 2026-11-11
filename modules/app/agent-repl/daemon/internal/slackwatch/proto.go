package slackwatch

import (
	"fmt"

	"google.golang.org/protobuf/types/known/timestamppb"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/wsm"
)

// The contract's spellings of the watcher's facts (agentrepl.v1
// slack_thread.proto and slack_reactions.proto), each one mapping, here.

// ModeOf reads a contract mode, false for one with no arm set.
func ModeOf(m *agentreplv1.SlackThreadMode) (wsm.SlackThreadMode, bool) {
	switch m.GetMode().(type) {
	case *agentreplv1.SlackThreadMode_Listen:
		return wsm.SlackThreadModeListen, true
	case *agentreplv1.SlackThreadMode_Respond:
		return wsm.SlackThreadModeRespond, true
	case *agentreplv1.SlackThreadMode_Interact:
		return wsm.SlackThreadModeInteract, true
	}
	return "", false
}

// ModeProto spells MODE.
func ModeProto(mode wsm.SlackThreadMode) *agentreplv1.SlackThreadMode {
	switch mode {
	case wsm.SlackThreadModeListen:
		return &agentreplv1.SlackThreadMode{Mode: &agentreplv1.SlackThreadMode_Listen{Listen: &agentreplv1.SlackThreadModeListen{}}}
	case wsm.SlackThreadModeRespond:
		return &agentreplv1.SlackThreadMode{Mode: &agentreplv1.SlackThreadMode_Respond{Respond: &agentreplv1.SlackThreadModeRespond{}}}
	case wsm.SlackThreadModeInteract:
		return &agentreplv1.SlackThreadMode{Mode: &agentreplv1.SlackThreadMode_Interact{Interact: &agentreplv1.SlackThreadModeInteract{}}}
	}
	panic(fmt.Sprintf("slackwatch: no mode %q", mode))
}

// ParseMode reads a mode as the command line spells it.
func ParseMode(text string) (wsm.SlackThreadMode, bool) {
	mode := wsm.SlackThreadMode(text)
	return mode, mode.Valid()
}

// PostsProto spells P.
func PostsProto(p Posts) *agentreplv1.SlackThreadPosts {
	return &agentreplv1.SlackThreadPosts{Made: uint32(p.Made), Limit: uint32(p.Limit)}
}

// ThreadProto spells the thread KEY as URL named it.
func ThreadProto(key wsm.SlackThreadKey, url string) *agentreplv1.SlackThreadRef {
	return &agentreplv1.SlackThreadRef{Channel: key.Channel, ThreadTs: key.ThreadTS, Url: url}
}

// SubscriptionProto spells SUB, its subscriber at DIR.
func SubscriptionProto(sub wsm.SlackThreadSubscription, dir string) *agentreplv1.SlackThreadSubscription {
	out := &agentreplv1.SlackThreadSubscription{
		Subscriber:      &workspacev1.WorkspaceRef{Id: string(sub.Subscriber), Dir: dir},
		Thread:          ThreadProto(sub.Thread, sub.URL),
		Mode:            ModeProto(sub.Mode),
		Origin:          &agentreplv1.SlackThreadSubscriptionOrigin{Origin: &agentreplv1.SlackThreadSubscriptionOrigin_Requested{Requested: &agentreplv1.SlackThreadSubscriptionOriginRequested{}}},
		SubscribedAt:    timestamppb.New(sub.SubscribedAt),
		Posts:           PostsProto(PostsOf(sub)),
		Health:          &agentreplv1.SlackThreadSubscriptionHealth{Health: &agentreplv1.SlackThreadSubscriptionHealth_Healthy{Healthy: &agentreplv1.SlackThreadSubscriptionHealthy{}}},
		PendingMessages: uint32(sub.Pending),
	}
	if sub.Origin == wsm.SlackThreadOriginCreation {
		out.Origin.Origin = &agentreplv1.SlackThreadSubscriptionOrigin_Creation{Creation: &agentreplv1.SlackThreadSubscriptionOriginCreation{}}
	}
	if sub.FailingSince != nil {
		out.Health.Health = &agentreplv1.SlackThreadSubscriptionHealth_Failing{Failing: &agentreplv1.SlackThreadSubscriptionFailing{
			Since: timestamppb.New(*sub.FailingSince), Detail: sub.FailureDetail}}
	}
	return out
}

// PriorityOf reads a contract priority, nil for none.
func PriorityOf(p *agentreplv1.WorkspacePriority) *wsm.Priority {
	var level wsm.Priority
	switch p.GetLevel().(type) {
	case *agentreplv1.WorkspacePriority_P05:
		level = wsm.PriorityP05
	case *agentreplv1.WorkspacePriority_P1:
		level = wsm.PriorityP1
	case *agentreplv1.WorkspacePriority_P2:
		level = wsm.PriorityP2
	case *agentreplv1.WorkspacePriority_P3:
		level = wsm.PriorityP3
	default:
		return nil
	}
	return &level
}

// priorityProto spells P.
func priorityProto(p wsm.Priority) *agentreplv1.WorkspacePriority {
	switch p {
	case wsm.PriorityP05:
		return &agentreplv1.WorkspacePriority{Level: &agentreplv1.WorkspacePriority_P05{P05: &agentreplv1.WorkspacePriorityP05{}}}
	case wsm.PriorityP1:
		return &agentreplv1.WorkspacePriority{Level: &agentreplv1.WorkspacePriority_P1{P1: &agentreplv1.WorkspacePriorityP1{}}}
	case wsm.PriorityP2:
		return &agentreplv1.WorkspacePriority{Level: &agentreplv1.WorkspacePriority_P2{P2: &agentreplv1.WorkspacePriorityP2{}}}
	case wsm.PriorityP3:
		return &agentreplv1.WorkspacePriority{Level: &agentreplv1.WorkspacePriority_P3{P3: &agentreplv1.WorkspacePriorityP3{}}}
	}
	panic(fmt.Sprintf("slackwatch: no priority %d", int(p)))
}

// BindingOf reads a contract binding, false when its thread arm or its
// subscription's mode is unset.
func BindingOf(b *agentreplv1.SlackReactionBinding) (wsm.SlackReactionBinding, bool) {
	out := wsm.SlackReactionBinding{Emoji: b.GetEmoji(), OneShot: b.GetAction().GetOneShot(), Priority: PriorityOf(b.GetAction().GetPriority())}
	switch arm := b.GetAction().GetThread().(type) {
	case *agentreplv1.SlackReactionAction_Announced:
		out.Thread = wsm.SlackReactionAnnounced
	case *agentreplv1.SlackReactionAction_Subscribed:
		mode, ok := ModeOf(arm.Subscribed.GetMode())
		if !ok {
			return wsm.SlackReactionBinding{}, false
		}
		out.Thread, out.Mode = wsm.SlackReactionSubscribed, mode
	default:
		return wsm.SlackReactionBinding{}, false
	}
	return out, true
}

// BindingProto spells B.
func BindingProto(b wsm.SlackReactionBinding) *agentreplv1.SlackReactionBinding {
	action := &agentreplv1.SlackReactionAction{OneShot: b.OneShot}
	switch b.Thread {
	case wsm.SlackReactionAnnounced:
		action.Thread = &agentreplv1.SlackReactionAction_Announced{Announced: &agentreplv1.SlackReactionActionAnnounced{}}
	case wsm.SlackReactionSubscribed:
		action.Thread = &agentreplv1.SlackReactionAction_Subscribed{Subscribed: &agentreplv1.SlackReactionActionSubscribed{Mode: ModeProto(b.Mode)}}
	default:
		panic(fmt.Sprintf("slackwatch: a binding of %q relates to its thread as %q", b.Emoji, b.Thread))
	}
	if b.Priority != nil {
		action.Priority = priorityProto(*b.Priority)
	}
	return &agentreplv1.SlackReactionBinding{Emoji: b.Emoji, Action: action}
}

// SettingsProto spells S, its repository at REPODIR.
func SettingsProto(s wsm.SlackReactionSettings, repoDir string) *agentreplv1.SlackReactionSettings {
	out := &agentreplv1.SlackReactionSettings{Baseline: &agentreplv1.SlackReactionSettings_Pending{Pending: &agentreplv1.SlackReactionBaselinePending{}}}
	if s.WatchUser != "" {
		out.WatchUser = &agentreplv1.SlackReactionWatchUser{UserId: s.WatchUser}
	}
	if s.RepoID != "" {
		out.Repository = &workspacev1.RepositoryRef{Id: s.RepoID, Dir: repoDir}
	}
	for _, b := range s.Bindings {
		out.Bindings = append(out.Bindings, BindingProto(b))
	}
	if s.BaselinedAt != nil {
		out.Baseline = &agentreplv1.SlackReactionSettings_Recorded{Recorded: &agentreplv1.SlackReactionBaselineRecorded{At: timestamppb.New(*s.BaselinedAt)}}
	}
	return out
}

// PollsProto spells P.
func PollsProto(p PollState) *agentreplv1.GetSlackStatusReactionPolls {
	switch {
	case len(p.Missing) > 0:
		return &agentreplv1.GetSlackStatusReactionPolls{Polls: &agentreplv1.GetSlackStatusReactionPolls_Inactive{Inactive: &agentreplv1.GetSlackStatusReactionPollsInactive{Missing: p.Missing}}}
	case !p.Polled:
		return &agentreplv1.GetSlackStatusReactionPolls{Polls: &agentreplv1.GetSlackStatusReactionPolls_Starting{Starting: &agentreplv1.GetSlackStatusReactionPollsStarting{}}}
	case p.Failure != "":
		return &agentreplv1.GetSlackStatusReactionPolls{Polls: &agentreplv1.GetSlackStatusReactionPolls_Failed{Failed: &agentreplv1.GetSlackStatusReactionPollsFailed{At: timestamppb.New(p.At), Detail: p.Failure}}}
	}
	return &agentreplv1.GetSlackStatusReactionPolls{Polls: &agentreplv1.GetSlackStatusReactionPolls_Succeeded{Succeeded: &agentreplv1.GetSlackStatusReactionPollsSucceeded{At: timestamppb.New(p.At), Fired: uint32(p.Fired)}}}
}

// LoginProto spells L.
func LoginProto(l Login) *agentreplv1.GetSlackStatusLogin {
	if l.Username != "" {
		return &agentreplv1.GetSlackStatusLogin{Login: &agentreplv1.GetSlackStatusLogin_Known{Known: &agentreplv1.GetSlackStatusLoginKnown{Username: l.Username}}}
	}
	return &agentreplv1.GetSlackStatusLogin{Login: &agentreplv1.GetSlackStatusLogin_Failed{Failed: &agentreplv1.GetSlackStatusLoginFailed{Detail: l.Failure}}}
}

// MessageProto spells M.
func MessageProto(m Message) *agentreplv1.SlackThreadMessage {
	return &agentreplv1.SlackThreadMessage{Ts: m.TS, Author: m.Author, Text: m.Text, OwnPost: m.OwnPost}
}

