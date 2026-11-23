package main

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"google.golang.org/protobuf/proto"
)

// slackRefusals is each Slack-thread verb's describer with its cases.
var slackRefusals = func() []describedRefusal {
	subscribe := &agentreplv1.SubscribeSlackThreadError{}
	unsubscribe := &agentreplv1.UnsubscribeSlackThreadError{}
	mode := &agentreplv1.SetSlackThreadModeError{}
	subscriptions := &agentreplv1.ListSlackThreadSubscriptionsError{}
	read := &agentreplv1.ReadSlackThreadError{}
	post := &agentreplv1.PostToSlackThreadError{}
	reactions := &agentreplv1.UpdateSlackReactionSettingsError{}
	const noWorkspace = "the daemon has no workspace ws"
	return []describedRefusal{
		{name: "slack subscribe", empty: subscribe, oneof: "cause",
			cases: []refusalCase{
				{name: "unknown_subscriber", err: armed(subscribe, "unknown_subscriber", nil), want: noWorkspace},
				{name: "subscriber_closed", err: armed(subscribe, "subscriber_closed", nil), want: "ws is closed, so it follows no thread"},
				{name: "unreachable", err: armedWith(subscribe, "unreachable", map[string]any{"detail": "timed out"}),
					want: "the thread could not be followed: Slack could not be reached through gns (timed out)"},
			},
			describe: func(e proto.Message) (string, error) {
				return describeSlackSubscribeRefusal("ws", e.(*agentreplv1.SubscribeSlackThreadError))
			}},
		{name: "slack unsubscribe", empty: unsubscribe, oneof: "cause",
			cases: []refusalCase{{name: "unknown_subscriber", err: armed(unsubscribe, "unknown_subscriber", nil), want: noWorkspace}},
			describe: func(e proto.Message) (string, error) {
				return describeSlackUnsubscribeRefusal("ws", e.(*agentreplv1.UnsubscribeSlackThreadError))
			}},
		{name: "slack mode", empty: mode, oneof: "cause",
			cases: []refusalCase{
				{name: "unknown_subscriber", err: armed(mode, "unknown_subscriber", nil), want: noWorkspace},
				{name: "not_subscribed", err: armed(mode, "not_subscribed", nil), want: "ws does not follow the thread; slack-subscribe it first"},
			},
			describe: func(e proto.Message) (string, error) {
				return describeSlackModeRefusal("ws", e.(*agentreplv1.SetSlackThreadModeError))
			}},
		{name: "slack subscriptions", empty: subscriptions, oneof: "cause",
			cases: []refusalCase{{name: "unknown_subscriber", err: armed(subscriptions, "unknown_subscriber", nil), want: noWorkspace}},
			describe: func(e proto.Message) (string, error) {
				return describeSlackSubscriptionsRefusal("ws", e.(*agentreplv1.ListSlackThreadSubscriptionsError))
			}},
		{name: "slack read", empty: read, oneof: "cause",
			cases: []refusalCase{
				{name: "unknown_reader", err: armed(read, "unknown_reader", nil), want: noWorkspace},
				{name: "unreachable", err: armed(read, "unreachable", nil), want: "the thread could not be read: Slack could not be reached through gns"},
			},
			describe: func(e proto.Message) (string, error) {
				return describeSlackReadRefusal("ws", e.(*agentreplv1.ReadSlackThreadError))
			}},
		{name: "slack post", empty: post, oneof: "cause",
			cases: []refusalCase{
				{name: "unknown_poster", err: armed(post, "unknown_poster", nil), want: noWorkspace},
				{name: "not_subscribed", err: armed(post, "not_subscribed", nil), want: "ws does not follow the thread; it posts only in threads it follows"},
				{name: "no_emoji_prefix", err: armed(post, "no_emoji_prefix", nil), want: "a post opens with exactly one emoji shortcode and a space (`:white_check_mark: tests pass`)"},
				{name: "reserved_emoji", err: armedWith(post, "reserved_emoji", map[string]any{"emoji": ":peepo_wave:"}), want: ":peepo_wave: is reserved for the daemon's own posts; open with another emoji"},
				{name: "listen_mode", err: armed(post, "listen_mode", nil), want: "ws follows the thread in listen mode, which posts nothing; draft the reply for the user, or -force once they approve it"},
				{name: "post_limit_reached", err: armed(post, "post_limit_reached", &agentreplv1.PostToSlackThreadPostLimitReached{Posts: &agentreplv1.SlackThreadPosts{Made: 20, Limit: 20}}),
					want: "ws made 20 of its 20 posts in the thread and now listens; ask the user before posting again with -force"},
				{name: "secret", err: armed(post, "secret", &agentreplv1.PostToSlackThreadSecret{Kinds: []string{"api key", "password"}}), want: "the post carries what reads as a secret (api key, password); nothing was posted"},
				{name: "unreachable", err: armedWith(post, "unreachable", map[string]any{"detail": "401"}), want: "nothing was posted: Slack could not be reached through gns (401)"},
			},
			describe: func(e proto.Message) (string, error) {
				return describeSlackPostRefusal("ws", e.(*agentreplv1.PostToSlackThreadError))
			}},
		{name: "slack reactions", empty: reactions, oneof: "cause",
			cases: []refusalCase{
				{name: "unknown_repository", err: armed(reactions, "unknown_repository", nil), want: "the daemon has no repository by that directory; register it, or name another"},
				{name: "not_bound", err: armedWith(reactions, "not_bound", map[string]any{"emoji": "agent-ack"}), want: ":agent-ack: is bound to nothing, so there is nothing to unbind"},
			},
			describe: func(e proto.Message) (string, error) {
				return describeSlackReactionsRefusal(e.(*agentreplv1.UpdateSlackReactionSettingsError))
			}},
	}
}()

func TestSlackRefusalsAreDescribed(t *testing.T) {
	testDescribedRefusals(t, slackRefusals)
}
