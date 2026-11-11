package main

import (
	"flag"
	"strings"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/slacklink"
	"agent-repld/internal/slackwatch"
	"agent-repld/internal/wsm"
)

// The Slack commands: a workspace's thread subscriptions and posts
// (`workspace slack-*`), and the daemon's Slack status and reaction
// initiator settings (`slack ...`).

// slackNoun is the daemon-wide Slack commands' noun.
const slackNoun = "slack"

// slackModes is how a mode flag is spelled.
const slackModes = "listen|respond|interact"

// slackThreadFlags declares the -thread every thread command names.
func slackThreadFlag(fs *flag.FlagSet) *string {
	return fs.String("thread", "", "the thread, as a Slack message link")
}

// threadOf checks a -thread link, refusing one that is no Slack message link
// before anything is dialed.
func (c *cliCall) threadOf(url string) bool {
	if _, err := slacklink.ParseThread(url); err != nil {
		c.usageFailed("-thread is a Slack message link: %v", err)
		return false
	}
	return true
}

// modeFlag reads a -mode.
func (c *cliCall) modeFlag(text string) (*agentreplv1.SlackThreadMode, bool) {
	mode, ok := slackwatch.ParseMode(text)
	if !ok {
		c.usageFailed("-mode is %s, not %q", slackModes, text)
		return nil, false
	}
	return slackwatch.ModeProto(mode), true
}

// slackThreadArgs parses a thread command's WS and -thread, and dials.
func (c *cliCall) slackThreadArgs(fs *flag.FlagSet, stateDir *string, thread *string, args []string) (*workspacev1.WorkspaceRef, bool) {
	ws, rest, ok := c.workspaceArg(fs, stateDir, args)
	if !ok {
		return nil, false
	}
	if len(rest) != 0 {
		c.usageFailed("unexpected arguments %v", rest)
		return nil, false
	}
	return ws, c.threadOf(*thread)
}

func workspaceSlackSubscribe(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	thread := slackThreadFlag(fs)
	modeText := fs.String("mode", "", "what the workspace may do in the thread: "+slackModes)
	ws, ok := c.slackThreadArgs(fs, stateDir, thread, args)
	if !ok {
		return exitFailure
	}
	mode, ok := c.modeFlag(*modeText)
	if !ok {
		return exitFailure
	}
	resp, err := c.client.SubscribeSlackThread(c.ctx, connect.NewRequest(&agentreplv1.SubscribeSlackThreadRequest{Subscriber: ws, ThreadUrl: *thread, Mode: mode}))
	return answer(c, resp, err)
}

func workspaceSlackUnsubscribe(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	thread := slackThreadFlag(fs)
	ws, ok := c.slackThreadArgs(fs, stateDir, thread, args)
	if !ok {
		return exitFailure
	}
	resp, err := c.client.UnsubscribeSlackThread(c.ctx, connect.NewRequest(&agentreplv1.UnsubscribeSlackThreadRequest{Subscriber: ws, ThreadUrl: *thread}))
	return answer(c, resp, err)
}

func workspaceSlackMode(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	thread := slackThreadFlag(fs)
	modeText := fs.String("mode", "", "the new mode: "+slackModes)
	ws, ok := c.slackThreadArgs(fs, stateDir, thread, args)
	if !ok {
		return exitFailure
	}
	mode, ok := c.modeFlag(*modeText)
	if !ok {
		return exitFailure
	}
	resp, err := c.client.SetSlackThreadMode(c.ctx, connect.NewRequest(&agentreplv1.SetSlackThreadModeRequest{Subscriber: ws, ThreadUrl: *thread, Mode: mode}))
	return answer(c, resp, err)
}

func workspaceSlackSubscriptions(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	ws, rest, ok := c.workspaceArg(fs, stateDir, args)
	if !ok || len(rest) != 0 {
		return exitFailure
	}
	resp, err := c.client.ListSlackThreadSubscriptions(c.ctx, connect.NewRequest(&agentreplv1.ListSlackThreadSubscriptionsRequest{Subscriber: ws}))
	return answer(c, resp, err)
}

func workspaceSlackTail(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	thread := slackThreadFlag(fs)
	limit := fs.Uint("limit", 0, "at most how many of the newest messages (1 to 200; the daemon's default when unset)")
	ws, ok := c.slackThreadArgs(fs, stateDir, thread, args)
	if !ok {
		return exitFailure
	}
	req := &agentreplv1.ReadSlackThreadRequest{Reader: ws, ThreadUrl: *thread}
	if *limit != 0 {
		n := uint32(*limit)
		req.Limit = &n
	}
	resp, err := c.client.ReadSlackThread(c.ctx, connect.NewRequest(req))
	return answer(c, resp, err)
}

func workspaceSlackPost(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	thread := slackThreadFlag(fs)
	text := fs.String("text", "", "the post, opening with one emoji shortcode and a space (`:white_check_mark: tests pass`)")
	force := fs.Bool("force", false, "the user explicitly approved this post: it goes through listen mode and the post limit")
	ws, ok := c.slackThreadArgs(fs, stateDir, thread, args)
	if !ok {
		return exitFailure
	}
	if strings.TrimSpace(*text) == "" {
		return c.usageFailed("-text is the post")
	}
	resp, err := c.client.PostToSlackThread(c.ctx, connect.NewRequest(&agentreplv1.PostToSlackThreadRequest{Poster: ws, ThreadUrl: *thread, Text: *text, Force: *force}))
	return answer(c, resp, err)
}

func slackStatus(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	if rest, ok := c.parse(fs, args); !ok || len(rest) != 0 || !c.dial(*stateDir) {
		return exitFailure
	}
	resp, err := c.client.GetSlackStatus(c.ctx, connect.NewRequest(&agentreplv1.GetSlackStatusRequest{}))
	return answer(c, resp, err)
}

// updateSlackReactions sends one settings change.
func (c *cliCall) updateSlackReactions(req *agentreplv1.UpdateSlackReactionSettingsRequest) int {
	resp, err := c.client.UpdateSlackReactionSettings(c.ctx, connect.NewRequest(req))
	return answer(c, resp, err)
}

// oneSlackPositional parses ARGS to the one positional the command names, and
// dials.
func (c *cliCall) oneSlackPositional(fs *flag.FlagSet, stateDir *string, args []string, what string) (string, bool) {
	rest, ok := c.parse(fs, args)
	if !ok {
		return "", false
	}
	if len(rest) != 1 {
		c.usageFailed("the command names %s", what)
		return "", false
	}
	return rest[0], c.dial(*stateDir)
}

func slackWatchUser(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	user, ok := c.oneSlackPositional(fs, stateDir, args, "the Slack member id (U...) whose messages and reactions trigger")
	if !ok {
		return exitFailure
	}
	return c.updateSlackReactions(&agentreplv1.UpdateSlackReactionSettingsRequest{Change: &agentreplv1.UpdateSlackReactionSettingsRequest_WatchUser{
		WatchUser: &agentreplv1.SlackReactionWatchUser{UserId: user}}})
}

func slackRepository(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	name, ok := c.oneSlackPositional(fs, stateDir, args, "the repository fired workspaces are created in: here or a directory")
	if !ok {
		return exitFailure
	}
	repository, ok := c.repository(name)
	if !ok {
		return exitFailure
	}
	return c.updateSlackReactions(&agentreplv1.UpdateSlackReactionSettingsRequest{Change: &agentreplv1.UpdateSlackReactionSettingsRequest_Repository{Repository: repository}})
}

func slackBind(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	thread := fs.String("thread", "", "how the workspace relates to the thread: announced (its repository's emoji-ack hooks tell the thread) or subscribed (it is subscribed and converses there)")
	modeText := fs.String("mode", "", "a subscribed workspace's mode: "+slackModes)
	oneShot := fs.Bool("one-shot", false, "the workspace is a one-shot, which lands its own work when done")
	priorityText := fs.String("priority", "", "the workspace's priority: p05, p1, p2 or p3")
	emoji, ok := c.oneSlackPositional(fs, stateDir, args, "the emoji's name, without colons")
	if !ok {
		return exitFailure
	}
	action := &agentreplv1.SlackReactionAction{OneShot: *oneShot}
	switch *thread {
	case string(wsm.SlackReactionAnnounced):
		if *modeText != "" {
			return c.usageFailed("-mode is a subscribed workspace's; an announced one has none")
		}
		action.Thread = &agentreplv1.SlackReactionAction_Announced{Announced: &agentreplv1.SlackReactionActionAnnounced{}}
	case string(wsm.SlackReactionSubscribed):
		mode, ok := c.modeFlag(*modeText)
		if !ok {
			return exitFailure
		}
		action.Thread = &agentreplv1.SlackReactionAction_Subscribed{Subscribed: &agentreplv1.SlackReactionActionSubscribed{Mode: mode}}
	default:
		return c.usageFailed("-thread is announced or subscribed, not %q", *thread)
	}
	if *priorityText != "" {
		level, err := priorityOf(*priorityText)
		if err != nil || level == nil {
			return c.usageFailed("-priority is p05, p1, p2 or p3, not %q", *priorityText)
		}
		action.Priority = level
	}
	return c.updateSlackReactions(&agentreplv1.UpdateSlackReactionSettingsRequest{Change: &agentreplv1.UpdateSlackReactionSettingsRequest_Bind{
		Bind: &agentreplv1.SlackReactionBinding{Emoji: strings.Trim(emoji, ":"), Action: action}}})
}

func slackUnbind(c *cliCall, args []string) int {
	fs, stateDir := c.flags()
	emoji, ok := c.oneSlackPositional(fs, stateDir, args, "the emoji's name, without colons")
	if !ok {
		return exitFailure
	}
	return c.updateSlackReactions(&agentreplv1.UpdateSlackReactionSettingsRequest{Change: &agentreplv1.UpdateSlackReactionSettingsRequest_Unbind{Unbind: strings.Trim(emoji, ":")}})
}
