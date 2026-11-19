package server

import (
	"context"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
)

// SlackChannels answers what the daemon knows of the user's Slack channels,
// starting a read of them when one is due. Implemented by
// slackchannels.Directory, which owns the reads and logs their failures.
type SlackChannels interface {
	Response() *agentreplv1.ListSlackChannelsResponse
}

// ListSlackChannels answers the user's Slack channels for the composer's `#`
// completion. The list is the user's, so no workspace is resolved.
func (s *server) ListSlackChannels(
	ctx context.Context,
	req *connect.Request[agentreplv1.ListSlackChannelsRequest],
) (*connect.Response[agentreplv1.ListSlackChannelsResponse], error) {
	resp := s.deps.SlackChannels.Response()
	branch := "listed"
	switch {
	case resp.GetError() != nil:
		branch = "read_failed"
	case resp.GetSuccess().GetPending() != nil:
		branch = "pending"
	}
	s.log.Debug("daemon.server.list_slack_channels", "answered the Slack channels", dlog.Context{
		"branch":   branch,
		"channels": len(resp.GetSuccess().GetListed().GetChannels()),
	})
	return connect.NewResponse(resp), nil
}
