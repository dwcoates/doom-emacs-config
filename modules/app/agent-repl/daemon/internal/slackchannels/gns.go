package slackchannels

import (
	"context"
	"fmt"
	"strings"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/gnsclient"
)

// ChannelLister reads the GNS login's channels (gnsclient.Client), the
// daemon's one way to Slack.
type ChannelLister interface {
	ListMyChannels(ctx context.Context) ([]gnsclient.Channel, error)
}

// GNSReader reads the channels through LISTER. Its failures are recorded by
// gnsclient, and the read's by the Directory.
func GNSReader(lister ChannelLister) Reader {
	return func(ctx context.Context) ([]*agentreplv1.SlackChannel, error) {
		channels, err := lister.ListMyChannels(ctx)
		if err != nil {
			return nil, fmt.Errorf("gns could not list the channels: %w", err)
		}
		return completable(channels)
	}
}

// completable answers the channels a composer completes: archived channels,
// direct messages and group DMs are dropped, Slack's order is kept.
func completable(all []gnsclient.Channel) ([]*agentreplv1.SlackChannel, error) {
	channels := make([]*agentreplv1.SlackChannel, 0, len(all))
	for _, c := range all {
		if c.IsArchived || c.IsIM || c.IsMPIM {
			continue
		}
		if c.ID == "" || c.Name == "" {
			return nil, fmt.Errorf("gns answered a channel without an id or a name: %+v", c)
		}
		channel := &agentreplv1.SlackChannel{Id: c.ID, Name: c.Name}
		if c.IsPrivate {
			channel.Visibility = &agentreplv1.SlackChannel_PrivateChannel{PrivateChannel: &agentreplv1.SlackChannelPrivate{}}
		} else {
			channel.Visibility = &agentreplv1.SlackChannel_PublicChannel{PublicChannel: &agentreplv1.SlackChannelPublic{}}
		}
		if purpose := strings.TrimSpace(c.Purpose); purpose != "" {
			channel.Purpose = &purpose
		}
		channels = append(channels, channel)
	}
	return channels, nil
}
