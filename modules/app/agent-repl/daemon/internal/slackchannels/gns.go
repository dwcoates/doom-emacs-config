package slackchannels

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"strings"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/outputtext"
)

// Runner runs argv in dir and answers its combined output and exit code; an
// error means the run could not be classified (scriptrunner.Runner).
type Runner interface {
	Run(ctx context.Context, dir string, argv []string) (string, int, error)
}

// gnsArgv is the read: every public and private channel the GNS login's user
// is a member of, every page, as JSON.
var gnsArgv = []string{"gns", "slack", "channel", "list-mine", "--all", "--json"}

// gnsChannel is one entry of `gns slack channel list-mine --json`'s answer,
// as far as the read needs it.
type gnsChannel struct {
	ID         string `json:"id"`
	Name       string `json:"name"`
	IsPrivate  bool   `json:"is_private"`
	IsArchived bool   `json:"is_archived"`
	IsIM       bool   `json:"is_im"`
	IsMPIM     bool   `json:"is_mpim"`
	Purpose    string `json:"purpose"`
}

// GNSReader reads the channels through the GNS CLI, run in dir (any
// directory; the read does not depend on it).
func GNSReader(run Runner, dir string) Reader {
	return func(ctx context.Context) ([]*agentreplv1.SlackChannel, error) {
		output, code, err := run.Run(ctx, dir, gnsArgv)
		if ctxErr := ctx.Err(); errors.Is(ctxErr, context.DeadlineExceeded) {
			return nil, fmt.Errorf("gns ran past its %s bound (a healthy read takes 13-15s)", ReadBound)
		}
		if err != nil {
			return nil, fmt.Errorf("gns could not be run: %w", err)
		}
		if code != 0 {
			return nil, fmt.Errorf("gns exited %d: %s", code, outputtext.LastLine(output))
		}
		return parseGNSChannels(output)
	}
}

// parseGNSChannels reads `list-mine --json`'s answer into the channels a
// composer completes: archived channels, direct messages and group DMs are
// dropped, Slack's order is kept.
func parseGNSChannels(output string) ([]*agentreplv1.SlackChannel, error) {
	var answer struct {
		Channels *[]gnsChannel `json:"channels"`
	}
	if err := json.Unmarshal([]byte(output), &answer); err != nil {
		return nil, fmt.Errorf("gns answered something that is not its channel list: %w", err)
	}
	if answer.Channels == nil {
		return nil, fmt.Errorf("gns answered no `channels` list")
	}
	channels := make([]*agentreplv1.SlackChannel, 0, len(*answer.Channels))
	for _, c := range *answer.Channels {
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
