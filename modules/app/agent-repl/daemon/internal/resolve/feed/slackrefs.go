package feed

import (
	"agent-repld/internal/slackrefs"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// SlackChannelSource answers the user's Slack channels' ids by name
// (slackchannels.Directory.ChannelIDs), nil while none are known yet. It
// never waits on Slack.
type SlackChannelSource interface {
	ChannelIDs() map[string]string
}

// linkChannels rewrites every drawn markdown of PARSED with each
// `#<channel-name>` the user's Slack channels name linked to its permalink
// (slackrefs.Linkify). The channels are whatever the source knows now: a text
// drawn before the first read finishes keeps its channels plain. A resolver
// with no source or no subdomain links none. The source is left as written.
// Only the text's end can still grow while it is ARRIVING.
func (r *resolver) linkChannels(parsed *frontendv1.ParsedProse, arriving bool) {
	if r.deps.SlackChannels == nil || r.deps.SlackSubdomain == "" {
		return
	}
	channels := slackrefs.Channels{Subdomain: r.deps.SlackSubdomain, IDs: r.deps.SlackChannels.ChannelIDs()}
	if len(channels.IDs) == 0 {
		return
	}
	eachDrawnMarkdown(parsed, true, func(markdown string, ends bool) string {
		return slackrefs.Linkify(markdown, channels, arriving && ends)
	})
}
