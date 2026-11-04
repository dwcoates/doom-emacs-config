// Package slacklink reads a Slack message link: the message a workspace was
// created to act on (agentrepl.v1.CreateWorkspaceSlackOrigin), whose link the
// create request carries and the daemon validates before anything is made.
// The daemon itself never posts to Slack: what a thread is told is its
// repository's hooks' to say (repoconfig.v1.LifecycleConfig).
package slacklink

import (
	"fmt"
	"net/url"
	"regexp"
	"strings"
)

// Thread is a Slack message link, read.
type Thread struct {
	// URL is the message link the thread was named by, verbatim.
	URL string
	// Channel is the channel id the thread is in.
	Channel string
	// TS is the thread's ROOT message timestamp: a link to a reply carries
	// its root as `thread_ts`, and a link to a root message is its own.
	TS string
}

// messagePath matches a Slack message link's path: `/archives/<channel>/p<ts
// digits>`, where the digits are the timestamp with its dot removed.
var messagePath = regexp.MustCompile(`^/archives/([A-Z0-9]+)/p([0-9]{7,})/?$`)

// ParseThread reads a Slack message link into the thread a reply to it goes
// into. Anything but an https link to a message on a slack.com workspace is
// refused, naming why.
func ParseThread(link string) (Thread, error) {
	u, err := url.Parse(link)
	if err != nil {
		return Thread{}, fmt.Errorf("slacklink: %q is not a url: %w", link, err)
	}
	if u.Scheme != "https" || !strings.HasSuffix(u.Hostname(), ".slack.com") {
		return Thread{}, fmt.Errorf("slacklink: %q is not an https link on a slack.com workspace", link)
	}
	m := messagePath.FindStringSubmatch(u.Path)
	if m == nil {
		return Thread{}, fmt.Errorf("slacklink: %q does not link a message (/archives/<channel>/p<timestamp>)", link)
	}
	ts := m[2][:len(m[2])-6] + "." + m[2][len(m[2])-6:]
	if root := u.Query().Get("thread_ts"); root != "" {
		ts = root
	}
	return Thread{URL: link, Channel: m[1], TS: ts}, nil
}
