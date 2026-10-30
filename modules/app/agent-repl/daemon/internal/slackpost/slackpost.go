// Package slackpost is how the daemon posts into a Slack thread on its own:
// the reply a workspace created for a Slack message makes when its work
// begins, and the one it makes when its work ends. No agent writes or sends
// these; the daemon composes each body and posts it here.
//
// EVERY BODY IS FORMATTED BY slackfmt, the port of the send-to-slack skill's
// formatter, so a daemon post reads exactly as a hand-sent one does: Block
// Kit with real nested lists, links labelled, pull requests as `#<number>`.
// A body slackfmt refuses as not Slack-native is never sent.
//
// THE POST GOES THROUGH `gns slack send`, as the signed-in user, with link
// previews suppressed (`--no-unfurl`), as every send-to-slack post is.
package slackpost

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"net/url"
	"regexp"
	"strings"

	"agent-repld/internal/childproc"
	"agent-repld/internal/slackfmt"
)

// Thread is a Slack thread a post replies into.
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
		return Thread{}, fmt.Errorf("slackpost: %q is not a url: %w", link, err)
	}
	if u.Scheme != "https" || !strings.HasSuffix(u.Hostname(), ".slack.com") {
		return Thread{}, fmt.Errorf("slackpost: %q is not an https link on a slack.com workspace", link)
	}
	m := messagePath.FindStringSubmatch(u.Path)
	if m == nil {
		return Thread{}, fmt.Errorf("slackpost: %q does not link a message (/archives/<channel>/p<timestamp>)", link)
	}
	ts := m[2][:len(m[2])-6] + "." + m[2][len(m[2])-6:]
	if root := u.Query().Get("thread_ts"); root != "" {
		ts = root
	}
	return Thread{URL: link, Channel: m[1], TS: ts}, nil
}

// Runner runs one command with STDIN on its standard input and answers its
// standard output; a command that fails answers an error carrying its
// standard error. Injected so no test runs the real gns.
type Runner func(ctx context.Context, name string, args []string, stdin []byte) (stdout string, err error)

// Poster replies into Slack threads.
type Poster interface {
	// Reply renders MARKDOWN and posts it into THREAD, answering the post's
	// permalink, empty when gns printed none. A body slackfmt refuses is a
	// *slackfmt.NotSlackNativeError and nothing is sent.
	Reply(ctx context.Context, thread Thread, markdown string) (string, error)
}

// Config builds a Poster.
type Config struct {
	// Gns is the gns executable; empty is `gns`, looked up on PATH.
	Gns string
	// Run runs it; nil runs it as a child process (RunCommand).
	Run Runner
	// Renderer formats every body.
	Renderer slackfmt.Renderer
}

// New builds a Poster.
func New(cfg Config) Poster {
	p := &gnsPoster{gns: cfg.Gns, run: cfg.Run, renderer: cfg.Renderer}
	if p.gns == "" {
		p.gns = "gns"
	}
	if p.run == nil {
		p.run = RunCommand
	}
	return p
}

type gnsPoster struct {
	gns      string
	run      Runner
	renderer slackfmt.Renderer
}

// permalinkRE finds a Slack permalink in gns's answer.
var permalinkRE = regexp.MustCompile(`https://[A-Za-z0-9.-]+\.slack\.com/archives/[^\s")]+`)

// Reply implements Poster.
func (p *gnsPoster) Reply(ctx context.Context, thread Thread, markdown string) (string, error) {
	payload, err := p.renderer.Render(markdown)
	if err != nil {
		return "", fmt.Errorf("slackpost: render the reply into %s: %w", thread.URL, err)
	}
	args, stdin, err := SendArgs(thread, payload)
	if err != nil {
		return "", err
	}
	out, err := p.run(ctx, p.gns, args, stdin)
	if err != nil {
		return "", fmt.Errorf("slackpost: Slack rejected the reply into %s: %w", thread.URL, err)
	}
	return permalinkRE.FindString(out), nil
}

// SendArgs is the `gns slack send` argument list and standard input that post
// PAYLOAD into THREAD: Block Kit or mrkdwn read from stdin, previews
// suppressed.
func SendArgs(thread Thread, payload slackfmt.Payload) ([]string, []byte, error) {
	args := []string{"slack", "send", "--channel", thread.Channel, "--thread", thread.TS, "--no-unfurl"}
	if payload.Blocks == nil {
		return append(args, "--message", "-"), []byte(payload.Text), nil
	}
	blocks, err := json.Marshal(payload.Blocks)
	if err != nil {
		return nil, nil, fmt.Errorf("slackpost: encode the reply's blocks: %w", err)
	}
	return append(args, "--blocks", "-"), blocks, nil
}

// RunCommand runs NAME as a child process, in childproc.Stable, with STDIN on
// its standard input. A failure carries the child's standard error.
func RunCommand(ctx context.Context, name string, args []string, stdin []byte) (string, error) {
	cmd := childproc.CommandContext(ctx, childproc.Stable, name, args...)
	cmd.Stdin = bytes.NewReader(stdin)
	stdout, stderr, waitErr, err := childproc.Run(cmd, nil)
	if err != nil {
		return "", fmt.Errorf("capture %s's output: %w", name, errors.Join(waitErr, err))
	}
	if waitErr != nil && cmd.Process == nil {
		return "", fmt.Errorf("start %s: %w", name, waitErr)
	}
	if waitErr != nil {
		return "", fmt.Errorf("%s exited: %w: %s", name, waitErr, strings.TrimSpace(stderr))
	}
	return stdout, nil
}
