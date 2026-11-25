// Package gnsclient is the daemon's ONE way to Slack: it runs the user's own
// `gns` command line, exactly the calls the gns-sockets plugin made, and
// answers what Slack said or a *Failure naming the call and what `gns` said.
//
// THE LOGIN NEVER PASSES THROUGH THE DAEMON. `gns` reads its own token;
// nothing here sees it, logs it or stores it.
//
// EVERY FAILURE IS RECORDED HERE, ONCE, the ownership point: a caller that
// propagates a *Failure does not record it again at error level.
//
// EVERY CALL IS BOUNDED. A one-shot call runs under CallBound; a stream runs
// until the context its caller hands it ends, which the caller bounds.
package gnsclient

import (
	"bufio"
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"slices"
	"sort"
	"strconv"
	"strings"
	"time"

	"agent-repld/internal/childproc"
	"agent-repld/internal/dlog"
)

// gnsBinary is the `gns` every call runs: "gns", found on PATH, unless the
// build named another. Test builds of the daemon link an unreachable path in
// here (`-ldflags -X agent-repld/internal/gnsclient.gnsBinary=<path>`), so a
// test daemon cannot reach Slack whatever its PATH holds.
var gnsBinary = "gns"

// Binary answers the `gns` executable this build runs.
func Binary() string { return gnsBinary }

// CallBound bounds every one-shot `gns` call. Measured 2026-11-10 from the
// gns-sockets daemon's own log: its startup poll's first dispatch came 9.0s
// after the watcher started, covering a `gns slack user get` and the first
// `gns slack search` page, the slowest healthy pair of one-shot calls on
// record; 25s is just under three times that.
const CallBound = 25 * time.Second

// ChannelsBound bounds the one slow one-shot call, the user's channel list.
// Measured 2026-11-19: `gns slack channel list-mine --all --json` answered
// 520 channels in 13.0s, 15.4s and 13.1s on a healthy host, so 40s is between
// two and three times the healthy read.
const ChannelsBound = 40 * time.Second

// Failure is a `gns` call that did not answer what was asked.
type Failure struct {
	// Call is the call, `gns` and its arguments.
	Call string
	// Message is what `gns` said on stderr, or why its answer is unreadable.
	Message string
}

func (f *Failure) Error() string { return fmt.Sprintf("gnsclient: %s: %s", f.Call, f.Message) }

// PipeMessage is one entry of a channel's echo pipe.
type PipeMessage struct {
	// Seq is the entry's sequence number in the pipe.
	Seq int64
	// TS is the Slack message's timestamp.
	TS string
	// ThreadTS is the root of the thread it is in.
	ThreadTS string
	// Author is who wrote it: the user's name when the pipe carries one,
	// else their id.
	Author string
	// Text is the message, in mrkdwn.
	Text string
}

// ThreadMessage is one message `gns slack thread` read.
type ThreadMessage struct {
	TS       string
	ThreadTS string
	Author   string
	Text     string
}

// SearchMessage is one `gns slack search` match.
type SearchMessage struct {
	Channel   string
	TS        string
	Text      string
	Permalink string
}

// SearchPage is one page of matches.
type SearchPage struct {
	Messages []SearchMessage
	// Pages is how many pages the search has; zero when Slack did not say.
	Pages int
}

// ReactedMessage is a message a user reacted to, with the emoji names of
// that user's own reactions on it.
type ReactedMessage struct {
	Channel   string
	TS        string
	Text      string
	Permalink string
	Emojis    []string
}

// Client is every Slack call the daemon makes.
type Client interface {
	// Whoami answers the GNS username, which names the user's echo pipes.
	Whoami(ctx context.Context) (string, error)
	// SubscribeChannel routes CHANNEL into the user's echo pipe; idempotent.
	SubscribeChannel(ctx context.Context, channel string) error
	// ThreadRoot answers the root of the thread the message at TS is in: TS
	// itself for a root or a message in no thread.
	ThreadRoot(ctx context.Context, channel, ts string) (string, error)
	// PipeHead answers the highest sequence number in PIPE.
	PipeHead(ctx context.Context, pipe string) (int64, error)
	// Stream reads PIPE's entries after AFTER that are in the thread rooted at
	// THREADTS, calling EACH on every one in order, until ctx ends, and
	// answers the highest sequence number read. ctx ending is the stream's
	// normal end, not a failure.
	Stream(ctx context.Context, pipe string, after int64, threadTS string, each func(PipeMessage)) (int64, error)
	// Thread reads up to LIMIT messages of the thread rooted at THREADTS.
	Thread(ctx context.Context, channel, threadTS string, limit int) ([]ThreadMessage, error)
	// Post posts BLOCKS (Slack Block Kit JSON) into the thread, answering the
	// posted message's timestamp, empty when `gns` did not say it.
	Post(ctx context.Context, channel, threadTS string, blocks []byte) (string, error)
	// SearchName answers the account name `from:@<name>` searches match.
	SearchName(ctx context.Context, userID string) (string, error)
	// Search reads one page (from 1) of QUERY's matches.
	Search(ctx context.Context, query string, page int) (SearchPage, error)
	// ListReacted reads the messages USERID reacted to, most recent first.
	ListReacted(ctx context.Context, userID string) ([]ReactedMessage, error)
	// ListMyChannels reads every channel the login's user is a member of,
	// every page, in Slack's order, under ChannelsBound.
	ListMyChannels(ctx context.Context) ([]Channel, error)
}

// Channel is one channel `gns slack channel list-mine` answers.
type Channel struct {
	ID         string `json:"id"`
	Name       string `json:"name"`
	IsPrivate  bool   `json:"is_private"`
	IsArchived bool   `json:"is_archived"`
	IsIM       bool   `json:"is_im"`
	IsMPIM     bool   `json:"is_mpim"`
	Purpose    string `json:"purpose"`
}

// Config is a client's collaborators.
type Config struct {
	// Binary is the `gns` to run; empty is Binary().
	Binary string
	// Log records every failure.
	Log dlog.Logger
	// Bound bounds every one-shot call; zero is CallBound. Only a test of the
	// bound itself sets it.
	Bound time.Duration
	// ChannelsBound bounds ListMyChannels; zero is ChannelsBound. Only a test
	// of the bound itself sets it.
	ChannelsBound time.Duration
	// Observe is shown the combined output of every call gns exits non-zero
	// on (gnsauth.Renewer.Observe, which renews a refused sign-in); nil when
	// nothing keeps the sign-in alive (a daemon with its vendors mocked, or
	// with no browser to sign in through).
	Observe func(output string) bool
}

// New builds a client.
func New(cfg Config) (Client, error) {
	if cfg.Log == nil {
		return nil, errors.New("gnsclient: Log is required")
	}
	if cfg.Binary == "" {
		cfg.Binary = gnsBinary
	}
	if cfg.Bound == 0 {
		cfg.Bound = CallBound
	}
	if cfg.ChannelsBound == 0 {
		cfg.ChannelsBound = ChannelsBound
	}
	return &client{cfg: cfg}, nil
}

type client struct {
	cfg Config
}

// PipeFor is the user's echo pipe for CHANNEL.
func PipeFor(username, channel string) string {
	return "user." + username + ".tools.slack.channel." + channel + ".echo"
}

// The operation every record of this package carries.
const op = "daemon.gnsclient.call"

// run runs `gns ARGS` under CallBound, with STDIN as its input when set, and
// answers its stdout.
func (c *client) run(ctx context.Context, stdin []byte, args ...string) ([]byte, error) {
	return c.runBounded(ctx, c.cfg.Bound, "9s", stdin, args...)
}

// runBounded is run under BOUND, a call whose healthy answer takes HEALTHY.
func (c *client) runBounded(ctx context.Context, bound time.Duration, healthy string, stdin []byte, args ...string) ([]byte, error) {
	ctx, cancel := context.WithTimeout(ctx, bound)
	defer cancel()
	cmd := childproc.CommandContext(ctx, childproc.Stable, c.cfg.Binary, args...)
	if stdin != nil {
		cmd.Stdin = bytes.NewReader(stdin)
	}
	cmd.Env = append(os.Environ(), "NO_COLOR=1")
	call := "gns " + strings.Join(args, " ")
	stdout, stderr, runErr, captureErr := childproc.Run(cmd, nil)
	switch {
	case captureErr != nil:
		return nil, c.failed(call, "could not capture gns's output: "+captureErr.Error())
	case errors.Is(ctx.Err(), context.DeadlineExceeded):
		return nil, c.failed(call, fmt.Sprintf("gns did not answer within %s (healthy calls answer within %s)", bound, healthy))
	case ctx.Err() != nil:
		// The daemon stopped the call: no fact about Slack.
		return nil, ctx.Err()
	case runErr != nil:
		c.refused(stdout + stderr)
		detail := strings.TrimSpace(stderr)
		if detail == "" {
			detail = runErr.Error()
		}
		return nil, c.failed(call, detail)
	}
	return []byte(stdout), nil
}

// refused shows OUTPUT, a call gns exited non-zero on, to the observer, when
// one is set.
func (c *client) refused(output string) {
	if c.cfg.Observe != nil {
		c.cfg.Observe(output)
	}
}

// failed records and answers a failure of CALL.
func (c *client) failed(call, message string) *Failure {
	f := &Failure{Call: call, Message: message}
	c.cfg.Log.Error(op, "a gns call failed", dlog.Context{"call": call, "cause": message})
	return f
}

// decode reads OUT as JSON into V, recording an unreadable answer as CALL's
// failure. Everything before the first `{` or `[` is a banner gns prints and
// is skipped.
func (c *client) decode(call string, out []byte, v any) error {
	if i := bytes.IndexAny(out, "{["); i > 0 {
		out = out[i:]
	}
	if err := json.Unmarshal(out, v); err != nil {
		return c.failed(call, fmt.Sprintf("its answer is not the JSON expected: %v (%.200s)", err, out))
	}
	return nil
}

func (c *client) Whoami(ctx context.Context) (string, error) {
	out, err := c.run(ctx, nil, "whoami", "--output", "json")
	if err != nil {
		return "", err
	}
	var who struct {
		Username string `json:"username"`
	}
	if err := c.decode("gns whoami", out, &who); err != nil {
		return "", err
	}
	if who.Username == "" {
		return "", c.failed("gns whoami", "it named no username")
	}
	return who.Username, nil
}

func (c *client) SubscribeChannel(ctx context.Context, channel string) error {
	_, err := c.run(ctx, nil, "slack", "subscribe", "--channel", channel)
	return err
}

func (c *client) ThreadRoot(ctx context.Context, channel, ts string) (string, error) {
	msgs, err := c.Thread(ctx, channel, ts, 1)
	if err != nil {
		return "", err
	}
	if len(msgs) == 0 {
		return "", c.failed("gns slack thread "+channel+" "+ts, "Slack holds no such message: it was deleted, or the login cannot see it")
	}
	if msgs[0].ThreadTS != "" {
		return msgs[0].ThreadTS, nil
	}
	return msgs[0].TS, nil
}

func (c *client) PipeHead(ctx context.Context, pipe string) (int64, error) {
	call := "gns pipe read " + pipe + " --tail 1"
	out, err := c.run(ctx, nil, "pipe", "read", pipe, "--tail", "1", "--output", "json")
	if err != nil {
		return 0, err
	}
	var entries []struct {
		Seq int64 `json:"seq"`
	}
	if err := c.decode(call, out, &entries); err != nil {
		return 0, err
	}
	if len(entries) == 0 {
		return 0, nil
	}
	return entries[0].Seq, nil
}

// pipeEntry is one `gns pipe read --output json` entry.
type pipeEntry struct {
	Seq     int64             `json:"seq"`
	Message string            `json:"message"`
	Tags    map[string]string `json:"tags"`
}

func (e pipeEntry) message() (PipeMessage, bool) {
	m := PipeMessage{Seq: e.Seq, TS: e.Tags["message_ts"], ThreadTS: e.Tags["thread_ts"], Author: e.Tags["user_name"], Text: e.Message}
	if m.Author == "" {
		m.Author = e.Tags["user"]
	}
	return m, m.TS != ""
}

func (c *client) Stream(ctx context.Context, pipe string, after int64, threadTS string, each func(PipeMessage)) (int64, error) {
	args := []string{"pipe", "read", pipe, "--follow", "--after-seq", strconv.FormatInt(after, 10), "--output", "json", "--tag", "thread_ts:" + threadTS}
	call := "gns " + strings.Join(args, " ")
	cmd := childproc.CommandContext(ctx, childproc.Stable, c.cfg.Binary, args...)
	cmd.Env = append(os.Environ(), "NO_COLOR=1")
	// ITS ERRORS ARE READ ONLY AFTER WAIT, so they go to a file Wait never
	// waits on (childproc.CaptureStderr).
	capture, err := childproc.CaptureStderr(cmd)
	if err != nil {
		return after, c.failed(call, "could not capture its errors: "+err.Error())
	}
	// A STREAM IS READ AS IT COMES, so its stdout is a pipe read to its end:
	// the scan below stops only at EOF, which the child's exit or kill gives.
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		return after, c.failed(call, errors.Join(errors.New("could not open its output: "+err.Error()), capture.Close()).Error())
	}
	if err := cmd.Start(); err != nil {
		return after, c.failed(call, errors.Join(errors.New("could not start: "+err.Error()), capture.Close()).Error())
	}
	last := after
	scanErr := scanEntries(stdout, func(e pipeEntry) {
		if e.Seq > last {
			last = e.Seq
		}
		if m, ok := e.message(); ok {
			each(m)
		}
	})
	waitErr := cmd.Wait()
	stderr, captureErr := capture.Finish()
	if captureErr != nil {
		return last, c.failed(call, "could not read its errors: "+captureErr.Error())
	}
	if ctx.Err() != nil {
		// The caller ended the window: the stream's normal end.
		return last, nil
	}
	if scanErr != nil {
		return last, c.failed(call, "could not read its output: "+scanErr.Error())
	}
	if waitErr != nil {
		// The stream's output went to its reader; its errors are what is left
		// to show.
		c.refused(stderr)
		detail := strings.TrimSpace(stderr)
		if detail == "" {
			detail = waitErr.Error()
		}
		return last, c.failed(call, detail)
	}
	return last, nil
}

// scanEntries calls EACH on every JSON line R carries, to R's end. A line
// that is not an entry (a banner) is skipped.
func scanEntries(r io.Reader, each func(pipeEntry)) error {
	sc := bufio.NewScanner(r)
	sc.Buffer(make([]byte, 64*1024), 1024*1024)
	for sc.Scan() {
		line := bytes.TrimSpace(sc.Bytes())
		if len(line) == 0 || line[0] != '{' {
			continue
		}
		var e pipeEntry
		if json.Unmarshal(line, &e) != nil {
			continue
		}
		each(e)
	}
	return sc.Err()
}

func (c *client) Thread(ctx context.Context, channel, threadTS string, limit int) ([]ThreadMessage, error) {
	return c.thread(ctx, "gns slack thread "+channel+" "+threadTS, "slack", "thread", channel, threadTS, "--limit", strconv.Itoa(limit), "--output", "json")
}

// thread runs the `gns slack thread` ARGS, CALL as its record names it, and
// answers the messages it read, oldest first.
func (c *client) thread(ctx context.Context, call string, args ...string) ([]ThreadMessage, error) {
	out, err := c.run(ctx, nil, args...)
	if err != nil {
		return nil, err
	}
	type raw struct {
		TS        string `json:"ts"`
		Timestamp string `json:"timestamp"`
		ThreadTS  string `json:"thread_ts"`
		User      string `json:"user"`
		UserName  string `json:"user_name"`
		Text      string `json:"text"`
	}
	// gns answers the thread as an object holding its messages, or as the
	// bare list.
	var shaped struct {
		Messages []raw `json:"messages"`
	}
	if trimmed := bytes.TrimSpace(out); len(trimmed) > 0 && trimmed[0] == '[' {
		if err := c.decode(call, out, &shaped.Messages); err != nil {
			return nil, err
		}
	} else if err := c.decode(call, out, &shaped); err != nil {
		return nil, err
	}
	var msgs []ThreadMessage
	for _, m := range shaped.Messages {
		ts := m.TS
		if ts == "" {
			ts = m.Timestamp
		}
		author := m.UserName
		if author == "" {
			author = m.User
		}
		msgs = append(msgs, ThreadMessage{TS: ts, ThreadTS: m.ThreadTS, Author: author, Text: m.Text})
	}
	sort.SliceStable(msgs, func(i, j int) bool { return msgs[i].TS < msgs[j].TS })
	return msgs, nil
}

func (c *client) Post(ctx context.Context, channel, threadTS string, blocks []byte) (string, error) {
	out, err := c.run(ctx, blocks, "slack", "send", "--channel", channel, "--thread", threadTS, "--blocks", "-", "--output", "json")
	if err != nil {
		return "", err
	}
	var sent struct {
		TS string `json:"ts"`
	}
	if !bytes.ContainsAny(out, "{") {
		// gns answered the post in words: it was made, and its time is
		// unknown.
		c.cfg.Log.Info(op, "gns posted and named no timestamp", dlog.Context{"channel": channel, "thread_ts": threadTS})
		return "", nil
	}
	if err := c.decode("gns slack send", out, &sent); err != nil {
		return "", err
	}
	return sent.TS, nil
}

func (c *client) SearchName(ctx context.Context, userID string) (string, error) {
	call := "gns slack user get " + userID
	out, err := c.run(ctx, nil, "slack", "user", "get", userID, "--json")
	if err != nil {
		return "", err
	}
	var user struct {
		Name    string `json:"name"`
		Profile struct {
			DisplayNameNormalized string `json:"display_name_normalized"`
		} `json:"profile"`
	}
	if err := c.decode(call, out, &user); err != nil {
		return "", err
	}
	if user.Name != "" {
		return user.Name, nil
	}
	if user.Profile.DisplayNameNormalized != "" {
		return user.Profile.DisplayNameNormalized, nil
	}
	return "", c.failed(call, "Slack named no account name for the user")
}

// searchPageSize is how many matches a search page asks for, Slack's most.
const searchPageSize = 100

func (c *client) Search(ctx context.Context, query string, page int) (SearchPage, error) {
	call := "gns slack search " + query
	out, err := c.run(ctx, nil, "slack", "search", query, "--count", strconv.Itoa(searchPageSize), "--page", strconv.Itoa(page), "--json")
	if err != nil {
		return SearchPage{}, err
	}
	var raw struct {
		Messages []struct {
			Channel struct {
				ID string `json:"id"`
			} `json:"channel"`
			TS        string `json:"ts"`
			Timestamp string `json:"timestamp"`
			Text      string `json:"text"`
			Permalink string `json:"permalink"`
		} `json:"messages"`
		Total int `json:"total"`
	}
	if err := c.decode(call, out, &raw); err != nil {
		return SearchPage{}, err
	}
	res := SearchPage{Pages: (raw.Total + searchPageSize - 1) / searchPageSize}
	for _, m := range raw.Messages {
		ts := m.TS
		if ts == "" {
			ts = m.Timestamp
		}
		res.Messages = append(res.Messages, SearchMessage{Channel: m.Channel.ID, TS: ts, Text: m.Text, Permalink: m.Permalink})
	}
	return res, nil
}

func (c *client) ListReacted(ctx context.Context, userID string) ([]ReactedMessage, error) {
	call := "gns slack reactions list --user " + userID
	// --full makes Slack name every reacting user on each reaction, so a
	// popular emoji's truncated list cannot hide the watch user.
	out, err := c.run(ctx, nil, "slack", "reactions", "list", "--user", userID, "--limit", "100", "--full", "--json")
	if err != nil {
		return nil, err
	}
	var raw struct {
		OK    bool `json:"ok"`
		Items []struct {
			Channel string `json:"channel"`
			Message *struct {
				TS        string `json:"ts"`
				Text      string `json:"text"`
				Permalink string `json:"permalink"`
				Reactions []struct {
					Name  string   `json:"name"`
					Users []string `json:"users"`
				} `json:"reactions"`
			} `json:"message"`
		} `json:"items"`
	}
	if err := c.decode(call, out, &raw); err != nil {
		return nil, err
	}
	if !raw.OK {
		return nil, c.failed(call, fmt.Sprintf("Slack answered ok=false (%.200s)", out))
	}
	var res []ReactedMessage
	for _, it := range raw.Items {
		// A file or a file comment carries no message, so it is no trigger.
		if it.Message == nil || it.Message.TS == "" {
			continue
		}
		m := ReactedMessage{Channel: it.Channel, TS: it.Message.TS, Text: it.Message.Text, Permalink: it.Message.Permalink}
		for _, r := range it.Message.Reactions {
			if slices.Contains(r.Users, userID) {
				m.Emojis = append(m.Emojis, r.Name)
			}
		}
		if len(m.Emojis) > 0 {
			res = append(res, m)
		}
	}
	return res, nil
}

func (c *client) ListMyChannels(ctx context.Context) ([]Channel, error) {
	args := []string{"slack", "channel", "list-mine", "--all", "--json"}
	out, err := c.runBounded(ctx, c.cfg.ChannelsBound, "15.4s", nil, args...)
	if err != nil {
		return nil, err
	}
	call := "gns " + strings.Join(args, " ")
	var answer struct {
		Channels *[]Channel `json:"channels"`
	}
	if err := c.decode(call, out, &answer); err != nil {
		return nil, err
	}
	if answer.Channels == nil {
		return nil, c.failed(call, "its answer holds no `channels` list")
	}
	return *answer.Channels, nil
}
