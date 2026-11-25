package gnsclient

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"reflect"
	"strconv"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

// SLACK IS NEVER CALLED: every run is a fake `gns` that writes a scripted
// answer and records the arguments and stdin it was given.

// fakeGNS writes a `gns` that records its arguments one per line and its
// stdin, prints STDOUT, writes STDERR and exits CODE; a non-empty TAIL runs
// after the output (`exec tail -f /dev/null`, to block until killed). It answers the binary
// and its directory.
func fakeGNS(t *testing.T, stdout, stderr string, code int, tail string) (string, string) {
	t.Helper()
	dir := t.TempDir()
	out, errOut := filepath.Join(dir, "out"), filepath.Join(dir, "err")
	for path, text := range map[string]string{out: stdout, errOut: stderr} {
		if err := os.WriteFile(path, []byte(text), 0o600); err != nil {
			t.Fatal(err)
		}
	}
	script := "#!/bin/sh\nfor a in \"$@\"; do printf '%s\\n' \"$a\" >> '" + filepath.Join(dir, "args") + "'; done\n"
	if tail == "" {
		script += "cat > '" + filepath.Join(dir, "stdin") + "'\n"
	}
	script += "cat '" + out + "'\ncat '" + errOut + "' >&2\n" + tail + "\nexit " + strconv.Itoa(code) + "\n"
	bin := filepath.Join(dir, "gns")
	if err := os.WriteFile(bin, []byte(script), 0o700); err != nil {
		t.Fatal(err)
	}
	return bin, dir
}

func newClient(t *testing.T, bin string, bound time.Duration) (Client, *dlog.TestLogger) {
	t.Helper()
	log := dlog.NewTestLogger()
	c, err := New(Config{Binary: bin, Log: log, Bound: bound})
	if err != nil {
		t.Fatal(err)
	}
	return c, log
}

func argsOf(t *testing.T, dir string) []string {
	t.Helper()
	b, err := os.ReadFile(filepath.Join(dir, "args"))
	if err != nil {
		t.Fatal(err)
	}
	return strings.Split(strings.TrimSuffix(string(b), "\n"), "\n")
}

// failedRecord reports whether LOG holds the one failure record of CALL.
func failedRecord(log *dlog.TestLogger, call string) bool {
	r, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: op, Message: "a gns call failed"})
	return ok && strings.HasPrefix(r.Context["call"].(string), call)
}

func TestNewRefusesNoLog(t *testing.T) {
	if _, err := New(Config{}); err == nil {
		t.Fatal("New accepted no Log")
	}
}

func TestWhoami(t *testing.T) {
	tests := []struct {
		name    string
		out     string
		code    int
		want    string
		wantErr bool
	}{
		{name: "a banner before the json", out: "  Username: dodge\n{\"username\":\"dodge\",\"email\":\"d@x\"}", want: "dodge"},
		{name: "no username", out: `{"email":"d@x"}`, wantErr: true},
		{name: "not json", out: "hello", wantErr: true},
		{name: "gns fails", code: 1, wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			bin, _ := fakeGNS(t, tt.out, "not logged in", tt.code, "")
			c, log := newClient(t, bin, 0)

			// Act
			got, err := c.Whoami(context.Background())

			// Assert
			if got != tt.want || (err != nil) != tt.wantErr {
				t.Fatalf("Whoami = %q, %v; want %q (error %v)", got, err, tt.want, tt.wantErr)
			}
			var f *Failure
			if tt.wantErr && (!errors.As(err, &f) || !failedRecord(log, "gns whoami")) {
				t.Fatalf("the failure %v was not answered and recorded once: %+v", err, log.Records())
			}
		})
	}
}

func TestAFailedCallAnswersWhatGnsSaid(t *testing.T) {
	// Arrange
	bin, _ := fakeGNS(t, "", "Error: Invalid or expired JWT", 1, "")
	c, _ := newClient(t, bin, 0)

	// Act
	err := c.SubscribeChannel(context.Background(), "C1")

	// Assert
	var f *Failure
	if !errors.As(err, &f) || f.Message != "Error: Invalid or expired JWT" || f.Call != "gns slack subscribe --channel C1" {
		t.Fatalf("SubscribeChannel = %#v", err)
	}
}

func TestACallThatOutrunsItsBoundFails(t *testing.T) {
	// Arrange
	bin, _ := fakeGNS(t, "", "", 0, "exec tail -f /dev/null")
	c, log := newClient(t, bin, 50*time.Millisecond)

	// Act
	_, _, err := c.PipeHead(context.Background(), "p")

	// Assert
	var f *Failure
	if !errors.As(err, &f) || !strings.Contains(f.Message, "did not answer within 50ms") || !failedRecord(log, "gns pipe read p") {
		t.Fatalf("PipeHead = %v; want the bound's failure recorded: %+v", err, log.Records())
	}
}

func TestThreadRoot(t *testing.T) {
	tests := []struct {
		name    string
		out     string
		want    string
		wantErr bool
	}{
		{name: "a reply names its root", out: `{"messages":[{"timestamp":"1.5","thread_ts":"1.1"}]}`, want: "1.1"},
		{name: "a message in no thread is its own root", out: `{"messages":[{"timestamp":"1.5"}]}`, want: "1.5"},
		{name: "a bare list", out: `[{"ts":"1.5","thread_ts":"1.2"}]`, want: "1.2"},
		{name: "no message", out: `{"messages":[]}`, wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			bin, dir := fakeGNS(t, tt.out, "", 0, "")
			c, _ := newClient(t, bin, 0)

			// Act
			got, err := c.ThreadRoot(context.Background(), "C1", "1.5")

			// Assert
			if got != tt.want || (err != nil) != tt.wantErr {
				t.Fatalf("ThreadRoot = %q, %v; want %q", got, err, tt.want)
			}
			if want := []string{"slack", "thread", "C1", "1.5", "--limit", "1", "--output", "json"}; !reflect.DeepEqual(argsOf(t, dir), want) {
				t.Fatalf("args = %v", argsOf(t, dir))
			}
		})
	}
}

func TestPipeHead(t *testing.T) {
	tests := []struct {
		name       string
		out        string
		stderr     string
		code       int
		want       int64
		wantExists bool
		wantErr    bool
	}{
		{name: "an empty pipe", out: `[]`, wantExists: true},
		{name: "a pipe's newest entry", out: `[{"seq":42,"message":"x"}]`, want: 42, wantExists: true},
		{name: "a pipe gns keeps none of", stderr: "Error: Pipe not found: user.dodge.tools.slack.channel.D1.echo [PIPE_NOT_FOUND]", code: 1},
		{name: "any other refusal", stderr: "Error: Invalid or expired JWT", code: 1, wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			bin, _ := fakeGNS(t, tt.out, tt.stderr, tt.code, "")
			c, log := newClient(t, bin, 0)

			// Act
			got, exists, err := c.PipeHead(context.Background(), PipeFor("dodge", "C1"))

			// Assert
			if got != tt.want || exists != tt.wantExists || (err != nil) != tt.wantErr {
				t.Fatalf("PipeHead = %d, %v, %v; want %d, %v (error %v)", got, exists, err, tt.want, tt.wantExists, tt.wantErr)
			}
			if failed := failedRecord(log, "gns pipe read"); failed != tt.wantErr {
				t.Fatalf("failure recorded %v; want %v: %+v", failed, tt.wantErr, log.Records())
			}
		})
	}
}

func TestAPipeGnsKeepsNoneOfIsRecordedAsAnAnswer(t *testing.T) {
	// Arrange
	bin, _ := fakeGNS(t, "", "Error: Pipe not found: p [PIPE_NOT_FOUND]", 1, "")
	c, log := newClient(t, bin, 0)

	// Act
	_, _, err := c.PipeHead(context.Background(), "p")

	// Assert
	r, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: op, Message: "gns keeps no such pipe"})
	if err != nil || !ok || r.Context["pipe"] != "p" || r.Context["call"] != "gns pipe read p --tail 1" {
		t.Fatalf("PipeHead = %v; records %+v", err, log.Records())
	}
}

func TestThreadAfter(t *testing.T) {
	tests := []struct {
		name    string
		out     string
		want    []ThreadMessage
		wantErr bool
	}{
		{
			name: "the root and every message up to the bound are dropped",
			out:  `{"messages":[{"ts":"1700000000.000100","user":"U1"},{"ts":"1700000000.000200","user":"U1"},{"ts":"1700000001.000050","thread_ts":"1700000000.000100","user_name":"alice","text":"hi"}]}`,
			want: []ThreadMessage{{TS: "1700000001.000050", ThreadTS: "1700000000.000100", Author: "alice", Text: "hi"}},
		},
		{name: "nothing later", out: `{"messages":[{"ts":"1700000000.000100"}]}`},
		{name: "a message with no Slack timestamp", out: `{"messages":[{"ts":"soon"}]}`, wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			bin, dir := fakeGNS(t, tt.out, "", 0, "")
			c, log := newClient(t, bin, 0)

			// Act
			got, err := c.ThreadAfter(context.Background(), "D1", "1700000000.000100", "1700000000.000200")

			// Assert
			if (err != nil) != tt.wantErr || !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("ThreadAfter = %+v, %v; want %+v (error %v)", got, err, tt.want, tt.wantErr)
			}
			if tt.wantErr && !failedRecord(log, "gns slack thread D1 1700000000.000100 --oldest 1700000000.000200") {
				t.Fatalf("the failure was not recorded: %+v", log.Records())
			}
			if want := []string{"slack", "thread", "D1", "1700000000.000100", "--all", "--oldest", "1700000000.000200", "--output", "json"}; !reflect.DeepEqual(argsOf(t, dir), want) {
				t.Fatalf("args = %v", argsOf(t, dir))
			}
		})
	}
}

func TestTSAfter(t *testing.T) {
	tests := []struct {
		name    string
		a, b    string
		want    bool
		wantErr bool
	}{
		{name: "a later second", a: "1700000001.000000", b: "1700000000.999999", want: true},
		{name: "a later microsecond", a: "1700000000.000002", b: "1700000000.000001", want: true},
		{name: "the same timestamp", a: "1700000000.000001", b: "1700000000.000001"},
		{name: "an earlier one", a: "1700000000.000001", b: "1700000000.000002"},
		{name: "a longer seconds part compares by value", a: "10000000000.000000", b: "9999999999.000000", want: true},
		{name: "no fraction", a: "1700000000", b: "1700000000.000001", wantErr: true},
		{name: "a short fraction", a: "1700000000.1", b: "1700000000.000001", wantErr: true},
		{name: "not a number", a: "1700000000.000001", b: "x.000001", wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := TSAfter(tt.a, tt.b)

			// Assert
			if got != tt.want || (err != nil) != tt.wantErr {
				t.Fatalf("TSAfter(%q, %q) = %v, %v; want %v (error %v)", tt.a, tt.b, got, err, tt.want, tt.wantErr)
			}
		})
	}
}

// pipeLines is a stream of three entries, a banner and an entry with no
// message timestamp.
const pipeLines = "following...\n" +
	`{"seq":5,"message":"hi","tags":{"channel":"C1","message_ts":"1.2","thread_ts":"1.1","user":"U1","user_name":"alice"}}` + "\n" +
	`{"seq":6,"message":"meta","tags":{}}` + "\n" +
	`{"seq":7,"message":"yo","tags":{"channel":"C1","message_ts":"1.3","thread_ts":"1.1","user":"U2"}}` + "\n"

func TestStreamReadsEveryMessageInOrderAndAnswersTheLastSequence(t *testing.T) {
	// Arrange
	bin, dir := fakeGNS(t, pipeLines, "", 0, "")
	c, _ := newClient(t, bin, 0)
	var got []PipeMessage

	// Act
	last, err := c.Stream(context.Background(), "p", 4, "1.1", func(m PipeMessage) { got = append(got, m) })

	// Assert
	want := []PipeMessage{{Seq: 5, TS: "1.2", ThreadTS: "1.1", Author: "alice", Text: "hi"}, {Seq: 7, TS: "1.3", ThreadTS: "1.1", Author: "U2", Text: "yo"}}
	if err != nil || last != 7 || !reflect.DeepEqual(got, want) {
		t.Fatalf("Stream = %d, %v, %+v", last, err, got)
	}
	if args := strings.Join(argsOf(t, dir), " "); args != "pipe read p --follow --after-seq 4 --output json --tag thread_ts:1.1" {
		t.Fatalf("args = %q", args)
	}
}

func TestAStreamItsCallerEndsIsNoFailure(t *testing.T) {
	// Arrange
	bin, _ := fakeGNS(t, pipeLines, "", 0, "exec tail -f /dev/null")
	c, log := newClient(t, bin, 0)
	ctx, cancel := context.WithCancel(context.Background())

	// Act
	last, err := c.Stream(ctx, "p", 0, "1.1", func(m PipeMessage) {
		if m.Seq == 7 {
			cancel()
		}
	})

	// Assert
	if err != nil || last != 7 {
		t.Fatalf("Stream = %d, %v; want a normal end at 7", last, err)
	}
	if len(log.Records()) != 0 {
		t.Fatalf("a normal end was recorded: %+v", log.Records())
	}
}

func TestAStreamThatFailsAnswersWhatItReadAndTheFailure(t *testing.T) {
	// Arrange
	bin, _ := fakeGNS(t, pipeLines, "Pipe not found", 1, "")
	c, log := newClient(t, bin, 0)

	// Act
	last, err := c.Stream(context.Background(), "p", 0, "1.1", func(PipeMessage) {})

	// Assert
	var f *Failure
	if last != 7 || !errors.As(err, &f) || f.Message != "Pipe not found" || !failedRecord(log, "gns pipe read p") {
		t.Fatalf("Stream = %d, %v", last, err)
	}
}

func TestPost(t *testing.T) {
	tests := []struct {
		name string
		out  string
		want string
	}{
		{name: "the posted timestamp", out: `{"ok":true,"ts":"1.9"}`, want: "1.9"},
		{name: "an answer in words", out: "Message sent", want: ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			bin, dir := fakeGNS(t, tt.out, "", 0, "")
			c, _ := newClient(t, bin, 0)

			// Act
			got, err := c.Post(context.Background(), "C1", "1.1", []byte(`[{"type":"section"}]`))

			// Assert
			if err != nil || got != tt.want {
				t.Fatalf("Post = %q, %v; want %q", got, err, tt.want)
			}
			stdin, _ := os.ReadFile(filepath.Join(dir, "stdin"))
			if string(stdin) != `[{"type":"section"}]` || strings.Join(argsOf(t, dir), " ") != "slack send --channel C1 --thread 1.1 --blocks - --output json" {
				t.Fatalf("args %v, stdin %q", argsOf(t, dir), stdin)
			}
		})
	}
}

func TestSearchName(t *testing.T) {
	tests := []struct {
		name    string
		out     string
		want    string
		wantErr bool
	}{
		{name: "the account name", out: `{"name":"dodge","profile":{"display_name_normalized":"Dodge"}}`, want: "dodge"},
		{name: "the display name when no account name", out: `{"profile":{"display_name_normalized":"Dodge"}}`, want: "Dodge"},
		{name: "neither", out: `{}`, wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			bin, _ := fakeGNS(t, tt.out, "", 0, "")
			c, _ := newClient(t, bin, 0)

			got, err := c.SearchName(context.Background(), "U1")

			if got != tt.want || (err != nil) != tt.wantErr {
				t.Fatalf("SearchName = %q, %v", got, err)
			}
		})
	}
}

func TestSearchReadsAPageAndCountsThePages(t *testing.T) {
	// Arrange
	bin, dir := fakeGNS(t, `{"messages":[{"channel":{"id":"C1"},"ts":"1.2","text":":agent-ack: do it","permalink":"https://x.slack.com/archives/C1/p12"}],"total":150}`, "", 0, "")
	c, _ := newClient(t, bin, 0)

	// Act
	got, err := c.Search(context.Background(), "from:@dodge", 2)

	// Assert
	want := SearchPage{Pages: 2, Messages: []SearchMessage{{Channel: "C1", TS: "1.2", Text: ":agent-ack: do it", Permalink: "https://x.slack.com/archives/C1/p12"}}}
	if err != nil || !reflect.DeepEqual(got, want) {
		t.Fatalf("Search = %+v, %v", got, err)
	}
	if strings.Join(argsOf(t, dir), " ") != "slack search from:@dodge --count 100 --page 2 --json" {
		t.Fatalf("args = %v", argsOf(t, dir))
	}
}

func TestListReactedKeepsTheUsersOwnReactionsOnMessages(t *testing.T) {
	// Arrange
	out := `{"ok":true,"items":[
	  {"channel":"C1","message":{"ts":"1.2","text":"t","permalink":"https://x.slack.com/archives/C1/p12","reactions":[{"name":"agent-ack","users":["U2","U1"]},{"name":"eyes","users":["U2"]}]}},
	  {"channel":"C1","message":{"ts":"1.3","reactions":[{"name":"eyes","users":["U2"]}]}},
	  {"type":"file","file":{"id":"F1"}}]}`
	bin, _ := fakeGNS(t, out, "", 0, "")
	c, _ := newClient(t, bin, 0)

	// Act
	got, err := c.ListReacted(context.Background(), "U1")

	// Assert
	want := []ReactedMessage{{Channel: "C1", TS: "1.2", Text: "t", Permalink: "https://x.slack.com/archives/C1/p12", Emojis: []string{"agent-ack"}}}
	if err != nil || !reflect.DeepEqual(got, want) {
		t.Fatalf("ListReacted = %+v, %v", got, err)
	}
}

func TestListReactedRefusesSlacksRefusal(t *testing.T) {
	// Arrange
	bin, _ := fakeGNS(t, `{"ok":false,"error":"invalid_auth"}`, "", 0, "")
	c, log := newClient(t, bin, 0)

	// Act
	_, err := c.ListReacted(context.Background(), "U1")

	// Assert
	if err == nil || !failedRecord(log, "gns slack reactions list") {
		t.Fatalf("ListReacted = %v; want Slack's refusal recorded", err)
	}
}

func TestListMyChannelsReadsEveryChannelInSlacksOrder(t *testing.T) {
	// Arrange
	bin, dir := fakeGNS(t, `{"channels": [
		{"id": "C1", "name": "general", "purpose": "news"},
		{"id": "C2", "name": "team", "is_private": true},
		{"id": "C3", "name": "old", "is_archived": true}
	], "count": 3}`, "", 0, "")
	c, _ := newClient(t, bin, 0)

	// Act
	got, err := c.ListMyChannels(context.Background())

	// Assert
	want := []Channel{{ID: "C1", Name: "general", Purpose: "news"}, {ID: "C2", Name: "team", IsPrivate: true}, {ID: "C3", Name: "old", IsArchived: true}}
	if err != nil || !reflect.DeepEqual(got, want) {
		t.Fatalf("ListMyChannels = %+v, %v", got, err)
	}
	if args := argsOf(t, dir); !reflect.DeepEqual(args, []string{"slack", "channel", "list-mine", "--all", "--json"}) {
		t.Fatalf("ran gns %v", args)
	}
}

func TestListMyChannelsRefusesAnAnswerWithNoChannelsList(t *testing.T) {
	// Arrange
	bin, _ := fakeGNS(t, `{"count": 0}`, "", 0, "")
	c, log := newClient(t, bin, 0)

	// Act
	_, err := c.ListMyChannels(context.Background())

	// Assert
	var f *Failure
	if !errors.As(err, &f) || !strings.Contains(f.Message, "no `channels` list") || !failedRecord(log, "gns slack channel list-mine --all --json") {
		t.Fatalf("ListMyChannels = %v; want the missing list recorded", err)
	}
}

func TestListMyChannelsRunsUnderItsOwnBound(t *testing.T) {
	// Arrange: the one-shot bound is far longer, so only the channels bound
	// can end the call.
	bin, _ := fakeGNS(t, "", "", 0, "exec tail -f /dev/null")
	log := dlog.NewTestLogger()
	c, err := New(Config{Binary: bin, Log: log, Bound: time.Hour, ChannelsBound: 50 * time.Millisecond})
	if err != nil {
		t.Fatal(err)
	}

	// Act
	_, err = c.ListMyChannels(context.Background())

	// Assert
	var f *Failure
	if !errors.As(err, &f) || !strings.Contains(f.Message, "did not answer within 50ms (healthy calls answer within 15.4s)") {
		t.Fatalf("ListMyChannels = %v; want the channels bound named", err)
	}
}

// observedClient is a client whose observer records every output it is shown.
func observedClient(t *testing.T, bin string) (Client, *[]string) {
	t.Helper()
	var shown []string
	c, err := New(Config{Binary: bin, Log: dlog.NewTestLogger(), Observe: func(output string) bool {
		shown = append(shown, output)
		return false
	}})
	if err != nil {
		t.Fatal(err)
	}
	return c, &shown
}

func TestTheObserverIsShownEveryCallGnsRefused(t *testing.T) {
	tests := []struct {
		name   string
		stdout string
		stderr string
		code   int
		want   []string
	}{
		{name: "a refused call shows its combined output", stdout: "Visit: x\n", stderr: "Error: credential is invalid or expired", code: 1,
			want: []string{"Visit: x\nError: credential is invalid or expired"}},
		{name: "an answered call shows nothing", stdout: `{"ok":true}`, code: 0, want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			bin, _ := fakeGNS(t, tt.stdout, tt.stderr, tt.code, "")
			c, shown := observedClient(t, bin)

			// Act
			_ = c.SubscribeChannel(context.Background(), "C1")

			// Assert
			if !reflect.DeepEqual(*shown, tt.want) {
				t.Fatalf("observer shown %q, want %q", *shown, tt.want)
			}
		})
	}
}

func TestTheObserverIsShownAFailedStreamsErrors(t *testing.T) {
	// Arrange
	bin, _ := fakeGNS(t, pipeLines, "Error: credential is invalid or expired", 1, "")
	c, shown := observedClient(t, bin)

	// Act
	_, _ = c.Stream(context.Background(), "p", 0, "1.1", func(PipeMessage) {})

	// Assert
	if !reflect.DeepEqual(*shown, []string{"Error: credential is invalid or expired"}) {
		t.Fatalf("observer shown %q, want the stream's errors", *shown)
	}
}
