package slackpost

import (
	"context"
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"

	"agent-repld/internal/slackfmt"
)

func TestParseThread(t *testing.T) {
	tests := []struct {
		name string
		link string
		want Thread
	}{
		{
			"a root message is its own thread",
			"https://chesscom.slack.com/archives/C0123ABC/p1730000000123456",
			Thread{Channel: "C0123ABC", TS: "1730000000.123456"},
		},
		{
			"a reply posts into its root's thread",
			"https://chesscom.slack.com/archives/C0123ABC/p1730000099000001?thread_ts=1730000000.123456&cid=C0123ABC",
			Thread{Channel: "C0123ABC", TS: "1730000000.123456"},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got, err := ParseThread(tc.link)

			// Assert.
			tc.want.URL = tc.link
			if err != nil || got != tc.want {
				t.Fatalf("ParseThread(%q) = (%+v, %v), want %+v", tc.link, got, err, tc.want)
			}
		})
	}
}

func TestParseThreadRefuses(t *testing.T) {
	tests := []struct {
		name     string
		link     string
		fragment string
	}{
		{"a plain http link", "http://chesscom.slack.com/archives/C0123ABC/p1730000000123456", "not an https link"},
		{"another host", "https://example.com/archives/C0123ABC/p1730000000123456", "not an https link"},
		{"a channel link", "https://chesscom.slack.com/archives/C0123ABC", "does not link a message"},
		{"not a url", "://", "is not a url"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			_, err := ParseThread(tc.link)

			// Assert.
			if err == nil || !strings.Contains(err.Error(), tc.fragment) {
				t.Fatalf("ParseThread(%q) = %v, want a refusal naming %q", tc.link, err, tc.fragment)
			}
		})
	}
}

var thread = Thread{URL: "https://w.slack.com/archives/C1/p1730000000123456", Channel: "C1", TS: "1730000000.123456"}

// recordedRun is one call a fake Runner saw.
type recordedRun struct {
	name  string
	args  []string
	stdin string
}

func fakeRunner(out string, err error, calls *[]recordedRun) Runner {
	return func(_ context.Context, name string, args []string, stdin []byte) (string, error) {
		*calls = append(*calls, recordedRun{name: name, args: args, stdin: string(stdin)})
		return out, err
	}
}

func TestReplySendsBlocksIntoTheThread(t *testing.T) {
	// Arrange.
	var calls []recordedRun
	p := New(Config{Run: fakeRunner("sent https://w.slack.com/archives/C1/p1730000001000000 ok", nil, &calls)})

	// Act.
	link, err := p.Reply(context.Background(), thread, "- [The fix](https://example.com/a)\n")

	// Assert.
	if err != nil || link != "https://w.slack.com/archives/C1/p1730000001000000" {
		t.Fatalf("Reply() = (%q, %v), want the permalink", link, err)
	}
	wantArgs := []string{"slack", "send", "--channel", "C1", "--thread", "1730000000.123456", "--no-unfurl", "--blocks", "-"}
	if len(calls) != 1 || calls[0].name != "gns" || !reflect.DeepEqual(calls[0].args, wantArgs) {
		t.Fatalf("calls = %+v, want one gns send with %q", calls, wantArgs)
	}
	var blocks []map[string]any
	if err := json.Unmarshal([]byte(calls[0].stdin), &blocks); err != nil || len(blocks) != 1 || blocks[0]["type"] != "rich_text" {
		t.Fatalf("stdin = %q (%v), want one rich_text block", calls[0].stdin, err)
	}
}

func TestReplySendsAMentionAsMrkdwn(t *testing.T) {
	// Arrange.
	var calls []recordedRun
	p := New(Config{Gns: "/opt/gns", Run: fakeRunner("", nil, &calls)})

	// Act.
	link, err := p.Reply(context.Background(), thread, "ping <@U012ABCDEF> now\n")

	// Assert.
	if err != nil || link != "" {
		t.Fatalf("Reply() = (%q, %v), want no permalink and no error", link, err)
	}
	if len(calls) != 1 || calls[0].name != "/opt/gns" || calls[0].args[len(calls[0].args)-2] != "--message" || calls[0].stdin != "ping <@U012ABCDEF> now" {
		t.Fatalf("calls = %+v, want one mrkdwn send through the configured gns", calls)
	}
}

func TestReplyRefusesABodyThatIsNotSlackNative(t *testing.T) {
	// Arrange.
	var calls []recordedRun
	p := New(Config{Run: fakeRunner("", nil, &calls)})

	// Act.
	_, err := p.Reply(context.Background(), thread, "• hand drawn\n")

	// Assert.
	var refused *slackfmt.NotSlackNativeError
	if !errors.As(err, &refused) || len(calls) != 0 {
		t.Fatalf("Reply() = %v after %d sends, want a refusal and nothing sent", err, len(calls))
	}
}

func TestReplyAnswersSlacksRejection(t *testing.T) {
	// Arrange.
	var calls []recordedRun
	p := New(Config{Run: fakeRunner("", errors.New("not_in_channel"), &calls)})

	// Act.
	_, err := p.Reply(context.Background(), thread, "Work on this has begun.\n")

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "Slack rejected the reply") || !strings.Contains(err.Error(), "not_in_channel") {
		t.Fatalf("Reply() = %v, want Slack's rejection", err)
	}
}

// fakeGns writes an executable script standing in for gns.
func fakeGns(t *testing.T, script string) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "gns")
	if err := os.WriteFile(path, []byte("#!/bin/sh\n"+script), 0o700); err != nil {
		t.Fatalf("WriteFile() = %v", err)
	}
	return path
}

func TestRunCommandAnswersStdoutAndFeedsStdin(t *testing.T) {
	// Arrange.
	gns := fakeGns(t, `printf 'args:%s ' "$*"; cat`)

	// Act.
	out, err := RunCommand(context.Background(), gns, []string{"slack", "send"}, []byte("body"))

	// Assert.
	if err != nil || out != "args:slack send body" {
		t.Fatalf("RunCommand() = (%q, %v), want the args and the stdin echoed", out, err)
	}
}

func TestRunCommandCarriesStderrOnFailure(t *testing.T) {
	// Arrange.
	gns := fakeGns(t, "echo 'invalid_auth' >&2\nexit 3\n")

	// Act.
	_, err := RunCommand(context.Background(), gns, nil, nil)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "invalid_auth") {
		t.Fatalf("RunCommand() = %v, want the failure with its stderr", err)
	}
}

func TestRunCommandAnswersACommandThatCannotStart(t *testing.T) {
	// Act.
	_, err := RunCommand(context.Background(), filepath.Join(t.TempDir(), "absent"), nil, nil)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "start") {
		t.Fatalf("RunCommand() = %v, want a start failure", err)
	}
}
