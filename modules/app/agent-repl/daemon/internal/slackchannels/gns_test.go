package slackchannels

import (
	"context"
	"errors"
	"slices"
	"strings"
	"testing"
	"time"

	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// fakeRunner answers one run and records what it was asked to run.
type fakeRunner struct {
	output string
	code   int
	err    error
	// block makes the run wait for its context to end, as a hung gns does.
	block bool
	dir   string
	argv  []string
}

func (r *fakeRunner) Run(ctx context.Context, dir string, argv []string) (string, int, error) {
	r.dir, r.argv = dir, argv
	if r.block {
		<-ctx.Done()
		return "", -1, ctx.Err()
	}
	return r.output, r.code, r.err
}

func TestGNSReaderRunsListMine(t *testing.T) {
	// Arrange.
	run := &fakeRunner{output: `{"channels": [], "count": 0}`}

	// Act.
	_, err := GNSReader(run, "/home")(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	want := []string{"gns", "slack", "channel", "list-mine", "--all", "--json"}
	if run.dir != "/home" || !slices.Equal(run.argv, want) {
		t.Fatalf("ran %v in %q, want %v in /home", run.argv, run.dir, want)
	}
}

func TestGNSReaderReadsChannels(t *testing.T) {
	// Arrange.
	run := &fakeRunner{output: `{"channels": [
		{"id": "C1", "name": "general", "is_private": false, "purpose": "  Company-wide announcements.  "},
		{"id": "C2", "name": "team-secret", "is_private": true, "purpose": ""},
		{"id": "C3", "name": "old", "is_archived": true},
		{"id": "D4", "name": "someone", "is_im": true},
		{"id": "G5", "name": "mpdm-a--b", "is_mpim": true}
	], "count": 5}`}

	// Act.
	got, err := GNSReader(run, "/home")(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	purpose := "Company-wide announcements."
	want := []*agentreplv1.SlackChannel{
		{Id: "C1", Name: "general", Purpose: &purpose, Visibility: &agentreplv1.SlackChannel_PublicChannel{PublicChannel: &agentreplv1.SlackChannelPublic{}}},
		{Id: "C2", Name: "team-secret", Visibility: &agentreplv1.SlackChannel_PrivateChannel{PrivateChannel: &agentreplv1.SlackChannelPrivate{}}},
	}
	if len(got) != len(want) {
		t.Fatalf("channels = %v, want %v", got, want)
	}
	for i := range want {
		if !proto.Equal(got[i], want[i]) {
			t.Fatalf("channel %d = %v, want %v", i, got[i], want[i])
		}
	}
}

func TestGNSReaderFailures(t *testing.T) {
	cases := []struct {
		name    string
		run     *fakeRunner
		wantErr string
	}{
		{"spawn failure", &fakeRunner{err: errors.New("exec: \"gns\": not found")}, `gns could not be run: exec: "gns": not found`},
		{"non-zero exit", &fakeRunner{code: 1, output: "Error: HTTP 401: Invalid or expired JWT\n"}, "gns exited 1: Error: HTTP 401: Invalid or expired JWT"},
		{"not json", &fakeRunner{output: "Channels:\n  general\n"}, "gns answered something that is not its channel list"},
		{"no channels list", &fakeRunner{output: `{"count": 0}`}, "gns answered no `channels` list"},
		{"nameless channel", &fakeRunner{output: `{"channels": [{"id": "C1"}]}`}, "gns answered a channel without an id or a name"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got, err := GNSReader(tc.run, "/home")(context.Background())

			// Assert.
			if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
				t.Fatalf("err = %v, want one containing %q", err, tc.wantErr)
			}
			if got != nil {
				t.Fatalf("channels = %v alongside an error, want none", got)
			}
		})
	}
}

func TestGNSReaderNamesItsBoundWhenItRunsPastIt(t *testing.T) {
	// Arrange.
	run := &fakeRunner{block: true}
	ctx, cancel := context.WithDeadline(context.Background(), time.Unix(0, 0))
	defer cancel()

	// Act.
	_, err := GNSReader(run, "/home")(ctx)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "gns ran past its 40s bound") {
		t.Fatalf("err = %v, want the bound named", err)
	}
}
