package slackchannels

import (
	"context"
	"errors"
	"strings"
	"testing"

	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/gnsclient"
)

// fakeLister answers one channel list, as gnsclient would.
type fakeLister struct {
	channels []gnsclient.Channel
	err      error
	calls    int
}

func (l *fakeLister) ListMyChannels(context.Context) ([]gnsclient.Channel, error) {
	l.calls++
	return l.channels, l.err
}

func TestGNSReaderReadsChannels(t *testing.T) {
	// Arrange.
	lister := &fakeLister{channels: []gnsclient.Channel{
		{ID: "C1", Name: "general", Purpose: "  Company-wide announcements.  "},
		{ID: "C2", Name: "team-secret", IsPrivate: true},
		{ID: "C3", Name: "old", IsArchived: true},
		{ID: "D4", Name: "someone", IsIM: true},
		{ID: "G5", Name: "mpdm-a--b", IsMPIM: true},
	}}

	// Act.
	got, err := GNSReader(lister)(context.Background())

	// Assert.
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	purpose := "Company-wide announcements."
	want := []*agentreplv1.SlackChannel{
		{Id: "C1", Name: "general", Purpose: &purpose, Visibility: &agentreplv1.SlackChannel_PublicChannel{PublicChannel: &agentreplv1.SlackChannelPublic{}}},
		{Id: "C2", Name: "team-secret", Visibility: &agentreplv1.SlackChannel_PrivateChannel{PrivateChannel: &agentreplv1.SlackChannelPrivate{}}},
	}
	if lister.calls != 1 || len(got) != len(want) {
		t.Fatalf("listed %d times; channels = %v, want %v", lister.calls, got, want)
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
		lister  *fakeLister
		wantErr string
	}{
		{"the list fails", &fakeLister{err: &gnsclient.Failure{Call: "gns slack channel list-mine --all --json", Message: "HTTP 401: Invalid or expired JWT"}},
			"gns could not list the channels: gnsclient: gns slack channel list-mine --all --json: HTTP 401: Invalid or expired JWT"},
		{"nameless channel", &fakeLister{channels: []gnsclient.Channel{{ID: "C1"}}}, "gns answered a channel without an id or a name"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got, err := GNSReader(tc.lister)(context.Background())

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

func TestGNSReaderKeepsTheListersFailure(t *testing.T) {
	// Arrange.
	failure := &gnsclient.Failure{Call: "gns slack channel list-mine --all --json", Message: "gns did not answer within 40s"}

	// Act.
	_, err := GNSReader(&fakeLister{err: failure})(context.Background())

	// Assert.
	var got *gnsclient.Failure
	if !errors.As(err, &got) || got != failure {
		t.Fatalf("err = %v, want it to wrap gnsclient's failure", err)
	}
}

func TestReadBoundIsGnsOwnChannelsBound(t *testing.T) {
	if ReadBound != gnsclient.ChannelsBound {
		t.Fatalf("ReadBound = %s, want gnsclient.ChannelsBound %s", ReadBound, gnsclient.ChannelsBound)
	}
}
