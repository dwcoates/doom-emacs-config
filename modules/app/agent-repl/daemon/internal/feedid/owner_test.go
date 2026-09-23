package feedid

import (
	"errors"
	"testing"
)

func TestAgentFeedPlacesOnlyTheMainAgentOnTheRoot(t *testing.T) {
	for _, tc := range []struct {
		name      string
		owner     string
		mainAgent string
		wantRoot  bool
		wantAgent string
		wantErr   error
	}{
		{name: "the main agent's work is on the root", owner: "main", mainAgent: "main", wantRoot: true},
		{name: "a subagent's work is on its own sub-feed", owner: "sub", mainAgent: "main", wantAgent: "sub"},
		{name: "an unknown owner is an error, never the root", owner: "", mainAgent: "main", wantErr: ErrOwnerUnknown},
		{name: "an unnamed main agent is an error, never the root", owner: "sub", mainAgent: "", wantErr: ErrMainAgentUnknown},
	} {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			feed, err := AgentFeed(tc.owner, tc.mainAgent)

			// Assert
			if tc.wantErr != nil {
				if !errors.Is(err, tc.wantErr) {
					t.Fatalf("err = %v, want %v", err, tc.wantErr)
				}
				if feed != (Feed{}) {
					t.Fatalf("feed = %+v, want no feed at all", feed)
				}
				return
			}
			if err != nil {
				t.Fatalf("err = %v, want none", err)
			}
			if feed.Root != tc.wantRoot || feed.Agent.GetValue() != tc.wantAgent {
				t.Fatalf("feed = {root %v agent %q}, want {root %v agent %q}",
					feed.Root, feed.Agent.GetValue(), tc.wantRoot, tc.wantAgent)
			}
		})
	}
}

func TestDetachedOwnerTakesEitherSourceAndRefusesADisagreement(t *testing.T) {
	for _, tc := range []struct {
		name    string
		stated  string
		carrier string
		want    string
		wantErr bool
	}{
		{name: "the stated owner answers alone", stated: "sub", want: "sub"},
		{name: "the carrier answers alone", carrier: "sub", want: "sub"},
		{name: "two agreeing sources answer", stated: "sub", carrier: "sub", want: "sub"},
		{name: "two disagreeing sources are refused", stated: "sub", carrier: "main", wantErr: true},
		{name: "no source is refused", wantErr: true},
	} {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := DetachedOwner(tc.stated, tc.carrier)

			// Assert
			if (err != nil) != tc.wantErr {
				t.Fatalf("err = %v, want error %v", err, tc.wantErr)
			}
			if got != tc.want {
				t.Fatalf("owner = %q, want %q", got, tc.want)
			}
		})
	}
}
