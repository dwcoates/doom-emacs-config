package main

import (
	"context"
	"encoding/hex"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

func (c *cliClient) SubscribeSlackThread(_ context.Context, req *connect.Request[agentreplv1.SubscribeSlackThreadRequest]) (*connect.Response[agentreplv1.SubscribeSlackThreadResponse], error) {
	return connect.NewResponse(&agentreplv1.SubscribeSlackThreadResponse{}), c.record(req.Msg)
}

func (c *cliClient) UnsubscribeSlackThread(_ context.Context, req *connect.Request[agentreplv1.UnsubscribeSlackThreadRequest]) (*connect.Response[agentreplv1.UnsubscribeSlackThreadResponse], error) {
	return connect.NewResponse(&agentreplv1.UnsubscribeSlackThreadResponse{}), c.record(req.Msg)
}

func (c *cliClient) SetSlackThreadMode(_ context.Context, req *connect.Request[agentreplv1.SetSlackThreadModeRequest]) (*connect.Response[agentreplv1.SetSlackThreadModeResponse], error) {
	return connect.NewResponse(&agentreplv1.SetSlackThreadModeResponse{}), c.record(req.Msg)
}

func (c *cliClient) ListSlackThreadSubscriptions(_ context.Context, req *connect.Request[agentreplv1.ListSlackThreadSubscriptionsRequest]) (*connect.Response[agentreplv1.ListSlackThreadSubscriptionsResponse], error) {
	return connect.NewResponse(&agentreplv1.ListSlackThreadSubscriptionsResponse{}), c.record(req.Msg)
}

func (c *cliClient) ReadSlackThread(_ context.Context, req *connect.Request[agentreplv1.ReadSlackThreadRequest]) (*connect.Response[agentreplv1.ReadSlackThreadResponse], error) {
	return connect.NewResponse(&agentreplv1.ReadSlackThreadResponse{}), c.record(req.Msg)
}

func (c *cliClient) PostToSlackThread(_ context.Context, req *connect.Request[agentreplv1.PostToSlackThreadRequest]) (*connect.Response[agentreplv1.PostToSlackThreadResponse], error) {
	return connect.NewResponse(&agentreplv1.PostToSlackThreadResponse{}), c.record(req.Msg)
}

func (c *cliClient) GetSlackStatus(_ context.Context, req *connect.Request[agentreplv1.GetSlackStatusRequest]) (*connect.Response[agentreplv1.GetSlackStatusResponse], error) {
	return connect.NewResponse(&agentreplv1.GetSlackStatusResponse{}), c.record(req.Msg)
}

func (c *cliClient) UpdateSlackReactionSettings(_ context.Context, req *connect.Request[agentreplv1.UpdateSlackReactionSettingsRequest]) (*connect.Response[agentreplv1.UpdateSlackReactionSettingsResponse], error) {
	return connect.NewResponse(&agentreplv1.UpdateSlackReactionSettingsResponse{}), c.record(req.Msg)
}

// slackLink is the thread every command names.
const slackLink = "https://acme.slack.com/archives/C1/p1730000000123456"

func TestSlackWorkspaceVerbsSendTheirRequests(t *testing.T) {
	tests := []struct {
		name  string
		args  []string
		check func(t *testing.T, f *cliFixture)
	}{
		{name: "slack-subscribe sends the thread and its mode", args: []string{"slack-subscribe", "here", "-thread", slackLink, "-mode", "respond"},
			check: func(t *testing.T, f *cliFixture) {
				req := only[*agentreplv1.SubscribeSlackThreadRequest](t, f)
				if req.GetSubscriber().GetId() != "ws-caller" || req.GetThreadUrl() != slackLink || req.GetMode().GetRespond() == nil {
					t.Errorf("request = %v", req)
				}
			}},
		{name: "slack-close is slack-unsubscribe", args: []string{"slack-close", "here", "-thread", slackLink},
			check: func(t *testing.T, f *cliFixture) {
				if req := only[*agentreplv1.UnsubscribeSlackThreadRequest](t, f); req.GetThreadUrl() != slackLink {
					t.Errorf("request = %v", req)
				}
			}},
		{name: "slack-unsubscribe sends the thread", args: []string{"slack-unsubscribe", "peer", "-thread", slackLink},
			check: func(t *testing.T, f *cliFixture) {
				if req := only[*agentreplv1.UnsubscribeSlackThreadRequest](t, f); req.GetSubscriber().GetId() != "ws-peer" {
					t.Errorf("request = %v", req)
				}
			}},
		{name: "slack-mode sends the new mode", args: []string{"slack-mode", "here", "-thread", slackLink, "-mode", "listen"},
			check: func(t *testing.T, f *cliFixture) {
				if req := only[*agentreplv1.SetSlackThreadModeRequest](t, f); req.GetMode().GetListen() == nil {
					t.Errorf("request = %v", req)
				}
			}},
		{name: "slack-subscriptions lists", args: []string{"slack-subscriptions", "here"},
			check: func(t *testing.T, f *cliFixture) {
				if req := only[*agentreplv1.ListSlackThreadSubscriptionsRequest](t, f); req.GetSubscriber().GetId() != "ws-caller" {
					t.Errorf("request = %v", req)
				}
			}},
		{name: "slack-tail leaves the limit to the daemon", args: []string{"slack-tail", "here", "-thread", slackLink},
			check: func(t *testing.T, f *cliFixture) {
				if req := only[*agentreplv1.ReadSlackThreadRequest](t, f); req.Limit != nil || req.GetReader().GetId() != "ws-caller" {
					t.Errorf("request = %v", req)
				}
			}},
		{name: "slack-tail sends a limit", args: []string{"slack-tail", "here", "-thread", slackLink, "-limit", "5"},
			check: func(t *testing.T, f *cliFixture) {
				if req := only[*agentreplv1.ReadSlackThreadRequest](t, f); req.GetLimit() != 5 {
					t.Errorf("request = %v", req)
				}
			}},
		{name: "slack-post sends the text, forced", args: []string{"slack-post", "here", "-thread", slackLink, "-text", ":memo: done", "-force"},
			check: func(t *testing.T, f *cliFixture) {
				if req := only[*agentreplv1.PostToSlackThreadRequest](t, f); req.GetText() != ":memo: done" || !req.GetForce() {
					t.Errorf("request = %v", req)
				}
			}},
		{name: "create subscribes its workspace to its thread", args: []string{"create", "-repository", "here", "-relationship", "peer", "-context", "fresh", "-slack-thread", slackLink, "-slack-subscribe", "interact"},
			check: func(t *testing.T, f *cliFixture) {
				req := only[*agentreplv1.CreateWorkspaceRequest](t, f)
				if req.GetSlackOrigin().GetThreadUrl() != slackLink || req.GetSlackOrigin().GetSubscribe().GetInteract() == nil {
					t.Errorf("request = %v", req)
				}
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(workspaceNoun, tt.args...)

			// Assert
			if code != exitSuccess {
				t.Fatalf("exit %d, stderr %q", code, f.errOut.String())
			}
			tt.check(t, f)
		})
	}
}

func TestSlackWorkspaceVerbsRefuseAMalformedCall(t *testing.T) {
	tests := []struct {
		name string
		args []string
	}{
		{name: "slack-subscribe naming no thread", args: []string{"slack-subscribe", "here", "-mode", "respond"}},
		{name: "slack-subscribe naming no Slack link", args: []string{"slack-subscribe", "here", "-thread", "https://example.com/x", "-mode", "respond"}},
		{name: "slack-subscribe naming an unknown mode", args: []string{"slack-subscribe", "here", "-thread", slackLink, "-mode", "shout"}},
		{name: "slack-mode naming no mode", args: []string{"slack-mode", "here", "-thread", slackLink}},
		{name: "slack-post with no text", args: []string{"slack-post", "here", "-thread", slackLink}},
		{name: "create subscribing with no thread", args: []string{"create", "-repository", "here", "-relationship", "peer", "-context", "fresh", "-slack-subscribe", "respond"}},
		{name: "create subscribing in an unknown mode", args: []string{"create", "-repository", "here", "-relationship", "peer", "-context", "fresh", "-slack-thread", slackLink, "-slack-subscribe", "shout"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(workspaceNoun, tt.args...)

			// Assert
			if code == exitSuccess || len(f.client.requests) != 0 {
				t.Fatalf("exit %d, requests %v; want a usage failure and nothing sent", code, f.client.requests)
			}
		})
	}
}

func TestSlackNounSendsItsRequests(t *testing.T) {
	tests := []struct {
		name  string
		args  []string
		check func(t *testing.T, f *cliFixture)
	}{
		{name: "status", args: []string{"status"},
			check: func(t *testing.T, f *cliFixture) { only[*agentreplv1.GetSlackStatusRequest](t, f) }},
		{name: "watch-user", args: []string{"watch-user", "U7HH4U1J4"},
			check: func(t *testing.T, f *cliFixture) {
				if req := only[*agentreplv1.UpdateSlackReactionSettingsRequest](t, f); req.GetWatchUser().GetUserId() != "U7HH4U1J4" {
					t.Errorf("request = %v", req)
				}
			}},
		{name: "repository by directory", args: []string{"repository", "/repos/ee"},
			check: func(t *testing.T, f *cliFixture) {
				if req := only[*agentreplv1.UpdateSlackReactionSettingsRequest](t, f); req.GetRepository().GetDir() != "/repos/ee" {
					t.Errorf("request = %v", req)
				}
			}},
		{name: "bind an announced one-shot with a priority", args: []string{"bind", "-thread", "announced", "-one-shot", "-priority", "p1", ":p1-one-shot:"},
			check: func(t *testing.T, f *cliFixture) {
				b := only[*agentreplv1.UpdateSlackReactionSettingsRequest](t, f).GetBind()
				if b.GetEmoji() != "p1-one-shot" || b.GetAction().GetAnnounced() == nil || !b.GetAction().GetOneShot() || b.GetAction().GetPriority().GetP1() == nil {
					t.Errorf("binding = %v", b)
				}
			}},
		{name: "bind a subscribed workspace", args: []string{"bind", "-thread", "subscribed", "-mode", "respond", "sockets-ack"},
			check: func(t *testing.T, f *cliFixture) {
				b := only[*agentreplv1.UpdateSlackReactionSettingsRequest](t, f).GetBind()
				if b.GetAction().GetSubscribed().GetMode().GetRespond() == nil || b.GetAction().GetOneShot() || b.GetAction().Priority != nil {
					t.Errorf("binding = %v", b)
				}
			}},
		{name: "unbind", args: []string{"unbind", "agent-ack"},
			check: func(t *testing.T, f *cliFixture) {
				if req := only[*agentreplv1.UpdateSlackReactionSettingsRequest](t, f); req.GetUnbind() != "agent-ack" {
					t.Errorf("request = %v", req)
				}
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(slackNoun, tt.args...)

			// Assert
			if code != exitSuccess {
				t.Fatalf("exit %d, stderr %q", code, f.errOut.String())
			}
			tt.check(t, f)
		})
	}
}

func TestSlackNounRefusesAMalformedCall(t *testing.T) {
	tests := []struct {
		name string
		args []string
	}{
		{name: "watch-user naming nobody", args: []string{"watch-user"}},
		{name: "bind with no thread relation", args: []string{"bind", "agent-ack"}},
		{name: "bind announced with a mode", args: []string{"bind", "-thread", "announced", "-mode", "respond", "agent-ack"}},
		{name: "bind subscribed with no mode", args: []string{"bind", "-thread", "subscribed", "sockets-ack"}},
		{name: "bind with an unknown priority", args: []string{"bind", "-thread", "announced", "-priority", "p9", "agent-ack"}},
		{name: "unbind naming two", args: []string{"unbind", "a", "b"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newCLIFixture()

			// Act
			code := f.run(slackNoun, tt.args...)

			// Assert
			if code == exitSuccess || len(f.client.requests) != 0 {
				t.Fatalf("exit %d, requests %v; want a usage failure and nothing sent", code, f.client.requests)
			}
		})
	}
}

func TestSlackNonceIsSixteenHexDigitsAndFresh(t *testing.T) {
	// Act
	first, err1 := slackNonce()
	second, err2 := slackNonce()

	// Assert
	if err1 != nil || err2 != nil || len(first) != 16 || first == second {
		t.Fatalf("slackNonce = %q, %q (%v, %v)", first, second, err1, err2)
	}
	if _, err := hex.DecodeString(first); err != nil {
		t.Fatalf("%q is not hex: %v", first, err)
	}
}
