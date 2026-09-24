//go:build integration

// Package integration: THE PAGE MUX. One browser page, one connection, every
// standing view it draws. See proto/src/agentrepl/v1/endpoint_watch_page.proto
// and daemon/internal/server/page.go.
package integration

import (
	"context"
	"crypto/tls"
	"net"
	"net/http"
	"strings"
	"sync"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/integration/harness"

	"connectrpc.com/connect"
	"golang.org/x/net/http2"
)

// countingDialer is a client transport that records how many connections it
// opened to the daemon.
//
// THE COUNT IS THE WHOLE POINT OF THE MUX, so it is measured rather than
// argued. A browser holds about six connections per origin and a
// server-streaming Connect call pins one for its whole life, so what the page
// must be able to say is not "few streams" but "one connection, whatever I
// watch". Every dial this transport performs is one connection the daemon
// accepted from this page.
type countingDialer struct {
	mu    sync.Mutex
	dials int
}

func (c *countingDialer) client() *http.Client {
	return &http.Client{Transport: &http2.Transport{
		AllowHTTP: true,
		DialTLSContext: func(ctx context.Context, network, addr string, _ *tls.Config) (net.Conn, error) {
			c.mu.Lock()
			c.dials++
			c.mu.Unlock()
			var dialer net.Dialer
			return dialer.DialContext(ctx, network, addr)
		},
	}}
}

func (c *countingDialer) count() int {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.dials
}

// pageFrames pumps a page's stream onto a channel, exactly as the harness pumps
// a dedicated one.
func pageFrames(
	t *testing.T,
	ctx context.Context,
	stream *connect.ServerStreamForClient[agentreplv1.WatchPageResponse],
) <-chan *agentreplv1.WatchPageResponse {
	t.Helper()
	frames := make(chan *agentreplv1.WatchPageResponse, 256)
	go func() {
		defer close(frames)
		for stream.Receive() {
			select {
			case frames <- stream.Msg():
			case <-ctx.Done():
				return
			}
		}
	}()
	return frames
}

// TestOnePageHoldsEveryViewOverOneConnection pins the mux end to end against a
// real daemon: a page that watches ALL EIGHT of the views `SubscribePage`
// carries hears from every one of them, and it opened exactly ONE connection to
// do it.
//
// The eight are the whole standing surface a webview draws — the roster, the
// workspace's web link, the daemon's own facts, the topbar, the footer, the
// hold tray, a feed tail and the login terminal. Six of them were what filled a
// browser's connection budget before the seventh silently queued forever.
func TestOnePageHoldsEveryViewOverOneConnection(t *testing.T) {
	t.Parallel()
	// Arrange: an opened workspace, and a page client whose dials are counted.
	f := newOpened(t, harness.Opts{})
	ctx := f.d.Ctx()
	dialer := &countingDialer{}
	page := agentreplv1connect.NewAgentReplClient(dialer.client(), "http://"+f.d.Addr)

	stream, err := page.WatchPage(ctx, connect.NewRequest(&agentreplv1.WatchPageRequest{Page: "page-everything"}))
	if err != nil {
		t.Fatalf("WatchPage = error %v, want the page's one stream", err)
	}
	frames := pageFrames(t, ctx, stream)
	if first := awaitPageFrame(t, ctx, frames, "the attachment latch"); first.GetAttached() == nil {
		t.Fatalf("the page's first frame = %T, want PageAttached", first.GetFrame())
	}

	// The two views that must be OPENED before they can be watched.
	feedResp, err := page.OpenFeed(ctx, connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: f.ws}))
	if err != nil || feedResp.Msg.GetSuccess() == nil {
		t.Fatalf("OpenFeed = (%v, %v), want a success minting a watch token", feedResp, err)
	}
	if _, err := page.OpenLogin(ctx, connect.NewRequest(&agentreplv1.OpenLoginRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("OpenLogin = error %v, want a success", err)
	}

	// Act: eight subscriptions, on the one stream.
	subscriptions := []*agentreplv1.SubscribePageRequest{
		{Subscription: "roster", Request: &agentreplv1.SubscribePageRequest_Roster{
			Roster: &agentreplv1.WatchWorkspaceRosterRequest{}}},
		{Subscription: "web", Request: &agentreplv1.SubscribePageRequest_WebWorkspace{
			WebWorkspace: &agentreplv1.WatchWebWorkspaceRequest{Workspace: f.ws, WebappBuild: harness.FakeWebappEntry}}},
		{Subscription: "daemon", Request: &agentreplv1.SubscribePageRequest_Daemon{
			Daemon: &agentreplv1.WatchDaemonRequest{Client: &agentreplv1.WatchDaemonRequest_Webview{Webview: &agentreplv1.WatchDaemonWebview{}}}}},
		{Subscription: "topbar", Request: &agentreplv1.SubscribePageRequest_Topbar{
			Topbar: &agentreplv1.WatchTopbarRequest{Workspace: f.ws}}},
		{Subscription: "footer", Request: &agentreplv1.SubscribePageRequest_Footer{
			Footer: &agentreplv1.WatchFooterRequest{Workspace: f.ws}}},
		{Subscription: "holds", Request: &agentreplv1.SubscribePageRequest_Holds{
			Holds: &agentreplv1.WatchDaemonHoldsRequest{Workspace: f.ws}}},
		{Subscription: "feed", Request: &agentreplv1.SubscribePageRequest_Feed{
			Feed: &agentreplv1.WatchFeedRequest{Watch: feedResp.Msg.GetSuccess().GetWatch()}}},
		{Subscription: "login", Request: &agentreplv1.SubscribePageRequest_LoginTerminal{
			LoginTerminal: &agentreplv1.WatchLoginTerminalRequest{Workspace: f.ws}}},
	}
	for _, req := range subscriptions {
		req.Page = "page-everything"
		if _, err := page.SubscribePage(ctx, connect.NewRequest(req)); err != nil {
			t.Fatalf("SubscribePage{%s} = error %v, want an accepted subscription", req.GetSubscription(), err)
		}
	}

	// The two views nothing has published to yet: a prompt makes a feed row,
	// and a scheduled drain makes a daemon-level fact.
	f.submit("draw every view at once", "k-page-mux-all-eight", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	if _, err := f.d.Client().UpdateShutdownSchedule(ctx, connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
		Action: &agentreplv1.UpdateShutdownScheduleRequest_Schedule{Schedule: &agentreplv1.UpdateShutdownScheduleSchedule{
			AtMs: time.Now().Add(time.Hour).UnixMilli(), Reason: drainReasonDeploy(),
		}},
	})); err != nil {
		t.Fatalf("UpdateShutdownSchedule{schedule} = error %v, want a success", err)
	}

	// Assert: every subscription is heard from, each in the arm its own rpc
	// owns, and the page never opened a second connection.
	want := map[string]func(*agentreplv1.PageFrame) bool{
		"roster": func(p *agentreplv1.PageFrame) bool { return p.GetRoster() != nil },
		"web":    func(p *agentreplv1.PageFrame) bool { return p.GetWebWorkspace() != nil },
		"daemon": func(p *agentreplv1.PageFrame) bool { return p.GetDaemon() != nil },
		"topbar": func(p *agentreplv1.PageFrame) bool { return p.GetTopbar() != nil },
		"footer": func(p *agentreplv1.PageFrame) bool { return p.GetFooter() != nil },
		"holds":  func(p *agentreplv1.PageFrame) bool { return p.GetHolds() != nil },
		"feed":   func(p *agentreplv1.PageFrame) bool { return p.GetFeed() != nil },
		"login":  func(p *agentreplv1.PageFrame) bool { return p.GetLoginTerminal() != nil },
	}
	heard := map[string]bool{}
	for len(heard) < len(want) {
		frame := awaitPageFrame(t, ctx, frames, missingSubscriptions(want, heard))
		push := frame.GetPush()
		if push == nil {
			t.Fatalf("the page carried %T while eight subscriptions were live", frame.GetFrame())
		}
		arm, known := want[push.GetSubscription()]
		if !known {
			t.Fatalf("a push was addressed to the unknown subscription %q", push.GetSubscription())
		}
		if !arm(push) {
			t.Fatalf("subscription %q pushed %T, not the arm its rpc owns",
				push.GetSubscription(), push.GetPayload())
		}
		heard[push.GetSubscription()] = true
	}
	if got := dialer.count(); got != 1 {
		t.Fatalf("the page opened %d connections to hold eight views, want exactly 1", got)
	}
}

// awaitPageFrame reads the page's next frame. The bound is ONE WAIT'S, taken as
// a child of the run's budget, so a slow earlier step does not surface as a
// timeout on a later blameless one. Nothing sleeps.
func awaitPageFrame(
	t *testing.T,
	ctx context.Context,
	frames <-chan *agentreplv1.WatchPageResponse,
	what string,
) *agentreplv1.WatchPageResponse {
	t.Helper()
	wait, cancel := context.WithTimeout(ctx, harness.DefaultTimeout)
	defer cancel()
	select {
	case frame, ok := <-frames:
		if !ok {
			t.Fatalf("the page's stream ended while waiting for %s", what)
			return nil
		}
		return frame
	case <-wait.Done():
		t.Fatalf("waiting for %s: %v", what, wait.Err())
		return nil
	}
}

// missingSubscriptions names the subscriptions still unheard from, so a stalled
// mux says WHICH view never pushed rather than only that something did not.
func missingSubscriptions(want map[string]func(*agentreplv1.PageFrame) bool, heard map[string]bool) string {
	var missing []string
	for name := range want {
		if !heard[name] {
			missing = append(missing, name)
		}
	}
	return "a push from " + strings.Join(missing, ", ")
}
