package server

import (
	"context"
	"testing"

	"connectrpc.com/connect"

	"claude-repld/internal/login"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// topbarView is one complete topbar view, distinguishable from the footer's so
// a multiplexed frame's arm can be told apart from its neighbor's.
func topbarView() *frontendv1.TopbarView {
	return &frontendv1.TopbarView{Title: &frontendv1.TopbarTitle{Text: "the workspace"}}
}

// attachPageStream opens a page's one stream and reads its attachment latch,
// so a test that subscribes afterwards is doing what a real page does.
func attachPageStream(
	t *testing.T,
	h *harness,
	ctx context.Context,
	page string,
) *connect.ServerStreamForClient[agentreplv1.WatchPageResponse] {
	t.Helper()
	stream, err := h.Client.WatchPage(ctx, connect.NewRequest(&agentreplv1.WatchPageRequest{Page: page}))
	if err != nil {
		t.Fatalf("open the page's stream: %v", err)
	}
	if !stream.Receive() {
		t.Fatalf("the page's stream ended before it attached: %v", stream.Err())
	}
	if stream.Msg().GetAttached() == nil {
		t.Fatalf("the page's first frame was %T, not the attachment latch", stream.Msg().GetFrame())
	}
	return stream
}

// TestPageStreamAttachesBeforeAnySubscription pins the LATCH: the first frame a
// page reads says it is registered, and it arrives before any subscription can
// exist. Without it a client would have to guess when SubscribePage is safe.
func TestPageStreamAttachesBeforeAnySubscription(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act.
	stream := attachPageStream(t, h, ctx, "page-1")

	// Assert: the latch is the frame, and nothing else was needed to get it.
	if stream.Msg().GetAttached() == nil {
		t.Fatal("the page's first frame was not PageAttached")
	}
}

// TestPageCarriesASubscriptionsPushes pins the whole point: a view published
// after a subscription is accepted arrives on the PAGE's stream, in the arm its
// rpc owns, addressed to the subscription that asked for it.
func TestPageCarriesASubscriptionsPushes(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := attachPageStream(t, h, ctx, "page-1")

	// Act.
	if _, err := h.Client.SubscribePage(ctx, connect.NewRequest(&agentreplv1.SubscribePageRequest{
		Page:         "page-1",
		Subscription: "footer-1",
		Request: &agentreplv1.SubscribePageRequest_Footer{
			Footer: &agentreplv1.WatchFooterRequest{Workspace: ref()},
		},
	})); err != nil {
		t.Fatalf("subscribe the footer: %v", err)
	}
	h.Footer.topic.Publish(footerView(42))

	// Assert.
	if !stream.Receive() {
		t.Fatalf("the page's stream ended before the footer push: %v", stream.Err())
	}
	push := stream.Msg().GetPush()
	if push == nil {
		t.Fatalf("the page's second frame was %T, not a push", stream.Msg().GetFrame())
	}
	if push.GetSubscription() != "footer-1" {
		t.Fatalf("the push was addressed to %q, not to the subscription that asked for it", push.GetSubscription())
	}
	if got := footerMark(push.GetFooter().GetFooter()); got != 42 {
		t.Fatalf("the push carried footer %d, not the one that was published", got)
	}
}

// TestSubscribePageAnswersOnlyOnceSubscribed pins the acceptance rule for a
// muxed subscription: the unary's answer means the subscription is registered
// with its publisher, so a view published AFTER the answer cannot be missed.
func TestSubscribePageAnswersOnlyOnceSubscribed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := attachPageStream(t, h, ctx, "page-1")

	// Act: publish strictly after the answer.
	if _, err := h.Client.SubscribePage(ctx, connect.NewRequest(&agentreplv1.SubscribePageRequest{
		Page:         "page-1",
		Subscription: "footer-1",
		Request: &agentreplv1.SubscribePageRequest_Footer{
			Footer: &agentreplv1.WatchFooterRequest{Workspace: ref()},
		},
	})); err != nil {
		t.Fatalf("subscribe the footer: %v", err)
	}
	h.Footer.topic.Publish(footerView(7))

	// Assert.
	if !stream.Receive() {
		t.Fatalf("a view published after acceptance never arrived: %v", stream.Err())
	}
	if got := footerMark(stream.Msg().GetPush().GetFooter().GetFooter()); got != 7 {
		t.Fatalf("the push carried footer %d, not the one published after acceptance", got)
	}
}

// TestPageCarriesTwoSubscriptionsAtOnce pins the multiplexing itself: two
// watches, one stream, each push addressed to its own subscription. This is the
// property the browser's six-connection cap made necessary.
func TestPageCarriesTwoSubscriptionsAtOnce(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := attachPageStream(t, h, ctx, "page-1")
	for _, sub := range []struct {
		id  string
		req *agentreplv1.SubscribePageRequest
	}{
		{"footer-1", &agentreplv1.SubscribePageRequest{
			Page: "page-1", Subscription: "footer-1",
			Request: &agentreplv1.SubscribePageRequest_Footer{
				Footer: &agentreplv1.WatchFooterRequest{Workspace: ref()},
			},
		}},
		{"topbar-1", &agentreplv1.SubscribePageRequest{
			Page: "page-1", Subscription: "topbar-1",
			Request: &agentreplv1.SubscribePageRequest_Topbar{
				Topbar: &agentreplv1.WatchTopbarRequest{Workspace: ref()},
			},
		}},
	} {
		if _, err := h.Client.SubscribePage(ctx, connect.NewRequest(sub.req)); err != nil {
			t.Fatalf("subscribe %s: %v", sub.id, err)
		}
	}

	// Act.
	h.Footer.topic.Publish(footerView(1))
	h.Topbar.topic.Publish(topbarView())

	// Assert: both arrive, on the one stream, each addressed to its own
	// subscription. The two publishers are independent, so the ORDER of the two
	// frames is not asserted — only that one stream carried both.
	seen := map[string]bool{}
	for len(seen) < 2 {
		if !stream.Receive() {
			t.Fatalf("the page's stream ended with only %d of 2 subscriptions heard from: %v", len(seen), stream.Err())
		}
		push := stream.Msg().GetPush()
		if push == nil {
			t.Fatalf("the page sent %T while two subscriptions were live", stream.Msg().GetFrame())
		}
		switch push.GetSubscription() {
		case "footer-1":
			if push.GetFooter() == nil {
				t.Fatalf("the footer subscription's push carried %T", push.GetPayload())
			}
		case "topbar-1":
			if push.GetTopbar() == nil {
				t.Fatalf("the topbar subscription's push carried %T", push.GetPayload())
			}
		default:
			t.Fatalf("a push was addressed to the unknown subscription %q", push.GetSubscription())
		}
		seen[push.GetSubscription()] = true
	}
}

// TestSubscribePageRefusesAnUnattachedPage pins the latch's error half: a
// subscription for a page that holds no stream is REFUSED, loudly, rather than
// buffered against a stream that may never arrive.
func TestSubscribePageRefusesAnUnattachedPage(t *testing.T) {
	// Arrange: no page has attached.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act.
	_, err := h.Client.SubscribePage(ctx, connect.NewRequest(&agentreplv1.SubscribePageRequest{
		Page:         "never-attached",
		Subscription: "footer-1",
		Request: &agentreplv1.SubscribePageRequest_Footer{
			Footer: &agentreplv1.WatchFooterRequest{Workspace: ref()},
		},
	}))

	// Assert.
	if err == nil {
		t.Fatal("subscribing to an unattached page was accepted")
	}
	if got := connectCode(t, err); got != connect.CodeNotFound {
		t.Fatalf("the refusal came back as %v, not not_found", got)
	}
}

// TestSubscribePageRecordsAnUnattachedPageRefusal pins the canonical log record
// behind that refusal: the one place a page-mux refusal enters the shared log.
func TestSubscribePageRecordsAnUnattachedPageRefusal(t *testing.T) {
	// Arrange.
	log := &recordingLogger{}
	h := newHarness(t, func(d *Deps) { d.Log = &fakeSurfaces{global: log} })
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act.
	_, err := h.Client.SubscribePage(ctx, connect.NewRequest(&agentreplv1.SubscribePageRequest{
		Page:         "never-attached",
		Subscription: "footer-1",
		Request: &agentreplv1.SubscribePageRequest_Footer{
			Footer: &agentreplv1.WatchFooterRequest{Workspace: ref()},
		},
	}))
	if err == nil {
		t.Fatal("subscribing to an unattached page was accepted")
	}

	// Assert.
	var found bool
	for _, rec := range log.at("INFO") {
		if rec.Operation == opTransportClosed && rec.Context["cause"] == "page_not_attached" {
			found = true
			if rec.Context["rpc"] != "SubscribePage" {
				t.Fatalf("the refusal was recorded against rpc %v", rec.Context["rpc"])
			}
		}
	}
	if !found {
		t.Fatalf("no page_not_attached refusal was recorded; got %v", log.records)
	}
}

// TestSubscribePageRefusesASecondSubscriptionOnOneId pins the addressing
// invariant: a subscription id addresses ONE watch, so reusing a live one is
// refused rather than silently taking the first one's pushes.
func TestSubscribePageRefusesASecondSubscriptionOnOneId(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	attachPageStream(t, h, ctx, "page-1")
	first := &agentreplv1.SubscribePageRequest{
		Page: "page-1", Subscription: "footer-1",
		Request: &agentreplv1.SubscribePageRequest_Footer{
			Footer: &agentreplv1.WatchFooterRequest{Workspace: ref()},
		},
	}
	if _, err := h.Client.SubscribePage(ctx, connect.NewRequest(first)); err != nil {
		t.Fatalf("subscribe the footer: %v", err)
	}

	// Act.
	_, err := h.Client.SubscribePage(ctx, connect.NewRequest(first))

	// Assert.
	if err == nil {
		t.Fatal("a second subscription reused a live id without being refused")
	}
	if got := connectCode(t, err); got != connect.CodeFailedPrecondition {
		t.Fatalf("the refusal came back as %v, not failed_precondition", got)
	}
}

// TestWatchPageRefusesASecondStreamForOnePage pins the page's own uniqueness: a
// second stream would make the first page's subscriptions addressable by a
// client that does not own them.
func TestWatchPageRefusesASecondStreamForOnePage(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	attachPageStream(t, h, ctx, "page-1")

	// Act.
	second, dialErr := h.Client.WatchPage(ctx, connect.NewRequest(&agentreplv1.WatchPageRequest{Page: "page-1"}))
	if dialErr != nil {
		// A refusal may surface on the open itself.
		if got := connectCode(t, dialErr); got != connect.CodeFailedPrecondition {
			t.Fatalf("the refusal came back as %v, not failed_precondition", got)
		}
		return
	}

	// Assert: or on the first receive, which is where a Connect stream reports
	// a handler that refused before sending anything.
	if second.Receive() {
		t.Fatalf("a second stream for one page was served frame %T", second.Msg().GetFrame())
	}
	if got := connectCode(t, second.Err()); got != connect.CodeFailedPrecondition {
		t.Fatalf("the refusal came back as %v, not failed_precondition", got)
	}
}

// TestSubscribePageForwardsAWatchsOwnRefusal pins that a muxed subscription
// refuses exactly as its dedicated rpc does: the body is the same body, so an
// unknown feed token is the same not_found here as it is on WatchFeed.
func TestSubscribePageForwardsAWatchsOwnRefusal(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	attachPageStream(t, h, ctx, "page-1")

	// Act: a token this daemon never minted.
	_, err := h.Client.SubscribePage(ctx, connect.NewRequest(&agentreplv1.SubscribePageRequest{
		Page: "page-1", Subscription: "feed-1",
		Request: &agentreplv1.SubscribePageRequest_Feed{
			Feed: &agentreplv1.WatchFeedRequest{
				Watch: &agentreplv1.FeedWatchToken{Value: "never-minted"},
			},
		},
	}))

	// Assert.
	if err == nil {
		t.Fatal("a feed tail on an unminted token was accepted")
	}
	if got := connectCode(t, err); got != connect.CodeNotFound {
		t.Fatalf("the refusal came back as %v, not the not_found WatchFeed refuses with", got)
	}
}

// TestUnsubscribePageStopsOnlyThatSubscription pins that ending one watch
// leaves the page's others running — the property that lets a bubble collapse
// without disturbing the root feed.
func TestUnsubscribePageStopsOnlyThatSubscription(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := attachPageStream(t, h, ctx, "page-1")
	for _, req := range []*agentreplv1.SubscribePageRequest{
		{Page: "page-1", Subscription: "footer-1",
			Request: &agentreplv1.SubscribePageRequest_Footer{
				Footer: &agentreplv1.WatchFooterRequest{Workspace: ref()},
			}},
		{Page: "page-1", Subscription: "topbar-1",
			Request: &agentreplv1.SubscribePageRequest_Topbar{
				Topbar: &agentreplv1.WatchTopbarRequest{Workspace: ref()},
			}},
	} {
		if _, err := h.Client.SubscribePage(ctx, connect.NewRequest(req)); err != nil {
			t.Fatalf("subscribe %s: %v", req.GetSubscription(), err)
		}
	}

	// Act.
	if _, err := h.Client.UnsubscribePage(ctx, connect.NewRequest(&agentreplv1.UnsubscribePageRequest{
		Page: "page-1", Subscription: "footer-1",
	})); err != nil {
		t.Fatalf("unsubscribe the footer: %v", err)
	}
	h.Topbar.topic.Publish(topbarView())

	// Assert: the surviving subscription still pushes, and the ended one never
	// announces itself — a client that asked for the end is not told about it.
	if !stream.Receive() {
		t.Fatalf("the surviving subscription stopped pushing: %v", stream.Err())
	}
	push := stream.Msg().GetPush()
	if push == nil || push.GetSubscription() != "topbar-1" {
		t.Fatalf("the page's next frame was %v, not the surviving subscription's push", stream.Msg().GetFrame())
	}
}

// TestUnsubscribePageAcceptsAnAbsentSubscription pins that the end is a STATE,
// not an event: unsubscribing one the daemon already ended is exactly the state
// the caller asked for, so it succeeds.
func TestUnsubscribePageAcceptsAnAbsentSubscription(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	attachPageStream(t, h, ctx, "page-1")

	// Act.
	_, err := h.Client.UnsubscribePage(ctx, connect.NewRequest(&agentreplv1.UnsubscribePageRequest{
		Page: "page-1", Subscription: "never-subscribed",
	}))

	// Assert.
	if err != nil {
		t.Fatalf("unsubscribing an absent subscription was refused: %v", err)
	}
}

// TestWatchPageRefusesAnUnnamedPage pins the one argument this stream takes.
func TestWatchPageRefusesAnUnnamedPage(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act.
	stream, dialErr := h.Client.WatchPage(ctx, connect.NewRequest(&agentreplv1.WatchPageRequest{}))
	var err error
	if dialErr != nil {
		err = dialErr
	} else {
		stream.Receive()
		err = stream.Err()
	}

	// Assert.
	if err == nil {
		t.Fatal("an unnamed page was attached")
	}
	if got := connectCode(t, err); got != connect.CodeInvalidArgument {
		t.Fatalf("the refusal came back as %v, not invalid_argument", got)
	}
}

// TestSubscribePageRefusesAnEmptyRequest pins that a subscription must name a
// watch: an arm-less request would otherwise register a subscription that could
// never push anything.
func TestSubscribePageRefusesAnEmptyRequest(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	attachPageStream(t, h, ctx, "page-1")

	// Act.
	_, err := h.Client.SubscribePage(ctx, connect.NewRequest(&agentreplv1.SubscribePageRequest{
		Page: "page-1", Subscription: "nothing-1",
	}))

	// Assert.
	if err == nil {
		t.Fatal("a subscription naming no watch was accepted")
	}
	if got := connectCode(t, err); got != connect.CodeInvalidArgument {
		t.Fatalf("the refusal came back as %v, not invalid_argument", got)
	}
}

// TestSubscriptionThatEndsOnItsOwnIsAnnounced pins the `ended` frame: a
// subscription the DAEMON finished — here the login pty closing — tells the
// page so, because otherwise a client would read a watch that simply stopped
// pushing as a watch that had nothing to say.
func TestSubscriptionThatEndsOnItsOwnIsAnnounced(t *testing.T) {
	// Arrange: a login terminal whose pty is about to close.
	frames := make(chan login.Output, 1)
	h := newHarness(t)
	h.Login.watchFrames = frames
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := attachPageStream(t, h, ctx, "page-1")
	if _, err := h.Client.SubscribePage(ctx, connect.NewRequest(&agentreplv1.SubscribePageRequest{
		Page: "page-1", Subscription: "login-1",
		Request: &agentreplv1.SubscribePageRequest_LoginTerminal{
			LoginTerminal: &agentreplv1.WatchLoginTerminalRequest{Workspace: ref()},
		},
	})); err != nil {
		t.Fatalf("subscribe the login terminal: %v", err)
	}

	// Act: the pty's LAST frame, which ends the watch.
	frames <- login.Output{Closed: true}

	// Assert: the closing frame arrives on the page, and the end after it.
	if !stream.Receive() {
		t.Fatalf("the login terminal's closing frame never arrived: %v", stream.Err())
	}
	if stream.Msg().GetPush().GetLoginTerminal().GetClosed() == nil {
		t.Fatalf("the page's frame was %v, not the pty's closure", stream.Msg().GetFrame())
	}
	if !stream.Receive() {
		t.Fatalf("the page's stream ended instead of announcing the subscription's end: %v", stream.Err())
	}
	ended := stream.Msg().GetEnded()
	if ended == nil {
		t.Fatalf("the page's frame was %T, not a subscription end", stream.Msg().GetFrame())
	}
	if ended.GetSubscription() != "login-1" {
		t.Fatalf("the end named subscription %q", ended.GetSubscription())
	}
}

// TestDetachingAPageReleasesItsId pins the registry's half of the lifetime
// rule: a page's registration goes away with its stream, so the id is free for
// the page that reloads into it.
func TestDetachingAPageReleasesItsId(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	surface, ok := h.Server.(*server)
	if !ok {
		t.Fatalf("the surface under test is %T, not the one that holds the page registry", h.Server)
	}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	page, err := surface.attachPage("page-1", ctx)
	if err != nil {
		t.Fatalf("attach the page: %v", err)
	}

	// Act.
	surface.detachPage(page)

	// Assert.
	if _, held := surface.pageFor("page-1"); held {
		t.Fatal("a detached page still holds its id")
	}
	if _, err := surface.attachPage("page-1", ctx); err != nil {
		t.Fatalf("the released id was refused to the next page: %v", err)
	}
}

// TestAttachingOneIdTwiceIsRefusedInTheRegistry pins the uniqueness at its own
// level, so the guarantee is not only observable through a served stream.
func TestAttachingOneIdTwiceIsRefusedInTheRegistry(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	surface, ok := h.Server.(*server)
	if !ok {
		t.Fatalf("the surface under test is %T, not the one that holds the page registry", h.Server)
	}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	if _, err := surface.attachPage("page-1", ctx); err != nil {
		t.Fatalf("attach the page: %v", err)
	}

	// Act.
	_, err := surface.attachPage("page-1", ctx)

	// Assert.
	if err == nil {
		t.Fatal("one page id was attached twice")
	}
}
