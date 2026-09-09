package server

import (
	"context"
	"errors"
	"testing"
	"time"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

	"claude-repld/internal/login"
	"claude-repld/internal/publish"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
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

	// Assert: the surviving subscription still pushes. The end of the other one
	// is announced too — its `how` arm is TestUnsubscribingAnnouncesTheEndAsUnsubscribed's
	// subject — and it races the surviving subscription's push, so the frames
	// are scanned rather than counted.
	for {
		if !stream.Receive() {
			t.Fatalf("the surviving subscription stopped pushing: %v", stream.Err())
		}
		if ended := stream.Msg().GetEnded(); ended != nil {
			if ended.GetSubscription() != "footer-1" {
				t.Fatalf("the end named subscription %q, not the one that was unsubscribed", ended.GetSubscription())
			}
			continue
		}
		push := stream.Msg().GetPush()
		if push == nil || push.GetSubscription() != "topbar-1" {
			t.Fatalf("the page's next frame was %v, not the surviving subscription's push", stream.Msg().GetFrame())
		}
		return
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

// awaitSubscribers waits for a topic to be serving exactly want subscriptions.
//
// IT WAITS ON THE CONDITION, NOT ON A DELAY. A subscription's release is
// concurrent with the frame that announces it: `serveTopic` returns when the
// stream's context ends, while the topic's own pump unregisters the subscriber
// from its own goroutine, so the announcement can reach the page before the
// registry has caught up. The bound is three orders of magnitude over the two
// goroutine hops it covers and exists only so a leak fails loudly instead of
// hanging the suite.
func awaitSubscribers[T any](t *testing.T, what string, topic *publish.Topic[T], want int) {
	t.Helper()
	deadline := time.Now().Add(2 * time.Second)
	for {
		got := topic.Subscribers()
		if got == want {
			return
		}
		if !time.Now().Before(deadline) {
			t.Fatalf("the %s topic is serving %d subscriptions, want %d", what, got, want)
		}
		time.Sleep(time.Millisecond)
	}
}

// TestPageStartsASubscriptionAddedMidStream pins that the mux is not a set
// fixed at attach time: a bubble expanded halfway through a conversation gets
// its own subscription, and the subscriptions already running are undisturbed
// by it.
func TestPageStartsASubscriptionAddedMidStream(t *testing.T) {
	// Arrange: one subscription, already delivering.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := attachPageStream(t, h, ctx, "page-1")
	if _, err := h.Client.SubscribePage(ctx, connect.NewRequest(&agentreplv1.SubscribePageRequest{
		Page: "page-1", Subscription: "footer-1",
		Request: &agentreplv1.SubscribePageRequest_Footer{
			Footer: &agentreplv1.WatchFooterRequest{Workspace: ref()},
		},
	})); err != nil {
		t.Fatalf("subscribe the footer: %v", err)
	}
	h.Footer.topic.Publish(footerView(1))
	if !stream.Receive() {
		t.Fatalf("the first subscription never delivered: %v", stream.Err())
	}

	// Act: a second subscription, opened while the first is live.
	if _, err := h.Client.SubscribePage(ctx, connect.NewRequest(&agentreplv1.SubscribePageRequest{
		Page: "page-1", Subscription: "topbar-1",
		Request: &agentreplv1.SubscribePageRequest_Topbar{
			Topbar: &agentreplv1.WatchTopbarRequest{Workspace: ref()},
		},
	})); err != nil {
		t.Fatalf("subscribe the topbar mid-stream: %v", err)
	}
	h.Topbar.topic.Publish(topbarView())
	h.Footer.topic.Publish(footerView(2))

	// Assert: the newcomer delivers AND the incumbent keeps delivering. The two
	// publishers are independent, so only the set of what arrived is asserted.
	seen := map[string]bool{}
	for len(seen) < 2 {
		if !stream.Receive() {
			t.Fatalf("the page's stream ended with %d of 2 pushes seen: %v", len(seen), stream.Err())
		}
		push := stream.Msg().GetPush()
		if push == nil {
			t.Fatalf("the page sent %T while two subscriptions were live", stream.Msg().GetFrame())
		}
		switch push.GetSubscription() {
		case "footer-1":
			if got := footerMark(push.GetFooter().GetFooter()); got != 2 {
				t.Fatalf("the incumbent subscription delivered footer %d, not the one published after the newcomer joined", got)
			}
		case "topbar-1":
			if push.GetTopbar() == nil {
				t.Fatalf("the newcomer's push carried %T", push.GetPayload())
			}
		default:
			t.Fatalf("a push was addressed to the unknown subscription %q", push.GetSubscription())
		}
		seen[push.GetSubscription()] = true
	}
}

// TestUnsubscribingReleasesTheTopicItHeld pins that ending a subscription
// releases the publisher's own registration. Without it every collapsed bubble
// would leave a subscriber on a topic for the life of the page, and the
// publisher would fan out to readers nothing drains.
func TestUnsubscribingReleasesTheTopicItHeld(t *testing.T) {
	// Arrange: two subscriptions on two topics.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	attachPageStream(t, h, ctx, "page-1")
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
	awaitSubscribers(t, "footer", &h.Footer.topic, 1)

	// Act.
	if _, err := h.Client.UnsubscribePage(ctx, connect.NewRequest(&agentreplv1.UnsubscribePageRequest{
		Page: "page-1", Subscription: "footer-1",
	})); err != nil {
		t.Fatalf("unsubscribe the footer: %v", err)
	}

	// Assert: the ended subscription's topic is released and the surviving
	// one's is untouched.
	awaitSubscribers(t, "footer", &h.Footer.topic, 0)
	awaitSubscribers(t, "topbar", &h.Topbar.topic, 1)
}

// TestUnsubscribingAnnouncesTheEndAsUnsubscribed pins the `how` arm for the end
// the CLIENT asked for. It is announced rather than suppressed so an end a page
// requested and an end that raced its request are one fact on one wire.
func TestUnsubscribingAnnouncesTheEndAsUnsubscribed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := attachPageStream(t, h, ctx, "page-1")
	if _, err := h.Client.SubscribePage(ctx, connect.NewRequest(&agentreplv1.SubscribePageRequest{
		Page: "page-1", Subscription: "footer-1",
		Request: &agentreplv1.SubscribePageRequest_Footer{
			Footer: &agentreplv1.WatchFooterRequest{Workspace: ref()},
		},
	})); err != nil {
		t.Fatalf("subscribe the footer: %v", err)
	}

	// Act.
	if _, err := h.Client.UnsubscribePage(ctx, connect.NewRequest(&agentreplv1.UnsubscribePageRequest{
		Page: "page-1", Subscription: "footer-1",
	})); err != nil {
		t.Fatalf("unsubscribe the footer: %v", err)
	}

	// Assert.
	if !stream.Receive() {
		t.Fatalf("the page's stream ended instead of announcing the end: %v", stream.Err())
	}
	ended := stream.Msg().GetEnded()
	if ended == nil {
		t.Fatalf("the page's frame was %T, not a subscription end", stream.Msg().GetFrame())
	}
	if ended.GetUnsubscribed() == nil {
		t.Fatalf("the end carried %T, not the unsubscribed arm", ended.GetHow())
	}
}

// TestASourceThatFinishesAnnouncesTheSourceEndedArm pins the `how` arm for an
// ending nothing asked for and nothing broke on: the login pty exiting.
func TestASourceThatFinishesAnnouncesTheSourceEndedArm(t *testing.T) {
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

	// Act.
	frames <- login.Output{Closed: true}

	// Assert: the closing frame, then the end, naming the source as its cause.
	if !stream.Receive() {
		t.Fatalf("the login terminal's closing frame never arrived: %v", stream.Err())
	}
	if !stream.Receive() {
		t.Fatalf("the page's stream ended instead of announcing the end: %v", stream.Err())
	}
	ended := stream.Msg().GetEnded()
	if ended == nil {
		t.Fatalf("the page's frame was %T, not a subscription end", stream.Msg().GetFrame())
	}
	if ended.GetSourceEnded() == nil {
		t.Fatalf("the end carried %T, not the source_ended arm", ended.GetHow())
	}
}

// TestAFailedSubscriptionEndsAloneAndNamesItsError pins the containment rule
// the whole mux stands on: ONE subscription failing is ONE subscription's news.
// The page's other subscriptions keep delivering and the page's stream stays
// open, because a failure that closed the stream would take every other view on
// the page down with it.
func TestAFailedSubscriptionEndsAloneAndNamesItsError(t *testing.T) {
	// Arrange: two live subscriptions on one page.
	h := newHarness(t)
	surface, ok := h.Server.(*server)
	if !ok {
		t.Fatalf("the surface under test is %T, not the one that holds the page registry", h.Server)
	}
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
	page, attached := surface.pageFor("page-1")
	if !attached {
		t.Fatal("the attached page is not in the registry")
	}

	// Act: the footer's body returns the error a dedicated WatchFooter would
	// have ended its own stream with.
	failure := connect.NewError(connect.CodeInternal, errors.New("the footer resolver broke"))
	surface.endPageSubscription(page, "footer-1", failure)

	// Assert: the failure is that subscription's own frame, named as a failure.
	if !stream.Receive() {
		t.Fatalf("the page's stream ended instead of announcing the failure: %v", stream.Err())
	}
	ended := stream.Msg().GetEnded()
	if ended == nil {
		t.Fatalf("the page's frame was %T, not a subscription end", stream.Msg().GetFrame())
	}
	if ended.GetSubscription() != "footer-1" {
		t.Fatalf("the failure was addressed to %q, not to the subscription that failed", ended.GetSubscription())
	}
	if ended.GetFailed() == nil {
		t.Fatalf("the end carried %T, not the failed arm", ended.GetHow())
	}
	if got := ended.GetFailed().GetCode(); got != connect.CodeInternal.String() {
		t.Fatalf("the failure carried code %q, not the code the dedicated rpc would have ended with", got)
	}

	// And the page is still whole: its other subscription keeps delivering.
	h.Topbar.topic.Publish(topbarView())
	if !stream.Receive() {
		t.Fatalf("one subscription's failure ended the whole page stream: %v", stream.Err())
	}
	push := stream.Msg().GetPush()
	if push == nil || push.GetSubscription() != "topbar-1" {
		t.Fatalf("the page's next frame was %v, not the surviving subscription's push", stream.Msg().GetFrame())
	}
}

// TestAFailedSubscriptionCarriesTheDedicatedRpcsRefusal pins that the `failed`
// arm is the SAME error the dedicated rpc reports, code and sentence alike,
// rather than a second vocabulary for the same conditions.
func TestAFailedSubscriptionCarriesTheDedicatedRpcsRefusal(t *testing.T) {
	// Arrange: the refusal WatchFooter itself raises for an unknown workspace.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	dedicated, dialErr := h.Client.WatchFooter(ctx, connect.NewRequest(&agentreplv1.WatchFooterRequest{
		Workspace: &workspacev1.WorkspaceRef{Id: "no-such-workspace"},
	}))
	var refusal error
	if dialErr != nil {
		refusal = dialErr
	} else {
		dedicated.Receive()
		refusal = dedicated.Err()
	}
	if refusal == nil {
		t.Fatal("WatchFooter served a workspace this daemon does not hold")
	}
	var coded *connect.Error
	if !errors.As(refusal, &coded) {
		t.Fatalf("WatchFooter's refusal was %v, not a Connect error", refusal)
	}

	// Act.
	failed := failedAs(coded)

	// Assert.
	if failed.GetCode() != coded.Code().String() {
		t.Fatalf("the failure carried code %q, not the dedicated rpc's %q", failed.GetCode(), coded.Code().String())
	}
	if failed.GetMessage() != coded.Message() {
		t.Fatalf("the failure carried %q, not the dedicated rpc's own sentence %q", failed.GetMessage(), coded.Message())
	}
}

// TestAPageThatEndsReleasesEverySubscription pins the lifetime rule for the
// client simply going away: every subscription the page held dies with the
// stream and every topic it held is released. A page that leaked would keep the
// daemon fanning views out to nothing for the rest of its life.
func TestAPageThatEndsReleasesEverySubscription(t *testing.T) {
	// Arrange: a page holding subscriptions on four topics.
	h := newHarness(t)
	surface, ok := h.Server.(*server)
	if !ok {
		t.Fatalf("the surface under test is %T, not the one that holds the page registry", h.Server)
	}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	pageCtx, endPage := context.WithCancel(ctx)
	attachPageStream(t, h, pageCtx, "page-1")
	for _, req := range []*agentreplv1.SubscribePageRequest{
		{Page: "page-1", Subscription: "roster-1",
			Request: &agentreplv1.SubscribePageRequest_Roster{
				Roster: &agentreplv1.WatchWorkspaceRosterRequest{},
			}},
		{Page: "page-1", Subscription: "footer-1",
			Request: &agentreplv1.SubscribePageRequest_Footer{
				Footer: &agentreplv1.WatchFooterRequest{Workspace: ref()},
			}},
		{Page: "page-1", Subscription: "topbar-1",
			Request: &agentreplv1.SubscribePageRequest_Topbar{
				Topbar: &agentreplv1.WatchTopbarRequest{Workspace: ref()},
			}},
		{Page: "page-1", Subscription: "holds-1",
			Request: &agentreplv1.SubscribePageRequest_Holds{
				Holds: &agentreplv1.WatchDaemonHoldsRequest{Workspace: ref()},
			}},
	} {
		if _, err := h.Client.SubscribePage(ctx, connect.NewRequest(req)); err != nil {
			t.Fatalf("subscribe %s: %v", req.GetSubscription(), err)
		}
	}
	awaitSubscribers(t, "footer", &h.Footer.topic, 1)

	// Act: the client goes away.
	endPage()

	// Assert: every topic is released and the page's id is free again.
	awaitSubscribers(t, "roster", &h.Sidebar.topic, 0)
	awaitSubscribers(t, "footer", &h.Footer.topic, 0)
	awaitSubscribers(t, "topbar", &h.Topbar.topic, 0)
	awaitSubscribers(t, "holds", &h.Holds.topic, 0)
	deadline := time.Now().Add(2 * time.Second)
	for {
		if _, held := surface.pageFor("page-1"); !held {
			break
		}
		if !time.Now().Before(deadline) {
			t.Fatal("the ended page still holds its id")
		}
		time.Sleep(time.Millisecond)
	}
}

// TestASlowPageDoesNotStallATopicsOtherSubscribers pins the backpressure rule
// the mux inherits: a page whose writer is not draining holds up ITS OWN
// subscriptions and nobody else's, because publish.Topic gives every subscriber
// its own unbounded queue and its own pump. A page on a busy laptop must not be
// able to freeze the Emacs client watching the same view.
func TestASlowPageDoesNotStallATopicsOtherSubscribers(t *testing.T) {
	// Arrange: a page whose writer never runs, so every push it takes blocks
	// forever on the mux's unbuffered hand-off.
	h := newHarness(t)
	surface, ok := h.Server.(*server)
	if !ok {
		t.Fatalf("the surface under test is %T, not the one that holds the page registry", h.Server)
	}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	pageCtx, endPage := context.WithCancel(ctx)
	defer endPage()
	if _, err := surface.attachPage("stalled-page", pageCtx); err != nil {
		t.Fatalf("attach the stalled page: %v", err)
	}
	if _, err := h.Client.SubscribePage(ctx, connect.NewRequest(&agentreplv1.SubscribePageRequest{
		Page: "stalled-page", Subscription: "footer-1",
		Request: &agentreplv1.SubscribePageRequest_Footer{
			Footer: &agentreplv1.WatchFooterRequest{Workspace: ref()},
		},
	})); err != nil {
		t.Fatalf("subscribe the stalled page's footer: %v", err)
	}
	other, err := h.Client.WatchFooter(ctx, connect.NewRequest(&agentreplv1.WatchFooterRequest{
		Workspace: ref(),
	}))
	if err != nil {
		t.Fatalf("open the dedicated footer stream: %v", err)
	}
	awaitSubscribers(t, "footer", &h.Footer.topic, 2)

	// Act: publish more views than any hand-off could absorb.
	for mark := int64(1); mark <= 20; mark++ {
		h.Footer.topic.Publish(footerView(mark))
	}

	// Assert: the healthy subscriber sees every one of them, in order, while
	// the stalled page is still holding its first.
	for want := int64(1); want <= 20; want++ {
		if !other.Receive() {
			t.Fatalf("the healthy subscriber stalled at view %d behind a page that stopped reading: %v", want, other.Err())
		}
		if got := footerMark(other.Msg().GetFooter()); got != want {
			t.Fatalf("the healthy subscriber read view %d, want %d", got, want)
		}
	}
}

// TestAPageSubscriptionReplaysWhatTheDedicatedRpcReplays pins that the mux
// changes the SOCKET and nothing else: a fresh subscription's first frame is
// the frame the dedicated rpc's fresh stream gets, so a view already published
// is drawn by a page exactly as it is drawn by a dedicated watcher.
func TestAPageSubscriptionReplaysWhatTheDedicatedRpcReplays(t *testing.T) {
	// Arrange: a view published BEFORE either watcher exists.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	h.Footer.topic.Publish(footerView(31))

	// Act: the dedicated rpc's first frame, and the page's.
	dedicated, err := h.Client.WatchFooter(ctx, connect.NewRequest(&agentreplv1.WatchFooterRequest{
		Workspace: ref(),
	}))
	if err != nil {
		t.Fatalf("open the dedicated footer stream: %v", err)
	}
	if !dedicated.Receive() {
		t.Fatalf("the dedicated stream carried no first frame: %v", dedicated.Err())
	}
	stream := attachPageStream(t, h, ctx, "page-1")
	if _, err := h.Client.SubscribePage(ctx, connect.NewRequest(&agentreplv1.SubscribePageRequest{
		Page: "page-1", Subscription: "footer-1",
		Request: &agentreplv1.SubscribePageRequest_Footer{
			Footer: &agentreplv1.WatchFooterRequest{Workspace: ref()},
		},
	})); err != nil {
		t.Fatalf("subscribe the footer: %v", err)
	}
	if !stream.Receive() {
		t.Fatalf("the page carried no first frame: %v", stream.Err())
	}

	// Assert: the same response message, byte for byte.
	muxed := stream.Msg().GetPush().GetFooter()
	if muxed == nil {
		t.Fatalf("the page's first frame was %v, not a footer push", stream.Msg().GetFrame())
	}
	if !proto.Equal(dedicated.Msg(), muxed) {
		t.Fatalf("the page replayed %v; the dedicated rpc replayed %v", muxed, dedicated.Msg())
	}
}

// TestSubscribingToAnotherWorkspaceIsRefusedAsTheDedicatedRpcRefuses pins that
// a page has no more reach than a dedicated watcher: a view for a workspace
// this daemon does not hold is refused by NAME, with the same arm and the same
// code the dedicated rpc refuses with. A mux that refused differently would be
// a second authorization surface to keep in step by hand.
func TestSubscribingToAnotherWorkspaceIsRefusedAsTheDedicatedRpcRefuses(t *testing.T) {
	// Arrange: a workspace no page has any right to.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	attachPageStream(t, h, ctx, "page-1")
	elsewhere := &workspacev1.WorkspaceRef{Id: "somebody-elses-workspace"}

	// Act: the dedicated rpc's refusal, and the mux's.
	dedicated, dialErr := h.Client.WatchFooter(ctx, connect.NewRequest(&agentreplv1.WatchFooterRequest{
		Workspace: elsewhere,
	}))
	dedicatedErr := dialErr
	if dedicatedErr == nil {
		dedicated.Receive()
		dedicatedErr = dedicated.Err()
	}
	_, muxedErr := h.Client.SubscribePage(ctx, connect.NewRequest(&agentreplv1.SubscribePageRequest{
		Page: "page-1", Subscription: "footer-1",
		Request: &agentreplv1.SubscribePageRequest_Footer{
			Footer: &agentreplv1.WatchFooterRequest{Workspace: elsewhere},
		},
	}))

	// Assert: the same refusal, named the same way.
	if dedicatedErr == nil {
		t.Fatal("WatchFooter served a workspace this daemon does not hold")
	}
	if muxedErr == nil {
		t.Fatal("a page subscribed to a workspace this daemon does not hold")
	}
	var dedicatedCoded, muxedCoded *connect.Error
	if !errors.As(dedicatedErr, &dedicatedCoded) {
		t.Fatalf("WatchFooter's refusal was %v, not a Connect error", dedicatedErr)
	}
	if !errors.As(muxedErr, &muxedCoded) {
		t.Fatalf("SubscribePage's refusal was %v, not a Connect error", muxedErr)
	}
	if muxedCoded.Code() != dedicatedCoded.Code() {
		t.Fatalf("the mux refused with %v; the dedicated rpc refuses with %v",
			muxedCoded.Code(), dedicatedCoded.Code())
	}
	if muxedCoded.Message() != dedicatedCoded.Message() {
		t.Fatalf("the mux refused with %q; the dedicated rpc refuses with %q",
			muxedCoded.Message(), dedicatedCoded.Message())
	}
}

// TestARefusedSubscriptionAnnouncesNothingOnThePage pins the other half of a
// refusal: SubscribePage's error IS the answer, so the page is told nothing
// about a subscription the client was told does not exist. An `ended` frame for
// it would name an id the client never held.
func TestARefusedSubscriptionAnnouncesNothingOnThePage(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := attachPageStream(t, h, ctx, "page-1")

	// Act: a refused subscription, then a live one whose push must be the very
	// next frame the page reads.
	if _, err := h.Client.SubscribePage(ctx, connect.NewRequest(&agentreplv1.SubscribePageRequest{
		Page: "page-1", Subscription: "footer-refused",
		Request: &agentreplv1.SubscribePageRequest_Footer{
			Footer: &agentreplv1.WatchFooterRequest{Workspace: &workspacev1.WorkspaceRef{Id: "no-such-workspace"}},
		},
	})); err == nil {
		t.Fatal("a subscription for an unknown workspace was accepted")
	}
	if _, err := h.Client.SubscribePage(ctx, connect.NewRequest(&agentreplv1.SubscribePageRequest{
		Page: "page-1", Subscription: "topbar-1",
		Request: &agentreplv1.SubscribePageRequest_Topbar{
			Topbar: &agentreplv1.WatchTopbarRequest{Workspace: ref()},
		},
	})); err != nil {
		t.Fatalf("subscribe the topbar: %v", err)
	}
	h.Topbar.topic.Publish(topbarView())

	// Assert.
	if !stream.Receive() {
		t.Fatalf("the page's stream ended: %v", stream.Err())
	}
	if ended := stream.Msg().GetEnded(); ended != nil {
		t.Fatalf("the page was told subscription %q ended; it was refused and never existed",
			ended.GetSubscription())
	}
	if push := stream.Msg().GetPush(); push == nil || push.GetSubscription() != "topbar-1" {
		t.Fatalf("the page's frame was %v, not the live subscription's push", stream.Msg().GetFrame())
	}
}

// TestWatchPageIsNotHeldOpenByTheExitsGrace pins the page stream's standing
// with the write barrier's gate: a `Watch*` handler returns only when its
// client goes away, so counting one would make every orderly exit spend its
// whole grace on it. The set is DERIVED from the service descriptor, so this
// asserts the derivation covers the mux's own stream.
func TestWatchPageIsNotHeldOpenByTheExitsGrace(t *testing.T) {
	// Arrange, Act.
	standing := standingStreamPaths[agentreplv1connect.AgentReplWatchPageProcedure]

	// Assert.
	if !standing {
		t.Fatal("WatchPage is counted as an ordinary call; an exit would wait out its whole grace on a page that never leaves")
	}
}

// TestSubscribePageIsHeldOpenUntilItsAnswerIsWritten pins the other half: the
// mux's verbs are UNARY, so they are counted and their answers are held by the
// write barrier exactly as a dedicated stream's open is. An exit that ran over
// a SubscribePage answer would leave a page believing a subscription exists.
func TestSubscribePageIsHeldOpenUntilItsAnswerIsWritten(t *testing.T) {
	// Arrange, Act.
	standing := standingStreamPaths[agentreplv1connect.AgentReplSubscribePageProcedure]

	// Assert.
	if standing {
		t.Fatal("SubscribePage is excluded from the exit's gate; its answer could be cut off the wire")
	}
}
