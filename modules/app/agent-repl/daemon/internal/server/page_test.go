package server

import (
	"agent-repld/internal/dlog"
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"testing"
	"time"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

	"agent-repld/internal/login"
	"agent-repld/internal/publish"

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

func TestPageAttachStatesTheInstalledWebapp(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Deployer.webapp = "webapp-0443"
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act.
	stream := attachPageStream(t, h, ctx, "page-1")

	// Assert.
	if got := stream.Msg().GetAttached().GetWebappBuild(); got != "webapp-0443" {
		t.Fatalf("attached webapp_build = %q, want the installed build", got)
	}
}

func TestAPageIsNotAttachedWhenTheInstalledWebappCannotBeRead(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })
	h.Deployer.webappErr = errors.New("dist/index.html: no such file")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act.
	stream, err := h.Client.WatchPage(ctx, connect.NewRequest(&agentreplv1.WatchPageRequest{Page: "page-1"}))
	if err != nil {
		t.Fatalf("open the page's stream: %v", err)
	}
	received := stream.Receive()

	// Assert.
	if received || connect.CodeOf(stream.Err()) != connect.CodeInternal {
		t.Fatalf("Receive = %v, err %v; want the stream refused Internal", received, stream.Err())
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: "WatchPage", Message: "could not read the installed webapp's build; the page is not attached"}); !ok {
		t.Fatalf("records = %+v, want the refusal at ERROR", log.Records())
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
	subscribePage(t, ctx, h, &agentreplv1.SubscribePageRequest{
		Page:         "page-1",
		Subscription: "footer-1",
		Request: &agentreplv1.SubscribePageRequest_Footer{
			Footer: &agentreplv1.WatchFooterRequest{Workspace: ref()},
		},
	})
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
	subscribePage(t, ctx, h, &agentreplv1.SubscribePageRequest{
		Page:         "page-1",
		Subscription: "footer-1",
		Request: &agentreplv1.SubscribePageRequest_Footer{
			Footer: &agentreplv1.WatchFooterRequest{Workspace: ref()},
		},
	})
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
		subscribePage(t, ctx, h, sub.req)
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

// footerSubscription is a footer subscription on PAGE under ATTACHMENT, nil
// for the attachment the page stands under.
func footerSubscription(page string, attachment *agentreplv1.PageAttachment) *agentreplv1.SubscribePageRequest {
	return &agentreplv1.SubscribePageRequest{
		Page:         page,
		Attachment:   attachment,
		Subscription: "footer-1",
		Request: &agentreplv1.SubscribePageRequest_Footer{
			Footer: &agentreplv1.WatchFooterRequest{Workspace: ref()},
		},
	}
}

// TestSubscribePageAnswersPageGoneForAnAttachmentThatIsOver pins every way an
// attachment is over: each is answered `page_gone`, never a refusal, and
// subscribes nothing.
func TestSubscribePageAnswersPageGoneForAnAttachmentThatIsOver(t *testing.T) {
	cases := []struct {
		name string
		// attached is whether page-1 holds a stream when the call arrives.
		attached bool
		// attachment builds what the call echoes, from the one the page
		// stands under.
		attachment func(current *agentreplv1.PageAttachment) *agentreplv1.PageAttachment
		why        string
	}{
		{
			name:       "the page holds no stream on this daemon",
			attachment: func(current *agentreplv1.PageAttachment) *agentreplv1.PageAttachment { return current },
			why:        "the page holds no stream on this daemon",
		},
		{
			name:     "the page was issued a newer attachment",
			attached: true,
			attachment: func(current *agentreplv1.PageAttachment) *agentreplv1.PageAttachment {
				return &agentreplv1.PageAttachment{Instance: current.GetInstance(), Sequence: current.GetSequence() + 1}
			},
			why: "the page was issued a newer attachment",
		},
		{
			name:     "the daemon that issued the attachment no longer runs",
			attached: true,
			attachment: func(current *agentreplv1.PageAttachment) *agentreplv1.PageAttachment {
				return &agentreplv1.PageAttachment{Instance: "daemon-that-exited", Sequence: current.GetSequence()}
			},
			why: "the daemon that issued the attachment no longer runs",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			log := &recordingLogger{}
			h := newHarness(t, func(d *Deps) { d.Log = &fakeSurfaces{global: log} })
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			if tc.attached {
				attachPageStream(t, h, ctx, "page-1")
			}
			req := footerSubscription("page-1", tc.attachment(h.attachmentOf("page-1")))

			// Act.
			resp, err := h.Unmarked.SubscribePage(ctx, connect.NewRequest(req))

			// Assert.
			if err != nil || resp.Msg.GetSuccess().GetPageGone() == nil {
				t.Fatalf("SubscribePage = (%v, %v), want page_gone", resp, err)
			}
			if page, held := h.Server.(*server).pageFor("page-1"); held {
				page.mu.Lock()
				subs := len(page.subs)
				page.mu.Unlock()
				if subs != 0 {
					t.Fatalf("the page holds %d subscriptions after page_gone", subs)
				}
			}
			var recorded bool
			for _, rec := range log.at("INFO") {
				if rec.Operation == "daemon.server.page_gone" && rec.Context["why"] == tc.why &&
					rec.Context["rpc"] == "SubscribePage" && rec.Context["page"] == "page-1" {
					recorded = true
				}
			}
			if !recorded {
				t.Fatalf("no page_gone record names %q; got %v", tc.why, log.records)
			}
		})
	}
}

// TestAPageCallWithNoAttachmentIsMalformed pins that the attachment is
// required on both page verbs: a call that names no daemon cannot be routed
// to the one holding its page.
func TestAPageCallWithNoAttachmentIsMalformed(t *testing.T) {
	cases := []struct {
		name string
		call func(ctx context.Context, h *harness) error
	}{
		{name: "SubscribePage", call: func(ctx context.Context, h *harness) error {
			_, err := h.Client.SubscribePage(ctx, connect.NewRequest(footerSubscription("page-1", &agentreplv1.PageAttachment{})))
			return err
		}},
		{name: "UnsubscribePage", call: func(ctx context.Context, h *harness) error {
			_, err := h.Client.UnsubscribePage(ctx, connect.NewRequest(&agentreplv1.UnsubscribePageRequest{
				Page: "page-1", Subscription: "footer-1", Attachment: &agentreplv1.PageAttachment{},
			}))
			return err
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			attachPageStream(t, h, ctx, "page-1")

			// Act.
			err := tc.call(ctx, h)

			// Assert.
			if err == nil {
				t.Fatal("a page call with no attachment was served")
			}
			if got := connectCode(t, err); got != connect.CodeInvalidArgument {
				t.Fatalf("the refusal came back as %v, not invalid_argument", got)
			}
		})
	}
}

// TestPageAttachIssuesAnAttachment pins what the latch hands the page: the
// daemon's own instance and a sequence, which every later page call echoes.
func TestPageAttachIssuesAnAttachment(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act.
	stream := attachPageStream(t, h, ctx, "page-1")

	// Assert.
	got := stream.Msg().GetAttached().GetAttachment()
	held, _ := h.Server.(*server).pageFor("page-1")
	if got.GetInstance() != string(testInstance) || got.GetSequence() == 0 || got.GetSequence() != held.sequence {
		t.Fatalf("the attachment issued was %v, want this daemon's instance and the page's sequence %d", got, held.sequence)
	}
}

// TestUnsubscribePageUnderAnAttachmentThatIsOverEndsNothing pins that an
// unsubscribe left over from an earlier attachment cannot end a subscription
// the page made under its current one, even when both use the same id.
func TestUnsubscribePageUnderAnAttachmentThatIsOverEndsNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	attachPageStream(t, h, ctx, "page-1")
	subscribePage(t, ctx, h, footerSubscription("page-1", nil))
	stale := h.attachmentOf("page-1")
	stale.Sequence++

	// Act.
	_, err := h.Client.UnsubscribePage(ctx, connect.NewRequest(&agentreplv1.UnsubscribePageRequest{
		Page: "page-1", Subscription: "footer-1", Attachment: stale,
	}))

	// Assert.
	if err != nil {
		t.Fatalf("UnsubscribePage: %v", err)
	}
	page, _ := h.Server.(*server).pageFor("page-1")
	page.mu.Lock()
	_, live := page.subs["footer-1"]
	page.mu.Unlock()
	if !live {
		t.Fatal("an unsubscribe under another attachment ended the subscription")
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
	subscribePage(t, ctx, h, first)

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

// TestASecondStreamForOnePageReplacesTheFirst pins what a page that reopens
// before the daemon has unwound its earlier stream gets: the earlier stream
// ends, and the second is attached under its own attachment.
func TestASecondStreamForOnePageReplacesTheFirst(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	first, err := h.Client.WatchPage(ctx, connect.NewRequest(&agentreplv1.WatchPageRequest{Page: "page-1"}))
	if err != nil {
		t.Fatalf("open the page's stream: %v", err)
	}
	if !first.Receive() {
		t.Fatalf("the first stream ended before it attached: %v", first.Err())
	}
	firstAttachment := first.Msg().GetAttached().GetAttachment()

	// Act.
	second := attachPageStream(t, h, ctx, "page-1")

	// Assert.
	if first.Receive() {
		t.Fatalf("the replaced stream was served frame %T", first.Msg().GetFrame())
	}
	if err := first.Err(); err != nil {
		t.Fatalf("the replaced stream ended in %v, want a clean end", err)
	}
	if got := second.Msg().GetAttached().GetAttachment().GetSequence(); got == firstAttachment.GetSequence() {
		t.Fatalf("both streams were issued sequence %d", got)
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
				Watch: &agentreplv1.FeedWatchToken{Value: "never-minted", Instance: string(testInstance)},
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
		subscribePage(t, ctx, h, req)
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
	subscribePage(t, ctx, h, &agentreplv1.SubscribePageRequest{
		Page: "page-1", Subscription: "login-1",
		Request: &agentreplv1.SubscribePageRequest_LoginTerminal{
			LoginTerminal: &agentreplv1.WatchLoginTerminalRequest{Workspace: ref()},
		},
	})

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
	page := surface.attachPage("page-1", ctx, cancel)

	// Act.
	surface.detachPage(page)

	// Assert.
	if _, held := surface.pageFor("page-1"); held {
		t.Fatal("a detached page still holds its id")
	}
}

// TestAttachingOneIdTwiceEndsTheEarlierStream pins the replacement at the
// registry's own level: the second attachment of one page id ends the first's
// stream and stands in its place under a sequence of its own.
func TestAttachingOneIdTwiceEndsTheEarlierStream(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	surface := h.Server.(*server)
	olderCtx, endOlder := context.WithCancel(context.Background())
	defer endOlder()
	older := surface.attachPage("page-1", olderCtx, endOlder)
	newerCtx, endNewer := context.WithCancel(context.Background())
	defer endNewer()

	// Act.
	newer := surface.attachPage("page-1", newerCtx, endNewer)

	// Assert.
	if olderCtx.Err() == nil {
		t.Fatal("the earlier stream was left standing")
	}
	if newerCtx.Err() != nil {
		t.Fatal("the newer stream was ended")
	}
	if held, _ := surface.pageFor("page-1"); held != newer {
		t.Fatal("the registry does not hold the newer attachment")
	}
	if newer.sequence == older.sequence {
		t.Fatalf("both attachments were issued sequence %d", newer.sequence)
	}
}

// TestDetachingAReplacedStreamLeavesItsReplacementAttached pins that the
// earlier stream's late detach takes nothing from the page that replaced it:
// not its registration, and not the opens' knowledge that the page is here.
func TestDetachingAReplacedStreamLeavesItsReplacementAttached(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	surface := h.Server.(*server)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	older := surface.attachPage("page-1", ctx, cancel)
	newer := surface.attachPage("page-1", ctx, cancel)

	// Act.
	surface.detachPage(older)

	// Assert.
	if held, _ := surface.pageFor("page-1"); held != newer {
		t.Fatal("the replaced stream's detach removed its replacement")
	}
	present := pagesHeld(surface, "page-1")
	if present != 1 {
		t.Fatalf("the page is held %d times after the replaced stream detached, want 1", present)
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
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
	defer cancel()
	if err := topic.AwaitSubscribers(ctx, want); err != nil {
		t.Fatalf("the %s topic: %v", what, err)
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
	subscribePage(t, ctx, h, &agentreplv1.SubscribePageRequest{
		Page: "page-1", Subscription: "footer-1",
		Request: &agentreplv1.SubscribePageRequest_Footer{
			Footer: &agentreplv1.WatchFooterRequest{Workspace: ref()},
		},
	})
	h.Footer.topic.Publish(footerView(1))
	if !stream.Receive() {
		t.Fatalf("the first subscription never delivered: %v", stream.Err())
	}

	// Act: a second subscription, opened while the first is live.
	subscribePage(t, ctx, h, &agentreplv1.SubscribePageRequest{
		Page: "page-1", Subscription: "topbar-1",
		Request: &agentreplv1.SubscribePageRequest_Topbar{
			Topbar: &agentreplv1.WatchTopbarRequest{Workspace: ref()},
		},
	})
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
		subscribePage(t, ctx, h, req)
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
	subscribePage(t, ctx, h, &agentreplv1.SubscribePageRequest{
		Page: "page-1", Subscription: "footer-1",
		Request: &agentreplv1.SubscribePageRequest_Footer{
			Footer: &agentreplv1.WatchFooterRequest{Workspace: ref()},
		},
	})

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
	subscribePage(t, ctx, h, &agentreplv1.SubscribePageRequest{
		Page: "page-1", Subscription: "login-1",
		Request: &agentreplv1.SubscribePageRequest_LoginTerminal{
			LoginTerminal: &agentreplv1.WatchLoginTerminalRequest{Workspace: ref()},
		},
	})

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
		subscribePage(t, ctx, h, req)
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
	} {
		subscribePage(t, ctx, h, req)
	}
	awaitSubscribers(t, "footer", &h.Footer.topic, 1)

	// Act: the client goes away.
	endPage()

	// Assert: every topic is released and the page's id is free again.
	awaitSubscribers(t, "roster", &h.Sidebar.topic, 0)
	awaitSubscribers(t, "footer", &h.Footer.topic, 0)
	awaitSubscribers(t, "topbar", &h.Topbar.topic, 0)
	detached, endWait := context.WithTimeout(context.Background(), 2*time.Second)
	defer endWait()
	if _, err := h.OpenWorld.log.Await(detached, dlog.RecordQuery{
		Operation: "daemon.opentabs.page", Message: "a page stream ended; its facts are forgotten",
	}); err != nil {
		t.Fatalf("the ended page was never detached: %v", err)
	}
	if _, held := surface.pageFor("page-1"); held {
		t.Fatal("the ended page still holds its id")
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
	surface.attachPage("stalled-page", pageCtx, endPage)
	subscribePage(t, ctx, h, &agentreplv1.SubscribePageRequest{
		Page: "stalled-page", Subscription: "footer-1",
		Request: &agentreplv1.SubscribePageRequest_Footer{
			Footer: &agentreplv1.WatchFooterRequest{Workspace: ref()},
		},
	})
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
	subscribePage(t, ctx, h, &agentreplv1.SubscribePageRequest{
		Page: "page-1", Subscription: "footer-1",
		Request: &agentreplv1.SubscribePageRequest_Footer{
			Footer: &agentreplv1.WatchFooterRequest{Workspace: ref()},
		},
	})
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
	subscribePage(t, ctx, h, &agentreplv1.SubscribePageRequest{
		Page: "page-1", Subscription: "topbar-1",
		Request: &agentreplv1.SubscribePageRequest_Topbar{
			Topbar: &agentreplv1.WatchTopbarRequest{Workspace: ref()},
		},
	})
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

// TestASubscriptionThatAcceptsAndEndsAtOnceIsStillAccepted pins what
// SubscribePage answers for a watch that accepts and finishes in the same
// breath — a login terminal whose pty is already gone. The subscription DID
// exist and the page's own stream announces its end, so answering a refusal
// would tell the client something the page's frames contradict.
//
// It is taken fifty times because acceptance and ending arrive on two separate
// channels the handler selects over, and an answer that depended on which the
// scheduler made ready first would be an answer that varies run to run.
func TestASubscriptionThatAcceptsAndEndsAtOnceIsStillAccepted(t *testing.T) {
	// Arrange: a login source that is already over, so every subscription
	// accepts and returns in the same breath.
	closed := make(chan login.Output)
	close(closed)
	h := newHarness(t)
	h.Login.watchFrames = closed
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := attachPageStream(t, h, ctx, "page-1")
	drained := make(chan struct{})
	go func() {
		defer close(drained)
		for stream.Receive() {
		}
	}()

	// Act, Assert.
	for attempt := 0; attempt < 50; attempt++ {
		id := fmt.Sprintf("login-%d", attempt)
		// A subscription accepted and ended at once must be answered accepted.
		subscribePage(t, ctx, h, &agentreplv1.SubscribePageRequest{
			Page: "page-1", Subscription: id,
			Request: &agentreplv1.SubscribePageRequest_LoginTerminal{
				LoginTerminal: &agentreplv1.WatchLoginTerminalRequest{Workspace: ref()},
			},
		})
	}
	cancel()
	<-drained
}

// pageFramesToEnd reads a page's stream to its end, answering every frame and
// the end's error.
func pageFramesToEnd(stream *connect.ServerStreamForClient[agentreplv1.WatchPageResponse]) ([]*agentreplv1.WatchPageResponse, error) {
	var frames []*agentreplv1.WatchPageResponse
	for stream.Receive() {
		frames = append(frames, stream.Msg())
	}
	return frames, stream.Err()
}

// TestAPlannedCloseEndsAPageInOrder pins the page's planned exit: each
// subscription's own planned ending first, where its rpc has one, then the
// page's `ending` as the LAST frame, and no `ended` frame for a subscription
// the exit ended. The webhost's page read the bare end of its stream as a cut
// link and drew the unreachable chip on every handover (2026-10-24).
func TestAPlannedCloseEndsAPageInOrder(t *testing.T) {
	tests := []struct {
		name string
		// subscribe opens the subscription and proves it live by reading one
		// push off the page.
		subscribe   func(t *testing.T, ctx context.Context, h *harness, stream *connect.ServerStreamForClient[agentreplv1.WatchPageResponse])
		wantEndings int
	}{
		{
			name: "a roster subscription sends the roster's ending",
			subscribe: func(t *testing.T, ctx context.Context, h *harness, stream *connect.ServerStreamForClient[agentreplv1.WatchPageResponse]) {
				subscribePage(t, ctx, h, &agentreplv1.SubscribePageRequest{Page: "page-1", Subscription: "roster-1",
					Request: &agentreplv1.SubscribePageRequest_Roster{Roster: &agentreplv1.WatchWorkspaceRosterRequest{}}})
				h.Sidebar.topic.Publish(wholePush(1))
				if !stream.Receive() || stream.Msg().GetPush().GetRoster().GetStreams() == nil {
					t.Fatalf("prove the roster subscription: %v", stream.Err())
				}
			},
			wantEndings: 1,
		},
		{
			name: "a footer subscription, whose rpc has no ending, sends nothing more",
			subscribe: func(t *testing.T, ctx context.Context, h *harness, stream *connect.ServerStreamForClient[agentreplv1.WatchPageResponse]) {
				subscribePage(t, ctx, h, &agentreplv1.SubscribePageRequest{Page: "page-1", Subscription: "footer-1",
					Request: &agentreplv1.SubscribePageRequest_Footer{Footer: &agentreplv1.WatchFooterRequest{Workspace: ref()}}})
				h.Footer.topic.Publish(footerView(7))
				if !stream.Receive() || stream.Msg().GetPush().GetFooter() == nil {
					t.Fatalf("prove the footer subscription: %v", stream.Err())
				}
			},
			wantEndings: 0,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			stream := attachPageStream(t, h, ctx, "page-1")
			tt.subscribe(t, ctx, h, stream)

			// Act
			if err := h.Server.Close(); err != nil {
				t.Fatalf("Close: %v", err)
			}
			frames, err := pageFramesToEnd(stream)

			// Assert
			if err != nil {
				t.Fatalf("the page's stream ended with %v, want a clean end", err)
			}
			if len(frames) == 0 || frames[len(frames)-1].GetEnding() == nil {
				t.Fatalf("frames = %v, want the page's ending last", frames)
			}
			endings := 0
			for _, frame := range frames[:len(frames)-1] {
				if frame.GetEnded() != nil {
					t.Fatalf("frames = %v, want no ended frame for a subscription the exit ended", frames)
				}
				if frame.GetPush().GetRoster().GetEnding() != nil {
					endings++
				}
			}
			if endings != tt.wantEndings {
				t.Fatalf("subscription endings = %d, want %d; frames = %v", endings, tt.wantEndings, frames)
			}
		})
	}
}

// TestAPageClosingAnswersANewSubscriptionPageGone pins the drain's bound:
// once a planned exit drains a page, nothing new may join what it waits out,
// and the page is told its attachment is over rather than refused.
func TestAPageClosingAnswersANewSubscriptionPageGone(t *testing.T) {
	// Arrange: a page whose drain has begun.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	attachPageStream(t, h, ctx, "page-1")
	page, ok := h.Server.(*server).pageFor("page-1")
	if !ok {
		t.Fatal("the attached page is not registered")
	}
	page.mu.Lock()
	page.closing = true
	page.mu.Unlock()

	// Act
	resp, err := h.Client.SubscribePage(ctx, connect.NewRequest(&agentreplv1.SubscribePageRequest{
		Page: "page-1", Subscription: "footer-1",
		Request: &agentreplv1.SubscribePageRequest_Footer{Footer: &agentreplv1.WatchFooterRequest{Workspace: ref()}},
	}))

	// Assert
	if err != nil || resp.Msg.GetSuccess().GetPageGone() == nil {
		t.Fatalf("SubscribePage = (%v, %v), want page_gone", resp, err)
	}
}

// subscribePage subscribes REQ on its page, failing the test on a refusal.
func subscribePage(t *testing.T, ctx context.Context, h *harness, req *agentreplv1.SubscribePageRequest) {
	t.Helper()
	resp, err := h.Client.SubscribePage(ctx, connect.NewRequest(req))
	if err != nil {
		t.Fatalf("subscribe %s: %v", req.GetSubscription(), err)
	}
	if resp.Msg.GetSuccess().GetSubscribed() == nil {
		t.Fatalf("subscribe %s answered %v, want subscribed", req.GetSubscription(), resp.Msg)
	}
}

// TestPageTestsSubscribeThroughTheHelper keeps every subscription a test
// expects to be accepted on subscribePage, so the accepted path is spelled
// once: a hand-rolled SubscribePage that fails the test on its error is a
// second copy of the helper.
func TestPageTestsSubscribeThroughTheHelper(t *testing.T) {
	// Arrange
	helperDef := regexp.MustCompile(`(?s)\nfunc subscribePage\(.*?\n\}\n`)
	handRolled := regexp.MustCompile(`if _, err := h\.Client\.SubscribePage\(ctx, connect\.NewRequest\([^;]*?\)\); err != nil \{\n\s*t\.Fatalf`)
	names, err := filepath.Glob("*_test.go")
	if err != nil {
		t.Fatalf("glob: %v", err)
	}

	// Act
	var offenders []string
	for _, name := range names {
		source, err := os.ReadFile(name)
		if err != nil {
			t.Fatalf("read %s: %v", name, err)
		}
		// The helper's own body is the one spelling the rule allows.
		if handRolled.Match(helperDef.ReplaceAll(source, nil)) {
			offenders = append(offenders, name)
		}
	}

	// Assert
	if len(offenders) != 0 {
		t.Fatalf("%v subscribe a page by hand; use subscribePage", offenders)
	}
}

// TestAPageAttachingIsRecordedAtInfo pins that a page's attach is page
// lifecycle, recorded at INFO: an e2e whose page never attached could not tell
// that from one that attached and never drew.
func TestAPageAttachingIsRecordedAtInfo(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act.
	attachPageStream(t, h, ctx, "page-1")

	// Assert.
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Operation: "WatchPage", Message: "attached a page's one standing stream"}); !ok {
		t.Fatalf("records = %+v, want the page's attach at INFO", log.Records())
	}
}

func TestAHandedAwayRefusalFailsTheSubscriptionNamingTheSuccessor(t *testing.T) {
	// Arrange.
	coded := transportClosed(nil, "WatchFooter", "transferring_away", "handed away", false, "/sock/successor.sock")

	// Act.
	failed := failedAs(coded)

	// Assert.
	if got := failed.GetHandedAway().GetAddress(); got != "/sock/successor.sock" {
		t.Fatalf("handed_away = %+v, want the successor named", failed.GetHandedAway())
	}
}

func TestAnyOtherRefusalFailsTheSubscriptionHandedNowhere(t *testing.T) {
	// Arrange.
	coded := transportClosed(nil, "WatchFooter", "not_yet_adopted", "not adopted yet", false, "/sock/other.sock")

	// Act.
	failed := failedAs(coded)

	// Assert.
	if failed.HandedAway != nil {
		t.Fatalf("handed_away = %+v, want unset", failed.GetHandedAway())
	}
}

// TestAPageRosterSubscriptionSendsOnlyItsUnseenStreams pins the page-mux arm
// of the roster: its first push carries every stream, and a change to one
// stream carries that stream alone, as the dedicated rpc's do.
func TestAPageRosterSubscriptionSendsOnlyItsUnseenStreams(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := attachPageStream(t, h, ctx, "page-1")
	subscribePage(t, ctx, h, &agentreplv1.SubscribePageRequest{Page: "page-1", Subscription: "roster-1",
		Request: &agentreplv1.SubscribePageRequest_Roster{Roster: &agentreplv1.WatchWorkspaceRosterRequest{}}})
	first := wholePush(1)

	// Act.
	h.Sidebar.topic.Publish(first)
	var frames [][]string
	for _, push := range []*frontendv1.RosterPush{nil, nextPush(first)} {
		if push != nil {
			h.Sidebar.topic.Publish(push)
		}
		if !stream.Receive() {
			t.Fatalf("the page's stream ended before the roster push: %v", stream.Err())
		}
		frames = append(frames, carried(stream.Msg().GetPush().GetRoster().GetStreams()))
	}

	// Assert.
	want := [][]string{everyStream, {"workspaces"}}
	if !slices.EqualFunc(frames, want, slices.Equal) {
		t.Fatalf("frames carried %v, want %v", frames, want)
	}
}
