package server

import (
	"context"
	"errors"
	"net/http"
	"net/http/httptest"
	"net/url"
	"strings"
	"testing"
	"time"

	"connectrpc.com/connect"
	"golang.org/x/net/http2"
	"golang.org/x/net/http2/h2c"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"

	"agent-repld/internal/ids"
	"agent-repld/internal/workspace"
)

// relayBound bounds one relayed frame's arrival in these tests: two in-process
// h2c hops on loopback. MEASURED 2026-11-19: each relayed case ran in under
// 10ms on an idle host (the runner records no wall time below that). The
// bound is the one this package already gives the same kind of in-process
// hand-off under the suite's own load (awaitSubscribers), not a figure of its
// own.
const relayBound = 2 * time.Second

// pagesHeld answers how many holders SURFACE counts for page ID: its own
// stream here, and every relayed subscription run here for it.
func pagesHeld(surface *server, id string) int {
	surface.mu.Lock()
	defer surface.mu.Unlock()
	return surface.pagePresence[id]
}

// servingElsewhereAt builds a surface that does not serve the test workspace,
// whose serving row names the daemon at TARGET.
func servingElsewhereAt(t *testing.T, target string) *harness {
	t.Helper()
	return newHarness(t, func(d *Deps) {
		d.Ownership = &fakeOwnership{standing: workspace.StandingNotYetAdopted}
		d.ServingElsewhere = func(context.Context, ids.WorkspaceID) (string, bool, error) {
			return target, true, nil
		}
	})
}

// scriptedFooterDaemon serves WatchFooter as SERVE says and nothing else, on
// h2c, and answers its host. It stands for the daemon serving a workspace.
func scriptedFooterDaemon(t *testing.T, serve func(context.Context, *connect.Request[agentreplv1.WatchFooterRequest], *connect.ServerStream[agentreplv1.WatchFooterResponse]) error) string {
	t.Helper()
	mux := http.NewServeMux()
	mux.Handle(agentreplv1connect.AgentReplWatchFooterProcedure,
		connect.NewServerStreamHandler(agentreplv1connect.AgentReplWatchFooterProcedure, serve))
	there := httptest.NewServer(h2c.NewHandler(mux, &http2.Server{}))
	t.Cleanup(there.Close)
	target, err := url.Parse(there.URL)
	if err != nil {
		t.Fatal(err)
	}
	return target.Host
}

// nextPageFrame reads the page stream's next frame under relayBound.
func nextPageFrame(t *testing.T, cancel context.CancelFunc, stream *connect.ServerStreamForClient[agentreplv1.WatchPageResponse]) *agentreplv1.WatchPageResponse {
	t.Helper()
	timer := time.AfterFunc(relayBound, cancel)
	defer timer.Stop()
	if !stream.Receive() {
		t.Fatalf("the page's stream gave no frame within %s: %v", relayBound, stream.Err())
	}
	return stream.Msg()
}

func TestAViewOfAWorkspaceServedElsewhereIsRelayedOntoThePage(t *testing.T) {
	// Arrange: the daemon serving the workspace, and the one holding the page.
	there := newHarness(t, func(d *Deps) { d.Instance = "daemon-there" })
	here := servingElsewhereAt(t, hostOf(t, there))
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := attachPageStream(t, here, ctx, "page-1")
	subscribePage(t, ctx, here, footerSubscription("page-1", nil))

	// Act
	there.Footer.topic.Publish(footerView(42))

	// Assert
	push := nextPageFrame(t, cancel, stream).GetPush()
	if push.GetSubscription() != "footer-1" || footerMark(push.GetFooter().GetFooter()) != 42 {
		t.Fatalf("the page was sent %v, want the serving daemon's footer on footer-1", push)
	}
}

func TestAViewWhoseServingDaemonNoLongerAcceptsIsServedHere(t *testing.T) {
	// Arrange: the serving row names a daemon whose listener is closed, and
	// this daemon has not adopted the workspace yet.
	here := servingElsewhereAt(t, refusingAddress(t))
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	attachPageStream(t, here, ctx, "page-1")

	// Act
	_, err := here.Client.SubscribePage(ctx, connect.NewRequest(footerSubscription("page-1", nil)))

	// Assert: the view's own body ran here and answered the handover's
	// refusal, which the page waits out; the refused dial never reached it.
	if connect.CodeOf(err) != connect.CodeFailedPrecondition || !strings.Contains(err.Error(), "not_yet_adopted") {
		t.Fatalf("SubscribePage = %v, want this daemon's own not_yet_adopted refusal", err)
	}
	if strings.Contains(err.Error(), "connection refused") {
		t.Fatalf("SubscribePage = %v, want no dial failure passed to the page", err)
	}
}

func TestARelayedSubscriptionAnswersOnceItsTargetAcceptsWithNothingPushed(t *testing.T) {
	// Arrange: a web link pushes nothing when it is accepted.
	there := newHarness(t, func(d *Deps) { d.Instance = "daemon-there" })
	here := servingElsewhereAt(t, hostOf(t, there))
	ctx, cancel := context.WithTimeout(context.Background(), relayBound)
	defer cancel()
	attachPageStream(t, here, ctx, "page-1")

	// Act
	resp, err := here.Client.SubscribePage(ctx, connect.NewRequest(&agentreplv1.SubscribePageRequest{
		Page: "page-1", Subscription: "web-1",
		Request: &agentreplv1.SubscribePageRequest_WebWorkspace{
			WebWorkspace: &agentreplv1.WatchWebWorkspaceRequest{Workspace: ref(), WebappBuild: "webapp-test"},
		},
	}))

	// Assert
	if err != nil || resp.Msg.GetSuccess().GetSubscribed() == nil {
		t.Fatalf("SubscribePage = (%v, %v), want subscribed before any push", resp, err)
	}
}

func TestARelayedSubscriptionItsTargetRefusesIsRefused(t *testing.T) {
	// Arrange: the daemon the serving row names does not serve the workspace
	// either.
	there := newHarness(t, func(d *Deps) {
		d.Instance = "daemon-there"
		d.Ownership = &fakeOwnership{standing: workspace.StandingNotYetAdopted}
	})
	here := servingElsewhereAt(t, hostOf(t, there))
	ctx, cancel := context.WithTimeout(context.Background(), relayBound)
	defer cancel()
	attachPageStream(t, here, ctx, "page-1")

	// Act
	_, err := here.Client.SubscribePage(ctx, connect.NewRequest(footerSubscription("page-1", nil)))

	// Assert
	if err == nil {
		t.Fatal("a subscription its target refused was accepted")
	}
	if got := connectCode(t, err); got != connect.CodeFailedPrecondition {
		t.Fatalf("the refusal came back as %v, not the target's failed_precondition", got)
	}
}

func TestARelayedSubscriptionTellsItsTargetWhichPageItPushesTo(t *testing.T) {
	// Arrange
	seen := make(chan http.Header, 1)
	target := scriptedFooterDaemon(t, func(ctx context.Context, req *connect.Request[agentreplv1.WatchFooterRequest], stream *connect.ServerStream[agentreplv1.WatchFooterResponse]) error {
		seen <- req.Header()
		stream.ResponseHeader().Set(AcceptedHeader, "1")
		if err := stream.Send(&agentreplv1.WatchFooterResponse{}); err != nil {
			return err
		}
		<-ctx.Done()
		return ctx.Err()
	})
	here := servingElsewhereAt(t, target)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	attachPageStream(t, here, ctx, "page-1")

	// Act
	subscribePage(t, ctx, here, footerSubscription("page-1", nil))

	// Assert
	header := <-seen
	if header.Get(RelayedPageHeader) != "page-1" || header.Get(ForwardedHeader) == "" {
		t.Fatalf("the target saw page %q and forwarded mark %q", header.Get(RelayedPageHeader), header.Get(ForwardedHeader))
	}
}

func TestARelayedSubscriptionEndsAsItsTargetEndedIt(t *testing.T) {
	handedAway := connect.NewError(connect.CodeFailedPrecondition, errors.New("handed on"))
	handedAway.Meta().Set(SuccessorHeader, "127.0.0.1:7")
	cases := []struct {
		name string
		// end is what the target's stream returns after its one push.
		end error
		// want reads the arm the page is told.
		want func(*agentreplv1.PageSubscriptionEnded) bool
	}{
		{
			name: "a clean end is moved, and the page subscribes again",
			want: func(e *agentreplv1.PageSubscriptionEnded) bool { return e.GetMoved() != nil },
		},
		{
			name: "an error is the failure the dedicated rpc would have carried",
			end:  connect.NewError(connect.CodeInternal, errors.New("the view broke")),
			want: func(e *agentreplv1.PageSubscriptionEnded) bool {
				return e.GetFailed().GetCode() == "internal" && e.GetFailed().GetHandedAway() == nil
			},
		},
		{
			name: "a handed-away workspace names its successor",
			end:  handedAway,
			want: func(e *agentreplv1.PageSubscriptionEnded) bool {
				return e.GetFailed().GetHandedAway().GetAddress() == "127.0.0.1:7"
			},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			target := scriptedFooterDaemon(t, func(_ context.Context, _ *connect.Request[agentreplv1.WatchFooterRequest], stream *connect.ServerStream[agentreplv1.WatchFooterResponse]) error {
				stream.ResponseHeader().Set(AcceptedHeader, "1")
				if err := stream.Send(&agentreplv1.WatchFooterResponse{}); err != nil {
					return err
				}
				return tc.end
			})
			here := servingElsewhereAt(t, target)
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			stream := attachPageStream(t, here, ctx, "page-1")

			// Act
			subscribePage(t, ctx, here, footerSubscription("page-1", nil))

			// Assert: the target's one push, then how it ended.
			if push := nextPageFrame(t, cancel, stream).GetPush(); push == nil {
				t.Fatal("the target's push did not reach the page first")
			}
			ended := nextPageFrame(t, cancel, stream).GetEnded()
			if ended.GetSubscription() != "footer-1" || !tc.want(ended) {
				t.Fatalf("the page was told %v", ended)
			}
		})
	}
}

func TestRelayTargetNamesTheDaemonAViewIsServedFrom(t *testing.T) {
	relay := func(context.Context, string) error { return nil }
	cases := []struct {
		name     string
		watch    pageWatch
		standing workspace.Standing
		// successor is the daemon a handover in flight hands workspaces to.
		successor string
		want      string
	}{
		{name: "a view of the daemon itself is never relayed", watch: pageWatch{Name: "WatchDaemon"}, standing: workspace.StandingNotYetAdopted},
		{name: "a workspace this daemon serves is served here", watch: pageWatch{Name: "WatchFooter", Workspace: "ws-1", Relay: relay}, standing: workspace.StandingOwned},
		{name: "a workspace handed to a successor is relayed from the successor", watch: pageWatch{Name: "WatchFooter", Workspace: "ws-1", Relay: relay}, standing: workspace.StandingTransferringAway, successor: "127.0.0.1:3", want: "127.0.0.1:3"},
		{name: "a workspace another daemon serves is relayed from it", watch: pageWatch{Name: "WatchFooter", Workspace: "ws-1", Relay: relay}, standing: workspace.StandingNotYetAdopted, want: "127.0.0.1:1"},
		{name: "a feed tail another running daemon pinned is relayed from it", watch: pageWatch{Name: "WatchFeed", Minter: "daemon-there", Relay: relay}, standing: workspace.StandingOwned, want: "127.0.0.1:2"},
		{name: "a feed tail this daemon pinned is served here", watch: pageWatch{Name: "WatchFeed", Minter: string(testInstance), Relay: relay}, standing: workspace.StandingOwned},
		{name: "a feed tail pinned by a daemon that no longer runs is served here", watch: pageWatch{Name: "WatchFeed", Minter: "daemon-that-exited", Relay: relay}, standing: workspace.StandingOwned},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t, func(d *Deps) {
				d.Ownership = &fakeOwnership{standing: tc.standing}
				d.SuccessorAddress = func() string { return tc.successor }
				d.ServingElsewhere = func(context.Context, ids.WorkspaceID) (string, bool, error) {
					return "127.0.0.1:1", true, nil
				}
			})
			h.Instances = map[ids.InstanceID]string{"daemon-there": "127.0.0.1:2"}

			// Act
			target, elsewhere, err := h.Server.(*server).relayTarget(context.Background(), tc.watch)

			// Assert
			if err != nil || target != tc.want || elsewhere != (tc.want != "") {
				t.Fatalf("relayTarget = (%q, %v, %v), want %q", target, elsewhere, err, tc.want)
			}
		})
	}
}

func TestARelayedStreamHoldsItsPageConnectedToItsWorkspaceHere(t *testing.T) {
	// Arrange: the daemon serving the workspace, asked by another daemon for
	// page-1's footer.
	h := newHarness(t)
	surface := h.Server.(*server)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	h.Footer.topic.Publish(footerView(1))
	req := connect.NewRequest(&agentreplv1.WatchFooterRequest{Workspace: ref()})
	req.Header().Set(RelayedPageHeader, "page-1")

	// Act: the first frame follows the stream's acceptance.
	stream, err := h.Client.WatchFooter(ctx, req)
	if err != nil {
		t.Fatalf("open the relayed stream: %v", err)
	}
	if !stream.Receive() {
		t.Fatalf("the relayed stream ended before its first view: %v", stream.Err())
	}

	// Assert
	present := pagesHeld(surface, "page-1")
	if present != 1 {
		t.Fatalf("the relayed page is held %d times here, want 1", present)
	}
}

func TestHoldRelayedPageHoldsFromAcceptanceUntilRelease(t *testing.T) {
	steps := []struct {
		name string
		// act runs on the hold's two functions.
		act  func(accepted, release func())
		want int
	}{
		{name: "nothing is held before the stream is accepted", act: func(_, _ func()) {}, want: 0},
		{name: "a stream never accepted releases nothing", act: func(_, release func()) { release() }, want: 0},
		{name: "the page is held once the stream is accepted", act: func(accepted, _ func()) { accepted() }, want: 1},
		{name: "a second acceptance holds it no further", act: func(accepted, _ func()) { accepted(); accepted() }, want: 1},
		{name: "the release lets it go", act: func(accepted, release func()) { accepted(); release() }, want: 0},
		{name: "a second release lets nothing further go", act: func(accepted, release func()) { accepted(); release(); release() }, want: 0},
	}
	for _, tc := range steps {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			surface := h.Server.(*server)
			accepted, release := surface.holdRelayedPage("page-1", "ws-1")

			// Act
			tc.act(accepted, release)

			// Assert
			present := pagesHeld(surface, "page-1")
			if present != tc.want {
				t.Fatalf("the page is held %d times, want %d", present, tc.want)
			}
		})
	}
}

func TestReleasingAPageNothingHoldsPanics(t *testing.T) {
	// Arrange
	h := newHarness(t)
	surface := h.Server.(*server)
	surface.mu.Lock()
	defer surface.mu.Unlock()
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("releasing a page with no holder counted did not panic")
		}
	}()

	// Act
	surface.pageAbsentLocked("never-held")
}

func TestEndedFrameNamesAMovedSubscription(t *testing.T) {
	cases := []struct {
		name         string
		unsubscribed bool
		want         string
	}{
		{name: "a relayed view its target ended cleanly is moved", want: "moved"},
		{name: "the client's own unsubscribe wins over the relay's end", unsubscribed: true, want: "unsubscribed"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			_, arm := endedFrame("footer-1", tc.unsubscribed, errSubscriptionMoved)

			// Assert
			if arm != tc.want {
				t.Fatalf("endedFrame named %q, want %q", arm, tc.want)
			}
		})
	}
}
