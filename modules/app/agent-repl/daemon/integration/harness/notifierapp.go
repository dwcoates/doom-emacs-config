package harness

import (
	"context"
	"crypto/sha256"
	"encoding/hex"
	"os"
	"runtime"
	"sync"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
)

// fakeNotifierExecutable is the bytes the fake deploy build stages as the
// notifier app's executable (NewFakeDeployBuilder).
const fakeNotifierExecutable = "harness notifier\n"

// FakeNotifierBuild is the build the fake notifier app reports on its stream:
// the content hash of the executable the fake deploy build stages, so a
// deploy judges the attached app up to date and restarts nothing.
var FakeNotifierBuild = func() string {
	sum := sha256.Sum256([]byte(fakeNotifierExecutable))
	return hex.EncodeToString(sum[:])
}()

// FakeNotifierApp stands in for Zettel.app, macOS's banner poster: it holds
// the daemon's WatchDesktopBanners stream, records every banner it is handed,
// and answers each one posted, so no test ever raises a real banner. It is
// the macOS counterpart of the Linux banner program's recorder (Notifier).
type FakeNotifierApp struct {
	client agentreplv1connect.AgentReplClient

	mu      sync.Mutex
	banners []*agentreplv1.DesktopBanner
}

// StartFakeNotifierApp opens the banner stream on client and returns once the
// daemon has acknowledged it, so every banner the test causes reaches it. The
// stream ends with the test.
func StartFakeNotifierApp(t testing.TB, ctx context.Context, client agentreplv1connect.AgentReplClient) *FakeNotifierApp {
	t.Helper()
	app := &FakeNotifierApp{client: client}
	stream, err := client.WatchDesktopBanners(ctx, connect.NewRequest(&agentreplv1.WatchDesktopBannersRequest{
		Process: &agentreplv1.DesktopNotifierProcess{Pid: int32(os.Getpid())},
		Build:   &agentreplv1.DesktopNotifierBuild{Sha256: FakeNotifierBuild},
	}))
	if err != nil {
		t.Fatalf("harness: open the fake notifier app's banner stream: %v", err)
	}
	if !stream.Receive() || stream.Msg().GetAttached() == nil {
		t.Fatalf("harness: the banner stream's first frame = %v (err %v), want attached", stream.Msg(), stream.Err())
	}
	go app.serve(ctx, stream)
	return app
}

// serve answers each banner posted until the stream ends.
func (a *FakeNotifierApp) serve(ctx context.Context, stream *connect.ServerStreamForClient[agentreplv1.WatchDesktopBannersResponse]) {
	for stream.Receive() {
		banner := stream.Msg().GetBanner()
		if banner == nil {
			continue
		}
		a.mu.Lock()
		a.banners = append(a.banners, banner)
		a.mu.Unlock()
		// The answer's error is the daemon's to record; a test reading the
		// banner's settlement sees its record either way.
		_, _ = a.client.ReportDesktopBannerPosted(ctx, connect.NewRequest(&agentreplv1.ReportDesktopBannerPostedRequest{
			PostId: banner.GetPostId(),
			Outcome: &agentreplv1.DesktopBannerPostOutcome{Outcome: &agentreplv1.DesktopBannerPostOutcome_Posted{
				Posted: &agentreplv1.DesktopBannerPostOutcomePosted{},
			}},
		}))
	}
}

// Banners answers every banner the app was handed, in order.
func (a *FakeNotifierApp) Banners() []*agentreplv1.DesktopBanner {
	a.mu.Lock()
	defer a.mu.Unlock()
	return append([]*agentreplv1.DesktopBanner(nil), a.banners...)
}

// PostedBanners answers the words of every banner the daemon posted, in
// order, whichever platform posted it: on macOS each banner's title and body
// as the Zettel app was handed them, elsewhere each run of the banner
// program's argv (the boot's version query set aside).
func (d *Daemon) PostedBanners() [][]string {
	d.t.Helper()
	if runtime.GOOS == "darwin" {
		var words [][]string
		for _, b := range d.NotifierApp.Banners() {
			words = append(words, []string{b.GetTitle().GetText(), b.GetBody().GetText()})
		}
		return words
	}
	var words [][]string
	for _, inv := range d.Notifier.Invocations() {
		words = append(words, inv.Argv)
	}
	return words
}
