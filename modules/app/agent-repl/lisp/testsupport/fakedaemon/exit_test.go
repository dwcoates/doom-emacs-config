package main

import (
	"context"
	"net/http"
	"net/http/httptest"
	"testing"
	"time"
)

// The exit path has to DROP standing streams before it asks the HTTP server to
// shut down.  A subscription never concludes on its own, so a graceful
// shutdown that waits for in-flight requests waits for something that by
// construction never returns: it burns its whole grace period and then closes
// the connections anyway.  The integration suites stop a daemon in most of
// their scenarios, so that wasted grace period is paid over and over.

func TestAbortAllStreamsDropsAStandingStream(t *testing.T) {
	// Arrange.
	server, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)
	stream, cancel := openHost(t, server, client, "ws-a")
	defer cancel()

	// Act.
	if dropped := server.abortAllStreams(); dropped != 1 {
		t.Fatalf("abortAllStreams reported %d dropped stream(s), want 1", dropped)
	}

	// Assert: the stream did not merely end, it FAILED -- no end frame at all,
	// which is what a process going away looks like on the wire.
	if stream.Receive() {
		t.Fatalf("the aborted stream delivered a message: %v", stream.Msg())
	}
	if stream.Err() == nil {
		t.Fatalf("the aborted stream concluded cleanly; an abort writes no end frame")
	}
}

func TestAbortAllStreamsDrainsTheSubscriberRegistry(t *testing.T) {
	// Arrange.
	server, baseURL := newTestServer(t)
	client := newTestClient(t, baseURL)
	_, cancel := openHost(t, server, client, "ws-a")
	defer cancel()

	// Act.
	server.abortAllStreams()

	// Assert.
	server.mustAwaitSubscribers(t, streamHost, "ws-a", 0)
}

func TestAbortAllStreamsWithNoSubscribersDropsNothing(t *testing.T) {
	// Arrange.
	server, _ := newTestServer(t)

	// Act / Assert.
	if dropped := server.abortAllStreams(); dropped != 0 {
		t.Fatalf("abortAllStreams reported %d dropped stream(s) with none open", dropped)
	}
}

// TestShutdownAfterAbortDoesNotWaitOutItsGrace is the reason the abort exists:
// with a standing stream open, `Shutdown' returns promptly once the stream has
// been dropped, instead of spending its whole grace period on a request that
// never finishes.
func TestShutdownAfterAbortDoesNotWaitOutItsGrace(t *testing.T) {
	// Arrange: the same handler main() serves, with a stream standing on it.
	server := newFakeServer()
	httpTest := httptest.NewUnstartedServer(nil)
	httpTest.Config.Handler = newHandler(server, func() {})
	httpTest.Config.ConnContext = connContext
	httpTest.Start()
	defer httpTest.Close()
	client := newTestClient(t, httpTest.URL)
	_, cancel := openHost(t, server, client, "ws-a")
	defer cancel()

	// Act.
	server.abortAllStreams()
	start := time.Now()
	// A grace far shorter than the exit path's own 2s: if the abort did not
	// free the handler, this deadline is what would expire.
	ctx, cancelCtx := context.WithTimeout(context.Background(), 200*time.Millisecond)
	defer cancelCtx()
	err := httpTest.Config.Shutdown(ctx)

	// Assert.
	if err != nil {
		t.Fatalf("Shutdown did not finish after the abort: %v", err)
	}
	if elapsed := time.Since(start); elapsed >= 200*time.Millisecond {
		t.Fatalf("Shutdown took %s: it waited out the grace rather than returning on the abort", elapsed)
	}
}

// TestExitPathClosesTheListener pins that the shutdown really did stop
// serving, so the fast return above is a finished shutdown and not a skipped
// one.
func TestExitPathClosesTheListener(t *testing.T) {
	// Arrange.
	server := newFakeServer()
	httpTest := httptest.NewUnstartedServer(nil)
	httpTest.Config.Handler = newHandler(server, func() {})
	httpTest.Config.ConnContext = connContext
	httpTest.Start()
	defer httpTest.Close()
	url := httpTest.URL
	client := newTestClient(t, url)
	_, cancel := openHost(t, server, client, "ws-a")
	defer cancel()

	// Act.
	server.abortAllStreams()
	ctx, cancelCtx := context.WithTimeout(context.Background(), 2*time.Second)
	defer cancelCtx()
	if err := httpTest.Config.Shutdown(ctx); err != nil {
		t.Fatalf("Shutdown: %v", err)
	}

	// Assert.
	if _, err := http.Get(url + "/_fake/subscribers"); err == nil {
		t.Fatalf("the server still answered after shutdown")
	}
}
