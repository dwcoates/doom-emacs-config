package integration

import (
	"context"
	"net/http"
	"sync/atomic"
	"testing"

	storev1 "agentrepl/proto/store/v1"

	"connectrpc.com/connect"
)

// fake_store_bounce_test.go pins what a bounce of the fake store does to its
// own reading client (fakeStore.shutdown).

// countingTransport is an *http.Transport that counts the times its pool was
// dropped.
type countingTransport struct {
	*http.Transport
	drops atomic.Int32
}

func (c *countingTransport) CloseIdleConnections() {
	c.drops.Add(1)
	c.Transport.CloseIdleConnections()
}

func TestAStoppedFakeStoreDropsItsClientsConnections(t *testing.T) {
	// Arrange: the fake's client rides a transport that counts its drops.
	f := startFakeStoreAt(t, shortSocketPath(t, "drop"))
	counting := &countingTransport{Transport: f.httpClient.Transport.(*ownedRequestBody).next.(*http.Transport)}
	f.httpClient.Transport = &ownedRequestBody{next: counting}

	// Act
	f.Stop()

	// Assert
	if got := counting.drops.Load(); got != 1 {
		t.Fatalf("the fake's client dropped its connections %d time(s) at the stop, want once", got)
	}
}

func TestAFakeStoreReadAfterABounceSucceeds(t *testing.T) {
	// Arrange: a read before the bounce leaves a connection in the pool.
	ctx, cancel := testContext(t)
	defer cancel()
	f := startFakeStoreAt(t, shortSocketPath(t, "bounce"))
	if _, err := f.Client.GetSidecarCursors(ctx, connect.NewRequest(&storev1.GetSidecarCursorsRequest{})); err != nil {
		t.Fatalf("read before the bounce: %v", err)
	}

	// Act
	f.Stop()
	f.Resume()
	_, err := f.Client.GetSidecarCursors(context.WithoutCancel(ctx), connect.NewRequest(&storev1.GetSidecarCursorsRequest{}))

	// Assert
	if err != nil {
		t.Fatalf("read after the bounce: %v, want it answered on a new connection", err)
	}
}
