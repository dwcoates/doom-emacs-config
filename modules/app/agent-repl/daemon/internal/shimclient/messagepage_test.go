package shimclient

import (
	"context"
	"errors"
	"net"
	"testing"
	"time"

	protocolv1 "agentrepl/proto/protocol/v1"
	"agentrepl/wire"
)

// pageRig stands a client up against a fake shim that hands every inbound
// MessagePageRequest to `serve`, so a test scripts the shim's half of the
// exchange — a page, a Nack, or nothing at all.
type pageRig struct {
	client   *Client
	stop     func()
	requests chan *protocolv1.MessagePageRequest
}

func newPageRig(t *testing.T, serve func(conn net.Conn, req *protocolv1.MessagePageRequest)) *pageRig {
	t.Helper()
	h := newHarness()
	requests := make(chan *protocolv1.MessagePageRequest, 8)
	path := startFakeShim(t, func(conn net.Conn) {
		fakeServerHandshake(t, conn, "s1", "1", false)
		for {
			m, err := wire.ReadAny(conn)
			if err != nil {
				return
			}
			req, ok := m.(*protocolv1.MessagePageRequest)
			if !ok {
				continue // heartbeats and control traffic: not this test's business
			}
			requests <- req
			if serve != nil {
				serve(conn, req)
			}
		}
	})
	c := New(h.config(t, "s1", path))
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan struct{})
	go func() { defer close(done); _ = c.Run(ctx) }()
	if err := c.AwaitReady(ctx); err != nil {
		cancel()
		t.Fatalf("AwaitReady: %v", err)
	}
	stop := func() {
		cancel()
		<-done
	}
	t.Cleanup(stop)
	return &pageRig{client: c, stop: stop, requests: requests}
}

func TestAMessagePageRequestCarriesTheHeadAnchor(t *testing.T) {
	// Arrange — the head is a fact the serving side resolves. A daemon that
	// named a seq for it would be authoring the position this contract removes.
	rig := newPageRig(t, func(conn net.Conn, req *protocolv1.MessagePageRequest) {
		mustWriteMsg(t, conn, &protocolv1.MessagePage{
			RequestId: req.GetRequestId(),
			Boundary:  &protocolv1.MessagePage_Floor{Floor: &protocolv1.HistoryAtRetainedFloor{}},
		})
	})

	// Act.
	if _, err := rig.client.MessagePage(context.Background(), MessagePageAnchor{Head: true}); err != nil {
		t.Fatalf("MessagePage: %v", err)
	}

	// Assert.
	select {
	case req := <-rig.requests:
		if req.GetHead() == nil {
			t.Fatalf("MessagePageRequest = %+v, want the head anchor", req)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("the shim never received a MessagePageRequest")
	}
}

func TestAMessagePageRequestCarriesABeforeSeqAnchorVerbatim(t *testing.T) {
	// Arrange — a continuation names a place the caller has demonstrably been,
	// and the value travels untouched.
	rig := newPageRig(t, func(conn net.Conn, req *protocolv1.MessagePageRequest) {
		mustWriteMsg(t, conn, &protocolv1.MessagePage{
			RequestId: req.GetRequestId(),
			Boundary:  &protocolv1.MessagePage_More{More: &protocolv1.HistoryRemainsBelow{}},
		})
	})

	// Act.
	if _, err := rig.client.MessagePage(context.Background(), MessagePageAnchor{BeforeSeq: 4242}); err != nil {
		t.Fatalf("MessagePage: %v", err)
	}

	// Assert.
	select {
	case req := <-rig.requests:
		if req.GetBeforeSeq() != 4242 {
			t.Fatalf("MessagePageRequest before_seq = %d, want 4242 carried verbatim", req.GetBeforeSeq())
		}
	case <-time.After(2 * time.Second):
		t.Fatal("the shim never received a MessagePageRequest")
	}
}

func TestAMessagePageIsReturnedToItsRequester(t *testing.T) {
	// Arrange.
	rig := newPageRig(t, func(conn net.Conn, req *protocolv1.MessagePageRequest) {
		mustWriteMsg(t, conn, &protocolv1.MessagePage{
			RequestId:   req.GetRequestId(),
			LastPageSeq: 77,
			Boundary:    &protocolv1.MessagePage_More{More: &protocolv1.HistoryRemainsBelow{}},
		})
	})

	// Act.
	page, err := rig.client.MessagePage(context.Background(), MessagePageAnchor{Head: true})

	// Assert.
	if err != nil {
		t.Fatalf("MessagePage: %v", err)
	}
	if page.GetLastPageSeq() != 77 {
		t.Fatalf("last_page_seq = %d, want 77", page.GetLastPageSeq())
	}
}

func TestANackOnThePageRequestIdIsAFailureAndNotAnEmptyPage(t *testing.T) {
	// Arrange — THE failure arm. The shim reports every page failure as a Nack
	// bearing the page's own request id; read as an empty page it would be
	// indistinguishable from a conversation with no history.
	rig := newPageRig(t, func(conn net.Conn, req *protocolv1.MessagePageRequest) {
		mustWriteMsg(t, conn, &protocolv1.Nack{RequestId: req.GetRequestId(), Reason: "store unreachable"})
	})

	// Act.
	page, err := rig.client.MessagePage(context.Background(), MessagePageAnchor{Head: true})

	// Assert.
	if !errors.Is(err, ErrMessagePageRefused) {
		t.Fatalf("MessagePage error = %v, want ErrMessagePageRefused", err)
	}
	if page != nil {
		t.Fatalf("a refused page returned %+v, and a refusal must never arrive as a page", page)
	}
}

func TestAPageRequestWithNoLiveConnectionIsRefused(t *testing.T) {
	// Arrange — a client that never connected has no side door to the store.
	h := newHarness()
	c := New(h.config(t, "s1", "/nonexistent/shim.sock"))

	// Act.
	_, err := c.MessagePage(context.Background(), MessagePageAnchor{Head: true})

	// Assert.
	if !errors.Is(err, ErrMessagePageNotConnected) {
		t.Fatalf("MessagePage error = %v, want ErrMessagePageNotConnected", err)
	}
}

func TestAPageRequestThatIsNeverAnsweredTimesOut(t *testing.T) {
	// Arrange — a page is one bounded query at the far end, so silence is a
	// fault rather than a slow drain.
	rig := newPageRig(t, nil)

	// Act.
	_, err := rig.client.MessagePage(context.Background(), MessagePageAnchor{Head: true})

	// Assert.
	if !errors.Is(err, ErrMessagePageTimeout) {
		t.Fatalf("MessagePage error = %v, want ErrMessagePageTimeout", err)
	}
}

func TestACancelledPageRequestReportsTheCancellation(t *testing.T) {
	// Arrange.
	rig := newPageRig(t, nil)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	_, err := rig.client.MessagePage(ctx, MessagePageAnchor{Head: true})

	// Assert.
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("MessagePage error = %v, want the caller's cancellation", err)
	}
}
