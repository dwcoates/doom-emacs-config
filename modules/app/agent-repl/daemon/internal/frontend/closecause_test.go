package frontend

import (
	"errors"
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"sync"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"github.com/gorilla/websocket"
)

// causeConn is a conn that records the cause it was closed with, so a test can
// assert on what the TRANSPORT was told rather than only on what was logged.
type causeConn struct {
	mu     sync.Mutex
	closed []closeCause
	// blockRead parks readCommand until the connection is closed, which is what
	// a real idle connection does.
	blockRead chan struct{}
	once      sync.Once
}

func newCauseConn() *causeConn {
	return &causeConn{blockRead: make(chan struct{})}
}

func (c *causeConn) writeFrame([]byte, func()) error { return nil }

func (c *causeConn) readCommand() (*frontendv1.FrontendCommand, error) {
	<-c.blockRead
	return nil, io.EOF
}

func (c *causeConn) close(cause closeCause) error {
	c.mu.Lock()
	c.closed = append(c.closed, cause)
	c.mu.Unlock()
	c.once.Do(func() { close(c.blockRead) })
	return nil
}

func (c *causeConn) causes() []closeCause {
	c.mu.Lock()
	defer c.mu.Unlock()
	return append([]closeCause(nil), c.closed...)
}

// awaitClose rendezvouses with the teardown reaching the transport, so a test
// observes the close rather than waiting out a clock. The deadline is a failure
// bound, never a synchronization device.
func (c *causeConn) awaitClose(t *testing.T) closeCause {
	t.Helper()
	<-c.blockRead
	deadline := time.After(ticketTestDeadline)
	for {
		if got := c.causes(); len(got) > 0 {
			return got[0]
		}
		select {
		case <-deadline:
			t.Fatal("the connection was never closed before the failure deadline")
			return closeCause{}
		default:
		}
	}
}

func TestCloseCauseNamesItsReasonAndWireCode(t *testing.T) {
	tests := []struct {
		name       string
		cause      closeCause
		wantReason string
		wantCode   int
	}{
		{
			name:       "the whole-server teardown is a going-away",
			cause:      causeServerShutdown,
			wantReason: closeReasonServerShutdown,
			wantCode:   websocket.CloseGoingAway,
		},
		{
			name:       "an accept that lost the race with Close is a going-away",
			cause:      causeServerClosed,
			wantReason: closeReasonServerClosed,
			wantCode:   websocket.CloseGoingAway,
		},
		{
			name:       "a daemon-side marshal failure is an internal error",
			cause:      causeInternal(closeReasonSnapshotMarshal, errors.New("boom")),
			wantReason: closeReasonSnapshotMarshal,
			wantCode:   websocket.CloseInternalServerErr,
		},
		{
			name:       "a slow consumer is told to come back later",
			cause:      causeOverflow(pushResult{reason: overflowCeiling}, "delivery"),
			wantReason: closeReasonOverflow,
			wantCode:   websocket.CloseTryAgainLater,
		},
		{
			name:       "a failed write is an abnormal closure",
			cause:      causeWriteFailed(errors.New("broken pipe")),
			wantReason: closeReasonWriteFailed,
			wantCode:   websocket.CloseAbnormalClosure,
		},
		{
			name:       "a clean EOF is the client's own decision",
			cause:      causeInboundEnded(io.EOF),
			wantReason: closeReasonPeerClosed,
			wantCode:   websocket.CloseNormalClosure,
		},
		{
			name:       "a broken inbound frame stream is a protocol error",
			cause:      causeInboundEnded(errors.New("protojson unmarshal command")),
			wantReason: closeReasonReadFailed,
			wantCode:   websocket.CloseProtocolError,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange/Act: the cause itself is the unit under test.
			got := tc.cause

			// Assert.
			if got.reason != tc.wantReason {
				t.Fatalf("reason = %q, want %q", got.reason, tc.wantReason)
			}
			if got.code != tc.wantCode {
				t.Fatalf("ws close code = %d, want %d", got.code, tc.wantCode)
			}
		})
	}
}

func TestCloseCauseWireReasonFitsTheControlFrameBudget(t *testing.T) {
	// Arrange: a detail far longer than a close frame's payload allows. A
	// WebSocket peer rejects an oversized control frame outright, which would
	// turn the record into the very silent close it exists to prevent.
	cause := causeInternal(closeReasonSnapshotMarshal, errors.New(strings.Repeat("x", 500)))

	// Act.
	wire := cause.wireReason()

	// Assert.
	if len(wire) > closeReasonMax {
		t.Fatalf("wire reason = %d bytes, want at most %d", len(wire), closeReasonMax)
	}
}

func TestCloseConnRecordsTheCauseAndHandsItToTheTransport(t *testing.T) {
	// Arrange.
	log := newCapturedLogf()
	s := newStallServer(t, log, time.Hour)
	c := newCauseConn()

	// Act.
	s.closeConn(c, 7, ClientKindGUIStream, causeOverflow(pushResult{reason: overflowStalled, depth: 1580, soft: 256, hard: 4096}, "delivery"))

	// Assert: the record names the connection and the reason, and the transport
	// was given the cause so it can put a code on the wire.
	line := log.awaitLine(t, "closing connection")
	for _, want := range []string{"client_id=7", "kind=gui_stream", "cause=outbound_overflow", "limit=stalled", "ws_close_code="} {
		if !strings.Contains(line, want) {
			t.Fatalf("close record %q is missing %q", line, want)
		}
	}
	got := c.causes()
	if len(got) != 1 || got[0].reason != closeReasonOverflow {
		t.Fatalf("transport was closed with %+v, want the overflow cause", got)
	}
}

func TestCloseConnReportsATeardownThatNamedNoCause(t *testing.T) {
	// Arrange: the accountability backstop. A close reaching the transport with
	// no recorded reason is a bug in this package, and the whole point of this
	// mechanism is that such a close is never quiet.
	log := newCapturedLogf()
	s := newStallServer(t, log, time.Hour)
	c := newCauseConn()

	// Act.
	s.closeConn(c, 3, ClientKindHost, closeCause{})

	// Assert.
	log.awaitLine(t, "WITHOUT A RECORDED CAUSE")
	got := c.causes()
	if len(got) != 1 || got[0].reason != closeReasonUnrecorded {
		t.Fatalf("transport was closed with %+v, want the unrecorded backstop", got)
	}
}

func TestCloseConnSurfacesAFailedTransportClose(t *testing.T) {
	// Arrange: a close that itself fails must not be swallowed.
	log := newCapturedLogf()
	s := newStallServer(t, log, time.Hour)

	// Act.
	s.closeConn(failingCloseConn{}, 4, ClientKindHost, causeServerShutdown)

	// Assert.
	line := log.awaitLine(t, "connection close client_id=4")
	if !strings.Contains(line, "close frame refused") {
		t.Fatalf("close-failure record %q does not carry the underlying error", line)
	}
}

// failingCloseConn's close always fails, which is what a socket already reset by
// the peer does.
type failingCloseConn struct{}

func (failingCloseConn) writeFrame([]byte, func()) error { return nil }
func (failingCloseConn) readCommand() (*frontendv1.FrontendCommand, error) {
	return nil, io.EOF
}
func (failingCloseConn) close(closeCause) error { return errors.New("close frame refused") }

func TestDisconnectRecordsTheCauseOnTheClientAndInTheLog(t *testing.T) {
	tests := []struct {
		name       string
		cause      closeCause
		wantReason string
	}{
		{
			name:       "a slow consumer",
			cause:      causeOverflow(pushResult{reason: overflowStalled}, "delivery"),
			wantReason: closeReasonOverflow,
		},
		{
			name:       "the server shutting down",
			cause:      causeServerShutdown,
			wantReason: closeReasonServerShutdown,
		},
		{
			name:       "the client hanging up",
			cause:      causeInboundEnded(io.EOF),
			wantReason: closeReasonPeerClosed,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			log := newCapturedLogf()
			s := newStallServer(t, log, time.Hour)
			cl := newClient(defaultClientBuffer, nil, ClientKindHost)
			s.clients[cl] = struct{}{}

			// Act.
			s.disconnect(cl, tc.cause)

			// Assert: the cause is readable by the writer's teardown AND present
			// in the disconnect record.
			if got := cl.closeCause().reason; got != tc.wantReason {
				t.Fatalf("recorded cause = %q, want %q", got, tc.wantReason)
			}
			line := log.awaitLine(t, "client disconnected")
			if !strings.Contains(line, "cause="+tc.wantReason) {
				t.Fatalf("disconnect record %q does not name the cause", line)
			}
		})
	}
}

func TestWriteLoopClosesTheSocketWithTheRecordedCause(t *testing.T) {
	// Arrange: the writer owns the socket teardown, so the cause a disconnect
	// records has to survive the hand-off to it.
	log := newCapturedLogf()
	s := newStallServer(t, log, time.Hour)
	cl := newClient(defaultClientBuffer, nil, ClientKindHost)
	s.clients[cl] = struct{}{}
	c := newCauseConn()
	go s.writeLoop(c, cl)

	// Act.
	s.disconnect(cl, causeOverflow(pushResult{reason: overflowStalled, depth: 1580}, "delivery"))

	// Assert.
	got := c.awaitClose(t)
	if got.reason != closeReasonOverflow {
		t.Fatalf("socket closed with %+v, want the recorded overflow cause", got)
	}
	if got.code != websocket.CloseTryAgainLater {
		t.Fatalf("socket closed with ws code %d, want %d", got.code, websocket.CloseTryAgainLater)
	}
}

func TestOverflowEvictionRecordsWhichLimitItHit(t *testing.T) {
	tests := []struct {
		name string
		// build fills a client's queue to the point of refusal and returns it.
		build     func(t *testing.T, s *Server) *client
		wantLimit string
	}{
		{
			name: "a flat queue past its ceiling",
			build: func(t *testing.T, s *Server) *client {
				t.Helper()
				// A GUI stream is soft==hard with no grace: the flat policy.
				cl := newClient(1, nil, ClientKindGUIStream)
				s.clients[cl] = struct{}{}
				s.enqueue(cl, outFrame{data: []byte(`{"a":1}`)})
				s.enqueue(cl, outFrame{data: []byte(`{"a":2}`)})
				return cl
			},
			wantLimit: string(overflowCeiling),
		},
		{
			name: "an elastic queue whose consumer showed no progress at all",
			build: func(t *testing.T, s *Server) *client {
				t.Helper()
				clock := newFakeClock()
				cl := newClient(2, nil, ClientKindHost)
				cl.out.now = clock.Now
				s.clients[cl] = struct{}{}
				s.enqueue(cl, outFrame{data: []byte(`{"a":1}`)})
				s.enqueue(cl, outFrame{data: []byte(`{"a":2}`)})
				s.enqueue(cl, outFrame{data: []byte(`{"a":3}`)})
				clock.advance(31 * time.Second)
				s.enqueue(cl, outFrame{data: []byte(`{"a":4}`)})
				return cl
			},
			wantLimit: string(overflowStalled),
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			log := newCapturedLogf()
			s := newStallServer(t, log, time.Hour)

			// Act.
			cl := tc.build(t, s)

			// Assert: the eviction happened, and it says which bound produced it.
			cause := cl.closeCause()
			if cause.reason != closeReasonOverflow {
				t.Fatalf("cause = %+v, want an overflow eviction", cause)
			}
			if !strings.Contains(cause.detail, "limit="+tc.wantLimit) {
				t.Fatalf("cause detail %q does not name limit=%s", cause.detail, tc.wantLimit)
			}
			if !log.contains("cause=outbound_overflow") {
				t.Fatal("the eviction was not recorded with its cause")
			}
		})
	}
}

func TestConnectingToAClosedServerRecordsWhyTheSocketWasDropped(t *testing.T) {
	// Arrange: this path used to close with no record at all — a connection
	// accepted into a closing server simply vanished from both ends.
	log := newCapturedLogf()
	s := newStallServer(t, log, time.Hour)
	if err := s.Close(); err != nil {
		t.Fatalf("close server: %v", err)
	}
	c := newCauseConn()

	// Act.
	s.serveClient(c, nil, ClientKindHost)

	// Assert.
	got := c.causes()
	if len(got) != 1 || got[0].reason != closeReasonServerClosed {
		t.Fatalf("connection closed with %+v, want %q", got, closeReasonServerClosed)
	}
	log.awaitLine(t, "cause=server_closed")
}

func TestServerCloseTearsEveryClientDownWithTheShutdownCause(t *testing.T) {
	// Arrange.
	log := newCapturedLogf()
	s := newStallServer(t, log, time.Hour)
	cl := newClient(defaultClientBuffer, nil, ClientKindHost)
	s.clients[cl] = struct{}{}

	// Act.
	if err := s.Close(); err != nil {
		t.Fatalf("close server: %v", err)
	}

	// Assert.
	if got := cl.closeCause().reason; got != closeReasonServerShutdown {
		t.Fatalf("cause = %q, want %q", got, closeReasonServerShutdown)
	}
}

func TestWebSocketTeardownSendsAStatusCodeRatherThanNone(t *testing.T) {
	tests := []struct {
		name     string
		cause    closeCause
		wantCode int
	}{
		{
			name:     "a slow consumer is told to come back later",
			cause:    causeOverflow(pushResult{reason: overflowCeiling, depth: 4096}, "delivery"),
			wantCode: websocket.CloseTryAgainLater,
		},
		{
			name:     "a shutting-down daemon is a going-away",
			cause:    causeServerShutdown,
			wantCode: websocket.CloseGoingAway,
		},
		{
			name:     "a daemon-side failure is an internal error",
			cause:    causeInternal(closeReasonLeaseMarshal, errors.New("marshal")),
			wantCode: websocket.CloseInternalServerErr,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a real WebSocket pair. The browser reported close=1005 —
			// "no status received" — which is exactly what a bare Close leaves,
			// so only a real socket can prove the frame is on the wire.
			upgraded := make(chan *wsConn, 1)
			serving := make(chan struct{})
			var upgrader websocket.Upgrader
			srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
				ws, err := upgrader.Upgrade(w, r, nil)
				if err != nil {
					close(upgraded)
					return
				}
				upgraded <- newWSConn(ws)
				<-serving
			}))
			t.Cleanup(func() { close(serving); srv.Close() })
			client, _, err := websocket.DefaultDialer.Dial("ws"+strings.TrimPrefix(srv.URL, "http"), nil)
			if err != nil {
				t.Fatalf("dial: %v", err)
			}
			t.Cleanup(func() { _ = client.Close() })
			server := <-upgraded
			if server == nil {
				t.Fatal("the upgrade failed")
			}

			// Act.
			if err := server.close(tc.cause); err != nil {
				t.Fatalf("close = %v, want the close handshake to succeed", err)
			}

			// Assert: the peer sees a real status code, never 1005.
			if err := client.SetReadDeadline(time.Now().Add(ticketTestDeadline)); err != nil {
				t.Fatalf("set read deadline: %v", err)
			}
			_, _, readErr := client.ReadMessage()
			var closeErr *websocket.CloseError
			if !errors.As(readErr, &closeErr) {
				t.Fatalf("client read error = %v, want a WebSocket close error", readErr)
			}
			if closeErr.Code != tc.wantCode {
				t.Fatalf("close code = %d, want %d", closeErr.Code, tc.wantCode)
			}
			if closeErr.Text == "" {
				t.Fatal("close frame carried no reason text")
			}
		})
	}
}
