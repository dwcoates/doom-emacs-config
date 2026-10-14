package shimclient

import (
	"bytes"
	"context"
	"crypto/tls"
	"errors"
	"fmt"
	"io"
	"net"
	"net/http"
	"sync"
	"time"

	"golang.org/x/net/http2"
)

// udsBaseURL is the base URL every shim connection uses. The authority is
// never resolved: the transport dials the unix socket the client was given.
const udsBaseURL = "http://shim.uds"

// newUDSClient builds the http.Client for one shim socket: h2c over the unix
// domain socket, so one connection carries every unary call and every open
// watch stream at once. The dialed connections are kept in conns, so the
// client can hang up on the shim (dialedConns.hangUp).
func newUDSClient(udsPath string, conns *dialedConns) *http.Client {
	return &http.Client{
		Transport: &ownedRequestBody{next: &http2.Transport{
			AllowHTTP: true,
			DialTLSContext: func(ctx context.Context, _, _ string, _ *tls.Config) (net.Conn, error) {
				var d net.Dialer
				conn, err := d.DialContext(ctx, "unix", udsPath)
				if err != nil {
					return nil, err
				}
				return conns.track(conn), nil
			},
		}},
	}
}

// dialedConns is every connection a client's transport has dialed to its shim
// and not yet closed.
//
// IT EXISTS SO THE DAEMON CAN HANG UP, which `http2.Transport` has no verb
// for: CloseIdleConnections leaves a connection carrying a standing watch
// open. A shim that answered KillSession exits on this hang-up, never on its
// answer leaving (agent-repld/internal/hangup carries the account),
// so the daemon hangs up once it has read the answer.
type dialedConns struct {
	mu   sync.Mutex
	open map[*trackedConn]struct{}
}

func newDialedConns() *dialedConns {
	return &dialedConns{open: map[*trackedConn]struct{}{}}
}

// track keeps conn until it is closed.
func (d *dialedConns) track(conn net.Conn) net.Conn {
	t := &trackedConn{Conn: conn, owner: d}
	d.mu.Lock()
	d.open[t] = struct{}{}
	d.mu.Unlock()
	return t
}

// hangUp closes every connection still open, answering how many it closed.
// Every failed close is answered, never dropped.
func (d *dialedConns) hangUp() (int, error) {
	d.mu.Lock()
	conns := make([]*trackedConn, 0, len(d.open))
	for c := range d.open {
		conns = append(conns, c)
	}
	d.mu.Unlock()
	var errs []error
	for _, c := range conns {
		if err := c.Close(); err != nil && !errors.Is(err, net.ErrClosed) {
			errs = append(errs, fmt.Errorf("shimclient: hang up a shim connection: %w", err))
		}
	}
	return len(conns), errors.Join(errs...)
}

// trackedConn leaves its owner's set when it is closed, by anyone.
type trackedConn struct {
	net.Conn
	owner *dialedConns
}

func (c *trackedConn) Close() error {
	c.owner.mu.Lock()
	delete(c.owner.open, c)
	c.owner.mu.Unlock()
	return c.Conn.Close()
}

// ownedRequestBody takes ownership of a request body whose length is already
// declared, so the bytes the request PROMISED cannot be withdrawn while http2
// is still writing them.
//
// THE DEFECT THIS EXISTS FOR, IN FULL — it is not defensive.
//
// connect-go sends a SERVER-STREAMING request the way it sends a unary one:
// the whole request message is known up front, so `duplexHTTPCall.sendUnary`
// sets `Request.ContentLength` and hands the transport a `payloadCloser` over
// a POOLED buffer. It then releases that buffer as soon as `Do` returns —
// `defer payloadBody.Release()` — and a released `payloadCloser` answers every
// subsequent Read with `io.EOF`. Its own doc calls that "after the response is
// received", which is only safe if a response implies the request was fully
// sent.
//
// Over HTTP/2 it does not. `Do` returns when the response HEAD arrives, while
// `http2.clientStream.writeRequestBody` is still running on its own goroutine
// — Go's own comment in that function puts the reuse point at "after the
// Response's Body is closed", not at the head. And this daemon's peer, the
// shim, writes a streaming response's head THE MOMENT IT ACCEPTS THE STREAM
// (agent-shim/claude/shim/src/service/server.ts, "the head goes out ON
// ACCEPT"), because a Go client cannot otherwise tell an accepted-but-quiet
// stream from a refused one. So the head routinely beats the body write, the
// release wins the race, `writeRequestBody` reads EOF at offset zero, and
// http2 closes the stream with an empty DATA frame — after having announced
// `content-length: 5`.
//
// That request is malformed, and nghttp2 says so: a remote END_STREAM whose
// received length disagrees with the declared `content-length` is an HTTP
// messaging violation, and the shim RST_STREAMs it with PROTOCOL_ERROR. The
// daemon then reads "stream error: ...; PROTOCOL_ERROR; received from peer" on
// a WatchSession it had just opened, opens a `link_severed` fault, and redials.
// The peer is RIGHT to reset it; the daemon must not emit it. Observed only
// under container contention, where the body-writing goroutine is descheduled
// between the header write and the body write for long enough to lose.
//
// The fix is to make the promise unbreakable rather than to survive its
// breach: the body is copied here, INSIDE RoundTrip and therefore before `Do`
// can return and before anything can be released, and the transport writes
// from a buffer this wrapper owns. Retrying the stream is deliberately NOT the
// fix — connect-go's streaming request body is a pipe that cannot be replayed,
// and a request that lies about its own length must not be sent in the first
// place.
type ownedRequestBody struct {
	next http.RoundTripper
}

// RoundTrip copies a declared-length body and delegates.
//
// A body of UNDECLARED length (`ContentLength < 0`, which is what connect-go
// uses for a genuine client stream) is passed through untouched: those bytes
// arrive over time and reading them here would deadlock the very call the
// caller is still writing to. Only a request that has already committed to a
// byte count is copied, and those are tens of bytes each.
func (t *ownedRequestBody) RoundTrip(req *http.Request) (*http.Response, error) {
	if req.Body == nil || req.Body == http.NoBody || req.ContentLength <= 0 {
		return t.next.RoundTrip(req)
	}
	body, err := io.ReadAll(req.Body)
	// The RoundTripper contract makes closing the request body this
	// transport's job, and this is the layer that consumed it.
	closeErr := req.Body.Close()
	if err != nil {
		return nil, fmt.Errorf("shimclient: read the request body for %s: %w", req.URL.Path, err)
	}
	if closeErr != nil {
		return nil, fmt.Errorf("shimclient: close the request body for %s: %w", req.URL.Path, closeErr)
	}
	// A body that is already short of its own declared length is the defect
	// this wrapper exists for, arriving too early to be repaired. Refusing the
	// call names it here rather than letting the peer reset a malformed stream.
	if int64(len(body)) != req.ContentLength {
		return nil, fmt.Errorf(
			"shimclient: %s declared content-length %d but its body held %d bytes; refusing to send a request that contradicts its own length",
			req.URL.Path, req.ContentLength, len(body),
		)
	}
	owned := req.Clone(req.Context())
	owned.Body = io.NopCloser(bytes.NewReader(body))
	owned.GetBody = func() (io.ReadCloser, error) {
		return io.NopCloser(bytes.NewReader(body)), nil
	}
	return t.next.RoundTrip(owned)
}

// backoff is the capped exponential schedule for redialing. It is a VALUE so
// tests can make it instant; nothing about the daemon's behavior depends on
// the delays, only on how long they grow to.
type backoff struct {
	// Initial is the first delay.
	Initial time.Duration
	// Max caps the growth.
	Max time.Duration
	// Factor multiplies the delay per attempt.
	Factor float64
}

// defaultBackoff is the schedule a production supervisor redials on.
//
// IT STARTS SMALL BECAUSE THE FIRST DIAL OF EVERY SPAWN FAILS: the shim has
// not bound its socket yet (afterFailedDial). A 100ms first delay made every
// spawn wait at least 100ms past the shim's own readiness; measured on a
// daemon-integration fixture, the gap from spawn to session_live was 103ms of
// a 205ms boot against a fake shim that listens in single-digit
// milliseconds. Doubling from 10ms reaches the same 5s cap within ten
// attempts, so a shim that is genuinely slow or gone costs a handful of
// extra cheap dial attempts, never a different outcome.
var defaultBackoff = backoff{Initial: 10 * time.Millisecond, Max: 5 * time.Second, Factor: 2}

// delay is the wait before attempt n, counting from zero.
func (b backoff) delay(attempt int) time.Duration {
	if b.Initial <= 0 {
		return 0
	}
	factor := b.Factor
	if factor < 1 {
		factor = 1
	}
	d := float64(b.Initial)
	for i := 0; i < attempt; i++ {
		d *= factor
		if b.Max > 0 && d >= float64(b.Max) {
			return b.Max
		}
	}
	if b.Max > 0 && d > float64(b.Max) {
		return b.Max
	}
	return time.Duration(d)
}

// wait sleeps out the attempt's delay, cutting it short when the context ends
// or the process is known dead. It returns the reason it stopped waiting, or
// nil when the delay simply elapsed.
func (b backoff) wait(ctx context.Context, dead <-chan struct{}, attempt int) error {
	d := b.delay(attempt)
	if d <= 0 {
		select {
		case <-ctx.Done():
			return ctx.Err()
		case <-dead:
			return errProcessDead
		default:
			return nil
		}
	}
	timer := time.NewTimer(d)
	defer timer.Stop()
	select {
	case <-ctx.Done():
		return ctx.Err()
	case <-dead:
		return errProcessDead
	case <-timer.C:
		return nil
	}
}
