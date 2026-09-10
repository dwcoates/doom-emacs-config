package integration

import (
	"bytes"
	"fmt"
	"io"
	"net/http"
)

// helpers_transport_test.go — THE REQUEST BODY THIS PACKAGE'S CONNECT CLIENTS
// PROMISE IS A PROMISE THEY KEEP.
//
// THE DEFECT THIS EXISTS FOR, IN FULL — it is not defensive.
//
// connect-go sends a SERVER-STREAMING request the way it sends a unary one:
// the whole request message is known up front, so `duplexHTTPCall.sendUnary`
// sets `Request.ContentLength` and hands the transport a `payloadCloser` over a
// POOLED buffer. It then releases that buffer as soon as `Do` returns —
// `defer payloadBody.Release()` — and a released `payloadCloser` answers every
// subsequent Read with `io.EOF`. Its own doc calls that "after the response is
// received", which is only safe if a response implies the request was fully
// sent.
//
// It does not. `Do` returns when the response HEAD arrives, and this package's
// peer — the SHIM — writes a streaming response's head THE MOMENT IT ACCEPTS
// THE STREAM (`agent-shim/claude/shim/src/service/server.ts`, `flushStreamHead`),
// because a Go client cannot otherwise tell an accepted-but-quiet stream from a
// refused one. So the head routinely beats the body write, the release wins the
// race, and the transport reads EOF at offset zero on a request that already
// announced `Content-Length: 48`. Go's HTTP/1.1 transport calls that what it
// is — `http: ContentLength=48 with Body length 0` — and TEARS THE CONNECTION
// DOWN, which surfaces on the WatchAgent tail that was riding the same
// connection as
//
//	invalid_argument: protocol error: incomplete envelope: read unix -> …:
//	use of closed network connection
//
// captured, with the httptrace timeline that proves the ordering, on the
// mocked-vendor drive: `GotConn reused=true` → `WroteHeaders` →
// `GotFirstResponseByte` → `WroteRequest err=http: ContentLength=48 with Body
// length 0`. It is a RACE against the shim's own scheduler, so it lands on
// whichever scenario happens to be descheduled between the header write and
// the body write — no row owns it, and it is a defect wherever it lands.
//
// THE DAEMON ALREADY FOUND THIS AND FIXED IT, over h2c, in
// `daemon/internal/shimclient/transport.go` (`ownedRequestBody`). Read that
// file's comment for the HTTP/2 shape of the same defect; this is the same fix
// for the same cause on the HTTP/1.1 side, and the two must stay in step.
//
// The fix is to make the promise unbreakable rather than to survive its breach:
// the body is copied INSIDE RoundTrip and therefore before `Do` can return and
// before anything can be released, and the transport writes from a buffer this
// wrapper owns.
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
		return nil, fmt.Errorf("the request body for %s could not be read: %w", req.URL.Path, err)
	}
	if closeErr != nil {
		return nil, fmt.Errorf("the request body for %s could not be closed: %w", req.URL.Path, closeErr)
	}
	// A body that is already short of its own declared length is the defect
	// this wrapper exists for, arriving too early to be repaired. Refusing the
	// call names it here rather than letting the transport cut a connection
	// whose other streams did nothing wrong.
	if int64(len(body)) != req.ContentLength {
		return nil, fmt.Errorf(
			"%s declared content-length %d but its body held %d bytes; refusing to send a request that contradicts its own length",
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
