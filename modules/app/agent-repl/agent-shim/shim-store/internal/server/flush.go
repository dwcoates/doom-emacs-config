package server

import (
	"context"
	"net/http"

	"agentrepl/shim-store/internal/logging"

	"connectrpc.com/connect"
)

// responseFlusherKey addresses the http.Flusher of the response a handler is
// writing. It is an unexported struct type so nothing outside this package can
// collide with it or forge one.
type responseFlusherKey struct{}

// withResponseFlusher publishes the response's http.Flusher on the request
// context.
//
// IT IS WHY A STANDING WATCH IS REACHABLE AT ALL. Connect's client blocks the
// WatchAgentSession call inside RoundTrip until the server's response HEADERS
// arrive (duplex_http_call.go sends a server-streaming request synchronously),
// and net/http emits headers only on the first body write. A tail that has
// nothing to replay writes nothing, so the caller that is about to produce the
// very lines the tail is waiting for is itself still blocked opening the tail:
// a deadlock that ends at the caller's deadline, never at a frame. Connect
// offers no "send the headers" API on ServerStream, and it fills the response
// header map before the handler runs, so one flush from inside the handler
// emits exactly the headers Connect prepared. This middleware hands the handler
// the only object that can do it.
//
// It must be mounted INSIDE the mux, wrapping the Connect handler, so the
// writer captured here is the one Connect writes through.
func (s *Server) withResponseFlusher(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		flusher, ok := w.(http.Flusher)
		if !ok {
			// Connect refuses server streams on an unflushable writer itself;
			// this records the same condition in the store's own log so the
			// resulting CodeInternal is not a mystery.
			s.log.Log(logging.Fields{Operation: "store.stream.flusher", Level: "warn"},
				"this response writer cannot flush, so no standing stream can be opened on it writer=%T", w)
			next.ServeHTTP(w, r)
			return
		}
		next.ServeHTTP(w, r.WithContext(context.WithValue(r.Context(), responseFlusherKey{}, flusher)))
	})
}

// openStream flushes the response headers of a standing server stream, which
// is what releases the caller's blocked call and makes the tail live.
//
// A stream whose writer cannot flush is an INVARIANT VIOLATION, not a degraded
// mode: the caller would hang until its deadline, so the stream is refused
// loudly instead.
func (s *Server) openStream(ctx context.Context, log *logging.Logger, operation string) error {
	flusher, ok := ctx.Value(responseFlusherKey{}).(http.Flusher)
	if !ok {
		ref := refuse(SiteStreamNotFlushable, "", "watch: this stream's transport cannot flush its headers, so the tail could never be opened")
		s.logOwnFailure(log, operation, ref, logging.Fields{})
		return connect.NewError(connect.CodeInternal, ref)
	}
	flusher.Flush()
	log.LogVerbose(logging.Fields{Operation: operation}, "response headers flushed; the stream is open")
	return nil
}
