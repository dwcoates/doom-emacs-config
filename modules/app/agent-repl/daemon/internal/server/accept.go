package server

import (
	"context"
	"net/http"
)

// THE STANDING-STREAM ACCEPTANCE RULE (ARCHITECTURE.md "Standing-stream
// mechanics"): a client treats the ARRIVAL OF RESPONSE HEADERS as the stream
// being accepted, before any frame exists, and only ever ends a watch by
// cancelling its transport. So the daemon must flush status 200 and the
// streaming content type the moment it accepts a subscription — a watch with
// nothing to say yet is still an ACCEPTED watch.
//
// connect-go does not do this on its own: it writes headers lazily, on the
// handler's first Send. A subscriber with no published view and no push would
// therefore leave the client's open outstanding indefinitely, and every client
// would read "accepted but quiet" as "not accepted".
//
// acceptWriter is the mechanism, copied from the proven implementation the
// architecture cites (lisp/testsupport/fakedaemon/accept.go at e1164ed58): the
// mux wraps every response in one, each stream handler calls `accept` once its
// subscription is registered, and connect-go's own later WriteHeader is
// swallowed so the runtime never logs a superfluous call.

// acceptWriterKey carries the wrapper down to the handler.
type acceptWriterKey struct{}

// streamContentTypeKey carries the request's content type, which is what an
// accepted stream echoes back.
type streamContentTypeKey struct{}

// acceptWriter is one response's writer, able to send headers before the
// handler's first Send.
type acceptWriter struct {
	http.ResponseWriter
	wroteHeader bool
	// acceptedCode is the status this writer sent on acceptance, kept so a
	// later disagreement from connect-go is visible rather than silent.
	acceptedCode int
	// conflict records a disagreeing later WriteHeader so the handler that
	// installed the writer can log it against its own workspace sink.
	conflict *int
}

// Unwrap lets http.NewResponseController — which connect-go uses to set write
// deadlines — reach the real ResponseWriter through this wrapper.
func (w *acceptWriter) Unwrap() http.ResponseWriter { return w.ResponseWriter }

// WriteHeader sends the status once. A second, DIFFERENT status is recorded
// rather than swallowed silently: it would mean a handler tried to refuse a
// stream this writer had already accepted.
func (w *acceptWriter) WriteHeader(code int) {
	if w.wroteHeader {
		if code != w.acceptedCode {
			recorded := code
			w.conflict = &recorded
		}
		return
	}
	w.wroteHeader = true
	w.acceptedCode = code
	w.ResponseWriter.WriteHeader(code)
}

// Flush pushes what is buffered to the client, when the transport can.
func (w *acceptWriter) Flush() {
	if flusher, ok := w.ResponseWriter.(http.Flusher); ok {
		flusher.Flush()
	}
}

// accept sends and flushes the response headers for an accepted stream.
// contentType is echoed from the request, which for a Connect streaming call is
// "application/connect+json" or "application/connect+proto".
func (w *acceptWriter) accept(contentType string) {
	if w.wroteHeader {
		return
	}
	if contentType != "" {
		w.Header().Set("Content-Type", contentType)
	}
	w.WriteHeader(http.StatusOK)
	w.Flush()
}

// withAcceptWriter wraps every response so a stream handler can accept its
// subscription explicitly. It is harmless for unary calls: nothing calls
// accept, so the first WriteHeader is connect-go's own and passes straight
// through.
func withAcceptWriter(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		wrapped := &acceptWriter{ResponseWriter: w}
		ctx := context.WithValue(r.Context(), acceptWriterKey{}, wrapped)
		ctx = context.WithValue(ctx, streamContentTypeKey{}, r.Header.Get("Content-Type"))
		next.ServeHTTP(wrapped, r.WithContext(ctx))
	})
}

// acceptWriterFrom answers the wrapper installed for this request, nil when the
// request did not travel through the mux (which only a test can arrange).
func acceptWriterFrom(ctx context.Context) *acceptWriter {
	writer, _ := ctx.Value(acceptWriterKey{}).(*acceptWriter)
	return writer
}

// streamContentTypeFrom answers the request's content type.
func streamContentTypeFrom(ctx context.Context) string {
	contentType, _ := ctx.Value(streamContentTypeKey{}).(string)
	return contentType
}

// acceptNotifierKey carries a muxed subscription's acceptance signal.
type acceptNotifierKey struct{}

// withAcceptNotifier arms a subscription that runs on a page's mux rather than
// on a stream of its own.
//
// A MUXED SUBSCRIPTION HAS NO HEADERS TO FLUSH — it shares the page's — so the
// acceptance edge has to be delivered somewhere else. It is delivered to
// `SubscribePage`, which withholds its answer until it fires: the unary's
// answer is the acceptance a dedicated stream states by flushing headers, and
// it means the same thing, at the same instant in the same body.
func withAcceptNotifier(ctx context.Context, notify func()) context.Context {
	return context.WithValue(ctx, acceptNotifierKey{}, notify)
}

// acceptNotifierFrom answers the acceptance signal armed for a muxed
// subscription, nil for a stream serving its own request.
func acceptNotifierFrom(ctx context.Context) func() {
	notify, _ := ctx.Value(acceptNotifierKey{}).(func())
	return notify
}

// acceptStream flushes the response headers for a stream this handler has just
// accepted. It is called AFTER the subscription is registered and AFTER every
// refusal has been answered, so a refused open stays a refusal.
func (s *server) acceptStream(ctx context.Context, rpc string) {
	if notify := acceptNotifierFrom(ctx); notify != nil {
		notify()
		s.log.Debug(rpc, "a muxed subscription was accepted on its page's stream", nil)
		return
	}
	writer := acceptWriterFrom(ctx)
	if writer == nil {
		s.log.Debug(rpc, "no accept writer is installed for this request", nil)
		return
	}
	writer.accept(streamContentTypeFrom(ctx))
	s.log.Debug(rpc, "flushed the response headers on stream acceptance", nil)
}
