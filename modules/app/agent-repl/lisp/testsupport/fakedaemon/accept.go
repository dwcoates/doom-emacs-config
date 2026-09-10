package main

import (
	"context"
	"net/http"
)

// THE STANDING-STREAM ACCEPTANCE RULE (project lead): a client treats the
// ARRIVAL OF RESPONSE HEADERS as the stream being accepted, before any frame
// exists, and only ever ends a watch by killing its transport.  So the daemon
// must flush status 200 and the streaming content type the moment it accepts a
// subscription — a watch with nothing to say yet is still an ACCEPTED watch.
//
// connect-go does not do this on its own: it writes headers lazily, on the
// handler's first Send.  A subscriber with no snapshot and no push would
// therefore leave the client's open outstanding indefinitely, and every client
// would read "accepted but quiet" as "not accepted".
//
// acceptWriter is the mechanism.  The mux wraps every response in one, the
// stream handlers call `accept' once the subscription is registered, and
// connect-go's own later WriteHeader is swallowed so the runtime never logs a
// superfluous call.

type acceptWriterKey struct{}

type acceptWriter struct {
	http.ResponseWriter
	wroteHeader bool
	// acceptedCode is the status this writer sent on acceptance, kept so a
	// later disagreement from connect-go is visible rather than silent.
	acceptedCode int
}

// Unwrap lets http.NewResponseController — which connect-go uses to set write
// deadlines — reach the real ResponseWriter through this wrapper.
func (w *acceptWriter) Unwrap() http.ResponseWriter { return w.ResponseWriter }

func (w *acceptWriter) WriteHeader(code int) {
	if w.wroteHeader {
		if code != w.acceptedCode {
			// Never silently swallow a DIFFERENT status: it would mean the
			// handler tried to refuse a stream this writer already accepted.
			logWarn("fakedaemon.stream.header-conflict",
				"a second WriteHeader disagreed with the accepted status",
				map[string]any{"accepted": w.acceptedCode, "attempted": code})
		}
		return
	}
	w.wroteHeader = true
	w.acceptedCode = code
	w.ResponseWriter.WriteHeader(code)
}

func (w *acceptWriter) Flush() {
	if flusher, ok := w.ResponseWriter.(http.Flusher); ok {
		flusher.Flush()
	}
}

// accept sends and flushes the response headers for an accepted stream.
// CONTENTTYPE is echoed from the request, which for a connect streaming call
// is `application/connect+json'.
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
// subscription explicitly.  Harmless for unary calls: nothing calls `accept',
// so the first WriteHeader is connect-go's own and passes straight through.
func withAcceptWriter(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		wrapped := &acceptWriter{ResponseWriter: w}
		ctx := context.WithValue(r.Context(), acceptWriterKey{}, wrapped)
		ctx = withStreamContentType(ctx, r.Header.Get("Content-Type"))
		next.ServeHTTP(wrapped, r.WithContext(ctx))
	})
}

func acceptWriterFrom(ctx context.Context) *acceptWriter {
	writer, _ := ctx.Value(acceptWriterKey{}).(*acceptWriter)
	return writer
}
