package main

import (
	"context"
	"net/http"
	"strings"
)

// THE REQUEST-HEADER RECORDING.  The Connect protocol is not only a body
// shape: fanout §3 fixes the headers a unary call carries
// (`Content-Type: application/json', `Connect-Protocol-Version: 1') and the
// distinct streaming content type (`application/connect+json').  Nothing in
// `/_fake/calls' observed them, so a client that sent the wrong content type
// — or dropped the protocol-version header a real daemon may one day require
// — passed every assertion in the suite.  A mux middleware keeps them, in
// exactly the same shape and for exactly the same reason `raw' is kept: only
// what the client actually put on the wire can settle the question.

type headersKey struct{}

// withHeaderCapture carries each agentrepl.v1 request's headers to `record'
// through the request context.  It reads only; the handler is untouched.
func withHeaderCapture(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		captured := make(map[string]string, len(r.Header))
		for name, values := range r.Header {
			// Canonical header name, values joined the way HTTP itself
			// folds a repeated header, so an assertion reads one string.
			captured[name] = strings.Join(values, ", ")
		}
		next.ServeHTTP(w, r.WithContext(context.WithValue(r.Context(), headersKey{}, captured)))
	})
}

// headersFrom returns the request headers captured for CTX, or nil when the
// middleware was not in the chain (which no served path is).
func headersFrom(ctx context.Context) map[string]string {
	headers, _ := ctx.Value(headersKey{}).(map[string]string)
	return headers
}
