package main

import (
	"bytes"
	"context"
	"io"
	"net/http"
)

// THE RAW-BODY RECORDING.  /_fake/calls echoes each request as protojson
// re-marshalled from the DECODED message, and protojson drops every
// zero-valued scalar on the way out.  A `false' a client sent EXPLICITLY is
// therefore indistinguishable there from one it omitted — which is exactly
// the distinction several contract sentences turn on ("`force' ... always
// encoded explicitly, false included"; CreateWorkspace's "Default false" for
// `self_certified' and `add_to_merge_queue').  Only the bytes the client
// actually put on the wire can settle it, so the mux keeps them.

// maxRecordedRawBody caps what is retained per request.  A body larger than
// this is passed through untouched and recorded with no raw — the assertion
// that needs raw is always about a small verb request.
const maxRecordedRawBody = 1 << 20

type rawBodyKey struct{}

// withRawBodyCapture buffers each agentrepl.v1 request body, hands the
// handler an identical reader, and carries the bytes to `record' through the
// request context.
func withRawBodyCapture(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.Body == nil {
			next.ServeHTTP(w, r)
			return
		}
		raw, err := io.ReadAll(io.LimitReader(r.Body, maxRecordedRawBody+1))
		buffered := io.Reader(bytes.NewReader(raw))
		if err != nil || len(raw) > maxRecordedRawBody {
			// Never swallowed: the handler still receives every byte (what was
			// buffered, then whatever remains), and the skip is logged so a
			// missing `raw' is explained rather than mysterious.
			reason := "body exceeds the recording cap"
			if err != nil {
				reason = err.Error()
			}
			logWarn("fakedaemon.raw.capture-skipped", "recorded no raw body for a request",
				map[string]any{"path": r.URL.Path, "reason": reason, "read": len(raw)})
			r.Body = io.NopCloser(io.MultiReader(buffered, r.Body))
			next.ServeHTTP(w, r)
			return
		}
		r.Body = io.NopCloser(buffered)
		next.ServeHTTP(w, r.WithContext(context.WithValue(r.Context(), rawBodyKey{}, raw)))
	})
}

// rawBodyFrom returns the exact request bytes captured for CTX, or "" when
// the capture was skipped.
func rawBodyFrom(ctx context.Context) string {
	raw, _ := ctx.Value(rawBodyKey{}).([]byte)
	return string(raw)
}
