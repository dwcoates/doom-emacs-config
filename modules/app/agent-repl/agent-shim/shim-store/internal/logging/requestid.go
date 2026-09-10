package logging

import "context"

// requestIDKey addresses the caller's correlation id on a request context. It
// is an unexported struct type so nothing outside this package can collide with
// it or forge one.
type requestIDKey struct{}

// ContextWithRequestID carries the caller's correlation id down to the layers
// that do the work.
//
// IT RIDES THE CONTEXT RATHER THAN A LOGGER because the storage layer is
// constructed once, at boot, and its logger belongs to the process — while a
// request id belongs to one call in flight. Handing internal/db a per-request
// logger would mean either rebuilding it per call or passing it through every
// signature; the context is the parameter that already crosses every one of
// them and already means "this call".
func ContextWithRequestID(ctx context.Context, requestID string) context.Context {
	if requestID == "" {
		return ctx
	}
	return context.WithValue(ctx, requestIDKey{}, requestID)
}

// RequestIDFrom reports the correlation id bound to ctx, or "" when the caller
// sent none. An absent id is ordinary: only a caller that chose to correlate
// sends the header.
func RequestIDFrom(ctx context.Context) string {
	if ctx == nil {
		return ""
	}
	id, _ := ctx.Value(requestIDKey{}).(string)
	return id
}
