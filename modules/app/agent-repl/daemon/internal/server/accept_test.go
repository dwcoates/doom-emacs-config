package server

import (
	"context"
	"net/http"
	"net/http/httptest"
	"testing"

	"agent-repld/internal/dlog"
)

// TestAcceptFlushesTheStreamingContentType pins the acceptance mechanism: the
// headers are the acceptance, and they carry the request's streaming codec.
func TestAcceptFlushesTheStreamingContentType(t *testing.T) {
	// Arrange.
	recorder := httptest.NewRecorder()
	writer := &acceptWriter{ResponseWriter: recorder}

	// Act.
	writer.accept("application/connect+json")

	// Assert.
	if got := recorder.Header().Get("Content-Type"); got != "application/connect+json" {
		t.Fatalf("Content-Type = %q, want the request's streaming codec", got)
	}
}

// TestAcceptStatesTheAcceptanceInItsHeaders pins what a daemon relaying this
// stream reads: a refused stream answers headers too, so the acceptance is a
// header of its own and not the headers' arrival.
func TestAcceptStatesTheAcceptanceInItsHeaders(t *testing.T) {
	// Arrange.
	recorder := httptest.NewRecorder()
	writer := &acceptWriter{ResponseWriter: recorder}

	// Act.
	writer.accept("application/connect+json")

	// Assert.
	if recorder.Header().Get(AcceptedHeader) == "" {
		t.Fatal("an accepted stream's headers do not state its acceptance")
	}
}

// TestAResponseNeverAcceptedStatesNoAcceptance pins the other half: a
// response the handler wrote without accepting carries no acceptance.
func TestAResponseNeverAcceptedStatesNoAcceptance(t *testing.T) {
	// Arrange.
	recorder := httptest.NewRecorder()
	writer := &acceptWriter{ResponseWriter: recorder}

	// Act.
	writer.WriteHeader(http.StatusOK)

	// Assert.
	if got := recorder.Header().Get(AcceptedHeader); got != "" {
		t.Fatalf("a response nobody accepted states acceptance %q", got)
	}
}

// TestAcceptStreamRunsItsHookBeforeStatingTheAcceptance pins the order a
// relayed page's hold depends on: the hook has run by the time the client can
// read that the stream is accepted.
func TestAcceptStreamRunsItsHookBeforeStatingTheAcceptance(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	recorder := httptest.NewRecorder()
	writer := &acceptWriter{ResponseWriter: recorder}
	var headersWrittenAtHook bool
	ctx := context.WithValue(context.Background(), acceptWriterKey{}, writer)
	ctx = withAcceptHook(ctx, func() { headersWrittenAtHook = writer.wroteHeader })

	// Act.
	h.Server.(*server).acceptStream(ctx, "WatchFooter")

	// Assert.
	if headersWrittenAtHook || !writer.wroteHeader {
		t.Fatalf("the hook ran with headers written = %v, and the stream ended with headers written = %v", headersWrittenAtHook, writer.wroteHeader)
	}
}

// TestAcceptSendsStatusOK pins that acceptance is a 200 before any frame.
func TestAcceptSendsStatusOK(t *testing.T) {
	// Arrange.
	recorder := httptest.NewRecorder()
	writer := &acceptWriter{ResponseWriter: recorder}

	// Act.
	writer.accept("application/connect+proto")

	// Assert.
	if recorder.Code != http.StatusOK {
		t.Fatalf("status = %d, want %d", recorder.Code, http.StatusOK)
	}
}

// TestAcceptIsIdempotent pins that a second accept does not re-send headers,
// which would be a superfluous WriteHeader the runtime logs.
func TestAcceptIsIdempotent(t *testing.T) {
	// Arrange.
	recorder := httptest.NewRecorder()
	writer := &acceptWriter{ResponseWriter: recorder}
	writer.accept("application/connect+json")

	// Act.
	writer.accept("application/connect+proto")

	// Assert.
	if got := recorder.Header().Get("Content-Type"); got != "application/connect+json" {
		t.Fatalf("Content-Type = %q, want the first acceptance's", got)
	}
}

// TestALaterAgreeingWriteHeaderIsSwallowed pins that connect-go's own
// WriteHeader after acceptance passes without a recorded conflict.
func TestALaterAgreeingWriteHeaderIsSwallowed(t *testing.T) {
	// Arrange.
	recorder := httptest.NewRecorder()
	writer := &acceptWriter{ResponseWriter: recorder}
	writer.accept("application/connect+json")

	// Act.
	writer.WriteHeader(http.StatusOK)

	// Assert.
	if writer.conflict != nil {
		t.Fatalf("conflict = %v, want none for an agreeing status", *writer.conflict)
	}
}

// TestADisagreeingWriteHeaderIsRecorded pins that a handler trying to REFUSE a
// stream this writer already accepted is never silently swallowed.
func TestADisagreeingWriteHeaderIsRecorded(t *testing.T) {
	// Arrange.
	recorder := httptest.NewRecorder()
	writer := &acceptWriter{ResponseWriter: recorder}
	writer.accept("application/connect+json")

	// Act.
	writer.WriteHeader(http.StatusInternalServerError)

	// Assert.
	if writer.conflict == nil || *writer.conflict != http.StatusInternalServerError {
		t.Fatalf("conflict = %v, want the disagreeing status recorded", writer.conflict)
	}
}

// TestUnaryResponsesAreUntouched pins that a unary call's first WriteHeader is
// connect-go's own and passes straight through.
func TestUnaryResponsesAreUntouched(t *testing.T) {
	// Arrange.
	recorder := httptest.NewRecorder()
	writer := &acceptWriter{ResponseWriter: recorder}

	// Act.
	writer.WriteHeader(http.StatusTeapot)

	// Assert.
	if recorder.Code != http.StatusTeapot {
		t.Fatalf("status = %d, want %d", recorder.Code, http.StatusTeapot)
	}
}

// TestUnwrapReachesTheRealWriter pins that http.NewResponseController — which
// connect-go uses to set write deadlines — still reaches the real writer.
func TestUnwrapReachesTheRealWriter(t *testing.T) {
	// Arrange.
	recorder := httptest.NewRecorder()
	writer := &acceptWriter{ResponseWriter: recorder}

	// Act.
	unwrapped := writer.Unwrap()

	// Assert.
	if unwrapped != http.ResponseWriter(recorder) {
		t.Fatal("Unwrap did not answer the real ResponseWriter")
	}
}

// TestAcceptStreamLogsUnderTheContractedOperation pins that every branch of a
// stream's acceptance is recorded as daemon.server.stream_accept with the rpc
// as context, never under the bare rpc name.
func TestAcceptStreamLogsUnderTheContractedOperation(t *testing.T) {
	cases := []struct {
		name    string
		ctx     func() context.Context
		message string
	}{
		{name: "a muxed subscription", message: "a muxed subscription was accepted on its page's stream",
			ctx: func() context.Context { return withAcceptNotifier(context.Background(), func() {}) }},
		{name: "no accept writer", message: "no accept writer is installed for this request",
			ctx: context.Background},
		{name: "a dedicated stream", message: "flushed the response headers on stream acceptance",
			ctx: func() context.Context {
				return context.WithValue(context.Background(), acceptWriterKey{}, &acceptWriter{ResponseWriter: httptest.NewRecorder()})
			}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			log := dlog.NewTestLogger()
			s := &server{log: log}

			// Act.
			s.acceptStream(tc.ctx(), "WatchExample")

			// Assert.
			r, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Message: tc.message})
			if !ok || r.Operation != "daemon.server.stream_accept" || r.Context["rpc"] != "WatchExample" {
				t.Fatalf("records = %+v, want %q under daemon.server.stream_accept naming the rpc", log.Records(), tc.message)
			}
		})
	}
}
