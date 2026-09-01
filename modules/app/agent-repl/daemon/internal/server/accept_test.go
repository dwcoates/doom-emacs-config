package server

import (
	"net/http"
	"net/http/httptest"
	"testing"
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
