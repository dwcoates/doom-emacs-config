package server

import (
	"context"
	"errors"
	"testing"

	"connectrpc.com/connect"
)

// TestBoundaryFailureIsAnInternalError pins the answer a scope that could not
// be bound gives.
func TestBoundaryFailureIsAnInternalError(t *testing.T) {
	// Arrange.
	cause := errors.New("the registry read failed")

	// Act.
	got := boundaryFailure(cause)

	// Assert.
	if got.Code() != connect.CodeInternal {
		t.Fatalf("boundaryFailure code = %v, want internal", got.Code())
	}
	if !errors.Is(got, cause) {
		t.Fatalf("boundaryFailure = %v, want it to wrap the cause", got)
	}
}

// TestBoundaryFailureOfAnEndedRequestIsCanceled: the caller left, so the answer
// says so rather than blaming the daemon.
func TestBoundaryFailureOfAnEndedRequestIsCanceled(t *testing.T) {
	// Arrange.
	cause := context.Canceled

	// Act.
	got := boundaryFailure(cause)

	// Assert.
	if got.Code() != connect.CodeCanceled {
		t.Fatalf("boundaryFailure code = %v, want canceled", got.Code())
	}
}
