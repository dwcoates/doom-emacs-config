package server

import (
	"errors"
	"os"
	"strings"
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

// TestEveryWrapperAnswersABoundaryFailureThroughTheOneHelper fails a wrapper
// that hand-rolls its own answer instead of sharing boundaryFailure.
func TestEveryWrapperAnswersABoundaryFailureThroughTheOneHelper(t *testing.T) {
	// Arrange.
	raw, err := os.ReadFile("requestlog_server.go")
	if err != nil {
		t.Fatalf("read requestlog_server.go: %v", err)
	}
	source := string(raw)

	// Act.
	begins := strings.Count(source, "s.server.beginRequest(")
	shared := strings.Count(source, "boundaryFailure(err)")

	// Assert.
	if begins == 0 || begins != shared {
		t.Fatalf("%d wrappers call beginRequest and %d answer through boundaryFailure; every wrapper must share the helper", begins, shared)
	}
	if strings.Contains(source, "connect.CodeInternal") {
		t.Fatal("a wrapper spells its own internal error instead of calling boundaryFailure")
	}
}
