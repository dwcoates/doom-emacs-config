package logging

import (
	"context"
	"testing"
)

func TestContextCarriesTheRequestIDToALowerLayer(t *testing.T) {
	// Arrange
	ctx := ContextWithRequestID(context.Background(), "req-1")

	// Act
	got := RequestIDFrom(ctx)

	// Assert
	if got != "req-1" {
		t.Fatalf("RequestIDFrom = %q, want req-1", got)
	}
}

func TestAnAbsentRequestIDIsOrdinary(t *testing.T) {
	// Arrange: only a caller that chose to correlate sends the header.

	// Act
	got := RequestIDFrom(context.Background())

	// Assert
	if got != "" {
		t.Fatalf("RequestIDFrom = %q, want an empty id", got)
	}
}

func TestAnEmptyRequestIDBindsNothing(t *testing.T) {
	// Arrange: binding "" would put an empty correlation key on every record of
	// every uncorrelated call.
	ctx := ContextWithRequestID(context.Background(), "")

	// Act
	got := ctx.Value(requestIDKey{})

	// Assert
	if got != nil {
		t.Fatalf("value = %v, want nothing bound", got)
	}
}

func TestRequestIDFromANilContextIsEmpty(t *testing.T) {
	// Arrange: A NIL CONTEXT IS THE SUBJECT. RequestIDFrom guards it, and the
	// guard is the reason a logging helper reached from a layer that may have
	// been handed a bare context never panics on the correlation lookup — so
	// the test hands it the one input the guard exists for. It arrives the way
	// a real caller's does, as an unset context.Context variable, which also
	// needs no linter suppression: SA1012 flags only a literal nil argument.
	var unset context.Context

	// Act
	got := RequestIDFrom(unset)

	// Assert
	if got != "" {
		t.Fatalf("RequestIDFrom(nil) = %q, want an empty id", got)
	}
}
