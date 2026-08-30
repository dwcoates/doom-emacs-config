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
	// Arrange, Act
	got := RequestIDFrom(nil) //nolint:staticcheck // the guard is the subject

	// Assert
	if got != "" {
		t.Fatalf("RequestIDFrom(nil) = %q, want an empty id", got)
	}
}
