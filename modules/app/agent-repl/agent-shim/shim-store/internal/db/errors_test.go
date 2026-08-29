package db

import (
	"errors"
	"strings"
	"testing"
)

func TestInvalidfIsErrInvalid(t *testing.T) {
	// Arrange, Act
	err := invalidf("field %s is empty", "write_id")

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("error = %v, want ErrInvalid", err)
	}
	if !strings.Contains(err.Error(), "field write_id is empty") {
		t.Fatalf("detail lost: %v", err)
	}
}

func TestStalePointerfIsErrStalePointer(t *testing.T) {
	// Arrange, Act
	err := stalePointerf("after %q names no line of book %q", "sip1-1", "agent-1")

	// Assert
	if !errors.Is(err, ErrStalePointer) {
		t.Fatalf("error = %v, want ErrStalePointer", err)
	}
}

func TestStoragefKeepsTheDriverCauseReachable(t *testing.T) {
	// Arrange: the driver's own classification must survive the wrapping, or
	// an operator loses the only account of what the database actually said.
	cause := errors.New("disk I/O error")

	// Act
	err := storagef(cause, "writing entry %s", "u1")

	// Assert
	if !errors.Is(err, ErrStorage) {
		t.Fatalf("error = %v, want ErrStorage", err)
	}
	if !errors.Is(err, cause) {
		t.Fatalf("driver cause is unreachable through %v", err)
	}
}

func TestSentinelsAreDistinct(t *testing.T) {
	// Arrange, Act, Assert: a refusal must never satisfy two classes at once,
	// or the server would map one refusal to two failure arms.
	if errors.Is(invalidf("x"), ErrStorage) {
		t.Fatal("an invalid request satisfied ErrStorage")
	}
	if errors.Is(stalePointerf("x"), ErrInvalid) {
		t.Fatal("a stale pointer satisfied ErrInvalid")
	}
}
