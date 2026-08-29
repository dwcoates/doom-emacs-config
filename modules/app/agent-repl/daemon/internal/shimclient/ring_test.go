package shimclient

import (
	"strings"
	"testing"
)

// TestRingKeepsEverythingUnderCapacity asserts a small stderr survives whole.
func TestRingKeepsEverythingUnderCapacity(t *testing.T) {
	// Arrange.
	r := newRing(16)

	// Act.
	if _, err := r.Write([]byte("hello")); err != nil {
		t.Fatalf("Write() error = %v", err)
	}

	// Assert.
	if got := r.String(); got != "hello" {
		t.Fatalf("String() = %q, want %q", got, "hello")
	}
}

// TestRingDropsTheOldestBytes asserts the ring keeps the TAIL: the last words
// of a dying shim are the evidence, not its first.
func TestRingDropsTheOldestBytes(t *testing.T) {
	// Arrange.
	r := newRing(8)
	if _, err := r.Write([]byte("aaaaaa")); err != nil {
		t.Fatalf("Write() error = %v", err)
	}

	// Act.
	if _, err := r.Write([]byte("bbbb")); err != nil {
		t.Fatalf("Write() error = %v", err)
	}

	// Assert.
	if got := r.String(); got != "aaaabbbb" {
		t.Fatalf("String() = %q, want %q", got, "aaaabbbb")
	}
}

// TestRingKeepsTheTailOfAnOversizedWrite asserts one write larger than the
// whole ring keeps its tail rather than failing.
func TestRingKeepsTheTailOfAnOversizedWrite(t *testing.T) {
	// Arrange.
	r := newRing(4)

	// Act.
	n, err := r.Write([]byte("abcdefgh"))

	// Assert.
	if err != nil {
		t.Fatalf("Write() error = %v", err)
	}
	if n != 8 {
		t.Fatalf("Write() n = %d, want 8 (a short write would lose stderr)", n)
	}
	if got := r.String(); got != "efgh" {
		t.Fatalf("String() = %q, want %q", got, "efgh")
	}
}

// TestRingCapacityFallsBackToTheDefault asserts a nonsense capacity does not
// produce a ring that keeps nothing.
func TestRingCapacityFallsBackToTheDefault(t *testing.T) {
	// Arrange.
	r := newRing(0)

	// Act.
	if _, err := r.Write([]byte(strings.Repeat("x", 100))); err != nil {
		t.Fatalf("Write() error = %v", err)
	}

	// Assert.
	if got := len(r.String()); got != 100 {
		t.Fatalf("kept %d bytes, want 100", got)
	}
}
