package footer

import (
	"testing"
	"time"
)

func TestSubSecondLatencyDrawsInMilliseconds(t *testing.T) {
	// Arrange, Act
	got := formatLatency(412 * time.Millisecond)

	// Assert
	if got != "412ms" {
		t.Fatalf("formatLatency = %q, want %q", got, "412ms")
	}
}

func TestLatencyOverASecondDrawsInSeconds(t *testing.T) {
	// Arrange, Act
	got := formatLatency(2500 * time.Millisecond)

	// Assert
	if got != "2.5s" {
		t.Fatalf("formatLatency = %q, want %q", got, "2.5s")
	}
}

func TestTruncationLeavesAShortLineAlone(t *testing.T) {
	// Arrange, Act
	got := truncate("npm test", 40)

	// Assert
	if got != "npm test" {
		t.Fatalf("truncate = %q, want the line unchanged", got)
	}
}

func TestTruncationMarksWhatItCut(t *testing.T) {
	// Arrange, Act
	got := truncate("abcdefghij", 5)

	// Assert
	if got != "abcd…" {
		t.Fatalf("truncate = %q, want %q", got, "abcd…")
	}
}

func TestTruncationCountsRunesNotBytes(t *testing.T) {
	// Arrange, Act
	got := truncate("ααααααα", 4)

	// Assert
	if got != "ααα…" {
		t.Fatalf("truncate = %q, want three runes and the mark", got)
	}
}
