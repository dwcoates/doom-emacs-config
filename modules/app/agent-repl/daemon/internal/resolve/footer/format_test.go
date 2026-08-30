package footer

import (
	"testing"
	"time"
)

func TestTokensBelowAThousandDrawAsAPlainInteger(t *testing.T) {
	// Arrange, Act
	got := formatTokens(842)

	// Assert
	if got != "842" {
		t.Fatalf("formatTokens(842) = %q, want %q", got, "842")
	}
}

func TestTokensInTheThousandsDrawWithOneDecimal(t *testing.T) {
	// Arrange, Act
	got := formatTokens(18_240)

	// Assert
	if got != "18.2k" {
		t.Fatalf("formatTokens(18240) = %q, want %q", got, "18.2k")
	}
}

func TestARoundThousandDropsItsTrailingZero(t *testing.T) {
	// Arrange, Act
	got := formatTokens(3_000)

	// Assert
	if got != "3k" {
		t.Fatalf("formatTokens(3000) = %q, want %q", got, "3k")
	}
}

func TestTokensInTheMillionsDrawInMillions(t *testing.T) {
	// Arrange, Act
	got := formatTokens(2_400_000)

	// Assert
	if got != "2.4M" {
		t.Fatalf("formatTokens(2400000) = %q, want %q", got, "2.4M")
	}
}

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
