package topbar

import "testing"

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
	got := formatTokens(142_300)

	// Assert
	if got != "142.3k" {
		t.Fatalf("formatTokens(142300) = %q, want %q", got, "142.3k")
	}
}

func TestARoundThousandDropsItsTrailingZero(t *testing.T) {
	// Arrange, Act
	got := formatTokens(200_000)

	// Assert
	if got != "200k" {
		t.Fatalf("formatTokens(200000) = %q, want %q", got, "200k")
	}
}

func TestTokensInTheMillionsDrawInMillions(t *testing.T) {
	// Arrange, Act
	got := formatTokens(1_500_000)

	// Assert
	if got != "1.5M" {
		t.Fatalf("formatTokens(1500000) = %q, want %q", got, "1.5M")
	}
}

func TestANegativeCountDrawsAsZero(t *testing.T) {
	// Arrange, Act
	got := formatTokens(-5)

	// Assert
	if got != "0" {
		t.Fatalf("formatTokens(-5) = %q, want %q", got, "0")
	}
}

func TestAPercentOfNoBasisIsEmpty(t *testing.T) {
	// Arrange, Act
	got := percentOf(100, 0)

	// Assert
	if got != "" {
		t.Fatalf("percentOf(100, 0) = %q, want empty: there is nothing to divide by", got)
	}
}

func TestAPercentRoundsToTheNearestWhole(t *testing.T) {
	// Arrange, Act
	got := percentOf(38_100, 200_000)

	// Assert
	if got != "19%" {
		t.Fatalf("percentOf = %q, want %q", got, "19%")
	}
}

func TestAPermilleOfNoBasisReportsNoShare(t *testing.T) {
	// Arrange, Act
	_, ok := permilleOf(100, 0)

	// Assert
	if ok {
		t.Fatalf("permilleOf reported a share with no basis")
	}
}

func TestAPermilleIsRoundedAndCapped(t *testing.T) {
	// Arrange, Act
	got, ok := permilleOf(500, 1_000)

	// Assert
	if !ok || got != 500 {
		t.Fatalf("permilleOf(500, 1000) = %d (ok=%v), want 500", got, ok)
	}
}

func TestAPermilleNeverExceedsAThousand(t *testing.T) {
	// Arrange, Act
	got, _ := permilleOf(2_000, 1_000)

	// Assert
	if got != 1_000 {
		t.Fatalf("permilleOf(2000, 1000) = %d, want the capped 1000", got)
	}
}

func TestJoinNonEmptyDropsWhatWasNotStated(t *testing.T) {
	// Arrange, Act
	got := joinNonEmpty(" · ", "vend-1", "", "claude-opus-5")

	// Assert
	if got != "vend-1 · claude-opus-5" {
		t.Fatalf("joinNonEmpty = %q, want no bare separator around an unstated fact", got)
	}
}

func TestPluralRendersTheSingular(t *testing.T) {
	// Arrange, Act
	got := plural(1, "response")

	// Assert
	if got != "1 response" {
		t.Fatalf("plural = %q, want %q", got, "1 response")
	}
}

func TestPluralRendersThePlural(t *testing.T) {
	// Arrange, Act
	got := plural(3, "response")

	// Assert
	if got != "3 responses" {
		t.Fatalf("plural = %q, want %q", got, "3 responses")
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

func TestAbbreviatingNoArgumentsYieldsNoLines(t *testing.T) {
	// Arrange, Act
	got := abbreviateArguments(nil)

	// Assert
	if got != nil {
		t.Fatalf("lines = %+v, want none", got)
	}
}
