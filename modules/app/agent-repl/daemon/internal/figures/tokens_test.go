package figures

import "testing"

func TestTokensRendersEveryMagnitudeTheArchitectureFixes(t *testing.T) {
	tests := []struct {
		name string
		n    uint64
		want string
	}{
		{name: "zero is bare digits", n: 0, want: "0"},
		{name: "the last unscaled count", n: 999, want: "999"},
		{name: "a whole thousand drops the fraction", n: 1_000, want: "1k"},
		{name: "one fractional digit in the thousands", n: 1_200, want: "1.2k"},
		{name: "one fractional digit in the ten-thousands", n: 12_340, want: "12.3k"},
		{name: "a rounded zero fraction is trimmed", n: 182_000, want: "182k"},
		{name: "the last count that still renders in thousands", n: 999_949, want: "999.9k"},
		{name: "a count whose thousands-rendering would read 1000k", n: 999_950, want: "1M"},
		{name: "one fractional digit in the millions", n: 1_200_000, want: "1.2M"},
		{name: "a plain integer under a thousand", n: 842, want: "842"},
		{name: "a plain integer just under the scale", n: 940, want: "940"},
		{name: "the proto's own thousands example", n: 18_240, want: "18.2k"},
		{name: "a whole thousand drops its fraction", n: 18_000, want: "18k"},
		{name: "a round three thousand", n: 3_000, want: "3k"},
		{name: "the proto's second thousands example", n: 142_300, want: "142.3k"},
		{name: "two hundred thousand drops its fraction", n: 200_000, want: "200k"},
		{name: "a millions figure with a fraction", n: 1_430_000, want: "1.4M"},
		{name: "one and a half million", n: 1_500_000, want: "1.5M"},
		{name: "two point four million", n: 2_400_000, want: "2.4M"},
		{name: "a whole million drops its fraction", n: 2_000_000, want: "2M"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange, Act.
			got := Tokens(tt.n)

			// Assert.
			if got != tt.want {
				t.Fatalf("Tokens(%d) = %q, want %q", tt.n, got, tt.want)
			}
		})
	}
}

func TestTokensKeepsAFractionAtEveryScaledMagnitude(t *testing.T) {
	// Arrange, Act: there is no drop-the-fraction-from-ten rule.
	got := Tokens(182_400)

	// Assert.
	if got != "182.4k" {
		t.Fatalf("Tokens(182400) = %q, want %q", got, "182.4k")
	}
}

func TestTokensPicksTheUnitByTheRenderedValueAtTheMillionBoundary(t *testing.T) {
	// Arrange, Act: the raw count is below a million, the RENDERED one is not.
	below, at := Tokens(999_949), Tokens(999_950)

	// Assert.
	if below != "999.9k" || at != "1M" {
		t.Fatalf("boundary = %q/%q, want %q/%q", below, at, "999.9k", "1M")
	}
}
