package figures

import (
	"math"
	"testing"
)

func TestTokenHeatPlacesAFigureOnTheGradient(t *testing.T) {
	tests := []struct {
		name  string
		fresh uint64
		want  float64
	}{
		{name: "nothing is green", fresh: 0, want: 0},
		{name: "halfway to yellow", fresh: 15_000, want: 1.0 / 6},
		{name: "yellow at 30k", fresh: 30_000, want: 1.0 / 3},
		{name: "halfway from yellow to orange", fresh: 40_000, want: 0.5},
		{name: "orange at 50k", fresh: 50_000, want: 2.0 / 3},
		{name: "halfway from orange to red", fresh: 75_000, want: 5.0 / 6},
		{name: "red at 100k", fresh: 100_000, want: 1},
		{name: "held at red past the last stop", fresh: 250_000, want: 1},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := TokenHeat(tc.fresh)

			// Assert
			if math.Abs(got-tc.want) > 1e-9 {
				t.Fatalf("TokenHeat(%d) = %v, want %v", tc.fresh, got, tc.want)
			}
		})
	}
}
