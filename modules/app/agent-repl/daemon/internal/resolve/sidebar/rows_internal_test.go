package sidebar

import (
	"testing"
	"time"
)

func TestLaterInstant(t *testing.T) {
	early := time.Unix(1_700_000_000, 0)
	late := early.Add(time.Hour)
	tie := early
	tests := []struct {
		name string
		a, b *time.Time
		want *time.Time
	}{
		{name: "both absent", a: nil, b: nil, want: nil},
		{name: "only the first", a: &early, b: nil, want: &early},
		{name: "only the second", a: nil, b: &early, want: &early},
		{name: "the second is later", a: &early, b: &late, want: &late},
		{name: "the first is later", a: &late, b: &early, want: &late},
		{name: "a tie keeps the first", a: &early, b: &tie, want: &early},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := laterInstant(tc.a, tc.b)

			// Assert
			if got != tc.want {
				t.Fatalf("laterInstant = %v, want %v", got, tc.want)
			}
		})
	}
}
