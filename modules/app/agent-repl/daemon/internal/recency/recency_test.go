package recency

import (
	"slices"
	"testing"
)

func TestNewestFirst(t *testing.T) {
	tests := []struct {
		name  string
		names map[string]int64
		want  []string
	}{
		{name: "no names is an empty list", names: nil, want: []string{}},
		{name: "the latest instant comes first", names: map[string]int64{"autocompact": 10, "compact": 20}, want: []string{"compact", "autocompact"}},
		{name: "a tie is broken by name", names: map[string]int64{"b": 5, "a": 5, "c": 9}, want: []string{"c", "a", "b"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := NewestFirst(tt.names)

			// Assert
			if !slices.Equal(got, tt.want) {
				t.Fatalf("NewestFirst(%v) = %v, want %v", tt.names, got, tt.want)
			}
		})
	}
}
