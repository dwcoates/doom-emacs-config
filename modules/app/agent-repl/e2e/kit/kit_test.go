package kit

import (
	"slices"
	"testing"
)

func TestSortedKeysAnswersTheKeysInOrder(t *testing.T) {
	tests := []struct {
		name string
		m    map[string]int
		want []string
	}{
		{"several keys", map[string]int{"b": 1, "a": 2, "c": 3}, []string{"a", "b", "c"}},
		{"no key", map[string]int{}, []string{}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := SortedKeys(tt.m)

			// Assert
			if !slices.Equal(got, tt.want) {
				t.Fatalf("SortedKeys = %q, want %q", got, tt.want)
			}
		})
	}
}
