package recency

import (
	"os"
	"slices"
	"strings"
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

// TestTheComposerRecordsShareNewestFirst pins that both composer records state
// their order through NewestFirst rather than a sort of their own, so the two
// orders and their tie-break cannot drift apart.
func TestTheComposerRecordsShareNewestFirst(t *testing.T) {
	tests := []struct {
		name string
		path string
	}{
		{name: "the composer mentions", path: "../composermentions/keeper.go"},
		{name: "the composer selections", path: "../composerselections/keeper.go"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			src, err := os.ReadFile(tt.path)
			if err != nil {
				t.Fatalf("read %s: %v", tt.path, err)
			}

			// Act
			text := string(src)

			// Assert
			if !strings.Contains(text, "recency.NewestFirst(") {
				t.Fatalf("%s does not order its record through recency.NewestFirst", tt.path)
			}
			if strings.Contains(text, "slices.SortFunc") {
				t.Fatalf("%s sorts a record by hand beside recency.NewestFirst", tt.path)
			}
		})
	}
}
