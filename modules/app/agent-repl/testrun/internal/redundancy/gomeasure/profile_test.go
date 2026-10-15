package gomeasure

import (
	"reflect"
	"strings"
	"testing"
)

func TestParseProfileSplitsReachedBlocksFromAll(t *testing.T) {
	// Arrange
	text := "mode: atomic\nm/p/a.go:3.10,5.2 2 4\nm/p/a.go:7.1,8.3 1 0\n"

	// Act
	all, covered, err := ParseProfile(text)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(all) != 2 {
		t.Fatalf("all = %v, want both blocks", all)
	}
	want := []Block{{File: "m/p/a.go", Line0: 3, Col0: 10, Line1: 5, Col1: 2}}
	if !reflect.DeepEqual(covered, want) {
		t.Fatalf("covered = %v, want %v", covered, want)
	}
	if covered[0].Key() != "m/p/a.go:3.10,5.2" {
		t.Fatalf("Key() = %q", covered[0].Key())
	}
}

func TestParseProfileRefuses(t *testing.T) {
	tests := []struct {
		name string
		text string
		want string
	}{
		{name: "no mode line", text: "m/p/a.go:3.10,5.2 2 4\n", want: "starts with its mode"},
		{name: "a line with too few fields", text: "mode: set\nm/p/a.go:3.10,5.2 2\n", want: "malformed"},
		{name: "a span without a comma", text: "mode: set\nm/p/a.go:3.10 2 1\n", want: "malformed"},
		{name: "a position without a column", text: "mode: set\nm/p/a.go:3,5.2 2 1\n", want: "malformed"},
		{name: "a count that is not a number", text: "mode: set\nm/p/a.go:3.1,5.2 2 x\n", want: "malformed"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			_, _, err := ParseProfile(tt.text)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("err = %v, want %q", err, tt.want)
			}
		})
	}
}

func TestBlockContains(t *testing.T) {
	b := Block{File: "f", Line0: 3, Col0: 10, Line1: 5, Col1: 2}
	tests := []struct {
		line, col int
		want      bool
	}{
		{3, 10, true},
		{3, 9, false},
		{4, 1, true},
		{5, 2, true},
		{5, 3, false},
		{2, 50, false},
	}
	for _, tt := range tests {
		if got := b.Contains(tt.line, tt.col); got != tt.want {
			t.Errorf("Contains(%d, %d) = %t, want %t", tt.line, tt.col, got, tt.want)
		}
	}
}

func TestHoldingIsInnermostFirstAndPerFile(t *testing.T) {
	// Arrange: a function literal's block inside the statement holding it.
	outer := Block{File: "f", Line0: 1, Col0: 1, Line1: 9, Col1: 1}
	inner := Block{File: "f", Line0: 3, Col0: 5, Line1: 4, Col1: 2}
	other := Block{File: "g", Line0: 1, Col0: 1, Line1: 9, Col1: 1}
	idx := newBlockIndex([]Block{outer, inner, other})

	// Act
	got := idx.holding("f", 3, 6)

	// Assert
	if !reflect.DeepEqual(got, []Block{inner, outer}) {
		t.Fatalf("holding = %v, want inner then outer", got)
	}
	if len(idx.holding("f", 10, 1)) != 0 {
		t.Fatal("a position past every block is held")
	}
}
