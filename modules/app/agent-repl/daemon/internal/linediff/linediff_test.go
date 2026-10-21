package linediff

import (
	"reflect"
	"strings"
	"testing"
)

func ctx(text string) Line { return Line{Kind: Context, Text: text} }
func del(text string) Line { return Line{Kind: Removed, Text: text} }
func add(text string) Line { return Line{Kind: Added, Text: text} }

func TestLines(t *testing.T) {
	tests := []struct {
		name string
		text string
		want []string
	}{
		{name: "the empty text has no lines", text: "", want: nil},
		{name: "a final newline ends the last line", text: "a\nb\n", want: []string{"a", "b"}},
		{name: "an unterminated last line is still a line", text: "a\nb", want: []string{"a", "b"}},
		{name: "a lone newline is one empty line", text: "\n", want: []string{""}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := Lines(tt.text)

			// Assert.
			if !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("Lines(%q) = %q, want %q", tt.text, got, tt.want)
			}
		})
	}
}

func TestDiff(t *testing.T) {
	tests := []struct {
		name     string
		old, new string
		want     []Line
	}{
		{
			name: "identical texts are all context",
			old:  "a\nb", new: "a\nb",
			want: []Line{ctx("a"), ctx("b")},
		},
		{
			name: "one changed line among shared ones",
			old:  "a\nb\nc", new: "a\nB\nc",
			want: []Line{ctx("a"), del("b"), add("B"), ctx("c")},
		},
		{
			name: "an inserted line",
			old:  "a\nc", new: "a\nb\nc",
			want: []Line{ctx("a"), add("b"), ctx("c")},
		},
		{
			name: "a deleted line",
			old:  "a\nb\nc", new: "a\nc",
			want: []Line{ctx("a"), del("b"), ctx("c")},
		},
		{
			name: "the empty old text is all added",
			old:  "", new: "x\ny",
			want: []Line{add("x"), add("y")},
		},
		{
			name: "the empty new text is all removed",
			old:  "x\ny", new: "",
			want: []Line{del("x"), del("y")},
		},
		{
			name: "a line shared inside a changed stretch stays context",
			old:  "p\nkeep\nq", new: "P\nkeep\nQ",
			want: []Line{del("p"), add("P"), ctx("keep"), del("q"), add("Q")},
		},
		{
			name: "removals come before additions in one changed stretch",
			old:  "a\nb", new: "c\nd",
			want: []Line{del("a"), del("b"), add("c"), add("d")},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := Diff(tt.old, tt.new)

			// Assert.
			if !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("Diff(%q, %q) = %v, want %v", tt.old, tt.new, got, tt.want)
			}
		})
	}
}

func TestDiffPastTheTableBoundIsUnmatched(t *testing.T) {
	// Arrange: two stretches whose product passes MaxCells, sharing a line
	// the table would otherwise have matched.
	n := 2001
	a := make([]string, n)
	b := make([]string, n)
	for i := range a {
		a[i] = "old"
		b[i] = "new"
	}
	a[n/2], b[n/2] = "shared", "shared"

	// Act.
	got := Diff(strings.Join(a, "\n"), strings.Join(b, "\n"))

	// Assert.
	if len(got) != 2*n {
		t.Fatalf("got %d lines, want %d", len(got), 2*n)
	}
	for i, line := range got {
		want := Removed
		if i >= n {
			want = Added
		}
		if line.Kind != want {
			t.Fatalf("line %d is kind %d, want %d", i, line.Kind, want)
		}
	}
}
