package runecap

import (
	"testing"
)

func TestHead(t *testing.T) {
	tests := []struct {
		name string
		s    string
		n    int
		want string
	}{
		{name: "shorter than the bound", s: "abc", n: 5, want: "abc"},
		{name: "exactly the bound", s: "abc", n: 3, want: "abc"},
		{name: "longer than the bound", s: "abcdef", n: 3, want: "abc"},
		{name: "multibyte runes are kept whole", s: "héllo wörld", n: 4, want: "héll"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := Head(tt.s, tt.n)

			// Assert
			if got != tt.want {
				t.Fatalf("Head(%q, %d) = %q, want %q", tt.s, tt.n, got, tt.want)
			}
		})
	}
}

func TestEllipsis(t *testing.T) {
	tests := []struct {
		name string
		s    string
		n    int
		want string
	}{
		{name: "an uncut string has no ellipsis", s: "abc", n: 3, want: "abc"},
		{name: "a cut string ends in one", s: "abcdef", n: 3, want: "abc…"},
		{name: "multibyte runes are kept whole", s: "ääää", n: 2, want: "ää…"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := Ellipsis(tt.s, tt.n)

			// Assert
			if got != tt.want {
				t.Fatalf("Ellipsis(%q, %d) = %q, want %q", tt.s, tt.n, got, tt.want)
			}
		})
	}
}
