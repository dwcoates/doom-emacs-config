package mdinline

import (
	"reflect"
	"strings"
	"testing"
)

func TestRewriteProse(t *testing.T) {
	cases := []struct {
		name     string
		in       string
		arriving bool
		want     string
		stretch  []Stretch
	}{
		{name: "prose is handed to the rewrite", in: "ab", want: "<ab>",
			stretch: []Stretch{{Line: "ab", From: 0, To: 2}}},
		{name: "a code span is copied", in: "a `b` c", want: "<a >`b`< c>",
			stretch: []Stretch{{Line: "a `b` c", From: 0, To: 2}, {Line: "a `b` c", From: 5, To: 7}}},
		{name: "a fence is copied", in: "```\nx\n```\ny", want: "```\nx\n```\n<y>",
			stretch: []Stretch{{Line: "y", From: 0, To: 1}}},
		{name: "a link is copied", in: "[a](u) b", want: "[a](u)< b>",
			stretch: []Stretch{{Line: "[a](u) b", From: 6, To: 8}}},
		{name: "a bare url is copied", in: "see https://x/y", want: "<see >https://x/y",
			stretch: []Stretch{{Line: "see https://x/y", From: 0, To: 4}}},
		{name: "a definition is copied", in: "[a]: https://x\n", want: "[a]: https://x\n"},
		{name: "a settled text's end is not arriving", in: "a\nb", want: "<a\n><b>",
			stretch: []Stretch{{Line: "a\n", From: 0, To: 2}, {Line: "b", From: 0, To: 1}}},
		{name: "an arriving text's last stretch is at its end", in: "a\nb", arriving: true, want: "<a\n><b>",
			stretch: []Stretch{{Line: "a\n", From: 0, To: 2}, {Line: "b", From: 0, To: 1, AtEnd: true}}},
		{name: "an arriving text ending in code ends no stretch", in: "a `b`", arriving: true, want: "<a >`b`",
			stretch: []Stretch{{Line: "a `b`", From: 0, To: 2}}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			var seen []Stretch

			// Act
			got := RewriteProse(tc.in, tc.arriving, func(out *strings.Builder, s Stretch) {
				seen = append(seen, s)
				out.WriteString("<" + s.Line[s.From:s.To] + ">")
			})

			// Assert
			if got != tc.want {
				t.Errorf("RewriteProse() = %q, want %q", got, tc.want)
			}
			if !reflect.DeepEqual(seen, tc.stretch) {
				t.Errorf("stretches = %+v, want %+v", seen, tc.stretch)
			}
		})
	}
}

func TestStartsWord(t *testing.T) {
	cases := []struct {
		name string
		line string
		at   int
		want bool
	}{
		{"the line's start", "#a", 0, true},
		{"after a space", "x #a", 2, true},
		{"after a tab", "x\t#a", 2, true},
		{"after an opening parenthesis", "(#a", 1, true},
		{"glued to a word", "x#a", 1, false},
		{"after an ampersand", "&#1", 1, false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			if got := StartsWord(tc.line, tc.at); got != tc.want {
				t.Errorf("StartsWord(%q, %d) = %v, want %v", tc.line, tc.at, got, tc.want)
			}
		})
	}
}

func TestIsWordByte(t *testing.T) {
	for _, c := range []byte("azAZ09_") {
		if !IsWordByte(c) {
			t.Errorf("IsWordByte(%q) = false, want true", c)
		}
	}
	for _, c := range []byte(" -.#)\n") {
		if IsWordByte(c) {
			t.Errorf("IsWordByte(%q) = true, want false", c)
		}
	}
}

func TestWriteLink(t *testing.T) {
	// Arrange
	var out strings.Builder

	// Act
	WriteLink(&out, "#12", "https://x/pull/12")

	// Assert
	if got, want := out.String(), "[#12](https://x/pull/12)"; got != want {
		t.Errorf("WriteLink() wrote %q, want %q", got, want)
	}
}
