package mdinline

import (
	"reflect"
	"testing"
)

// piece is a span reduced to what a case asserts: its kind and source.
type piece struct {
	Kind Kind
	Raw  string
}

func pieces(spans []Span) []piece {
	out := make([]piece, 0, len(spans))
	for _, s := range spans {
		out = append(out, piece{s.Kind, s.Raw})
	}
	return out
}

func TestSpans(t *testing.T) {
	tests := []struct {
		name string
		line string
		want []piece
	}{
		{"plain prose", "just words", []piece{{Text, "just words"}}},
		{"code span", "run `go test` now", []piece{{Text, "run "}, {Code, "`go test`"}, {Text, " now"}}},
		{"unclosed backtick is text", "a ` b", []piece{{Text, "a ` b"}}},
		{"escape", `a \` + "`b` c", []piece{{Text, "a "}, {Escape, "\\`"}, {Text, "b` c"}}},
		{"bare https URL runs through backticks", "see https://github.com/`ChessCom`/x now",
			[]piece{{Text, "see "}, {URL, "https://github.com/`ChessCom`/x"}, {Text, " now"}}},
		{"bare URL drops trailing period", "at https://x.com/a.", []piece{{Text, "at "}, {URL, "https://x.com/a"}, {Text, "."}}},
		{"bare URL stops at unbalanced paren", "(see https://x/y)`foo`",
			[]piece{{Text, "(see "}, {URL, "https://x/y"}, {Text, ")"}, {Code, "`foo`"}}},
		{"bare URL keeps balanced parens", "https://x/a_(b) z", []piece{{URL, "https://x/a_(b)"}, {Text, " z"}}},
		{"bare URL glued to a word is text", "xhttps://x/y", []piece{{Text, "xhttps://x/y"}}},
		{"www host", "go www.x.com/`a`.", []piece{{Text, "go "}, {URL, "www.x.com/`a`"}, {Text, "."}}},
		{"www glued to a slash is text", "a/www.x.com", []piece{{Text, "a/www.x.com"}}},
		{"code span holding a URL stays code", "`https://x/y` z", []piece{{Code, "`https://x/y`"}, {Text, " z"}}},
		{"autolink", "<https://a/`b`> c", []piece{{Autolink, "<https://a/`b`>"}, {Text, " c"}}},
		{"angle bracket that is no autolink", "a < b", []piece{{Text, "a < b"}}},
		{"inline link", "[`foo`](https://x/`y` \"t\") z",
			[]piece{{Link, "[`foo`](https://x/`y` \"t\")"}, {Text, " z"}}},
		{"image", "![alt](https://x/i.png)", []piece{{Link, "![alt](https://x/i.png)"}}},
		{"link text hides a bracket in code", "[a `]` b](u)", []piece{{Link, "[a `]` b](u)"}}},
		{"unclosed destination runs to the end", "[a](https://x/`y", []piece{{Link, "[a](https://x/`y"}}},
		{"brackets with no definition are text", "[note] here", []piece{{Text, "[note] here"}}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			doc := Read(tc.line)

			// Act
			got := pieces(doc.Spans(tc.line))

			// Assert
			if !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("Spans(%q) = %+v, want %+v", tc.line, got, tc.want)
			}
		})
	}
}

func TestSpansLinkParts(t *testing.T) {
	tests := []struct {
		name string
		line string
		want Span
	}{
		{"inline link", "[`a`](<https://x/`y`> 'T')",
			Span{Kind: Link, Raw: "[`a`](<https://x/`y`> 'T')", Open: "[", Label: "`a`", Mid: "](", Dest: "<https://x/`y`>", Tail: " 'T')"}},
		{"image", "![b](u)", Span{Kind: Link, Raw: "![b](u)", Open: "![", Label: "b", Mid: "](", Dest: "u", Tail: ")"}},
		{"autolink", "<https://a>", Span{Kind: Autolink, Raw: "<https://a>", Open: "<", Dest: "https://a", Tail: ">"}},
		{"bare URL", "https://a/b", Span{Kind: URL, Raw: "https://a/b", Dest: "https://a/b"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := Read(tc.line).Spans(tc.line)[0]
			if got != tc.want {
				t.Fatalf("Spans(%q)[0] = %+v, want %+v", tc.line, got, tc.want)
			}
			if got.Open+got.Label+got.Mid+got.Dest+got.Tail != got.Raw {
				t.Fatalf("the parts of %+v do not spell its source", got)
			}
		})
	}
}

func TestReferenceLinks(t *testing.T) {
	text := "[full][Ref] and [ref][] and [Ref] and [none][x]\n\n[ref]: https://a/`b` \"t\"\n"
	doc := Read(text)
	got := pieces(doc.Spans(doc.Lines[0].Text))
	want := []piece{
		{Link, "[full][Ref]"}, {Text, " and "}, {Link, "[ref][]"}, {Text, " and "}, {Link, "[Ref]"},
		{Text, " and [none][x]\n"},
	}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("Spans = %+v, want %+v", got, want)
	}
}

func TestRead(t *testing.T) {
	tests := []struct {
		name string
		text string
		want []LineKind
	}{
		{"prose", "a\nb", []LineKind{Prose, Prose}},
		{"fence", "a\n```go\nx\n```\nb", []LineKind{Prose, Fenced, Fenced, Fenced, Prose}},
		{"shorter run does not close", "````\n```\n````\nb", []LineKind{Fenced, Fenced, Fenced, Prose}},
		{"tilde fence", "~~~\n`x`\n~~~", []LineKind{Fenced, Fenced, Fenced}},
		{"indented four is no fence", "    ```\na", []LineKind{Prose, Prose}},
		{"definition at the start", "[a]: https://x\nb", []LineKind{Definition, Prose}},
		{"definition after a blank line", "p\n\n[a]: https://x", []LineKind{Prose, Prose, Definition}},
		{"definitions in a row", "[a]: u\n[b]: v", []LineKind{Definition, Definition}},
		{"definition cannot interrupt a paragraph", "p\n[a]: https://x", []LineKind{Prose, Prose}},
		{"definition with a bad title is prose", "[a]: u junk", []LineKind{Prose}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			var got []LineKind
			for _, line := range Read(tc.text).Lines {
				got = append(got, line.Kind)
			}
			if !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("Read(%q) kinds = %v, want %v", tc.text, got, tc.want)
			}
		})
	}
}

func TestDefinitionParts(t *testing.T) {
	line := Read("[Ref]:  https://a/`b` \"t\"\n").Lines[0]
	want := Span{Kind: Def, Raw: "[Ref]:  https://a/`b` \"t\"\n", Open: "[Ref]:  ", Dest: "https://a/`b`", Tail: " \"t\"\n"}
	if line.Def == nil || *line.Def != want {
		t.Fatalf("Def = %+v, want %+v", line.Def, want)
	}
}

func TestIsLink(t *testing.T) {
	for kind, want := range map[Kind]bool{Text: false, Escape: false, Code: false, Link: true, Autolink: true, URL: true, Def: true} {
		if got := (Span{Kind: kind}).IsLink(); got != want {
			t.Errorf("Span{Kind: %d}.IsLink() = %v, want %v", kind, got, want)
		}
	}
}
