package codespans

import (
	"reflect"
	"testing"
)

func TestExtract(t *testing.T) {
	tests := []struct {
		name     string
		markdown string
		want     []string
	}{
		{"single span", "call `foo()` now", []string{"foo()"}},
		{"distinct in order, duplicates dropped", "`b` `a.go` `b` `a.go`", []string{"a.go"}},
		{"one-character span left out", "use `x` and `xy`", []string{"xy"}},
		{"double-backtick span", "see ``a`b`` ok", nil},
		{"fenced block holds no span", "```go\n`inside`\n```\nafter `out`", []string{"out"}},
		{"unclosed backtick is plain", "a ` b", nil},
		{"surrounding space trimmed", "`` ab ``", []string{"ab"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := Extract(tc.markdown)
			if !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("Extract(%q) = %q, want %q", tc.markdown, got, tc.want)
			}
		})
	}
}

func TestMarkup(t *testing.T) {
	spans := []string{"foo()", "go", "pkg/a.go", "a.go"}
	tests := []struct {
		name string
		text string
		want string
	}{
		{"plain occurrence wrapped", "run foo() now", "run `foo()` now"},
		{"every occurrence wrapped", "foo() then foo()", "`foo()` then `foo()`"},
		{"already code left alone", "run `foo()` now", "run `foo()` now"},
		{"word span not matched inside a word", "gopher and go.", "gopher and `go`."},
		{"longest span wins", "edit pkg/a.go please", "edit `pkg/a.go` please"},
		{"fenced block left alone", "```\nfoo()\n```\nfoo()", "```\nfoo()\n```\n`foo()`"},
		{"no spans leaves text", "foo()", "foo()"},
		{"multibyte text kept", "héllo foo() ünï", "héllo `foo()` ünï"},
		{"bare URL left alone, prose beside it marked", "go to https://x/foo()/go then foo()",
			"`go` to https://x/foo()/go then `foo()`"},
		{"autolink left alone", "<https://x/foo()> foo()", "<https://x/foo()> `foo()`"},
		{"link destination left alone", "[here](https://x/foo()) foo()", "[here](https://x/foo()) `foo()`"},
		{"link text left alone", "[call foo() now](https://x) foo()", "[call foo() now](https://x) `foo()`"},
		{"reference definition left alone", "[r]: https://x/a.go\n\nsee a.go", "[r]: https://x/a.go\n\nsee `a.go`"},
		{"escaped character left alone", `\` + "go", `\` + "go"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			in := spans
			if tc.name == "no spans leaves text" {
				in = nil
			}
			if got := Markup(tc.text, in); got != tc.want {
				t.Fatalf("Markup(%q) = %q, want %q", tc.text, got, tc.want)
			}
		})
	}
}

// TestMarkupLeavesThePromptsURLsWhole is the prompt that drew
// https://github.com/`ChessCom`/explanation-engine, its href broken, after a
// response had shown `ChessCom` as code (2026-11-19).
func TestMarkupLeavesThePromptsURLsWhole(t *testing.T) {
	// Arrange
	prompt := "i dont seem to have access to https://github.com/ChessCom/explanation-engine either. I can see https://github.com/ChessCom"
	spans := []string{"ChessCom", "explanation-engine"}

	// Act
	got := Markup(prompt, spans)

	// Assert
	if got != prompt {
		t.Fatalf("Markup(%q) = %q, want it unchanged", prompt, got)
	}
}

func TestUnmark(t *testing.T) {
	tests := []struct {
		name string
		text string
		want string
	}{
		{"link text drawn as plain text", "[`foo`](https://x)", "[foo](https://x)"},
		{"backticks out of a destination", "[foo](https://x/`y`)", "[foo](https://x/y)"},
		{"both at once", "[`foo`](https://x/`y`)", "[foo](https://x/y)"},
		{"title kept", "[a](https://x/`y` \"`t`\")", "[a](https://x/y \"`t`\")"},
		{"bare URL cleaned", "see https://github.com/`ChessCom`/explanation-engine now", "see https://github.com/ChessCom/explanation-engine now"},
		{"www URL cleaned", "www.x.com/`a`.", "www.x.com/a."},
		{"autolink cleaned", "<https://a/`b`>", "<https://a/b>"},
		{"image alt cleaned", "![`x`](https://a/`i`.png)", "![x](https://a/i.png)"},
		{"reference definition cleaned", "[`t`][r]\n\n[r]: https://a/`b`", "[t][r]\n\n[r]: https://a/b"},
		{"code outside links kept", "`a` and [b](u) and `c`", "`a` and [b](u) and `c`"},
		{"code span holding a URL drawn as its autolink", "open `https://x/y?a=1` now", "open <https://x/y?a=1> now"},
		{"code span URL's surrounding space trimmed", "`` https://x/y ``", "<https://x/y>"},
		{"code span holding a URL and more kept", "`curl https://x/y`", "`curl https://x/y`"},
		{"code span holding a www URL kept", "`www.x.com/a`", "`www.x.com/a`"},
		{"code span holding a non-URL kept", "`https:x`", "`https:x`"},
		{"fenced URL kept", "```\n`https://x/y`\n```", "```\n`https://x/y`\n```"},
		{"fenced block kept", "```\n[`a`](https://x/`y`)\n```", "```\n[`a`](https://x/`y`)\n```"},
		{"markup characters in code escaped", "[`*a*`](u)", "[\\*a\\*](u)"},
		{"path kept as one piece", "[`~/a/foo_bar.el`](~/a/foo_bar.el)", "[~/a/foo_bar.el](~/a/foo_bar.el)"},
		{"double-backtick code", "[``a`b``](u)", "[a\\`b](u)"},
		{"text with no backtick untouched", "[a](u) https://x", "[a](u) https://x"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			if got := Unmark(tc.text); got != tc.want {
				t.Fatalf("Unmark(%q) = %q, want %q", tc.text, got, tc.want)
			}
		})
	}
}
