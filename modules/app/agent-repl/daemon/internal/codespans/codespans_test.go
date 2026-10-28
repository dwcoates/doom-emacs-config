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
