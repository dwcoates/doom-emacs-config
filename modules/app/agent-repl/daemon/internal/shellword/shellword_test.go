package shellword

import (
	"os/exec"
	"testing"
)

func TestQuote(t *testing.T) {
	tests := []struct {
		name string
		word string
		want string
	}{
		{name: "a plain word is wrapped", word: "plain", want: `'plain'`},
		{name: "an empty word stays one empty argument", word: "", want: `''`},
		{name: "spaces stay inside the one word", word: "/tmp/a dir/x", want: `'/tmp/a dir/x'`},
		{name: "a single quote is closed, escaped and reopened", word: "/tmp/it's here", want: `'/tmp/it'\''s here'`},
		{name: "shell syntax is inert", word: `$HOME; "x" $(rm -rf /)`, want: `'$HOME; "x" $(rm -rf /)'`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := Quote(tt.word)

			// Assert.
			if got != tt.want {
				t.Fatalf("Quote(%q) = %s, want %s", tt.word, got, tt.want)
			}
		})
	}
}

func TestQuoteRoundTripsThroughSh(t *testing.T) {
	words := []string{"plain", "", "a b", "it's", `'''`, `"\`, "$HOME", "line\nbreak", "tab\there"}
	for _, word := range words {
		t.Run(word, func(t *testing.T) {
			// Arrange.
			script := "printf %s " + Quote(word)

			// Act.
			out, err := exec.Command("/bin/sh", "-c", script).Output()

			// Assert.
			if err != nil {
				t.Fatalf("sh -c %q: %v", script, err)
			}
			if string(out) != word {
				t.Fatalf("sh read %q back as %q", word, out)
			}
		})
	}
}
