package shellword

import (
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
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

// TestNoProductionSourceHandRollsTheQuoting fails on any daemon source outside
// this package that spells the quote-escape idiom itself, so every caller goes
// through Quote rather than a copy that can drift.
func TestNoProductionSourceHandRollsTheQuoting(t *testing.T) {
	// Arrange.
	root := filepath.Join("..", "..")
	self, err := filepath.Abs(".")
	if err != nil {
		t.Fatalf("resolve this package: %v", err)
	}
	var offenders []string

	// Act.
	walkErr := filepath.WalkDir(root, func(path string, entry fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if entry.IsDir() {
			abs, absErr := filepath.Abs(path)
			if absErr != nil {
				return absErr
			}
			if abs == self || entry.Name() == "node_modules" {
				return filepath.SkipDir
			}
			return nil
		}
		if filepath.Ext(path) != ".go" || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		source, readErr := os.ReadFile(path)
		if readErr != nil {
			return readErr
		}
		if strings.Contains(string(source), `'\''`) {
			offenders = append(offenders, path)
		}
		return nil
	})

	// Assert.
	if walkErr != nil {
		t.Fatalf("walk the daemon sources: %v", walkErr)
	}
	if len(offenders) > 0 {
		t.Fatalf("these sources quote for sh by hand; call shellword.Quote instead: %v", offenders)
	}
}
