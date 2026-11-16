package composermentions

import (
	"slices"
	"testing"
)

func TestSlashWords(t *testing.T) {
	tests := []struct {
		name string
		text string
		want []string
	}{
		{"none in prose", "no commands here", nil},
		{"in order of first appearance, without repeats", "/b then /a then /b", []string{"b", "a"}},
		{"a plugin command keeps its colon", "run /gns-cowork:gns-bootstrap.", []string{"gns-cowork:gns-bootstrap"}},
		{"a path names no command", "see /usr/local and /Users/x/.config", nil},
		{"a bare slash names nothing", "a / b", nil},
		{"inside inline code", "use `/merge-queue` now", []string{"merge-queue"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := SlashWords(tt.text)

			// Assert
			if !slices.Equal(got, tt.want) {
				t.Fatalf("SlashWords(%q) = %q, want %q", tt.text, got, tt.want)
			}
		})
	}
}

func TestMentions(t *testing.T) {
	names := []string{"alpha", "alpha-beta", "gamma"}
	tests := []struct {
		name string
		text string
		want []string
	}{
		{"none", "nothing named", nil},
		{"a whole name in prose", "ask alpha about it", []string{"alpha"}},
		{"a longer name is not its prefix's mention", "ask alpha-beta", []string{"alpha-beta"}},
		{"a name inside a word is no mention", "alphabet and xgamma", nil},
		{"inline code and sentence punctuation border a name", "see `gamma`. Then alpha.", []string{"alpha", "gamma"}},
		{"a later whole occurrence counts after a partial one", "alphas then alpha", []string{"alpha"}},
		{"a name at the text's edges", "gamma", []string{"gamma"}},
		{"a letter beyond ASCII borders no mention", "éalpha", nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := Mentions(tt.text, names)

			// Assert
			if !slices.Equal(got, tt.want) {
				t.Fatalf("Mentions(%q) = %q, want %q", tt.text, got, tt.want)
			}
		})
	}
}
