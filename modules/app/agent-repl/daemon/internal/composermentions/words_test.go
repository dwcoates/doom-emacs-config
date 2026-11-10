package composermentions

import (
	"encoding/json"
	"os"
	"slices"
	"testing"
)

// slashWordVocab is proto/vocab/slash-word.json, the one rule the daemon, the
// composer and the webapp hold to.
type slashWordVocab struct {
	Opening string `json:"opening"`
	Closing string `json:"closing"`
	Cases   []struct {
		Text   string   `json:"text"`
		Known  []string `json:"known"`
		Marked []string `json:"marked"`
	} `json:"cases"`
}

func readSlashWordVocab(t *testing.T) slashWordVocab {
	t.Helper()
	raw, err := os.ReadFile("../../../proto/vocab/slash-word.json")
	if err != nil {
		t.Fatalf("read the slash-word vocabulary: %v", err)
	}
	var v slashWordVocab
	if err := json.Unmarshal(raw, &v); err != nil {
		t.Fatalf("decode the slash-word vocabulary: %v", err)
	}
	return v
}

func TestTheSlashWordPunctuationIsTheVocabularys(t *testing.T) {
	// Arrange
	v := readSlashWordVocab(t)

	// Act, Assert
	if opening != v.Opening || closing != v.Closing {
		t.Fatalf("opening, closing = %q, %q; the vocabulary has %q, %q", opening, closing, v.Opening, v.Closing)
	}
}

func TestSlashWordsMarksEveryVocabularyCase(t *testing.T) {
	for _, tc := range readSlashWordVocab(t).Cases {
		t.Run(tc.Text, func(t *testing.T) {
			// Act
			var got []string
			for _, name := range SlashWords(tc.Text) {
				if slices.Contains(tc.Known, name) {
					got = append(got, "/"+name)
				}
			}

			// Assert
			if !slices.Equal(got, tc.Marked) {
				t.Fatalf("marked %q, want %q", got, tc.Marked)
			}
		})
	}
}

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
