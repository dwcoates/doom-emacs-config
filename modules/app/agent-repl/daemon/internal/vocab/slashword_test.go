package vocab_test

// The slash-word vocabulary's pins on the daemon's rule
// (internal/composermentions) live here, with the other tests that read
// proto/vocab as data, because this package's test unit is the one that
// declares that directory as its input.

import (
	"encoding/json"
	"os"
	"slices"
	"testing"

	"agent-repld/internal/composermentions"
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
	if composermentions.SlashOpening != v.Opening || composermentions.SlashClosing != v.Closing {
		t.Fatalf("opening, closing = %q, %q; the vocabulary has %q, %q", composermentions.SlashOpening, composermentions.SlashClosing, v.Opening, v.Closing)
	}
}

func TestSlashWordsMarksEveryVocabularyCase(t *testing.T) {
	for _, tc := range readSlashWordVocab(t).Cases {
		t.Run(tc.Text, func(t *testing.T) {
			// Act
			var got []string
			for _, name := range composermentions.SlashWords(tc.Text) {
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
