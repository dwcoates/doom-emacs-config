// Package textline holds the daemon's rules for reading text by its lines:
// "the first non-blank line, trimmed" that the footer's warning rows and the
// turn banner's summary both show, and the split of a text into its lines
// that a drawn line list and a line diff both read; and the rule a short
// drawn text that must be one sentence of a bounded word count is held to.
package textline

import (
	"errors"
	"fmt"
	"strings"
	"unicode"
	"unicode/utf8"
)

// FirstNonBlank is the first line of text that is not blank, trimmed of
// surrounding whitespace; "" when every line is blank.
func FirstNonBlank(text string) string {
	for _, line := range strings.Split(text, "\n") {
		if trimmed := strings.TrimSpace(line); trimmed != "" {
			return trimmed
		}
	}
	return ""
}

// Lines splits text into its lines. A final newline ends the last line rather
// than opening an empty one, and the empty text has no lines.
func Lines(text string) []string {
	if text == "" {
		return nil
	}
	return strings.Split(strings.TrimSuffix(text, "\n"), "\n")
}

// OneSentence answers why TEXT is not one line holding one sentence of MIN to
// MAX words with no whitespace at either end, worded for NOUN ("a label is
// one line"); nil when it is. The sentence may end on its last word's period,
// but no earlier word ends one.
//
// It is the ONE rule every short text the daemon draws in a bounded slot is
// held to: a link's label in the topbar summary's menu, and the summary
// itself.
func OneSentence(noun, text string, min, max int) error {
	if strings.ContainsAny(text, "\r\n") {
		return errors.New("a " + noun + " is one line")
	}
	if strings.TrimSpace(text) != text {
		return errors.New("a " + noun + " carries no whitespace at either end")
	}
	words := strings.Fields(text)
	if len(words) < min || len(words) > max {
		return fmt.Errorf("a %s is %d to %d words, not %d", noun, min, max, len(words))
	}
	for _, word := range words[:len(words)-1] {
		if last, _ := utf8.DecodeLastRuneInString(word); isSentenceEnd(last) {
			return fmt.Errorf("a %s is one sentence, and %q ends one before its last word", noun, word)
		}
	}
	return nil
}

// isSentenceEnd reports whether R ends a sentence.
func isSentenceEnd(r rune) bool {
	return r == '.' || r == '!' || r == '?' || unicode.Is(unicode.Sentence_Terminal, r)
}
