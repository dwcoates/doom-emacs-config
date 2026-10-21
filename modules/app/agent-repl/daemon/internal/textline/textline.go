// Package textline holds the daemon's rules for reading text by its lines:
// "the first non-blank line, trimmed" that the footer's warning rows and the
// turn banner's summary both show, and the split of a text into its lines
// that a drawn line list and a line diff both read.
package textline

import "strings"

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
