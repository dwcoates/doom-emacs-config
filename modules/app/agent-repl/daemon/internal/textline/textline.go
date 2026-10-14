// Package textline reduces text to one of its lines. It is the one rule for
// "the first non-blank line, trimmed" that the footer's warning rows and the
// turn banner's summary both show.
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
