// Package outputtext reads the parts of a command's captured output that a
// record or a reader-facing line quotes: its last line, and its tail.
//
// ONE PLACE for both, because every caller that runs a command and reports
// its failure needs exactly these two cuts, and two spellings of "the last
// line" disagree at the edges (a trailing blank line, an empty output).
package outputtext

import "strings"

// LastLine answers text's last non-blank line, trimmed, or "" when text has
// none.
func LastLine(text string) string {
	lines := strings.Split(text, "\n")
	for i := len(lines) - 1; i >= 0; i-- {
		if line := strings.TrimSpace(lines[i]); line != "" {
			return line
		}
	}
	return ""
}

// TailLines answers text's last n lines, with its trailing newlines dropped.
func TailLines(text string, n int) string {
	lines := strings.Split(strings.TrimRight(text, "\n"), "\n")
	if len(lines) > n {
		lines = lines[len(lines)-n:]
	}
	return strings.Join(lines, "\n")
}
