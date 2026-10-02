// Package runecap bounds text by RUNES, never bytes, so a multibyte string is
// never split mid-character. It is the one bound every prompt a daemon model
// call composes uses: the turn summary's answer, the title synthesizer's
// prompts, and the news digest's material.
package runecap

import "unicode/utf8"

// Head keeps the first n runes of s; s itself when it holds no more.
func Head(s string, n int) string {
	if utf8.RuneCountInString(s) <= n {
		return s
	}
	return string([]rune(s)[:n])
}

// Ellipsis is Head with "…" appended when it cut, so a reader sees that the
// text goes on.
func Ellipsis(s string, n int) string {
	if utf8.RuneCountInString(s) <= n {
		return s
	}
	return Head(s, n) + "…"
}
