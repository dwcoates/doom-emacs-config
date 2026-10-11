// Package shellword quotes one word for sh.
//
// THE ONE QUOTER. Every place the daemon writes a word into a command line sh
// will read -- a GIT_SEQUENCE_EDITOR, a fake's environment file, a banner's
// click command -- quotes it here, so a path with spaces or quotes in it
// reaches the program as exactly one argument, and no copy of the idiom can
// drift from the others (TestNoProductionSourceHandRollsTheQuoting).
package shellword

import "strings"

// Quote answers WORD as one sh word: wrapped in single quotes, with each
// single quote inside it closed, escaped and reopened.
func Quote(word string) string {
	return "'" + strings.ReplaceAll(word, "'", `'\''`) + "'"
}
