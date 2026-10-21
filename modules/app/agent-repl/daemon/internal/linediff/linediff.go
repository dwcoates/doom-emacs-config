// Package linediff diffs two texts line by line: the lines both share are
// context, the rest removed or added, in the order a unified diff states them.
//
// The match is the LONGEST COMMON SUBSEQUENCE of the two line lists, so a
// replacement that changes one line of twenty draws nineteen context lines and
// one removed/added pair rather than twenty of each. The table is quadratic,
// so a pair of texts whose line counts multiply past MaxCells is diffed
// without a match: every old line removed, then every new line added. That is
// still exactly the change, only drawn without its shared lines.
package linediff

import "agent-repld/internal/textline"

// Kind says what one diffed line is.
type Kind int

const (
	// Context is a line both texts carry.
	Context Kind = iota
	// Removed is a line only the old text carries.
	Removed
	// Added is a line only the new text carries.
	Added
)

// Line is one diffed line: its kind and its text, without any marker.
type Line struct {
	Kind Kind
	Text string
}

// MaxCells bounds the match table (old lines x new lines).
const MaxCells = 4_000_000

// Diff diffs oldText against newText.
func Diff(oldText, newText string) []Line {
	a, b := textline.Lines(oldText), textline.Lines(newText)
	prefix := 0
	for prefix < len(a) && prefix < len(b) && a[prefix] == b[prefix] {
		prefix++
	}
	suffix := 0
	for suffix < len(a)-prefix && suffix < len(b)-prefix && a[len(a)-1-suffix] == b[len(b)-1-suffix] {
		suffix++
	}
	out := make([]Line, 0, len(a)+len(b))
	for _, line := range a[:prefix] {
		out = append(out, Line{Kind: Context, Text: line})
	}
	out = append(out, middle(a[prefix:len(a)-suffix], b[prefix:len(b)-suffix])...)
	for _, line := range a[len(a)-suffix:] {
		out = append(out, Line{Kind: Context, Text: line})
	}
	return out
}

// middle diffs the stretch the shared prefix and suffix leave.
func middle(a, b []string) []Line {
	if len(a)*len(b) > MaxCells || len(a) == 0 || len(b) == 0 {
		return unmatched(a, b)
	}
	// lcs[i][j] is the common subsequence length of a[i:] and b[j:].
	lcs := make([][]int, len(a)+1)
	for i := range lcs {
		lcs[i] = make([]int, len(b)+1)
	}
	for i := len(a) - 1; i >= 0; i-- {
		for j := len(b) - 1; j >= 0; j-- {
			if a[i] == b[j] {
				lcs[i][j] = lcs[i+1][j+1] + 1
			} else {
				lcs[i][j] = max(lcs[i+1][j], lcs[i][j+1])
			}
		}
	}
	out := make([]Line, 0, len(a)+len(b))
	i, j := 0, 0
	for i < len(a) && j < len(b) {
		switch {
		case a[i] == b[j]:
			out = append(out, Line{Kind: Context, Text: a[i]})
			i++
			j++
		case lcs[i+1][j] >= lcs[i][j+1]:
			out = append(out, Line{Kind: Removed, Text: a[i]})
			i++
		default:
			out = append(out, Line{Kind: Added, Text: b[j]})
			j++
		}
	}
	return append(out, unmatched(a[i:], b[j:])...)
}

// unmatched draws every old line removed, then every new line added.
func unmatched(a, b []string) []Line {
	out := make([]Line, 0, len(a)+len(b))
	for _, line := range a {
		out = append(out, Line{Kind: Removed, Text: line})
	}
	for _, line := range b {
		out = append(out, Line{Kind: Added, Text: line})
	}
	return out
}
