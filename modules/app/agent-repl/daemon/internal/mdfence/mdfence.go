// Package mdfence reads a line as a markdown fence delimiter by CommonMark's
// length rule, the one rule every daemon reader of fences shares with the
// tree grammar's scanner (tree-sitter-tldr/tldr/src/scanner.c,
// is_fence_close): a fence opens on a run of at least Min backticks or
// tildes, and closes only on a bare run of the same character at least as
// long as the one that opened it. Any other line inside a fence, a shorter
// run or one carrying an info string, is the fence's content.
package mdfence

import "strings"

// Min is the smallest run that opens a fence.
const Min = 3

// Delimiter is how a line reads as a fence delimiter: the character its
// leading run repeats ('`' or '~', 0 for neither), the run's length, and
// whether nothing but whitespace follows the run.
type Delimiter struct {
	Char byte
	Run  int
	Bare bool
}

// Parse reads LINE, a line without its newline whose indent the caller has
// already taken off, as a delimiter.
func Parse(line string) Delimiter {
	if line == "" || (line[0] != '`' && line[0] != '~') {
		return Delimiter{Bare: strings.TrimSpace(line) == ""}
	}
	run := len(line) - len(strings.TrimLeft(line, line[:1]))
	return Delimiter{Char: line[0], Run: run, Bare: strings.TrimSpace(line[run:]) == ""}
}

// Opens is whether the delimiter's run is long enough to open a fence.
func (d Delimiter) Opens() bool { return d.Char != 0 && d.Run >= Min }

// Closes is whether the delimiter closes the fence OPEN opened: a bare run of
// the same character at least as long.
func (d Delimiter) Closes(open Delimiter) bool {
	return d.Bare && d.Char == open.Char && d.Run >= open.Run && d.Opens()
}
