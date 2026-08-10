package statedb

import "strings"

// Placeholders renders n comma-separated SQL bind placeholders, for the IN
// lists whose width is decided at runtime.
//
// IT EXISTS SO THE COUNT IS NEVER HAND-WRITTEN. A literal `(?,?,?)` beside a
// slice of arguments is two statements of one number, and they disagree
// silently the day the slice grows — the query then binds the wrong argument to
// the wrong column, or fails at execution with an argument-count error, both a
// long way from the line that caused it. Deriving the list from the arguments
// themselves makes that unrepresentable.
//
// A ZERO WIDTH RENDERS EMPTY, which is a syntactically invalid `IN ()` on
// purpose: a caller with nothing to match is asking a question with no answer,
// and every caller here guards that case before it builds the clause rather
// than being handed a silently-matching-nothing query.
func Placeholders(n int) string {
	return strings.TrimSuffix(strings.Repeat("?,", n), ",")
}
