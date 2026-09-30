package db

import "strings"

// inlist.go — the one spelling of an unscoped `IN (…)` lookup's placeholder
// list. A statement that answers a caller-supplied set of ids keeps a single
// `%s` where the list goes, at package scope so the suite EXPLAINs the
// production text itself, and this fills it with one `?` per id.

// expandInList replaces the statement's one `%s` with n comma-separated `?`
// placeholders. n is at least one: every caller refuses an empty id list
// before any statement is built, so zero is a caller defect and panics.
func expandInList(query string, n int) string {
	if n < 1 {
		panic("db: an IN list needs at least one placeholder; the caller must refuse an empty id list first")
	}
	return strings.Replace(query, "%s", strings.TrimSuffix(strings.Repeat("?,", n), ","), 1)
}
