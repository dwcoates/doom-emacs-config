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

// idListArgs validates an unscoped id-set lookup's ids and returns them as the
// statement's bound arguments. An empty list is refused naming `field`, and an
// empty id naming `field[i]`, both as ErrInvalid, before any statement runs.
func idListArgs(ids []string, field, noneDetail, emptyDetail string) ([]any, error) {
	if len(ids) == 0 {
		return nil, invalidFieldf(field, "%s", noneDetail)
	}
	args := make([]any, len(ids))
	for i, id := range ids {
		if id == "" {
			return nil, invalidFieldf(field+"["+itoa(i)+"]", "%s", emptyDetail)
		}
		args[i] = id
	}
	return args, nil
}
