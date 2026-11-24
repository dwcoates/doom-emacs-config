// Package recency orders names by the instant each was last seen, the order
// the composer's completions are offered in: the composer mentions
// (internal/composermentions) and the composer selections
// (internal/composerselections) both state their records through it, so the
// two records order and break ties identically.
package recency

import (
	"cmp"
	"slices"
)

// NewestFirst answers NAMES's keys, the latest instant first, a tie by name.
func NewestFirst(names map[string]int64) []string {
	out := make([]string, 0, len(names))
	for name := range names {
		out = append(out, name)
	}
	slices.SortFunc(out, func(a, b string) int {
		return cmp.Or(cmp.Compare(names[b], names[a]), cmp.Compare(a, b))
	})
	return out
}
