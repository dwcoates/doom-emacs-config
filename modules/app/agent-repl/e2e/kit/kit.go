// Package kit is the e2e harness's own code that builds no world: what the
// e2e scenarios (agentrepl/e2e) and the scans of their sources
// (agentrepl/e2e/kit/scans) share, kept here so its tests run in a unit the
// test runner can reuse rather than in the e2e suite, which never is.
package kit

import "sort"

// SortedKeys is m's keys in order.
func SortedKeys[V any](m map[string]V) []string {
	out := make([]string, 0, len(m))
	for k := range m {
		out = append(out, k)
	}
	sort.Strings(out)
	return out
}
