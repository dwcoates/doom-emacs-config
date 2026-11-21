package prwatch

import "agent-repld/internal/wsm"

// grouped is ITEMS grouped under KEY, each as VALUE spells it, with the keys
// in the order each first appears: how a tick reads every subject once for
// all its subscriptions, in a stable order.
func grouped[K comparable, T, V any](items []T, key func(T) K, value func(T) V) (map[K][]V, []K) {
	out := map[K][]V{}
	var order []K
	for _, item := range items {
		k := key(item)
		if _, seen := out[k]; !seen {
			order = append(order, k)
		}
		out[k] = append(out[k], value(item))
	}
	return out, order
}

// itself is the identity, for grouping items as they are.
func itself[T any](item T) T { return item }

// failingOf is each of SUBS' failing stretch (nil while its reads succeed),
// as FAILING reads it: what rateLimitWaitOf judges.
func failingOf[T any](subs []T, failing func(T) *wsm.PullRequestFailure) []*wsm.PullRequestFailure {
	out := make([]*wsm.PullRequestFailure, len(subs))
	for i, sub := range subs {
		out[i] = failing(sub)
	}
	return out
}
