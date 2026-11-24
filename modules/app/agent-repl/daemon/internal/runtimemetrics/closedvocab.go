package runtimemetrics

import "fmt"

// closedVocab is one metric's children resolved once for every value of a
// CLOSED label vocabulary: the prompt queue's origins, the visibility
// reasons, the page load and unload reasons. Every value is served from 0
// before its first record, so increase_pure reads the first one, and a
// record is one atomic update on a map nothing writes after construction:
// no lock, no I/O. A value the vocabulary does not hold is a broken
// invariant of its producer and panics, naming the metric.
type closedVocab[K comparable, M any] struct {
	metric   string
	children map[K]M
}

// newClosedVocab resolves CHILD for every value of VALUES of METRIC.
func newClosedVocab[K comparable, M any](metric string, values []K, child func(K) M) closedVocab[K, M] {
	children := make(map[K]M, len(values))
	for _, v := range values {
		children[v] = child(v)
	}
	return closedVocab[K, M]{metric: metric, children: children}
}

// of answers VALUE's child, panicking when the vocabulary does not hold it.
func (c closedVocab[K, M]) of(value K) M {
	child, ok := c.children[value]
	if !ok {
		panic(fmt.Sprintf("runtimemetrics: %s was recorded under %v, which its vocabulary does not hold", c.metric, value))
	}
	return child
}
