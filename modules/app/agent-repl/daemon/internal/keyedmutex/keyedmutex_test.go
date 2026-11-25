package keyedmutex

import (
	"testing"
)

func TestFor(t *testing.T) {
	cases := []struct {
		name  string
		first string
		then  string
		same  bool
	}{
		{name: "the same key answers the same mutex", first: "a", then: "a", same: true},
		{name: "another key answers another mutex", first: "a", then: "b", same: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			var set Set[string]

			// Act.
			first, then := set.For(tc.first), set.For(tc.then)

			// Assert.
			if first == nil || then == nil {
				t.Fatalf("answered %p and %p, want a mutex for each key", first, then)
			}
			if (first == then) != tc.same {
				t.Fatalf("mutexes for %q and %q are the same: %v, want %v", tc.first, tc.then, first == then, tc.same)
			}
		})
	}
}

func TestLockHoldsTheKeysMutexUntilReleased(t *testing.T) {
	// Arrange.
	var set Set[string]

	// Act.
	release := set.Lock("a")

	// Assert.
	if set.For("a").TryLock() {
		t.Fatal("the key's mutex was free while held")
	}
	if !set.For("b").TryLock() {
		t.Fatal("another key's mutex was held by the first key's lock")
	}
	release()
	if !set.For("a").TryLock() {
		t.Fatal("the key's mutex stayed held after its release")
	}
}
