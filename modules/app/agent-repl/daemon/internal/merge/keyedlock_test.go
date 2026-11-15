package merge

import (
	"sync"
	"testing"
)

func TestKeyedLock(t *testing.T) {
	cases := []struct {
		name  string
		first string
		then  string
		same  bool
	}{
		{name: "the same key answers the same lock", first: "a", then: "a", same: true},
		{name: "another key answers another lock", first: "a", then: "b", same: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			locks := map[string]*sync.Mutex{}

			// Act.
			first := keyedLock(h.o, locks, tc.first)
			then := keyedLock(h.o, locks, tc.then)

			// Assert.
			if (first == then) != tc.same {
				t.Fatalf("locks for %q and %q are the same: %v, want %v", tc.first, tc.then, first == then, tc.same)
			}
			if locks[tc.first] != first || locks[tc.then] != then {
				t.Fatalf("the map holds %v, want each answered lock recorded under its key", locks)
			}
		})
	}
}

func TestKeyedLockCallSitesShareTheOrchestratorsMaps(t *testing.T) {
	cases := []struct {
		name   string
		answer func(*harness) *sync.Mutex
		stored func(*harness) *sync.Mutex
	}{
		{
			name:   "the line lock",
			answer: func(h *harness) *sync.Mutex { return h.o.lineLock(h.repoKey()) },
			stored: func(h *harness) *sync.Mutex { return h.o.lines[h.repoKey()] },
		},
		{
			name:   "the hand-off lock",
			answer: func(h *harness) *sync.Mutex { return h.o.handOffLock(theWorkspace) },
			stored: func(h *harness) *sync.Mutex { return h.o.handOffLocks[theWorkspace] },
		},
		{
			name:   "the drop lock",
			answer: func(h *harness) *sync.Mutex { return h.o.dropLock(theWorkspace) },
			stored: func(h *harness) *sync.Mutex { return h.o.dropLocks[theWorkspace] },
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			first, again := tc.answer(h), tc.answer(h)

			// Assert.
			if first == nil || first != again || tc.stored(h) != first {
				t.Fatalf("answered %p then %p, map holds %p; want one lock, kept in the orchestrator's map", first, again, tc.stored(h))
			}
		})
	}
}
