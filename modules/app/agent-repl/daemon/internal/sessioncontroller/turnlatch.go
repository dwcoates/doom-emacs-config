package sessioncontroller

import "sync"

// turnLatch is a set of turn ids recording a ONCE-PER-TURN FACT — a fact that
// is false until something sets it and never becomes false again for that turn.
//
// It exists because more than one authority in this package speaks about the
// same turn from a different goroutine: the stream's own event path, a
// teardown's stop path, an interrupt ack. Each of them needs the same three
// operations over its own set, and hand-rolling the map and its mutex at every
// such site is how two of them drift into disagreeing about whether an empty id
// is a member or whether a test-and-set is atomic.
//
// A latch NEVER forgets. Its sets are per-session and bounded by the turns one
// session runs, and the alternative — expiring a fact — would let a late
// speaker find the turn clean and act as if it were the first.
type turnLatch struct {
	mu  sync.Mutex
	ids map[string]struct{}
}

func newTurnLatch() *turnLatch {
	return &turnLatch{ids: map[string]struct{}{}}
}

// mark records the fact for a turn. An empty turn id names no turn, so marking
// it is a no-op rather than a member under the "" key that a later real turn
// could never match anyway.
func (l *turnLatch) mark(turnID string) {
	if turnID == "" {
		return
	}
	l.mu.Lock()
	defer l.mu.Unlock()
	l.ids[turnID] = struct{}{}
}

// marked reports whether the fact holds for a turn.
func (l *turnLatch) marked(turnID string) bool {
	if turnID == "" {
		return false
	}
	l.mu.Lock()
	defer l.mu.Unlock()
	_, ok := l.ids[turnID]
	return ok
}

// claim records the fact and reports whether THIS caller is the one that
// established it. Exactly one of any number of concurrent claims for a turn
// gets true; the rest get false and must stand down.
func (l *turnLatch) claim(turnID string) bool {
	l.mu.Lock()
	defer l.mu.Unlock()
	if _, ok := l.ids[turnID]; ok {
		return false
	}
	l.ids[turnID] = struct{}{}
	return true
}
