// Package lockwatch detects a hot lock held far past any legitimate hold, and
// says so in the log while the daemon is still wedged on it.
//
// On 2026-09-27 a workspace's session-watcher mutex was held for eighteen
// minutes across a watch open that never got its first frame. Every
// SubmitPrompt, feed update and turn-tracking call for that workspace queued
// behind it, and the daemon wrote NOTHING: the only signal was Emacs's 10s
// unary timeout, and the goroutine dump that located the wedge exists only
// because the operator SIGQUIT-killed the daemon. A wedge was invisible, and
// the one way to see it destroyed the process that had it.
//
// Two halves:
//
//   - Mutex, a sync.Mutex that counts its own acquisitions and releases in one
//     atomic word. Taking and releasing it costs one extra atomic store each:
//     no clock read, no allocation, no map, no lock.
//   - Watchdog, ONE daemon-wide goroutine on a coarse ticker that reads every
//     registered Mutex's word, notices a hold that has not changed across
//     ticks for longer than the threshold, and records it once, with a
//     goroutine dump. It is a detector: it never releases, kills or works
//     around anything.
package lockwatch

import (
	"sync"
	"sync/atomic"
)

// Mutex is a sync.Mutex whose holds the Watchdog can see. Its zero value is an
// unlocked mutex, and like sync.Mutex it must not be copied after first use.
//
// holds counts every acquisition and every release, so it is ODD exactly while
// the mutex is held, and a hold that is still the same hold one tick later has
// the same value. That is everything the watchdog needs: how long a hold has
// lasted is measured by the watchdog's own ticks, never by a clock read on the
// hot path.
type Mutex struct {
	mu sync.Mutex
	// holds is written ONLY by the goroutine holding mu, so a plain load and
	// a store are enough: mu's Unlock-to-Lock edge orders every write after
	// the previous holder's. It is atomic because the watchdog reads it
	// without taking mu, which is the point.
	holds atomic.Uint64
}

// Lock locks m.
func (m *Mutex) Lock() {
	m.mu.Lock()
	m.holds.Store(m.holds.Load() + 1)
}

// TryLock tries to lock m and reports whether it succeeded.
func (m *Mutex) TryLock() bool {
	if !m.mu.TryLock() {
		return false
	}
	m.holds.Store(m.holds.Load() + 1)
	return true
}

// Unlock unlocks m. The count moves BEFORE the release, while this goroutine
// still owns the word.
func (m *Mutex) Unlock() {
	m.holds.Store(m.holds.Load() + 1)
	m.mu.Unlock()
}

// hold reports the mutex's current hold: a value that is the same for as long
// as one hold lasts, and whether the mutex is held at all.
func (m *Mutex) hold() (uint64, bool) {
	h := m.holds.Load()
	return h, h&1 == 1
}
