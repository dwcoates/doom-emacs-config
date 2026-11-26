// Package keyedmutex holds one mutex per key, minted on first ask, for work
// that must run one at a time per workspace, per repository, or per any other
// key, while different keys run side by side.
//
// Every per-key lock in the daemon is a Set, so each one mints, keeps and
// answers its locks the same way: a lock is never removed, because dropping
// one while a waiter is behind it would hand the next caller a lock nobody
// else is waiting on, and the two would run at once.
package keyedmutex

import "sync"

// Set is one mutex per key. Its zero value is ready to use, and it must not
// be copied after first use.
type Set[K comparable] struct {
	mu    sync.Mutex
	locks map[K]*sync.Mutex
}

// For answers KEY's mutex, minting it on first ask, so every caller naming
// the same key serializes on the same mutex.
func (s *Set[K]) For(key K) *sync.Mutex {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.locks == nil {
		s.locks = map[K]*sync.Mutex{}
	}
	lock, ok := s.locks[key]
	if !ok {
		lock = &sync.Mutex{}
		s.locks[key] = lock
	}
	return lock
}

// Lock takes KEY's mutex and answers its release.
func (s *Set[K]) Lock(key K) func() {
	lock := s.For(key)
	lock.Lock()
	return lock.Unlock
}
