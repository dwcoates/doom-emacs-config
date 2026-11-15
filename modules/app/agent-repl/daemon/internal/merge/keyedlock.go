package merge

import "sync"

// keyedLock answers the lock LOCKS holds for KEY, minting it on first ask, so
// every caller naming the same key serializes on the same lock. The map is
// the orchestrator's and is read and written under its mutex, which the
// caller must NOT hold.
func keyedLock[K comparable](o *orchestrator, locks map[K]*sync.Mutex, key K) *sync.Mutex {
	o.mu.Lock()
	defer o.mu.Unlock()
	lock, ok := locks[key]
	if !ok {
		lock = &sync.Mutex{}
		locks[key] = lock
	}
	return lock
}
