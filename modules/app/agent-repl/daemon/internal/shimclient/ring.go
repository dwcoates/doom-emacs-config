package shimclient

import "sync"

// stderrRingBytes is the stderr ring buffer's capacity. A shim that dies
// noisily still yields its last words; a shim that dies chattily cannot grow
// the daemon's heap.
const stderrRingBytes = 64 << 10

// ring is a bounded byte buffer keeping the TAIL of everything written to it.
// It is the shim's stderr, kept as failure evidence for the exit record.
type ring struct {
	mu       sync.Mutex
	buf      []byte
	capacity int
}

// newRing builds a ring of the given capacity.
func newRing(capacity int) *ring {
	if capacity <= 0 {
		capacity = stderrRingBytes
	}
	return &ring{capacity: capacity}
}

// Write implements io.Writer, dropping the OLDEST bytes when the capacity is
// exceeded. It never fails and never short-writes: the caller is a copy loop
// off a pipe, and a write error there would lose the process's stderr.
func (r *ring) Write(p []byte) (int, error) {
	r.mu.Lock()
	defer r.mu.Unlock()

	n := len(p)
	if n >= r.capacity {
		r.buf = append(r.buf[:0], p[n-r.capacity:]...)
		return n, nil
	}
	if len(r.buf)+n > r.capacity {
		drop := len(r.buf) + n - r.capacity
		r.buf = append(r.buf[:0], r.buf[drop:]...)
	}
	r.buf = append(r.buf, p...)
	return n, nil
}

// String is the retained tail.
func (r *ring) String() string {
	r.mu.Lock()
	defer r.mu.Unlock()
	return string(r.buf)
}
