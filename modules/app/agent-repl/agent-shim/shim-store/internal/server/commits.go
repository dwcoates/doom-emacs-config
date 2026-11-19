package server

import "sync"

// commitSignal tells every waiter that a write batch committed, so a reader
// waiting for a record to appear re-reads it then and at no other time.
//
// GAPLESS BY ORDER: a waiter takes the channel BEFORE it reads the record. A
// commit after that read closes the very channel the waiter holds, so no
// commit can fall between the read and the wait.
type commitSignal struct {
	mu sync.Mutex
	ch chan struct{}
}

func newCommitSignal() *commitSignal {
	return &commitSignal{ch: make(chan struct{})}
}

// next is the channel the NEXT commit closes.
func (c *commitSignal) next() <-chan struct{} {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.ch
}

// fire wakes every waiter holding the current channel and arms the next one.
func (c *commitSignal) fire() {
	c.mu.Lock()
	defer c.mu.Unlock()
	close(c.ch)
	c.ch = make(chan struct{})
}
