package main

import (
	"context"
	"fmt"
	"sync"

	"google.golang.org/protobuf/proto"
)

// Recorder holds every request the fake received, per verb, in arrival order.
// `expect` pops the oldest unread request for a verb and blocks — bounded by
// the caller's context, never by a sleep — until one arrives.
type Recorder struct {
	mu       sync.Mutex
	received map[string][]proto.Message
	read     map[string]int
	waiters  map[string][]chan struct{}
}

// NewRecorder builds an empty recorder.
func NewRecorder() *Recorder {
	return &Recorder{
		received: map[string][]proto.Message{},
		read:     map[string]int{},
		waiters:  map[string][]chan struct{}{},
	}
}

// Record files one received request and wakes everything waiting on the verb.
func (r *Recorder) Record(rpc string, msg proto.Message) {
	r.mu.Lock()
	r.received[rpc] = append(r.received[rpc], proto.Clone(msg))
	waiters := r.waiters[rpc]
	r.waiters[rpc] = nil
	r.mu.Unlock()
	for _, w := range waiters {
		close(w)
	}
}

// Count reports how many requests the verb has received in total, read or not.
func (r *Recorder) Count(rpc string) int {
	r.mu.Lock()
	defer r.mu.Unlock()
	return len(r.received[rpc])
}

// Expect pops the oldest unread request for the verb, waiting for one to
// arrive if none has. The context bounds the wait; its cancellation is
// returned as an error and never swallowed.
func (r *Recorder) Expect(ctx context.Context, rpc string) (proto.Message, error) {
	for {
		r.mu.Lock()
		if n := r.read[rpc]; n < len(r.received[rpc]) {
			msg := r.received[rpc][n]
			r.read[rpc] = n + 1
			r.mu.Unlock()
			return msg, nil
		}
		wait := make(chan struct{})
		r.waiters[rpc] = append(r.waiters[rpc], wait)
		r.mu.Unlock()

		select {
		case <-wait:
		case <-ctx.Done():
			return nil, fmt.Errorf("fakeshim: waiting for %s: %w", rpc, ctx.Err())
		}
	}
}
