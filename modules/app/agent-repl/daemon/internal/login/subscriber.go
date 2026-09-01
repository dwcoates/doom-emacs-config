package login

import (
	"context"
	"sync"
)

// subscriber is one attached viewer's stream.
//
// IT NEVER DROPS A FRAME. The obvious alternative — a bounded channel that
// discards for a slow viewer — silently corrupts the screen it is trying to
// draw, and the daemon does not degrade quietly. Instead every frame is
// queued and a pump goroutine hands them over in order, so a slow viewer costs
// memory (bounded in practice by the scrollback cap and the pty's own read
// rate) rather than correctness, and the reader is never stalled by a viewer.
type subscriber struct {
	mu     sync.Mutex
	queue  []Output
	ended  bool
	notify chan struct{}
	out    chan Output
}

// newSubscriber builds an unstarted subscriber.
func newSubscriber() *subscriber {
	return &subscriber{
		notify: make(chan struct{}, 1),
		out:    make(chan Output),
	}
}

// push queues one frame. Callers hold the session lock, so ordering across
// viewers is the reader's ordering.
func (s *subscriber) push(o Output) {
	s.mu.Lock()
	s.queue = append(s.queue, o)
	s.mu.Unlock()
	s.wake()
}

// end marks the stream complete: everything already queued is still delivered,
// and the channel closes after it.
func (s *subscriber) end() {
	s.mu.Lock()
	s.ended = true
	s.mu.Unlock()
	s.wake()
}

// wake nudges the pump without ever blocking a producer.
func (s *subscriber) wake() {
	select {
	case s.notify <- struct{}{}:
	default:
	}
}

// start runs the pump and answers the viewer's channel. onDone runs once the
// pump stops, so the session can unregister a viewer whose context was
// cancelled.
func (s *subscriber) start(ctx context.Context, onDone func()) <-chan Output {
	go func() {
		defer onDone()
		defer close(s.out)
		for {
			s.mu.Lock()
			batch := s.queue
			s.queue = nil
			ended := s.ended
			s.mu.Unlock()

			for _, o := range batch {
				select {
				case s.out <- o:
				case <-ctx.Done():
					return
				}
			}
			// ended was read under the same lock as the batch, so a true here
			// means the batch just delivered held every frame there will be.
			if ended {
				return
			}
			select {
			case <-s.notify:
			case <-ctx.Done():
				return
			}
		}
	}()
	return s.out
}
