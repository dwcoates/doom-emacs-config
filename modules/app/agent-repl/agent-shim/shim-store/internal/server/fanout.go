package server

import (
	"sync"

	corev1 "agentrepl/proto/agentshim/core/v1"
)

// defaultSubBuffer bounds per-subscriber server-side buffering before a slow
// consumer is hard-disconnected (§6.5). A disconnected subscriber reconnects
// and replays from its last seq, so buffering is bounded with no data loss by
// construction.
const defaultSubBuffer = 1024

// subscriber is one live-tail consumer registered with a fanout. Delivery is a
// buffered channel; done is closed exactly once when the subscriber is dropped
// (explicit unsubscribe or slow-consumer disconnect).
type subscriber struct {
	id        uint64
	sessionID string
	ch        chan *corev1.Event
	done      chan struct{}
	closeOnce sync.Once
}

func (s *subscriber) close() { s.closeOnce.Do(func() { close(s.done) }) }

// fanout is the live-tail subscriber registry (§6.5). It broadcasts every
// published event to the registered subscribers of that event's session in
// arrival order, and disconnects any subscriber whose bounded buffer overflows.
// It is class-agnostic: EPHEMERAL events published here pass through to live
// subscribers without ever being persisted (the DB never sees them).
type fanout struct {
	mu     sync.Mutex
	nextID uint64
	subs   map[string]map[uint64]*subscriber
	buffer int
}

func newFanout(buffer int) *fanout {
	if buffer <= 0 {
		buffer = defaultSubBuffer
	}
	return &fanout{
		subs:   make(map[string]map[uint64]*subscriber),
		buffer: buffer,
	}
}

// subscribe registers a new live-tail subscriber for sessionID.
func (f *fanout) subscribe(sessionID string) *subscriber {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.nextID++
	s := &subscriber{
		id:        f.nextID,
		sessionID: sessionID,
		ch:        make(chan *corev1.Event, f.buffer),
		done:      make(chan struct{}),
	}
	m := f.subs[sessionID]
	if m == nil {
		m = make(map[uint64]*subscriber)
		f.subs[sessionID] = m
	}
	m[s.id] = s
	return s
}

// unsubscribe removes a subscriber and closes its done channel.
func (f *fanout) unsubscribe(s *subscriber) {
	f.mu.Lock()
	if m := f.subs[s.sessionID]; m != nil {
		if _, ok := m[s.id]; ok {
			delete(m, s.id)
			if len(m) == 0 {
				delete(f.subs, s.sessionID)
			}
		}
	}
	f.mu.Unlock()
	s.close()
}

// publish broadcasts ev to every subscriber of its session in arrival order.
// A subscriber whose buffer is full is disconnected rather than blocking the
// publisher; the workspace-aware requester reconnects and replays.
func (f *fanout) publish(ev *corev1.Event) {
	sid := ev.GetSessionId()

	f.mu.Lock()
	var slow []*subscriber
	for _, s := range f.subs[sid] {
		select {
		case s.ch <- ev:
		default:
			slow = append(slow, s)
		}
	}
	f.mu.Unlock()

	for _, s := range slow {
		f.unsubscribe(s)
	}
}

// subscriberCount reports the number of live subscribers for a session
// (test/introspection helper).
func (f *fanout) subscriberCount(sessionID string) int {
	f.mu.Lock()
	defer f.mu.Unlock()
	return len(f.subs[sessionID])
}
