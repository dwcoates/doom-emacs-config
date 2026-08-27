package server

import (
	"fmt"
	"sync"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-store/internal/logging"
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
	ch        chan *storev1.WatchAgentSessionResponse
	done      chan struct{}
	closeOnce sync.Once
	onDrop    func(subscriberDropReason)
}

type subscriberDropReason string

const (
	subscriberDropUnsubscribed subscriberDropReason = "unsubscribed"
	subscriberDropSlowConsumer subscriberDropReason = "slow-consumer"
)

// drop reports a candidate terminal cause to the connection-owned terminal
// state machine.  Fanout owns only registry membership and never closes a
// subscriber socket itself.
func (s *subscriber) drop(reason subscriberDropReason) {
	dropped := false
	s.closeOnce.Do(func() {
		close(s.done)
		dropped = true
	})
	if dropped {
		s.onDrop(reason)
	}
}

func (s *subscriber) stop() {
	s.closeOnce.Do(func() { close(s.done) })
}

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
	log    *logging.Logger
}

func newFanout(buffer int, log *logging.Logger) *fanout {
	if log == nil {
		panic("shim-store fanout: nil logger")
	}
	if buffer <= 0 {
		buffer = defaultSubBuffer
	}
	log.Log(logging.Fields{Operation: "fanout-init"}, "live-tail registry initialized buffer=%d", buffer)
	return &fanout{
		subs:   make(map[string]map[uint64]*subscriber),
		buffer: buffer,
		log:    log,
	}
}

// subscribe registers a new live-tail subscriber for sessionID.
func (f *fanout) subscribe(sessionID string, onDrop func(subscriberDropReason), prepare func(*subscriber)) *subscriber {
	if onDrop == nil {
		panic("shim-store fanout: nil subscriber drop owner")
	}
	if prepare == nil {
		panic("shim-store fanout: nil subscriber prepare owner")
	}
	f.mu.Lock()
	defer f.mu.Unlock()
	f.nextID++
	s := &subscriber{
		id:        f.nextID,
		sessionID: sessionID,
		ch:        make(chan *storev1.WatchAgentSessionResponse, f.buffer),
		done:      make(chan struct{}),
		onDrop:    onDrop,
	}
	prepare(s)
	m := f.subs[sessionID]
	if m == nil {
		m = make(map[uint64]*subscriber)
		f.subs[sessionID] = m
	}
	m[s.id] = s
	f.log.LogVerbose(logging.Fields{Operation: "subscribe", Session: sessionID, Subscriber: subscriberName(s.id)}, "live-tail subscriber registered buffer=%d", f.buffer)
	return s
}

// unsubscribe removes a subscriber and closes its done channel.
func (f *fanout) unsubscribe(s *subscriber) {
	removed := f.remove(s)
	s.drop(subscriberDropUnsubscribed)
	if removed {
		f.log.LogVerbose(logging.Fields{Operation: "unsubscribe", Session: s.sessionID, Subscriber: subscriberName(s.id)}, "live-tail subscriber removed")
	}
}

// remove atomically retires s from the registry.  Terminal ownership lives at
// the connection, so registry removal deliberately does not report a cause.
func (f *fanout) remove(s *subscriber) bool {
	f.mu.Lock()
	defer f.mu.Unlock()
	m := f.subs[s.sessionID]
	if _, ok := m[s.id]; !ok {
		return false
	}
	delete(m, s.id)
	if len(m) == 0 {
		delete(f.subs, s.sessionID)
	}
	return true
}

// publish broadcasts one line to every subscriber of a session in arrival
// order. A subscriber whose buffer is full is disconnected rather than blocking
// the publisher; the workspace-aware requester reconnects.
//
// THE ROUTING KEY IS NOW A PARAMETER. It used to be read off the frame:
// `EntryDelivery`'s external half carried the vendor session_id. The store.v1
// replacement frame, `WatchAgentSessionResponse`, carries a `StoreLineAt` and
// names no session at all — a watch is addressed by an opaque, store-minted
// AgentSessionToken instead. Deriving a session from the frame is therefore no
// longer possible, and minting a token-to-session map is a design decision, so
// the caller states the key.
func (f *fanout) publish(sessionID string, line *storev1.WatchAgentSessionResponse) {
	f.mu.Lock()
	var slow []*subscriber
	for _, s := range f.subs[sessionID] {
		select {
		case s.ch <- line:
		default:
			slow = append(slow, s)
		}
	}
	f.mu.Unlock()

	for _, s := range slow {
		f.log.Log(logging.Fields{Operation: "slow-consumer", Session: sessionID, Subscriber: subscriberName(s.id), Level: "warn"}, "live-tail subscriber disconnected after buffer overflow buffer=%d", f.buffer)
		f.remove(s)
		s.drop(subscriberDropSlowConsumer)
	}
}

// subscriberCount reports the number of live subscribers for a session
// (test/introspection helper).
func (f *fanout) subscriberCount(sessionID string) int {
	f.mu.Lock()
	defer f.mu.Unlock()
	return len(f.subs[sessionID])
}

func subscriberName(id uint64) string { return fmt.Sprintf("%d", id) }
