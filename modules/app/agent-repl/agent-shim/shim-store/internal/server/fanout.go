package server

import (
	"fmt"
	"sync"

	protocolv1 "agentrepl/proto/protocol/v1"
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
	ch        chan *protocolv1.EntryDelivery
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
		ch:        make(chan *protocolv1.EntryDelivery, f.buffer),
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

// publish broadcasts one delivery to every subscriber of its session in
// arrival order. A subscriber whose buffer is full is disconnected rather than
// blocking the publisher; the workspace-aware requester reconnects and replays.
//
// THE STORE ONLY EVER PUBLISHES A `stored` DELIVERY. The `live` arm exists for
// a record handed straight to the daemon that the store never saw, which by
// definition cannot arrive here — and the write surface has no way to say "fan
// this out without storing it" now that EventClass is retired. deliverySession
// still reads both arms so a routing key is never silently empty.
func (f *fanout) publish(delivery *protocolv1.EntryDelivery) {
	sid := deliverySession(delivery)

	f.mu.Lock()
	var slow []*subscriber
	for _, s := range f.subs[sid] {
		select {
		case s.ch <- delivery:
		default:
			slow = append(slow, s)
		}
	}
	f.mu.Unlock()

	for _, s := range slow {
		f.log.Log(logging.Fields{Operation: "slow-consumer", Session: sid, Subscriber: subscriberName(s.id), Level: "warn"}, "live-tail subscriber disconnected after buffer overflow buffer=%d entry_seq=%d", f.buffer, delivery.GetStored().GetSeq())
		f.remove(s)
		s.drop(subscriberDropSlowConsumer)
	}
}

// deliverySession is the fan-out routing key: the VENDOR session id, which both
// delivery arms carry on the external half they wrap.
func deliverySession(delivery *protocolv1.EntryDelivery) string {
	switch d := delivery.GetDelivery().(type) {
	case *protocolv1.EntryDelivery_Stored:
		return d.Stored.GetEntry().GetSessionId()
	case *protocolv1.EntryDelivery_Live:
		return d.Live.GetEntry().GetSessionId()
	default:
		return ""
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
