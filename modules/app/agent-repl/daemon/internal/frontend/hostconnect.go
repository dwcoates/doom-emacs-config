package frontend

import "sync"

// hostConnectSignal is the transport's one-shot announcement that a HOST
// connection's connect snapshot has been handed to its outbound queue.
//
// IT EXISTS BECAUSE DEFERRED BOOT WORK USED TO STARVE THE HOST'S ACCEPT. The
// daemon's boot moves expensive repairs (the merge-geometry backfill, ~145
// git/stat subprocesses at fleet scale) off the serial boot path — but "off the
// serial path" put them in a footrace with the reconnecting Emacs host, which
// they won, delaying the accept the whole recovery budget is spent on by
// ~1.5s. The backfill now WAITS on this signal instead of racing it.
//
// It is a signal, not a lock: the transport never blocks on anyone reading it,
// and a daemon nobody ever connects to still fires nothing here — the waiter,
// not the transport, owns what to do about that (see bootbackfillgate.go).
type hostConnectSignal struct {
	once sync.Once
	ch   chan struct{}
}

func newHostConnectSignal() *hostConnectSignal {
	return &hostConnectSignal{ch: make(chan struct{})}
}

// fire closes the channel exactly once. Safe to call on every host connect.
func (h *hostConnectSignal) fire() { h.once.Do(func() { close(h.ch) }) }

// wait returns the channel, closed once a host connect snapshot has been
// served.
func (h *hostConnectSignal) wait() <-chan struct{} { return h.ch }

// HostConnectSnapshotServed is closed the first time a ClientKindHost
// connection's connect snapshot (and its catch-up, roster, and registration —
// the whole delivery-lock operation) has been enqueued. It never closes for a
// connection that failed before registration, because such a connection was
// never served.
func (s *Server) HostConnectSnapshotServed() <-chan struct{} { return s.hostConnect.wait() }
