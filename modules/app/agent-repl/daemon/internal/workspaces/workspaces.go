// Package workspaces relays the Emacs drawer's sidebar view-model to
// GUI clients (shared/protocol.md, "Workspace sidebar stream"): Emacs
// POSTs each snapshot, the hub keeps the newest one, and every attached
// stream client receives it. The daemon never interprets a snapshot —
// Emacs owns every fact in it and the webapp renders it verbatim — so
// the hub stores raw bytes.
//
// This is deliberately NOT a Layer-2 session stream: there is no seq,
// no history, and no replay. One snapshot always supersedes the
// previous one, so the only state worth keeping — or delivering — is
// the latest.
package workspaces

import "sync"

// Client is one attached sidebar-stream consumer. The server owns the
// socket; the hub owns the outbound snapshot queue.
type Client struct {
	// Send carries whole snapshots. Closed by the hub on detach.
	Send chan []byte
}

// NewClient returns a client with the single-snapshot buffer that
// latest-wins delivery relies on: a deeper buffer could only ever hold
// snapshots a newer one has already superseded.
func NewClient() *Client {
	return &Client{Send: make(chan []byte, 1)}
}

// Hub holds the latest ingested snapshot and fans each new one out to
// attached clients.
type Hub struct {
	mu      sync.Mutex
	latest  []byte
	clients map[*Client]struct{}
}

// NewHub returns an empty hub: no snapshot, no clients.
func NewHub() *Hub {
	return &Hub{clients: map[*Client]struct{}{}}
}

// SetLatest stores data as the latest snapshot and broadcasts it to
// every attached client. Delivery never blocks: a client that has not
// drained its previous snapshot has that stale one dropped in favor of
// data, so a slow client is at most one whole snapshot behind and can
// never stall ingest.
func (h *Hub) SetLatest(data []byte) {
	h.mu.Lock()
	defer h.mu.Unlock()
	h.latest = data
	for c := range h.clients {
		sendLatestLocked(c, data)
	}
}

// Latest returns the most recent snapshot, or nil when none has been
// ingested since daemon boot.
func (h *Hub) Latest() []byte {
	h.mu.Lock()
	defer h.mu.Unlock()
	return h.latest
}

// Attach registers c and primes it with the latest snapshot when one
// exists. Priming happens under the same lock as registration so no
// broadcast can slip between the two: the first snapshot a client reads
// is always current, and everything after is newer.
func (h *Hub) Attach(c *Client) {
	h.mu.Lock()
	defer h.mu.Unlock()
	h.clients[c] = struct{}{}
	if h.latest != nil {
		sendLatestLocked(c, h.latest)
	}
}

// Detach unregisters c and closes its Send channel. Detaching a client
// that is not attached is a no-op, so the server's writer and reader
// goroutines may both call it without coordinating.
func (h *Hub) Detach(c *Client) {
	h.mu.Lock()
	defer h.mu.Unlock()
	if _, ok := h.clients[c]; ok {
		delete(h.clients, c)
		close(c.Send)
	}
}

// sendLatestLocked queues data on c, dropping c's undelivered snapshot
// first when the buffer is full. The hub's lock serializes every sender,
// so the final send cannot block: after the drain the buffer has room
// and nobody else can fill it.
func sendLatestLocked(c *Client, data []byte) {
	select {
	case c.Send <- data:
		return
	default:
	}
	select {
	case <-c.Send: // Drop the stale pending snapshot.
	default: // The client drained it concurrently.
	}
	c.Send <- data
}
