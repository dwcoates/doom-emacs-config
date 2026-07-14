package session

import (
	"crypto/rand"
	"encoding/hex"
	"encoding/json"
	"fmt"

	"claude-repld/internal/protocol"
)

// queueItem is one user-message parked on the per-session FIFO queue
// (§2.13) because a turn was already in flight when it was submitted. It
// keeps the decoded command (for the user-turn broadcast at drain) and
// the original raw bytes (forwarded verbatim to the shim at drain), plus
// the classifier verdict once known.
type queueItem struct {
	queueID string
	// cmd is the decoded user-message: OnUserMessageCmd consumes it at
	// drain to broadcast the item's user-turn and set turnActive.
	cmd *protocol.L1Command
	// raw is the original command line, forwarded byte-for-byte to the
	// shim at drain so the SDK sees the exact turn it would have.
	raw []byte
	// content is the normalized ContentBlock[] for frames and snapshots
	// (matches the user-turn the item becomes).
	content json.RawMessage
	// status tracks the item's lifecycle for the snapshot: "classifying"
	// on add, "waiting" on a wait verdict, "interrupt" on an escalation.
	status string
	// verdict / reason are the classifier decision once applied; empty
	// while still classifying.
	verdict string
	reason  string
}

// msgQueue is a per-session ordered FIFO of parked user messages. It is
// not goroutine-safe; the session hub serializes access under s.mu.
type msgQueue struct {
	items []*queueItem
}

// add appends item to the back of the queue.
func (q *msgQueue) add(item *queueItem) {
	q.items = append(q.items, item)
}

// get returns the item with queueID, or nil when the queue no longer
// holds it (already drained or cancelled).
func (q *msgQueue) get(queueID string) *queueItem {
	for _, item := range q.items {
		if item.queueID == queueID {
			return item
		}
	}
	return nil
}

// remove deletes the item with queueID and returns it, or nil when it is
// no longer present.
func (q *msgQueue) remove(queueID string) *queueItem {
	for i, item := range q.items {
		if item.queueID == queueID {
			q.items = append(q.items[:i], q.items[i+1:]...)
			return item
		}
	}
	return nil
}

// popFront removes and returns the front item, or nil when empty.
func (q *msgQueue) popFront() *queueItem {
	if len(q.items) == 0 {
		return nil
	}
	item := q.items[0]
	q.items = q.items[1:]
	return item
}

// moveFront promotes queueID to the front, preserving the relative order
// of the rest. A no-op when the item is absent or already at the front.
func (q *msgQueue) moveFront(queueID string) {
	for i, item := range q.items {
		if item.queueID == queueID {
			if i == 0 {
				return
			}
			q.items = append(q.items[:i], q.items[i+1:]...)
			q.items = append([]*queueItem{item}, q.items...)
			return
		}
	}
}

// drainAll returns every item in front-to-back order and empties the
// queue (session-end teardown).
func (q *msgQueue) drainAll() []*queueItem {
	items := q.items
	q.items = nil
	return items
}

// empty reports whether the queue holds no items.
func (q *msgQueue) empty() bool { return len(q.items) == 0 }

// snapshot renders the queue as protocol.QueuedItem values in
// front-to-back order for the hello frame and the GET /sessions listing.
func (q *msgQueue) snapshot() []protocol.QueuedItem {
	if len(q.items) == 0 {
		return nil
	}
	out := make([]protocol.QueuedItem, 0, len(q.items))
	for _, item := range q.items {
		out = append(out, protocol.QueuedItem{
			QueueID:   item.queueID,
			RequestID: item.cmd.RequestID,
			Content:   item.content,
			Status:    item.status,
			Verdict:   item.verdict,
			Reason:    item.reason,
		})
	}
	return out
}

// newQueueID mints a stable id for a queued item, in the crypto-random
// hex style of newRequestID.
func newQueueID() string {
	var b [16]byte
	if _, err := rand.Read(b[:]); err != nil {
		panic(fmt.Sprintf("session: crypto/rand failed: %v", err))
	}
	return "q_" + hex.EncodeToString(b[:])
}
