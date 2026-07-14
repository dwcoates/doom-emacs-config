package session

import (
	"encoding/json"
	"testing"

	"claude-repld/internal/protocol"
)

// mkItem builds a queued item with a decoded command carrying id as its
// request id, so snapshots and drains are inspectable.
func mkItem(queueID, reqID, text string) *queueItem {
	cmd := &protocol.L1Command{Type: "user-message", RequestID: reqID, Content: json.RawMessage(`"` + text + `"`)}
	return &queueItem{
		queueID: queueID,
		cmd:     cmd,
		raw:     []byte(`{"type":"user-message","request_id":"` + reqID + `","content":"` + text + `"}`),
		content: normalizeContent(cmd.Content),
		status:  "classifying",
	}
}

func ids(items []*queueItem) []string {
	out := make([]string, 0, len(items))
	for _, it := range items {
		out = append(out, it.queueID)
	}
	return out
}

func TestQueueAddPreservesFIFOOrder(t *testing.T) {
	// Arrange
	var q msgQueue
	// Act
	q.add(mkItem("q1", "r1", "a"))
	q.add(mkItem("q2", "r2", "b"))
	q.add(mkItem("q3", "r3", "c"))
	// Assert
	if got := ids(q.items); !equalStrs(got, []string{"q1", "q2", "q3"}) {
		t.Fatalf("order = %v", got)
	}
}

func TestQueueGetReturnsItemOrNil(t *testing.T) {
	// Arrange
	var q msgQueue
	q.add(mkItem("q1", "r1", "a"))
	// Act + Assert
	if q.get("q1") == nil {
		t.Error("get(q1) = nil, want the item")
	}
	if q.get("ghost") != nil {
		t.Error("get(ghost) should be nil")
	}
}

func TestQueueRemoveReturnsItemAndShrinks(t *testing.T) {
	// Arrange
	var q msgQueue
	q.add(mkItem("q1", "r1", "a"))
	q.add(mkItem("q2", "r2", "b"))
	// Act
	removed := q.remove("q1")
	// Assert
	if removed == nil || removed.queueID != "q1" {
		t.Fatalf("removed = %v", removed)
	}
	if got := ids(q.items); !equalStrs(got, []string{"q2"}) {
		t.Fatalf("after remove = %v", got)
	}
}

func TestQueueRemoveStaleIsNil(t *testing.T) {
	// Arrange
	var q msgQueue
	q.add(mkItem("q1", "r1", "a"))
	// Act + Assert
	if q.remove("ghost") != nil {
		t.Error("remove(ghost) should be nil")
	}
}

func TestQueuePopFrontIsFIFO(t *testing.T) {
	// Arrange
	var q msgQueue
	q.add(mkItem("q1", "r1", "a"))
	q.add(mkItem("q2", "r2", "b"))
	// Act
	front := q.popFront()
	// Assert
	if front.queueID != "q1" {
		t.Errorf("front = %q, want q1", front.queueID)
	}
	if got := ids(q.items); !equalStrs(got, []string{"q2"}) {
		t.Fatalf("after pop = %v", got)
	}
}

func TestQueuePopFrontEmptyIsNil(t *testing.T) {
	// Arrange
	var q msgQueue
	// Act + Assert
	if q.popFront() != nil {
		t.Error("popFront on empty queue should be nil")
	}
}

func TestQueueMoveFrontPromotesPreservingRest(t *testing.T) {
	// Arrange
	var q msgQueue
	q.add(mkItem("q1", "r1", "a"))
	q.add(mkItem("q2", "r2", "b"))
	q.add(mkItem("q3", "r3", "c"))
	// Act — promote the middle item.
	q.moveFront("q2")
	// Assert — q2 leads; q1 and q3 keep their relative order.
	if got := ids(q.items); !equalStrs(got, []string{"q2", "q1", "q3"}) {
		t.Fatalf("after moveFront = %v", got)
	}
}

func TestQueueMoveFrontOfFrontIsNoOp(t *testing.T) {
	// Arrange
	var q msgQueue
	q.add(mkItem("q1", "r1", "a"))
	q.add(mkItem("q2", "r2", "b"))
	// Act
	q.moveFront("q1")
	// Assert
	if got := ids(q.items); !equalStrs(got, []string{"q1", "q2"}) {
		t.Fatalf("order = %v", got)
	}
}

func TestQueueDrainAllEmptiesAndReturnsInOrder(t *testing.T) {
	// Arrange
	var q msgQueue
	q.add(mkItem("q1", "r1", "a"))
	q.add(mkItem("q2", "r2", "b"))
	// Act
	drained := q.drainAll()
	// Assert
	if got := ids(drained); !equalStrs(got, []string{"q1", "q2"}) {
		t.Fatalf("drained = %v", got)
	}
	if !q.empty() {
		t.Error("queue should be empty after drainAll")
	}
}

func TestQueueSnapshotFrontToBackCarriesFields(t *testing.T) {
	// Arrange
	var q msgQueue
	item := mkItem("q1", "r1", "hi")
	item.status = "waiting"
	item.verdict = "wait"
	item.reason = "follow-up"
	q.add(item)
	q.add(mkItem("q2", "r2", "bye"))
	// Act
	snap := q.snapshot()
	// Assert — front-to-back, with verdict/reason surfaced.
	if len(snap) != 2 || snap[0].QueueID != "q1" || snap[1].QueueID != "q2" {
		t.Fatalf("snapshot order = %+v", snap)
	}
	if snap[0].Status != "waiting" || snap[0].Verdict != "wait" || snap[0].Reason != "follow-up" {
		t.Errorf("snapshot[0] = %+v", snap[0])
	}
	if snap[0].RequestID != "r1" {
		t.Errorf("snapshot[0].RequestID = %q", snap[0].RequestID)
	}
}

func TestQueueSnapshotEmptyIsNil(t *testing.T) {
	// Arrange
	var q msgQueue
	// Act + Assert
	if q.snapshot() != nil {
		t.Error("snapshot of an empty queue should be nil (omitempty on the wire)")
	}
}

func TestNewQueueIDIsPrefixedAndUnique(t *testing.T) {
	// Arrange + Act
	a, b := newQueueID(), newQueueID()
	// Assert
	if len(a) < 3 || a[:2] != "q_" {
		t.Errorf("queue id = %q, want q_ prefix", a)
	}
	if a == b {
		t.Error("queue ids should be unique")
	}
}

func equalStrs(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}
