package frontend

import (
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
)

// The curation contract for the two first-class context cuts, at the seam the
// existing unit coverage leaves open: the NAMESPACING that keeps a clear's
// identity and a compaction's identity from ever colliding. Field-by-field
// carriage of the compaction message itself is covered by
// TestContextCompactedCuratesIntoItsArm in clearcompact_test.go.

func TestAClearAndACompactionAtTheSamePositionGetDistinctIDs(t *testing.T) {
	// Arrange — the namespacing regression. Both cuts derive their identity
	// from the SAME vendor position when no dedup key was stamped (one session,
	// one seq), so a derivation that ignored WHICH cut it was naming would hand
	// both the same id and let a frontend replace one bubble with the other.
	// The compaction's id must also carry no `clear:` prefix: an earlier
	// derivation prefixed every cut with the clear's name.
	position := struct {
		sessionID string
		seq       uint64
	}{sessionID: "s1", seq: 41}
	clear := &corev1.Event{
		SessionId: position.sessionID, Seq: position.seq,
		Payload: &corev1.Event_ContextCleared{ContextCleared: &corev1.ContextCleared{}},
	}
	compact := &corev1.Event{
		SessionId: position.sessionID, Seq: position.seq,
		Payload: &corev1.Event_ContextCompacted{ContextCompacted: &corev1.ContextCompacted{}},
	}

	// Act.
	clearDelta, clearErr := ConversationDeltaFromEvent("ws", clear)
	compactDelta, compactErr := ConversationDeltaFromEvent("ws", compact)
	if clearErr != nil || compactErr != nil {
		t.Fatalf("ConversationDeltaFromEvent: clear=%v compact=%v", clearErr, compactErr)
	}

	// Assert.
	clearUUID := clearDelta.GetItems()[0].GetUuid()
	compactUUID := compactDelta.GetItems()[0].GetUuid()
	if clearUUID == compactUUID {
		t.Fatalf("both cuts derived uuid %q; the two id spaces must never overlap", clearUUID)
	}
	if strings.HasPrefix(compactUUID, "clear:") {
		t.Fatalf("compaction uuid = %q, want no clear: prefix — it names the wrong cut", compactUUID)
	}
}
