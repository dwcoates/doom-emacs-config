package frontend

import (
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"google.golang.org/protobuf/proto"
)

// The curation contract for the two first-class context cuts, at the seams the
// existing unit coverage leaves open: the FAILED compaction (whose `error` is
// the only field an account of a failure has), and the NAMESPACING that keeps a
// clear's identity and a compaction's identity from ever colliding.

// curatedClearCompactMs is this file's own envelope timestamp, kept local so
// these cases do not read a constant another file owns.
const curatedClearCompactMs int64 = 1_700_000_555_000

func TestFailedCompactionCarriesEveryFieldIncludingItsError(t *testing.T) {
	// Arrange — a compaction that FAILED. `error` is populated only in this
	// case, so it is the one field no successful compaction can prove survives,
	// and it is the entire account of what went wrong. Arm 33 carries the
	// core.v1 message VERBATIM, so every field must come through untouched.
	compacted := &corev1.ContextCompacted{
		Trigger:    corev1.ContextCompactTrigger_CONTEXT_COMPACT_TRIGGER_MANUAL,
		PreTokens:  164000,
		PostTokens: 164000,
		DurationMs: 900,
		Summary:    "",
		Result:     "failed",
		Error:      "summarization request was refused",
	}
	ev := &corev1.Event{
		SessionId: "s1", Seq: 77, ProducedAtMs: curatedClearCompactMs,
		DedupKey: "compact:b-fail",
		Payload:  &corev1.Event_ContextCompacted{ContextCompacted: compacted},
	}

	// Act.
	got, err := ConversationDeltaFromEvent("ws", ev)
	if err != nil {
		t.Fatalf("ConversationDeltaFromEvent: %v", err)
	}

	// Assert.
	if got == nil || len(got.GetItems()) != 1 {
		t.Fatalf("delta = %v, want exactly one item", got)
	}
	arm, ok := got.GetItems()[0].GetItem().(*frontendv1.ConversationItem_ContextCompacted)
	if !ok {
		t.Fatalf("item arm = %T, want ConversationItem_ContextCompacted", got.GetItems()[0].GetItem())
	}
	if !proto.Equal(arm.ContextCompacted, compacted) {
		t.Fatalf("payload = %v, want the core.v1 message verbatim (including error=%q)",
			arm.ContextCompacted, compacted.GetError())
	}
}

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
