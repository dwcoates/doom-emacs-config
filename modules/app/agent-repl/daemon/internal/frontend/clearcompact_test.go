package frontend

import (
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
	protocolv1 "agentrepl/proto/protocol/v1"

	"google.golang.org/protobuf/proto"
)

// The two first-class events curate to Message arms 32 and 33,
// carrying the core.v1 message VERBATIM. They are the only non-vendor Event
// payloads that curate to an item at all.

func TestContextClearedCuratesIntoItsArm(t *testing.T) {
	// Arrange.
	cleared := &protocolv1.ContextCleared{}
	ev := &protocolv1.Event{
		SessionId: "s1", Seq: 41, ProducedAtMs: producedMs,
		RequestId: "req-7", DedupKey: "clear:u-1",
		Payload: &protocolv1.Event_ContextCleared{ContextCleared: cleared},
	}

	// Act.
	got, _, err := conversationDeltaFromEvent("ws", "", ev)
	if err != nil {
		t.Fatalf("conversationDeltaFromEvent: %v", err)
	}

	// Assert.
	if got == nil || len(got.GetMessages()) != 1 {
		t.Fatalf("delta = %v, want exactly one item", got)
	}
	item := got.GetMessages()[0]
	arm, ok := item.GetPayload().(*frontendv1.Message_ContextCleared)
	if !ok {
		t.Fatalf("item arm = %T, want Message_ContextCleared", item.GetPayload())
	}
	if !proto.Equal(arm.ContextCleared, cleared) {
		t.Fatalf("payload = %v, want the core.v1 message verbatim", arm.ContextCleared)
	}
}

func TestContextCompactedCuratesIntoItsArm(t *testing.T) {
	// Arrange — the coalesced account, every field of which must survive.
	compacted := &protocolv1.ContextCompacted{
		Trigger:    protocolv1.ContextCompactTrigger_CONTEXT_COMPACT_TRIGGER_AUTO,
		PreTokens:  180000,
		PostTokens: 24000,
		DurationMs: 4200,
		Summary:    "we were refactoring the curator",
	}
	ev := &protocolv1.Event{
		SessionId: "s1", Seq: 88, ProducedAtMs: producedMs, DedupKey: "compact:b-1",
		Payload: &protocolv1.Event_ContextCompacted{ContextCompacted: compacted},
	}

	// Act.
	got, _, err := conversationDeltaFromEvent("ws", "", ev)
	if err != nil {
		t.Fatalf("conversationDeltaFromEvent: %v", err)
	}

	// Assert.
	if got == nil || len(got.GetMessages()) != 1 {
		t.Fatalf("delta = %v, want exactly one item", got)
	}
	arm, ok := got.GetMessages()[0].GetPayload().(*frontendv1.Message_ContextCompacted)
	if !ok {
		t.Fatalf("item arm = %T, want Message_ContextCompacted", got.GetMessages()[0].GetPayload())
	}
	if !proto.Equal(arm.ContextCompacted, compacted) {
		t.Fatalf("payload = %v, want the core.v1 message verbatim", arm.ContextCompacted)
	}
}

func TestClearCarriesTheEventEnvelopeOntoItsItem(t *testing.T) {
	// Arrange — ts_ms draws the work and request_id correlates it.
	ev := &protocolv1.Event{
		SessionId: "s1", Seq: 41, ProducedAtMs: producedMs, RequestId: "req-7",
		DedupKey: "clear:u-1",
		Payload:  &protocolv1.Event_ContextCleared{ContextCleared: &protocolv1.ContextCleared{}},
	}

	// Act.
	got, _, _ := conversationDeltaFromEvent("ws", "", ev)

	// Assert.
	item := got.GetMessages()[0]
	if item.GetTsMs() != producedMs || item.GetRequestId() != "req-7" {
		t.Fatalf("envelope ts_ms=%d request_id=%q, want %d/%q",
			item.GetTsMs(), item.GetRequestId(), producedMs, "req-7")
	}
}

func TestClearThroughSeqIsTheEventSeq(t *testing.T) {
	// Arrange — the frontend reconciles on through_seq, and the floor it will
	// be replayed from next time is this very seq.
	ev := &protocolv1.Event{
		SessionId: "s1", Seq: 41, ProducedAtMs: producedMs,
		Payload: &protocolv1.Event_ContextCleared{ContextCleared: &protocolv1.ContextCleared{}},
	}

	// Act.
	got, _, _ := conversationDeltaFromEvent("ws", "", ev)

	// Assert.
	if got.GetThroughSeq() != 41 {
		t.Fatalf("through_seq = %d, want 41", got.GetThroughSeq())
	}
}

func TestClearUUIDIsTheDedupKey(t *testing.T) {
	// Arrange — the dedup key is the only identity stable across replays, so a
	// re-push REPLACES the item rather than accumulating a second work.
	ev := &protocolv1.Event{
		SessionId: "s1", Seq: 41, DedupKey: "clear:u-1",
		Payload: &protocolv1.Event_ContextCleared{ContextCleared: &protocolv1.ContextCleared{}},
	}

	// Act.
	got, _, _ := conversationDeltaFromEvent("ws", "", ev)

	// Assert.
	if uuid := got.GetMessages()[0].GetUuid(); uuid != "clear:u-1" {
		t.Fatalf("uuid = %q, want %q", uuid, "clear:u-1")
	}
}

func TestCompactUUIDIsTheDedupKey(t *testing.T) {
	// Arrange.
	ev := &protocolv1.Event{
		SessionId: "s1", Seq: 88, DedupKey: "compact:b-1",
		Payload: &protocolv1.Event_ContextCompacted{ContextCompacted: &protocolv1.ContextCompacted{}},
	}

	// Act.
	got, _, _ := conversationDeltaFromEvent("ws", "", ev)

	// Assert.
	if uuid := got.GetMessages()[0].GetUuid(); uuid != "compact:b-1" {
		t.Fatalf("uuid = %q, want %q", uuid, "compact:b-1")
	}
}

func TestClearWithoutADedupKeyDerivesAStableUUID(t *testing.T) {
	// Arrange — an event that was never deduped still has to RENDER. Its
	// identity is then its store position, which is just as stable across
	// replays. Dropping it would leave a frontend discarding its history at a
	// floor it can show no reason for.
	ev := &protocolv1.Event{
		SessionId: "s1", Seq: 41,
		Payload: &protocolv1.Event_ContextCleared{ContextCleared: &protocolv1.ContextCleared{}},
	}

	// Act.
	got, _, _ := conversationDeltaFromEvent("ws", "", ev)

	// Assert.
	if uuid := got.GetMessages()[0].GetUuid(); uuid != "clear:s1:41" {
		t.Fatalf("uuid = %q, want %q", uuid, "clear:s1:41")
	}
}

func TestCompactWithoutADedupKeyDerivesACompactPrefixedUUID(t *testing.T) {
	// Arrange — the derived form names WHICH of the two it is, matching the
	// producer's own prefixes so the two id spaces can never overlap.
	ev := &protocolv1.Event{
		SessionId: "s1", Seq: 41,
		Payload: &protocolv1.Event_ContextCompacted{ContextCompacted: &protocolv1.ContextCompacted{}},
	}

	// Act.
	got, _, _ := conversationDeltaFromEvent("ws", "", ev)

	// Assert.
	if uuid := got.GetMessages()[0].GetUuid(); uuid != "compact:s1:41" {
		t.Fatalf("uuid = %q, want %q", uuid, "compact:s1:41")
	}
}

func TestAClearArmWithNoMessageCuratesToNothing(t *testing.T) {
	// Arrange — a set arm carrying a nil message is not a clear that happened.
	ev := &protocolv1.Event{
		SessionId: "s1", Seq: 41,
		Payload: &protocolv1.Event_ContextCleared{ContextCleared: nil},
	}

	// Act.
	got, _, err := conversationDeltaFromEvent("ws", "", ev)

	// Assert.
	if err != nil || got != nil {
		t.Fatalf("delta = %v err = %v, want nil/nil", got, err)
	}
}

func TestACompactArmWithNoMessageCuratesToNothing(t *testing.T) {
	// Arrange.
	ev := &protocolv1.Event{
		SessionId: "s1", Seq: 41,
		Payload: &protocolv1.Event_ContextCompacted{ContextCompacted: nil},
	}

	// Act.
	got, _, err := conversationDeltaFromEvent("ws", "", ev)

	// Assert.
	if err != nil || got != nil {
		t.Fatalf("delta = %v err = %v, want nil/nil", got, err)
	}
}
