package convert

// place.go — WHERE EACH ENTRY SITS IN ITS CONVERSATION (conversation.v1
// ConversationPlace), stated from the record's own bytes.
//
// Every entry a transcript record converts to carries StoreEntry.place, set
// ONCE PER RECORD at the converter's one door (Line), exactly as the turn is:
//
//   - at_ms is the timestamp of the record that OPENED the entry's unit. For a
//     unit that states its start (a tool call's started_at, restated on its
//     settle), that is the start instant — so a unit first written at its
//     RESULT (a deferred spawn announcement) is still placed at its call. For
//     anything else it is the record's own timestamp.
//   - ordinal is the entry's index among the entries this record produced, in
//     the order the conversion produced them (a response's blocks in block
//     order).
//
// BOTH ARE READ FROM THE BYTES, so a re-read mints the identical place, and no
// clock is read here. A record with no parsable timestamp whose entry states
// no start either leaves the place UNSET — the producer's honest "not known" —
// and the store orders that row by its receipt instant.
//
// The store keeps a row's FIRST stated place, so the settle a result record
// writes for a unit its call already placed moves nothing.

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"google.golang.org/protobuf/reflect/protoreflect"
)

// stampPlace states the conversation place of every entry one record produced.
func stampPlace(entries []*storev1.StoreEntry, recordAtMs int64) {
	for i, entry := range entries {
		atMs := openingInstant(entry)
		if atMs == 0 {
			atMs = recordAtMs
		}
		if atMs <= 0 {
			continue
		}
		entry.Place = &conversationv1.ConversationPlace{AtMs: atMs, Ordinal: uint32(i)}
	}
}

// openingInstant is the start instant an entry's frame states for its unit, or
// 0 when it states none.
//
// IT READS EVERY AgentActivityStartedAt IN THE ENTRY AND TAKES THE EARLIEST.
// A start arm carries its started_at and a settle restates it
// (AgentActivitySettledAt.started_at); the arms differ per activity kind, and
// reading them by type rather than by arm is what keeps a new activity kind
// from being placed at its result by omission. They all name the one call that
// opened the unit, and the minimum is the same answer whichever order they are
// visited in.
func openingInstant(entry *storev1.StoreEntry) int64 {
	var earliest int64
	var walk func(m protoreflect.Message)
	walk = func(m protoreflect.Message) {
		// A residue arm's verbatim record is a Struct of vendor JSON, which
		// holds no instant of ours and can be large; it is never walked.
		if m.Descriptor().FullName() == "google.protobuf.Struct" {
			return
		}
		if started, ok := m.Interface().(*conversationv1.AgentActivityStartedAt); ok {
			if at := started.GetAtMs(); at > 0 && (earliest == 0 || at < earliest) {
				earliest = at
			}
			return
		}
		m.Range(func(fd protoreflect.FieldDescriptor, v protoreflect.Value) bool {
			switch {
			case fd.IsList() && fd.Kind() == protoreflect.MessageKind:
				list := v.List()
				for i := 0; i < list.Len(); i++ {
					walk(list.Get(i).Message())
				}
			case fd.IsMap() && fd.MapValue().Kind() == protoreflect.MessageKind:
				v.Map().Range(func(_ protoreflect.MapKey, mv protoreflect.Value) bool {
					walk(mv.Message())
					return true
				})
			case !fd.IsList() && !fd.IsMap() && fd.Kind() == protoreflect.MessageKind:
				walk(v.Message())
			}
			return true
		})
	}
	walk(entry.GetAgentUpdate().ProtoReflect())
	return earliest
}
