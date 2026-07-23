// Package dedup derives the store dedup key for an Event per design §6.4.
//
// The store dedups the same fact arriving twice (the shim stream plane and the
// sidecar file plane observe overlapping records). The dedup key is what makes
// the unique (session_id, dedup_key) index collapse those twins to one row,
// first-writer-wins.
//
// Key derivation (§6.4):
//
//   - claude messages with a uuid → "uuid:<uuid>"
//   - tool results             → "tur:<tool_use_id>"
//   - journal records          → "wf:<run>:<key>:<type>"
//   - turn ends                → "turn:<session>:<turn-uuid>"
//
// Two of these four (uuid, tur) are derivable from the payload the store
// already holds: the same claude message uuid and the same tool_use_id appear
// on both the stream twin (ClaudeStreamMessage) and the disk twin
// (TranscriptLine), so the store computes them directly via a type switch over
// the Event payload oneof and the vendor Any. The other two (wf, turn) depend
// on context the payload alone does NOT carry — the workflow run_id lives in
// the journal file PATH (not the JournalRecord), and a turn has no stable id
// on core.TurnEnded. Those keys are therefore supplied by the producer on the
// Event.dedup_key envelope field (the producer has the path/turn context), and
// the store honors a producer-set key verbatim. A producer-set key always wins
// so the store never second-guesses context it cannot see.
//
// An empty result means "no dedup identity" → the event is never deduped
// (Event.dedup_key stays empty, the partial unique index skips it), matching
// the core.proto "Empty = never deduped" contract.
package dedup

import (
	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
)

// Derive returns the dedup key for ev, or "" when ev has no dedup identity.
func Derive(ev *corev1.Event) string {
	if ev == nil {
		return ""
	}
	// A producer-set key is authoritative: it carries run_id / turn-uuid
	// context (wf:… and turn:… keys) that the payload alone cannot supply.
	if k := ev.GetDedupKey(); k != "" {
		return k
	}
	// Otherwise derive uuid:/tur: keys from the payload the store holds.
	if v := ev.GetVendor(); v != nil {
		msg, err := v.UnmarshalNew()
		if err != nil {
			// Unresolvable vendor type: opaque to the store, not deduped.
			return ""
		}
		switch m := msg.(type) {
		case *datav1.ClaudeStreamMessage:
			return streamKey(m)
		case *datav1.TranscriptLine:
			return transcriptKey(m)
		}
	}
	return ""
}

// streamKey derives the key for a stream-plane claude message. A tool result
// (a user message carrying a ToolResultBlock) keys on the tool_use_id so it
// reconciles with the disk twin; every other message keys on its uuid.
func streamKey(m *datav1.ClaudeStreamMessage) string {
	if u := m.GetUser(); u != nil {
		if id := toolUseID(u.GetMessage()); id != "" {
			return "tur:" + id
		}
		return uuidKey(u.GetUuid())
	}
	if a := m.GetAssistant(); a != nil {
		return uuidKey(a.GetUuid())
	}
	if r := m.GetResult(); r != nil {
		return uuidKey(r.GetUuid())
	}
	return ""
}

// transcriptKey derives the key for a file-plane transcript line, symmetric
// with streamKey so the twins collide. A tool_result user line exposes its
// tool_use_id both on the envelope (source_tool_use_id) and in the content
// block; the content block is preferred so it matches the stream twin exactly,
// with the envelope as a fallback.
func transcriptKey(m *datav1.TranscriptLine) string {
	if u := m.GetUser(); u != nil {
		if id := toolUseID(u.GetMessage()); id != "" {
			return "tur:" + id
		}
		if id := u.GetEnvelope().GetSourceToolUseId(); id != "" {
			return "tur:" + id
		}
		return uuidKey(u.GetEnvelope().GetUuid())
	}
	if a := m.GetAssistant(); a != nil {
		return uuidKey(a.GetEnvelope().GetUuid())
	}
	if s := m.GetSystem(); s != nil {
		return uuidKey(s.GetEnvelope().GetUuid())
	}
	if at := m.GetAttachment(); at != nil {
		return uuidKey(at.GetEnvelope().GetUuid())
	}
	return ""
}

// toolUseID scans a user message's content blocks for a tool_result block and
// returns its tool_use_id, or "" if the message carries no tool result.
func toolUseID(m *datav1.ApiUserMessage) string {
	blocks := m.GetContentBlocks()
	if blocks == nil {
		return ""
	}
	for _, b := range blocks.GetBlocks() {
		if tr := b.GetToolResult(); tr != nil {
			if id := tr.GetToolUseId(); id != "" {
				return id
			}
		}
	}
	return ""
}

func uuidKey(u string) string {
	if u == "" {
		return ""
	}
	return "uuid:" + u
}
