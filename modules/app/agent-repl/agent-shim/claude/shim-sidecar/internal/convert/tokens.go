package convert

// tokens.go — the vendor's three disjoint input counters onto the one canonical
// token shape.
//
// The mapping is fixed by TokenUsage's own contract and is NOT a judgment call:
//
//	cache_read_input_tokens     -> input_hits.read        (the cheap bucket)
//	cache_creation_input_tokens -> input_misses.written   (fresh + write premium)
//	input_tokens                -> input_misses.unwritten (fresh, never cached)
//	output_tokens               -> output_tokens
//
// The nesting is what makes the expensive sum structural: `input_misses` is
// both of its fields together, so no reader has to know to add them.

import conversationv1 "agentrepl/proto/conversation/v1"

// tokenUsage converts the vendor's per-message usage object.
//
// Returns nil when the vendor reported no usage at all, which is different from
// a message that cost nothing: an absent record must not be readable as a zero
// bill.
func tokenUsage(v any) *conversationv1.TokenUsage {
	usage, ok := v.(map[string]any)
	if !ok {
		return nil
	}
	return &conversationv1.TokenUsage{
		InputHits: &conversationv1.TokenCacheHits{
			Read: counter(usage["cache_read_input_tokens"]),
		},
		InputMisses: &conversationv1.TokenCacheMisses{
			Written:   counter(usage["cache_creation_input_tokens"]),
			Unwritten: counter(usage["input_tokens"]),
		},
		OutputTokens: counter(usage["output_tokens"]),
	}
}

// counter reads one token counter. JSON numbers decode as float64; anything
// else (absent, null, a string) is zero, which is what the vendor omitting a
// counter means.
//
// A NEGATIVE COUNT IS CLAMPED TO ZERO rather than wrapped. The fields are
// unsigned, so a negative would become an astronomically large count and read
// as a catastrophic bill.
func counter(v any) uint64 {
	f, ok := v.(float64)
	if !ok || f <= 0 {
		return 0
	}
	return uint64(f)
}

// stopReason converts the vendor's stop reason string onto the closed set a
// client branches on. An unmodeled value is STATED as unsupported rather than
// defaulted to end_turn, which would report a truncated response as a complete
// one.
func stopReason(v any) *conversationv1.StopReason {
	s, _ := v.(string)
	switch s {
	case "":
		// The vendor said nothing, which happens mid-stream and on old lines.
		// There is no arm for "not stated", so the reason is left unset.
		return nil
	case "end_turn", "stop_sequence":
		return &conversationv1.StopReason{Reason: &conversationv1.StopReason_EndTurn{EndTurn: &conversationv1.StopEndTurn{}}}
	case "tool_use":
		return &conversationv1.StopReason{Reason: &conversationv1.StopReason_ToolCall{ToolCall: &conversationv1.StopToolCall{}}}
	case "max_tokens":
		return &conversationv1.StopReason{Reason: &conversationv1.StopReason_MaxTokens{MaxTokens: &conversationv1.StopMaxTokens{}}}
	case "refusal", "pause_turn":
		return &conversationv1.StopReason{Reason: &conversationv1.StopReason_Interrupted{Interrupted: &conversationv1.StopInterrupted{}}}
	default:
		return &conversationv1.StopReason{Reason: &conversationv1.StopReason_Unsupported{
			Unsupported: &conversationv1.StopUnsupported{Reason: s},
		}}
	}
}
