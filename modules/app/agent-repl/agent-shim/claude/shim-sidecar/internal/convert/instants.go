package convert

// instants.go — TIME COMES FROM THE FILE.
//
// Nothing here reads a clock. Every instant a frame carries is the vendor's own
// `timestamp` on the record that stated the fact, which is what makes a re-read
// after a restart produce the identical frame: a producer that re-stamped at
// emit time would make every replayed row differ from the one already stored,
// and a drawn clock would reset every time the sidecar bounced.

import (
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// parseInstant reads the vendor's RFC3339 timestamp into unix millis. A record
// with no parsable timestamp yields 0, which callers treat as "the producer
// observed no instant" and express as an UNSET optional rather than as a zero.
func parseInstant(s string) int64 {
	if s == "" {
		return 0
	}
	t, err := time.Parse(time.RFC3339Nano, s)
	if err != nil {
		return 0
	}
	return t.UnixMilli()
}

// startedAt builds the announcement instant. The field is NON-optional on every
// start arm, so a record with no timestamp still carries the zero the vendor
// effectively gave us — and the log says so at the call site.
func startedAt(ms int64) *conversationv1.AgentActivityStartedAt {
	return &conversationv1.AgentActivityStartedAt{AtMs: ms}
}

// settledAt builds the settle instant, UNSET when the producer observed none.
//
// IT RESTATES THE START IT CLOSES, so the settled frame alone states the call's
// runtime: the start and the settle upsert one unit, and a replay serves the
// settle with no start beside it. `startMs` is REQUIRED, so every settle site
// decides — the call's own start instant (read off the same record the start
// arm read, so the two cannot disagree), or 0 for an arm whose start carries no
// instant (a prose block) or a call this reader never saw announced, which
// leaves the restated start UNSET rather than a zero a consumer would draw.
func settledAt(ms, startMs int64) *conversationv1.AgentActivitySettledAt {
	if ms == 0 {
		return nil
	}
	settled := &conversationv1.AgentActivitySettledAt{AtMs: ms}
	if startMs != 0 {
		settled.StartedAt = startedAt(startMs)
	}
	return settled
}

// ---------------------------------------------------------------------------
// usage accounting
// ---------------------------------------------------------------------------

// readUsage translates the vendor's usage object into the one canonical token
// shape, ORGANIZED BY ECONOMICS rather than by the vendor's field names:
// cache reads are the cheap bucket, and both cache-miss buckets are expensive.
//
// Returns nil when the response carried no usage at all, so absence stays
// absence.
func readUsage(usage map[string]any) *conversationv1.TokenUsage {
	if usage == nil {
		return nil
	}
	return &conversationv1.TokenUsage{
		InputHits: &conversationv1.TokenCacheHits{
			Read: uint64(number(usage["cache_read_input_tokens"])),
		},
		InputMisses: &conversationv1.TokenCacheMisses{
			Written:   uint64(number(usage["cache_creation_input_tokens"])),
			Unwritten: uint64(number(usage["input_tokens"])),
		},
		OutputTokens:         uint64(number(usage["output_tokens"])),
		OutputThinkingTokens: uint64(number(usage["output_thinking_tokens"])),
	}
}

// readEffort maps the vendor's effort literal onto the one canonical effort
// vocabulary. An absent or unrecognized level is UNSET, never "low": a consumer
// that draws effort must draw nothing when the producer reported none.
func readEffort(v any) *conversationv1.AgentEffortLevel {
	level, ok := effortLevels[str(v)]
	if !ok {
		return nil
	}
	return &level
}

var effortLevels = map[string]conversationv1.AgentEffortLevel{
	"low":    conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_LOW,
	"medium": conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_MEDIUM,
	"high":   conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_HIGH,
	"xhigh":  conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_XHIGH,
	"max":    conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_MAX,
	"ultra":  conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_MAX,
}
