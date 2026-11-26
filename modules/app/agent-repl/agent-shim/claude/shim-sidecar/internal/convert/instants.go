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
	"agentrepl/shim-claude-sidecar/internal/logging"
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
//
// PROVISIONAL STAYS FALSE: the file plane reads the record the vendor itself
// wrote once the response settled, so its usage is the settled one, and it
// supersedes the stream plane's in-flight snapshot of the same unit.
func (c *Converter) readUsage(usage map[string]any, at Attribution, operation string) *conversationv1.TokenUsage {
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
		OutputThinkingTokens: c.thinkingTokens(obj(usage["output_tokens_details"]), at, operation),
	}
}

// thinkingTokens reads the thinking tokens out of the vendor's output-token
// details.
//
// THE VENDOR WRITES thinking_tokens (transcript record 6ed6dcd3 of session
// 43614927 on 2026-11-26 carries output_tokens_details.thinking_tokens: 209,
// and no top-level output_thinking_tokens); reasoning_tokens is the name the
// earlier harvest modeled and is still read. Both present and differing is a
// vendor contradiction: it is warned and thinking_tokens, the vendor's current
// name, is taken. No details at all means no thinking: 0.
func (c *Converter) thinkingTokens(details map[string]any, at Attribution, operation string) uint64 {
	if details == nil {
		return 0
	}
	thinking, hasThinking := details["thinking_tokens"].(float64)
	reasoning, hasReasoning := details["reasoning_tokens"].(float64)
	if hasThinking && hasReasoning && thinking != reasoning {
		c.log.With(at.ctxWarn(operation)).
			Log("the vendor's output-token details name two different thinking counts (thinking_tokens={thinking}, reasoning_tokens={reasoning}); taking thinking_tokens",
				logging.Values{"thinking": thinking, "reasoning": reasoning})
	}
	if hasThinking {
		return uint64(thinking)
	}
	return uint64(reasoning)
}

// readModel reads the model the vendor says answered a response, nil when it
// named none — absence draws no model, never a guessed one.
func readModel(v any) *conversationv1.AgentModel {
	name, ok := v.(string)
	if !ok || name == "" {
		return nil
	}
	return &conversationv1.AgentModel{Name: name}
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
