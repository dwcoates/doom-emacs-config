package convert

// assistant.go — ONE API RESPONSE BECOMES SEVERAL UNITS.
//
// An assistant record holds a list of content blocks, and each block is its own
// unit with its own identity: the reasoning, each prose block, and each tool
// call. Collapsing them into one row is the single most damaging thing this
// converter could do — the last block written would be the only one stored.
//
// EXACTLY ONE UNIT PER RESPONSE CARRIES `usage` AND `effort`: the unit for the
// response's FIRST content block. Every other unit of that response leaves both
// UNSET, because a consumer summing units would otherwise over-count the bill by
// the number of blocks.

import (
	"slices"
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// synthesizedNoResponse is the vendor's own placeholder assistant record, which
// no model wrote. It is withheld rather than drawn as something the agent said.
const synthesizedNoResponse = "No response requested."

// assistantLine converts an `assistant` record into one unit per content block.
func (c *Converter) assistantLine(record map[string]any, at Attribution) []*storev1.StoreEntry {
	env := readEnvelope(record)
	message := obj(record["message"])
	agent := c.frameAgent(at, env)

	// The response's identity. A response the vendor did not name falls back to
	// the LINE's own uuid, which is a real identity — it simply cannot fold two
	// lines of one response together.
	messageID := firstNonEmpty(str(message["id"]), env.uuid)

	blocks := list(message["content"])
	if len(blocks) == 0 {
		c.log.With(at.ctxWarn("assistant-line")).
			Log("assistant record carries no content blocks; stored as unknown residue")
		return []*storev1.StoreEntry{UnknownEntry(at, "assistant.empty_content", "message.content", record)}
	}

	usage := readUsage(obj(message["usage"]))
	effort := readEffort(pick(record, "effort"))

	// The block ordinal continues across every LINE of this response and resets
	// only when the response changes.
	if messageID != c.currentMessageID {
		c.currentMessageID = messageID
		c.nextBlockOrdinal = 0
		c.log.With(at.ctxFor("assistant-line")).
			LogVerbose("a new API response opens here; block ordinals restart at 0")
	}

	var out []*storev1.StoreEntry
	// reasons collects, in block order, why each block that produced no unit
	// produced none. It is only ever read when the WHOLE record produced none.
	var reasons []noUnitReason
	for _, raw := range blocks {
		index := c.nextBlockOrdinal
		// EVERY block consumes an ordinal, including a tool_use block that is
		// identified by its own tool_use_id and an exempt one that produces no
		// unit at all — otherwise the ordinals would stop matching the API
		// message's block positions and diverge from the stream plane's.
		c.nextBlockOrdinal++
		block := obj(raw)
		if block == nil {
			c.log.With(at.ctxWarn("assistant-line")).With(logging.Context{ActivityID: BlockActivityID(messageID, index)}).
				Log("assistant content block is not an object; stored as unknown residue")
			out = append(out, UnknownEntry(at, "assistant.block", "message.content[]", record))
			continue
		}
		entries, reason := c.assistantBlock(block, index, messageID, record, at, env, agent)
		if len(entries) == 0 && reason != "" {
			reasons = append(reasons, reason)
		}
		// USAGE RIDES BLOCK 0 OF THE RESPONSE ALONE — the first block of the
		// first line, never "index 0 of each line". It is stamped after the
		// block converted, so a block that produced no unit (an exempt tool
		// call) does not silently take the response's accounting with it.
		if index == 0 {
			stampAccounting(entries, usage, effort)
		}
		out = append(out, entries...)
	}

	if len(out) == 0 {
		// A LEGITIMATE OUTCOME WITH SEVERAL CAUSES, AND THE RECORD MUST NAME
		// THE REAL ONE. This used to assert the exempt set unconditionally,
		// which is a lie on the commonest shape it fires for: a response whose
		// only block is a subagent SPAWN, whose unit is real and simply
		// appears at the call's result. A reader hunting a modelling gap then
		// found a sentence about a decision that was never taken. The
		// accounting still has nowhere to land either way, so the record
		// stands — it just says which.
		c.log.With(at.ctxWarn("assistant-line")).
			Log("assistant record produced no units (%s), so this response's usage is not carried", describeNoUnits(reasons))
	}
	return out
}

// stampAccounting puts the response's usage and effort on the FIRST unit the
// response produced, and on no other.
func stampAccounting(entries []*storev1.StoreEntry, usage *conversationv1.TokenUsage, effort *conversationv1.AgentEffortLevel) {
	for _, e := range entries {
		activity := e.GetAgentUpdate().GetServeableFrame().GetAgentItem().GetAgentFrame().GetUpdate().GetActivity()
		if activity == nil {
			continue
		}
		activity.Usage = usage
		activity.Effort = effort
		return
	}
}

// assistantBlock converts ONE content block into the entries it implies, and —
// when it implies none — WHY.
func (c *Converter) assistantBlock(block map[string]any, index int, messageID string, record map[string]any, at Attribution, env envelope, agent string) ([]*storev1.StoreEntry, noUnitReason) {
	kind := str(block["type"])
	switch kind {
	case "thinking", "redacted_thinking":
		return c.thinkingBlock(block, index, messageID, at, env, agent), ""
	case "text":
		return c.textBlock(block, index, messageID, record, at, env, agent), ""
	case "tool_use", "server_tool_use", "mcp_tool_use":
		return c.toolCallBlock(block, index, messageID, at, env, agent)
	default:
		// A content block kind this schema does not model. It is a real
		// assistant block, so it becomes a unit whose payload is the vendor's
		// own — never a silent drop and never prose the agent did not write.
		c.log.With(at.ctxWarn("assistant-block")).With(logging.Context{ActivityID: BlockActivityID(messageID, index)}).
			Log("assistant content block type=%q is not modeled; stored as vendor_specific", kind)
		return []*storev1.StoreEntry{VendorSpecificEntry(at, "content_block/"+kind, block)}, ""
	}
}

// thinkingBlock converts the agent's reasoning.
//
// A withheld block settles WITHHELD rather than as empty text: the model emitted
// a signature and no text at all, and a consumer draws nothing for it once it
// settles — which is different from drawing an empty card.
func (c *Converter) thinkingBlock(block map[string]any, index int, messageID string, at Attribution, env envelope, agent string) []*storev1.StoreEntry {
	id := BlockActivityID(messageID, index)
	text := str(pick(block, "thinking", "text"))

	var success *conversationv1.AgentThinkingSuccess
	if text == "" {
		c.log.With(at.ctxFor("thinking")).With(logging.Context{ActivityID: id, UpsertKey: ActivityKey(id)}).
			LogVerbose("reasoning block settled withheld (signature without text)")
		success = &conversationv1.AgentThinkingSuccess{
			Reasoning: &conversationv1.AgentThinkingSuccess_Withheld{Withheld: &conversationv1.AgentThinkingWithheld{}},
		}
	} else {
		c.log.With(at.ctxFor("thinking")).With(logging.Context{ActivityID: id, UpsertKey: ActivityKey(id)}).
			LogVerbose("reasoning block settled with %d characters", len(text))
		success = &conversationv1.AgentThinkingSuccess{
			Reasoning: &conversationv1.AgentThinkingSuccess_Text{Text: &conversationv1.AgentThinkingText{Text: text}},
		}
	}

	return []*storev1.StoreEntry{c.activityEntry(at, env, agent, id, index, &conversationv1.AgentActivity{
		ActivityId: activityID(id),
		Item: &conversationv1.AgentActivity_Thinking{Thinking: &conversationv1.AgentThinking{
			Result: &conversationv1.AgentThinking_Success{Success: success},
		}},
	})}
}

// textBlock converts one block of prose the agent addressed to the user.
//
// THE VENDOR SYNTHESIZES ERROR NOTICES AS ASSISTANT PROSE, so authorship is
// stated rather than assumed: "API Error: …" and allowance notices arrive in the
// same shape as an answer, and only a producer that sees the vendor's own
// markers can tell them apart.
func (c *Converter) textBlock(block map[string]any, index int, messageID string, record map[string]any, at Attribution, env envelope, agent string) []*storev1.StoreEntry {
	id := BlockActivityID(messageID, index)
	text := str(block["text"])

	if text == synthesizedNoResponse {
		// The vendor's own placeholder for a turn that produced nothing. No
		// model wrote these words, so it is withheld rather than drawn.
		c.log.With(at.ctxFor("withhold")).
			LogVerbose("synthetic %q assistant record withheld as vendor_specific", synthesizedNoResponse)
		return []*storev1.StoreEntry{VendorSpecificEntry(at, "assistant/no_response_requested", record)}
	}

	success := &conversationv1.AgentResponseSuccess{
		Prose: &conversationv1.AgentResponseProse{Markdown: text},
	}
	if notice := synthesizedNotice(record, text); notice != nil {
		c.log.With(at.ctxFor("response")).With(logging.Context{ActivityID: id, UpsertKey: ActivityKey(id)}).
			Log("assistant prose is a vendor-synthesized notice, not the model's answer")
		success.Authorship = &conversationv1.AgentResponseSuccess_SynthesizedNotice{SynthesizedNotice: notice}
	} else {
		c.log.With(at.ctxFor("response")).With(logging.Context{ActivityID: id, UpsertKey: ActivityKey(id)}).
			LogVerbose("assistant prose settled with %d characters", len(text))
		success.Authorship = &conversationv1.AgentResponseSuccess_FromModel{FromModel: &conversationv1.AgentResponseFromModel{}}
	}

	return []*storev1.StoreEntry{c.activityEntry(at, env, agent, id, index, &conversationv1.AgentActivity{
		ActivityId: activityID(id),
		Item: &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{
			Result: &conversationv1.AgentResponse_Success{Success: success},
		}},
	})}
}

// synthesizedNotice recognizes prose the vendor's own tooling wrote, and what
// the notice is ABOUT so a surface can route it. Returns nil for ordinary model
// prose — absence means "not evaluated as synthesized", never a guarantee.
func synthesizedNotice(record map[string]any, text string) *conversationv1.AgentResponseSynthesizedNotice {
	if !boolean(record["isApiErrorMessage"]) && !hasNoticePrefix(text) {
		return nil
	}
	notice := &conversationv1.AgentResponseSynthesizedNotice{
		Subject: &conversationv1.AgentResponseSynthesizedNotice_Unclassified{Unclassified: &conversationv1.AgentNoticeUnclassified{}},
	}
	switch {
	case containsFold(text, "limit reached"), containsFold(text, "spend limit"), containsFold(text, "usage limit"):
		notice.Subject = &conversationv1.AgentResponseSynthesizedNotice_UsageLimit{UsageLimit: &conversationv1.AgentNoticeUsageLimit{}}
	case containsFold(text, "approaching"):
		notice.Subject = &conversationv1.AgentResponseSynthesizedNotice_UsageWarning{UsageWarning: &conversationv1.AgentNoticeUsageWarning{}}
	case containsFold(text, "switched to"), containsFold(text, "resets at"):
		notice.Subject = &conversationv1.AgentResponseSynthesizedNotice_UsageTransition{UsageTransition: &conversationv1.AgentNoticeUsageTransition{}}
	}
	return notice
}

// noticePrefixes are the vendor's own openings for prose no model wrote.
var noticePrefixes = []string{"API Error", "Claude's response was interrupted", "You've hit your"}

func hasNoticePrefix(text string) bool {
	for _, prefix := range noticePrefixes {
		if len(text) >= len(prefix) && equalFold(text[:len(prefix)], prefix) {
			return true
		}
	}
	return false
}

// describeNoUnits renders, in block order and without repeating itself, why a
// response produced no units at all.
func describeNoUnits(reasons []noUnitReason) string {
	if len(reasons) == 0 {
		// Every block produced nothing for a reason no block owner named. That
		// is a converter gap rather than a decision, and saying so is the
		// point of this record.
		return "for no reason any block could name, which is a modelling gap rather than a decision"
	}
	var distinct []string
	for _, reason := range reasons {
		if !slices.Contains(distinct, string(reason)) {
			distinct = append(distinct, string(reason))
		}
	}
	return "every content block produced none: " + strings.Join(distinct, "; ")
}
