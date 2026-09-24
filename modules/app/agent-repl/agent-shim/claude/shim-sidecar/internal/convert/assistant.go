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

	// A record this transcript merely QUOTES from a parent is not this agent's
	// to book. A fork copies the parent's whole conversation ahead of its own
	// work, and the copied assistant records keep the parent's `message.id` — so
	// re-booking their blocks under THIS agent hands the store the same
	// `activity:<message id>:<block>` key the producer already used under ANOTHER
	// book, and an upsert may supersede a row's content but never move it between
	// books. The producer already stored it correctly, so nothing new is lost by
	// keeping only a durable residue copy here.
	if c.quotesInheritedContext(at, env) {
		return c.quotedAssistant(record, message, at, env, agent)
	}

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
		//
		// BENIGN, AND THE COMMON SHAPE — debug, not warn. The dominant cause is
		// a response whose only block is a tool call that announces at its
		// RESULT rather than its call (a subagent spawn): the modelled, correct
		// outcome, not a gap. Left at warn it emitted once per such response and
		// flooded a cold re-scan's strict harvest with a false alarm. The record
		// still stands and names the real cause; only its severity drops.
		c.log.With(at.ctxFor("assistant-line")).
			LogVerbose("assistant record produced no units (%s), so this response's usage is not carried", describeNoUnits(reasons))
	}
	return out
}

// quotesInheritedContext reports that this assistant record was produced by
// ANOTHER agent and is only quoted here.
//
// THE VENDOR ITSELF STATES THE PRODUCER. Every sidechain assistant record
// carries `attributionAgent`, the TYPE of the agent that produced it, and a fork
// PRESERVES it across the copy of the parent's conversation it prepends to its
// own transcript while stamping its OWN records with its own type. So a record
// whose `attributionAgent` names a type OTHER than this agent's is inherited
// context, not this agent's work. The guard is narrow on purpose: a session
// transcript has no `AgentType` and its records carry no `attributionAgent`, so
// it never matches, and a non-fork subagent's records all attribute to its own
// type, so it never matches either — only a fork's copied prefix does.
func (c *Converter) quotesInheritedContext(at Attribution, env envelope) bool {
	return at.AgentType != "" && env.attributionAgent != "" && env.attributionAgent != at.AgentType
}

// quotedAssistant handles an assistant record this transcript only QUOTES. It
// books nothing under this agent — the producing agent already booked every unit
// — and keeps the record as residue so the copy is durable and total ingestion
// holds. Residue carries NO book (route.go leaves an unserved item's book NULL)
// and is keyed by the record's own uuid, which is IDENTICAL across every fork
// that quotes it, so all those writes collapse onto one row rather than each
// asking to move it.
//
// THE QUOTED TOOL CALLS ARE STILL REMEMBERED, marked inherited, so the results
// the fork also copied settle as residue in settle.go rather than orphaning at a
// warning or re-booking the settled unit under this agent.
func (c *Converter) quotedAssistant(record, message map[string]any, at Attribution, env envelope, agent string) []*storev1.StoreEntry {
	for _, raw := range list(message["content"]) {
		block := obj(raw)
		if block == nil {
			continue
		}
		switch str(block["type"]) {
		case "tool_use", "server_tool_use", "mcp_tool_use":
			id := str(block["id"])
			if id == "" {
				continue
			}
			input := obj(block["input"])
			if input == nil {
				input = map[string]any{}
			}
			c.rememberCall(id, openCall{
				name:       str(block["name"]),
				input:      input,
				startedAt:  env.timestampMs,
				activityID: id,
				agentID:    agent,
				inherited:  true,
			})
		}
	}
	c.log.With(at.ctxFor("quoted-context")).
		LogVerbose("assistant record attributed to %q is quoted context, not produced by this %q agent; kept as residue, not re-booked", env.attributionAgent, at.AgentType)
	return []*storev1.StoreEntry{VendorSpecificEntry(at, "assistant/quoted_context", record)}
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
		//
		// BENIGN FORWARD-COMPAT — debug, not warn, and for the identical reason
		// 23f26d5e2 gave the LINE-level arm one level up: a vendor adding a
		// block type is expected, not a fault, and the stored unit IS the
		// coverage — it re-converts the day the type is modelled. That commit
		// leveled five sibling arms and missed this one, which left one
		// unmodelled-shape record warning while the rest did not, and a
		// re-scan restating one warn per such block. A block that is not an
		// object at all stays warn above: that is malformed, not merely
		// unmodelled.
		c.log.With(at.ctxFor("assistant-block")).With(logging.Context{ActivityID: BlockActivityID(messageID, index)}).
			LogVerbose("assistant content block type=%q is not modeled; stored as vendor_specific", kind)
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
		// Replay-stable by construction: the settle instant is the transcript
		// record's OWN timestamp, exactly as every sibling terminal stamps it,
		// so a re-compose from this record reproduces the same instant rather
		// than the daemon's compose-time clock.
		//
		// NO START IS RESTATED: a prose block's start arm carries no instant.
		SettledAt: settledAt(env.timestampMs, 0),
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
