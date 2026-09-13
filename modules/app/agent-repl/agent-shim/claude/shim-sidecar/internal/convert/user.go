package convert

// user.go — A `user` RECORD IS FOUR DIFFERENT THINGS wearing one type tag: a
// person's prompt, a tool's result the vendor filed under the user, the
// harness's own compaction summary, and the expanded `/clear` envelope.
//
// R15: A FILE-PLANE USER PROMPT IS NEVER A PAGE LINE. AgentPrompt carries a
// TurnId and a PromptOrigin, both DAEMON-MINTED — a file reader holds neither
// and inventing them would put a fabricated turn identity on the wire. The
// shim's AgentPrompt is the one served form; here the prompt is classified as
// vendor_specific so the record is durable and investigable without ever
// regrowing a fake prompt bubble in a history page.

import (
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// userLine converts a `user` record.
func (c *Converter) userLine(record map[string]any, at Attribution) []*storev1.StoreEntry {
	env := readEnvelope(record)
	message := obj(record["message"])
	agent := c.frameAgent(at, env)

	// Tool results are lifted out of the user's content and become the settled
	// state of the units that MADE the calls.
	out := c.toolReturns(record, message, at, env, agent)

	switch {
	case env.isSummary:
		// The harness's own prose standing in for discarded history. It is
		// CONSUMED by the compaction boundary that precedes it, so emitting it
		// here would render the summary twice and attribute the harness's text
		// to the person.
		//
		// CONSUMED HERE TOO WHEN IT WAS NOT THE NEXT LINE. The boundary's own
		// lookahead reaches exactly one record, and the harness does not always
		// write the summary there — an observed transcript put a
		// `system/scheduled_task_fire` between them. A summary that names a
		// boundary this converter drew with the placeholder supersedes that
		// draw on the cut's own key rather than being dropped.
		if attached := c.attachCompactSummary(record, at); attached != nil {
			return append(out, attached)
		}
		c.log.With(at.ctxFor("compact-summary")).
			LogVerbose("compaction summary folded into its boundary")
		return out
	case c.isClearCommand(message):
		return append(out, c.contextCleared(record, at, env, agent))
	case c.skillDocument(record, at, env, agent, &out):
		// The skill's document landed and settled its invocation.
		return out
	case hasToolResults(message):
		// A carrier for tool results, already lifted above. The vendor files
		// results under the user's role; emitting an empty prompt beside them
		// would put words in a person's mouth.
		return out
	case env.isMeta:
		// A harness-injected user record: a system reminder, an attachment
		// carrier. Not something a person said.
		c.log.With(at.ctxFor("withhold")).
			LogVerbose("harness-injected user record withheld as vendor_specific")
		return append(out, VendorSpecificEntry(at, "user/meta", record))
	default:
		c.noteKeepalive(message, at)
		c.log.With(at.ctxFor("user-prompt")).
			LogVerbose("file-plane user prompt withheld as vendor_specific (R15: TurnId and PromptOrigin are daemon-minted)")
		return append(out, VendorSpecificEntry(at, "user_prompt", record))
	}
}

// hasToolResults reports whether a user message carries any tool result at all,
// which is what makes it a carrier rather than something a person said.
func hasToolResults(message map[string]any) bool {
	for _, raw := range list(message["content"]) {
		block := obj(raw)
		if block != nil && resultBlockTypes[str(block["type"])] {
			return true
		}
	}
	return false
}

// ---------------------------------------------------------------------------
// the skill document
// ---------------------------------------------------------------------------

// rememberSkillCall notes a skill invocation so the document that arrives later
// settles it. The join is the vendor's own `sourceToolUseID`, which is DIRECT
// AND STRUCTURAL — never a skill-name map matched against whatever arrives next.
func (c *Converter) rememberSkillCall(call openCall) {
	c.openSkills[call.activityID] = call
}

// skillDocument settles a skill invocation from the isMeta user record that
// carries its body, joined by `sourceToolUseID`.
//
// Returns true when this record WAS a skill document, so the caller does not
// also classify it as a prompt.
func (c *Converter) skillDocument(record map[string]any, at Attribution, env envelope, agent string, out *[]*storev1.StoreEntry) bool {
	if env.sourceTool == "" {
		return false
	}
	call, ok := c.openSkills[env.sourceTool]
	if !ok {
		return false
	}
	delete(c.openSkills, env.sourceTool)

	markdown := firstText(obj(record["message"]))
	c.log.With(at.ctxFor("skill-document")).With(logging.Context{ActivityID: call.activityID, UpsertKey: ActivityKey(call.activityID)}).
		LogVerbose("skill document (%d characters) settles its invocation", len(markdown))

	activity := c.skillSettled(call, markdown, env.timestampMs)
	activity.ActivityId = activityID(call.activityID)
	*out = append(*out, c.settledEntry(at, agent, call.activityID, activity))
	return true
}
