package convert

// skill.go — A SKILL IS INSTRUCTIONS LOADED INTO THIS AGENT'S CONTEXT, not
// another agent and not a subprocess. The invocation's own return is a bare
// acknowledgement worth nothing to draw; what is worth carrying is the DOCUMENT,
// which arrives afterwards as a separate injected record linked back to the call.
//
// SO THIS UNIT SETTLES WHEN THE DOCUMENT LANDS, not when the acknowledgement
// returns — and NOTHING DELIMITS A SKILL'S SCOPE, so it contains no nested work:
// there is no skill-ended record anywhere on disk to read.

import conversationv1 "agentrepl/proto/conversation/v1"

// skillSettled builds the settled invocation from the document that landed.
func (c *Converter) skillSettled(call openCall, markdown string, ts int64) *conversationv1.AgentActivity {
	success := &conversationv1.AgentSkillUseSuccess{
		Skill:     &conversationv1.AgentSkillName{Name: skillName(call)},
		Document:  &conversationv1.AgentSkillDocument{Markdown: markdown},
		SettledAt: settledAt(ts),
	}
	if call.retainedAllowedTools != nil {
		success.AllowedTools = call.retainedAllowedTools
	}
	return item(&conversationv1.AgentActivity_SkillUse{SkillUse: &conversationv1.AgentSkillUse{
		Result: &conversationv1.AgentSkillUse_Success{Success: success},
	}})
}

func skillName(call openCall) string {
	return str(pick(call.input, "skill", "name", "command"))
}

// skillAllowedTools states what invoking the skill PERMITS — a fact about
// consent rather than about the document's content. Read from the
// ACKNOWLEDGEMENT's own typed result, which is the only record that carries the
// declared set. UNSET when the skill declared no allowances, which is distinct
// from declaring an empty set.
func skillAllowedTools(result map[string]any) *conversationv1.AgentSkillAllowedTools {
	raw := pick(result, "allowedTools", "allowed_tools")
	if raw == nil {
		return nil
	}
	allowed := &conversationv1.AgentSkillAllowedTools{}
	for _, el := range list(raw) {
		if name := str(el); name != "" {
			allowed.ToolNames = append(allowed.ToolNames, name)
		}
	}
	return allowed
}
