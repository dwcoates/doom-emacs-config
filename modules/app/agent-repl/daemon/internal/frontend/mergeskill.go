// mergeskill.go is the daemon's ONE reading of "this call is a merge run".
//
// WHY IT IS A CLASSIFIER AND NOT A SUBSTRING TEST. `/create-or-update-workspace`
// is one skill with seven verbs — create, prompt, merge, close, open, send,
// status — and only ONE of them drives a whole conversation through this
// workspace's own session. The other six are ordinary skill cards. So the rule
// requires BOTH facts, spelled exactly: the Skill tool naming
// `create-or-update-workspace`, and an argument list whose FIRST token is the
// bare verb `merge`. A prompt that merely mentions merging, a `merge-status`
// verb that does not exist today but might tomorrow, and every other skill are
// all near-misses that must NOT open a bubble, because opening one swallows the
// rest of the session's feed into it.
package frontend

import (
	"strings"

	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// SkillToolName is the harness's tool for launching a skill. Skill invocations
// are synchronous cards by contract; this name is what a merge invocation has
// to be found among.
const SkillToolName = "Skill"

const (
	// mergeSkillName is the skill whose merge verb detaches. It is matched
	// EXACTLY, never by prefix or suffix: a plugin-qualified `x:create-or-
	// update-workspace` is a different skill from a different source, and
	// treating it as this one would hand a stranger's skill the power to
	// swallow the session's feed.
	mergeSkillName = "create-or-update-workspace"
	// mergeSkillVerb is the one verb of that skill which opens a bubble.
	mergeSkillVerb = "merge"
	// skillInputSkillKey and skillInputArgsKey are the two keys a Skill call's
	// input carries on disk: {"skill": "<name>", "args": "<verb and flags>"}.
	skillInputSkillKey = "skill"
	skillInputArgsKey  = "args"
)

// MergeSkillCall reports whether one tool_use block is the merge skill's
// invocation, and the label the bubble wears.
//
// The label is the invocation as the agent wrote it — `/create-or-update-
// workspace merge <rest>` — rather than a resolved merge target: the target is
// not knowable from the call (the skill resolves it itself), and a label
// guessing at one would name the wrong workspace on every ambiguous
// invocation.
func MergeSkillCall(use *datav1.ToolUseBlock) (label string, ok bool) {
	if use.GetName() != SkillToolName {
		return "", false
	}
	fields := use.GetInput().GetFields()
	if fields[skillInputSkillKey].GetStringValue() != mergeSkillName {
		return "", false
	}
	args := fields[skillInputArgsKey].GetStringValue()
	if verb, _, _ := strings.Cut(strings.TrimSpace(args), " "); verb != mergeSkillVerb {
		return "", false
	}
	return "/" + mergeSkillName + " " + strings.TrimSpace(args), true
}

// MergeSkillCallInItem finds the merge skill's invocation among one curated
// conversation item's tool calls, returning the call's tool_use id and the
// bubble's label.
//
// It reads the SAME assistant content blocks harvestToolNames reads, so the
// classification and the tool-name harvest cannot disagree about which calls an
// item made.
func MergeSkillCallInItem(item *frontendv1.ConversationItem) (toolUseID, label string, ok bool) {
	for _, block := range item.GetAgent().GetResponse().GetBody().GetContent() {
		use := block.GetToolUse()
		if use.GetId() == "" {
			continue
		}
		if label, ok := MergeSkillCall(use); ok {
			return use.GetId(), label, true
		}
	}
	return "", "", false
}

// ItemBelongsToCall reports whether one curated item is part of the named
// call's CARD: the call itself, its result, its typed outcome, or its skill
// body.
//
// It exists for the merge window, which must let the merge Skill call's own
// card reach the feed and settle normally while every other emission of the
// window folds into the bubble. The card is where the bubble hangs, so folding
// it away would leave the bubble anchored to a card the reader never sees.
func ItemBelongsToCall(item *frontendv1.ConversationItem, toolUseID string) bool {
	if toolUseID == "" {
		return false
	}
	for _, block := range item.GetAgent().GetResponse().GetBody().GetContent() {
		if block.GetToolUse().GetId() == toolUseID {
			return true
		}
	}
	if item.GetAgent().GetToolCall().GetCall().GetId() == toolUseID {
		return true
	}
	if item.GetAgent().GetToolResult().GetResult().GetToolUseId() == toolUseID {
		return true
	}
	if item.GetAgent().GetToolOutcome().GetToolUseId() == toolUseID {
		return true
	}
	if item.GetAgent().GetSkillBody().GetToolUseId() == toolUseID {
		return true
	}
	for _, block := range item.GetUserMessage().GetContentBlocks().GetBlocks() {
		if block.GetToolResult().GetToolUseId() == toolUseID {
			return true
		}
	}
	return false
}

// EmissionsFromItem converts one curated conversation item into the emissions a
// bubble can carry, or nothing when the item has no emission arm to carry it.
//
// It is detachedEmissions under an exported name, deliberately the SAME
// function: a merge window folds the session's own feed items into a bubble,
// and doing that through a second converter would give the two folds two
// vocabularies for one contract that says they share one.
func EmissionsFromItem(item *frontendv1.ConversationItem) []*frontendv1.AgentEmission {
	return detachedEmissions(item)
}
