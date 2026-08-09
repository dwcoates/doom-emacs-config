// mergeskill.go is the daemon's ONE reading of a Skill tool call: whether it is
// an invocation at all, and if so whether it is THE merge run or one of every
// other skill.
//
// ONE PASS, ONE VERDICT. Both questions are answered by the same function, so
// merge detection cannot disagree with skill detection about the same call:
// `/create-or-update-workspace merge` is a merge run and never also a skill
// bubble, and every other invocation is a skill bubble and never also a merge.
// async-bubble.proto states that split — "`merge` is the one skill with an arm
// of its own … every other skill arrives as `skill`" — and this is where it is
// decided.
//
// WHY THE MERGE READING IS A CLASSIFIER AND NOT A SUBSTRING TEST.
// `/create-or-update-workspace`
// is one skill with seven verbs — create, prompt, merge, close, open, send,
// status — and only ONE of them drives a whole conversation through this
// workspace's own session. The other six are ordinary skill cards. So the rule
// requires BOTH facts, spelled exactly: the Skill tool naming
// `create-or-update-workspace`, and an argument list whose FIRST token is the
// bare verb `merge`. A prompt that merely mentions merging, a `merge-status`
// verb that does not exist today but might tomorrow, and every other skill are
// all near-misses that must NOT open a MERGE bubble: they are ordinary skill
// invocations, and rendering one as a merge run would describe work nobody
// started.
package frontend

import (
	"strings"

	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// SkillToolName is the harness's tool for launching a skill. Every invocation of
// it is bubble-forming by contract; this name is what both readings find their
// invocations among.
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

// SkillInvocation is one classified Skill call: what it invoked, how it is
// labelled, and which of the two bubble kinds it opens.
type SkillInvocation struct {
	// ToolUseID is the call that made the invocation. It is the bubble's
	// identity, its origin_tool_use_id, and the handle the card is stamped by.
	ToolUseID string
	// SkillName is the skill as the call named it, verbatim.
	SkillName string
	// Args are the invocation's arguments, verbatim — untrimmed, because the
	// contract says verbatim and the label below is the only place a tidied
	// rendering belongs.
	Args string
	// Label is the invocation as the agent wrote it: `/<skill> <args>`.
	Label string
	// IsMerge states that this invocation is THE merge run — the one skill call
	// that opens a Merge bubble rather than a Skill one.
	IsMerge bool
}

// SkillCall reports whether one tool_use block is a skill invocation, and what
// it invoked.
//
// A CALL THAT NAMES NO SKILL IS NOT AN INVOCATION. The skill's name is the whole
// of what identifies the work — it is the bubble's label, and the only thing a
// reader could act on — so a `Skill` call whose input carries no name opens no
// bubble. It is not silently dropped either: its card still renders, and the
// caller records the refusal (see the consumer's observeSkillSpawn), because a
// nameless bubble would be a fold nobody could say anything about.
//
// The label is the invocation as the agent wrote it — `/create-or-update-
// workspace merge <rest>` — rather than a resolved target: for a merge the
// target is not knowable from the call (the skill resolves it itself), and a
// label guessing at one would name the wrong workspace on every ambiguous
// invocation.
func SkillCall(use *datav1.ToolUseBlock) (SkillInvocation, bool) {
	if use.GetName() != SkillToolName {
		return SkillInvocation{}, false
	}
	fields := use.GetInput().GetFields()
	name := fields[skillInputSkillKey].GetStringValue()
	if name == "" {
		return SkillInvocation{}, false
	}
	args := fields[skillInputArgsKey].GetStringValue()
	trimmed := strings.TrimSpace(args)
	inv := SkillInvocation{
		ToolUseID: use.GetId(),
		SkillName: name,
		Args:      args,
		Label:     skillLabel(name, trimmed),
	}
	// THE MERGE READING, AND ONLY FOR ITS EXACT INVOCATION. Both facts are
	// required and spelled exactly: the skill matched by whole name, and an
	// argument list whose FIRST token is the bare verb. Every near-miss stays an
	// ordinary skill invocation rather than becoming a merge.
	if name == mergeSkillName {
		if verb, _, _ := strings.Cut(trimmed, " "); verb == mergeSkillVerb {
			inv.IsMerge = true
		}
	}
	return inv, true
}

// skillLabel renders the invocation as the agent wrote it. A skill invoked with
// no arguments gets no trailing space, so the label reads `/demo` rather than
// `/demo `.
func skillLabel(name, args string) string {
	if args == "" {
		return "/" + name
	}
	return "/" + name + " " + args
}

// MergeSkillCall reports whether one tool_use block is the merge skill's
// invocation, and the label the bubble wears. It is SkillCall's merge verdict
// under the name the merge window asks by, so the two readings are one.
func MergeSkillCall(use *datav1.ToolUseBlock) (label string, ok bool) {
	inv, ok := SkillCall(use)
	if !ok || !inv.IsMerge {
		return "", false
	}
	return inv.Label, true
}

// SkillToolCallsInItem finds every `Skill` call among one curated conversation
// item's tool calls, in the order the agent made them, INVOCATION OR NOT.
//
// It hands back the raw calls rather than only the classified ones so a caller
// can tell "this item invoked no skill" from "this item made a Skill call that
// named none" — different facts, and only the second one is worth a record.
//
// It reads the SAME assistant content blocks harvestToolNames reads, so the
// classification and the tool-name harvest cannot disagree about which calls an
// item made.
func SkillToolCallsInItem(item *frontendv1.ConversationItem) []*datav1.ToolUseBlock {
	var out []*datav1.ToolUseBlock
	for _, block := range item.GetAgent().GetResponse().GetBody().GetContent() {
		use := block.GetToolUse()
		if use.GetId() == "" || use.GetName() != SkillToolName {
			continue
		}
		out = append(out, use)
	}
	return out
}

// ItemBelongsToCall reports whether one curated item is part of the named
// call's CARD: the call itself, its result, its typed outcome, or its skill
// body.
//
// It exists for the window apparatus, which must let each open window's own
// Skill call card reach the feed and settle normally while every other emission
// of the window folds into the bubble. The card is where the bubble hangs, so folding
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
// function: a window folds the session's own feed items into a bubble, and
// doing that through a second converter would give the two folds two
// vocabularies for one contract that says they share one.
func EmissionsFromItem(item *frontendv1.ConversationItem) []*frontendv1.AgentEmission {
	return detachedEmissions(item)
}
