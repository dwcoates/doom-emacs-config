package sessioncontroller

import (
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// userRecordText is the TEXT a "user" transcript record carries, or "" for a
// record that is not a user record or carries no text at all.
//
// Both curators over user records need exactly this and nothing else — the
// slash-command curator (machinery.go) reads its head, the skill-body curator
// (skillbody.go) reads the whole thing — so the extraction lives once here
// rather than twice beside them.
//
// FOR A BLOCK-SHAPED BODY IT IS THE FIRST TEXT BLOCK. That block is the record:
// a synthesized record's later blocks are more of the same bookkeeping, and a
// record whose first block is real text is a real prompt whatever follows it.
// A pure tool_result record has no text block and yields "".
func userRecordText(it *frontendv1.ConversationItem) string {
	um := it.GetUserMessage()
	if um == nil {
		return ""
	}
	switch content := um.GetContent().(type) {
	case *datav1.ApiUserMessage_ContentString:
		return content.ContentString
	case *datav1.ApiUserMessage_ContentBlocks:
		for _, b := range content.ContentBlocks.GetBlocks() {
			if t := b.GetText(); t.GetText() != "" {
				return t.GetText()
			}
		}
	}
	return ""
}

// toolResultIDs are the tool_use ids a user record reports results for, in the
// order the record carries them. A prompt reports none.
func toolResultIDs(it *frontendv1.ConversationItem) []string {
	um := it.GetUserMessage()
	if um == nil {
		return nil
	}
	blocks, ok := um.GetContent().(*datav1.ApiUserMessage_ContentBlocks)
	if !ok {
		return nil
	}
	var out []string
	for _, b := range blocks.ContentBlocks.GetBlocks() {
		if tr := b.GetToolResult(); tr.GetToolUseId() != "" {
			out = append(out, tr.GetToolUseId())
		}
	}
	return out
}

// toolUseCalls are the (id, name) tool calls an assistant record makes.
func toolUseCalls(it *frontendv1.ConversationItem) map[string]string {
	am := it.GetAssistantMessage()
	if am == nil {
		return nil
	}
	var out map[string]string
	for _, b := range am.GetContent() {
		tu := b.GetToolUse()
		if tu.GetId() == "" {
			continue
		}
		if out == nil {
			out = map[string]string{}
		}
		out[tu.GetId()] = tu.GetName()
	}
	return out
}
