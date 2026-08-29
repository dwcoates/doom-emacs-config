package convert

// settle.go — THE RETURN SIDE: a tool result UPSERTS its unit's whole settled
// state under the SAME identity the call announced.
//
// A RETURN IS NEVER A CHILD. Nothing in this contract has a tool call as a
// parent: the settled frame supersedes the announcement's row entirely, which is
// why every settled arm restates what was called rather than relying on the
// consumer having kept the earlier frame.
//
// THE JOIN IS ONE LOOKUP: the vendor's `tool_use_id`, carried on the result
// block. A result whose call this reader never observed is NOT given an invented
// parent — it lands as vendor_specific residue, loudly, because after a cursor
// restart a lost settle is a real gap and the integration loop must see it.

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// resultBlockTypes are the vendor's spellings of "a tool answered".
var resultBlockTypes = map[string]bool{
	"tool_result":            true,
	"web_search_tool_result": true,
	"mcp_tool_result":        true,
}

// toolReturns settles every tool result carried on a user record.
func (c *Converter) toolReturns(record map[string]any, message map[string]any, at Attribution, env envelope, agent string) []*storev1.StoreEntry {
	var out []*storev1.StoreEntry
	for _, raw := range list(message["content"]) {
		block := obj(raw)
		if block == nil || !resultBlockTypes[str(block["type"])] {
			continue
		}
		out = append(out, c.toolReturn(block, record, at, env, agent)...)
	}
	return out
}

// toolReturn settles ONE result.
func (c *Converter) toolReturn(block, record map[string]any, at Attribution, env envelope, agent string) []*storev1.StoreEntry {
	callID := str(pick(block, "tool_use_id", "toolUseId"))
	result := obj(record["toolUseResult"])
	failed := boolean(block["is_error"])

	call, resolved := c.openCalls[callID]
	if !resolved {
		// The call was read before this reader's cursor. There is no unit to
		// settle and none is invented.
		c.log.With(logging.Context{Operation: "orphan-tool-result", Path: at.Path, Level: "warn"}).
			Log("tool result activity_id=%s at offset=%d path=%s names no call this reader observed; the settle is lost and the record is stored as vendor_specific residue", callID, at.Offset, at.Path)
		return []*storev1.StoreEntry{VendorSpecificEntry(at, "orphan_tool_result", record)}
	}
	delete(c.openCalls, callID)

	// The ONE carve-out in the exempt set: the TaskStop CALL is dropped, but its
	// RESULT is consumed as the owning task's CANCELLED terminal before the drop
	// — deliberately-stopped work must resolve cancelled, never LOST.
	if call.name == taskStopTool {
		return c.taskStopTerminal(result, at, env, agent)
	}
	if IsExempt(call.name) {
		c.log.With(logging.Context{Operation: "exempt-drop", Path: at.Path}).
			LogVerbose("tool result for exempt tool name=%q activity_id=%s at offset=%d dropped entirely", call.name, callID, at.Offset)
		return nil
	}

	// A launch tells owner resolution which spool belongs to which call. It is
	// reported before the settle so the root package can attach a spool that is
	// already being written.
	c.reportLaunch(call, result, at)

	kind, known := classifyTool(call.name)
	if known && kind == kindQuestion {
		return []*storev1.StoreEntry{c.settleQuestion(call, result, block, failed, at, env, agent)}
	}
	if !known {
		return []*storev1.StoreEntry{c.settleUnmodeled(call, block, failed, at, env, agent)}
	}

	settled := c.settledItem(kind, call, result, block, failed, env.timestampMs, at)
	if settled == nil {
		c.log.With(logging.Context{Operation: "tool-return", Path: at.Path, Level: "warn"}).
			Log("tool result name=%q activity_id=%s at offset=%d produced no settled unit; stored as vendor_specific residue so the answer is not lost", call.name, callID, at.Offset)
		return []*storev1.StoreEntry{VendorSpecificEntry(at, "unsettled_tool_result/"+call.name, record)}
	}
	settled.ActivityId = activityID(call.activityID)

	// A write or an edit is the unit an IDE diagnostics attachment joins to by
	// ADJACENCY. One remembered value, replaced on every change.
	if kind == kindWrite || kind == kindEdit {
		c.lastChangeUnit = call.activityID
		c.lastChangeWasEdit = kind == kindEdit
	}

	c.log.With(logging.Context{Operation: "tool-return", Path: at.Path}).
		LogVerbose("tool result name=%q activity_id=%s upsert_key=%s settled at offset=%d failed=%t", call.name, callID, ActivityKey(call.activityID), at.Offset, failed)
	return []*storev1.StoreEntry{c.settledEntry(at, agent, call.activityID, settled)}
}

// toolFailure builds the ONE shape every tool's failure arm carries: what the
// call said when it failed, and when it settled.
func toolFailure(block map[string]any, ts int64) *conversationv1.AgentToolFailure {
	failure := &conversationv1.AgentToolFailure{SettledAt: settledAt(ts)}
	if content := resultContent(block["content"]); content != nil {
		failure.Content = content
	}
	return failure
}

// resultContent reads a tool's returned blocks. Returns nil when the producer
// observed a failed call with NO error content at all, which happens and is
// different from an empty text block.
func resultContent(raw any) *conversationv1.ToolResultContent {
	switch value := raw.(type) {
	case string:
		return &conversationv1.ToolResultContent{Blocks: []*conversationv1.ToolResultContentBlock{{
			Block: &conversationv1.ToolResultContentBlock_Text{Text: &conversationv1.TextBlock{Text: value}},
		}}}
	case []any:
		content := &conversationv1.ToolResultContent{}
		for _, el := range value {
			block := obj(el)
			if block == nil {
				continue
			}
			content.Blocks = append(content.Blocks, resultContentBlock(block))
		}
		if len(content.Blocks) == 0 {
			return nil
		}
		return content
	default:
		return nil
	}
}

func resultContentBlock(block map[string]any) *conversationv1.ToolResultContentBlock {
	switch str(block["type"]) {
	case "text":
		return &conversationv1.ToolResultContentBlock{
			Block: &conversationv1.ToolResultContentBlock_Text{Text: &conversationv1.TextBlock{Text: str(block["text"])}},
		}
	case "image":
		return &conversationv1.ToolResultContentBlock{
			Block: &conversationv1.ToolResultContentBlock_Image{Image: imageBlock(block)},
		}
	default:
		// A block kind this schema does not model, kept WHOLE so the decision
		// to not model it stays reversible from stored data.
		return &conversationv1.ToolResultContentBlock{
			Block: &conversationv1.ToolResultContentBlock_Unsupported{Unsupported: &conversationv1.UnsupportedBlock{
				Kind: str(block["type"]),
				Raw:  rawStruct(block),
			}},
		}
	}
}

// imageBlock carries an image by REFERENCE. The vendor inlines base64 bytes;
// this producer keeps no spill file, so a source it cannot reference by path or
// url carries the media type and an empty reference rather than megabytes that
// would be replayed on every read.
func imageBlock(block map[string]any) *conversationv1.ImageBlock {
	source := obj(block["source"])
	image := &conversationv1.ImageBlock{MediaType: str(source["media_type"])}
	if url := str(source["url"]); url != "" {
		image.Location = &conversationv1.ImageBlock_Url{Url: &conversationv1.ImageBlockUrl{Url: url}}
		return image
	}
	if path := str(pick(source, "path", "file_path")); path != "" {
		image.Location = &conversationv1.ImageBlock_Path{Path: &conversationv1.ImageBlockPath{Path: path}}
	}
	return image
}
