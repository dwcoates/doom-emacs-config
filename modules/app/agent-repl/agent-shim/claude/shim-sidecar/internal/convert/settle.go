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
		//
		// BENIGN ON A RE-SCAN — debug, not warn. A cursor-resumed reader (a cold
		// restart, a boot rewind) legitimately observes a result whose call sits
		// before its window; the call cannot be in-window and unresolved, since
		// an in-window call is in openCalls. Storing it as residue is the correct
		// forward path, so this must not flood the strict harvest. The residue
		// record stays; only the severity drops.
		c.log.With(at.ctxFor("orphan-tool-result")).With(logging.Context{ActivityID: callID}).
			LogVerbose("tool result names no call this reader observed; the settle is lost and the record is stored as vendor_specific residue")
		return []*storev1.StoreEntry{VendorSpecificEntry(at, "orphan_tool_result", record)}
	}
	delete(c.openCalls, callID)

	if call.inherited {
		// The call was one this transcript only QUOTED from a parent (a fork's
		// copied context; see assistant.go quotedAssistant). Its producer already
		// settled the unit under the producing agent, so settling it again here
		// would re-book that row under THIS agent, which the store refuses as a
		// book move. Kept as residue instead — book NULL, keyed by the record's
		// own uuid — so the copy is durable, moves no one's row, and re-ingests
		// idempotently across every fork that quotes it.
		c.log.With(at.ctxFor("quoted-tool-result")).With(logging.Context{ActivityID: callID}).
			LogVerbose("tool result for a quoted (inherited) call name=%q kept as residue, not re-settled under this agent", call.name)
		return []*storev1.StoreEntry{VendorSpecificEntry(at, "tool_result/quoted_context", record)}
	}

	// The ONE carve-out in the exempt set: the TaskStop CALL is dropped, but its
	// RESULT is consumed as the owning task's CANCELLED terminal before the drop
	// — deliberately-stopped work must resolve cancelled, never LOST.
	if call.name == taskStopTool {
		return c.taskStopTerminal(result, at, env, agent)
	}
	if IsStreamOwned(call.name) {
		// The stream plane authors this unit whole; see streamowned.go. The
		// result is dropped rather than settled, because the transcript states
		// the answer only in the vendor's joined form and a settle minted from
		// it would supersede the shim's structured one.
		c.log.With(at.ctxFor("stream-owned-drop")).With(logging.Context{ActivityID: callID}).
			LogVerbose("tool result for stream-owned tool name=%q not converted here", call.name)
		return nil
	}
	if IsExempt(call.name) {
		c.log.With(at.ctxFor("exempt-drop")).With(logging.Context{ActivityID: callID}).
			LogVerbose("tool result for exempt tool name=%q dropped entirely", call.name)
		return nil
	}

	// A SUBAGENT'S TRANSCRIPT SOMETIMES CARRIES NO toolUseResult AT ALL, and
	// the vendor's backgrounding sentence is then its only statement that a
	// shell left. See backgroundLaunchFromProse.
	if kind, _ := classifyTool(call.name); result == nil && kind == kindBash {
		if launch, ok := backgroundLaunchFromProse(block); ok {
			c.log.With(at.ctxFor("launch-from-prose")).With(logging.Context{ActivityID: call.activityID, TaskID: str(launch["backgroundTaskId"])}).
				Log("the shell result carries no structured result; its backgrounding sentence names the launched task")
			result = launch
		}
	}

	// A launch tells owner resolution which spool belongs to which call. It is
	// reported before the settle so the root package can attach a spool that is
	// already being written.
	c.reportLaunch(call, result, at)

	kind, known := classifyTool(call.name)
	if !known && call.mcp != nil {
		return []*storev1.StoreEntry{c.settleMcpToolCall(call, block, failed, at, env, agent)}
	}
	if !known {
		return []*storev1.StoreEntry{c.settleUnmodeled(call, block, failed, at, env, agent)}
	}

	settled := c.settledItem(kind, call, result, block, failed, env.timestampMs, at)
	if settled == nil {
		if settlesLater(kind, result) {
			// Deliberate: this kind's unit settles on a later record, and its own
			// branch has already logged which. Filing it as residue would report
			// a perfectly-handled result as a mapping gap.
			return nil
		}
		c.log.With(at.ctxWarn("tool-return")).With(logging.Context{ActivityID: callID}).
			Log("tool result name=%q produced no settled unit; stored as vendor_specific residue so the answer is not lost", call.name)
		return []*storev1.StoreEntry{VendorSpecificEntry(at, "unsettled_tool_result/"+call.name, record)}
	}
	settled.ActivityId = activityID(call.activityID)

	// A write or an edit is the unit an IDE diagnostics attachment joins to by
	// ADJACENCY. One remembered value, replaced on every change.
	if kind == kindWrite || kind == kindEdit {
		c.lastChangeUnit = call.activityID
		c.lastChangeWasEdit = kind == kindEdit
	}

	c.log.With(at.ctxFor("tool-return")).With(logging.Context{ActivityID: call.activityID, UpsertKey: ActivityKey(call.activityID)}).
		LogVerbose("tool result name=%q settled failed=%t", call.name, failed)
	return []*storev1.StoreEntry{c.settledEntry(at, agent, call.activityID, settled)}
}

// toolFailure builds the ONE shape every tool's failure arm carries: what the
// call said when it failed, and when it settled — with the call's own start
// restated beside the settle instant.
func toolFailure(block map[string]any, ts, startMs int64) *conversationv1.AgentToolFailure {
	failure := &conversationv1.AgentToolFailure{SettledAt: settledAt(ts, startMs)}
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
