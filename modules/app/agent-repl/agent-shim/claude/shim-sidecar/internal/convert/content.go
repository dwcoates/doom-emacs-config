package convert

// content.go — the vendor's seventeen block kinds onto the neutral content
// model's four facts.
//
// The vendor names blocks for its API surface: MCP tool use, server tool use,
// web search result, code execution result, container upload. Those are the
// same two facts wearing different names — a tool was CALLED, and a tool
// RETURNED — so the call-shaped ones all land on ToolCallBlock and the tool's
// own name carries the rest. A block kind with no fact behind it is stated as
// UnsupportedBlock rather than folded into whichever arm is nearest.

import conversationv1 "agentrepl/proto/conversation/v1"

// callBlockTypes are the vendor block kinds that all mean ONE fact: the agent
// called a tool. They differ only in who runs the tool, which the tool's own
// name already says.
var callBlockTypes = map[string]bool{
	"tool_use":        true,
	"server_tool_use": true,
	"mcp_tool_use":    true,
}

// resultBlockTypes are the vendor block kinds that mean a tool RETURNED. They
// are not blocks in the neutral model at all — they become ToolReturned update
// records — so a converter that meets one inside a content array has met a
// record boundary, not a block.
var resultBlockTypes = map[string]bool{
	"tool_result":                true,
	"mcp_tool_result":            true,
	"web_search_tool_result":     true,
	"web_fetch_tool_result":      true,
	"code_execution_tool_result": true,
}

// blockType reads a content block's discriminator.
func blockType(block map[string]any) string {
	t, _ := block["type"].(string)
	return t
}

// userContent converts the vendor's `message.content` for a USER record.
//
// The vendor spells it as EITHER a bare string OR an array of blocks, with the
// same key, so the runtime JSON type is the only discriminator there is.
func userContent(v any) *conversationv1.UserContent {
	out := &conversationv1.UserContent{}
	switch t := v.(type) {
	case string:
		if t == "" {
			return out
		}
		out.Blocks = append(out.Blocks, &conversationv1.UserContentBlock{
			Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: t}},
		})
	case []any:
		for _, el := range t {
			block, ok := el.(map[string]any)
			if !ok {
				out.Blocks = append(out.Blocks, unsupportedUserBlock("", map[string]any{"value": el}))
				continue
			}
			// A tool result inside a user record is the vendor filing a tool's
			// output under the person who did not run it. It is lifted out as a
			// ToolReturned record by the caller and never becomes user content.
			if resultBlockTypes[blockType(block)] {
				continue
			}
			out.Blocks = append(out.Blocks, userBlock(block))
		}
	}
	return out
}

func userBlock(block map[string]any) *conversationv1.UserContentBlock {
	switch blockType(block) {
	case "text":
		text, _ := block["text"].(string)
		return &conversationv1.UserContentBlock{
			Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: text}},
		}
	case "image":
		return &conversationv1.UserContentBlock{
			Block: &conversationv1.UserContentBlock_Image{Image: imageBlock(block)},
		}
	default:
		return unsupportedUserBlock(blockType(block), block)
	}
}

func unsupportedUserBlock(kind string, raw map[string]any) *conversationv1.UserContentBlock {
	return &conversationv1.UserContentBlock{
		Block: &conversationv1.UserContentBlock_Unsupported{Unsupported: unsupportedBlock(kind, raw)},
	}
}

// agentContent converts the vendor's assistant `message.content` array.
//
// Tool RESULTS are skipped here for the same reason as in userContent: they are
// records, not blocks. An assistant record never carries one in practice, but
// the guard is what keeps a vendor change from silently rendering a result as
// an unsupported block nobody looks at.
func agentContent(v any) *conversationv1.AgentContent {
	out := &conversationv1.AgentContent{}
	arr, ok := v.([]any)
	if !ok {
		if s, isString := v.(string); isString && s != "" {
			out.Blocks = append(out.Blocks, &conversationv1.AgentContentBlock{
				Block: &conversationv1.AgentContentBlock_Text{Text: &conversationv1.TextBlock{Text: s}},
			})
		}
		return out
	}
	for _, el := range arr {
		block, ok := el.(map[string]any)
		if !ok {
			out.Blocks = append(out.Blocks, unsupportedAgentBlock("", map[string]any{"value": el}))
			continue
		}
		if resultBlockTypes[blockType(block)] {
			continue
		}
		out.Blocks = append(out.Blocks, agentBlock(block))
	}
	return out
}

func agentBlock(block map[string]any) *conversationv1.AgentContentBlock {
	kind := blockType(block)
	switch {
	case kind == "text":
		text, _ := block["text"].(string)
		return &conversationv1.AgentContentBlock{
			Block: &conversationv1.AgentContentBlock_Text{Text: &conversationv1.TextBlock{Text: text}},
		}
	case kind == "thinking":
		text, _ := block["thinking"].(string)
		return &conversationv1.AgentContentBlock{
			Block: &conversationv1.AgentContentBlock_Thinking{Thinking: &conversationv1.ThinkingBlock{Text: text}},
		}
	case kind == "redacted_thinking":
		// The vendor WITHHELD the reasoning. Stated as redacted rather than left
		// as empty text, so a client can say "reasoning was hidden" instead of
		// showing nothing and implying the agent did not reason.
		return &conversationv1.AgentContentBlock{
			Block: &conversationv1.AgentContentBlock_Thinking{Thinking: &conversationv1.ThinkingBlock{Redacted: true}},
		}
	case callBlockTypes[kind]:
		return &conversationv1.AgentContentBlock{
			Block: &conversationv1.AgentContentBlock_ToolCall{ToolCall: toolCallBlock(block)},
		}
	default:
		return unsupportedAgentBlock(kind, block)
	}
}

func unsupportedAgentBlock(kind string, raw map[string]any) *conversationv1.AgentContentBlock {
	return &conversationv1.AgentContentBlock{
		Block: &conversationv1.AgentContentBlock_Unsupported{Unsupported: unsupportedBlock(kind, raw)},
	}
}

// toolCallBlock reads the three facts a call has, whichever of the vendor's
// three call-shaped kinds carried them.
func toolCallBlock(block map[string]any) *conversationv1.ToolCallBlock {
	id, _ := block["id"].(string)
	name, _ := block["name"].(string)
	out := &conversationv1.ToolCallBlock{ToolCallId: id, ToolName: name}
	if input, ok := block["input"].(map[string]any); ok {
		out.Arguments = rawStruct(input)
	}
	return out
}

// toolResultContent converts what a tool returned.
//
// Narrow and separate from user content on purpose: the vendor delivers tool
// results inside user-role records, and reusing the user's own union here would
// preserve in a new schema the exact accident it was built to erase.
func toolResultContent(v any) *conversationv1.ToolResultContent {
	out := &conversationv1.ToolResultContent{}
	switch t := v.(type) {
	case string:
		if t == "" {
			return out
		}
		out.Blocks = append(out.Blocks, &conversationv1.ToolResultContentBlock{
			Block: &conversationv1.ToolResultContentBlock_Text{Text: &conversationv1.TextBlock{Text: t}},
		})
	case []any:
		for _, el := range t {
			block, ok := el.(map[string]any)
			if !ok {
				out.Blocks = append(out.Blocks, &conversationv1.ToolResultContentBlock{
					Block: &conversationv1.ToolResultContentBlock_Unsupported{
						Unsupported: unsupportedBlock("", map[string]any{"value": el}),
					},
				})
				continue
			}
			out.Blocks = append(out.Blocks, toolResultBlock(block))
		}
	case map[string]any:
		// An MCP or server tool result whose payload is one object rather than an
		// array. Kept whole as an unsupported block rather than flattened, so the
		// shape stays recoverable.
		out.Blocks = append(out.Blocks, &conversationv1.ToolResultContentBlock{
			Block: &conversationv1.ToolResultContentBlock_Unsupported{
				Unsupported: unsupportedBlock("object", t),
			},
		})
	}
	return out
}

func toolResultBlock(block map[string]any) *conversationv1.ToolResultContentBlock {
	switch blockType(block) {
	case "text":
		text, _ := block["text"].(string)
		return &conversationv1.ToolResultContentBlock{
			Block: &conversationv1.ToolResultContentBlock_Text{Text: &conversationv1.TextBlock{Text: text}},
		}
	case "image":
		return &conversationv1.ToolResultContentBlock{
			Block: &conversationv1.ToolResultContentBlock_Image{Image: imageBlock(block)},
		}
	default:
		return &conversationv1.ToolResultContentBlock{
			Block: &conversationv1.ToolResultContentBlock_Unsupported{
				Unsupported: unsupportedBlock(blockType(block), block),
			},
		}
	}
}

// imageBlock reads the vendor's image source.
//
// ImageBlock carries an image BY REFERENCE — "a path or URL rather than bytes"
// — and the vendor's transcript carries base64 bytes inline, so there is no
// reference to give. The media type is stated and the source left empty rather
// than inventing a path or inlining megabytes into a record that is replayed on
// every page load. See the gap note on inline image bytes.
func imageBlock(block map[string]any) *conversationv1.ImageBlock {
	out := &conversationv1.ImageBlock{}
	source, ok := block["source"].(map[string]any)
	if !ok {
		return out
	}
	if mediaType, ok := source["media_type"].(string); ok {
		out.MediaType = mediaType
	}
	// A URL-sourced image is the one case the vendor DOES give a reference for.
	if url, ok := source["url"].(string); ok {
		out.Source = url
	}
	return out
}

func unsupportedBlock(kind string, raw map[string]any) *conversationv1.UnsupportedBlock {
	return &conversationv1.UnsupportedBlock{Kind: kind, Raw: rawStruct(raw)}
}
