package discover

// meta.go — agent-<id>.meta.json, the companion file a subagent transcript
// cannot be ingested without.
//
// IT IS THE SOURCE OF THE AGENT'S IDENTITY, not merely of its description. The
// CROSS-PLANE MINTING RULE (conversation/v1 AgentId) binds every producer to one
// id for one agent: a SUBAGENT's AgentId is the `tool_use_id` of the call that
// SPAWNED it — as the vendor's stream carries it, and as the file plane records
// it HERE, in `toolUseId`. The `agent-<id>` in the filename is a LOCATOR and is
// never the AgentId; reading it as one would have the file plane and the stream
// plane name the same agent differently, and no consumer could join them.
//
// FOUR FIELDS, ALL camelCase, AND NO MODEL. The vendor writes exactly
// agentType, description, toolUseId and spawnDepth. The model is deliberately
// absent: it is not stated here at all, and the agent's models_used comes from
// its transcript's own assistant lines (`message.model`) — inventing one from
// this file would report a model nobody observed.

import (
	"encoding/json"
	"fmt"
	"os"
)

// Meta is the parsed companion file.
type Meta struct {
	// AgentType is the resolved subagent type ("general-purpose", a plugin's
	// name, ...).
	AgentType string
	// Description is the one-line task description the spawn was given.
	Description string
	// ToolUseID is the spawning call's tool_use_id, which IS this agent's
	// AgentId under the cross-plane minting rule.
	ToolUseID string
	// SpawnDepth is how deep this agent sits below the main thread.
	SpawnDepth uint32
}

// metaFile is the on-disk shape. Presence-typed so a missing field is
// distinguishable from a zero one: an unset required field is an error here, not
// a value defaulted away.
type metaFile struct {
	AgentType   *string `json:"agentType"`
	Description *string `json:"description"`
	ToolUseID   *string `json:"toolUseId"`
	SpawnDepth  *uint32 `json:"spawnDepth"`
}

// ReadMeta parses a companion meta file.
//
// A MISSING `toolUseId` IS AN ERROR, NEVER A FALLBACK TO THE FILENAME. Without
// it this agent has no identity the stream plane would agree with, and naming it
// by its filename would produce a second book for one agent that no consumer
// could ever reconcile. The caller HOLDS the transcript instead — the same
// treatment a meta file that has not appeared yet gets.
func ReadMeta(path string) (Meta, error) {
	raw, err := os.ReadFile(path)
	if err != nil {
		return Meta{}, fmt.Errorf("reading agent meta %s: %w", path, err)
	}
	var file metaFile
	if err := json.Unmarshal(raw, &file); err != nil {
		return Meta{}, fmt.Errorf("parsing agent meta %s: %w", path, err)
	}
	if file.ToolUseID == nil || *file.ToolUseID == "" {
		return Meta{}, fmt.Errorf("agent meta %s states no toolUseId, which IS the agent's identity under the cross-plane minting rule", path)
	}
	meta := Meta{ToolUseID: *file.ToolUseID}
	if file.AgentType != nil {
		meta.AgentType = *file.AgentType
	}
	if file.Description != nil {
		meta.Description = *file.Description
	}
	if file.SpawnDepth != nil {
		meta.SpawnDepth = *file.SpawnDepth
	}
	return meta, nil
}
