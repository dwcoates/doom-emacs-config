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
// TWO SHAPES, ALL camelCase. The vendor writes ONE file name for TWO different
// documents, and the difference is not a defect:
//
//   - the SUBAGENT shape, under `subagents/agent-<id>.meta.json`, always states
//     `toolUseId` alongside `agentType`, `description` and `spawnDepth` (plus,
//     variously, `parentAgentId`, `model`, `isFork`, `cwd`, `stoppedByUser` and
//     the worktree fields). `toolUseId` IS the identity.
//   - the WORKFLOW shape, under `subagents/workflows/wf_<id>/agent-<id>.meta.json`,
//     states `agentType` ("workflow-subagent"), `spawnDepth` and `model`, plus
//     `worktreePath`/`spawnedWithWorktree` when the agent was given a worktree.
//     IT CARRIES NO `toolUseId` AND NO `description`, because a workflow agent
//     is not spawned by a tool call — there is no spawning call to name it by.
//
// A WORKFLOW AGENT IS ATTRIBUTED TO ITS WORKFLOW, NOT TO A CALL. Its run is the
// `wf_<id>` directory and its parent session is the transcript directory above
// it, both of which the PATH carries; the meta adds the type, depth, model and
// worktree. Holding those transcripts for a `toolUseId` the vendor never writes
// held every workflow agent forever and restated the hold on every rescan.
//
// The model is read but NEVER used as the agent's models_used: that still comes
// from the transcript's own assistant lines (`message.model`), because this file
// states the requested model rather than an observed one.

import (
	"encoding/json"
	"fmt"
	"os"
)

// Shape names which of the two documents a meta file is.
type Shape string

const (
	// ShapeSubagent is a tool-spawned subagent's meta: it states toolUseId,
	// which IS the agent's identity.
	ShapeSubagent Shape = "subagent"
	// ShapeWorkflow is a workflow-spawned agent's meta: no toolUseId, so the
	// agent is attributed to its workflow run and parent session instead.
	ShapeWorkflow Shape = "workflow"
)

// Meta is the parsed companion file.
type Meta struct {
	// Shape says which document this is, and therefore whether ToolUseID is
	// populated at all.
	Shape Shape
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
	// ParentAgentID is the vendor `agent-<id>` locator of the spawning agent,
	// when the vendor states one. It is a locator, never an AgentId.
	ParentAgentID string
	// Model is the model the spawn REQUESTED. It is never reported as the
	// agent's observed model; that comes from the transcript's assistant lines.
	Model string
	// WorktreePath is the worktree the agent was given, when it was given one.
	WorktreePath string
	// SpawnedWithWorktree states that the vendor created that worktree for this
	// agent rather than inheriting one.
	SpawnedWithWorktree bool
}

// metaFile is the on-disk shape. Presence-typed so a missing field is
// distinguishable from a zero one: an unset required field is an error here, not
// a value defaulted away.
type metaFile struct {
	AgentType           *string `json:"agentType"`
	Description         *string `json:"description"`
	ToolUseID           *string `json:"toolUseId"`
	SpawnDepth          *uint32 `json:"spawnDepth"`
	ParentAgentID       *string `json:"parentAgentId"`
	Model               *string `json:"model"`
	WorktreePath        *string `json:"worktreePath"`
	SpawnedWithWorktree *bool   `json:"spawnedWithWorktree"`
}

// ReadMeta parses a companion meta file, in either of its two shapes.
//
// A MISSING `toolUseId` IS NOT AUTOMATICALLY A DEFECT, AND IT IS NEVER A
// FALLBACK TO THE FILENAME. A file that states one is the SUBAGENT shape and
// that id IS the agent's identity under the cross-plane minting rule. A file
// that states none but does state `agentType` is the WORKFLOW shape, which the
// vendor writes without a spawning call at all; the caller attributes such an
// agent to its workflow run and parent session, both of which the path carries.
// A file that states NEITHER names nothing at all and is an error: the caller
// HOLDS the transcript, the same treatment a meta that has not appeared yet
// gets. Naming an agent by its filename is never an option in either shape —
// that would produce a second book for one agent that no consumer could
// reconcile.
func ReadMeta(path string) (Meta, error) {
	raw, err := os.ReadFile(path)
	if err != nil {
		return Meta{}, fmt.Errorf("reading agent meta %s: %w", path, err)
	}
	var file metaFile
	if err := json.Unmarshal(raw, &file); err != nil {
		return Meta{}, fmt.Errorf("parsing agent meta %s: %w", path, err)
	}
	meta := Meta{Shape: ShapeSubagent}
	if file.ToolUseID != nil && *file.ToolUseID != "" {
		meta.ToolUseID = *file.ToolUseID
	} else if file.AgentType != nil && *file.AgentType != "" {
		meta.Shape = ShapeWorkflow
	} else {
		return Meta{}, fmt.Errorf("agent meta %s states neither toolUseId nor agentType, so it names no agent at all", path)
	}
	if file.AgentType != nil {
		meta.AgentType = *file.AgentType
	}
	if file.Description != nil {
		meta.Description = *file.Description
	}
	if file.SpawnDepth != nil {
		meta.SpawnDepth = *file.SpawnDepth
	}
	if file.ParentAgentID != nil {
		meta.ParentAgentID = *file.ParentAgentID
	}
	if file.Model != nil {
		meta.Model = *file.Model
	}
	if file.WorktreePath != nil {
		meta.WorktreePath = *file.WorktreePath
	}
	if file.SpawnedWithWorktree != nil {
		meta.SpawnedWithWorktree = *file.SpawnedWithWorktree
	}
	return meta, nil
}
