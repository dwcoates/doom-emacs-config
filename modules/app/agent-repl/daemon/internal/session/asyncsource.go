// AsyncSource classification (§2.6 async-source).
//
// One tool call in a hundred spawns work that outlives it: a background
// agent, a backgrounded shell, a workflow run. This file answers "did THIS
// call spawn such work, and where does its stream live?" — once, from the
// SDK's structured tool_use_result, for every async type there is.
//
// Before this, that question was answered independently at four layers by
// regex over English prose (tailer.go's spawn regexes, replay.go's agentID
// regex, the webapp's SPAWNED_ID_RE and AGENT_SPAWN_RE). Every one of them
// failed silently and identically on a CLI wording change: no match is
// indistinguishable from no spawn, so tailing, the poll route, and every
// watcher fold would go quiet with no error. The structured result carries
// the same facts as data, so the classification is now total and typed.
package session

import (
	"encoding/json"

	"claude-repld/internal/protocol"
)

// asyncStructured is the union of the tool_use_result fields that announce
// detached work. Every field is optional: the shape is per-tool, and this
// struct only names the parts that mean "a stream exists". A tool_use_result
// carrying none of them is not an async spawn.
type asyncStructured struct {
	// Agent, run_in_background: the harness's own agent id and the file it
	// writes. Both structural — see parseAgentSpawn for the prose fallback.
	IsAsync           bool   `json:"isAsync"`
	AgentID           string `json:"agentId"`
	OutputFile        string `json:"outputFile"`
	CanReadOutputFile bool   `json:"canReadOutputFile"`
	Description       string `json:"description"`
	// Bash, run_in_background. The id is structural; the spool PATH is not
	// (it is announced only in the result prose), so the tailer's own
	// discovery still owns the file and this only names the source.
	BackgroundTaskID string `json:"backgroundTaskId"`
	// Shared: "async_launched", "completed", "killed", ...
	Status string `json:"status"`
}

// asyncStatus maps the harness's per-tool status vocabulary onto the closed
// AsyncSource enum. An unrecognized value reads as running rather than as a
// terminal state: a fold that wrongly says "done" hides live output, while
// one that wrongly says "running" only shows a spinner a beat too long.
func asyncStatus(raw string) string {
	switch raw {
	case "completed", "done", "success":
		return "done"
	case "failed", "error":
		return "error"
	case "killed", "cancelled", "canceled":
		return "killed"
	default:
		return "running"
	}
}

// classifyAsyncSource derives the §2.6 descriptor from a settled call's
// structured result, or nil when the call spawned nothing detached.
//
// isError is deliberately load-bearing: a failed spawn announces no work to
// stream, so it never yields a source no matter what the payload says.
func classifyAsyncSource(toolName string, structured json.RawMessage, isError bool) *protocol.AsyncSource {
	if isError || len(structured) == 0 {
		return nil
	}
	var s asyncStructured
	if err := json.Unmarshal(structured, &s); err != nil {
		// A tool_use_result that is not an object (a bare string result) is
		// simply not an async spawn — not a failure worth surfacing.
		return nil
	}

	switch toolName {
	case "Task", "Agent":
		// A synchronous subagent is NOT an async source: its stream already
		// arrived inline, parent_tool_use_id-tagged, and its children are
		// already nested by the feed partition. Only a detached one owns a
		// stream that outlives the call.
		if !s.IsAsync || s.AgentID == "" {
			return nil
		}
		src := &protocol.AsyncSource{
			SourceID: s.AgentID,
			Kind:     "agent",
			Label:    s.Description,
			Status:   asyncStatus(s.Status),
		}
		// The output file is the agent's FULL JSONL transcript — the same
		// shape replay.go already parses — so the client renders it as
		// nested bubbles, exactly as it renders an inline subagent. It is
		// polled rather than pushed: pushing a whole transcript through the
		// retention ring would evict the conversation it belongs to. The
		// path rides the descriptor so the poll route's record no longer
		// depends on the prose announcement matching (parseAgentSpawn).
		if s.OutputFile != "" && s.CanReadOutputFile {
			src.Stream = &protocol.StreamRef{Transport: "poll", Format: "jsonl-transcript"}
			src.OutputFile = s.OutputFile
		}
		return src

	case "Bash":
		if s.BackgroundTaskID == "" {
			return nil
		}
		// A shell's spool file is unstructured bytes with nothing to
		// recover, so it stays a pre. The daemon already tails it and
		// pushes task-output-delta, hence ws rather than poll.
		return &protocol.AsyncSource{
			SourceID: s.BackgroundTaskID,
			Kind:     "shell",
			Status:   asyncStatus(s.Status),
			Stream:   &protocol.StreamRef{Transport: "ws", Format: "text"},
		}
	}
	return nil
}
