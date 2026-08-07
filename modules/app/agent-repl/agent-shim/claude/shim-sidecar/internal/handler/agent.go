package handler

import (
	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/shim-claude-sidecar/internal/convert"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// AgentTranscriptHandler reads an agent sidechain transcript (agent-*.jsonl and
// the a*.output agent spool, which is the same JSONL shape). It REUSES the
// transcript line parser and emits (§7.2): one TaskProgress per batch (the
// running record count is the agent task's liveness signal) plus any recursive
// grandchild launches (nested Agent/Workflow/shell launches inside the sidechain
// become their own TaskStarted). Parse failures still surface as UnparsedEvents.
type AgentTranscriptHandler struct {
	conv *convert.Converter
	log  *logging.Bound
}

// NewAgentTranscriptHandler builds a handler with its own converter.
func NewAgentTranscriptHandler(log *logging.Bound) *AgentTranscriptHandler {
	log.With(logging.Context{Operation: "agent-handler-new"}).LogVerbose("constructing agent transcript handler")
	return &AgentTranscriptHandler{conv: convert.New(log), log: log}
}

// Handle implements Handler.
func (h *AgentTranscriptHandler) Handle(frames []tail.Frame, ctx *Context) []*corev1.Event {
	h.log.With(logging.Context{Operation: "agent-handle", Path: ctx.Path, Session: ctx.SessionID, Task: ctx.TaskID}).LogVerbose("handling frames=%d records_observed=%d", len(frames), ctx.RecordsObserved)
	var out []*corev1.Event
	sawRecord := false
	for _, f := range frames {
		if f.ParseErr != nil {
			h.log.With(logging.Context{Operation: "parse", Path: ctx.Path, Session: ctx.SessionID, Task: ctx.TaskID, Level: "warn"}).Log("parse failure at offset=%d; the record persists only as an UnparsedEvent and the user reads a structureless bubble: %v", f.Offset, f.ParseErr)
			out = append(out, unparsedEvent(ctx.SessionID, ctx.Path, f.Offset, f.Raw, f.ParseErr))
			continue
		}
		sawRecord = true
		line, _, err := h.conv.TranscriptLine(f.Obj)
		if err != nil {
			h.log.With(logging.Context{Operation: "convert", Path: ctx.Path, Session: ctx.SessionID, Task: ctx.TaskID, Level: "warn"}).Log("conversion failure at offset=%d; the record persists only as an UnparsedEvent and the user reads a structureless bubble: %v", f.Offset, err)
			out = append(out, unparsedEvent(ctx.SessionID, ctx.Path, f.Offset, f.Raw, err))
			continue
		}
		// Recursive grandchild launches: a sidechain agent can itself launch
		// detached work.
		if u := line.GetUser(); u != nil {
			out = append(out, launchTwins(u.GetToolUseResult(), u.GetEnvelope(), ctx)...)
		}
	}
	if sawRecord {
		out = append(out, taskProgressEvent(ctx.SessionID, corev1.Plane_PLANE_FILE, &corev1.TaskProgress{
			TaskId:          ctx.TaskID,
			Kind:            corev1.TaskKind_TASK_KIND_AGENT,
			RecordsObserved: ctx.RecordsObserved,
		}))
	}
	h.log.With(logging.Context{Operation: "agent-handle", Path: ctx.Path, Session: ctx.SessionID, Task: ctx.TaskID}).LogVerbose("handled frames=%d saw_record=%t events=%d", len(frames), sawRecord, len(out))
	return out
}
