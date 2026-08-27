package handler

import (
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/convert"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// AgentTranscriptHandler reads a subagent's sidechain transcript, which is the
// same JSONL shape as a session transcript and is therefore parsed by the same
// converter.
//
// WHAT DIFFERS IS LINEAGE, AND ONLY LINEAGE. Every record it reads sits INSIDE
// the detached-work card the subagent runs as, so a page of ten feed rows counts
// the subagent once however long its conversation runs. The card's message id is
// derived from the task id the discoverer already holds, so nothing has to be
// correlated with the session transcript that launched it and nothing has to
// survive a restart for the association to hold.
//
// A sidechain can itself launch detached work — a subagent spawning a subagent —
// and those launches open their own cards through the same path, because the
// converter reads them off the tool result rather than off the file it is in.
type AgentTranscriptHandler struct {
	conv *convert.Converter
	log  *logging.Bound
}

// NewAgentTranscriptHandler builds a handler with its own converter.
func NewAgentTranscriptHandler(log *logging.Bound) *AgentTranscriptHandler {
	log.With(logging.Context{Operation: "agent-handler-new"}).LogVerbose("constructing agent transcript handler")
	return &AgentTranscriptHandler{conv: convert.New(log), log: log}
}

// Handle implements tail.Handler.
func (h *AgentTranscriptHandler) Handle(frames []tail.Frame, ctx *Context) []*storev1.StoreEntry {
	h.log.With(logging.Context{Operation: "agent-handle", Path: ctx.Path, Session: ctx.SessionID, Task: ctx.TaskID}).
		LogVerbose("handling frames=%d records_observed=%d", len(frames), ctx.RecordsObserved)
	var out []*storev1.StoreEntry
	for i, frame := range frames {
		at := attribute(ctx, frame.Offset)
		if frame.ParseErr != nil {
			h.log.With(logging.Context{Operation: "parse", Path: ctx.Path, Session: ctx.SessionID, Task: ctx.TaskID, Level: "warn"}).
				Log("parse failure at offset=%d; the record is stored whole with no path to a page: %v", frame.Offset, frame.ParseErr)
			out = append(out, convert.UnparsedEntry(at, frame.Raw, frame.ParseErr))
			continue
		}
		out = append(out, h.conv.Line(frame.Obj, at, lookahead(frames, i+1))...)
	}
	logUnconverted(h.log, ctx, out)
	h.log.With(logging.Context{Operation: "agent-handle", Path: ctx.Path, Session: ctx.SessionID, Task: ctx.TaskID}).
		LogVerbose("handled frames=%d entries=%d", len(frames), len(out))
	return out
}
