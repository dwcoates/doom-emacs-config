package handler

import (
	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	"agentrepl/shim-claude-sidecar/internal/convert"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// SessionTranscriptHandler converts session transcript lines into their vendor
// file-plane twins PLUS the vendor-neutral lifecycle events the file plane owns
// (§7.2): TaskStarted from launch results (agent/workflow/shell, constructing a
// shell task's output path), TurnEnded from stop_hook_summary, and TaskEnded
// (STOPPED) from TaskStop results.
type SessionTranscriptHandler struct {
	conv *convert.Converter
	log  Logf
}

// NewSessionTranscriptHandler builds a handler with its own converter.
func NewSessionTranscriptHandler(log Logf) *SessionTranscriptHandler {
	return &SessionTranscriptHandler{conv: convert.New(log), log: log}
}

// Handle implements Handler.
func (h *SessionTranscriptHandler) Handle(frames []tail.Frame, ctx *Context) []*corev1.Event {
	var out []*corev1.Event
	for _, f := range frames {
		if f.ParseErr != nil {
			h.log("transcript: parse failure at %s:%d: %v", ctx.Path, f.Offset, f.ParseErr)
			out = append(out, unparsedEvent(ctx.SessionID, ctx.Path, f.Offset, f.Raw, f.ParseErr))
			continue
		}
		line, extras, err := h.conv.TranscriptLine(f.Obj)
		if err != nil {
			h.log("transcript: conversion failure at %s:%d: %v", ctx.Path, f.Offset, err)
			out = append(out, unparsedEvent(ctx.SessionID, ctx.Path, f.Offset, f.Raw, err))
			continue
		}
		if ev := vendorEvent(ctx.SessionID, line, extras, "", h.log); ev != nil {
			out = append(out, ev)
		}
		out = append(out, h.lifecycle(line, ctx)...)
	}
	return out
}

// lifecycle emits the vendor-neutral twins derivable from one transcript line.
func (h *SessionTranscriptHandler) lifecycle(line *datav1.TranscriptLine, ctx *Context) []*corev1.Event {
	var out []*corev1.Event
	if u := line.GetUser(); u != nil {
		out = append(out, launchTwins(u.GetToolUseResult(), u.GetEnvelope(), ctx)...)
	}
	if s := line.GetSystem(); s != nil {
		if sh := s.GetStopHookSummary(); sh != nil {
			uuid := s.GetEnvelope().GetUuid()
			out = append(out, turnEndedEvent(ctx.SessionID, turnDedupKey(ctx.SessionID, uuid), &corev1.TurnEnded{
				StopReason: sh.GetStopReason(),
				IsError:    sh.GetPreventedContinuation(),
			}))
		}
	}
	return out
}

// launchTwins turns a tool result into the detached-task lifecycle twin it
// implies: a launch → TaskStarted; a TaskStop → TaskEnded(STOPPED). Shared by the
// session and agent (recursive grandchild launches) handlers.
func launchTwins(tur *datav1.ToolUseResult, env *datav1.LineEnvelope, ctx *Context) []*corev1.Event {
	if tur == nil {
		return nil
	}
	switch {
	case tur.GetAgentAsyncLaunch() != nil:
		a := tur.GetAgentAsyncLaunch()
		return []*corev1.Event{taskStartedEvent(ctx.SessionID, corev1.Plane_PLANE_FILE, &corev1.TaskStarted{
			TaskId:      a.GetAgentId(),
			Kind:        corev1.TaskKind_TASK_KIND_AGENT,
			ToolUseId:   env.GetSourceToolUseId(),
			Description: a.GetDescription(),
			OutputPath:  a.GetOutputFile(),
		})}
	case tur.GetWorkflowLaunch() != nil:
		w := tur.GetWorkflowLaunch()
		out := w.GetTranscriptDir()
		if out != "" {
			out = out + "/journal.jsonl"
		}
		id := w.GetRunId()
		if id == "" {
			id = w.GetTaskId()
		}
		return []*corev1.Event{taskStartedEvent(ctx.SessionID, corev1.Plane_PLANE_FILE, &corev1.TaskStarted{
			TaskId:      id,
			Kind:        corev1.TaskKind_TASK_KIND_WORKFLOW,
			ToolUseId:   env.GetSourceToolUseId(),
			Description: w.GetSummary(),
			OutputPath:  out,
		})}
	case tur.GetBash().GetBackgroundTaskId() != "":
		b := tur.GetBash()
		return []*corev1.Event{taskStartedEvent(ctx.SessionID, corev1.Plane_PLANE_FILE, &corev1.TaskStarted{
			TaskId:     b.GetBackgroundTaskId(),
			Kind:       corev1.TaskKind_TASK_KIND_SHELL,
			ToolUseId:  env.GetSourceToolUseId(),
			OutputPath: shellOutputPath(ctx.SpoolDir, b.GetBackgroundTaskId()),
		})}
	case tur.GetTaskStop() != nil:
		ts := tur.GetTaskStop()
		return []*corev1.Event{taskEndedEvent(ctx.SessionID, corev1.Plane_PLANE_FILE, &corev1.TaskEnded{
			TaskId:  ts.GetTaskId(),
			Status:  corev1.TerminalStatus_TERMINAL_STATUS_STOPPED,
			Summary: ts.GetMessage(),
		})}
	}
	return nil
}
