package handler

import (
	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// ShellOutputHandler tracks a background shell spool (b*.output). Shell spools
// carry no structure the sidecar interprets (§7.2): the handler emits a single
// byte-count TaskProgress per batch. Completion is never inferred here — a shell
// spool just ends; terminal status comes from the stream plane or the staleness
// policy (§7.4).
type ShellOutputHandler struct {
	log Logf
}

// NewShellOutputHandler builds a handler.
func NewShellOutputHandler(log Logf) *ShellOutputHandler {
	return &ShellOutputHandler{log: log}
}

// Handle implements Handler.
func (h *ShellOutputHandler) Handle(frames []tail.Frame, ctx *Context) []*corev1.Event {
	if len(frames) == 0 {
		return nil
	}
	return []*corev1.Event{taskProgressEvent(ctx.SessionID, corev1.Plane_PLANE_FILE, &corev1.TaskProgress{
		TaskId:        ctx.TaskID,
		Kind:          corev1.TaskKind_TASK_KIND_SHELL,
		BytesObserved: ctx.BytesObserved,
	})}
}
