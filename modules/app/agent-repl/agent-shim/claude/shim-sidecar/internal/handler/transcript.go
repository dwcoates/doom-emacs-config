package handler

import (
	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	"agentrepl/shim-claude-sidecar/internal/convert"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
	"google.golang.org/protobuf/types/known/structpb"
)

// SessionTranscriptHandler converts session transcript lines into their vendor
// file-plane twins PLUS the vendor-neutral lifecycle events the file plane owns
// (§7.2): TaskStarted from launch results (agent/workflow/shell, constructing a
// shell task's output path), TurnEnded from stop_hook_summary, TaskEnded
// (STOPPED) from TaskStop results, and the two context-loss events
// (clearcompact.go) — ContextCleared from an expanded /clear envelope and
// ContextCompacted coalesced across a compact_boundary and the summary line
// following it. The file plane is the sole producer of both.
type SessionTranscriptHandler struct {
	conv *convert.Converter
	log  *logging.Bound
}

// NewSessionTranscriptHandler builds a handler with its own converter.
func NewSessionTranscriptHandler(log *logging.Bound) *SessionTranscriptHandler {
	return &SessionTranscriptHandler{conv: convert.New(log), log: log}
}

// converted is one frame's conversion result. Conversion is memoized per batch
// because a compaction has to look at the line AFTER the boundary, and a
// frame must not be converted (and so must not loud-log its unknown fields)
// twice.
type converted struct {
	line   *datav1.TranscriptLine
	extras *structpb.Struct
	err    error
}

// Handle implements Handler.
func (h *SessionTranscriptHandler) Handle(frames []tail.Frame, ctx *Context) []*corev1.Event {
	var out []*corev1.Event
	memo := make([]*converted, len(frames))
	// A compaction boundary at the very end of a batch is UNSETTLED: its summary
	// is the next line in the file and may not be written yet. holdCount defers
	// it (leaving the reader's cursor before it) rather than converting it on
	// half the evidence — see clearcompact.go.
	frames = frames[:h.holdCount(memo, frames, ctx)]
	for i, f := range frames {
		if f.ParseErr != nil {
			h.log.With(logging.Context{Operation: "parse", Path: ctx.Path, Session: ctx.SessionID, Task: ctx.TaskID}).Log("parse failure at offset=%d: %v", f.Offset, f.ParseErr)
			out = append(out, unparsedEvent(ctx.SessionID, ctx.Path, f.Offset, f.Raw, f.ParseErr))
			continue
		}
		c := h.convertAt(memo, frames, i)
		if c.err != nil {
			h.log.With(logging.Context{Operation: "convert", Path: ctx.Path, Session: ctx.SessionID, Task: ctx.TaskID}).Log("conversion failure at offset=%d: %v", f.Offset, c.err)
			out = append(out, unparsedEvent(ctx.SessionID, ctx.Path, f.Offset, f.Raw, c.err))
			continue
		}
		if ev := vendorEvent(ctx.SessionID, c.line, c.extras, "", h.log); ev != nil {
			out = append(out, ev)
		}
		out = append(out, h.lifecycle(c.line, h.convertAt(memo, frames, i+1).line, ctx)...)
	}
	return out
}

// convertAt returns the memoized conversion of frames[i]. An index past the end,
// or a frame that failed to parse, yields a zero result whose line is nil —
// which is exactly what a lookahead past the batch's last line means.
func (h *SessionTranscriptHandler) convertAt(memo []*converted, frames []tail.Frame, i int) *converted {
	if i < 0 || i >= len(frames) {
		return &converted{}
	}
	if memo[i] != nil {
		return memo[i]
	}
	if frames[i].ParseErr != nil {
		memo[i] = &converted{}
		return memo[i]
	}
	line, extras, err := h.conv.TranscriptLine(frames[i].Obj)
	memo[i] = &converted{line: line, extras: extras, err: err}
	return memo[i]
}

// lifecycle emits the vendor-neutral twins derivable from one transcript line.
// next is the line that FOLLOWS it in the file (nil at the end of a batch), and
// exists solely so a compaction boundary can pick up its summary.
func (h *SessionTranscriptHandler) lifecycle(line, next *datav1.TranscriptLine, ctx *Context) []*corev1.Event {
	var out []*corev1.Event
	if u := line.GetUser(); u != nil {
		out = append(out, launchTwins(u.GetToolUseResult(), u.GetEnvelope(), ctx)...)
		// A compaction summary is typed `user` but is the harness's text, not
		// the user's, so it is never read as a prompt — it is consumed by the
		// boundary that precedes it (compactSummary) and nothing else.
		if !u.GetEnvelope().GetIsCompactSummary() && isClearCommand(u.GetMessage()) {
			out = append(out, clearedEvent(ctx.SessionID, u.GetEnvelope().GetUuid(), h.log))
		}
	}
	if s := line.GetSystem(); s != nil {
		if sh := s.GetStopHookSummary(); sh != nil {
			uuid := s.GetEnvelope().GetUuid()
			out = append(out, turnEndedEvent(ctx.SessionID, turnDedupKey(ctx.SessionID, uuid), &corev1.TurnEnded{
				StopReason: sh.GetStopReason(),
				IsError:    sh.GetPreventedContinuation(),
			}))
		}
		if cb := s.GetCompactBoundary(); cb != nil {
			out = append(out, compactedEvent(ctx.SessionID, s.GetEnvelope(), cb, next, h.log))
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
