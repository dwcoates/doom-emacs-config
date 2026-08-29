package handler

import (
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/convert"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// WorkflowJournalHandler reads a workflow run's journal: the steps the run
// recorded as it executed.
//
// The run has a card in the feed, opened by the transcript that launched it, and
// its journal is what accumulates into that card. The run id lives in the file
// PATH rather than in any record, which is why the handler supplies it rather
// than the converter reading it.
type WorkflowJournalHandler struct {
	conv *convert.Converter
	log  *logging.Bound
}

// NewWorkflowJournalHandler builds a handler with its own converter.
func NewWorkflowJournalHandler(log *logging.Bound) *WorkflowJournalHandler {
	log.With(logging.Context{Operation: "journal-handler-new"}).LogVerbose("constructing workflow journal handler")
	return &WorkflowJournalHandler{conv: convert.New(log), log: log}
}

// Handle implements tail.Handler.
func (h *WorkflowJournalHandler) Handle(frames []tail.Frame, ctx *Context) []*storev1.StoreEntry {
	h.log.With(logging.Context{Operation: "journal-handle", Path: ctx.Path, VendorSessionID: ctx.SessionID, TaskID: ctx.TaskID}).
		LogVerbose("handling frames=%d run_id=%q", len(frames), ctx.RunID)
	// The run's identity is its run id where the path supplies one, and the task
	// id otherwise. Both name the same card, because the launch that opened it
	// used whichever the harness reported.
	taskID := ctx.RunID
	if taskID == "" {
		taskID = ctx.TaskID
	}
	var out []*storev1.StoreEntry
	for _, frame := range frames {
		at := attribute(ctx, frame.Offset)
		if frame.ParseErr != nil {
			h.log.With(logging.Context{Operation: "parse", Path: ctx.Path, VendorSessionID: ctx.SessionID, TaskID: ctx.TaskID, Level: "warn"}).
				Log("parse failure at offset=%d; the record is stored whole with no path to a page: %v", frame.Offset, frame.ParseErr)
			out = append(out, convert.UnparsedEntry(at, frame.Raw, frame.ParseErr))
			continue
		}
		out = append(out, h.conv.JournalRecord(frame.Obj, at, taskID)...)
	}
	logUnconverted(h.log, ctx, out)
	h.log.With(logging.Context{Operation: "journal-handle", Path: ctx.Path, VendorSessionID: ctx.SessionID, TaskID: ctx.TaskID}).
		LogVerbose("handled frames=%d entries=%d", len(frames), len(out))
	return out
}
