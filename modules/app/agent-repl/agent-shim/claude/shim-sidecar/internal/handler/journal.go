package handler

// journal.go — a workflow run's journal and its per-agent spools.
//
// WORKFLOW IS KICKED THIS WAVE: the files are discovered and cursor-tailed so
// nothing on disk is lost, and every record converts to residue rather than to a
// workflow frame nobody consumes yet. The run id lives in the file PATH rather
// than in any record, which is why the handler supplies it.

import (
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/convert"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// WorkflowJournalHandler reads a workflow run's journal.
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
	// The run's identity is its run id where the path supplies one, and the task
	// id otherwise. Both name the same run, because the launch that opened it
	// used whichever the harness reported.
	runID := ctx.RunID
	if runID == "" {
		runID = ctx.TaskID
	}
	h.log.With(handleCtx("journal-handle", ctx)).
		LogVerbose("handling frames=%d run_id=%q", len(frames), runID)

	var out []*storev1.StoreEntry
	for _, frame := range frames {
		at := attribute(ctx, frame.Offset)
		if frame.ParseErr != nil {
			h.log.With(handleWarn("parse", ctx)).With(logging.Context{Offset: logging.Off(frame.Offset)}).
				Log("parse failure; the record is stored whole with no path to a page: %v", frame.ParseErr)
			out = append(out, convert.UnparsedEntry(at, frame.Raw, frame.ParseErr))
			continue
		}
		out = append(out, h.conv.JournalRecord(frame.Obj, at, runID)...)
	}
	logResidue(h.log, ctx, out)
	h.log.With(handleCtx("journal-handle", ctx)).
		LogVerbose("handled frames=%d entries=%d", len(frames), len(out))
	return out
}
