package handler

// journal.go — a workflow run's journal and its per-agent transcripts.
//
// WORKFLOW IS KICKED THIS WAVE: the files are discovered and cursor-tailed so
// nothing on disk is lost, and every record converts to residue rather than to a
// workflow frame nobody consumes yet. The run id lives in the file PATH rather
// than in any record, which is why the handler supplies it.

import (
	"path/filepath"
	"strings"

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
	// A WORKFLOW RUN'S DIRECTORY HOLDS TWO DIFFERENT FILES. `journal.jsonl`
	// carries the run's two journal shapes; `agent-<id>.jsonl` carries ordinary
	// TRANSCRIPT records. They share a discovery kind, so running both through
	// the journal converter filed every transcript record as `unknown` residue —
	// "we parsed this and do not model it", which is false: the shapes are
	// modeled and the workflow FEATURE is what is kicked. `unknown` is the query
	// built to find real modelling gaps, so polluting it hides them.
	perAgent := isWorkflowAgentTranscript(ctx.Path)
	h.log.With(handleCtx("journal-handle", ctx)).
		LogVerbose("handling frames=%d run_id=%q per_agent=%t", len(frames), runID, perAgent)

	var out []*storev1.StoreEntry
	for _, frame := range frames {
		at := attribute(ctx, frame.Offset)
		if frame.ParseErr != nil {
			h.log.With(handleWarn("parse", ctx)).With(logging.Context{Offset: logging.Off(frame.Offset)}).
				Log("parse failure; the line is classified as unparsed residue and not stored, so the bytes to investigate are this file at this offset: %v", frame.ParseErr)
			unparsed := convert.UnparsedEntry(at, frame.Raw, frame.ParseErr)
			logResidue(h.log, ctx, frame.Offset, []*storev1.StoreEntry{unparsed})
			out = append(out, unparsed)
			continue
		}
		converted := h.convert(frame.Obj, at, runID, perAgent)
		logResidue(h.log, ctx, frame.Offset, converted)
		out = append(out, converted...)
	}
	h.log.With(handleCtx("journal-handle", ctx)).
		LogVerbose("handled frames=%d entries=%d", len(frames), len(out))
	return out
}

// convert routes a record to the disposition its FILE has, not the disposition
// the shared discovery kind suggests.
func (h *WorkflowJournalHandler) convert(record map[string]any, at convert.Attribution, runID string, perAgent bool) []*storev1.StoreEntry {
	if perAgent {
		return h.conv.WorkflowAgentRecord(record, at)
	}
	return h.conv.JournalRecord(record, at, runID)
}

// isWorkflowAgentTranscript reports whether a watched workflow file is a
// PER-AGENT transcript rather than the run's journal.
//
// It reads the BASENAME only, which is the same positional evidence discovery
// classified the file by (`wf_<id>/agent-<id>.jsonl` versus
// `wf_<id>/journal.jsonl`); nothing is decoded out of the path.
func isWorkflowAgentTranscript(path string) bool {
	return strings.HasPrefix(filepath.Base(path), "agent-")
}

// Conv exposes the handler's converter, so the reader can read the per-file
// facts the conversion accumulated.
func (h *WorkflowJournalHandler) Conv() *convert.Converter { return h.conv }
