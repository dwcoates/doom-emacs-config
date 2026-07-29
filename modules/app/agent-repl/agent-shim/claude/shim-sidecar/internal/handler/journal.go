package handler

import (
	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	"agentrepl/shim-claude-sidecar/internal/convert"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// WorkflowJournalHandler converts workflow journal records (started|result) into
// vendor file-plane events. The store cannot derive a journal record's dedup key
// (the run id lives in the file PATH, not the record), so this handler supplies
// wf:<run_id>:<key>:<type> on the envelope (§6.4).
type WorkflowJournalHandler struct {
	conv *convert.Converter
	log  *logging.Bound
}

// NewWorkflowJournalHandler builds a handler with its own converter.
func NewWorkflowJournalHandler(log *logging.Bound) *WorkflowJournalHandler {
	log.With(logging.Context{Operation: "journal-handler-new"}).LogVerbose("constructing workflow journal handler")
	return &WorkflowJournalHandler{conv: convert.New(log), log: log}
}

// Handle implements Handler.
func (h *WorkflowJournalHandler) Handle(frames []tail.Frame, ctx *Context) []*corev1.Event {
	h.log.With(logging.Context{Operation: "journal-handle", Path: ctx.Path, Session: ctx.SessionID, Task: ctx.TaskID}).LogVerbose("handling frames=%d run_id=%q", len(frames), ctx.RunID)
	var out []*corev1.Event
	for _, f := range frames {
		if f.ParseErr != nil {
			h.log.With(logging.Context{Operation: "parse", Path: ctx.Path, Session: ctx.SessionID, Task: ctx.TaskID}).Log("parse failure at offset=%d: %v", f.Offset, f.ParseErr)
			out = append(out, unparsedEvent(ctx.SessionID, ctx.Path, f.Offset, f.Raw, f.ParseErr))
			continue
		}
		rec, extras, err := h.conv.JournalRecord(f.Obj)
		if err != nil {
			h.log.With(logging.Context{Operation: "convert", Path: ctx.Path, Session: ctx.SessionID, Task: ctx.TaskID}).Log("conversion failure at offset=%d: %v", f.Offset, err)
			out = append(out, unparsedEvent(ctx.SessionID, ctx.Path, f.Offset, f.Raw, err))
			continue
		}
		key := journalDedupKey(ctx.RunID, journalKey(rec), journalType(rec))
		if ev := vendorEvent(ctx.SessionID, rec, extras, key, h.log); ev != nil {
			out = append(out, ev)
		}
	}
	h.log.With(logging.Context{Operation: "journal-handle", Path: ctx.Path, Session: ctx.SessionID, Task: ctx.TaskID}).LogVerbose("handled frames=%d events=%d", len(frames), len(out))
	return out
}

func journalKey(rec *datav1.JournalRecord) string {
	if s := rec.GetStarted(); s != nil {
		return s.GetKey()
	}
	if r := rec.GetResult(); r != nil {
		return r.GetKey()
	}
	return ""
}

func journalType(rec *datav1.JournalRecord) string {
	switch {
	case rec.GetStarted() != nil:
		return "started"
	case rec.GetResult() != nil:
		return "result"
	default:
		return ""
	}
}
