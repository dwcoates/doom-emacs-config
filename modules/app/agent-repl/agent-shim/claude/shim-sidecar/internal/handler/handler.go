// Package handler is the sidecar's record→record layer: pure functions with ZERO
// IO that turn decoded file records into the store entries shim-store persists.
//
// A handler owns a converter and does three things with a batch of frames:
// ATTRIBUTES each frame (whose book, which file, which offset), asks the
// converter what the record IS, and DEFERS the one record whose meaning depends
// on a line that may not be written yet.
//
// IT NEVER DECIDES WHETHER A RECORD IS INTERESTING. Curation is a downstream
// concern; ingestion's only job is that every byte on disk ends up in the store
// as a protobuf shape — as a page line, a run frame, or durable residue.
package handler

import (
	"path/filepath"
	"strings"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/convert"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// Producer is the fixed WriteBatch producer identity for the sidecar.
const Producer = convert.Producer

// Context / Kind live in the tail package (tailer-owned attribution); aliased
// here so handler code reads naturally.
type Context = tail.Context

// attribute builds the conversion attribution for one frame.
//
// EVERY IDENTITY COMES FROM THE READER, which derived it from the file path (R9:
// the main agent is the transcript FILE's session uuid, never the per-record
// `sessionId`, which diverges from the runtime's answer in ~22% of records).
// Reading it here rather than re-deriving it means the two halves of the seam
// cannot disagree about whose book a record lands in.
//
// EVERY FIELD IS READ DEFENSIVELY: an empty value means the reader has not
// supplied it, and the fallbacks below are what keep a book named rather than
// leaving a record unservable.
func attribute(ctx *Context, offset int64) convert.Attribution {
	main := firstNonEmpty(ctx.MainAgentID, ctx.SessionID, sessionIDFromPath(ctx.Path))
	at := convert.Attribution{
		WorkspaceDir:    ctx.WorkspaceDir,
		WorkspaceID:     ctx.WorkspaceID,
		ClaudeSessionID: ctx.ClaudeSessionID,
		VendorSessionID: firstNonEmpty(ctx.SessionID, main),
		MainAgentID:     main,
		Path:            ctx.Path,
		FileID:          ctx.FileID,
		Offset:          offset,
		TaskID:          ctx.TaskID,
		Backgrounded:    ctx.SpawnBackgrounded,
	}
	switch ctx.Kind {
	case tail.KindSessionTranscript:
		// The session's own book is the main agent's.
		at.AgentID = main
	case tail.KindAgentTranscript:
		// A subagent's constituents form ITS OWN book, keyed by the SPAWNING
		// CALL's tool_use_id (the cross-plane minting rule), which the reader
		// read out of the agent's meta file. The SPAWN that created it is a line
		// in the PARENT's book, which is why the two identities are distinct.
		//
		// THERE IS NO FILENAME FALLBACK. `agent-<id>` is a locator, and naming
		// the book by it would give one agent two books — one per plane — that no
		// consumer could ever reconcile. A transcript whose meta has not been
		// read is HELD by the reader and never reaches here, so an empty id is a
		// reader defect and is stated as one.
		at.AgentID = ctx.AgentID
		// The agent's OWN type, so an assistant record it merely QUOTES from a
		// parent (a fork's inherited context) can be told from one it produced
		// and is not re-booked under this agent (convert assistant.go).
		at.AgentType = ctx.AgentType
	default:
		// A spool or a journal: the run's frames name the run, and the owning
		// agent is whatever the reader resolved.
		at.AgentID = firstNonEmpty(ctx.AgentID, main)
	}
	return at
}

// sessionIDFromPath reads the session uuid out of a transcript path, for the case
// where the reader supplied none.
//
// `projects/<project>/<session>.jsonl` names it directly; a subagent transcript
// at `projects/<project>/<session>/subagents/agent-<id>.jsonl` names it as the
// directory two levels above.
func sessionIDFromPath(path string) string {
	if path == "" {
		return ""
	}
	base := strings.TrimSuffix(filepath.Base(path), ".jsonl")
	if !strings.HasPrefix(base, "agent-") {
		return base
	}
	return filepath.Base(filepath.Dir(filepath.Dir(path)))
}

// firstNonEmpty is the defensive read the seam requires: the first value the
// reader actually supplied.
func firstNonEmpty(values ...string) string {
	for _, v := range values {
		if v != "" {
			return v
		}
	}
	return ""
}

// logResidue records every record that landed with no path to a page.
//
// IT IS THE RUNNING MEASURE of how much of what the vendor writes this schema
// does not carry. An unserved item is by construction not a servable frame, so it
// never reaches a page and is invisible to the user — a legitimate outcome, and
// the one worth counting.
//
// IT IS PER SOURCE RECORD RATHER THAN PER BATCH, because a write record owes the
// full write vocabulary — file_id, path, OFFSET and write_id — and the offset is
// the one coordinate a batch cannot state: the position is the record's own, and
// a record whose position is unstated cannot be found again on disk.
func logResidue(log *logging.Bound, ctx *Context, offset int64, entries []*storev1.StoreEntry) {
	for _, entry := range entries {
		if entry.GetAgentUpdate().GetUnservedItem() == nil {
			continue
		}
		// THIS IS THE CLASSIFICATION RECORD, AND IT IS THE ONE WITH THE
		// POSITION. Since the residue ruling (2026-09-13) the bytes themselves
		// are not stored, so this record — path, file id and offset, plus the
		// arm and its discriminator — is how an unreadable or uncarried line is
		// investigated: it names the exact bytes to go and look at in the
		// vendor's own durable file. The WITHHOLDING is stated once more, at the
		// write path (`residue-drop`), which is the layer that knows nothing was
		// stored and keeps the per-file tally; the two join on `reason`.
		ctxFields := logging.Context{
			Operation: "residue", Path: ctx.Path, FileID: ctx.FileID, TaskID: ctx.TaskID,
			AgentID: ctx.AgentID, VendorSessionID: ctx.SessionID, Offset: logging.Off(offset),
			UpsertKey: entry.GetUpsertKey(), WriteID: entry.GetWriteId(),
			Reason: convert.ResidueLabel(entry),
		}
		log.With(ctxFields).LogVerbose("record classified as an unserved item: %s", convert.Describe(entry))
	}
}

// handleCtx is the correlation base for a handler's own records: the reader's
// identities in DEDICATED KEYS, never interpolated into a sentence, so the
// integration loop that reads these logs can filter and join on them.
func handleCtx(operation string, ctx *Context) logging.Context {
	return logging.Context{
		Operation:       operation,
		WorkspaceDir:    ctx.WorkspaceDir,
		WorkspaceID:     ctx.WorkspaceID,
		ClaudeSessionID: ctx.ClaudeSessionID,
		Producer:        Producer,
		Path:            ctx.Path,
		FileID:          ctx.FileID,
		TaskID:          ctx.TaskID,
		AgentID:         firstNonEmpty(ctx.AgentID, ctx.MainAgentID),
		VendorSessionID: ctx.SessionID,
	}
}

// handleWarn is handleCtx at warning level.
func handleWarn(operation string, ctx *Context) logging.Context {
	c := handleCtx(operation, ctx)
	c.Level = "warn"
	return c
}

// handleErr is handleCtx at error level.
func handleErr(operation string, ctx *Context) logging.Context {
	c := handleCtx(operation, ctx)
	c.Level = "error"
	return c
}

// lookahead returns the decoded record that FOLLOWS a frame in the file, or nil
// past the end of the batch. It exists for exactly one record: a compaction
// boundary, whose summary the harness writes as the following line.
func lookahead(frames []tail.Frame, i int) map[string]any {
	if i < 0 || i >= len(frames) {
		return nil
	}
	return frames[i].Obj
}
